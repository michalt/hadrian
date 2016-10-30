module Rules.Generate (
    isGeneratedCFile, isGeneratedCmmFile, generatePackageCode, generateRules,
    installTargets, copyRules, includesDependencies, generatedDependencies,
    needDependencies
    ) where

import qualified System.Directory as IO

import Development.Shake.Util

import Base
import Context hiding (package)
import Expression
import GHC
import Oracles.ModuleFiles
import Predicate
import Rules.Generators.ConfigHs
import Rules.Generators.GhcAutoconfH
import Rules.Generators.GhcBootPlatformH
import Rules.Generators.GhcPlatformH
import Rules.Generators.GhcSplit
import Rules.Generators.GhcVersionH
import Rules.Generators.VersionHs
import Rules.Libffi
import Settings.Packages.Rts
import Settings.Path
import Target
import UserSettings
import Util

import Debug.Trace

installTargets :: [FilePath]
installTargets = [ "inplace/lib/ghc-usage.txt"
                 , "inplace/lib/ghci-usage.txt"
                 , "inplace/lib/platformConstants"
                 , "inplace/lib/settings"
                 , "inplace/lib/template-hsc.h" ]

primopsSource :: FilePath
primopsSource = "compiler/prelude/primops.txt.pp"

primopsTxt :: Stage -> FilePath
primopsTxt stage = buildPath (vanillaContext stage compiler) -/- "primops.txt"

platformH :: Stage -> FilePath
platformH stage = buildPath (vanillaContext stage compiler) -/- "ghc_boot_platform.h"

isGeneratedCFile :: FilePath -> Bool
isGeneratedCFile file = takeBaseName file `elem` ["Evac_thr", "Scav_thr"]

isGeneratedCmmFile :: FilePath -> Bool
isGeneratedCmmFile file = takeBaseName file == "AutoApply"

-- FIXME: This is included from things like primops.txt.pp (through MachDeps.h)
includesDependencies :: [FilePath]
includesDependencies = fmap (generatedPath -/-)
    [ "ghcautoconf.h"
    , "ghcplatform.h"
    , "ghcversion.h" ]

ghcPrimDependencies :: Expr [FilePath]
ghcPrimDependencies = do
    stage <- getStage
    let path = buildPath $ vanillaContext stage ghcPrim
    return [path -/- "GHC/Prim.hs", path -/- "GHC/PrimopWrappers.hs"]

derivedConstantsDependencies :: [FilePath]
derivedConstantsDependencies = installTargets ++ fmap (generatedPath -/-)
    [ "DerivedConstants.h"
    , "GHCConstantsHaskellExports.hs"
    , "GHCConstantsHaskellType.hs"
    , "GHCConstantsHaskellWrappers.hs" ]

compilerDependencies :: Expr [FilePath]
compilerDependencies =
    notStage0 ? return (gmpLibraryH : libffiDependencies)

generatedDependencies :: Expr [FilePath]
generatedDependencies = mconcat
    [ package compiler ? compilerDependencies
    , package ghcPrim  ? ghcPrimDependencies
    , package rts      ? return libffiDependencies
    ]

generate :: FilePath -> Context -> Expr String -> Action ()
generate file context expr = do
    contents <- interpretInContext context expr
    writeFileChanged file contents
    putSuccess $ "| Successfully generated " ++ file ++ "."

generatePackageCode :: Context -> Rules ()
generatePackageCode context@(Context stage pkg _) =
    let path        = buildPath context
        generated f = (path ++ "//*.hs") ?== f && not ("//autogen/*" ?== f)
        go gen file = generate file context gen
    in do
        generated ?> \file -> do
            let unpack = fromMaybe . error $ "No generator for " ++ file ++ "."
            (src, builder) <- unpack <$> findGenerator context file
            need [src]
            build $ Target context builder [src] [file]
            let boot = src -<.> "hs-boot"
            whenM (doesFileExist boot) . copyFile boot $ file -<.> "hs-boot"

        priority 2.0 $ do
            when (pkg == compiler) $ path -/- "Config.hs" %> go generateConfigHs
            when (pkg == ghcPkg) $ path -/- "Version.hs" %> go generateVersionHs

        -- TODO: needing platformH is ugly and fragile
        when (pkg == compiler) $ do
            primopsTxt stage %> \file -> do
                need $ [platformH stage, primopsSource] ++ includesDependencies
                build $ Target context HsCpp [primopsSource] [file]

            platformH stage %> go generateGhcBootPlatformH

        -- TODO: why different folders for generated files?
        fmap (path -/-)
            [ "GHC/Prim.hs"
            , "GHC/PrimopWrappers.hs"
            , "*.hs-incl" ] |%> \file -> do
                need [primopsTxt stage]
                build $ Target context GenPrimopCode [primopsTxt stage] [file]

        when (pkg == rts) $ path -/- "cmm/AutoApply.cmm" %> \file ->
            build $ Target context GenApply [] [file]

copyRules :: Rules ()
copyRules = do
    "inplace/lib/ghc-usage.txt"      <~ "driver"
    "inplace/lib/ghci-usage.txt"     <~ "driver"
    "inplace/lib/platformConstants"  <~ generatedPath
    "inplace/lib/settings"           <~ "."
    "inplace/lib/template-hsc.h"     <~ pkgPath hsc2hs
    rtsBuildPath -/- "c/sm/Evac_thr.c" %> copyFile (pkgPath rts -/- "sm/Evac.c")
    rtsBuildPath -/- "c/sm/Scav_thr.c" %> copyFile (pkgPath rts -/- "sm/Scav.c")
  where
    file <~ dir = file %> copyFile (dir -/- takeFileName file)

generateRules :: Rules ()
generateRules = do
    (generatedPath -/- "ghcautoconf.h") <~ generateGhcAutoconfH
    (generatedPath -/- "ghcplatform.h") <~ generateGhcPlatformH
    (generatedPath -/-  "ghcversion.h") <~ generateGhcVersionH

    ghcSplit %> \_ -> do
        generate ghcSplit emptyTarget generateGhcSplit
        makeExecutable ghcSplit

    -- TODO: simplify, get rid of fake rts context
    generatedPath ++ "//*" %> \file -> do
        withTempDir $ \dir -> build $
            Target rtsContext DeriveConstants [] [file, dir]
  where
    file <~ gen = file %> \out -> generate out emptyTarget gen

-- TODO: Use the Types, Luke! (drop partial function)
-- We sometimes need to evaluate expressions that do not require knowing all
-- information about the context. In this case, we don't want to know anything.
emptyTarget :: Context
emptyTarget = vanillaContext (error "Rules.Generate.emptyTarget: unknown stage")
                             (error "Rules.Generate.emptyTarget: unknown package")

-- | Discover dependencies of a given source file by iteratively calling @gcc@
-- in the @-MM -MG@ mode and building generated dependencies if they are missing
-- until reaching a fixed point.
needDependencies :: CcMode -> Context -> FilePath -> FilePath -> Action ()
needDependencies ccmode context@Context {..} src depFile = discover
  where
    discover = do
        trace ("### looking at " ++ src) $ return ()
        build $ Target context (Cc ccmode stage) [src] [depFile]
        deps <- parseFile depFile
        -- Generated dependencies, if not yet built, will not be found and hence
        -- will be referred to simply by their file names.
        let notFound = filter (\file -> file == takeFileName file) deps
        trace ("### notFound=" ++ show notFound) $ return ()

        -- We find the full paths to generated dependencies, so we can request
        -- to build them by calling 'need'.
        todo <- catMaybes <$> mapM (fullPathIfGenerated context) notFound
        trace ("### todo=" ++ show todo) $ return ()

        if null todo
        then do
            trace ("### need deps: " ++ show deps) $ return ()
            need deps -- The list of dependencies is final, need all
        else do
            trace ("### need todo: " ++ show todo) $ return ()
            need todo  -- Build newly discovered generated dependencies
            discover   -- Continue the discovery process

    parseFile :: FilePath -> Action [String]
    parseFile file = do
        input <- liftIO $ readFile file
        case parseMakefile input of
            [(_file, deps)] -> return deps
            _               -> return []

-- | To be used only in @fullPathIfGenerated@ for figuring out whether something
-- is generated and what is its path.
allGeneratedDependencies :: Expr [FilePath]
allGeneratedDependencies = mconcat
    [ package compiler ? allCompilerDependencies
    , package ghcPrim  ? ghcPrimDependencies
    , package rts      ? return (libffiDependencies
        ++ includesDependencies
        ++ derivedConstantsDependencies)
    , stage0 ? return includesDependencies ]

-- | To be used only in @allGeneratedDependencies@.
allCompilerDependencies :: Expr [FilePath]
allCompilerDependencies = do
    stage <- getStage
    let path = buildPath $ vanillaContext stage compiler
    mconcat [ return [platformH stage]
            , return includesDependencies
            , return derivedConstantsDependencies
            , notStage0 ? return (gmpLibraryH : libffiDependencies)
            , return $ fmap (path -/-)
                  [ "primop-can-fail.hs-incl"
                  , "primop-code-size.hs-incl"
                  , "primop-commutable.hs-incl"
                  , "primop-data-decl.hs-incl"
                  , "primop-fixity.hs-incl"
                  , "primop-has-side-effects.hs-incl"
                  , "primop-list.hs-incl"
                  , "primop-out-of-line.hs-incl"
                  , "primop-primop-info.hs-incl"
                  , "primop-strictness.hs-incl"
                  , "primop-tag.hs-incl"
                  , "primop-vector-tycons.hs-incl"
                  , "primop-vector-tys-exports.hs-incl"
                  , "primop-vector-tys.hs-incl"
                  , "primop-vector-uniques.hs-incl" ] ]

-- | Find a given 'FilePath' in the list of generated files in the given
-- 'Context' and return its full path.
fullPathIfGenerated :: Context -> FilePath -> Action (Maybe FilePath)
fullPathIfGenerated context file = interpretInContext context $ do
    generated <- allGeneratedDependencies
    return $ find ((== file) . takeFileName) generated
