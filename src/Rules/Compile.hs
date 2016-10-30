module Rules.Compile (compilePackage) where

import Development.Shake.Util

import Base
import Context
import Expression
import Oracles.Dependencies
import Rules.Generate
import Settings.Path
import Target
import Util

import Debug.Trace

compilePackage :: [(Resource, Int)] -> Context -> Rules ()
compilePackage rs context@Context {..} = do
    let path            = buildPath context
        nonHs extension = path -/- extension <//> "*" <.> osuf way
        compile compiler obj2src obj = do
            let src = obj2src context obj
            need [src]
            needDependencies FindCDependencies context src $ obj <.> "d"
            build $ Target context (compiler stage) [src] [obj]
        compileHs = \[obj, _hi] -> do
            (src, deps) <- fileDependencies context obj
            need $ src : deps
            trace src $ return ()
            -- FIXME: Only try to run `needDependencies` for things under
            -- `compiler/`
            if "compiler" `isPrefixOf` src
              then needDependencies FindCDependenciesOfHs context src $ obj <.> "dhs"
              else return ()
            when (isLibrary package) $ need =<< return <$> pkgConfFile context
            needContext =<< contextDependencies context
            buildWithResources rs $ Target context (Ghc CompileHs stage) [src] [obj]

    priority 2.0 $ do
        nonHs "c"   %> compile (Cc  CompileC ) (obj2src "c"   isGeneratedCFile  )
        nonHs "cmm" %> compile (Ghc CompileHs) (obj2src "cmm" isGeneratedCmmFile)
        nonHs "s"   %> compile (Ghc CompileHs) (obj2src "S"   $ const False     )

    -- TODO: Add dependencies for #include of .h and .hs-incl files (gcc -MM?).
    [ path <//> "*" <.> suf way | suf <- [    osuf,     hisuf] ] &%> compileHs
    [ path <//> "*" <.> suf way | suf <- [obootsuf, hibootsuf] ] &%> compileHs


obj2src :: String -> (FilePath -> Bool) -> Context -> FilePath -> FilePath
obj2src extension isGenerated context@Context {..} obj
    | isGenerated src = src
    | otherwise       = pkgPath package ++ suffix
  where
    src    = obj -<.> extension
    suffix = fromMaybe ("Cannot determine source for " ++ obj)
           $ stripPrefix (buildPath context -/- extension) src
