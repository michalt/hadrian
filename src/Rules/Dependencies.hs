module Rules.Dependencies (buildPackageDependencies) where

import Development.Shake.Util

import Base
import Context
import Expression
import Oracles.ModuleFiles
import Rules.Generate
import Settings.Path
import Target
import Util
import Rules.Generate

buildPackageDependencies :: [(Resource, Int)] -> Context -> Rules ()
buildPackageDependencies rs context@Context {..} =
    buildPath context -/- ".dependencies" %> \deps -> do
        srcs <- hsSources context
        need srcs
        let f src =
                if "compiler" `isPrefixOf` src
                  then needDependencies FindCDependenciesOfHs context src $ src <.> "dhs"
                  else return ()
        mapM_ f srcs
        let mk = deps <.> "mk"
        if srcs == []
        then writeFileChanged mk ""
        else buildWithResources rs $
            -- FIXME: This wil run before we compile .hs files and thus can fail
            -- if the auto-generated header files are not there (e.g.,
            -- primout-code-size.hs-incl)
            Target context (Ghc FindHsDependencies stage) srcs [mk]
        removeFile $ mk <.> "bak"
        mkDeps <- readFile' mk
        writeFileChanged deps . unlines
                              . map (\(src, deps) -> unwords $ src : deps)
                              . map (bimap unifyPath (map unifyPath))
                              . map (bimap head concat . unzip)
                              . groupBy ((==) `on` fst)
                              . sortBy (compare `on` fst)
                              $ parseMakefile mkDeps
