{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

buildPackageDependencies :: Package -> TodoItem -> Rules ()
buildPackageDependencies (Package name path _) (stage, dist, settings) =
    let buildDir = toStandard $ path </> dist </> "build"
        pkgData  = toStandard $ path </> dist </> "package-data.mk"
    in
    (buildDir </> name <.> "m") %> \out -> do
        need ["shake/src/Package/Dependencies.hs"]
        terseRun (Ghc stage) $ arg "-M"
            <> packageArgs stage pkgData
            <> includeArgs path dist
            <> concatArgs ["-optP"] (CppOpts pkgData) 
            <> productArgs ["-odir", "-stubdir", "-hidir"] buildDir
            <> arg ["-dep-makefile", toStandard $ out <.> "new"]
            <> productArgs "-dep-suffix" (map wayPrefix <$> ways settings)
            <> arg (pkgHsSources path dist)
            -- TODO: Check that skipping all _HC_OPTS is safe.
            -- <> arg SrcHcOpts
            -- TODO: i) is this needed? ii) shall we run GHC -M multiple times?
            -- <> wayHcOpts vanilla
        -- Avoid rebuilding dependecies of out if it hasn't changed:
        -- Note: cannot use copyFileChanged as it depends on the source file
        deps <- liftIO $ readFile $ out <.> "new"
        writeFileChanged out deps
        removeFilesAfter "." [out <.> "new"]