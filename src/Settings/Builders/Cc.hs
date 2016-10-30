module Settings.Builders.Cc (ccBuilderArgs) where

import Settings.Builders.Common

ccBuilderArgs :: Args
ccBuilderArgs = builder Cc ? mconcat
    [ append =<< getPkgDataList CcArgs
    , argSettingList . ConfCcArgs =<< getStage
    , cIncludeArgs

    , builder (Cc CompileC) ?
        mconcat [ arg "-c", arg =<< getInput
                , arg "-o", arg =<< getOutput ]

    , builder (Cc FindCDependencies) ? do
        output <- getOutput
        mconcat [ arg "-E"
                , arg "-MM", arg "-MG"
                , arg "-MF", arg output
                , arg "-MT", arg $ dropExtension output -<.> "o"
                , arg "-x", arg "c"
                , arg =<< getInput ]

    , builder (Cc FindCDependenciesOfHs) ? do
        output <- getOutput
        path    <- getBuildPath
        mconcat [ arg "-E"
                , arg "-MM", arg "-MG"
                , arg "-MF", arg output
                , arg "-MT", arg $ dropExtension output -<.> "o"
                -- Both ghcversio.h and cabal_macros.h are included
                -- automatically for Haskell files that use CPP.
                , arg $ "-include" ++ generatedPath -/- "ghcversion.h"
                , arg $ "-include" ++ path -/- "autogen/cabal_macros.h"
                -- Force language C, otherwise `.hs` files are treated in a
                -- weird way by GCC.
                , arg "-x", arg "c"
                , arg =<< getInput ]
    ]
