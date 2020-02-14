-- pilfered from lens package
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Pretty ( prettyShow )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Flag ( fromFlag )
import Distribution.Simple.Utils ( rewriteFileEx, createDirectoryIfMissingVerbose, ordNub )
import Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity) )
import Distribution.Simple.LocalBuildInfo ( withTestLBI, LocalBuildInfo, componentPackageDeps )
import Distribution.Types.Component ( foldComponent )
import Distribution.Types.LocalBuildInfo ( allTargetsInBuildOrder' )
import Distribution.Types.MungedPackageId ( MungedPackageId(mungedName, mungedVersion) )
import Distribution.Types.MungedPackageName ( encodeCompatPackageName )
import Distribution.Types.PackageName ( unPackageName )
import Distribution.Types.TargetInfo ( TargetInfo (targetComponent,targetCLBI) )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.Verbosity ( Verbosity )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenPackageModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  let libdeps = concatMap (componentPackageDeps . targetCLBI) $
                filter (foldComponent (const True) (const False) (const False)
                                (const False) (const False) . targetComponent) $
                allTargetsInBuildOrder' pkg lbi
  withTestLBI pkg lbi $ \suite suitecfg -> do
      let testSuiteName = unUnqualComponentName (testName suite)
          testSuiteDeps = ordNub $ componentPackageDeps suitecfg ++ libdeps
      rewriteFileEx verbosity (dir </> "Build_" ++ testSuiteName ++ ".hs") $ unlines
        [ "module Build_" ++ testSuiteName ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps testSuiteDeps)
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p =
      unPackageName (encodeCompatPackageName (mungedName p))
        ++ "-" ++ prettyShow (mungedVersion p)
