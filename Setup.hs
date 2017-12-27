-- pilfered from lens package
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.Package ( PackageName, Package, PackageId, InstalledPackageId, packageVersion, packageName, unPackageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose, copyFiles, ordNub )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), Flag(..), fromFlag, HaddockFlags(haddockDistPref))
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Text ( display )
import Distribution.Types.MungedPackageId ( MungedPackageId(mungedName, mungedVersion) )
import Distribution.Types.MungedPackageName ( MungedPackageName, unMungedPackageName )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.Verbosity ( Verbosity, normal )
import Distribution.Version ( showVersion )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      let testSuiteName = unUnqualComponentName (testName suite)
      rewriteFile (dir </> "Build_" ++ testSuiteName ++ ".hs") $ unlines
        [ "module Build_" ++ testSuiteName ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p =
      unMungedPackageName (mungedName p) ++ "-" ++ showVersion (mungedVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, MungedPackageId)]
testDeps xs ys = ordNub $ componentPackageDeps xs ++ componentPackageDeps ys
