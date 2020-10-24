module Main where

import Paths_cabal_auto_expose
import Test.Hspec
import System.FilePath
import Distribution.Simple.AutoExpose
import Distribution.PackageDescription.PrettyPrint
import Distribution.Types.GenericPackageDescription
import Test.QuickCheck
import Data.List

prop_preserves_order :: [String] -> Bool
prop_preserves_order xs =
  map fst (indexWithNeighbors xs) == xs

prop_contains_list_contents :: [String] -> Bool
prop_contains_list_contents xs =
  all (\(x,ns) -> sort x:ns == sort xs) (indexWithNeighbors xs)

testProjectAgainstGoldenCabalFile :: FilePath -> FilePath -> IO ()
testProjectAgainstGoldenCabalFile projectPath goldenFilePath = do
  gpdE <- cabalFileToGenericPackageDescription projectPath
  case gpdE of
    Left _ -> fail $ "Could not read test cabal file at project path: " ++ projectPath
    Right gpd -> do
      newGpd <- updateGenericPackageDescription (Just projectPath) gpd []
      expected <- readFile goldenFilePath
      (showGenericPackageDescription newGpd) `shouldBe` expected

main = do
  quickCheck prop_preserves_order
  quickCheck prop_contains_list_contents
  testProjects <- getDataDir
  hspec $ do
    describe "Simple builds:" $ do
      it "exposes component modules" $
        testProjectAgainstGoldenCabalFile
          (testProjects </> "simple-build")
          (testProjects </> "golden" </> "simple-build-expected.cabal")
      it "exposes overlapping components" $
        testProjectAgainstGoldenCabalFile
          (testProjects </> "overlapping-build")
          (testProjects </> "golden" </> "overlapping-build-expected.cabal")
