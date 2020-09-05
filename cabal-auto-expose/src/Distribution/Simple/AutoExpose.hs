module Distribution.Simple.AutoExpose where

import Control.Exception(catch,IOException)
import Control.Monad((>=>),filterM,forM)
import Control.Monad.Extra(ifM,notM)
import Data.List(intercalate,nub)
import Data.Maybe(maybeToList)
import Distribution.Compat.Lens((%~),(&))
import Distribution.ModuleName(ModuleName,fromString,validModuleComponent)
import Distribution.PackageDescription(hsSourceDirs,buildInfo,testBuildInfo,benchmarkBuildInfo,executables,testSuites,benchmarks,libBuildInfo,subLibraries,library,Library)
import Distribution.Simple(defaultMainWithHooks,simpleUserHooks)
import Distribution.Simple.BuildPaths(autogenPathsModuleName)
import Distribution.Simple.PreProcess(knownSuffixHandlers)
import Distribution.Simple.Setup(BuildFlags, ReplFlags, HscolourFlags, HaddockFlags, CopyFlags, InstallFlags, TestFlags, BenchmarkFlags, RegisterFlags, DoctestFlags)
import Distribution.Simple.UserHooks(UserHooks,Args,hookedPreProcessors, buildHook,replHook,hscolourHook,doctestHook,haddockHook,copyHook,instHook,testHook,benchHook,regHook,unregHook)
import Distribution.Simple.Utils(findPackageDesc)
import Distribution.Types.LocalBuildInfo(LocalBuildInfo)
import Distribution.Types.PackageDescription(PackageDescription)
import GHC.Stack(HasCallStack)
import System.Directory(makeAbsolute,listDirectory,doesDirectoryExist,withCurrentDirectory,pathIsSymbolicLink)
import System.FilePath(splitDirectories, dropExtension, takeExtension,equalFilePath,makeRelative)
import qualified Distribution.Types.Library.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L

sourceExtensions :: [String]
sourceExtensions = ["hs","lhs"]

moduleNamesToExpose :: [String] -> [FilePath] -> [ModuleName]
moduleNamesToExpose extensions =
  map (fromString . intercalate ".")
  . filter (all validModuleComponent)
  . map toModuleComponents
  . filter hasExtension
  where
    hasExtension :: FilePath -> Bool
    hasExtension f =
      elem
        (drop 1 (takeExtension f)) -- 'takeExtension' preserves the '.' so drop it
        extensions
    toModuleComponents :: FilePath -> [String]
    toModuleComponents =
      splitDirectories . dropExtension

getDirectoryContents :: FilePath -> [FilePath] -> IO [FilePath]
getDirectoryContents dir excludedDirs = do
  (map (makeRelative dir)) <$> go [dir] []
  where
    go :: [FilePath] -> [FilePath] -> IO [FilePath]
    go (f:fs) accum
      | any (equalFilePath f) excludedDirs = go fs accum
      | otherwise =
          ifM (doesDirectoryExist f)
            (catch
              (withCurrentDirectory f $ do
                 contents <-
                   (listDirectory >=> filterM (notM . pathIsSymbolicLink) >=> mapM makeAbsolute) f
                 go (contents ++ fs) accum)
              (\(_ :: IOException) -> go fs accum))
            (go fs (f:accum))
    go [] accum = pure accum

nonLibraryHsSourcePaths :: PackageDescription -> [[FilePath]]
nonLibraryHsSourcePaths pd =
  map hsSourceDirs $
     (map buildInfo (executables pd))
  ++ (map testBuildInfo (testSuites pd))
  ++ (map benchmarkBuildInfo (benchmarks pd))

indexWithNeighbors :: [a] -> [(a,[a])]
indexWithNeighbors (a:as) = reverse (go [] a as [])
  where
    go [] x (r:rs) accum = go [x] r rs ((x,(r:rs)):accum)
    go ls x (r:rs) accum = go (ls++[x]) r rs ((x,(ls++(r:rs))):accum)
    go ls x [] accum = (x,ls):accum
indexWithNeighbors [] = []

data ExposedLibs =
  ExposedLibs
  { exposedLibsMainLib :: [ModuleName]
  , exposedLibsSubLibs :: [(Library,[ModuleName])]
  }
  deriving Show

getExposedModules :: [String] -> [FilePath] -> [[FilePath]] -> IO [ModuleName]
getExposedModules ppExts hsSrcDirs otherHsSrcDirs = do
  absHsSrcDirs <- mapM makeAbsolute hsSrcDirs
  absOtherHsSrcDirs <- mapM makeAbsolute (concat otherHsSrcDirs)
  contents <-
    concat
    <$> ( mapM (\(srcDir,excludeDirs) -> getDirectoryContents srcDir excludeDirs)
          $ map (\(srcDir,otherSrcDirs) -> (srcDir,otherSrcDirs ++ absOtherHsSrcDirs))
          $ indexWithNeighbors absHsSrcDirs
        )
  pure $ moduleNamesToExpose (sourceExtensions ++ ppExts) contents

getExposedLibs :: PackageDescription -> UserHooks -> IO ExposedLibs
getExposedLibs pd uhs =
  pure ExposedLibs
       <*> exposeMainLib
       <*> exposeSubLibs
  where
    ppExts :: [String]
    ppExts = (nub . map fst) ((hookedPreProcessors uhs) ++ knownSuffixHandlers)
    libSrcDir :: Library -> [FilePath]
    libSrcDir = nub . hsSourceDirs . libBuildInfo
    exposeMainLib :: IO [ModuleName]
    exposeMainLib =
      let excluded =
            map libSrcDir (subLibraries pd) ++ (nonLibraryHsSourcePaths pd)
          libExposedModules l =
            getExposedModules ppExts (libSrcDir l) excluded
      in maybe (pure []) libExposedModules (library pd)
    exposeSubLibs :: IO [(Library,[ModuleName])]
    exposeSubLibs =
      let excluded =
            nonLibraryHsSourcePaths pd ++ map libSrcDir (maybeToList (library pd))
          libExposedModules l ls =
            getExposedModules ppExts (libSrcDir l) ((map libSrcDir ls) ++ excluded)
      in forM (indexWithNeighbors (subLibraries pd)) (\(l,ls) -> ((,) l) <$> libExposedModules l ls)

updatePackageDescription :: HasCallStack => PackageDescription -> UserHooks -> IO PackageDescription
updatePackageDescription pd uhs = do
  cabalFilePath <- findPackageDesc "."
  case cabalFilePath of
    Left err -> error err
    Right _ ->
      withCurrentDirectory "." $ do
        exposedLibs <- getExposedLibs pd uhs
        let newMainLibrary =
             (L.libBuildInfo . L.otherModules %~ (nub . (++) [(autogenPathsModuleName pd)]))
             . (L.exposedModules %~ (nub . (++) (exposedLibsMainLib exposedLibs)))
        pure $
          pd { library = fmap newMainLibrary (library pd)
             , subLibraries = map (\(l,eps) -> l & L.exposedModules %~ (nub . (++) eps)) (exposedLibsSubLibs exposedLibs)
             }

autoExposeHooks :: UserHooks -> UserHooks
autoExposeHooks userHooks =
  userHooks
  { buildHook = bh
  , replHook = rh
  , hscolourHook = hscolourH
  , doctestHook = dth
  , haddockHook = hh
  , copyHook = ch
  , instHook = ih
  , testHook = th
  , benchHook = benchH
  , regHook = regH
  , unregHook = unregH
  }
  where
    bh :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
    bh pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (buildHook userHooks) newPd lbi uhs fs
    rh :: PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags -> [String] -> IO ()
    rh pd lbi uhs fs opts = do
      newPd <- updatePackageDescription pd uhs
      (replHook userHooks) newPd lbi uhs fs opts
    hscolourH :: PackageDescription -> LocalBuildInfo -> UserHooks -> HscolourFlags -> IO ()
    hscolourH pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (hscolourHook userHooks) newPd lbi uhs fs
    dth :: PackageDescription -> LocalBuildInfo -> UserHooks -> DoctestFlags -> IO ()
    dth pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (doctestHook userHooks) newPd lbi uhs fs
    hh :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
    hh pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (haddockHook userHooks) newPd lbi uhs fs
    ch :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
    ch pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (copyHook userHooks) newPd lbi uhs fs
    ih :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
    ih pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (instHook userHooks) newPd lbi uhs fs
    th :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO ()
    th args pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (testHook userHooks) args newPd lbi uhs fs
    benchH :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> BenchmarkFlags -> IO ()
    benchH args pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (benchHook userHooks) args newPd lbi uhs fs
    regH :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
    regH pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (regHook userHooks) newPd lbi uhs fs
    unregH :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
    unregH pd lbi uhs fs = do
      newPd <- updatePackageDescription pd uhs
      (unregHook userHooks) newPd lbi uhs fs

defaultMain :: IO ()
defaultMain = defaultMainWithHooks (autoExposeHooks simpleUserHooks)
