-- | Import this module in your @Setup.hs@ to auto detect library modules in
-- your project The API does not conceal it's internals but in most cases you
-- should only need 'defaultMain' or 'defaultMainWithHooks'.
module Distribution.Simple.AutoExpose where

import Control.Exception(catch,IOException)
import Control.Monad((>=>),filterM)
import Control.Monad.Extra(ifM,notM)
import Data.List(intercalate,nub)
import Distribution.Compat.Lens((%~))
import Distribution.ModuleName(ModuleName,fromString,validModuleComponent)
import Distribution.PackageDescription(hsSourceDirs,buildInfo,testBuildInfo,benchmarkBuildInfo,executables,testSuites,benchmarks,libBuildInfo,subLibraries,library,Library)
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
import qualified Distribution.Simple(defaultMainWithHooks,simpleUserHooks)
import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.Library.Lens as L

-- | The supported Haskell source extensions, just 'hs' and 'lhs'
sourceExtensions :: [String]
sourceExtensions = ["hs","lhs"]

-- | Search for file paths that look like valid modules and convert to the
--   components to the Cabal internal 'ModuleName'.
moduleNamesToExpose
  :: [String] -- ^ File extensions of valid Haskell modules, includes pre-processor extensions
  -> [FilePath] -- ^ File paths to search
  -> [ModuleName]
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

-- | Recursively collect the files in a directory, optionally excluding some
-- files. Symlinks are ignored and collected paths are relative to the search
-- directory, eg. if the search directory is @\/home\/user\/myproject\/src@ the path
-- @\/home\/user\/myproject\/src\/A\/B\/C.hs@ is returned as @A\/B\/C.hs@ so it can
-- converted by 'moduleNamesToExpose' to a valid module name.
getDirectoryContents
  :: FilePath -- ^ Directory to search (path must be absolute)
  -> [FilePath] -- ^ Paths to ignore (paths must be absolute)
  -> IO [FilePath] -- ^ File paths made relative to the search directory
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

-- | Source directories for all non-library components (benchmarks, executables
-- etc.) so that we can exclude their modules if they appear inside the library's
-- source directory
nonLibraryHsSourcePaths :: PackageDescription -> [[FilePath]]
nonLibraryHsSourcePaths pd =
  map hsSourceDirs $
     (map buildInfo (executables pd))
  ++ (map testBuildInfo (testSuites pd))
  ++ (map benchmarkBuildInfo (benchmarks pd))

-- | Associate each item in a list will it's left and right elements, eg.
-- > indexWithNeighbors [1,2,3,4] == [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
--
-- Used to associate a source directory with possibly inner directories that
-- should be ignored with searching for Haskell modules.
indexWithNeighbors :: [a] -> [(a,[a])]
indexWithNeighbors (a:as) = reverse (go [] a as [])
  where
    go [] x (r:rs) accum = go [x] r rs ((x,(r:rs)):accum)
    go ls x (r:rs) accum = go (ls++[x]) r rs ((x,(ls++(r:rs))):accum)
    go ls x [] accum = (x,ls):accum
indexWithNeighbors [] = []

-- | A wrapper around a list of 'ModuleName' for a more precise type
--
newtype ExposedLib =
  ExposedLib { exposedLibMainLib :: [ModuleName] }
  deriving Show

-- | Drill into the source trees for a component and find modules
-- excluding the source trees for other components
getExposedModules
  :: [String] -- ^ Known Haskell and preprocessor extensions
  -> [FilePath] -- ^ The source trees for this component
  -> [[FilePath]] -- ^ The source trees for all the other components
  -> IO [ModuleName] -- ^ Detect modules in this component
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

-- | Pull out the main library from the 'PackageDescription' of this
-- project and get a list of detected Haskell modules
getExposedLibs
  :: PackageDescription
  -> UserHooks -- ^ So we can grab the 'hookedPreProcessors' extensions
  -> IO [ModuleName]
getExposedLibs pd uhs =
  let excluded =
        map libSrcDir (subLibraries pd) ++ (nonLibraryHsSourcePaths pd)
      libExposedModules l =
        getExposedModules ppExts (libSrcDir l) excluded
  in maybe (pure []) libExposedModules (library pd)
  where
    ppExts :: [String]
    ppExts = (nub . map fst) ((hookedPreProcessors uhs) ++ knownSuffixHandlers)
    libSrcDir :: Library -> [FilePath]
    libSrcDir = nub . hsSourceDirs . libBuildInfo

-- | Update the 'PackageDescription' of this package to include auto detected
-- library modules. Also just to be nice fill in the 'Paths_...' module in
-- 'otherModules' field of the library's 'BuildInfo'.
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
             . (L.exposedModules %~ (nub . (++) exposedLibs))
        pure $
          pd { library = fmap newMainLibrary (library pd) }

-- | Modify a set of 'UserHooks' so that all relevant hooks see a 'PackageDescription' with
-- auto detected modules filled in.
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

-- | If you have already using custom 'UserHooks' use this in your Setup.hs's 'main'
--
-- > import qualified Distribution.Simple.AutoExpose as AutoExpose
-- > main = AutoExpose.defaultMainWithHooks myHooks
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks uhs = Distribution.Simple.defaultMainWithHooks (autoExposeHooks uhs)

-- | The common case top level function where this library is the only custom part of your project
--
-- > import qualified Distribution.Simple.AutoExpose
-- > main = AutoExpose.defaultMain
defaultMain :: IO ()
defaultMain = defaultMainWithHooks Distribution.Simple.simpleUserHooks
