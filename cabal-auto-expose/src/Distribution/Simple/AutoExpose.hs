{-# LANGUAGE LambdaCase #-}
-- | Import this module in your @Setup.hs@ to auto detect library modules in
-- your project. The API does not conceal it's internals but in most cases you
-- should only need the functions and datatype under
-- 'Quick Start Functions' ("Distribution.Simple.AutoExpose#QuickStartFunctions").
-- For more granular access the ones under
-- 'Internal Functions' ("Distribution.Simple.AutoExpose#InternalFunctions") are
-- available but subject to change.

module Distribution.Simple.AutoExpose where

import Control.Exception(catch,IOException)
import Control.Monad((>=>),filterM)
import Control.Monad.Extra(ifM,notM,whenJust)
import Data.List(intercalate,nub,inits,unfoldr,sort)
import Data.Maybe(fromMaybe)
import Distribution.Compat.Lens((%~),(&))
import Distribution.ModuleName(ModuleName,fromString,validModuleComponent)
import Distribution.PackageDescription(hsSourceDirs,buildInfo,testBuildInfo,benchmarkBuildInfo,executables,testSuites,benchmarks,libBuildInfo,subLibraries,library,Library,GenericPackageDescription(..),HookedBuildInfo,Executable,TestSuite,Benchmark,condTreeData,packageDescription)
import Distribution.PackageDescription.Parsec(readGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint(writeGenericPackageDescription)
import Distribution.Pretty(prettyShow)
import Distribution.Simple.BuildPaths(autogenPathsModuleName)
import Distribution.Simple.PreProcess(knownSuffixHandlers)
import Distribution.Simple.Setup(BuildFlags, ReplFlags, HscolourFlags, HaddockFlags, CopyFlags, InstallFlags, TestFlags, BenchmarkFlags, RegisterFlags, DoctestFlags, ConfigFlags,fromFlag, configVerbosity)
import Distribution.Simple.UserHooks(UserHooks,Args,hookedPreProcessors, buildHook,replHook,hscolourHook,doctestHook,haddockHook,copyHook,instHook,testHook,benchHook,regHook,unregHook,confHook)
import Distribution.Simple.Utils(findPackageDesc,notice)
import Distribution.Types.LocalBuildInfo(LocalBuildInfo)
import Distribution.Types.PackageDescription(PackageDescription,package)
import Distribution.Types.PackageId(PackageIdentifier(pkgName,pkgVersion))
import Distribution.Types.Version()
import Distribution.Verbosity(silent)
import GHC.Stack(HasCallStack)
import System.Directory(makeAbsolute,listDirectory,doesDirectoryExist,withCurrentDirectory,pathIsSymbolicLink,getTemporaryDirectory)
import System.FilePath(splitDirectories, dropExtension, takeExtension,equalFilePath,makeRelative,(</>),(<.>))
import qualified Distribution.Simple(defaultMainWithHooks,simpleUserHooks)
import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.Library.Lens as L

-- * Quick Start Functions #QuickStartFunctions#

-- | The common case top level function where this library is the only custom part of your project
--
-- > import qualified Distribution.Simple.AutoExpose
-- > main = AutoExpose.defaultMain
defaultMain :: IO ()
defaultMain = defaultMainWithHooks Distribution.Simple.simpleUserHooks

-- | If you have already using custom 'UserHooks' use this in your Setup.hs's 'main'
--
-- > import qualified Distribution.Simple.AutoExpose as AutoExpose
-- > main = AutoExpose.defaultMainWithHooks myHooks
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks uhs = Distribution.Simple.defaultMainWithHooks (autoExposeHooks Nothing uhs)

-- | The common case top level function where this library is the only custom part of your project
--
-- It also generates an explicit Cabal file at @\/<system-temp-directory>\/<package-name>-<package-version>-generated.cabal@
--
-- > import qualified Distribution.Simple.AutoExpose
-- > main = AutoExpose.defaultMainGenerateCabal
defaultMainGenerateCabal :: IO ()
defaultMainGenerateCabal = do
  defaultCabalWriter <- defaultWriteGeneratedCabal
  defaultMainWithHooksGenerateCabal defaultCabalWriter Distribution.Simple.simpleUserHooks

-- | If you have already using custom 'UserHooks' use this in your Setup.hs's 'main' and also
-- provide a way to generate an explicit Cabal file.
--
-- > import qualified Distribution.Simple.AutoExpose as AutoExpose
-- > main = do
-- >   cabalWriter <- defaultWriteGeneratedCabal
-- >   AutoExpose.defaultMainWithHooksGenerateCabal cabalWriter myHooks
defaultMainWithHooksGenerateCabal :: WriteGeneratedCabal -> UserHooks -> IO ()
defaultMainWithHooksGenerateCabal writeGeneratedCabal uhs =
  Distribution.Simple.defaultMainWithHooks (autoExposeHooks (Just writeGeneratedCabal) uhs)

-- | A datatype that wraps a function that outputs the name of the
-- explicity generated Cabal file and an absolute path to a directory
-- into which to write it.
data WriteGeneratedCabal =
  WriteGeneratedCabal
  { writeGeneratedCabalPath :: FilePath
  , writeGeneratedCabalName :: GenericPackageDescription -> FilePath
  }

-- | Write the Cabal file to the system temp directory by default using
-- 'defaultGeneratedCabalName' for the filename.
defaultWriteGeneratedCabal :: IO WriteGeneratedCabal
defaultWriteGeneratedCabal = do
  tmp <- getTemporaryDirectory
  pure (WriteGeneratedCabal tmp defaultGeneratedCabalName)

-- * Internal Functions #InternalFunctions#

-- | Search for file paths that look like valid modules and convert to the
--   components to the Cabal internal 'ModuleName'.
moduleNamesToExpose
  :: [String] -- ^ File extensions of valid Haskell modules, includes pre-processor extensions
  -> [FilePath] -- ^ File paths to search
  -> [ModuleName]
moduleNamesToExpose extensions =
  sort
  . map (fromString . intercalate ".")
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
  :: HasCallStack
  => FilePath -- ^ Directory to search (path must be absolute)
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

-- | Captures the detected Haskell modules and signatures in a library
data ExposedLib =
  ExposedLib
  { exposedLibModules :: [ModuleName]
  , exposedLibSignatures :: [ModuleName]
  }
  deriving Show

-- | All the exposed library components, main library and sub libraries
data AllExposed =
  AllExposed
  { allExposedMainLib :: ExposedLib
  , allExposedSubLibs :: [(Library,ExposedLib)]
  }
  deriving Show

-- | The common pieces of 'GenericPackageDescription' and 'PackageDescription'
-- which we need to auto detect Haskell modules /and/ signatures. We can't make
-- do with just the 'PackageDescription' because the 'confHook' which
-- instantiates the library with signatures only has access to
-- 'GenericPackageDescription'.
data PackageDescriptionSubset =
  PackageDescriptionSubset
  { packageDescriptionSubsetLibrary :: Maybe Library
  , packageDescriptionSubsetSubLibraries :: [Library]
  , packageDescriptionSubsetExecutables :: [Executable]
  , packageDescriptionSubsetTestSuites :: [TestSuite]
  , packageDescriptionSubsetBenchmarks :: [Benchmark]
  }
  deriving Show

genericPackageDescriptionToSubset :: GenericPackageDescription -> PackageDescriptionSubset
genericPackageDescriptionToSubset gpd =
  PackageDescriptionSubset
    (fmap condTreeData (condLibrary gpd))
    (map (condTreeData . snd) (condSubLibraries gpd))
    (map (condTreeData . snd) (condExecutables gpd))
    (map (condTreeData . snd) (condTestSuites gpd))
    (map (condTreeData . snd) (condBenchmarks gpd))

packageDescriptionToSubset :: PackageDescription -> PackageDescriptionSubset
packageDescriptionToSubset pd =
  PackageDescriptionSubset
    (library pd)
    (subLibraries pd)
    (executables pd)
    (testSuites pd)
    (benchmarks pd)

-- | Source directories for all non-library components (benchmarks, executables
-- etc.) so that we can exclude their modules if they appear inside the library's
-- source directory
nonLibraryHsSourcePaths :: PackageDescriptionSubset -> [[FilePath]]
nonLibraryHsSourcePaths pds =
  map hsSourceDirs $
     (map buildInfo (packageDescriptionSubsetExecutables pds))
  ++ (map testBuildInfo (packageDescriptionSubsetTestSuites pds))
  ++ (map benchmarkBuildInfo (packageDescriptionSubsetBenchmarks pds))

-- | Associate each item in a list will it's left and right elements, eg.
-- > indexWithNeighbors [1,2,3,4] == [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
--
-- Used to associate a source directory with possibly inner directories that
-- should be ignored with searching for Haskell modules.
indexWithNeighbors :: [a] -> [(a,[a])]
indexWithNeighbors as =
  unfoldr
    (\case
        (ll:lls,x:rs) -> Just ((x,ll++rs),(lls,rs))
        _ -> Nothing
    )
    (inits as, as)

-- | Drill into the source trees for a component and find modules
-- excluding the source trees for other components
getExposedModules
  :: HasCallStack
  => [String] -- ^ Known Haskell and/or preprocessor extensions
  -> [FilePath] -- ^ The source trees for this component
  -> [[FilePath]] -- ^ The source trees for all the other components
  -> IO [ModuleName] -- ^ Detect modules in this component
getExposedModules exts hsSrcDirs otherHsSrcDirs = do
  absHsSrcDirs <- mapM makeAbsolute hsSrcDirs
  absOtherHsSrcDirs <- mapM makeAbsolute (concat otherHsSrcDirs)
  contents <-
    concat
    <$> ( mapM (\(srcDir,excludeDirs) -> getDirectoryContents srcDir excludeDirs)
          $ map (\(srcDir,otherSrcDirs) -> (srcDir,otherSrcDirs ++ absOtherHsSrcDirs))
          $ indexWithNeighbors absHsSrcDirs
        )
  pure $ moduleNamesToExpose exts contents

-- | Get all the exposed modules and signatures in this project's main and sub libraries
getAllExposed
  :: HasCallStack
  => PackageDescriptionSubset
  -> [String] -- ^ Custom preprocessor extensions
  -> IO AllExposed
getAllExposed pds customPPExts =
  case (packageDescriptionSubsetLibrary pds) of
    Nothing -> do
      let mainLib = ExposedLib [] []
      subLibs <- subLibsExposed []
      pure (AllExposed mainLib subLibs)
    Just l -> do
      mainLib <- mainLibExposed l
      subLibs <- subLibsExposed (libSrcDir l)
      pure (AllExposed mainLib subLibs)
  where
    ppExts :: [String]
    ppExts = nub (customPPExts ++ map fst knownSuffixHandlers)
    libSrcDir :: Library -> [FilePath]
    libSrcDir = nub . hsSourceDirs . libBuildInfo
    mainLibExcludedPaths :: [[FilePath]]
    mainLibExcludedPaths =
      (map snd subLibSrcDirs) ++ (nonLibraryHsSourcePaths pds)
    mainLibExposed :: Library -> IO ExposedLib
    mainLibExposed l = do
      exposedMods <- getExposedModules (sourceExtensions ++ ppExts) (libSrcDir l) mainLibExcludedPaths
      exposedSigs <- getExposedModules hsigExtensions (libSrcDir l) mainLibExcludedPaths
      pure (ExposedLib exposedMods exposedSigs)
    subLibSrcDirs :: [(Library,[FilePath])]
    subLibSrcDirs = zip (packageDescriptionSubsetSubLibraries pds) (map libSrcDir (packageDescriptionSubsetSubLibraries pds))
    subLibSrcDirsWithExcludedPaths :: [FilePath] -> [(Library, ([FilePath], [[FilePath]]))]
    subLibSrcDirsWithExcludedPaths mainLibSrcs =
      map (\((subLib,subLibSrcs),otherSubLibs) ->
             let excluded = (map snd otherSubLibs) ++ (nonLibraryHsSourcePaths pds) ++ [mainLibSrcs]
             in (subLib, (subLibSrcs,excluded))
          )
          (indexWithNeighbors subLibSrcDirs)
    subLibsExposed :: [FilePath] -> IO [(Library,ExposedLib)]
    subLibsExposed mainLibSrcs =
      mapM (\(subLib,(subLibSrcs,excluded)) -> do
               exposedMods <- getExposedModules (sourceExtensions ++ ppExts) subLibSrcs excluded
               exposedSigs <- getExposedModules hsigExtensions subLibSrcs excluded
               pure (subLib, ExposedLib exposedMods exposedSigs)
           )
           (subLibSrcDirsWithExcludedPaths mainLibSrcs)

-- | Since the @hs-source-dirs@ fields in a @.cabal@ file take a source tree
-- path relative to the @.cabal@ file itself we need to make sure the current
-- working directory in which to search for module detection is the directory in
-- which the @.cabal@ file resides.
withCabalFileDirectoryCwd
  :: HasCallStack
  => Maybe FilePath -- ^ Absolute path to the directory containing a '.cabal' file, current directory if absent
  -> IO a -- ^ The IO action that auto detects modules & signatures
  -> IO a
withCabalFileDirectoryCwd projectPathM action = do
  let pp = fromMaybe "." projectPathM
  cabalFilePath <- findPackageDesc pp
  case cabalFilePath of
    Left err -> error err
    Right _ -> withCurrentDirectory pp action

-- | Update the exposed modules and signatures of a 'Library'
updateLibrary :: Library -> ExposedLib -> Library
updateLibrary lib exposedLib =
  lib & (L.exposedModules %~ (nub . (++) (exposedLibModules exposedLib)))
      . (L.signatures %~ (nub . (++) (exposedLibSignatures exposedLib)))

-- | Update the 'PackageDescription' of this package to include auto detected
-- library modules. Also just to be nice fill in the 'Paths_...' module in
-- 'otherModules' field of the library's 'BuildInfo'.
updatePackageDescription :: HasCallStack => PackageDescription -> UserHooks -> IO PackageDescription
updatePackageDescription pd uhs =
  withCabalFileDirectoryCwd Nothing $ do
    (AllExposed exposedLib exposedSubLibs) <-
      getAllExposed (packageDescriptionToSubset pd) (map fst (hookedPreProcessors uhs))
    let newMainLibrary l =
          (updateLibrary l exposedLib) &
            (L.libBuildInfo . L.otherModules %~ (nub . (++) [(autogenPathsModuleName pd)]))
    pure (pd { library = fmap newMainLibrary (library pd)
             , subLibraries = map (uncurry updateLibrary) exposedSubLibs
             })

-- | Update the 'GenericPackageDescription' of this package so the library can
-- be properly instantiated with Backpack signatures at configure time when the
-- 'confHook' is run.
updateGenericPackageDescription ::
  HasCallStack
  => Maybe FilePath -- ^ Absolute path of the directory which contains the '.cabal' file, in other words the root of the project
  -> GenericPackageDescription
  -> [String] -- ^ Custom preprocessor extensions
  -> IO GenericPackageDescription
updateGenericPackageDescription projectPath gpd ppExts =
  let updateCondTreeLib exposedLib condLib =
        condLib { condTreeData = updateLibrary (condTreeData condLib) exposedLib  }
  in withCabalFileDirectoryCwd projectPath $ do
      (AllExposed exposedLib exposedSubLibs) <-
        getAllExposed (genericPackageDescriptionToSubset gpd) ppExts
      pure $
        gpd { condLibrary = fmap (updateCondTreeLib exposedLib) (condLibrary gpd)
            , condSubLibraries =
                map (\((unqualName,condSubLib),subLib) -> (unqualName,condSubLib { condTreeData = subLib }))
                    (zip (condSubLibraries gpd) (map (uncurry updateLibrary) exposedSubLibs))
            }

-- | The default name to use when generating an explicit Cabal file
-- It defaults to @<package-name>-<package-version>-generated.cabal@
defaultGeneratedCabalName  :: GenericPackageDescription -> FilePath
defaultGeneratedCabalName gpd =
  let gpdPkg = package . packageDescription
  in (prettyShow (pkgName (gpdPkg gpd)))
     ++ "-"
     ++ (prettyShow (pkgVersion (gpdPkg gpd)))
     ++ "-generated"
     <.> "cabal"

-- | In service of the CLI app, read the ~.cabal~ file into a 'GenericPackageDescription'
cabalFileToGenericPackageDescription
  :: FilePath -- ^ An absolute path to the directory containing the .cabal file
  -> IO (Either String GenericPackageDescription)
cabalFileToGenericPackageDescription fp = do
  cabalFileE <- findPackageDesc fp
  case cabalFileE of
    Left err -> pure (Left err)
    Right cabalFile -> Right <$> readGenericPackageDescription silent cabalFile

autoExposeConfHook
  :: UserHooks
  -> Maybe WriteGeneratedCabal
  -> (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
autoExposeConfHook userHooks writeGeneratedCabalM (gpd,hbi) cfs = do
  newGpd <- updateGenericPackageDescription (Just ".") gpd (map fst (hookedPreProcessors userHooks))
  whenJust writeGeneratedCabalM
    (\(WriteGeneratedCabal outputDir generatedCabalName) -> do
        let f = outputDir </> (generatedCabalName newGpd)
        notice (fromFlag (configVerbosity cfs)) ("Writing generated Cabal file: " ++ f)
        writeGenericPackageDescription f newGpd
    )
  (confHook userHooks) (newGpd,hbi) cfs

-- | Modify a set of 'UserHooks' so that all relevant hooks see a
-- 'PackageDescription' or 'GenericPackageDescription' with auto detected
-- modules and signatures filled in.
--
-- Also optionally write an explicit Cabal file at 'confHook' time.
autoExposeHooks :: Maybe WriteGeneratedCabal -> UserHooks -> UserHooks
autoExposeHooks writeGeneratedCabalM userHooks =
  userHooks
  { confHook = autoExposeConfHook userHooks writeGeneratedCabalM
  , buildHook = bh
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

-- | The supported Haskell source extensions, currently 'hs' and 'lhs'
sourceExtensions :: [String]
sourceExtensions = ["hs","lhs"]

-- | Backpack signature extensions, currently 'hsig' and 'lhsig'
hsigExtensions :: [String]
hsigExtensions = ["hsig","lhsig"]
