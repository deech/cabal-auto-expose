{-# LANGUAGE OverloadedStrings #-}
module Main where
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.FilePath
import System.Directory
import Data.List
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils hiding(info)
import Control.Exception.Base
import Control.Monad
import System.IO.Error
import Distribution.Verbosity
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint
import Distribution.Simple.AutoExpose

newtype PPExt = PPExt { unPPExt :: String } deriving Show

makePPExt :: String -> Either String PPExt
makePPExt ext =
  case (concat [containsExtSeps,containsPathSeps]) of
    [] -> Right (PPExt ext)
    errs -> Left (show (linebreak <> (indent 2 . vsep . map text $ errs)))
  where
    containsExtSeps =
      if (any isExtSeparator ext)
      then ["'" ++ ext ++ "' contains the '" ++ [extSeparator] ++ "' extension separator."]
      else []
    containsPathSeps =
      case (filter (elem `flip` ext) pathSeparators) of
        [] -> []
        pss -> ["'" ++ ext ++ "' contains path separators: " ++ intersperse ',' pss]

data CabalAutoExposeConfig =
  CabalAutoExposeConfig [PPExt] FilePath
  deriving Show

cli :: Parser CabalAutoExposeConfig
cli = CabalAutoExposeConfig
  <$> many ppOption
  <*> projectPath
  where
    ppOption =
      option (eitherReader makePPExt)
        ( long "ppExt"
          <> metavar "PREPROCESSOREXTENSION"
          <> help ( "A custom preprocessor extension, for multiple extensions this option can be appear "
                    ++ "multiple times, eg. ... --ppExt=anExtension ... --ppExt=anotherExtension. "
                    ++ "Cabal already knows about: " ++ (intercalate "," (map fst knownSuffixHandlers))
                  )
        )
    projectPath =
      strOption ( long "projectPath"
                  <> metavar "PROJECTPATH"
                  <> help "Path to the Haskell project. Defaults to the current directory if omitted."
                  <> value "."
                  <> showDefault
                )

cabalFileString :: CabalAutoExposeConfig -> IO String
cabalFileString (CabalAutoExposeConfig ppExts projectPath) = do
  ppE <- try checkProjectPath
  case ppE of
    Left e | isPermissionError e -> throwIO (ioeSetErrorString e permsError)
           | isDoesNotExistError e -> throwIO (ioeSetErrorString e notExistsError)
           | otherwise -> throwIO e
    Right absPP -> do
      cabalFile <- findPackageDesc absPP >>= either (\_ -> throwIO cabalFileNotFound) pure
      gpd <- readGenericPackageDescription silent cabalFile
      showGenericPackageDescription <$> updateGenericPackageDescription (Just absPP) gpd (map unPPExt ppExts)
  where
    projectPathIsCwd = projectPath `equalFilePath` "."
    permsError =
      "The current user either cannot access or read "
      ++ (if projectPathIsCwd then "the current directory" else projectPath)
    notExistsError =
      (if projectPathIsCwd then "The current directory" else "The project path " ++ projectPath)
      ++ " does not exist"
    cabalFileNotFound =
      ioeSetErrorString
        (mkIOError doesNotExistErrorType "cabalFileString" Nothing Nothing)
        ("No '.cabal' file found in " ++ (if projectPathIsCwd then "the current directory" else projectPath))
    checkProjectPath = do
      absPath <- (makeAbsolute <=< canonicalizePath) projectPath
      perms <- getPermissions absPath
      when (not (readable perms))
        (ioError (mkIOError permissionErrorType "checkConfig" Nothing Nothing))
      pure absPath

cliDesc :: String
cliDesc =  "Generate a Cabal file from an existing one with modules and Backpack signatures explicitly exposed"
main :: IO ()
main =
  execParser (info (cli <**> helper) (fullDesc <> progDesc cliDesc)) >>=
    cabalFileString >>=
    putStr
