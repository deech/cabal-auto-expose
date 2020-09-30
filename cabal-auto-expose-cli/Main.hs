{-# LANGUAGE OverloadedStrings #-}
module Main where
import Options.Applicative
import System.FilePath
import System.Directory
import Control.Exception.Base
import Control.Monad
import System.IO.Error
import Distribution.PackageDescription.PrettyPrint
import Distribution.Simple.AutoExpose

cli :: Parser FilePath
cli = strOption ( long "projectPath"
                  <> metavar "PROJECTPATH"
                  <> help "Path to the Haskell project. Defaults to the current directory if omitted."
                  <> value "."
                  <> showDefault
                )

cabalFileString :: FilePath -> IO String
cabalFileString projectPath = do
  ppE <- try checkProjectPath
  case ppE of
    Left e | isPermissionError e -> throwIO (ioeSetErrorString e permsError)
           | isDoesNotExistError e -> throwIO (ioeSetErrorString e notExistsError)
           | otherwise -> throwIO e
    Right absPP -> do
      gpdE <- cabalFileToGenericPackageDescription absPP
      case gpdE of
        Left _ -> throwIO cabalFileNotFound
        Right gpd -> showGenericPackageDescription <$> updateGenericPackageDescription (Just absPP) gpd []
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
