module Main where

import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Options.Applicative
import Text.Printf (printf)

-- Local modules
import Checksum
import Dicom
import Mytardis
import MytardisRest
import Types


myTardisDefaultDir = "." -- FIXME move somewhere else?

data Command
    = CmdUploadAll       UploadAllOptions
    | CmdUploadOne       UploadOneOptions
    | CmdShowExperiments ShowExperimentsOptions
    deriving (Eq, Show)

data UploadAllOptions = UploadAllOptions { uploadAllDryRun :: Bool } deriving (Eq, Show)
data UploadOneOptions = UploadOneOptions { uploadOneHash :: String } deriving (Eq, Show)

data ShowExperimentsOptions = ShowExperimentsOptions { showFileSets :: Bool } deriving (Eq, Show)

data UploaderOptions = UploaderOptions
    { optDirectory  :: Maybe FilePath
    , optHost       :: Maybe String
    , optUser       :: Maybe String
    , optCommand    :: Command
    }
    deriving (Eq, Show)

pUploadAllOptions :: Parser Command
pUploadAllOptions = CmdUploadAll <$> UploadAllOptions <$> switch (long "dry-run" <> help "Dry run.")

pUploadOneOptions :: Parser Command
pUploadOneOptions = CmdUploadOne <$> UploadOneOptions <$> strOption (long "hash" <> help "Hash of experiment to upload.")

pShowExprOptions :: Parser Command
pShowExprOptions = CmdShowExperiments <$> ShowExperimentsOptions <$> switch (long "show-file-sets" <> help "Show experiments.")

pUploaderOptions :: Parser UploaderOptions
pUploaderOptions = UploaderOptions
    <$> optional (strOption (long "dir" <> metavar "DIRECTORY" <> help "Directory with DICOM files."))
    <*> optional (strOption (long "host" <> metavar "HOST" <> help "MyTARDIS host URL, e.g. http://localhost:8000"))
    <*> optional (strOption (long "user" <> metavar "USERNAME" <> help "MyTARDIS username."))
    <*> subparser x
  where
    x    = cmd1 <> cmd2 <> cmd3
    cmd1 = command "upload-all"       (info (helper <*> pUploadAllOptions) (progDesc "Upload all experiments."))
    cmd2 = command "upload-one"       (info (helper <*> pUploadOneOptions) (progDesc "Upload a single experiment."))
    cmd3 = command "show-experiments" (info (helper <*> pShowExprOptions)  (progDesc "Show local experiments."))

askDicomDir :: UploaderOptions -> FilePath
askDicomDir opts = fromMaybe "." (optDirectory opts)

hashFiles :: [FilePath] -> String
hashFiles = sha256 . unwords

dostuff :: UploaderOptions -> ReaderT MyTardisConfig IO ()

dostuff opts@(UploaderOptions _ _ _ (CmdShowExperiments cmdShow)) = do
    let dir = askDicomDir opts

    _files <- liftIO $ rights <$> (getDicomFilesInDirectory ".dcm" dir >>= mapM readDicomMetadata)
    let groups = group2 _files

    forM_ groups $ \files -> do
        let
            IdentifiedExperiment desc institution title metadata = identifyExperiment files
            hash = (sha256 . unwords) (map dicomFilePath files)

        liftIO $ if showFileSets cmdShow
            then printf "%s [%s] [%s] [%s] [%s]\n" hash institution desc title (unwords $ map dicomFilePath files)
            else printf "%s [%s] [%s] [%s]\n"      hash institution desc title

dostuff opts@(UploaderOptions _ _ _ (CmdUploadAll allOpts)) = uploadDicomAsMinc (askDicomDir opts)

dostuff opts@(UploaderOptions _ _ _ (CmdUploadOne oneOpts)) = do
    let hash = uploadOneHash oneOpts

    let dir = askDicomDir opts

    _files <- liftIO $ rights <$> (getDicomFilesInDirectory ".dcm" dir >>= mapM readDicomMetadata)
    let groups = group2 _files

    let
        hashes = map (hashFiles . fmap dicomFilePath) groups :: [String]
        matches = filter ((==) hash . snd) (zip groups hashes) :: [([DicomFile], String)]

    case matches of [match] -> liftIO $ print match
                    []      -> liftIO $ putStrLn "Hash does not match any identified experiment."
                    _       -> error "Multiple experiments with the same hash. Oh noes!"

main :: IO ()
main = do
    opts' <- execParser opts

    let
        host = fromMaybe "http://localhost:8000" $ optUser opts'
        user = fromMaybe "admin"                 $ optHost opts'
        pass = "admin" -- FIXME Fail on no password

        mytardisOpts = (defaultMyTardisOptions host user pass)

    runReaderT (dostuff opts') mytardisOpts

  where

    opts = info (helper <*> pUploaderOptions ) (fullDesc <> header "mytardis-dicom - upload DICOM files to a MyTARDIS server" )

testtmp = flip runReaderT (defaultMyTardisOptions "http://localhost:8000" "admin" "admin") blah
  where
    blah :: ReaderT MyTardisConfig IO ()
    blah = do
        {-
        let dir = "/tmp/dicomdump"

        _files <- liftIO $ rights <$> (getDicomFilesInDirectory ".dcm" dir >>= mapM readDicomMetadata)
        let files = head $ group2 _files

        x <- getExperimentWithMetadata (identifyExperiment files)

        liftIO $ print x
        -}

        d <- getDatasets

        liftIO $ print d
