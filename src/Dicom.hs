{-# LANGUAGE OverloadedStrings, RankNTypes #-}


module Dicom where

import Prelude hiding (FilePath)

import Data.Either
import Data.Char (toLower)
import Data.List
import Data.Maybe
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Trans.Writer.Lazy ()
import Control.Proxy
import Control.Proxy.Trans.Writer
import Data.Function (on)
import Data.Ord (comparing)
import Data.Functor.Identity
import Data.List (isInfixOf, groupBy, isSuffixOf)
import System.Directory
import System.FilePath
import System.FilePath.Glob
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Parsec

import System.IO.Temp
import System.Posix.Files
import System.FilePath
import System.Process
import System.Exit (ExitCode(..))

import System.Unix.Directory (removeRecursiveSafely)

import Utils (runShellCommand)

-- http://stackoverflow.com/a/7233657/3659845
unescapeEntities :: String -> String
unescapeEntities [] = []
unescapeEntities ('&':xs) =
  let (b, a) = break (== ';') xs in
  case (lookupEntity b, a) of
    (Just c, ';':as) ->  c ++ unescapeEntities as
    _                -> '&' : unescapeEntities xs
unescapeEntities (x:xs) = x : unescapeEntities xs


-- Note on execWriterT/raiseK: http://ocharles.org.uk/blog/posts/2012-12-16-24-days-of-hackage-pipes.html
getRecursiveContentsList :: FilePath -> IO [FilePath]
getRecursiveContentsList path =
    runProxy $ execWriterK $ (getRecursiveContents path) >-> toListD
  where
    -- http://stackoverflow.com/questions/14259229/streaming-recursive-descent-of-a-directory-in-haskell/14261710#14261710
    -- getRecursiveContents :: (Proxy p) => FilePath -> () -> Producer p FilePath IO ()
    getRecursiveContents topPath () = do -- runIdentityP $ do
      properNames <- fmap (filter (`notElem` [".", ".."])) (lift $ getDirectoryContents topPath)
      forM_ properNames $ \name -> do
        let path = topPath </> name
        isDirectory <- lift $ doesDirectoryExist path
        if isDirectory
          then getRecursiveContents path ()
          else respond path

dcmDump :: FilePath -> IO (Either String String)
dcmDump fileName = runShellCommand "dcmdump" ["+Qn", fileName]

globDcmFiles :: FilePath -> IO ([[FilePath]], [FilePath])
globDcmFiles = globDirWith (matchDefault { ignoreCase = True }) [compile "*.dcm"]

createLinksDirectory :: FilePath -> IO FilePath
createLinksDirectory dicomDir = concat . fst <$> globDcmFiles dicomDir >>= createLinksDirectoryFromList

createLinksDirectoryFromList :: [FilePath] -> IO FilePath
createLinksDirectoryFromList dicomFiles = do
    tempDir <- createTempDirectory "/tmp" "dicomConversion"

    forM_ dicomFiles $ \f -> do
        sourceName <- canonicalizePath f
        createSymbolicLink sourceName $ tempDir </> (takeFileName f)

    return tempDir

dicomToMinc :: [FilePath] -> IO (Either String [FilePath])
dicomToMinc dicomFiles = do
    dicomDir' <- createLinksDirectoryFromList dicomFiles
    outputDir <- createTempDirectory "/tmp" "dcm2mnc"

    result <- runShellCommand "dcm2mnc" [dicomDir', outputDir]

    removeRecursiveSafely dicomDir'

    case result of Right result' -> Right <$> getRecursiveContentsList outputDir
                   Left e        -> return $ Left e

mncToMnc2 :: FilePath -> IO (Either String FilePath)
mncToMnc2 filePath = do
    tmpFile <- fst <$> openTempFile "/tmp" "mincto2.mnc"

    result <- runShellCommand "mincconvert" ["-2", "-clobber", filePath, tmpFile]

    case result of Right _ -> do renameFile tmpFile filePath
                                 return $ Right filePath
                   Left e  -> error e -- return $ Left e

createMincThumbnail :: FilePath -> IO (Either String FilePath)
createMincThumbnail mncFile = do
    let mincThumbnail = mncFile ++ ".png"

    result <- runShellCommand "mincpik" [mncFile, mincThumbnail]

    case result of Right _ -> return $ Right mincThumbnail
                   Left e  -> return $ Left e

parseField :: forall u. String -> ParsecT String u Identity String
parseField prefix = do
    _ <- string prefix

    _ <- string " ["

    field <- many (noneOf "]")

    _ <- char ']'

    _ <- many anyChar

    return field


pPatientName                = parseField "(0010,0010) PN"
pPatientID                  = parseField "(0010,0020) LO"
pPatientBirthDate           = parseField "(0010,0030) DA"
pPatientSex                 = parseField "(0010,0040) CS"
pPatientAge                 = parseField "(0010,1010) AS"
pPatientWeight              = parseField "(0010,1030) DS"
pPatientPosition            = parseField "(0018,5100) CS"

pStudyDate                  = parseField "(0008,0020) DA"
pStudyTime                  = parseField "(0008,0030) TM"
pStudyDescription           = parseField "(0008,1030) LO"
pStudyInstanceID            = parseField "(0020,000d) UI"
pStudyID                    = parseField "(0020,0010) SH"

pSeriesDate                 = parseField "(0008,0021) DA"
pSeriesTime                 = parseField "(0008,0031) TM"
pSeriesDescription          = parseField "(0008,103e) LO"
pSeriesInstanceUID          = parseField "(0020,000e) UI"
pSeriesNumber               = parseField "(0020,0011) IS"
pCSASeriesHeaderType        = parseField "(0029,1018) CS"
pCSASeriesHeaderVersion     = parseField "(0029,1019) LO"
pCSASeriesHeaderInfo        = parseField "(0029,1020) OB"
pSeriesWorkflowStatus       = parseField "(0029,1160) LO"

pMediaStorageSOPInstanceUID = parseField "(0002,0003) UI"
pInstanceCreationDate       = parseField "(0008,0012) DA"
pInstanceCreationTime       = parseField "(0008,0013) TM"
pSOPInstanceUID             = parseField "(0008,0018) UI"
pStudyInstanceUID           = parseField "(0020,000d) UI"
pInstanceNumber             = parseField "(0020,0013) IS"

pInstitutionName             = parseField "(0008,0080) LO"
pInstitutionAddress          = parseField "(0008,0081) ST"
pInstitutionalDepartmentName = parseField "(0008,1040) LO"

pReferringPhysicianName      = parseField "(0008,0090) PN"

parseSingleMatch :: ParsecT String () Identity String -> String -> Maybe String
parseSingleMatch p s = case parses of
                            ["(no value available)"] -> Nothing
                            [match]                  -> Just match
                            _                        -> Nothing
  where
    parses = rights $ map (parse p "(parseSingleMatch)") (lines s)

data DicomFile = DicomFile
    { dicomFilePath                   :: FilePath
    , dicomPatientName                :: Maybe String
    , dicomPatientID                  :: Maybe String
    , dicomPatientBirthDate           :: Maybe String
    , dicomPatientSex                 :: Maybe String
    , dicomPatientAge                 :: Maybe String
    , dicomPatientWeight              :: Maybe String
    , dicomPatientPosition            :: Maybe String

    , dicomStudyDate                  :: Maybe String
    , dicomStudyTime                  :: Maybe String
    , dicomStudyDescription           :: Maybe String
    , dicomStudyInstanceID            :: Maybe String
    , dicomStudyID                    :: Maybe String

    , dicomSeriesDate                 :: Maybe String
    , dicomSeriesTime                 :: Maybe String
    , dicomSeriesDescription          :: Maybe String
    , dicomSeriesInstanceUID          :: Maybe String
    , dicomSeriesNumber               :: Maybe String
    , dicomCSASeriesHeaderType        :: Maybe String
    , dicomCSASeriesHeaderVersion     :: Maybe String
    , dicomCSASeriesHeaderInfo        :: Maybe String
    , dicomSeriesWorkflowStatus       :: Maybe String

    , dicomMediaStorageSOPInstanceUID :: Maybe String
    , dicomInstanceCreationDate       :: Maybe String
    , dicomInstanceCreationTime       :: Maybe String
    , dicomSOPInstanceUID             :: Maybe String
    , dicomStudyInstanceUID           :: Maybe String
    , dicomInstanceNumber             :: Maybe String


    , dicomInstitutionName              :: Maybe String
    , dicomInstitutionAddress           :: Maybe String
    , dicomInstitutionalDepartmentName  :: Maybe String

    , dicomReferringPhysicianName       :: Maybe String
    }
    deriving (Eq, Show)

readDicomMetadata :: FilePath -> IO (Either String DicomFile)
readDicomMetadata fileName = do
    dump <- dcmDump fileName

    case dump of
        Left e      -> return $ Left e
        Right dump' -> let parseHere p = (unescapeEntities <$> parseSingleMatch p dump') in
                            return $ Right $ DicomFile
                                    fileName
                                    (parseHere pPatientName)
                                    (parseHere pPatientID)
                                    (parseHere pPatientBirthDate)
                                    (parseHere pPatientSex)
                                    (parseHere pPatientAge)
                                    (parseHere pPatientWeight)
                                    (parseHere pPatientPosition)

                                    (parseHere pStudyDate)
                                    (parseHere pStudyTime)
                                    (parseHere pStudyDescription)
                                    (parseHere pStudyInstanceID)
                                    (parseHere pStudyID)

                                    (parseHere pSeriesDate)
                                    (parseHere pSeriesTime)
                                    (parseHere pSeriesDescription)
                                    (parseHere pSeriesInstanceUID)
                                    (parseHere pSeriesNumber)
                                    (parseHere pCSASeriesHeaderType)
                                    (parseHere pCSASeriesHeaderVersion)
                                    (parseHere pCSASeriesHeaderInfo)
                                    (parseHere pSeriesWorkflowStatus)

                                    (parseHere pMediaStorageSOPInstanceUID)
                                    (parseHere pInstanceCreationDate)
                                    (parseHere pInstanceCreationTime)
                                    (parseHere pSOPInstanceUID)
                                    (parseHere pStudyInstanceUID)
                                    (parseHere pInstanceNumber)

                                    (parseHere pInstitutionName)
                                    (parseHere pInstitutionAddress)
                                    (parseHere pInstitutionalDepartmentName)

                                    (parseHere pReferringPhysicianName)

getDicomFilesInDirectory :: String -> FilePath -> IO [FilePath]
getDicomFilesInDirectory suffix dir = filter (isLowerSuffix suffix) <$> getFilesInDirectory dir
  where
    isLowerSuffix a b = map toLower a `isSuffixOf ` map toLower b

    getFilesInDirectory :: FilePath -> IO [FilePath]
    getFilesInDirectory d = map (d </>) <$> filter (not . (`elem` [".", ".."])) <$> getDirectoryContents d >>= filterM doesFileExist

{-
groupDicomFilesByPatientID files = groupBy ((==) `on` dicomPatientID) files

groupDicomFilesByStudyAndSeries :: [DicomFile] -> [[DicomFile]]
groupDicomFilesByStudyAndSeries files = groupBy f files
  where
    f d1 d2 = dicomStudyDescription  d1 == dicomStudyDescription  d2 &&
              dicomSeriesDescription d1 == dicomSeriesDescription d2
-}

group2 :: [DicomFile] -> [[DicomFile]]
group2 files = fmap (map snd) $ groupBy ((==) `on` fst) (sortOn fst $ map toTuple files)

  where

    toTuple file = ((dicomPatientID file, dicomStudyDescription file, dicomSeriesDescription file), file)

    -- https://ghc.haskell.org/trac/ghc/ticket/9004
    sortOn :: Ord b => (a -> b) -> [a] -> [a]
    sortOn f = map snd . sortBy (comparing fst)
                       . map (\x -> let y = f x in y `seq` (y, x))



