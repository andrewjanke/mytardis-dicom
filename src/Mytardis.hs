{-# LANGUAGE OverloadedStrings #-}

module Mytardis where

import Data.Aeson (Result(..))
import Data.Either
import Data.Maybe
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Maybe
import Network.Wreq
import Safe
import System.Directory
import System.FilePath
import System.Posix.Files

import qualified Data.Map as M

import Dicom
import MytardisRest
import RestTypes
import Types
import Utils

-- FIXME Testing, need to work out what these will be later.
experimentTitlePrefix = "CAI Test Experiment "
experimentDescriptionPrefix = "CAI Test Experiment Description"
datasetDescription = "CAI Dataset Description"

-- FIXME These should be in a reader or something.
schemaExperiment  = "http://cai.uq.edu.au/schema/metadata/1"
schemaDataset     = "http://cai.uq.edu.au/schema/metadata/2"
schemaDicomFile   = "http://cai.uq.edu.au/schema/metadata/3"
schemaCaiProject  = "http://cai.uq.edu.au/schema/metadata/4"


-- FIXME Has hardcoded stuff, only a single schema, , dangerous fromJusts.
identifyExperiment :: [DicomFile] -> IdentifiedExperiment
identifyExperiment files = IdentifiedExperiment -- FIXME testing, remove these fromJusts
                                description
                                (fromJust institution)
                                (fromJust title)
                                [(schema, fromJust m)]
  where
    oneFile = headMay files

    patientName       = join $ dicomPatientName       <$> oneFile
    studyDescription  = join $ dicomStudyDescription  <$> oneFile
    seriesDescription = join $ dicomSeriesDescription <$> oneFile

    -- Experiment
    title       = (experimentTitlePrefix ++) <$> patientName
    description = experimentDescriptionPrefix

    institution = join $ dicomInstitutionName <$> oneFile

    institutionalDepartmentName = join $ dicomInstitutionName       <$> oneFile
    institutionAddress          = join $ dicomInstitutionAddress    <$> oneFile
    patientID                   = join $ dicomPatientID             <$> oneFile

    -- FIXME deal with multiple schemas.
    schema = "http://cai.uq.edu.au/schema/metadata/1" -- "DICOM Experiment Metadata"
    m = case (institution, institutionalDepartmentName, institutionAddress, patientID) of
        (Just institution', Just institutionalDepartmentName', Just institutionAddress', Just patientID') -> Just $ M.fromList
                                                                                                                [ ("InstitutionName",             institution')
                                                                                                                , ("InstitutionalDepartmentName", institutionalDepartmentName')
                                                                                                                , ("InstitutionAddress",          institutionAddress')
                                                                                                                , ("PatientID",                   patientID')
                                                                                                                ]
        _ -> Nothing -- FIXME report what went wrong?


-- FIXME WHAT HAPPENS WHEN ONE DATASET BELONGS TO MULTIPLE EXPERIMENTS? DO WE HAVE
-- TO DEAL WITH THAT WHEN MATCHING DATASETS?

-- FIXME Dangerous fromJusts, need to use a Maybe return type or something.
identifyDataset :: RestExperiment -> [DicomFile] -> IdentifiedDataset
identifyDataset re files = IdentifiedDataset
                                description
                                experiments
                                m
  where
    oneFile = head files -- FIXME this will explode, use headMay instead
    description = (fromJust $ dicomStudyDescription oneFile) ++ "/" ++ (fromJust $ dicomSeriesDescription oneFile) -- FIXME fromJust
    experiments = [eiResourceURI re]
    schema      = schemaDataset
    m           = [] -- FIXME we should do some metadata for datasets! [(schema, fromJust m)]

identifyDatasetFile :: RestDataset -> String -> String -> Integer -> [(String, M.Map String String)] -> IdentifiedFile
identifyDatasetFile rds filepath md5sum size metadata = IdentifiedFile
                                        (dsiResourceURI rds)
                                        filepath
                                        md5sum
                                        size
                                        metadata

-- FIXME only for testing, get rid of this.
fromSuccess :: Result a -> a
fromSuccess (Success s) = s
fromSuccess _ = error "derp"

uploadFileBasic  :: RestDataset -> FilePath -> [(String, M.Map String String)] -> ReaderT MyTardisConfig IO (Result RestDatasetFile)
uploadFileBasic d f m = do
    meta <- liftIO $ calcFileMetadata f

    liftIO $ print ("METADATA FOR FILE", m)

    case meta of
        Just  (filepath, md5sum, size) -> do let idf = identifyDatasetFile d filepath md5sum size m

                                             dsf <- fromSuccess <$> createFileLocation idf -- FIXME fromSuccess...
                                             liftIO $ print f
                                             liftIO $ copyFileToStore f dsf

                                             return $ Success dsf
        Nothing                        -> return $ Error "FIXME1234"


uploadDicomAsMinc :: FilePath -> ReaderT MyTardisConfig IO ()
uploadDicomAsMinc dir = do
    -- ever get an empty list in files?
    _files <- liftIO $ rights <$> (getDicomFilesInDirectory ".dcm" dir >>= mapM readDicomMetadata)
    let groups = concatMap groupDicomFilesByStudyAndSeries (groupDicomFilesByPatientID _files)

    forM_ groups $ \files -> do
        -- let IdentifiedExperiment desc institution title metadata = identifyExperiment files
        let ie@(IdentifiedExperiment desc institution title metadata) = identifyExperiment files

        -- Now pack the dicom files as Minc
        dicom <- liftIO $ dicomToMinc $ map dicomFilePath files
        case dicom of
            Right mincFiles -> do _e <- createExperiment ie
                                  liftIO $ print _e

                                  let e = fromSuccess _e

                                  let ids@(IdentifiedDataset desc experiments metadata) = identifyDataset e files

                                  d <- fromSuccess <$> createDataset ids
                                  liftIO $ print d

                                  -- Convert to MINC 2.0
                                  liftIO $ forM_ mincFiles mncToMnc2 -- FIXME check results

                                  let oneFile = head files -- FIXME unsafe
                                      filemetadata = [(schemaDicomFile, M.fromList
                                                                            [ ("PatientID",          fromJust $ dicomPatientID         oneFile) -- FIXME fromJusts....
                                                                            , ("StudyInstanceUID",   fromJust $ dicomStudyInstanceUID  oneFile)
                                                                            , ("SeriesInstanceUID",  fromJust $ dicomSeriesInstanceUID oneFile)
                                                                            ]
                                                      )
                                                     ]

                                  forM_ mincFiles $ \f -> do
                                        dsf <- uploadFileBasic d f filemetadata
                                        liftIO $ print dsf

                                        Right thumbnail <- liftIO $ createMincThumbnail f -- FIXME dangerous pattern match
                                        dsft <- uploadFileBasic d thumbnail filemetadata

                                        liftIO $ print dsft
