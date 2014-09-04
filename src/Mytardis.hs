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

-- FIXME WHAT HAPPENS WHEN ONE DATASET BELONGS TO MULTIPLE EXPERIMENTS? DO WE HAVE
-- TO DEAL WITH THAT WHEN MATCHING DATASETS?

-- FIXME only for testing, get rid of this.
fromSuccess :: Result a -> a
fromSuccess (Success s) = s
fromSuccess _ = error "derp"

-- uploadFileBasic  :: RestDataset -> FilePath -> [(String, M.Map String String)] -> ReaderT MyTardisConfig IO (Result RestDatasetFile)
uploadFileBasic identifyDatasetFile d f m = do
    meta <- liftIO $ calcFileMetadata f

    liftIO $ print ("METADATA FOR FILE", m)

    case meta of
        Just  (filepath, md5sum, size) -> do let idf = identifyDatasetFile d filepath md5sum size m

                                             dsf <- fromSuccess <$> createFileLocation idf -- FIXME fromSuccess...
                                             liftIO $ print f
                                             liftIO $ copyFileToStore f dsf

                                             return $ Success dsf
        Nothing                        -> return $ Error "FIXME1234"

createSchemasIfMissing :: (String, String, String) -> ReaderT MyTardisConfig IO (Result (RestSchema, RestSchema, RestSchema))
createSchemasIfMissing (schemaExperiment, schemaDataset, schemaDicomFile) = do
    schemas <- getSchemas

    case schemas of
        Error e -> return $ Error e
        Success schemas' -> do experimentSchema <- createIfMissing "DICOM Metadata Experiment" schemaExperiment SchemaExperiment  schemas'
                               datasetSchema    <- createIfMissing "DICOM Metadata Dataset"    schemaDataset    SchemaDataset     schemas'
                               fileSchema       <- createIfMissing "DICOM Metadata File"       schemaDicomFile  SchemaDatasetFile schemas'

                               return $ case (experimentSchema, datasetSchema, fileSchema) of
                                         (Success experimentSchema', Success datasetSchema', Success fileSchema') -> Success (experimentSchema', datasetSchema', fileSchema')
                                         _                                                                        -> Error "Failed to create schema (which one?)."

  where

    schemaExists  :: String -> [RestSchema] -> Bool
    schemaExists ns schemas = any ((==) ns . schemaNamespace) schemas

    -- createIfMissing :: String -> [RestSchema] -> ???
    createIfMissing name namespace stype schemas = if schemaExists namespace schemas
                                    then return $ Success $ head $ filter ((==) namespace . schemaNamespace) schemas
                                    else createSchema name namespace stype


-- uploadDicomAsMinc :: ([DicomFile] -> IdentifiedExperiment) -> FilePath -> FilePath -> ReaderT MyTardisConfig IO ()
uploadDicomAsMinc identifyExperiment identifyDataset identifyDatasetFile dir processedDir (schemaExperiment, schemaDataset, schemaDicomFile) = do

    schemas <- createSchemasIfMissing (schemaExperiment, schemaDataset, schemaDicomFile)

    -- FIXME Chuck a wobbly if the schemas aren't successfully made/found.
    -- FIXME Schema names should be configurable - and then we get to deal with name changes etc.
 
    -- ever get an empty list in files?
    _files <- liftIO $ rights <$> (getDicomFilesInDirectory ".dcm" dir >>= mapM readDicomMetadata)
    let groups = groupDicomFiles _files

    -- FIXME Just doing some defaults at the moment, dangerously
    -- assuming Success at each step.
    Success users <- getUsers
    let admin = head $ filter ((==) "admin" . ruserUsername) users -- FIXME assumes account exists...
    Success adminGroup <- getOrCreateGroup "admin"
    addGroupToUser admin adminGroup

    forM_ groups $ \files -> do
        let ie@(IdentifiedExperiment desc institution title metadata) = identifyExperiment files

        -- Now pack the dicom files as Minc
        dicom <- liftIO $ dicomToMinc $ map dicomFilePath files
        case dicom of
            Right mincFiles -> do _e <- createExperiment ie
                                  liftIO $ print _e

                                  let e = fromSuccess _e

                                  addGroupReadOnlyAccess e adminGroup

                                  let ids@(IdentifiedDataset desc experiments metadata) = identifyDataset e files

                                  d <- fromSuccess <$> createDataset ids
                                  liftIO $ print d

                                  -- Convert to MINC 2.0
                                  liftIO $ forM_ mincFiles mncToMnc2 -- FIXME check results

                                  let oneFile = head files -- FIXME unsafe
                                      filemetadata = [] -- FIXME need to pass a fn to do this
                                      {-
                                      filemetadata = [(schemaDicomFile, M.fromList
                                                                            [ ("PatientID",          fromMaybe "FIXME2 PATIENT ID"   $ dicomPatientID         oneFile)
                                                                            , ("StudyInstanceUID",   fromMaybe "FIXME2 STUDY UID"    $ dicomStudyInstanceUID  oneFile)
                                                                            , ("SeriesInstanceUID",  fromMaybe "FIXME2 INSTANCE UID" $ dicomSeriesInstanceUID oneFile)
                                                                            ]
                                                      )
                                                     ]
                                      -}
 
                                  forM_ mincFiles $ \f -> do
                                        dsf <- uploadFileBasic identifyDatasetFile d f filemetadata
                                        liftIO $ print dsf

                                        thumbnail <- liftIO $ createMincThumbnail f

                                        case thumbnail of
                                            Right thumbnail' -> do dsft <- uploadFileBasic identifyDatasetFile d thumbnail' filemetadata
                                                                   liftIO $ print dsft
                                            Left e           -> liftIO $ putStrLn $ "Error while creating thumbnail: " ++ e ++ " for file " ++ f


        -- FIXME Need to catch IO exceptions earlier...
        liftIO $ forM_ files $ \f -> do
            let f' = dicomFilePath f

            copyFile f' (processedDir </> (takeFileName f'))
            -- FIXME check exceptions
            -- FIXME check if target exists and make backup instead...
            removeFile f'

    return ()
