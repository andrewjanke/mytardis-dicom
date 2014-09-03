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


-- uploadDicomAsMinc :: ([DicomFile] -> IdentifiedExperiment) -> FilePath -> FilePath -> ReaderT MyTardisConfig IO ()
uploadDicomAsMinc identifyExperiment identifyDataset identifyDatasetFile dir processedDir = do
    -- ever get an empty list in files?
    _files <- liftIO $ rights <$> (getDicomFilesInDirectory ".dcm" dir >>= mapM readDicomMetadata)
    let groups = group2 _files

    forM_ groups $ \files -> do
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

                                        Right thumbnail <- liftIO $ createMincThumbnail f -- FIXME dangerous pattern match
                                        dsft <- uploadFileBasic identifyDatasetFile d thumbnail filemetadata

                                        liftIO $ print dsft


        -- FIXME Need to catch IO exceptions earlier...
        liftIO $ forM_ files $ \f -> do
            let f' = dicomFilePath f

            copyFile f' (processedDir </> (takeFileName f'))
            -- FIXME check exceptions
            -- FIXME check if target exists and make backup instead...
            removeFile f'
