-- | This module defines functions to read Java JAR files.
module Java.JAR.Archive where

import qualified Codec.Archive.Zip as Zip
import Data.Binary
import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import System.FilePath

import Java.ClassPath.Types
import Java.ClassPath.Common
import JVM.ClassFile
import JVM.Converter

import qualified Control.Monad.Catch as C
import qualified Data.Map as M

import Data.Functor (void)

readJAREntry :: FilePath -> String -> IO (Maybe BS.ByteString)
readJAREntry jarfile path = do
  pth <- Zip.mkEntrySelector path
  C.catch (Just `fmap` (Zip.withArchive jarfile $ Zip.getEntry pth)) handleZipException
  where
    handleZipException :: Zip.ZipException -> IO (Maybe BS.ByteString)
    handleZipException _ = return Nothing

archivePaths :: Zip.ZipArchive [FilePath]
archivePaths = (Zip.unEntrySelector <$>) <$> (M.keys <$> Zip.getEntries)

-- | Read all entires from JAR file
readAllJAR :: FilePath -> IO [Tree CPEntry]
readAllJAR jarfile = do
    files <- Zip.withArchive jarfile $ archivePaths
    return $ mapF (NotLoadedJAR jarfile) (buildTree $ filter good files)
  where
    good file = ".class" `isSuffixOf` file

-- | Read one class from JAR file
readFromJAR :: FilePath -> FilePath -> IO (Class Direct)
readFromJAR jarfile path = do
  pth <- Zip.mkEntrySelector path
  content <- Zip.withArchive jarfile $ Zip.getEntry pth
  let bstr = B.pack $ BS.unpack content
  return $ classFile2Direct (decode bstr)

checkClassTree :: [Tree CPEntry] -> IO [Tree (FilePath, Class Direct)]
checkClassTree forest = mapFMF check forest
  where
    check _ (NotLoaded path) = do
       cls <- parseClassFile path
       return (path, cls)
    check a (Loaded path cls) = return (a </> path, cls)
    check a (NotLoadedJAR jar path) = do
       cls <- readFromJAR jar (a </> path)
       return (a </> path, cls)
    check a (LoadedJAR _ cls) =
       return (a </> show (thisClass cls), cls)

-- zipJAR :: [Tree (FilePath, Class Direct)] -> Zip.ZipArchive ()
-- zipJAR forest = do
--     mapFM go forest
--     return ()
--   where
--     go (path, cls) = Zip.addFile path =<< Zip.sourceBuffer (B.unpack $ encodeClass cls)

zipJAR :: [Tree (FilePath, Class Direct)] -> Zip.ZipArchive ()
zipJAR trees =
  void $ traverse addEntry (trees >>= (treeToEntries mempty))
  where
    addEntry (f, a) = do
      pth <- Zip.mkEntrySelector f
      let cont = BS.pack $ B.unpack a
      Zip.addEntry Zip.Store cont pth

    treeToEntries folder (File (fileName, cls)) = [(folder </> fileName, encodeClass cls)]
    treeToEntries folder (Directory folderName contents) = (treeToEntries (folder </> folderName)) =<< contents

