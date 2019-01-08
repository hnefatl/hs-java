module Java.JAR
  (readManifest,
   readJAR,
   readMainClass,
   addJAR
  ) where

import qualified Codec.Archive.Zip as Zip

import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as St
import Data.List

import           Java.ClassPath
import           Java.JAR.Archive
import           Java.META

import qualified Data.ByteString.Char8 as B

readManifest :: Zip.ZipArchive (Maybe Manifest)
readManifest = do
  let manifestPath = "META-INF/MANIFEST.MF"
  files <- archivePaths
  if manifestPath `elem` files
    then do
         mpth <- Zip.mkEntrySelector manifestPath
         content <- Zip.getEntry mpth
         case parseMeta $ B.unpack content of
           Left e -> fail $ show e
           Right meta -> return $ Just (loadSpec meta)
    else return Nothing

readOne :: FilePath -> String -> Zip.ZipArchive [Tree CPEntry]
readOne jarfile str = do
    files <- archivePaths
    return $ mapF (NotLoadedJAR jarfile) (buildTree $ filter good files)
  where
    good name = (str `isPrefixOf` name) && (".class" `isSuffixOf` name)

-- | Read MainClass Entry of a MANIFEST.MF file
readMainClass :: FilePath -> IO (Maybe String)
readMainClass jarfile = do
  Zip.withArchive jarfile $ do
    m <- readManifest
    case m of
      Nothing -> return Nothing
      Just mf -> return $ mainClass mf

-- | Read entries from JAR file, using MANIFEST.MF if it exists.
readJAR :: FilePath -> IO [Tree CPEntry]
readJAR jarfile = do
  r <- Zip.withArchive jarfile $ do
         m <- readManifest
         case m of
           Nothing -> return Nothing
           Just mf -> do
                      trees <- mapM (readOne jarfile) (map meName $ manifestEntries mf)
                      let forest = merge (concat trees)
                      return (Just forest)
  case r of
    Nothing -> readAllJAR jarfile
    Just [] -> readAllJAR jarfile
    Just f  -> return f

-- | Add given JAR file to ClassPath
addJAR :: FilePath -> ClassPath ()
addJAR jarfile = do
  classes <- liftIO $ readJAR jarfile
  cp <- St.get
  let cp' = merge $ cp ++ classes
  St.put cp'

