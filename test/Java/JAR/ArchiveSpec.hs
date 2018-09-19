module Java.JAR.ArchiveSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>), takeDirectory)
import Java.JAR.Archive
import qualified Codec.Archive.Zip as Zip
import Java.ClassPath.Types (Tree(..))
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)

withTempFile :: FilePath -> IO () -> IO ()
withTempFile pth action = do
  _ <- createDirectoryIfMissing True (takeDirectory pth)
  _ <- action
  fileExists <- doesFileExist pth
  when fileExists (removeFile pth)

mkArchive :: FilePath -> [Tree (FilePath, BS.ByteString)] -> IO ()
mkArchive archiveName cont = Zip.createArchive archiveName $ zipJAREntries cont

spec :: Spec
spec =
  let archiveName = "test" </> "resources" </> "archivespec" </> "zip.jar"
      zipContent1 = [
          Directory ("dir1") [
              File ("aa", BS.pack "hola"),
              File ("bb", BS.pack "blah blah")
            ]
        ]
      zipContent2 = [
          Directory ("dir1") [
            File ("aa", BS.pack "hola"),
            File ("bb", BS.pack "blah blah")
          ],
          Directory ("dir2") [
            Directory ("dir3") [
              File ("cc.aaa", BS.pack "haskell!!!")
            ]
          ]
        ]
      entries cont = do
        _ <- mkArchive archiveName cont
        Zip.withArchive archiveName $ archivePaths
      getContent cont pth = do
        _ <- mkArchive archiveName cont
        readJAREntry archiveName pth
  in
  do around_ (withTempFile archiveName) $ do
      describe "zipJar" $ do
        it "must inlude all entries path" $ do
          (entries zipContent1) >>= (`shouldBe` ["dir1" </> "aa", "dir1" </> "bb"])
          (entries zipContent2) >>= (`shouldBe`
            ["dir1" </> "aa", "dir1" </> "bb", "dir2" </> "dir3" </> "cc.aaa"] )
        it "must inlude all content" $ do
          (getContent zipContent2 ("dir2" </> "dir3" </> "cc.aaa")) >>= ( `shouldBe`
            (Just (BS.pack "haskell!!!")) )