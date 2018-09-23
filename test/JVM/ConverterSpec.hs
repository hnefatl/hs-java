module JVM.ConverterSpec where

import Data.Binary
import Data.Functor (void)
import System.FilePath
import Test.Hspec
import JVM.Converter

import JVM.ClassFile
import Java.JAR.Archive

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B

import JVM.Dump

import qualified Codec.Archive.Zip as Zip

import System.Directory (createDirectoryIfMissing)

failedTests = "test" </> "resources" </> "converterspec" </> "fail"

shouldMatchDecodeEncode :: FilePath -> B.ByteString  -> IO ()
shouldMatchDecodeEncode pth input =
  do
    let clsDir = parseClass input
    let encoded = encodeClass clsDir
    if input /= encoded
      then
        do
          print "class"
          print (show clsDir)
          print "===================="
          dumpClass clsDir
          let outErrPth = (failedTests </> pth)
          _ <- createDirectoryIfMissing True (takeDirectory outErrPth)
          B.writeFile outErrPth encoded
      else
        return ()
    input `shouldBe` encoded

shouldMatchDecodeEncodeFile :: FilePath  -> IO ()
shouldMatchDecodeEncodeFile pth =
  do
    input <- B.readFile pth
    shouldMatchDecodeEncode pth input

shouldMatchDecodeEncodeJar :: FilePath -> IO ()
shouldMatchDecodeEncodeJar pth = do
  pths <- Zip.withArchive pth $ archivePaths
  void $ sequence $ fetchContentFrom <$> pths
  where
    fetchContentFrom jarEntryPth = do
      cont <- readJAREntry pth jarEntryPth
      case cont of
        Nothing -> error $ "cant find entry " ++ (show jarEntryPth) ++ " in jar " ++ (show pth)
        (Just c) -> do
          print $ "checking " ++ (show jarEntryPth)
          shouldMatchDecodeEncode jarEntryPth (B.pack $ BS.unpack c)

testClass path =
  it path $ do
    shouldMatchDecodeEncodeFile $ root </> path
  where
    root = "test" </> "resources" </> "converterspec"

testJar jar =
  it jar $ do
    shouldMatchDecodeEncodeJar $ jar

rtJar = "/" </> "java" </> "jdk1.6.0_45" </> "jre" </> "lib" </> "rt.jar"

spec :: Spec
spec = do
  describe "encode decode class" $ do
    testClass "Hello.class"
    testClass "Bicycle.class"
    testClass "ACMEBicycle.class"
    testClass "StaticInit.class"
    testClass "InnerClass.class"
    testClass "Rectangle.class"
    testClass "Rectangle$Point.class"
    testClass "AnonInsideMethod.class"
    testClass "AnonInsideMethod$1.class"
  -- describe "encode decode jar" $ do
  --   testJar rtJar