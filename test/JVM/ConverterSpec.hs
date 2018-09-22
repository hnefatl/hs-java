module JVM.ConverterSpec where

import Data.Binary

import System.FilePath
import Test.Hspec
import JVM.Converter

import JVM.ClassFile

import qualified Data.ByteString.Lazy as B

import JVM.Dump

shouldMatchDecodeEncode :: FilePath  -> IO ()
shouldMatchDecodeEncode pth =
  do
    input <- B.readFile pth
    -- print "input"
    -- print input
    clsDir <- parseClassFile pth
    print "class"
    print (show clsDir)
    print "===================="
    dumpClass clsDir
    let encoded = encodeClass clsDir
    -- print "encoded"
    -- print encoded
    input `shouldBe` encoded

root = "test" </> "resources" </> "converterspec"

testClass path =
  it path $ do
    shouldMatchDecodeEncode $ root </> path

spec :: Spec
spec = do
  describe "encode decode" $ do
    testClass "Hello.class"
    testClass "Bicycle.class"
    testClass "ACMEBicycle.class"
    testClass "StaticInit.class"
    testClass "InnerClass.class"
    testClass "Rectangle.class"
    testClass "Rectangle$Point.class"