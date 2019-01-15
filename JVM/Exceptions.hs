{-# Language GADTs #-}

module JVM.Exceptions where

import           Control.Monad.Except (Except, runExcept)
import qualified Data.ByteString.Lazy     as B

import           JVM.ClassFile

data GeneratorException where
    NoItemInPool :: Show a => a -> GeneratorException
    NoMethodUnderConstruction :: GeneratorException
    UnexpectedEndMethod :: GeneratorException
    ClassFileNotLoaded :: FilePath -> GeneratorException
    JARNotLoaded :: FilePath -> String -> GeneratorException
    ClassNotFound :: String -> GeneratorException
    FieldNotFound :: String -> B.ByteString -> GeneratorException
    MethodNotFound :: String -> B.ByteString -> GeneratorException
    OtherError :: String -> GeneratorException

instance Show GeneratorException where
  show (NoItemInPool s) = "Internal error: no such item in pool: <" <> show s <> ">"
  show UnexpectedEndMethod = "endMethod without startMethod!"
  show NoMethodUnderConstruction = "No method under construction"
  show (ClassFileNotLoaded p) = "Class file was not loaded: " <> p
  show (JARNotLoaded p c) = "Class was not loaded from JAR: " <> p <> ": " <> c
  show (ClassNotFound p)    = "No such class in ClassPath: " <> p
  show (FieldNotFound c f)  = "No such field in class " <> c <> ": " <> toString f
  show (MethodNotFound c m) = "No such method in class " <> c <> ": " <> toString m
  show (OtherError e) = show e

force :: Show e => String -> Except e a -> a
force s x =
  case runExcept x of
    Left  exc    -> error $ "Exception at " <> s <> ": " <> show exc
    Right result -> result
