{-# LANGUAGE OverloadedStrings, FlexibleContexts, Rank2Types #-}

-- import Control.Exception
import qualified Data.ByteString.Lazy as B

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import Java.ClassPath

import qualified Java.Lang
import qualified Java.IO

import Control.Exception.Safe.Checked
import Control.Monad.State

test ::(Throws ENotFound,
        Throws ENotLoaded,
        Throws UnexpectedEndMethod,
        MonadThrow m,
        MonadIO m,
        MonadState GState m) => m ()
test = do
  withClassPath $ do
      -- Add current directory (with Hello.class) to ClassPath
      addDirectory "."

  -- Load method signature: Hello.hello() from Hello.class
  helloJava <- getClassMethod "./Hello" "hello"

  -- Initializer method. Just calls java.lang.Object.<init>
  newMethod [ACC_PUBLIC] "<init>" [] ReturnsVoid $ do
      setStackSize 1

      aload_ I0
      invokeSpecial Java.Lang.object Java.Lang.objectInit
      i0 RETURN

  -- Declare hello() method and bind it's signature to hello.
  hello <- newMethod [ACC_PUBLIC, ACC_STATIC] "hello" [IntType] ReturnsVoid $ do
      setStackSize 8

      getStaticField Java.Lang.system Java.IO.out
      loadString "Здравствуй, мир!"
      invokeVirtual Java.IO.printStream Java.IO.println
      getStaticField Java.Lang.system Java.IO.out
      loadString "Argument: %d\n"
      iconst_1
      allocArray Java.Lang.object
      dup
      iconst_0
      iload_ I0
      invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
      aastore
      invokeVirtual Java.IO.printStream Java.IO.printf
      -- Call Hello.hello()
      invokeStatic "Hello" helloJava
      pop
      i0 RETURN

  -- Main class method.
  newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
      setStackSize 1

      iconst_5
      -- Call previously declared method
      invokeStatic "Test" hello
      i0 RETURN

  return ()

raiseErr :: Show e => e -> a
raiseErr e = error (show e)

catchENotFound :: MonadCatch m => (Throws ENotFound => m a) -> m a
catchENotFound ma = catch ma (raiseErr :: ENotFound -> a)

catchENotLoaded :: MonadCatch m => (Throws ENotLoaded => m a) -> m a
catchENotLoaded ma = catch ma (raiseErr :: ENotLoaded -> a)

catchUnexpectedEndMethod :: MonadCatch m => (Throws UnexpectedEndMethod => m a) -> m a
catchUnexpectedEndMethod ma = catch ma (raiseErr :: UnexpectedEndMethod -> a)

main :: IO ()
main = do
  testClass <- generateIO [] "Test" $ catchENotFound $ catchENotLoaded $ catchUnexpectedEndMethod test
  B.writeFile "Test.class" (encodeClass testClass)
