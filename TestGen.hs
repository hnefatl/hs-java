{-# LANGUAGE OverloadedStrings, FlexibleContexts, Rank2Types #-}

import qualified Data.ByteString.Lazy as B

import Control.Monad
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import Java.ClassPath

import qualified Java.Lang
import qualified Java.IO

test :: GeneratorIO ()
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
        allocNewArray Java.Lang.object
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

    switchTest <- newMethod [ACC_PUBLIC, ACC_STATIC] "switchTest" [arrayOf Java.Lang.integerClass] (Returns Java.Lang.integerClass) $ do
        let defaultGen = do
                pop
                aload_ I0
                iconst_0
                aaload
                aload_ I0
                iconst_1
                aaload
                invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
                astore_ I3
                aload_ I3
            alt = do
                pop
                aload_ I0
                iconst_1
                aaload
                aload_ I0
                iconst_2
                aaload
                invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
                astore 4
                aload 4
        aload_ I0
        iconst_1
        aaload
        invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
        astore_ I2
        getStaticField Java.Lang.system Java.IO.out
        invokeVirtual Java.IO.printStream Java.IO.printf
        invokeVirtual Java.IO.printStream Java.IO.printf
        checkCast Java.Lang.integer
        dup
        aload_ I0
        iconst_0
        aaload
        invokeVirtual Java.IO.printStream Java.IO.printf
        dup
        aload_ I0
        iconst_2
        aaload
        invokeVirtual Java.IO.printStream Java.IO.printf
        dup
        aload_ I2
        invokeVirtual Java.IO.printStream Java.IO.printf
        invokeVirtual Java.IO.printStream Java.IO.printf
        invokeVirtual Java.IO.printStream Java.IO.printf
        dup
        instanceOf Java.Lang.integer
        i0 $ IF C_EQ 3
        checkCast Java.Lang.integer
        dup
        getField Java.Lang.integer (NameType "value" IntType)
        goto 4
        iconst_m1
        lookupSwitchT defaultGen [(0, alt)]
        i0 ARETURN

    -- Main class method.
    newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
        setStackSize 1

        iconst_5
        -- Call previously declared method
        invokeStatic "Test" hello
        iconst_1
        allocNewArray Java.Lang.integer
        dup
        iconst_0
        iconst_2
        invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
        aastore
        invokeStatic "Test" switchTest
        i0 RETURN

    return ()

main :: IO ()
main = do
    testClass <- snd <$> generateIO [] "Test" test
    B.writeFile "Test.class" (encodeClass testClass)