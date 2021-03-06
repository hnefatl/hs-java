{-# LANGUAGE OverloadedStrings #-}
module JVM.Dump where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Bimap (assocs)
import           Text.Printf

import           JVM.Assembler
import           JVM.ClassFile
import           JVM.Common                 ()
import           JVM.Converter

-- | Dump a class to console.
dumpClass :: Class Direct -> IO ()
dumpClass cls = do
    putStr "Class: "
    B.putStrLn (thisClass cls)
    putStrLn "Constants pool:"
    forM_ (assocs $ constsPool cls) $ \(i, c) ->
      putStrLn $ printf "  #%d:\t%s" i (show c)
    putStrLn "Methods:"
    forM_ (classMethods cls) $ \m -> do
      putStr ">> Method "
      B.putStr (methodName m)
      print (methodSignature m)
      case attrByName m "Code" of
        Nothing -> putStrLn "(no code)\n"
        Just bytecode -> let code = decodeMethod bytecode
                         in  forM_ (codeInstructions code) $ \i -> do
                               putStr "  "
                               print i

