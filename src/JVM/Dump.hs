{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module JVM.Dump where

import Data.List

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Printf

import JVM.Common ()
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Attributes.InnerClasses

import qualified Control.Monad.State as S

putLn :: S.MonadState [String] m => String -> m ()
putLn txt = do
  lns <- S.get
  S.put (lns ++ [txt])

putLnB :: S.MonadState [String] m => B.ByteString -> m ()
putLnB txt = putLn $ B.unpack txt

putLnS :: Show a => S.MonadState [String] m => a -> m ()
putLnS txt = putLn $ show txt

dumpClassText :: S.MonadState [String] m => Class Direct -> m ()
dumpClassText cls = do
    putLn "Class: "
    putLnB (thisClass cls)
    putLn "Constants pool:"
    forM_ (M.assocs $ constsPool cls) $ \(i, c) ->
      putLn $ printf "  #%d:\t%s" i (show c)
    putLn "Methods:"
    forM_ (classMethods cls) $ \m -> do
      putLn ">> Method "
      putLnB (methodName m)
      putLnS (methodSignature m)
      case attrByName m "Code" of
        Nothing -> putLn "(no code)\n"
        Just bytecode -> let code = decodeMethod bytecode
                          in  forM_ (codeInstructions code) $ \i -> do
                                putLn "  "
                                putLnS i

    case arlookup "InnerClasses" (classAttributes cls) of
      Nothing -> return ()
      Just bytecode -> let classes = decodeInnerClasses bytecode
                        in  forM_ (icsClasses classes) $ \i -> do
                              putLn ">> InnerClasses "
                              putLn $
                                (intercalate " " (show <$> (innerClassAccessFlag i))) ++
                                " #" ++
                                (show $ innerNameIndex i) ++
                                "= #" ++
                                (show $ innerClassInfoIndex i) ++
                                " of #"
                                ++ (show $ outerClassInfoIndex i)

-- | Dump a class to console.
dumpClass :: Class Direct -> IO ()
dumpClass cls =
  do
    let cont = S.execState (dumpClassText cls) []
    putStrLn (intercalate "\n" cont)
{-     putStr "Class: "
    B.putStrLn (thisClass cls)
    putStrLn "Constants pool:"
    forM_ (M.assocs $ constsPool cls) $ \(i, c) ->
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

    case arlookup "InnerClasses" (classAttributes cls) of
      Nothing -> return ()
      Just bytecode -> let classes = decodeInnerClasses bytecode
                        in  forM_ (icsClasses classes) $ \i -> do
                              putStr ">> InnerClasses "
                              putStrLn $
                                (intercalate " " (show <$> (innerClassAccessFlag i))) ++
                                " #" ++
                                (show $ innerNameIndex i) ++
                                "= #" ++
                                (show $ innerClassInfoIndex i) ++
                                " of #"
                                ++ (show $ outerClassInfoIndex i) -}


