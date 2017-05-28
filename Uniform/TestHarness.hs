-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TestHarness
--
-- | two functions to deal wtih tests which
-- store data on disk
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
--{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.TestHarness (module Uniform.TestHarness
--    , module Uniform.Strings
    , module Uniform.Error
--    , module Safe
--    , module Control.Monad.Error  -- is monads-tf
--    , module Control.Exception   -- to avoid control.error
    , module Test.Framework

        )  where

--import           "monads-tf" Control.Monad.Error
import           Safe
import           Test.Framework
--import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.FileIO
import Uniform.Error

testDataDir = makeAbsDir  "/home/frank/Workspace8/LitTextWorkGeras/LitTextWork/TestData"


testVar2File :: (Eq b, Show b, Read b) => a -> FilePath -> (a-> ErrIO b) -> IO ()
-- ^ a text harness for the case that the start is a value (not a file)
testVar2File  a resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    t1 <- runErr $  op a
    case t1 of
        Left msg -> do
                    putIOwords ["test testVar2File", s2t resfile]
                    assertBool False
        Right tt1 -> do
--                    putIOwords ["the text result (for next) \n", showT tt1]
--                    putIOwords ["the text result   \n",   tt1]
--                    assertEqual result1B tt1
                let fn = testDataDir </> (resfile) :: Path Abs File
                let fnx = testDataDir </> ("x" ++ resfile  ) :: Path Abs File
                fnexist <- doesFileExist fn
                f1 <- if fnexist then readFile  (toFilePath fn)
                            else return zero
                let testres = (readNote zero f1) == tt1
                unless testres $
                    writeFile (toFilePath fnx )  (show tt1)
                assertBool testres

testFile2File :: (Read a, Eq b, Show b, Read b, Zeros b) => FilePath -> FilePath -> (a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File  startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    f0 <- readFile (toFilePath fn0)

    let tt1 =  op (readNote startfile f0)
    let fn = testDataDir </> resfile  :: Path Abs File
    let fnx = testDataDir </> ("x" ++ resfile ) :: Path Abs File
    fnexist <- doesFileExist fn
    f1 <- if fnexist then readFile  (toFilePath fn)
                else return zero
    let testres =  (readDef zero f1) == tt1
    unless testres $
            writeFile (toFilePath fnx )  (show tt1)
    assertBool testres

testFile3File :: (Read a, Eq b, Show b, Read b, Zeros b) =>
        base -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile3File  base startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    f0 <- readFile (toFilePath fn0)

    let tt1 =  op base (readNote startfile f0)
    let fn = testDataDir </> resfile  :: Path Abs File
    let fnx = testDataDir </> ("x" ++ resfile ) :: Path Abs File
    fnexist <- doesFileExist fn
    f1 <- if fnexist then readFile  (toFilePath fn)
                else return zero
    let testres =  (readDef zero f1) == tt1
    unless testres $
            writeFile (toFilePath fnx )  (show tt1)
    assertBool testres

