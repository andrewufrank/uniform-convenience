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
    , module Uniform.Error
    , module Test.Framework

        )  where

--import           "monads-tf" Control.Monad.Error
import           Safe
import           Test.Framework
--import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.FileIO
import Uniform.Error
import Uniform.TestHarnessUtilities.Utils

--initializeTestDataDir :: ErrIO (Path Abs Dir)
--initializeTestDataDir =   getAppUserDataDir "LitTextTest"
testvardebug = False

testVar2File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => a -> FilePath -> (a-> ErrIO b) -> IO ()
-- ^ a text harness for the case that the start is a value (not a file)
testVar2File  a resfile op = do
    when testvardebug $ putIOwords ["testVar2File read text for ", s2t resfile]
    t1 <- runErr $  op a
    case t1 of
        Left msg -> do
                    when testvardebug $ putIOwords ["test testVar2File", s2t resfile]
                    assertBool False
        Right tt1 -> do
--                    putIOwords ["the text result (for next) \n", showT tt1]
--                    putIOwords ["the text result   \n",   tt1]
--                    assertEqual result1B tt1
                testDataDir <- getAppUserDataDir "LitTextTest"
                checkResult testvardebug testDataDir resfile tt1

testFile2File :: (Read a, Eq b, Show b, Read b, Zeros b, ShowTestHarness b)
            => FilePath -> FilePath -> (a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File  startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getAppUserDataDir "LitTextTest"
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test2a testFile2File", s2t resfile, showT fn0]
    f0 <- readFile (toFilePath fn0)
--    let f1 = removeChar '\n' f0
    let tt1 =  op (readNote startfile f0)
    checkResult testvardebug testDataDir resfile tt1

testVar3File :: (CharChains2 b Text, Read a, Eq b, Show b, Read b, Zeros b, ShowTestHarness b) =>
        base -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3File  base startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getAppUserDataDir "LitTextTest"
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test3a testVar3File", "resultFile:", s2t resfile, "inputFile:", showT fn0]
    f0 <- readFile (toFilePath fn0)

    let tt1 =  op base (readNote startfile f0)
    checkResult testvardebug testDataDir resfile tt1

testVar2FileIO :: (CharChains2 b Text, Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness b) =>
         FilePath -> FilePath -> (  a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar2FileIO   startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getAppUserDataDir "LitTextTest"
    let fn0 =  testDataDir   </> startfile :: Path Abs File
--    when testvardebug $
    putIOwords ["test3 testVar2FileIO", "resultFile:", s2t resfile, "inputFile:", showT fn0]
    f0 <- readFile (toFilePath fn0)

    t1 <- runErr $ op   (readNote startfile f0)
    putIOwords ["test3 testVar2FileIO", "result", showT t1]
    case t1 of
        Left msg -> do
--                    when testvardebug $
                    putIOwords ["test3 Left testVar2FileIO\n"
                     , "possibly only the rest file not existing - create by hand"
--                                , s2t resfile, "\n", showT f0, "\n"
                                , msg, "."]
                    assertBool False
        Right tt1 -> do
                checkResult testvardebug testDataDir resfile tt1

testVar3FileIO :: (CharChains2 b Text, Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness b) =>
        base -> FilePath -> FilePath -> (base -> a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3FileIO  base startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getAppUserDataDir "LitTextTest"
    let fn0 =  testDataDir   </> startfile :: Path Abs File
--    when testvardebug $
    putIOwords ["test3 testVar3FileIO", "resultFile:", s2t resfile, "inputFile:", showT fn0]
    f0 <- readFile (toFilePath fn0)

    t1 <-  runErr $ op base (readNote startfile f0)
    putIOwords ["test3 testVar3FileIO", "result", showT t1]
    case t1 of
        Left msg -> do
--                    when testvardebug $
                    putIOwords ["test3 Left testVar3FileIO\n"
                     , "possibly only the rest file not existing - create by hand"
--                                , s2t resfile, "\n", showT f0, "\n"
                                , msg, "."]
                    assertBool False
        Right tt1 -> do
                checkResult testvardebug testDataDir resfile tt1
