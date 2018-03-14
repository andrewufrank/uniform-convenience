-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TestHarness
--
-- | two functions to deal wtih tests which
-- store data on disk

-- attention: the test result throws an exception HUnit.NN (caused by assertBool)

-----------------------------------------------------------Utils.hs------------------
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


module Uniform.Test.TestHarness (module Uniform.Test.TestHarness
    , module Uniform.Error
    , module Test.Framework
    , ShowTestHarness (..)
    , FilePath

        )  where

import           Safe
import           Test.Framework
import Uniform.FileIO
import Uniform.Error  hiding ((</>), (<.>)) -- to allow export
import Uniform.Test.Utils

testvardebug = True -- False

testVar1File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => a -> FilePath -> (a-> ErrIO b) -> IO ()
-- ^ a text harness for the case that the start is a value (not a file)
testVar1File  a resfile op = do
    when testvardebug $ putIOwords ["testVar1File read text for ", s2t resfile]
    t1 <- runErr $  op a
    case t1 of
        Left msg -> do
                    when testvardebug $ putIOwords ["test testVar1File", s2t resfile]
                    assertBool False
        Right tt1 -> do
--                    putIOwords ["the text result (for next) \n", showT tt1]
--                    putIOwords ["the text result   \n",   tt1]
--                    assertEqual result1B tt1
                testDataDir <- getLitTextTestDir
                r <- runErr $ checkResult testvardebug testDataDir resfile tt1
                assertBool (r == Right True)

testFile2File :: (Read a, Eq b, Show b, Read b, Zeros b, ShowTestHarness b, ShowTestHarness a)
            => FilePath -> FilePath -> (a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File  startfile resfile op = do
----    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    f0 <- readStartFile testvardebug testDataDir startfile
--    let f1 = removeChar '\n' f0
    let tt1 =  op    (readTestH2 "read start sfadsd" $ f0)  -- this is just a conversion to type a
    r <- runErr $ checkResult testvardebug testDataDir resfile tt1
    assertBool (r == Right True)

testVar3File :: (Read a, Eq b, Show b, Read b
            , Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        base -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3File  base startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    f0 <- readStartFile testvardebug testDataDir startfile

    let tt1 =  op base (readTestH2 startfile f0)
    r <- runErr $ checkResult testvardebug testDataDir resfile tt1
    assertBool (r == Right True)

test3File :: (Read base, Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness base, ShowTestHarness a, ShowTestHarness b) =>
        FilePath -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test3File  basefile startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    base0 <- readStartFile testvardebug testDataDir basefile
    f0 <- readStartFile testvardebug testDataDir startfile
    let base = readTestH2 "test3file readbase wer2" $ base0

    let tt1 =  op base (readTestH2 startfile f0)
    r <- runErr $ checkResult testvardebug testDataDir resfile tt1
    assertBool (r == Right True)

test2FileIO :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
          FilePath -> FilePath -> (  a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test2FileIO   startfile resfile op = do
    putIOwords ["read text for ", s2t startfile]
    testDataDir <- getLitTextTestDir
    f0 <- readStartFile testvardebug testDataDir startfile

    t1 <- runErr $ op    (readTestH2 startfile f0)
    r <- runErr $ checkResultIOop testvardebug  testDataDir resfile t1
    assertBool (r == Right True)

--    putIOwords ["test3 testVar2FileIO", "result", showT t1]
--    case t1 of
--        Left msg -> do
----                    when testvardebug $
--                    putIOwords ["test3 Left testVar2FileIO\n"
--                     , "possibly only the resultfile file not existing - create by hand"
--                                , s2t resfile, "\n", showT f0, "\n"
--                                , msg, "."]
----                    assertBool False
--        Right tt1 -> do
--                checkResult testvardebug testDataDir resfile tt1

testVar2FileIO :: (Exception String, Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        base -> FilePath -> FilePath -> (base -> a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar2FileIO  base startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    f0 <- readStartFile testvardebug testDataDir startfile

    t1 <-  runErr $ op base (read f0)
    r <- runErr $ checkResultIOop testvardebug  testDataDir resfile t1
    assertBool (r == Right True)
--    when True $ -- testvardebug $
--        putIOwords ["test3 testVar3FileIO B", "result",  showT t1]
--    case t1 of
--        Left msg -> do
----                    when testvardebug $
--                    putIOwords ["test3 Left testVar3FileIO\n"
--                     , "resultFile:", s2t resfile, "inputFile:", showT startfile
--                     , "possibly only the resultfile not existing - create by hand"
--                       , "\nMessage:", msg, "."]
----                    assertBool False
--        Right tt1 -> do
--                putIOwords ["test3 testVar3FileIO C check result"]
--                r <- checkResult testvardebug testDataDir resfile tt1
--                putIOwords ["test3 testVar3FileIO C check result gives", showT r ]
--                return r



