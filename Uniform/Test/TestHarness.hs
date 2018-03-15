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
            => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
-- ^ a text harness for the case that the start is a value (not a file)
-- the progname gives the hidden directory of the test results (i.e. ~/.progName)
testVar1File progName  a resfile op = do
    when testvardebug $ putIOwords ["testVar1File read text "
                , "with expected result ", showT resfile]
    r <- runErr $  testVar1File' progName  a resfile op
    assertBool (r == Right True)

testVar1File' :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Text -> a -> FilePath -> (a-> ErrIO b) -> ErrIO Bool
-- ^ a text harness for the case that the start is a value (not a file)
-- the progname gives the hidden directory of the test results (i.e. ~/.progName)
testVar1File' progName  a resfile op = do
    t1 <- op a
    testDataDir <- getLitTextTestDir3 progName
    checkResult testvardebug testDataDir resfile t1


testFile2File :: (Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness b, ShowTestHarness a)
            => Text -> FilePath -> FilePath -> (a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File  progName startfile resfile op = do
    when testvardebug $ putIOwords ["testFile2File read text for "
        , showT startfile, "with expected result ", showT resfile]
    r <- runErr $  testFile2File' progName startfile resfile op
    assertBool (r == Right True)


testFile2File' :: (Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness b, ShowTestHarness a)
            => Text -> FilePath -> FilePath -> (a->   b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File'  progName startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile
--    let f1 = removeChar '\n' f0
    let tt1 =  op    (readTestH2 "read start sfadsd" $ f0)
        -- this is just a conversion to type a
    checkResult testvardebug testDataDir resfile tt1

testVar3File :: (Read a, Eq b, Show b, Read b
            , Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3File progName base startfile resfile op = do
    when testvardebug $ putIOwords ["testVar3File read text for "
        , showT startfile, "with expected result ", showT resfile]
    r <- runErr $ testVar3File' progName base startfile resfile op
    assertBool (r == Right True)

testVar3File' :: (Read a, Eq b, Show b, Read b
            , Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a->   b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3File' progName base startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile

    let tt1 =  op base (readTestH2 startfile f0)
    checkResult testvardebug testDataDir resfile tt1

test3File :: (Read base, Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness base, ShowTestHarness a, ShowTestHarness b) =>
        Text -> FilePath -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test3File  progName basefile startfile resfile op = do
    when testvardebug $ putIOwords ["test3File read text for "
        , showT startfile, "with expected result ", showT resfile]
    r <- runErr $ test3File'  progName basefile startfile resfile op
    assertBool (r == Right True)


test3File' :: (Read base, Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness base, ShowTestHarness a, ShowTestHarness b) =>
        Text -> FilePath -> FilePath -> FilePath -> (base -> a->   b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test3File'  progName basefile startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    base0 <- readStartFile3 testvardebug testDataDir basefile
    f0 <- readStartFile3 testvardebug testDataDir startfile
    let base = readTestH2 "test3file readbase wer2" $ base0

    let tt1 =  op base (readTestH2 startfile f0)
    checkResult testvardebug testDataDir resfile tt1


test2FileIO :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
          Text -> FilePath -> FilePath -> (  a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test2FileIO  progName startfile resfile op = do
    when testvardebug $ putIOwords ["test2FileIO read text for ", s2t startfile]
    r <- runErr $ test2FileIO'  progName startfile resfile op
    assertBool (r == Right True)


test2FileIO' :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
          Text -> FilePath -> FilePath -> (  a-> ErrIO  b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test2FileIO'  progName startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile

    t1 <- op    (readTestH2 startfile f0)
    checkResult testvardebug  testDataDir resfile t1


testVar2FileIO :: (Exception String, Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar2FileIO progName base startfile resfile op = do
    when testvardebug $ putIOwords ["testVar2FileIO read text for ", s2t startfile]
    r <- runErr $ testVar2FileIO' progName base startfile resfile op
    assertBool (r == Right True)



testVar2FileIO' :: (Exception String, Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a-> ErrIO  b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar2FileIO' progName base startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile

    t1 <- op base (read f0)
    checkResult testvardebug  testDataDir resfile t1



