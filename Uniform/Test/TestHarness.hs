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

--import           "monads-tf" Control.Monad.Error
import           Safe
import           Test.Framework
--import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.FileIO
import Uniform.Error  hiding ((</>), (<.>)) -- to allow export
import Uniform.Test.Utils
--import qualified Path.IO as Path.IO (getAppUserDataDir)
        -- necessary for operations in IO

--initializeTestDataDir :: ErrIO (Path Abs Dir)
--initializeTestDataDir =   getAppUserDataDir "LitTextTest"
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
                checkResult testvardebug testDataDir resfile tt1

testFile2File :: (Read a, Eq b, Show b, Read b, Zeros b, ShowTestHarness b, ShowTestHarness a)
            => FilePath -> FilePath -> (a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File  startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test2a testFile2File filenames start ", showT fn0
            , "result file", s2t resfile]
    f0 :: String <- readFile (toFilePath fn0)
--    let f1 = removeChar '\n' f0
    let tt1 =  op    (readTestH $ f0)  -- this is just a conversion to type a
    checkResult testvardebug testDataDir resfile tt1

testVar3File :: (Read a, Eq b, Show b, Read b
            , Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        base -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3File  base startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test2a testVar3File filenames start ", showT fn0
            , "result file", s2t resfile]
    f0 <- readFile (toFilePath fn0)

    let tt1 =  op base (readTestH2 startfile f0)
    checkResult testvardebug testDataDir resfile tt1

test3File :: (Read base, Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness base, ShowTestHarness a, ShowTestHarness b) =>
        FilePath -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test3File  basefile startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    let fbase = testDataDir </> basefile :: Path Abs File
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test2a testVar3File filenames start ", showT fn0
            , "result file", s2t resfile]
    base0 <- readFile (toFilePath fbase)
    let base = readTestH2 "test3file readbase wer2" $ base0
    f0 <- readFile (toFilePath fn0)

    let tt1 =  op base (readTestH2 startfile f0)
    checkResult testvardebug testDataDir resfile tt1

test2FileIO :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
         FilePath -> FilePath -> (  a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test2FileIO   startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when True $ -- testvardebug $
        putIOwords ["test3 testVar2FileIO", "resultFile:", s2t resfile
                , "inputFile:", showT fn0]
    f0 <- readFile (toFilePath fn0)

    t1 <- runErr $ op   (readTestH2 startfile f0)
    putIOwords ["test3 testVar2FileIO", "result", showT t1]
    case t1 of
        Left msg -> do
--                    when testvardebug $
                    putIOwords ["test3 Left testVar2FileIO\n"
                     , "possibly only the resultfile file not existing - create by hand"
                                , s2t resfile, "\n", showT f0, "\n"
                                , msg, "."]
--                    assertBool False
        Right tt1 -> do
                checkResult testvardebug testDataDir resfile tt1

testVar2FileIO :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        base -> FilePath -> FilePath -> (base -> a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar2FileIO  base startfile resfile op = do
--    putIOwords ["read text for ", s2t . show $  textstate0]
    testDataDir <- getLitTextTestDir
    let fn0 =  testDataDir   </> startfile :: Path Abs File
--    when testvardebug $
    when True $ -- testvardebug $
        putIOwords ["test3 testVar3FileIO O", "resultFile:", s2t resfile, "inputFile:", showT fn0]
    f0 <- readFile (toFilePath fn0)
    putIOwords ["test3 testVar3FileIO A", "resultFile:", s2t resfile, "inputFile:", showT fn0]

    t1 <-  runErr $ op base (readTestH2 startfile f0)
    when True $ -- testvardebug $
        putIOwords ["test3 testVar3FileIO B", "result",  showT t1]
    case t1 of
        Left msg -> do
--                    when testvardebug $
                    putIOwords ["test3 Left testVar3FileIO\n"
                     , "resultFile:", s2t resfile, "inputFile:", showT fn0
                     , "possibly only the resultfile not existing - create by hand"
                       , "\nMessage:", msg, "."]
--                    assertBool False
        Right tt1 -> do
                putIOwords ["test3 testVar3FileIO C check result"]
                r <- checkResult testvardebug testDataDir resfile tt1
                putIOwords ["test3 testVar3FileIO C check result gives", showT r ]
                return r



