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


module Uniform.TestHarnessUtilities.Utils (module Uniform.TestHarnessUtilities.Utils
    , module Uniform.FileIO


        )  where

--import           "monads-tf" Control.Monad.Error
import           Safe
import           Test.Framework
import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.FileIO
import Uniform.Error

checkResult :: (Zeros b, Eq b, Show b, Read b, Zeros b)
            => Bool -> Path Abs Dir -> FilePath -> b -> IO ()
checkResult testvardebug testDataDir resfile tt1 = do
        let fn = testDataDir </> resfile :: Path Abs File
        let fnx = testDataDir </> ("x" ++ resfile  ) :: Path Abs File
        when testvardebug $ putIOwords ["test testVar2File", s2t resfile, showT fn]
        fnexist <- doesFileExist fn
        if fnexist
            then do
                f1 <- readFile  (toFilePath fn)
        --        let f1cont = readDef zero f1
        --        when testvardebug $ putIOwords ["test3a exprected result (raw)", s2t f1]
        --        when testvardebug $ putIOwords ["test3a exprected result (content)", show' f1cont]
                let testres = (readDef zero f1) == tt1
                unless (testres && testvardebug) $ do
                        putIOwords ["test3a testVar3FileIO failed", showT tt1]
                        putIOwords ["test3a testVar3FileIO expected file"
                                        , show' fn, "contains", showT f1]
                unless testres $
                    writeFile (toFilePath fnx )  (show tt1)
                assertBool testres
                assertEqual (readDef zero f1)  tt1
            else do
                writeFile (toFilePath fn )  (show tt1)
                assertBool True  -- no control, assume ok
                    -- when file does not exist, then write it
--        let f1cont = readDef zero f1
--        when testvardebug $ putIOwords ["test3a exprected result (raw)", s2t f1]
--        when testvardebug $ putIOwords ["test3a exprected result (content)", show' f1cont]


