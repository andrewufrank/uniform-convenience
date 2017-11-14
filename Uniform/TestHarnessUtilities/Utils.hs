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

checkResult :: (Zeros b, Eq b, Show b, Read b, Zeros b, ShowTestHarness b)
            => Bool -> Path Abs Dir -> FilePath -> b -> IO ()
checkResult testvardebug testDataDir resfile tt1 = do
        let fn = testDataDir </> resfile :: Path Abs File
        let fnx = testDataDir </> ("x" ++ resfile  ) :: Path Abs File
        when testvardebug $
            putIOwords ["checkResult test testVar2File", s2t resfile, showT fn]
        fnexist <- doesFileExist fn
        let result = showTestH tt1
        if fnexist
            then do
                f1 <- readFile  (toFilePath fn)
        --        let f1cont = readDef zero f1
        --        when testvardebug $ putIOwords ["test3a exprected result (raw)", s2t f1]
        --        when testvardebug $ putIOwords ["test3a exprected result (content)", show' f1cont]
--                let testres = (readDef zero f1) == result
                let testres = f1 == result
--                unless (testres && testvardebug) $ do
                when testvardebug  $ do
                        putIOwords ["checkResult test3a testVar3FileIO ", showT testres, "\n", showT result]
                        putIOwords ["checkResult test3a testVar3FileIO expected file"
                                        , show' fn, "contains\n", showT f1]
                unless testres $
                    writeFile (toFilePath fnx )  result
                assertBool testres
                assertEqual f1  result
            else do
                writeFile (toFilePath fn )  result
                assertBool True  -- no control, assume ok
                    -- when file does not exist, then write it
--        let f1cont = readDef zero f1
--        when testvardebug $ putIOwords ["test3a exprected result (raw)", s2t f1]
--        when testvardebug $ putIOwords ["test3a exprected result (content)", show' f1cont]

class ShowTestHarness t where
    showTestH :: Show t =>  t -> String
    showTestH = show

instance Show t => ShowTestHarness [t] where
    showTestH t@(a:as) = concat (["["] ++  map showTestH2 (init t) ++ [show (last t), "]"])

      where
        showTestH2 t =  show t ++ ",\n"

instance  ShowTestHarness Text where
instance  ShowTestHarness () where

--instance  ShowTestHarness t where
--    showTestH tx@(a:as)= show tx
--    showTestH tx = show tx


