-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TestHarness
--
-- | two functions to deal wtih tests which
-- store data on disk
 -- interface must be in the wrapped Path, to allow the reading ??
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
--import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.FileIO
--import Uniform.Error
--import qualified Path  as Path  (Path (..))
import qualified Path.IO as Path.IO (doesFileExist, getAppUserDataDir)
        -- necessary for operations in IO

-- operations are in IO not ErrIO, therefore here and not in fileio
getLitTextTestDir :: IO (Path Abs Dir)
getLitTextTestDir = fmap Path $ Path.IO.getAppUserDataDir "LitTextTest"

doesFileExistWrapped :: Path Abs File -> IO Bool
doesFileExistWrapped fn = Path.IO.doesFileExist (unPath fn)

checkResult :: (Zeros b, Eq b, Show b, Read b, Zeros b, ShowTestHarness b)
            => Bool ->  Path Abs Dir -> FilePath -> b -> IO ()
checkResult testvardebug testDataDir resfile tt1 = do
        let fn = testDataDir </> resfile :: Path Abs File
        let fnx = testDataDir </> ("x" ++ resfile  ) :: Path Abs File
        when testvardebug $
            putIOwords ["checkResult test testVar2File", s2t resfile, showT fn]
        fnexist <- doesFileExistWrapped fn
        let result = showTestH tt1
        f1 <- if fnexist
            then  readFile  (toFilePath fn)
            else  return ""
        if not . null' $ f1
            then do
        --        let f1cont = readDef zero f1
        --        when testvardebug $ putIOwords ["test3a exprected result (raw)", s2t f1]
        --        when testvardebug $ putIOwords ["test3a exprected result (content)", show' f1cont]
                let testres = f1 == result
--                unless (testres && testvardebug) $ do
                when testvardebug  $ do
                        putIOwords ["checkResult test3a testVar3FileIO ", showT testres, "\n", showT result]
                        putIOwords ["checkResult test3a testVar3FileIO expected file"
                                        , show' fn, "contains\n", showT f1]
                unless testres $
                    writeFile (toFilePath fnx )  result
                assertBool testres
--                assertEqual f1  result
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
    readTestH :: Read t => String -> t
    readTestH = readNote "showTestHarness t"
    readTestH2 :: Read t => String -> String -> t
    readTestH2 msg = readNote msg

instance Show t => ShowTestHarness [t] where
    showTestH t@(a:as) = concat (["["] ++  map showTestH2 (init t) ++ [show (last t), "]"])

      where
        showTestH2 t =  show t ++ ",\n"

instance  ShowTestHarness Text where
    -- to avoid the additional "" added when show text
    showTestH = t2s
    readTestH = readNote "showTestHarness Text" . show
    readTestH2 msg = readNote (  msg) . show
instance  ShowTestHarness String where
    -- to avoid the additional "" added when show text
    showTestH = id
    readTestH = readNote "showTestHarness String ". show
    readTestH2 msg = readNote (  msg) . show

instance  ShowTestHarness () where

--instance  ShowTestHarness t where
----    showTestH tx@(a:as)= show tx
----    showTestH tx = show tx


