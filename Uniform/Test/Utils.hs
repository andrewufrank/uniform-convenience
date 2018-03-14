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


module Uniform.Test.Utils (module Uniform.Test.Utils
    , module Uniform.FileIO


        )  where

--import           "monads-tf" Control.Monad.Error
import           Safe
import           Test.Framework
--import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.FileIO
import Text.Show.Pretty
--import Uniform.Error
--import qualified Path  as Path  (Path (..))
import qualified Path.IO as Path.IO (doesFileExist, getAppUserDataDir)
        -- necessary for operations in IO
--import Text.Read (readEither)

-- operations are in IO not ErrIO, therefore here and not in fileio
getLitTextTestDir :: IO (Path Abs Dir)
getLitTextTestDir = fmap Path $ Path.IO.getAppUserDataDir "LitTextTest"

doesFileExistWrapped :: Path Abs File -> IO Bool
doesFileExistWrapped fn = Path.IO.doesFileExist (unPath fn)

readStartFile :: Bool -> Path Abs Dir -> FilePath -> IO String
-- ^ read the start file as string
readStartFile testvardebug  testDataDir startfile = do
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test2a testFile2File filename input ", showT fn0]
    f0 :: String <- readFile (toFilePath fn0)
    return f0

checkResultIOop :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Bool -> Path Abs Dir -> FilePath -> Either Text b -> IO ()
-- ^ check the result when it may be an error
checkResultIOop testvardebug  testDataDir resfile t1  = do
    when True $ -- testvardebug $
        putIOwords ["test3 testVar3FileIO B", "result",  showT t1]
    case t1 of
        Left msg -> do
--                    when testvardebug $
                    putIOwords ["test3 Left testVar3FileIO\n"
                     , "resultFile:", s2t resfile
                     , "possibly only the resultfile not existing - create by hand"
                       , "\nMessage:", msg, "."]
--                    assertBool False
        Right tt1 -> do
                putIOwords ["test3 testVar3FileIO C check result"]
                r <- checkResult testvardebug testDataDir resfile tt1
                putIOwords ["test3 testVar3FileIO C check result gives", showT r ]
                return r

checkResult :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Bool ->  Path Abs Dir -> FilePath -> b -> IO ()
checkResult testvardebug testDataDir resfile tt1 = do
        let fn = testDataDir </> resfile :: Path Abs File
        let fnx = testDataDir </> ("x" ++ resfile  ) :: Path Abs File
        when testvardebug $
            putIOwords ["checkResult test", s2t resfile, showT fn]
        fnexist <- doesFileExistWrapped fn
--        let result = showTestH tt1
        if fnexist   -- issue : how to deal with "" files which do not have a parse?
            then do
                r0 :: String <- readFile  (toFilePath fn)
                if null' r0
                  then  do
                    putIOwords ["checkResult t9 file exists but is null", showT fn]
                    writeFile (toFilePath fn )  $ showTestH tt1
                    putIOwords ["checkResult t10 file written", s2t . take 100 $ showTestH tt1]
                    assertBool True  -- no control, assume ok
                  else do
                    let r1m = (readTestH2 "checkResult read result file" r0) `asTypeOf` (Just tt1)
                    r1 <- case r1m of
                        Nothing -> do
                            putIOwords ["checkResult t11 file does not parse", showT fn]
                            writeFile (toFilePath fnx )  $ showTestH tt1
                            putIOwords ["checkResult t10 file written", s2t . take 100 $ showTestH tt1]
                            assertBool True  -- no control, assume ok
                            return $ undef "wewer return t11 checkResult"
                        Just r1 -> return r1
                    when True $ -- testvardebug $
                        putIOwords ["test3 checkResult resultFile:", s2t resfile
                        , "\ninputFile content read\n", showT r1]

                    --        let f1cont = readDef zero f1
                    --        when testvardebug $ putIOwords ["test3a exprected result (raw)", s2t f1]
                    --        when testvardebug $ putIOwords ["test3a exprected result (content)", show' f1cont]
                    let testres = r1 == tt1
                    --                unless (testres && testvardebug) $ do
                    when testvardebug  $ do
                        putIOwords ["checkResult test3a  "
                                , showT testres, "\n", showT tt1]
                        putIOwords ["checkResult test3a  expected file"
                                        , show' fn, "contains\n", showT r1]
                    unless testres $ do
                        when testvardebug  $ do
                            putIOwords ["checkResult test4  - no previous file existing"
                                , " - write NEW result"
                                    , showT testres, "\n", showT tt1]
                        writeFile (toFilePath fnx )  $ showTestH tt1
                    assertBool testres
            else do
                writeFile (toFilePath fn )  $ showTestH tt1
                assertBool True  -- no control, assume ok
                    -- when file does not exist, then write it
----            else  return zero
----        if not . null' $ f1
----            then do
--        let f1cont = readDef zero f1
--        when testvardebug $ putIOwords ["test3a exprected result (raw)", s2t f1]
--        when testvardebug $ putIOwords ["test3a exprected result (content)", show' f1cont]

class ShowTestHarness t where
    showTestH :: Show t =>  t -> String
    -- used for the writing to the files
    showTestH = ppShow
    readTestH :: Read t => String -> t
    -- all reads from file are with readTestH2
    readTestH = readNote "showTestHarness t"
    readTestH2 :: Read t => String -> String -> Maybe t
    readTestH2 msg = readMay
--                readNote msg
--                either (throwErrorT) id $ readEither ("readTestH2 no parse " <> msg)

--instance (Show t , ShowTestHarness t) => ShowTestHarness [t] where
--
--    showTestH t =   ppShow t


instance  ShowTestHarness Text where
    -- to avoid the additional "" added when show text
    -- but the read must compensate!
    -- this is necessary that json files (and other with "") can be read
    showTestH = t2s
    readTestH = readNote "showTestHarness Text" . show
--    readTestH2 msg = readNote (  msg) . show
--
instance  ShowTestHarness String where
    -- to avoid the additional "" added when show text
    showTestH = id
    readTestH = readNote "showTestHarness String " -- . show
--    readTestH2 msg = readNote (  msg) . show
--
--instance  ShowTestHarness () where
--    showTestH = show
--    readTestH = readNote "showTestHarness bottom () " -- . show
----    readTestH2 msg = readNote (  msg) . show
--
--instance ShowTestHarness Int where
--    showTestH = show
--    readTestH = readNote "showTestHarness Int " -- . show
----    readTestH2 msg = readNote (  msg) . show

--instance  ShowTestHarness t where
----    showTestH tx@(a:as)= show tx
----    showTestH tx = show tx


--readEitherError :: Read a => Text -> Text -> Either Text a
--readEitherError msg t = either (throwErrorT . (msg <>) . s2t) s2t  $ readEither . t2s $ t

