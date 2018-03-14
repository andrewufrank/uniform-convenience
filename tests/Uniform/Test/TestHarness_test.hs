-----------------------------------------------------------------------------
--
-- Module      :  complete the sentence in Defs0 mit lemma and a second PoS
-- Copyright   : af
--
-- conversin F -> G
-- is calling sentence by sentence for german lemmatization
-- if other lemmatization are necessary, then select different port

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables
--        , BangPatterns
            , UndecidableInstances
         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Uniform.Test.TestHarness_test   where

import           Test.Framework
import Uniform.Test.TestHarness
import Uniform.Zero

test_1   = do
    testFile2File "test.test1" "test.test2" idx
test_2   = do
    testFile2File "test.test2" "test.test3" idx

idx :: Abx -> Abx
idx = const $ Abx $ map (\i -> A2 (showT i) (showT (i+100)) i) [1..10]

data Abx = Abx [A2] deriving (Eq, Ord, Show, Read)

data A2 = A2 Text Text Int deriving (Eq, Ord, Show, Read)

instance Zeros Abx where zero = Abx zero
instance ShowTestHarness Abx where
--    showTestH (Abx as) = "Abx " ++ showTestH as

instance ShowTestHarness A2 where
--    showTestH (A2 t1 t2  i ) = "A2 " ++ showTestH as
