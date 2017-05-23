--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
 {-# OPTIONS -Wall #-}


module Uniform.Convenience.LitTypes(
    module Uniform.Convenience.LitTypes
    , module Uniform.Error
    , module Uniform.Strings  
        )   where


import           Uniform.Error
import           Uniform.Strings
import Uniform.Zero

data LanguageCode = NoLanguage | German | USenglish | English    deriving (Eq, Ord, Show, Read)
instance Zeros LanguageCode where zero = NoLanguage

parseLanguageCode :: Text -> LanguageCode
parseLanguageCode "de" = German
parseLanguageCode "deu" = German
parseLanguageCode "en" = English
parseLanguageCode "xx" = NoLanguage
parseLanguageCode "xxx" = NoLanguage
parseLanguageCode c = errorT ["Extension.hs = parseLanguageCode ", c, "not found"]
