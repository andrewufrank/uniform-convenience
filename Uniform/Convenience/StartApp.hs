--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
 {-# OPTIONS -Wall #-}


module Uniform.Convenience.StartApp(startProg
--        , getAppConfigFile
        )   where


import           Uniform.Error
import           Uniform.Strings
--import Uniform.FileIO
--import Test.Framework

--import Control.Applicative (Applicative(..), Applicative)


startProg :: Show a => Text -> Text -> ErrIO a -> IO ()
startProg programName   progTitle mainProg = do  -- (mainProg prefsfilename gladefilename ) = do
--        putIOwords ["the files to start with \n"
--            ,"\n", "prefsfile", prefsfilename
--            , "\ngladefile", gladefilename]
        putIOwords [ "------------------ ", programName , progTitle, " ----------------------------"]
        r <- runErr $ mainProg
        putIOwords ["main", progTitle, "\nreturning", either id showT r, "\n -------------------------"]
        return ()
    `catchError` (\e  -> do
            putIOwords ["startProg error caught", programName, progTitle, showT e ] -- " showT msg])
            return ()
            )



--getAppConfigFile :: FilePath -> FilePath -> ErrIO FilePath
--getAppConfigFile programName prefsfilename =   do
--    appDataPath   <-   getAppConfigDirectory
--    putIOwords ["appDataPath is ",   appDataPath]
----    let pref = prefsfileDir </> prefsfilename
----    homeDir <- callIO $ getHomeDirectory
--    let prefdir = appDataPath </> programName
----    writeFileCreateDir  prefdir programName  (""::String) -- (homeDir </> prefsfileDir)
--    writeFile2   (prefdir </> programName)  (""::String) -- (homeDir </> prefsfileDir)
--    let prefname = appDataPath </>  programName </>  prefsfilename
--    putIOwords ["pref file name is ",   prefname]
--    return prefname


--instance (Eq x) => Zeros x where
--    zero = (undefined $ "Zeros for this type not instantiated")
-- unsafe overlap -- when defined in other modules
