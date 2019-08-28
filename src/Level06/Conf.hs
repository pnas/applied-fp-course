{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text, pack , unpack)    
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Monoid                        (Last (..),
                                                     Monoid (mappend, mempty))
import           GHC.Word                 (Word16)

import           Data.Bifunctor           (first)
import           Data.Monoid              ((<>))

import           Level06.AppM             (AppM(..), liftEither)
import           Level06.Types            (Conf(..), ConfigError(..),
                                           DBFilePath (DBFilePath, getDBFilePath), PartialConf(..),
                                           Port (Port , getPort))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = 
  PartialConf (Last $ Just (Port (fromIntegral 3399))) 
              (Last $ Just ( DBFilePath "c:/tmp/level06.db"))
  -- error "defaultConf not implemented"

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = 
  let portPConf = getPort <$> ((getLast . pcPort ) pc)
      dbPathPConf = getDBFilePath <$> ((getLast . pcDBFilePath ) pc)
  in case portPConf of 
      Just nn -> case dbPathPConf of 
                   Just pp -> if (nn > 1024 && nn < 65535) then 
                        Right $ Conf (Port nn) (DBFilePath pp)
                      else 
                        Left $ PortRangeError $ fromIntegral nn
                   Nothing -> Left $ DBFilePathError ""
      Nothing -> Left PortMissing 


  -- error "makeConfig not implemented"


-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> AppM ConfigError Conf
parseOptions fp =
  -- Parse the options from the config file: "files/appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  do 
    commandLine <- liftIO commandLineParser
    jsonConfFile <- parseJSONConfigFile fp
    liftEither $ makeConfig (defaultConf <> jsonConfFile <> commandLine)

  -- error "parseOptions not implemented"
