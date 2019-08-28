{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           System.IO 
import           Data.ByteString            (ByteString)
import qualified Data.ByteString         as BS

import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (encodeUtf8)

import           Data.Bifunctor             (first , bimap)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM(..) , liftEither)
import           Level06.Types              (ConfigError(..),
                                             PartialConf (PartialConf), partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = 
  let ios = do  
        eHandle <- try ( withFile fp ReadMode (\ h -> do 
                      fc <- hGetContents h
                      return $ (encodeUtf8 . pack) fc ) )
        case eHandle of
          Right handle -> return $ Right handle
          Left e -> return $ Left $ ConfFileNotFound e
  in AppM ios

-- readConfFile =
--   AppM
--   . fmap (first ConfFileNotFound)
--   . try
--   . BS.readFile


-- error "readConfFile not implemented"

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp = 
  readConfFile fp >>= (D.decodeFromByteString AB.parseOnly partialConfDecoder ) 
                  >>= liftEither . either (Left . BadConfFile . fst) Right

  -- 2nd alt , 
  -- let bsAppm = readConfFile fp
  --     jsonAppm = bsAppm >>= (D.decodeFromByteString AB.parseOnly partialConfDecoder ) 
  --                       >>= ( \x -> liftEither $ either (Left . BadConfFile . fst) Right x  )
  -- in  jsonAppm


  -- 1st alt more readable
  -- let bsAppm = readConfFile fp
  --     bsIo = runAppM bsAppm
  -- in AppM $ do 
  --   bse <- bsIo 
  --   case bse of 
  --     Right bb -> do 
  --                   ppc <- D.decodeFromByteString AB.parseOnly partialConfDecoder bb
  --                   return $ either (Left . BadConfFile . fst) Right ppc
  --     Left e -> return $ Left e

    
  -- error "parseJSONConfigFile not implemented"

-- Go to 'src/Level06/Conf.hs' next.
