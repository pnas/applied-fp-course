{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (Topic ,
                                           CommentText ,
                                           ContentType (PlainText , Json), 
                                           Error (Error),
                                           RqType (AddRq, ListRq, ViewRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse ss ct lb =
  responseLBS ss [("Content-Type", renderContentType ct)] lb
  -- error "mkResponse not implemented"

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 ct lb = mkResponse status200 ct lb
  -- error "resp200 not implemented"

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 ct lb = mkResponse status404 ct lb
  -- error "resp404 not implemented"

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 ct lb = mkResponse status400 ct lb
  -- error "resp400 not implemented"

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest inText inBs =  
    case mkTopic inText of 
      Left x -> Left x 
      Right tt -> case mkCommentText $ lazyByteStringToStrictText inBs  of 
                     Left z -> Left z
                     Right pp -> Right $ AddRq tt pp    
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- error "mkAddRequest not implemented"

      -- case inText of 
      --   empty -> Left $ Error "empty topic title"
      --   _ -> Right $ AddRq (mkTopic inText ) (mkCommentText $ lazyByteStringToStrictText inBs )      
-- error "mkAddRequest not implemented"

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest vT =
  case mkTopic vT of 
    Left x -> Left x 
    Right tt -> Right $ ViewRq tt

-- error "mkViewRequest not implemented"

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq
  -- error "mkListRequest not implemented"

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse (Error erMsg) = resp400 PlainText $ LBS.fromStrict ( encodeUtf8 erMsg)
  -- error "mkErrorResponse not implemented"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case pathInfo rq of
    ( "list" : _ ) -> pure $ mkListRequest
    ( ta : "add" : _ ) -> pure $ mkAddRequest ta (LBS.fromStrict ( encodeUtf8 "add topic"))
    ( tt : "view" : _ ) -> pure $ mkViewRequest tt
    _ -> pure $ Left (Error "unknown request type")
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  -- error "mkRequest not implemented"

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response

handleRequest ListRq = Right $ resp200 PlainText (LBS.fromStrict ( encodeUtf8 "view topics"))

handleRequest (ViewRq tt) = 
  Right $ resp200 PlainText (LBS.fromStrict ( encodeUtf8 "make view req"))

handleRequest (AddRq aa bb ) = 
  Right $ resp200 PlainText (LBS.fromStrict ( encodeUtf8 "make add request"))
  -- error "handleRequest not implemented"
  -- (Topic tt) (CommentText ll)
-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app req respond = do
  ss <- mkRequest req
  case ss of 
    Right rq -> handleRequest rq
    -- Left err -> 
  -- 
  -- error "app not reimplemented"

runApp :: IO ()
runApp = run 3000 app
