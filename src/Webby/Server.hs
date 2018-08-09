module Webby.Server where


import qualified Data.Aeson           as A
import qualified Data.Binary.Builder  as Bu
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified UnliftIO.Concurrent  as Conc
import qualified UnliftIO.Exception   as E

import           WebbyPrelude

import           Webby.Types

-- | Retrieve the app environment given to the application at
-- initialization.
getAppEnv :: WebbyM appEnv appEnv
getAppEnv = asks weAppEnv

-- | Retrieve all path captures (TODO: extend?)
captures :: WebbyM appEnv [(Text, Text)]
captures = asks weCaptures

-- | Retrieve a particular capture (TODO: extend?)
getCapture :: Text -> WebbyM appEnv (Maybe Text)
getCapture t = do cs <- captures
                  return $ fmap snd $ headMay $ filter ((== t) . fst) cs

setStatus :: Status -> WebbyM appEnv ()
setStatus st = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $ \wr -> return $ wr { wrStatus = st}

addHeader :: Header -> WebbyM appEnv ()
addHeader h = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                  return $ wr { wrHeaders = hs ++ [h] }

-- similar to addHeader but replaces a header
setHeader :: Header -> WebbyM appEnv ()
setHeader (k, v) = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                      ohs = filter ((/= k) . fst) hs
                  return $ wr { wrHeaders = ohs ++ [(k, v)] }

text :: Text -> WebbyM appEnv ()
text t = do
    setHeader (hContentType, "text/plain; charset=utf-8")
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromByteString $
                                          encodeUtf8 t }

json :: A.ToJSON b => b -> WebbyM appEnv ()
json j = do
    setHeader (hContentType, "application/json; charset=utf-8")
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromLazyByteString $
                                          A.encode j }

stream :: StreamingBody -> WebbyM appEnv ()
stream s = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Left s }

matchPattern :: Request -> RoutePattern -> Maybe Captures
matchPattern r (RoutePattern mthd ps)
    | requestMethod r == mthd = doesMatch ps (pathInfo r)
    | otherwise = Nothing
  where
    doesMatch :: [PathSegment] -> [Text] -> Maybe Captures
    doesMatch [] [] = Just []
    doesMatch [] _ = Nothing
    doesMatch _ [] = Nothing
    doesMatch (Literal t:xs) (y:ys) | t == y = doesMatch xs ys
                                    | otherwise = Nothing
    doesMatch (Capture t:xs) (y:ys) = let c = (t, y)
                                      in fmap (c:) $ doesMatch xs ys

-- | TODO: Request matching is currently naive. It is not performant
-- for a large number of routes.
matchRequest :: Request -> Routes env -> Maybe (Captures, WebbyM env ())
matchRequest req routes = do
    let rM = headMay $ filter (\(_, _, k) -> isJust k) $
             map (\(pat, h) -> (pat, h, matchPattern req pat)) routes
    (_, h, csM) <- rM
    cs <- csM
    return (cs, h)

errorResponse404 :: WebbyM appEnv ()
errorResponse404 = setStatus status404

-- | Use this function, to create a WAI application. It takes a
-- user/application defined `appEnv` data type and a list of
-- routes. If none of the requests match a request, a default 404
-- response is returned.
webbyApp :: appEnv -> Routes appEnv -> Application
webbyApp appEnv routes req respond = do
    let defaultHandler = errorResponse404
        (cs, handler) = fromMaybe ([], defaultHandler) $ matchRequest req routes
    wEnv <- do v <- Conc.newMVar defaultWyResp
               return $ WEnv v cs req appEnv
    E.handleAny (\_ -> respond $ responseLBS status500 []
                        "Something went wrong") $ do
        runWebbyM wEnv handler
        let wVar = weResp wEnv
        wr <- Conc.takeMVar wVar
        case wrRespData wr of
          Left s  -> respond $ responseStream (wrStatus wr) (wrHeaders wr) s
          Right b -> do
              let clen = LB.length $ Bu.toLazyByteString b
              respond $ responseBuilder (wrStatus wr)
                  (wrHeaders wr ++ [(hContentLength, show clen)]) b

text2PathSegments :: Text -> [PathSegment]
text2PathSegments path =
    let mayCapture t = if ":" `T.isPrefixOf` t
                       then Just $ T.drop 1 $ t
                       else Nothing

        mkSegs [] = []
        mkSegs (p:ps) = maybe (Literal p) Capture
                        (mayCapture p) : mkSegs ps

        fixPath = bool identity (drop 1) (T.isPrefixOf "/" path)

    in mkSegs $ fixPath $ T.splitOn "/" path

-- | Create a route for a user-provided HTTP request method, pattern
-- and handler function.
mkRoute :: Method -> Text -> WebbyM appEnv ()
        -> (RoutePattern, WebbyM appEnv ())
mkRoute m p h = (RoutePattern m (text2PathSegments p), h)

-- | Create a route for a POST request method, given the path pattern
-- and handler.
post :: Text -> WebbyM appEnv () -> (RoutePattern, WebbyM appEnv ())
post = mkRoute methodPost

-- | Create a route for a GET request method, given the path pattern
-- and handler.
get :: Text -> WebbyM appEnv () -> (RoutePattern, WebbyM appEnv ())
get = mkRoute methodGet

-- | Create a route for a PUT request method, given the path pattern
-- and handler.
put :: Text -> WebbyM appEnv () -> (RoutePattern, WebbyM appEnv ())
put = mkRoute methodPut
