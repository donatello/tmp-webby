module Main where

import           Conduit                  (awaitForever, runConduitRes, (.|))
import qualified Data.Aeson               as A
import           Data.ByteString.Builder  (byteString)
import qualified Data.Conduit.Combinators as C (sourceFile)
import qualified Network.Wai.Handler.Warp as W
import qualified UnliftIO.Exception       as E

import           WebbyPrelude

import           Webby

data AppEnv = AppEnv { aeAppName :: Text
                     }

type Handler = WebbyM AppEnv ()

bracketHandler :: Handler
bracketHandler = E.bracket (liftIO $ print "allocating")
                 (\_ -> liftIO $ print "cleaning up")
                 $ \_ -> do env <- getAppEnv
                            text (show $ aeAppName env)

streamingHandler :: Handler
streamingHandler = do
    let streamer w f = runConduitRes $
                       C.sourceFile "/etc/issue"
                       .| awaitForever (\b -> liftIO $ w (byteString b) >> f)

    stream streamer

jsonHandler :: Handler
jsonHandler = json $ A.object [ "language" A..= A.String "Haskell"
                              , "rating" A..= A.String "10"
                              ]

main :: IO ()
main = do
    let routes = [ get "/api/a" $ text "a"
                 , get "/api/b" $ text "b"
                 , post "/api/capture/:id" $ do idM <- getCapture "id"
                                                text $ show idM

                 , get "/aaah" $ E.throwString "oops!"

                 , get "/api/bracket" bracketHandler
                 , get "/api/streaming" streamingHandler
                 , get "/api/json" jsonHandler
                 ]
    W.runEnv 9000 $ webbyApp (AppEnv "myApp") routes
