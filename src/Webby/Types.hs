{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Webby.Types where

import qualified Data.Binary.Builder as Bu
import qualified UnliftIO            as U
import qualified UnliftIO.Concurrent as Conc

import           WebbyPrelude

-- | A data type to represent parts of the response constructed in the
-- handler when servicing a request.
data WyResp = WyResp { wrStatus    :: Status
                     , wrHeaders   :: ResponseHeaders
                     , wrRespData  :: Either StreamingBody Bu.Builder
                     , wrResponded :: Bool
                     }

defaultWyResp :: WyResp
defaultWyResp = WyResp status200 [] (Right Bu.empty) False

-- | The reader environment used by the web framework. It is
-- parameterized by the application's environment data type.
data WEnv appEnv = WEnv { weResp     :: Conc.MVar WyResp
                        , weCaptures :: [(Text, Text)]
                        , weRequest  :: Request
                        , weAppEnv   :: appEnv
                        }

-- | The main monad transformer stack used in the web-framework.
--
-- The type of a handler for a request is `WebbyM appEnv ()`. The
-- `appEnv` parameter is used by the web application to store an
-- (read-only) environment. For e.g. it can be used to store a
-- database connection pool.
newtype WebbyM appEnv a = WebbyM
    { unWebbyM :: ReaderT (WEnv appEnv) (ResourceT IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WEnv appEnv))

instance U.MonadUnliftIO (WebbyM appData) where
    askUnliftIO = WebbyM $ ReaderT $
                  \(w :: WEnv appData) -> U.withUnliftIO $
                  \u -> return $
                  U.UnliftIO (U.unliftIO u . flip runReaderT w . unWebbyM)

runWebbyM :: WEnv w -> WebbyM w a -> IO a
runWebbyM env = runResourceT . flip runReaderT env . unWebbyM

-- A route pattern specifies a HTTP method and a list of path segments
-- that must match.
data RoutePattern = RoutePattern Method [PathSegment]
                  deriving (Eq, Show)

-- | The Routes of a web-application are a list of mappings from a
-- `RoutePattern` to a handler function.
type Routes appEnv = [(RoutePattern, WebbyM appEnv ())]

-- | Captures are simply extracted path elements. TODO: extend this to
-- be more useful?
type Captures = [(Text, Text)]

data PathSegment = Literal Text
                 | Capture Text
                 deriving (Eq, Show)
