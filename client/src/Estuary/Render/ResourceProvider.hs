{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module Estuary.Render.ResourceProvider where

import Control.Concurrent
import Control.Concurrent.STM.TVar

import Control.Monad.IO.Class

import Control.Monad.STM

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Functor

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq, (><), (|>))
import qualified Data.Sequence as Seq

import Data.Text as T

import Estuary.Types.Resources

import GHCJS.DOM.Blob

-- not a node, but what ever you make a node with.
--class (ResourceDataProvider a) => ResourceProvider a where
  -- ^ prefetchResource is a mechanism for hinting to the provider that a resource will be needed soon and
  -- should be fetched and cached so that a future call to `getResource` is more likely to succeed.  
  -- prefetchResource :: a -> Resource m -> IO ()

class ResourceDataProvider a where
  --resourceIsProvidedBy :: a d -> Resource d -> Bool
  
  fetchResource :: a d -> Resource d -> IO (Either Text Blob)

class Decodable v where
  decodeResource :: Blob -> IO (Either Text v)
  
  decodeResourceAsync :: Blob -> (Either Text v -> IO ()) -> IO ()
  decodeResourceAsync blob cb = void $ forkIO $ decodeResource blob >>= cb

data ResourceRequest v 
  = Loading
  | Loaded v
  | Error Text

fromEither :: Either Text v -> ResourceRequest v
fromEither (Left err) = Error err
fromEither (Right value) = Loaded value

newtype ResourceCache v = ResourceCache { unResourceCache :: Map Text (Map Text (TVar (ResourceRequest v))) }

emptyResourceCache :: ResourceCache v
emptyResourceCache = ResourceCache Map.empty

markLoading :: Text -> Text -> ResourceCache v -> IO (ResourceCache v)
markLoading groupName resourceName (ResourceCache cache) =
  let group = Map.findWithDefault Map.empty groupName cache in do
    var <- newTVarIO Loading
    return $ ResourceCache $ Map.insert groupName (Map.insert resourceName var group) cache

cacheResult :: ResourceCache v -> Text -> Text -> Either Text v -> IO ()
cacheResult (ResourceCache cache) groupName resourceName result = 
  let var = (cache ! groupName) ! resourceName
  in liftIO $ atomically $ writeTVar var $ fromEither result

lookupCache :: ResourceCache v -> Text -> Text -> IO (Maybe (ResourceRequest v))
lookupCache (ResourceCache cache) groupName resourceName = 
  case Map.lookup groupName cache >>= Map.lookup resourceName of
    Nothing -> return Nothing
    Just var -> liftIO $ readTVarIO var <&> Just

type CachingResourceRequest v = StateT (ResourceCache v) IO

performRequest :: CachingResourceRequest v r -> ResourceCache v -> IO (r, ResourceCache v)
performRequest = runStateT

getResource :: (ResourceDataProvider p, Decodable v) => p d -> Resource d -> CachingResourceRequest v (Maybe v)
getResource dataProvider resource = do
  cache <- get
  lift (lookupCache cache (resourceGroup resource) (resourceFileName resource)) >>= \case
    Nothing -> loadResource dataProvider resource
    Just (Loaded value) -> return $ Just value
    _ -> return Nothing

getResourceIfReady :: (ResourceDataProvider p, Decodable v) => p d -> Resource d -> CachingResourceRequest v (Maybe v)
getResourceIfReady dataProvider resource = do
  cache <- get
  lift (lookupCache cache (resourceGroup resource) (resourceFileName resource)) >>= \case
    Nothing -> loadResourceAsync dataProvider resource (\_ -> return ()) $> Nothing
    Just (Loaded value) -> return $ Just value
    Just _ -> return Nothing

getResourceAsync :: (ResourceDataProvider p, Decodable v) => p d -> Resource d -> (Maybe v -> IO ()) -> CachingResourceRequest v ()
getResourceAsync dataProvider resource done = do
  cache <- get
  lift (lookupCache cache (resourceGroup resource) (resourceFileName resource)) >>= \case
    Nothing -> loadResourceAsync dataProvider resource done
    Just (Loaded value) -> lift $ done $ Just value
    _ -> lift $ done $ Nothing

loadResource :: (ResourceDataProvider p, Decodable v) => p d -> Resource d -> CachingResourceRequest v (Maybe v)
loadResource dataProvider resource = do
  get >>= lift . markLoading (resourceGroup resource) (resourceFileName resource) >>= put
  dataResponse <- lift $ fetchResource dataProvider resource
  case dataResponse of
    Left error -> do
      putCache (Left error) $> Nothing
    Right blob -> do
      (decodeResponse :: Either Text v) <- lift $ decodeResource blob 
      case decodeResponse of
        Left error -> putCache (Left error) $> Nothing
        Right value -> putCache (Right value) $> Just value
  where putCache :: Either Text v -> CachingResourceRequest v ()
        putCache result = get >>= \cache -> lift $ 
          cacheResult cache (resourceGroup resource) (resourceFileName resource) result

loadResourceAsync :: (ResourceDataProvider p, Decodable v) => p d -> Resource d -> (Maybe v -> IO ()) -> CachingResourceRequest v ()
loadResourceAsync dataProvider resource done = do
  get >>= lift . markLoading (resourceGroup resource) (resourceFileName resource) >>= put
  cache <- get
  let putResult = cacheResult cache (resourceGroup resource) (resourceFileName resource) 
      goLoad = do
        -- TODO all these identity returns can be cleaned up.
        fetchResource dataProvider resource >>= \case
          Left error -> return $ Left error
          Right blob -> decodeResource blob >>= \case
            Left error -> return $ Left error
            Right value -> return $ Right value

  lift $ liftIO $ void $ forkFinally goLoad $ \case
    Left err -> do
      putResult $ Left $ T.pack $ show err
      done Nothing
    Right res@(Left _) -> putResult res >> done Nothing
    Right res@(Right value) -> putResult res >> done (Just value)

  