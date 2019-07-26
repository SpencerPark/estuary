{-# LANGUAGE JavaScriptFFI, OverloadedStrings, LambdaCase #-}
module Estuary.WebDirt.SampleBank where
  
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Functor 
import Data.JSString.Text

import Data.Text

import Estuary.Render.ResourceProvider
import Estuary.Types.Resources

import GHCJS.DOM.Blob
import GHCJS.Foreign as JS
import GHCJS.Foreign.Callback
import GHCJS.Marshal.Pure
import GHCJS.Types as JS

import Sound.MusicW.AudioBuffer
import Sound.MusicW.AudioContext

-- > An AudioBuffer may be used by one or more AudioContexts, and can be shared between an OfflineAudioContext 
-- > and an AudioContext.
-- - https://www.w3.org/TR/webaudio/#audiobuffer
--
-- i.e. Decoding doesn't depend on using the same context as that being used to play the sound.
foreign import javascript interruptible
  " var r = new FileReader(); \
  \ r.onload = function(event) { \
  \   $1.decodeAudioData(event.target.result, $c, $c) \
  \ }; \
  \ r.readAsArrayBuffer($2);"
  js_decodeResource :: AudioContext -> Blob -> IO (JSVal)

instance Decodable AudioBuffer where
  decodeResource blob = do
    ctx <- getGlobalAudioContext
    jsResult <- liftIO $ js_decodeResource ctx blob
    if JS.isString jsResult then do
      return $ Left $ textFromJSVal jsResult
    else
      return $ Right $ pFromJSVal jsResult

performAudioResourceRequest :: MVar (ResourceCache AudioBuffer) -> CachingResourceRequest AudioBuffer r -> IO r
performAudioResourceRequest cacheVar request =
  modifyMVar cacheVar $ \cache -> do
    (r, nextCache) <- performRequest request cache
    return (nextCache, r)

load :: (ResourceDataProvider p) => MVar (ResourceCache AudioBuffer) -> MVar (ResourceMap AudioMeta) -> MVar (p AudioMeta) -> Text -> Int -> (IO ()) -> IO ()
load cacheVar resourcesVar dataProviderVar groupName number done = do
  resources <- readMVar resourcesVar
  dataProvider <- readMVar dataProviderVar
  case resolveResource resources groupName number of
    Nothing -> done
    Just resource ->  performAudioResourceRequest cacheVar $
      getResourceAsync dataProvider resource (\_ -> done)

loadAllNamed :: (ResourceDataProvider p) => MVar (ResourceCache AudioBuffer) -> MVar (ResourceMap AudioMeta) -> MVar (p AudioMeta) -> Text -> IO ()
loadAllNamed cacheVar resourcesVar dataProviderVar groupName = do
  resources <- readMVar resourcesVar
  dataProvider <- readMVar dataProviderVar
  performAudioResourceRequest cacheVar $
    forM_ (listResourceGroup resources groupName) $ \resource ->
      getResourceAsync dataProvider resource (\_ -> return ())

getBuffer :: (ResourceDataProvider p) => MVar (ResourceCache AudioBuffer) -> MVar (ResourceMap AudioMeta) -> MVar (p AudioMeta) -> Text -> Int -> IO (Maybe AudioBuffer)
getBuffer cacheVar resourcesVar dataProviderVar groupName number = do
  resources <- readMVar resourcesVar
  dataProvider <- readMVar dataProviderVar
  case resolveResource resources groupName number of
    Nothing -> return Nothing
    Just resource -> performAudioResourceRequest cacheVar $
      getResourceIfReady dataProvider resource


newtype SampleBank = SampleBank JSVal
instance PToJSVal SampleBank where pToJSVal (SampleBank val) = val
instance PFromJSVal SampleBank where pFromJSVal = SampleBank

newtype LoadedCallback = LoadedCallback JSVal
instance PToJSVal LoadedCallback where pToJSVal (LoadedCallback val) = val
instance PFromJSVal LoadedCallback where pFromJSVal = LoadedCallback

foreign import javascript unsafe
  "var bank = new Object(); \
  \bank.ac = $1; \
  \bank.loadAllNamed = $2; \
  \bank.load = $3; \
  \bank.getBuffer = $4; \
  \$r = bank;"
  js_makeSampleBank :: AudioContext 
    -> {- loadAllNamed -} Callback (JSVal {- groupName -} -> IO ()) 
    -> {- load -} Callback (JSVal {- groupName -} -> JSVal {- number? -} -> JSVal {- cb? -} -> IO ()) 
    -> {- getBuffer -} Callback (JSVal {- groupName -} -> JSVal {- number? -} -> IO (JSVal {- buffer -})) 
    -> IO (SampleBank)

foreign import javascript unsafe
  "$1();"
  js_invokeCallback :: LoadedCallback -> IO ()

makeSampleBank :: (ResourceDataProvider p) => AudioContext -> ResourceMap AudioMeta -> p AudioMeta -> IO (MVar (ResourceMap AudioMeta), MVar (p AudioMeta), SampleBank)
makeSampleBank ctx initialResources initialDataProvider = do
  cacheVar <- newMVar emptyResourceCache
  resourcesVar <- newMVar initialResources
  dataProviderVar <- newMVar initialDataProvider
  -- WARNING: these technically leak because releaseCallback is never called but there shouldn't be enough
  -- instances of samplebanks to cause any problem.
  loadAllNamedFn <- syncCallback1 ThrowWouldBlock $ \jsGroupName -> 
    loadAllNamed cacheVar resourcesVar dataProviderVar (textFromJSVal jsGroupName)
  loadFn <- syncCallback3 ThrowWouldBlock $ \jsGroupName mJsNumber mJsCb ->
    let groupName = textFromJSVal jsGroupName
        number = if JS.isNull mJsNumber || JS.isUndefined mJsNumber then 0 else pFromJSVal mJsNumber
        cb = if JS.isNull mJsCb || JS.isUndefined mJsCb then return () else js_invokeCallback $ pFromJSVal mJsCb
    in load cacheVar resourcesVar dataProviderVar groupName number cb
  getBufferFn <- syncCallback2' $ \jsGroupName mJsNumber ->
    let groupName = textFromJSVal jsGroupName
        number = if JS.isNull mJsNumber || JS.isUndefined mJsNumber then 0 else pFromJSVal mJsNumber
    in 
      getBuffer cacheVar resourcesVar dataProviderVar groupName number >>= \case
        Nothing -> return JS.nullRef
        Just buffer -> return $ pToJSVal buffer
  sampleBank <- js_makeSampleBank ctx loadAllNamedFn loadFn getBufferFn
  return (resourcesVar, dataProviderVar, sampleBank)