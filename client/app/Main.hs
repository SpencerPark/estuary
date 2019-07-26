 {-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.List(intercalate)
import Data.Time
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad(liftM)
import Control.Monad.IO.Class(liftIO)

import Sound.MusicW

import Estuary.Render.AudioContext
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Protocol.Peer
import Estuary.Types.Context
import Estuary.Types.CanvasState
import Estuary.Types.Resources
import Estuary.Widgets.Estuary
import Estuary.Widgets.Navigation(Navigation(..))
import Estuary.WebDirt.SampleBank
import Estuary.WebDirt.SampleEngine
import Estuary.RenderInfo
import Estuary.RenderState
import Estuary.Renderer
import Estuary.Render.DynamicsMode
import Estuary.Render.LocalResources

import GHC.Conc.Sync(setUncaughtExceptionHandler, getUncaughtExceptionHandler)

import GHCJS.DOM
import GHCJS.DOM.Types hiding (toJSString)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import GHCJS.Marshal.Pure
import GHCJS.Prim(toJSString)
import GHCJS.Types

import Reflex.Host.Class (HostFrame)

import System.Timeout(timeout)

main :: IO ()
main = do
  warnBeforeGoingBackInBrowser
  existingUncaughtHandler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler $ \e -> do
    existingUncaughtHandler e
    visuallyCrash e

  pp <- newPeerProtocol

  ac <- getGlobalAudioContext
  addWorklets ac

  (audioResourcesVar, audioProviderVar, sampleBank) <- makeSampleBank ac emptyResourceMap emptyLocalResourceServer

  mainBusNodes@(mainBusIn,_,_,_) <- initializeMainBus
  wd <- liftAudioIO $ newWebDirt sampleBank mainBusIn
  initializeWebAudio wd
  sd <- newSuperDirt
  mv <- emptyCanvasState >>= newMVar
  now <- liftAudioIO $ audioUTCTime
  c <- newMVar $ initialContext now mainBusNodes wd sd mv pp
  ri <- newMVar $ emptyRenderInfo
  forkRenderThreads c ri

  cb <- syncCallback1' $ \dest -> do
    ctx <- readMVar c
    node <- changeDestination (mainBus ctx) $
      if dest `js_eq` pToJSVal ("stream" :: JSString) then
        getSharedMediaStreamDestination
      else
        createDestination
    return $ pToJSVal node
  js_registerSetEstuaryAudioDestination cb

  mainWidgetInElementById "estuary-root" $ estuaryWidget c ri audioResourcesVar audioProviderVar

  -- Signal the splash page that estuary is loaded.
  js_setIconStateLoaded

  -- Resume the audio context after interaction.
  js_waitForClickBody
  mErr <- liftAudioIO $ do
    ac <- audioContext
    liftIO $ resumeSync ac
  case mErr of
    Just err -> putStrLn $ show err
    Nothing -> return ()


visuallyCrash :: SomeException -> IO ()
visuallyCrash e =
  let lines = [
          "Unhandled exception: ",
          displayException e,
          "Click 'OK' to reload the page or 'Cancel' to remain on the page which will be unresponsive."
        ]
  in js_confirmReload $ toJSString $ intercalate "\n" lines

foreign import javascript unsafe
  "if (window.confirm($1)) {        \
  \  window.___forcedReload = true; \
  \  window.location.reload();      \
  \}"
  js_confirmReload :: JSVal -> IO ()

foreign import javascript interruptible
  "document.body.addEventListener('click', $c, {once: true});"
  js_waitForClickBody :: IO ()

foreign import javascript safe
  "window.addEventListener('beforeunload', function (e) { \
  \  if (!window.___forcedReload) {                       \
  \    e.preventDefault();                                \
  \    e.returnValue = '';                                \
  \  }                                                    \
  \});"
  warnBeforeGoingBackInBrowser :: IO ()

foreign import javascript safe
  "document.querySelector('#estuary-root')"
  js_estuaryMountPoint :: IO JSVal

foreign import javascript safe
  "EstuaryIcon.state = 'loaded';"
  js_setIconStateLoaded :: IO ()

foreign import javascript unsafe
  "window.___setEstuaryAudioDestination = $1"
  js_registerSetEstuaryAudioDestination :: Callback (JSVal -> IO JSVal) -> IO ()
