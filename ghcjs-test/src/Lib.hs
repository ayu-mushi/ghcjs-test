{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE RecursiveDo  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}

module Lib
    ( myMain
    ) where

import GHCJS.Marshal.Pure (PFromJSVal(..))
import GHCJS.DOM (currentWindowUnchecked, currentDocumentUnchecked)
import GHCJS.DOM.Document (createElement, getBody)
import GHCJS.DOM.NonElementParentNode (getElementByIdUnchecked)
import GHCJS.DOM.Node (insertBefore)
import GHCJS.DOM.Element (setInnerHTML)
import Control.Concurrent
       (tryTakeMVar, takeMVar, readMVar, newMVar, MVar, swapMVar, threadDelay, putMVar, forkIO, newEmptyMVar, forkIOWithUnmask, killThread, isEmptyMVar, ThreadId, modifyMVar, modifyMVar_)
import GHCJS.Concurrent (OnBlocked(..))
import GHCJS.DOM.Types as DOMTypes (Element(unElement), castTo, ToJSString, HTMLInputElement(HTMLInputElement), Window, toElement)
import GHCJS.Types (JSString, JSVal)
import Control.Monad.Cont (ContT(..), callCC)
import GHCJS.Foreign.Callback (Callback, asyncCallback, asyncCallback1, asyncCallback2, syncCallback, syncCallback2, releaseCallback)
import Control.Monad (when, forever, forM_, void)
import Control.Monad.IO.Class (liftIO)
import GHCJS.DOM.HTMLInputElement (getValue)
import System.Timeout (timeout)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, NominalDiffTime)
import Data.Time.Format () -- Show instance
import qualified Data.JSString as JSString (pack)
import Data.JSString.Text (lazyTextToJSString)
import JavaScript.JQuery (append, selectElement, select)
import Reflex.Dom as Reflex
  (mainWidget, text, el, elAttr, el', elAttr', MonadWidget, textInput, TextInput(..), dynText, def,
  holdDyn, holdUniqDyn, EventName(Click), domEvent, foldDyn, mapDyn, El, tickLossy, TickInfo(_tickInfo_lastUTC, _tickInfo_n, _tickInfo_alreadyElapsed), Event, delay, count, Dynamic, ffilter, FunctorMaybe(fmapMaybe), keypress, display, leftmost, button, simpleList, webSocket, webSocketConfig_send,
  RawWebSocket(..), tag, current, setValue, value, textInputConfig_initialValue, foldDynM, mconcatDyn, combineDyn, attachPromptlyDynWith, zipDynWith, constant,
  sample, PushM, Reflex, updated, gate, DomBuilder)
import Control.Lens ((&), (.~))
import Data.Map (fromList)
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html (Html)
import Data.Text.Internal (Text)
import qualified Data.Text as Text(pack)
import Web.KeyCode (Key(..))
import Control.Monad.STM (retry, atomically)
import Control.Concurrent.STM (newTVar, readTVar, writeTVar, modifyTVar)

foreign import javascript unsafe "alert($1);" alert :: GHCJS.Types.JSString -> IO ()
foreign import javascript interruptible "setTimeout($c, $1);" delayJS :: Int -> IO ()
foreign import javascript unsafe "setTimeout($2, $1);" settimeout_js :: Int -> Callback (IO ()) -> IO ()
foreign import javascript interruptible "$1.onmousemove = function(e) { $c(e.clientX, e.clientY) };" mouseXY
    :: Element -> IO (Int, Int) -- 一応、うまく周期をずらせば複数のスレッドのループからでも使えるはず…
foreign import javascript interruptible "$1.onmousedown = function(e) { $c(e.clientX, e.clientY) };" clickXY
    :: Element -> IO (Int, Int)
foreign import javascript unsafe "(function(){ var flag = true; $1.onmousemove = function(e) { if (flag == true) { $2(e.clientX, e.clientY); } flag = false;}})();" mouseXY_contjs
    :: Element -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
foreign import javascript interruptible "document.onkeydown = function(e) { $c(e.keyCode) };" keydown
    :: IO Int

waiting :: (Int -> IO ()) -> IO ()
waiting act = waiting' 0
  where
    waiting' :: Int -> IO ()
    waiting' n = do
      act n
      delayJS $ 3 * (10^3)
      waiting' $ n+1

doSth :: IO Bool
doSth = do
  delayJS $ 10 * 10^3
  return True

initialHtml :: Html
initialHtml = [shamlet|
<div>
  <a href="https://github.com/ayu-mushi/ghcjs-test">source code of this "ghcjs test"
  <p>See also console!

<div>
  <canvas #canvas>
  <input #in>
  <p #out>
  <p #mouse>
  <p #p_key>
|]

showJS :: Show a => a -> JSString
showJS = JSString.pack . show

-- 継続モナドを利用する版
sleepCont :: Int -> ContT () IO ()
sleepCont n = ContT $ \f -> do
  cb <- asyncCallback $ f ()
  settimeout_js n cb
  releaseCallback cb

mouseXYCont :: Element -> ContT () IO (Int, Int) -- Element -> ((Int,Int) -> IO ()) -> IO ()
mouseXYCont elm = ContT $ \f -> do
  cb <- asyncCallback2 $ \x y -> f (pFromJSVal x, pFromJSVal y)
  mouseXY_contjs elm cb
  releaseCallback cb

sleepingPerson :: Element -> IO ()
sleepingPerson elm = (`runContT` return) $ callCC $ \k -> forever $ do
  liftIO $ putStrLn "zzz...zzz"
  () <- sleepCont $ 1 * 10^4
  (x, y) <- mouseXYCont elm
  when (x < 10) $ do
    liftIO $ putStrLn "your mouse is on (x, y) for some x that < 10. I get up now."
    k ()
  liftIO $ putStrLn $ "Oh, your mouse is on  " ++ show (x, y) ++ "...zzzz"

watchMouse :: MVar (Int, Int) -> Element -> IO ()
watchMouse mouseVar elm = forever $ do
  xy <- mouseXY elm
  isntEmp <- not <$> isEmptyMVar mouseVar
  when isntEmp $ void $ takeMVar mouseVar
  putMVar mouseVar xy
  forkIO $ do
    threadDelay $ 10 ^ 5
    isntEmp <- not <$> isEmptyMVar mouseVar
    when isntEmp $ void $ takeMVar mouseVar

threadLoop :: Int -> IO () -> IO ThreadId
threadLoop n = forkIO . forever . (>> threadDelay n)

-- 敵の攻撃とかに種類、多相性あるならオブジェクトを使う
data Action a where
  Attack :: Action ()
  Defence :: Action ()
  Cure :: Action ()
  Poison :: Action ()

data Player = Player {
  hp :: Int
, atk :: Int
, dfc :: Int
, isPoison :: Bool
  }

buttonDyn :: (DomBuilder t m, MonadWidget t m) => Dynamic t Text -> m (Event t ())
buttonDyn t = do
  (e, _) <- el' "button" $ dynText t
  return $ domEvent Click e

myWidget :: (MonadWidget t m) => m ()
myWidget = do
  el "div" $ text $ Text.pack "Welcome to Reflex"
  t <- textInput def
  el "div" $ dynText $ _textInput_value t
  el "div" $ do
    text "Last keypressed: "
    (holdDyn "None" $ show <$> _textInput_keypress t) >>= display
  rec
    (e, _) <- elAttr' "div" (fromList [("style", "color: red")]) $ (text ("[Click Here]: ")) >> clicker e
  el "div" $ do
    ct <- liftIO getCurrentTime
    (tick::Event t TickInfo) <- tickLossy 1 ct
    (holdDyn (0, ct) $ (\t -> (_tickInfo_n t, _tickInfo_lastUTC t)) <$> tick) & fmap (fmap $ Text.pack . show) >>= dynText

    return ()

  rec
    cookie_click <- button "get cookie"
    cookie <- count cookie_click >>= \sumcookie -> return $ foldl1 (zipDynWith (+)) [sumcookie, profit_grandma, profit_factory]
    display cookie

    grandma_button <- buttonDyn $ fmap (\pri -> Text.pack ("buy grandma ($"++ show pri ++" cookies)")) grandma_price

    let grandma_price = fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) uniq_grandma
    uniq_grandma <- buyDyn grandma_price cookie grandma_button
    profit_grandma <- profit uniq_grandma (const :: Int -> NominalDiffTime -> Int) 1 grandma_price

    dynText $ fmap (\n -> Text.pack $ "数:" ++ show n ++ "います！") uniq_grandma
    dynText $ fmap (\prof -> Text.pack $ "利潤: " ++ show prof ++ "だよ☆") profit_grandma

    -- 減価償却費
    -- 借金、リスク
    -- 破産

    factory_button <- buttonDyn $ fmap (\pri -> Text.pack ("buy factory ($"++ show pri ++" cookies)")) factory_price

    let factory_price = fmap (\n -> floor $ (50.0::Float) * ((1.15::Float) ^ n)) uniq_factory
    uniq_factory <- buyDyn factory_price cookie factory_button
    profit_factory <- profit uniq_factory (\n _ -> 10 * n) 3 factory_price

    dynText $ fmap (\n -> Text.pack $ "数:" ++ show n ++ "います！") uniq_factory
    dynText $ fmap (\prof -> Text.pack $ "利潤: " ++ show prof ++ "だよ☆") profit_factory

  return ()
    where
      clicker :: (MonadWidget t m) => El t -> m ()
      clicker e = do
        deleyed <- delay 0.1 $ domEvent Click e
        (d::Dynamic t Int) <- count deleyed
        dynText $ Text.pack . (\x -> ("鈍感 ["++show x++"]"::String)) <$> d
        (d'::Dynamic t Int) <- foldDyn (const(+1)) 0 $ domEvent Click e
        dynText $ Text.pack . (\x -> ("敏感 ["++show x++"]"::String)) <$> d'


type Price = Int
type Ability = Int -> NominalDiffTime -> Price
type Amount t = Dynamic t Int -- クッキー、おばあさん、工場などの数を表すDynamicだが、値段などは含まない
-- 値段もクッキーだけど。

work :: (MonadWidget t m) => Amount t -> Ability -> NominalDiffTime -> m (Dynamic t Int)
work labor ability interval = do
  ct <- liftIO getCurrentTime
  (tick::Event t TickInfo) <- tickLossy interval ct
  let (product::Event t Int) = attachPromptlyDynWith (\n t -> ability n $ _tickInfo_alreadyElapsed t) labor tick
  totalproduct <- foldDyn (+) 0 product
  return totalproduct

buyDyn :: (MonadWidget t m) => Dynamic t Price -> Amount t -> Event t () -> m (Amount t)
buyDyn priceDyn cookie buyEv = do
  commodity <- foldDynM (\() n -> do price <- sample $ current priceDyn
                                     buy price cookie () n) 0 buyEv
  (uniqCom::Dynamic t Int) <- holdUniqDyn commodity
  return uniqCom

consum :: (MonadWidget t m) => Dynamic t Price -> Amount t -> m (Dynamic t Price)
consum priceDyn commodity = foldDyn (+) 0 $ fmap (* (-1)) $ tag (current priceDyn) $ updated commodity

profit :: (MonadWidget t m) => Amount t -> Ability -> NominalDiffTime -> Dynamic t Price -> m (Dynamic t Int) -- profit = 利潤 = benefit - cost
profit labor ability interval priceDyn = do
  benefit <- work labor ability interval
  cost <- consum priceDyn labor
  return $ zipDynWith (+) cost benefit

buy :: (Reflex t) => Price -> Amount t -> () -> Int -> PushM t Int
buy price cookie () n = do
  nowcok <- sample $ current cookie
  if nowcok < price then
    return n
  else
    return $ n + 1

data CommoditySpec = CommoditySpec
  { initialPrice :: Price
  , workInterval :: NominalDiffTime
  , workAbility :: Ability
  }
-- CpS

htmlInputElem :: Element -> HTMLInputElement
htmlInputElem = HTMLInputElement . unElement

-- TODO  作ってみる： RPGの戦闘画面, Virtue & Viceシステム(実績解放のようなもの)
-- テトリス、戦略シミュレーション、生態系シミュレーション、ライフゲーム
-- 将棋
-- Cookie Clicker, TVar

myMain :: IO ()
myMain = do {- reflex part -}
  mainWidget myWidget

  {- normal part -}
  (window::DOMTypes.Window) <- currentWindowUnchecked
  doc <- currentDocumentUnchecked
  Just (body::Element) <- fmap toElement <$> getBody doc
  selectElement body >>= append (lazyTextToJSString $ renderHtml initialHtml)
  canvas <- getElementByIdUnchecked doc ("canvas"::String)
  inElem <- htmlInputElem <$> getElementByIdUnchecked doc ("in"::String)
  out <- getElementByIdUnchecked doc ("out"::String)
  mouse <- getElementByIdUnchecked doc ("mouse"::String)
  p_key <- getElementByIdUnchecked doc ("p_key"::String)

  mouseVar <- newEmptyMVar
  forkIO $ watchMouse mouseVar canvas
  setInnerHTML mouse $ Just ("See also console"::String)
  watchMouseTh <- forkIO $ forM_ (reverse [0..99]) $ \n -> do
    setInnerHTML mouse $ (Nothing :: Maybe String)
    setInnerHTML mouse $ Just ("mouse is not moving"::String)
    (mx1, my1) <- takeMVar mouseVar
    setInnerHTML mouse $ Just $ "rest: " ++ show n ++ ", moving to" ++ show (mx1, my1)
    putStrLn $ "rest: " ++ show n ++ ", moving to" ++ show (mx1, my1)
    threadDelay $ 10^6
  forkIO $ forever $ do
    k <- keydown
    setInnerHTML p_key $ Just $ show k

  forkIO $ forever $ do
    threadDelay $ 10^6
    xy <- takeMVar mouseVar
    putStrLn $ show xy

  -- 「10 秒間クリック可能」みたいなのはどうやる？→
  putStrLn "you can click canvas over 10 second"
  answer <- timeout (10 * 10^6) $ do
    xy <- clickXY canvas
    putStrLn $ "you clicked canvas: " ++ show xy
  when (answer == Nothing) $ do
    putStrLn "you did not clicked canvas"

  {-sleepingTh <- forkIO $ sleepingPerson canvas
  forkIO $ do
    (x, y) <- clickXY canvas
    putStrLn "Killed sleeping person (Sorry!)"
    when (x == x) $ killThread sleepingTh -- doesn't work
    -}
  w <- forkIO $ waiting $ \n -> setInnerHTML out $ Just $ show n
  u <- doSth
  when u $ killThread w
  setInnerHTML out $ Just $ show "killed waiting thread"
  delayJS $ 5 * 10^3
  w' <- forkIO $ forever $ do
    val <- getValue inElem :: IO (Maybe String)
    setInnerHTML out val
    threadDelay $ 5 * 10^5
  u' <- doSth
  when u' $ killThread w'
  return ()
