{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecursiveDo, Rank2Types, FlexibleContexts#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( someFunc
    ) where

import Reflex.Dom as Reflex
  (mainWidget, text, el, elAttr, el', elAttr', MonadWidget, textInput, TextInput(..), dynText, def,
  holdDyn, holdUniqDyn, EventName(Click), domEvent, foldDyn, mapDyn, El, tickLossy, TickInfo(_tickInfo_lastUTC, _tickInfo_n, _tickInfo_alreadyElapsed),  delay, count,  ffilter, FunctorMaybe(fmapMaybe), keypress, display, leftmost, button, simpleList, webSocket, webSocketConfig_send,
  RawWebSocket(..), tag, current, setValue, value, textInputConfig_initialValue, mconcatDyn, combineDyn, attachPromptlyDynWith, zipDynWith, constant,
  Reflex, updated, gate, DomBuilder, splitE,
  MonadHold, hold, tagPromptlyDyn, textArea, textArea_value, TextArea, attributes, constDyn, (=:), textAreaConfig_initialValue, attach, attachWith, getPostBuild, PostBuild, attachWidget, askJSContext, performEvent, ffor, PerformEvent, Performable, zipDyn, distributeListOverDyn)
import Reflex.Dom.Main (Widget, mainWidgetWithHead, mainWidgetWithHead')
import Reflex.Dynamic (distributeMapOverDynPure)
import Reflex.Class (accum, Dynamic, Event, Behavior, headE)
import Reflex.Spider (SpiderTimeline, Global, Spider)
import qualified Data.Text as Text(pack, lines, unlines, isPrefixOf)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, NominalDiffTime)
import Data.Text.Internal (Text)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomR, mkStdGen, Random, newStdGen)
import Control.Monad.Fix (MonadFix(mfix), fix)
import Control.Monad (void, join)
import Control.Monad.Writer (tell, runWriterT, Writer, WriterT)
import Data.Functor ((<$), ($>))
import Control.Monad.Trans (lift)
import Data.Monoid (Sum(Sum, getSum))
import Data.Map as Map (singleton, Map, insert, (!),fromList, toList, filterWithKey, elems)
import Control.Lens (Lens', (&), (.~))
import Control.Lens.Iso(iso, Iso')
import Data.Semigroup (Semigroup, (<>))
import GHCJS.DOM.Document(getHeadUnsafe)
import GHCJS.DOM (currentWindowUnchecked, currentDocumentUnchecked)
import GHCJS.DOM.Types (toElement)
import GHCJS.DOM.Window (getLocalStorage)
import GHCJS.DOM.Storage (setItem, getItem)
import Foreign.JavaScript.TH(JSContextSingleton(..))
import Reflex.TriggerEvent.Class(TriggerEvent)
import Data.Aeson ()
import Data.Traversable (sequence)

--import Control.Monad.Freer.Writer (tell, runWriter, Writer)
--import Control.Monad.Freer (send, Member, runM)
--import Control.Monad.Freer.Internal  as Freer (Eff(Val, E), decomp, qApp, extract, tsingleton)
--import Data.OpenUnion.Internal  as Union (inj)
import Game.Clicker.Helper
import Game.Clicker.Character

makeCookie :: (MonadWidget t m) => Int -> Dynamic t Price -> m (Amount t, Dynamic t Price)
makeCookie manual_pre sums = getRet $ el' "div" $ do
  cookie_click <- button "Mani wheel"
  clicked <- countDynFrom manual_pre cookie_click
  let cookie = zipDynWith (+) clicked sums
  display cookie
  el "div" $ dynText $ fmap (("manual: "<>). Text.pack . show) clicked
  return (cookie, clicked)

timer :: (MonadWidget t m) => m (Dynamic t Integer)
timer = do
  ct <- liftIO getCurrentTime
  (tick::Dynamic t Integer) <- (fmap (fmap _tickInfo_n) $ tickLossy 1 ct) >>= holdDyn 0
  el "div" $
    dynText $ fmap (\time -> Text.pack $ "総プレイ時間:" <> show time <> "s") tick
  return tick

headWidget :: (MonadWidget t m) => Amount t -> m ()
headWidget cookie = do
  el "title" $ dynText $ fmap toTitle cookie
  return ()

  where toTitle cookie = "(" <> (Text.pack $ show cookie) <> ") Clicker"




--data Fixable r a where
--  MFix :: (a -> Fixable r a) -> Fixable r a
--  LiftEff :: Eff r a -> Fixable r a
--
--instance (Member (Fixable r) r) => MonadFix (Eff r) where
--  -- (a -> Eff r a) -> Eff r a
--  mfix f = send (MFix (LiftEff . f))
--
--handleFixable :: Eff ('[Fixable ('[Widget ()]), Widget ()]) w -> Widget () w
--handleFixable (Val x) = return x
--handleFixable (Freer.E u q) =
--  case decomp u of
--       Right (MFix f) -> do
--         a <- mfix (handleFixable . send . f)
--         r <- handleFixable $ qApp q a
--         return r
--       Right (LiftEff e) -> runM $ e >>= (send . handleFixable . (qApp q))
--       Left u' -> runM $ E u' (tsingleton (send . handleFixable . qApp q))


newGame :: (MonadWidget t m) => Event t () -> Map Text Int -> GameT t m (Amount t, Dynamic t (Map Text Int))
newGame onload savedata = mdo
  tell (("Welcome to Clicker.\n"::Text) <$ onload)
  tell (("Grandma は 買ってから値上げまで2秒かかるのですばやく高速で買い上げると得!\n"::Text) <$ onload)

  (cookie, manual) <- lift $ makeCookie (savedata Map.! "manual") profs

  let nameToChara "grandma" = grandma
      nameToChara "factory" = factory
      nameToChara "gambling" = gambling
      getCharaFromSaved (name) =
        (name,
        nameToChara name (savedata Map.! ("number_"<>name))
                         (savedata Map.! ("profit_"<>name))
                         cookie)

  (chara_dyn_map' :: Map Text (Dynamic t Int, Dynamic t Int)) <- sequence $ Map.fromList $map getCharaFromSaved [
    ("grandma")
    ,("factory")
    ,("gambling")
    ]

  let
    chara_dyn_map = (distributeMapOverDynPure $ singleton "manual" manual) `mappend` (charasDynMap chara_dyn_map')
    profs = fmap (sum . Map.elems) $ fmap (Map.filterWithKey (\k x -> "profit" `Text.isPrefixOf` k)) chara_dyn_map

  --(uniq_grandma, profit_grandma) <- grandma 0 cookie
  --(uniq_factory, profit_factory) <- factory 0 cookie
  --(uniq_gambling, profit_gambling) <- gambling 0 cookie

  return (cookie, chara_dyn_map)

-- 読み込みは?
initialSaveData :: Map Text Int
initialSaveData = Map.fromList
  [("profit_grandma", 0), ("number_grandma", 0)
  ,("profit_factory", 0), ("number_factory", 0)
  ,("profit_gambling", 0), ("number_gambling", 0)
  ,("manual", 0)
  ]

-- (a, Dynamic t b) -> Dynamic t (a, b)
constPairDyn :: Reflex t => (a, Dynamic t b) -> Dynamic t (a, b)
constPairDyn (x, dyn) = zipDyn (constDyn x) dyn

charasDynMap :: (Reflex t) => Map Text (Amount t, Dynamic t Price) -> Dynamic t (Map Text Int)
charasDynMap name_and_dyn =
  fmap Map.fromList
    $ fmap concat $ distributeListOverDyn $ map (fmap toNameSpecAndDyn)
      $ map constPairDyn
        $ Map.toList
          $ fmap (uncurry zipDyn) name_and_dyn
  where
    toNameSpecAndDyn :: (Text, (Int, Price)) -> [(Text, Int)]
    toNameSpecAndDyn (name, (num, prof)) =
      [(("number_" <> name), num), (("profit_" <> name), prof)]

-- TODO: CpSの計算
myWidget :: (MonadWidget t m) => m (Amount t)
myWidget = do
  -- ダブルクリック判定と同様の、連続で買われたかどうかの判定をすると良さそう
  -- ダブルクリック判定は、普通にクリック数をカウントして、一定時間経ったらリセットする
  wallTime <- timer

  localstorage <- liftIO $ do
    window <- currentWindowUnchecked
    localstorage <- getLocalStorage window
    return localstorage

  (previous_save_data :: Maybe (Map Text Int)) <- liftIO $ fmap read <$> getItem localstorage ("savedata"::String)

  ((cookie::Amount t, savedataDyn::Dynamic t (Map Text Int)), log::Event t Text) <- runWriterT $ do
    (onload :: Event t ()) <- lift getPostBuild
    (cookies, savedataDyn) <- newGame onload $ (fromMaybe initialSaveData previous_save_data) `mappend` initialSaveData
    return (cookies, savedataDyn)

  (logAcc::Event t Text) <- accum (<>) "" log
  console <- textArea $ def
    & attributes .~ constDyn ("readonly" =: "readonly" <> "style" =: "width: 500px; height: 200px;")
    & setValue .~ logAcc


  saved <- button "save"
  performEvent $ ffor (tagPromptlyDyn savedataDyn saved) $ \savedata -> liftIO $ do
    setItem localstorage ("savedata"::String) $ Text.pack $ show savedata

  cleared <- button "clear"
  performEvent $ ffor cleared $ \() -> liftIO $ do
    setItem localstorage ("savedata"::String) $ Text.pack $ show initialSaveData


  recovered <- button "write to text"
  savedata <- performEvent $ ffor recovered $ \() -> liftIO $ do
    tx <- fromMaybe "No save data yet." <$> getItem localstorage ("savedata"::String)
    return tx

  dynText =<< (holdDyn "" savedata)
  -- 投資、資本
  -- 借金、ギャンブル、リスク
  -- 破産
  -- 果樹、果実
  -- 欲求
  -- コンソール
  -- 実績解放
  -- TODO: MVCの分離
  -- 運をお金で買う
  -- 曜日ごとに儲かる
  -- 値上げ時間をちょっとあとにしたら? 連打まとめ買いでお得
  return cookie


-- クッキーじゃなくて徳にすることも考えられる
-- マニ車を回す的な
-- 私度商 ～ Private Tokudo Campany
-- 徳の単位ってなんだ。離散量なのか？1菩薩2菩薩がいいのでは。あるいは刹那

-- titleDyn :: MonadWidget t m => Dynamic t Text -> m ()


someFunc :: IO ()
someFunc = mainWidgetWithHead' (headWidget, const myWidget)

-- 経過時間と生産力からクッキーの量を導出
-- 設備の量が途中で変わる場合はどうする
-- save button
-- DONE> TODO 通知 title "(1002)->(2005) Clicker"
-- 型クラスのデフォルトを使って挙動の共通性と例外を管理
--
-- (Monoid a, Monoid b) => Writer (a, b) x みたいにしても、ひとつずつtellすることはできないので、WriterDouble a b x みたいなのを作るか、WriterTを重ねるしかない?
-- aとbの型が違えば、lift二回とかする必要は無いのだろうか?
-- 押し続けると一定時間ごとに買える
