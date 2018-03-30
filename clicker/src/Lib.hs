{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecursiveDo, Rank2Types, FlexibleContexts#-}
module Lib
    ( someFunc
    ) where

import Reflex.Dom as Reflex
  (mainWidget, text, el, elAttr, el', elAttr', MonadWidget, textInput, TextInput(..), dynText, def,
  holdDyn, holdUniqDyn, EventName(Click), domEvent, foldDyn, mapDyn, El, tickLossy, TickInfo(_tickInfo_lastUTC, _tickInfo_n, _tickInfo_alreadyElapsed),  delay, count,  ffilter, FunctorMaybe(fmapMaybe), keypress, display, leftmost, button, simpleList, webSocket, webSocketConfig_send,
  RawWebSocket(..), tag, current, setValue, value, textInputConfig_initialValue, mconcatDyn, combineDyn, attachPromptlyDynWith, zipDynWith, constant,
  Reflex, updated, gate, DomBuilder, splitE,
  MonadHold, hold, tagPromptlyDyn, textArea, textArea_value, TextArea, attributes, constDyn, (=:), textAreaConfig_initialValue, attach, attachWith, getPostBuild, PostBuild, attachWidget, askJSContext, performEvent, ffor, PerformEvent, Performable)
import Reflex.Dom.Main (Widget, mainWidgetWithHead, mainWidgetWithHead')
import Reflex.Class (accum, Dynamic, Event, Behavior, headE)
import Reflex.Spider (SpiderTimeline, Global, Spider)
import qualified Data.Text as Text(pack, lines, unlines)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, NominalDiffTime)
import Data.Text.Internal (Text)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomR, mkStdGen, Random, newStdGen)
import Control.Monad.Fix (MonadFix)
import Control.Monad (void, join)
import Control.Monad.Writer (WriterT, tell, runWriterT, execWriterT)
import Data.Functor ((<$), ($>))
import Control.Monad.Trans (lift)
import Data.Monoid (Sum(Sum, getSum))
import Data.Map as Map (singleton)
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
--import LocalStorage (getLocal, saveLocal)

buttonDyn :: (DomBuilder t m, MonadWidget t m) => Dynamic t Text -> m (Event t ())
buttonDyn t = do
  (e, _) <- el' "button" $ dynText t
  return $ domEvent Click e

buttonDyn' :: (DomBuilder t m, MonadWidget t m) => m a -> m (Event t (), a)
buttonDyn' m = do
  (e, a) <- el' "button" $ m
  return (domEvent Click e, a)

-- https://hackage.haskell.org/package/reflex-gloss-scene-0.1.2/docs/src/Reflex-Gloss-Random.html#foldRand
foldGen :: (Reflex t, MonadHold t m, MonadFix m) => s -> (s -> (a, s)) -> Event t () -> m (Event t a)
foldGen initialState f input = do
  rec
    curState <- hold initialState newState
    let (output, newState) = splitE $ f <$> tag curState input
  return output

foldRandomRs :: (MonadWidget t m, Reflex t, MonadHold t m, MonadFix m, Random a) => (a, a) -> Event t () -> m (Event t a)
foldRandomRs range ev = do
  gen <- liftIO newStdGen
  e <- foldGen gen (randomR range) ev
  return e

type Price = Int
type Ability = Int -> NominalDiffTime -> Price
type Amount t = Dynamic t Int -- クッキー、おばあさん、工場などの数を表すDynamicだが、値段などは含まない
-- 値段もクッキーだけど。

work :: (MonadWidget t m) => Amount t -> Ability -> NominalDiffTime -> m (Dynamic t Int, Event t Int)
work labor ability interval = do
  ct <- liftIO getCurrentTime
  (tick::Event t TickInfo) <- tickLossy interval ct
  let (product::Event t Int) = attachPromptlyDynWith (\n t -> ability n $ _tickInfo_alreadyElapsed t) labor tick
  totalproduct <- foldDyn (+) 0 product
  return (totalproduct, product)

buy :: (Reflex t) => Dynamic t Price -> Amount t -> Event t () -> Event t ()
buy priceDyn cookie buying =
  let isBuyable = (>=) <$> (current cookie) <*> (current priceDyn)
    in gate isBuyable buying

buyDyn :: (MonadWidget t m) => Dynamic t Price -> Amount t -> Event t () -> m (Amount t)
buyDyn priceDyn cookie buyEv = do
  commodity <- count $ buy priceDyn cookie buyEv
  return commodity

consum :: (MonadWidget t m) => Dynamic t Price -> Amount t -> m (Event t Price)
consum priceDyn commodity = accum (+) 0 $ fmap (* (-1)) $ tag (current priceDyn) $ updated commodity

consum1 :: (Reflex t) => Dynamic t Price -> Amount t -> Event t Price
consum1 priceDyn commodity = fmap (* (-1)) $ tag (current priceDyn) $ updated commodity


profit :: (MonadWidget t m) => Amount t -> Ability -> NominalDiffTime -> Dynamic t Price -> m (Event t Int) -- profit = 利潤 = benefit - cost
profit labor ability interval priceDyn = do
  (benefit, mbenefit) <- work labor ability interval
  let mcost = consum1 priceDyn labor
  return $ fmap getSum $ (Sum <$> mcost) <> (Sum <$> mbenefit)

data CommoditySpec = CommoditySpec
  { priceSeq :: PriceSeq
  , workInterval :: NominalDiffTime
  , workAbility :: Ability
  }
-- CpS

-- View Side in Model and View
workerView :: (DomBuilder t m, MonadWidget t m) => Text -> Dynamic t Int -> Dynamic t Price -> Dynamic t Price -> m (El t, Event t ())
workerView name p uniq profit = el' "div" $ do
  text name
  button <- buttonDyn $ fmap (\price -> "buy " <> name <> " ($"<> (Text.pack $ show price) <>" cookies)") p
  dynText $ fmap (\n -> Text.pack $ "数:" <> show n <> "匹います！") uniq
  dynText $ fmap (\prof -> Text.pack $ "総利潤: " <> show prof <> "だよ☆") profit
  return button

getRet :: Functor f => f (a, b) -> f b
getRet = fmap snd


-- begin <workers>
grandma :: (MonadWidget t m) => Amount t -> m (Dynamic t Price)
grandma cookie = mdo
  grandma_button <- getRet $ workerView "Grandma" grandma_price uniq_grandma profit_grandma
  grandma_price' <- delay 2 $ fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) $ updated $ uniq_grandma
  grandma_price <- holdDyn 20 grandma_price'
  (uniq_grandma :: Amount t) <- buyDyn grandma_price cookie grandma_button
  mprofit_grandma <- profit uniq_grandma (const :: Int -> NominalDiffTime -> Int) 1 grandma_price
  profit_grandma <- foldDyn (+) 0 mprofit_grandma
  return profit_grandma

factory :: (MonadWidget t m) => Amount t -> m (Dynamic t Price)
factory cookie = mdo
  factory_button <- getRet $ workerView "factory" factory_price uniq_factory profit_factory
  let factory_price = fmap (\n -> floor $ (50.0::Float) * ((1.15::Float) ^ n)) uniq_factory
  (uniq_factory :: Amount t) <- buyDyn factory_price cookie factory_button
  mprofit_factory <- profit uniq_factory (\n _ -> 10 * n) 3 factory_price
  profit_factory <- foldDyn (+) 0 mprofit_factory
  return profit_factory

data Dice = Gold | Silver | Copper deriving (Eq)-- 出目

gamblingResultStr :: Dice -> Text
gamblingResultStr Gold = "You got Gold!(10% possibility)\n"
gamblingResultStr Silver = "You got Silver(40% possibility)\n"
gamblingResultStr Copper = "You got Copper(50% possibility)\n"

gamblePointing :: Price -> Dice -> Int
gamblePointing priceNow Gold = floor $ ((fromInteger $ toInteger priceNow)::Float) * 2.5
gamblePointing priceNow Silver = floor $ ((fromInteger $ toInteger priceNow)::Float) * 1.5
gamblePointing priceNow Copper = floor $ ((fromInteger $ toInteger priceNow)::Float) * 0.5

gambleSeq :: (MonadWidget t m) => Event t () -> m (Event t Dice)
gambleSeq ev = do
  (randEv :: Event t Int) <- foldRandomRs (0, 10) ev
  return $ fmap gamble_sheet randEv

  where gamble_sheet x | x == 10 = Gold
                       | x < 10 && 6 < x = Silver
                       | otherwise = Copper

gambling :: (MonadWidget t m) => Amount t -> WriterT (Event t Text) m (Dynamic t Price) -- ギャンブル
gambling cookie = do
  (marginal_benefit, gambling_price_original, profit_gambling, results) <- lift $ mdo
    let gambling_price = fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) $ uniq_gambling
    let gambling_price_original = gambling_price
    gambling_button <- getRet $ workerView "gambling" gambling_price uniq_gambling profit_gambling
    (uniq_gambling :: Amount t) <- buyDyn gambling_price cookie gambling_button
    (results :: Event t Dice) <- gambleSeq $ updated $ void uniq_gambling

    -- 限界クッキー
    let marginal_benefit = fmap (uncurry gamblePointing) $ attach (current gambling_price) results

    (gambling_benefit :: Dynamic t Int) <- foldDyn (+) 0 marginal_benefit
    gambling_cost <- foldDyn (+) 0 $ consum1 gambling_price uniq_gambling

    let profit_gambling = zipDynWith (+) gambling_benefit gambling_cost

    return (marginal_benefit, gambling_price_original, profit_gambling, results)


  let gamble_name (x::Int) | x == 10 = "You got Gold!(10% possibility)\n"
                       | x < 10 && 6 < x = "You got Silver(40% possibility)\n"
                       | otherwise = "You got Copper(50% possibility)\n"

-- "「銀」を三回獲得――スキル【強運】を開放します。"
  firstDame <- headE $ gate (current $ fmap ((-50) >=) profit_gambling) ("[破滅Lv1を開放しました]あなたは50以上のクッキーをギャンブルで失いました……賭け金はやればやるほど増えるから、もっとやれば取り返せるかも！がんばれ＞＜\n [Unlock the Vice named 'Ruin Lv.1'] ――You lost 50 cookie by gambling.\n" <$ (updated cookie))
  tell firstDame

  (numOfGold :: Dynamic t Int)<- lift $ count $ ffilter (==Gold) results
  luckyman <- headE $ gate (fmap (==3) $ current $ numOfGold) ("「金」を三回獲得――スキル【強運】を開放します。\n You got three golds ――thus, unlock a skill named 'Lucky'\n" <$ (updated cookie))
  tell luckyman

  tell $ fmap gamblingResultStr results

  let gambling_log = attachWith (\price mb -> Text.pack $ "you get " <> show mb <> " cookies by gambling!(original cookies is: " <> show price <> "cookies)\n") (current gambling_price_original) marginal_benefit
  tell gambling_log

  return profit_gambling

-- Dynamic t [a] -> [Dynamic t a]
-- switch
--debt :: (MonadWidget t m) => Amount t -> m (Dynamic t Price)
--debt cookie = do
--  debt_price <- getRet $ el' "div" $ mdo
--    let debt_price = fmap (\n -> floor $ (-20.0::Float) * ((1.15::Float) ^ n)) uniq_debt -- 借金
--    debt_button <- lift $ getRet $ workerView "借金" debt_price uniq_debt uniq_debt -- buttonDyn で返済ボタン
--    (uniq_debt :: Amount t) <- lift $ buyDyn debt_price cookie debt_button
--    (priceHistory::Dynamic t [Int]) <- lift $ accum (\x y -> y:x) [] $ updated debt_price
--    (paybackButtons::Dynamic t [(El t, Int)]) <- lift $ simpleList priceHistory $
--       \m -> el' "button" $ display $ fmap (Text.pack . ("debt: "++) . show . (/1.15)) $ m
--    return debt_price
--  accum (+) 0 (updated debt_price)
-- end </workers>

-- 徐々に出てくるようにする。いきなり多額の借金は負えない
-- 借金、返せるか返せないかギリギリのところを狙う。マイナスになったら破産して死。
-- 前方報酬、後方苦痛、依存性
-- Trap -- 最初だけ快楽がある

makeCookie :: (MonadWidget t m) => [Dynamic t Price] -> m (Amount t)
makeCookie sums = getRet $ el' "div" $ do
  cookie_click <- button "Mani wheel"
  clicked <- count cookie_click
  let cookie = foldl1 (zipDynWith (+)) $ clicked:sums
  display cookie
  el "div" $ dynText $ fmap (("manual: "<>). Text.pack . show) clicked
  return cookie

timer :: (MonadWidget t m) => m (Dynamic t Integer)
timer = do
  ct <- liftIO getCurrentTime
  (tick::Dynamic t Integer) <- (fmap (fmap _tickInfo_n) $ tickLossy 1 ct) >>= holdDyn 0
  el "div" $
    dynText $ fmap (\time -> Text.pack $ "総プレイ時間:" <> show time <> "s") tick
  return tick

-- 値段 = 初期値と値上げ率から定まる等比数列
data PriceSeq = PriceSeq {
  initialPrice :: Price,
  markupPercent :: Float
  }

headWidget :: (MonadWidget t m) => Amount t -> m ()
headWidget cookie = do
  el "title" $ dynText $ fmap toTitle cookie
  return ()

  where toTitle cookie = "(" <> (Text.pack $ show cookie) <> ") Clicker"

-- CpSの計算
myWidget :: (MonadWidget t m, PerformEvent t m) => m (Amount t)
myWidget = do
  -- ダブルクリック判定と同様の、連続で買われたかどうかの判定をすると良さそう
  -- ダブルクリック判定は、普通にクリック数をカウントして、一定時間経ったらリセットする

  wallTime <- timer

  (cookie, log) <- runWriterT $ mdo
    (onload :: Event t ()) <- lift getPostBuild
    tell ("Welcome to Clicker.\n" <$ onload)
    tell ("Grandma は 買ってから値上げまで2秒かかるのですばやく高速で買い上げると得!\n" <$ onload)

    cookie <- lift $ makeCookie [profit_grandma, profit_factory, profit_gambling]
    profit_grandma <- lift $ grandma cookie
    profit_factory <- lift $ factory cookie
    profit_gambling <- gambling cookie

    return cookie

  (logAcc::Event t Text) <- accum (<>) "" log
  console <- textArea $ def
    & attributes .~ constDyn ("readonly" =: "readonly" <> "style" =: "width: 500px; height: 200px;")
    & setValue .~ logAcc

  localstorage <- liftIO $ do
    window <- currentWindowUnchecked
    localstorage <- getLocalStorage window
    return localstorage

  saved <- button "save"
  performEvent $ ffor (tagPromptlyDyn cookie saved) $ \cookie -> liftIO $ do
    setItem localstorage ("saveLocation"::String) $ Text.pack $ show cookie

  recovered <- button "recover"
  savedata <- performEvent $ ffor recovered $ \() -> liftIO $ do
    Just tx <- getItem localstorage ("saveLocation"::String)
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
