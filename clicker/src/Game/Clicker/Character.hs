{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecursiveDo, Rank2Types#-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE TypeOperators, DataKinds, GADTs #-}

module Game.Clicker.Character (grandma, factory, gambling) where
import Game.Clicker.Helper

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
import Control.Monad.Writer (tell, runWriter, Writer, WriterT)
import Data.Functor ((<$), ($>))
import Control.Monad.Trans (lift)
import Data.Monoid (Sum(Sum, getSum))
import Data.Map as Map (singleton, Map, insert, (!), fromList)
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
--import Control.Monad.Freer (send, Member, Eff)
--import Control.Monad.Freer.Writer (tell, runWriter, Writer)
import Reflex.Dom.Builder.Immediate (GhcjsDomSpace)
import Reflex.Dom.Builder.Class (DomBuilderSpace)

-- begin <workers>

grandma :: (MonadWidget t m) => Int -> Price -> Amount t -> GameT t m (Amount t, Dynamic t Price)
grandma initialNum initialProfit cookie = lift $ mdo
  grandma_button <- getRet $ workerView "Grandma" grandma_price uniq_grandma profit_grandma mprofit_grandma
  grandma_price' <- delay 2 $ fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) $ updated $ uniq_grandma
  grandma_price <- holdDyn (floor $ (20.0::Float) * ((1.15::Float) ^ initialNum) ) grandma_price'
  (uniq_grandma :: Amount t) <- buyDyn initialNum grandma_price cookie grandma_button
  mprofit_grandma <- profit uniq_grandma (const :: Int -> NominalDiffTime -> Int) 1 grandma_price
  profit_grandma <- foldDyn (+) initialProfit mprofit_grandma
  return (uniq_grandma, profit_grandma)

factory :: (MonadWidget t m) => Int -> Price -> Amount t -> GameT t m (Amount t, Dynamic t Price)
factory initialNum initialProfit cookie = lift $ mdo
  factory_button <- getRet $ workerView "factory" factory_price uniq_factory profit_factory mprofit_factory
  let factory_price = fmap (\n -> floor $ (50.0::Float) * ((1.15::Float) ^ n)) uniq_factory
  (uniq_factory :: Amount t) <- buyDyn initialNum factory_price cookie factory_button
  mprofit_factory <- profit uniq_factory (\n _ -> 10 * n) 3 factory_price
  profit_factory <- foldDyn (+) initialProfit mprofit_factory
  return (uniq_factory, profit_factory)

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

-- type Widget x = PostBuildT Spider (ImmediateDomBuilderT Spider (WithJSContextSingleton x (PerformEventT Spider (SpiderHost Global))))

gambling :: (MonadWidget t m) => Int -> Price -> Amount t -> GameT t m (Amount t, Dynamic t Price) -- ギャンブル
gambling initialNum initialProfit cookie = do
  (mprofit_gambling, marginal_benefit, gambling_price_original, profit_gambling, results, uniq_gambling) <- lift $ mdo
    let gambling_price = fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) $ uniq_gambling
    let gambling_price_original = gambling_price
    gambling_button <- getRet $ (workerView "gambling" gambling_price uniq_gambling profit_gambling mprofit_gambling)
    (uniq_gambling :: Amount t)  <- buyDyn initialNum gambling_price cookie gambling_button
    (results :: Event t Dice) <- gambleSeq $ updated $ void uniq_gambling

    -- 限界クッキー
    let marginal_benefit = fmap (uncurry gamblePointing) $ attach (current gambling_price) results
    let mprofit_gambling = fmap getSum $ (Sum <$> consum1 gambling_price uniq_gambling) <> (Sum <$> marginal_benefit)
    profit_gambling <- foldDyn (+) initialProfit mprofit_gambling

    return (mprofit_gambling, marginal_benefit, gambling_price_original, profit_gambling, results, uniq_gambling)

  let gamble_name (x::Int) | x == 10 = "You got Gold!(10% possibility)\n"
                       | x < 10 && 6 < x = "You got Silver(40% possibility)\n"
                       | otherwise = "You got Copper(50% possibility)\n"

-- "「銀」を三回獲得――スキル【強運】を開放します。"
  firstDame <-  lift $ headE $ gate (current $ fmap ((-50) >=) profit_gambling) ("[破滅Lv1を開放しました]あなたは50以上のクッキーをギャンブルで失いました……賭け金はやればやるほど増えるから、もっとやれば取り返せるかも！がんばれ＞＜\n [Unlock the Vice named 'Ruin Lv.1'] ――You lost 50 cookie by gambling. Do your best!!!!!\n" <$ (updated cookie))
  tell firstDame

  (numOfGold :: Dynamic t Int)<- lift $count $ ffilter (==Gold) results
  (luckyman :: Event t Text) <- lift $headE $ gate (fmap (==3) $ current $ numOfGold) (("「金」を三回獲得――スキル【強運】を開放します。\n You have got three golds ――thus, unlock a Skill named 'Lucky'.\n"::Text) <$ (updated cookie))
  tell luckyman

  tell $ fmap gamblingResultStr results

  let gambling_log = attachWith (\price mb -> Text.pack $ "you get " <> show mb <> " cookies by gambling!(original cookies is: " <> show price <> "cookies)\n") (current gambling_price_original) marginal_benefit
  tell gambling_log

  return (uniq_gambling, profit_gambling)


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
