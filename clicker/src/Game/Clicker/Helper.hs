{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecursiveDo, Rank2Types, LambdaCase#-}
module Game.Clicker.Helper where

import Reflex.Dom as Reflex
  (mainWidget, text, el, elAttr, el', elAttr', MonadWidget, textInput, TextInput(..), dynText, def,
  holdDyn, holdUniqDyn, EventName(Click), domEvent, foldDyn, mapDyn, El, tickLossy, TickInfo(_tickInfo_lastUTC, _tickInfo_n, _tickInfo_alreadyElapsed),  delay, count,  ffilter, FunctorMaybe(fmapMaybe), keypress, display, leftmost, button, simpleList, webSocket, webSocketConfig_send,
  RawWebSocket(..), tag, current, setValue, value, textInputConfig_initialValue, mconcatDyn, combineDyn, attachPromptlyDynWith, zipDynWith, constant,
  Reflex, updated, gate, DomBuilder, splitE,
  MonadHold, hold, tagPromptlyDyn, textArea, textArea_value, TextArea, attributes, constDyn, (=:), textAreaConfig_initialValue, attach, attachWith, getPostBuild, PostBuild, attachWidget, askJSContext, performEvent, ffor, PerformEvent, Performable)
import Reflex.Dom.Main (Widget, mainWidgetWithHead, mainWidgetWithHead')
import Reflex.Class (accum, Dynamic, Event, Behavior, headE, alignEventWithMaybe)
import Data.These (These(This, That, These))
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

type GameT t m a = WriterT (Event t Text) m a

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

buyDyn :: (MonadWidget t m) => Int -> Dynamic t Price -> Amount t -> Event t () -> m (Amount t)
buyDyn initalNum priceDyn cookie buyEv = do
  commodity <- countDynFrom initalNum $ buy priceDyn cookie buyEv
  return commodity


consum1 :: (Reflex t) => Dynamic t Price -> Amount t -> Event t Price
consum1 priceDyn commodity = fmap (* (-1)) $ tag (current priceDyn) $ updated commodity

consum :: (MonadWidget t m) => Dynamic t Price -> Amount t -> m (Event t Price)
consum priceDyn commodity = accum (+) 0 $ consum1 priceDyn commodity

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
--
-- 値段 = 初期値と値上げ率から定まる等比数列
data PriceSeq = PriceSeq {
  initialPrice :: Price,
  markupPercent :: Float
  }

-- View Side in Model and View
workerView :: (DomBuilder t m, MonadWidget t m) => Text -> Dynamic t Int -> Dynamic t Price -> Dynamic t Price -> Event t Price -> m (El t, Event t ())
workerView name p uniq profit mprofit = el' "div" $ do
  text name
  button <- buttonDyn $ fmap (\price -> "buy " <> name <> " ($"<> (Text.pack $ show price) <>" cookies)") p
  dynText $ fmap (\n -> Text.pack $ "数:" <> show n <> "匹います！") uniq
  dynText $ fmap (\prof -> Text.pack $ "総利潤: " <> show prof <> "だよ☆") profit
  mprofit_dyn <- (holdDyn 0 mprofit)
  dynText $ (fmap (\prof -> Text.pack $ "現刹那当たり利潤" <> show prof <> "だよ☆")) mprofit_dyn
  return button

getRet :: Functor f => f (a, b) -> f b
getRet = fmap snd

countDynFrom :: (MonadWidget t m) => Int -> Event t a -> m(Dynamic t Int)
countDynFrom n = foldDyn (\_ i -> succ i) n

perSecond :: (MonadWidget t m, Monoid a) => Event t a -> m (Dynamic t a)
perSecond ev = do
  ct <- liftIO getCurrentTime

  (tick::Event t Integer) <- (fmap (fmap _tickInfo_n) $ tickLossy 1 ct)
  foldDyn (\(m::Maybe a) (n::a) -> case m of { Nothing -> (mempty::a) ; Just a -> a `mappend` n}) mempty $ alignEventWithMaybe (\case This a -> Just (Just a); That _ -> Just Nothing; These _ _ -> Just Nothing) ev tick
