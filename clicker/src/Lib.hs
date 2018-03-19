{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecursiveDo  #-}
module Lib
    ( someFunc
    ) where

import Reflex.Dom as Reflex
  (mainWidget, text, el, elAttr, el', elAttr', MonadWidget, textInput, TextInput(..), dynText, def,
  holdDyn, holdUniqDyn, EventName(Click), domEvent, foldDyn, mapDyn, El, tickLossy, TickInfo(_tickInfo_lastUTC, _tickInfo_n, _tickInfo_alreadyElapsed),  delay, count,  ffilter, FunctorMaybe(fmapMaybe), keypress, display, leftmost, button, simpleList, webSocket, webSocketConfig_send,
  RawWebSocket(..), tag, current, setValue, value, textInputConfig_initialValue, mconcatDyn, combineDyn, attachPromptlyDynWith, zipDynWith, constant,
  Reflex, updated, gate, DomBuilder, splitE,
  MonadHold, hold, tagPromptlyDyn, textArea, textArea_value, TextArea, attributes, constDyn, (=:), textAreaConfig_initialValue, attach, attachWith, getPostBuild)
import Reflex.Class (accum, Dynamic, Event, Behavior)
import qualified Data.Text as Text(pack, lines, unlines)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, NominalDiffTime)
import Data.Text.Internal (Text)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomR, mkStdGen, Random, newStdGen)
import Control.Monad.Fix (MonadFix)
import Control.Monad (void, join)
import Data.Functor ((<$), ($>))
import Control.Monad.Trans (lift)
import Data.Monoid ()
import Data.Map as Map (singleton)
import Control.Lens (Lens', (&), (.~))
import Control.Lens.Iso(iso, Iso')
import Data.Semigroup (Semigroup, (<>))

buttonDyn :: (DomBuilder t m, MonadWidget t m) => Dynamic t Text -> m (Event t ())
buttonDyn t = do
  (e, _) <- el' "button" $ dynText t
  return $ domEvent Click e

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

buy :: (Reflex t) => Dynamic t Price -> Amount t -> Event t () -> Event t ()
buy priceDyn cookie buying =
  let isBuyable = (>=) <$> (current cookie) <*> (current priceDyn)
    in gate isBuyable buying

buyDyn :: (MonadWidget t m) => Dynamic t Price -> Amount t -> Event t () -> m (Amount t)
buyDyn priceDyn cookie buyEv = do
  commodity <- count $ buy priceDyn cookie buyEv
  return commodity

consum :: (MonadWidget t m) => Dynamic t Price -> Amount t -> m (Dynamic t Price)
consum priceDyn commodity = foldDyn (+) 0 $ fmap (* (-1)) $ tag (current priceDyn) $ updated commodity

profit :: (MonadWidget t m) => Amount t -> Ability -> NominalDiffTime -> Dynamic t Price -> m (Dynamic t Int) -- profit = 利潤 = benefit - cost
profit labor ability interval priceDyn = do
  benefit <- work labor ability interval
  cost <- consum priceDyn labor
  return $ zipDynWith (+) cost benefit

data CommoditySpec = CommoditySpec
  { priceSeq :: PriceSeq
  , workInterval :: NominalDiffTime
  , workAbility :: Ability
  }
-- CpS

-- View Side in Model and View
workerView :: (DomBuilder t m, MonadWidget t m) => String -> Dynamic t Int -> Dynamic t Price -> Dynamic t Price -> m (El t, Event t ())
workerView name p uniq profit = el' "div" $ do
  button <- buttonDyn $ fmap (\price -> Text.pack ("buy " ++ name ++ " ($"++ show price ++" cookies)")) p
  dynText $ fmap (\n -> Text.pack $ "数:" ++ show n ++ "匹います！") uniq
  dynText $ fmap (\prof -> Text.pack $ "利潤: " ++ show prof ++ "だよ☆") profit
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
  profit_grandma <- profit uniq_grandma (const :: Int -> NominalDiffTime -> Int) 1 grandma_price
  return profit_grandma

factory :: (MonadWidget t m) => Amount t -> m (Dynamic t Price)
factory cookie = mdo
  factory_button <- getRet $ workerView "factory" factory_price uniq_factory profit_factory
  let factory_price = fmap (\n -> floor $ (50.0::Float) * ((1.15::Float) ^ n)) uniq_factory
  (uniq_factory :: Amount t) <- buyDyn factory_price cookie factory_button
  profit_factory <- profit uniq_factory (\n _ -> 10 * n) 3 factory_price
  return profit_factory

gambling :: (MonadWidget t m) => Amount t -> m (Dynamic t Price, Event t Log) -- ギャンブル
gambling cookie = mdo
  let gambling_price = fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) $ uniq_gambling
  let gambling_price_original = gambling_price
  gambling_button <- getRet $ workerView "gambling" gambling_price uniq_gambling profit_gambling
  (uniq_gambling :: Amount t) <- buyDyn gambling_price cookie gambling_button
  (randEv :: Event t Int) <- foldRandomRs (0, 10) (updated $ void $ uniq_gambling)

  let gamble_sheet (priceNow::Price) (x::Int) | x == 10 = floor $ ((fromInteger $ toInteger priceNow)::Float) * 2.5
                     | x < 10 && 6 < x = priceNow * 2
                     | otherwise = floor $ ((fromInteger $ toInteger priceNow)::Float) * 0.5

  let marginal_benefit = fmap (uncurry gamble_sheet) $ attach (current gambling_price) randEv

  (gambling_benefit :: Dynamic t Int) <- foldDyn (+) 0 marginal_benefit
  gambling_cost <- consum gambling_price uniq_gambling

  let profit_gambling = (+) <$> gambling_benefit <*> gambling_cost

  let gambling_log = attachWith (\price mb -> Text.pack $ "you get " <> show mb <> " cookies by gambling!(original cookies is: " <> show price <> "cookies)") (current gambling_price_original) marginal_benefit
  return (profit_gambling, fmap Log $ gambling_log)

-- end </workers>


makeCookie :: (MonadWidget t m) => [Dynamic t Price] -> m (Amount t)
makeCookie sums = getRet $ el' "div" $ do
  cookie_click <- button "get cookie"
  (cookie :: Amount t) <- count cookie_click >>= \sumcookie -> return $ foldl1 (zipDynWith (+)) $ sumcookie:sums
  display cookie
  -- (holdDyn 0 $ tagPromptlyDyn cookie (updated tick)) >>= display -- 1秒ごとにdisplayを更新
  return cookie

timer :: (MonadWidget t m) => m (Dynamic t Integer)
timer = do
  ct <- liftIO getCurrentTime
  (tick::Dynamic t Integer) <- (fmap (fmap _tickInfo_n) $ tickLossy 1 ct) >>= holdDyn 0
  dynText $ fmap (\time -> Text.pack $ "プレイ総時間:" <> show time) tick
  return tick

textArea_lines :: Reflex t => Lens' (TextArea t) (Dynamic t [Text])
textArea_lines = textArea_value . liner
  where
    liner :: Reflex t => Iso' (Dynamic t Text) (Dynamic t [Text])
    liner = iso (fmap Text.lines) (fmap Text.unlines)

-- 値段 = 初期値と値上げ率から定まる等比数列
data PriceSeq = PriceSeq {
  initialPrice :: Price,
  markupPercent :: Float
  }

newtype Log = Log { unLog :: Text } deriving Show

instance Semigroup Log where
  Log "" <> Log y = Log $ y
  Log x <> Log "" = Log $ x
  Log x <> Log y = Log $ x <> "\n" <> y

instance Monoid Log where
  mempty = Log ""

myWidget :: (MonadWidget t m) => m ()
myWidget = do
  text "Grandma は 買ってから値上げまで2秒かかるのですばやく高速で買い上げると得!"
  -- ダブルクリック判定と同様の、連続で買われたかどうかの判定をすると良さそう
  timer

  rec
    cookie <- makeCookie [profit_grandma, profit_factory, profit_gambling]
    profit_grandma <- grandma cookie
    profit_factory <- factory cookie
    (profit_gambling, gambling_log) <- gambling cookie

    let debt_price = fmap (\n -> floor $ (-20.0::Float) * ((1.15::Float) ^ n)) uniq_debt -- 借金
    debt_button <- getRet $ workerView "借金" debt_price uniq_debt uniq_debt -- buttonDyn で返済ボタン
    (uniq_debt :: Amount t) <- buyDyn debt_price cookie debt_button

    (onload :: Event t ()) <- getPostBuild
    (log::Event t Log) <- accum (<>) mempty $ (Log "Welcome to Clicker." <$ onload) <> gambling_log
    console <- textArea $ def
      & attributes .~ constDyn ("readonly" =: "readonly" <> "style" =: "width: 500px; height: 200px;")
      & setValue .~ (fmap unLog log)

  -- 投資、資本
  -- 借金、ギャンブル、リスク
  -- 破産
  -- 果樹、果実
  -- 欲求
  -- コンソール
  -- 実績解放
  -- TODO: MVCの分離
  -- 運をお金で買う
  -- "「銀」を三回獲得――スキル【強運】を開放します。"
  -- 曜日ごとに儲かる
  -- 値上げ時間をちょっとあとにしたら? 連打まとめ買いでお得
  return ()

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

someFunc :: IO ()
someFunc = mainWidget myWidget
