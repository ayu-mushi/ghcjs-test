{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecursiveDo  #-}

module Lib
    ( someFunc
    ) where

import Reflex.Dom as Reflex
  (mainWidget, text, el, elAttr, el', elAttr', MonadWidget, textInput, TextInput(..), dynText, def,
  holdDyn, holdUniqDyn, EventName(Click), domEvent, foldDyn, mapDyn, El, tickLossy, TickInfo(_tickInfo_lastUTC, _tickInfo_n, _tickInfo_alreadyElapsed), Event, delay, count, Dynamic, ffilter, FunctorMaybe(fmapMaybe), keypress, display, leftmost, button, simpleList, webSocket, webSocketConfig_send,
  RawWebSocket(..), tag, current, setValue, value, textInputConfig_initialValue, foldDynM, mconcatDyn, combineDyn, attachPromptlyDynWith, zipDynWith, constant,
  sample, PushM, Reflex, updated, gate, DomBuilder, splitE,
  MonadHold, hold, tagPromptlyDyn)
import qualified Data.Text as Text(pack)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, NominalDiffTime)
import Data.Text.Internal (Text)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomR, mkStdGen, Random, newStdGen)
import Control.Monad.Fix (MonadFix)
import Control.Monad (void)
import Data.Monoid ((<>))

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

myWidget :: (MonadWidget t m) => m ()
myWidget = mdo

  text "Grandma は 買ってから値上げまで2秒かかるのですばやく高速で買い上げると得!"
  -- ダブルクリック判定と同様の、連続で買われたかどうかの判定をすると良さそう
  ct <- liftIO getCurrentTime
  (tick::Dynamic t Integer) <- (fmap (fmap _tickInfo_n) $ tickLossy 1 ct) >>= holdDyn 0
  dynText $ fmap (\time -> Text.pack $ "プレイ総時間:" <> show time) tick

  cookie_click <- button "get cookie"
  (cookie :: Amount t) <- count cookie_click >>= \sumcookie -> return $ foldl1 (zipDynWith (+)) [sumcookie, profit_grandma, profit_factory, profit_gambling]
  display cookie
  --(holdDyn 0 $ tagPromptlyDyn cookie (updated tick)) >>= display

  grandma_button <- buttonDyn $ fmap (\price -> Text.pack ("buy grandma ($"++ show price ++" cookies)")) grandma_price

  grandma_price' <- delay 2 $ fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) $ updated $ uniq_grandma
  grandma_price <- holdDyn 20 grandma_price'
  (uniq_grandma :: Amount t) <- buyDyn grandma_price cookie grandma_button
  profit_grandma <- profit uniq_grandma (const :: Int -> NominalDiffTime -> Int) 1 grandma_price

  dynText $ fmap (\n -> Text.pack $ "数:" ++ show n ++ "匹います！") uniq_grandma
  dynText $ fmap (\prof -> Text.pack $ "利潤: " ++ show prof ++ "だよ☆") profit_grandma

  -- 減価償却費
  -- 借金、ギャンブル、リスク
  -- 破産

  factory_button <- buttonDyn $ fmap (\price -> Text.pack ("buy factory ($"++ show price ++" cookies)")) factory_price

  let factory_price = fmap (\n -> floor $ (50.0::Float) * ((1.15::Float) ^ n)) uniq_factory
  (uniq_factory :: Amount t) <- buyDyn factory_price cookie factory_button
  profit_factory <- profit uniq_factory (\n _ -> 10 * n) 3 factory_price

  dynText $ fmap (\n -> Text.pack $ "数:" ++ show n ++ "匹います！") uniq_factory
  dynText $ fmap (\prof -> Text.pack $ "利潤: " ++ show prof ++ "だよ☆") profit_factory

  -- 借金
  let debt_price = fmap (\n -> floor $ (-20.0::Float) * ((1.15::Float) ^ n)) uniq_debt
  debt_button <- buttonDyn $ fmap (\price -> Text.pack ("借金debt ($"++ show price ++" cookies)")) debt_price
  (uniq_debt :: Amount t) <- buyDyn debt_price cookie debt_button

  dynText $ fmap (\n -> Text.pack $ "数:" ++ show n ++ "匹います！") uniq_debt

  -- ギャンブル
  let gambling_price = fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) $ uniq_gambling
  gambling_button <- buttonDyn $ fmap (\price -> Text.pack ("gambling ($"++ show price ++" cookies)")) gambling_price
  (uniq_gambling :: Amount t) <- buyDyn gambling_price cookie gambling_button
  (randEv :: m (Event t Int)) <- liftIO $ foldRandomRs (0, 10) (updated $ void $ uniq_gambling)
  (randEv' :: Event t Int) <- randEv
  (gambling_benefit :: Dynamic t Int) <- foldDyn (+) 0 $ fmap gamble_sheet randEv'
  gambling_cost <- consum gambling_price uniq_gambling
  let profit_gambling = (+) <$> gambling_benefit <*> gambling_cost

  dynText $ fmap (\prof -> Text.pack $ "利潤: " ++ show prof ++ "だよ☆") profit_gambling


  -- TODO: MVCの分離
  -- 運をお金で買う
  -- "「銀」を三回獲得――スキル【強運】を開放します。"
  -- 曜日ごとに儲かる
  -- 値上げ時間をちょっとあとにしたら? 連打まとめ買いでお得
  return ()
    where
      gamble_sheet x
        | x == 10 = 50
        | x < 10 && 6 < x = 40
        | otherwise = 20

-- https://hackage.haskell.org/package/reflex-gloss-scene-0.1.2/docs/src/Reflex-Gloss-Random.html#foldRand
foldGen :: (Reflex t, MonadHold t m, MonadFix m) => s -> (s -> (a, s)) -> Event t () -> m (Event t a)
foldGen initialState f input = do
  rec
    curState <- hold initialState newState
    let (output, newState) = splitE $ f <$> tag curState input
  return output

foldRandomRs :: (Reflex t, MonadHold t m, MonadFix m, Random a) => (a, a) -> Event t () -> IO(m (Event t a))
foldRandomRs range ev = do
  gen <- newStdGen
  return (foldGen gen (randomR range) ev)

someFunc :: IO ()
someFunc = mainWidget myWidget
