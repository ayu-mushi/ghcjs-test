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
  sample, PushM, Reflex, updated, gate, DomBuilder)
import qualified Data.Text as Text(pack)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, NominalDiffTime)
import Data.Text.Internal (Text)
import Control.Monad.IO.Class (liftIO)

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
  cookie_click <- button "get cookie"
  (cookie :: Amount t) <- count cookie_click >>= \sumcookie -> return $ foldl1 (zipDynWith (+)) [sumcookie, profit_grandma, profit_factory]
  display cookie

  grandma_button <- buttonDyn $ fmap (\price -> Text.pack ("buy grandma ($"++ show price ++" cookies)")) grandma_price

  let grandma_price = fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) uniq_grandma
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
  let gambling_price = fmap (\n -> floor $ (20.0::Float) * ((1.15::Float) ^ n)) uniq_gambling
  gambling_button <- buttonDyn $ fmap (\price -> Text.pack ("gambling ($"++ show price ++" cookies)")) gambling_price
  (uniq_gambling :: Amount t) <- buyDyn gambling_price cookie gambling_button

  dynText $ fmap (\n -> Text.pack $ "数:" ++ show n ++ "匹います！") uniq_gambling

  -- TODO: MVCの分離
  -- 運をお金で買う
  -- "「銀」を三回獲得――スキル【強運】を開放します。"
  return ()

someFunc :: IO ()
someFunc = mainWidget myWidget
