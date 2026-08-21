{-# LANGUAGE OverloadedStrings #-}

module Domain.TickerPropSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

import Domain.Error (DomainError (..))
import Domain.Ticker (mkTicker, unTicker)
import Generators.Domain (genValidTicker, shrinkValidTicker)

spec :: Spec
spec = describe "Domain.Ticker (properties)" $ modifyMaxSuccess (const 200) $ do
  it "every generated ticker is non-empty after validation" $
    forAllShrink genValidTicker shrinkValidTicker $ \tickerValue ->
      not (T.null (unTicker tickerValue))

  it "mkTicker is idempotent over already validated ticker text" $
    forAllShrink genValidTicker shrinkValidTicker $ \tickerValue ->
      mkTicker (unTicker tickerValue) === Right tickerValue

  it "blank-only input is always rejected with EmptyTicker" $
    forAll (listOf1 (elements [' ', '\t'])) $ \rawBlank ->
      mkTicker (T.pack rawBlank) === Left EmptyTicker