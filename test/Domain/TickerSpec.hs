{-# LANGUAGE OverloadedStrings #-}

module Domain.TickerSpec (spec) where

import Test.Hspec

import Domain.Error (DomainError (..))
import Domain.Ticker (mkTicker, unTicker)

spec :: Spec
spec = describe "Domain.Ticker" $ do
  describe "mkTicker" $ do
    it "accepts a non-empty ticker" $
      fmap unTicker (mkTicker "PETR4") `shouldBe` Right "PETR4"

    it "rejects an empty string" $
      mkTicker "" `shouldBe` Left EmptyTicker

    it "rejects a blank string" $
      mkTicker "   " `shouldBe` Left EmptyTicker

    it "trims surrounding spaces from a valid ticker" $
      fmap unTicker (mkTicker "  BTC  ") `shouldBe` Right "BTC"

    it "unTicker returns the validated ticker text" $
      case mkTicker "PETR4" of
        Right tickerValue ->
          unTicker tickerValue `shouldBe` "PETR4"
        Left err ->
          expectationFailure (show err)