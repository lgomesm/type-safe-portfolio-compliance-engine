{-# LANGUAGE OverloadedStrings #-}

module Domain.CustomerIdSpec (spec) where

import Test.Hspec

import Domain.CustomerId (mkCustomerId, unCustomerId)
import Domain.Error (DomainError (..))

spec :: Spec
spec = describe "Domain.CustomerId" $ do
  it "accepts non-empty identifiers" $
    fmap unCustomerId (mkCustomerId "customer-001") `shouldBe` Right "customer-001"

  it "trims surrounding whitespace before validating" $
    fmap unCustomerId (mkCustomerId "  abc  ") `shouldBe` Right "abc"

  it "rejects empty identifiers" $
    mkCustomerId "" `shouldBe` Left EmptyCustomerId

  it "rejects identifiers made only of whitespace" $
    mkCustomerId "   " `shouldBe` Left EmptyCustomerId