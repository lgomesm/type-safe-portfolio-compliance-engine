{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Domain.HListSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Domain.HList (HList (HNil, (:&)))

spec :: Spec
spec = describe "Domain.HList" $ do
  it "represents an empty heterogeneous schema" $
    labelsForEmpty HNil `shouldBe` []

  it "accepts values with different types" $
    labelsForExample (42 :& ("abc" :: Text) :& True :& HNil)
      `shouldBe` ["42", "abc", "True"]

  it "preserves the order declared by the type-level schema" $
    labelsForExample (7 :& ("second" :: Text) :& False :& HNil)
      `shouldBe` ["7", "second", "False"]

  it "recovers the expected type at each position through pattern matching" $
    labelsForExample (1 :& ("two" :: Text) :& True :& HNil)
      `shouldBe` ["1", "two", "True"]

  it "terminates at HNil without adding an extra item" $
    labelsForExample (0 :& ("" :: Text) :& False :& HNil)
      `shouldBe` ["0", "", "False"]

labelsForEmpty :: HList '[] -> [Text]
labelsForEmpty HNil = []

labelsForExample :: HList '[Int, Text, Bool] -> [Text]
labelsForExample (number :& textValue :& boolean :& HNil) =
  [ T.pack (show number)
  , textValue
  , T.pack (show boolean)
  ]