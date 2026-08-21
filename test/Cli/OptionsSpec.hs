module Cli.OptionsSpec (spec) where

import Test.Hspec

import Cli.Options
  ( CliError (..)
  , CliOptions (..)
  , InputSource (..)
  , parseCliOptions
  , parseLocality
  )
import Risk.Policy (Locality (..))

spec :: Spec
spec = describe "Cli.Options" $ do
  describe "parseLocality" $ do
    it "converts Brazil and Chile into the closed locality type" $ do
      parseLocality "brazil" `shouldBe` Right Brazil
      parseLocality "chile" `shouldBe` Right Chile

    it "accepts case variations at the textual boundary" $ do
      parseLocality "Brazil" `shouldBe` Right Brazil
      parseLocality "BRAZIL" `shouldBe` Right Brazil
      parseLocality "Chile" `shouldBe` Right Chile
      parseLocality "CHILE" `shouldBe` Right Chile

    it "rejects unsupported and empty localities" $ do
      parseLocality "argentina" `shouldBe` Left (UnknownLocality "argentina")
      parseLocality "" `shouldBe` Left (UnknownLocality "")

  describe "parseCliOptions" $ do
    it "preserves the legacy scenario-only invocation" $
      parseCliOptions ["approved"]
        `shouldBe` Right (CliOptions (BuiltInScenario "approved") Nothing Nothing)

    it "parses locality and top count in either order" $ do
      parseCliOptions ["approved", "--locality", "chile"]
        `shouldBe` Right (CliOptions (BuiltInScenario "approved") Nothing (Just Chile))
      parseCliOptions ["approved", "--locality", "chile", "--top", "2"]
        `shouldBe` Right (CliOptions (BuiltInScenario "approved") (Just 2) (Just Chile))
      parseCliOptions ["approved", "--top", "2", "--locality", "chile"]
        `shouldBe` Right (CliOptions (BuiltInScenario "approved") (Just 2) (Just Chile))

    it "parses a JSON file as the exclusive input source in any flag order" $ do
      parseCliOptions ["--input", "portfolio.json"]
        `shouldBe` Right (CliOptions (JsonFile "portfolio.json") Nothing Nothing)
      parseCliOptions ["--input", "portfolio.json", "--top", "3", "--locality", "chile"]
        `shouldBe` Right (CliOptions (JsonFile "portfolio.json") (Just 3) (Just Chile))
      parseCliOptions ["--top", "3", "--locality", "chile", "--input", "portfolio.json"]
        `shouldBe` Right (CliOptions (JsonFile "portfolio.json") (Just 3) (Just Chile))

    it "requires exactly one source and rejects competing sources" $ do
      parseCliOptions [] `shouldBe` Left MissingInputSource
      parseCliOptions ["approved", "--input", "portfolio.json"]
        `shouldBe` Left MultipleInputSources
      parseCliOptions ["--input", "portfolio.json", "approved"]
        `shouldBe` Left MultipleInputSources

    it "rejects a locality without a value" $ do
      parseCliOptions ["approved", "--locality"]
        `shouldBe` Left (MissingValue "--locality")
      parseCliOptions ["approved", "--locality", "--top", "2"]
        `shouldBe` Left (MissingValue "--locality")

    it "rejects unknown and duplicated locality options" $ do
      parseCliOptions ["approved", "--locality", "argentina"]
        `shouldBe` Left (UnknownLocality "argentina")
      parseCliOptions ["approved", "--locality", "brazil", "--locality", "chile"]
        `shouldBe` Left (DuplicateOption "--locality")

    it "rejects missing and duplicated JSON input options" $ do
      parseCliOptions ["--input"]
        `shouldBe` Left (MissingValue "--input")
      parseCliOptions ["--input", "--top", "2"]
        `shouldBe` Left (MissingValue "--input")
      parseCliOptions ["--input", "first.json", "--input", "second.json"]
        `shouldBe` Left (DuplicateOption "--input")

    it "keeps top validation and rejects duplicate top options" $ do
      parseCliOptions ["approved", "--top", "abc"]
        `shouldBe` Left (InvalidTop "abc")
      parseCliOptions ["approved", "--top", "2", "--top", "3"]
        `shouldBe` Left (DuplicateOption "--top")