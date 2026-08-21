module Domain.SafeVectorSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (vector)

import Domain.SafeVector
  ( Fin (..)
  , SNat (SZ)
  , SomeSNat (..)
  , Vec (VNil, (:>))
  , indexVec
  , sN1
  , sN2
  , sN3
  , snatToInt
  , someSNatFromInt
  , takeVec
  , vecToList
  )

spec :: Spec
spec = describe "Domain.SafeVector" $ do
  let vector = "A" :> "B" :> "C" :> VNil

  it "indexes the first element safely" $
    indexVec FZ vector `shouldBe` "A"

  it "indexes the second element safely" $
    indexVec (FS FZ) vector `shouldBe` "B"

  it "indexes the third element safely" $
    indexVec (FS (FS FZ)) vector `shouldBe` "C"

  it "preserves the element order when converted to a list" $
    vecToList vector `shouldBe` ["A", "B", "C"]

  it "converts SZ to zero at runtime" $
    snatToInt SZ `shouldBe` 0

  it "converts sN1 to one at runtime" $
    snatToInt sN1 `shouldBe` 1

  it "converts sN2 to two at runtime" $
    snatToInt sN2 `shouldBe` 2

  it "converts sN3 to three at runtime" $
    snatToInt sN3 `shouldBe` 3

  it "builds Vec N3 when a list has more than three elements" $
    fmap vecToList (takeVec sN3 ([10, 20, 30, 40] :: [Int])) `shouldBe` Just [10, 20, 30]

  it "builds Vec N3 when a list has exactly three elements" $
    fmap vecToList (takeVec sN3 ([10, 20, 30] :: [Int])) `shouldBe` Just [10, 20, 30]

  it "returns Nothing when a list cannot satisfy sN3" $
    fmap vecToList (takeVec sN3 ([10, 20] :: [Int])) `shouldBe` Nothing

  it "always builds an empty vector for SZ" $
    fmap vecToList (takeVec SZ ([1, 2, 3] :: [Int])) `shouldBe` Just []

  it "converts zero to an existential singleton" $
    someSNatValue 0 `shouldBe` Just 0

  it "preserves three inside an existential singleton" $
    someSNatValue 3 `shouldBe` Just 3

  it "builds singleton values beyond the predefined canonical sizes" $
    someSNatValue 5 `shouldBe` Just 5

  it "rejects negative integers when creating an existential singleton" $
    someSNatValue (-1) `shouldBe` Nothing

  it "preserves singleton cardinality whenever takeVec succeeds" $
    property $ \values ->
      conjoin
        [ takeVecLengthMatches SZ values
        , takeVecLengthMatches sN1 values
        , takeVecLengthMatches sN2 values
        , takeVecLengthMatches sN3 values
        ]

  it "succeeds exactly when the list has enough elements" $
    property $ \values ->
      conjoin
        [ takeVecSucceedsExactlyWhenEnough SZ values
        , takeVecSucceedsExactlyWhenEnough sN1 values
        , takeVecSucceedsExactlyWhenEnough sN2 values
        , takeVecSucceedsExactlyWhenEnough sN3 values
        ]

  it "round-trips non-negative runtime integers through SomeSNat" $
    property $
      forAll (choose (0, 20)) $ \value ->
        someSNatValue value === Just value

takeVecLengthMatches :: SNat n -> [Int] -> Property
takeVecLengthMatches size values =
  case takeVec size values of
    Just vector -> length (vecToList vector) === snatToInt size
    Nothing -> property True

takeVecSucceedsExactlyWhenEnough :: SNat n -> [Int] -> Property
takeVecSucceedsExactlyWhenEnough size values =
  isJust (takeVec size values) === (length values >= snatToInt size)

isJust :: Maybe value -> Bool
isJust (Just _) = True
isJust Nothing = False

someSNatValue :: Int -> Maybe Int
someSNatValue value =
  case someSNatFromInt value of
    Just (SomeSNat size) -> Just (snatToInt size)
    Nothing -> Nothing