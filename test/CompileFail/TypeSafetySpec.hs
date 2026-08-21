{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module CompileFail.TypeSafetySpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Domain.SafeVector
  ( Fin (FZ, FS)
  , N1
  , Vec
  , indexVec
  , sN3
  )
import Risk.Approval
  ( Approved
  , ApprovalCase
  , ApprovalTransition (CompleteManagerReview)
  , AwaitingAnalyst
  , AwaitingCreditCommittee
  , applyTransition
  , completeAnalystReview
  )
import Risk.Concentration
  ( ConcentrationRequest (StaticTop)
  , SomeTopPositions
  , renderConcentration
  )

spec :: Spec
spec = describe "compile-time type safety" $ do
  it "does not allow analyst completion for a committee case" $
    shouldNotTypecheck
      ( ( completeAnalystReview
            :: ApprovalCase AwaitingCreditCommittee
            -> ApprovalCase Approved
        )
          `seq` ()
      )

  it "does not apply a manager transition to an analyst case" $
    shouldNotTypecheck
      ( ( applyTransition CompleteManagerReview
            :: ApprovalCase AwaitingAnalyst
            -> ApprovalCase Approved
        )
          `seq` ()
      )

  it "does not allow the second index on a one-element vector" $
    shouldNotTypecheck
      ( ( indexVec (FS FZ)
            :: Vec N1 Int
            -> Int
        )
          `seq` ()
      )

  it "does not pair a static request with a dynamic result" $
    shouldNotTypecheck
      ( ( renderConcentration (StaticTop sN3)
            :: SomeTopPositions
            -> Text
        )
          `seq` ()
      )