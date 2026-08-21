{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Agrupa os artefatos produzidos por uma mesma avaliacao.
module Risk.EvaluationBundle (ConcentrationEvidence (..), EvaluationBundle, RenderEvidence (..), RenderEvidenceList (..), 
  buildEvaluationBundle, renderConcentrationEvidence, renderEvaluationBundle) where

import Data.Text (Text)
import qualified Data.Text as T

import Domain.Customer (Customer)
import Domain.HList (HList (HNil, (:&)))
import Domain.Portfolio (Portfolio)
import Risk.Approval (ApprovalReport, evaluateApproval, renderApprovalReport, renderApprovalReportWithConcentration)
import Risk.Concentration (ConcentrationError, ConcentrationRequest, ConcentrationResult, analyzeConcentration, renderConcentration)
import Risk.Policy (PolicyConfig)
import Risk.Report (PortfolioReport, buildReportWithPolicy, renderReport)

-- Guardando a requisicao junto do resultado pq o tipo do resultado depende do modo de concentracao solicitado
data ConcentrationEvidence mode where
  ConcentrationEvidence
    :: ConcentrationRequest mode
    -> Either ConcentrationError (ConcentrationResult mode)
    -> ConcentrationEvidence mode

-- A hlist faz com que a composicao do bundle tb seja conhecida pelo compilador
type EvaluationBundle mode =
  HList
    '[ PortfolioReport
     , ConcentrationEvidence mode
     , ApprovalReport
     ]

class RenderEvidence a where
  renderEvidence :: a -> Text

instance RenderEvidence PortfolioReport where
  renderEvidence = renderReport

instance RenderEvidence ApprovalReport where
  renderEvidence = renderApprovalReport

instance RenderEvidence (ConcentrationEvidence mode) where
  renderEvidence = renderConcentrationEvidence

-- Renderizando a lista genericamente pq cada elemento declara como deve ser apresentado por meio do RenderEvidence
class RenderEvidenceList xs where
  renderEvidenceList :: HList xs -> [Text]

instance RenderEvidenceList '[] where
  renderEvidenceList HNil = []

instance (RenderEvidence x, RenderEvidenceList xs) => RenderEvidenceList (x ': xs) where
  renderEvidenceList (evidence :& remaining) =
    renderEvidence evidence : renderEvidenceList remaining

-- Só combino os resultados existentes pra manter as regras de compliance, concentracao e aprovacao nos seus respectivos modulos
buildEvaluationBundle :: ConcentrationRequest mode -> PolicyConfig -> Customer -> Portfolio -> EvaluationBundle mode
buildEvaluationBundle request policyConfig customerValue portfolioValue =
  portfolioReport
    :& concentrationEvidence
    :& approvalReport
    :& HNil
  where
    portfolioReport :: PortfolioReport
    portfolioReport = buildReportWithPolicy policyConfig customerValue portfolioValue

    concentrationEvidence =
      ConcentrationEvidence request (analyzeConcentration request portfolioValue)

    approvalReport :: ApprovalReport
    approvalReport = evaluateApproval portfolioReport

renderConcentrationEvidence :: ConcentrationEvidence mode -> Text
renderConcentrationEvidence (ConcentrationEvidence request result) =
  case result of
    Left concentrationError -> renderConcentrationError concentrationError
    Right positions -> renderConcentration request positions

-- Na saida final estou evitando repetir o relatorio base pq o relatorio de aprovacao ja o incorpora quando a concentracao foi calculada 
-- certinho
renderEvaluationBundle :: EvaluationBundle mode -> Text
renderEvaluationBundle (portfolioReport :& ConcentrationEvidence request result :& approvalReport :& HNil) =
  case result of
    Right positions -> renderApprovalReportWithConcentration approvalReport request positions
    Left concentrationError ->
      T.intercalate
        "\n\n"
        [ renderEvidence portfolioReport
        , renderConcentrationError concentrationError
        , renderEvidence approvalReport
        ]

renderConcentrationError :: ConcentrationError -> Text
renderConcentrationError concentrationError =
  "Concentration analysis unavailable: " <> T.pack (show concentrationError)