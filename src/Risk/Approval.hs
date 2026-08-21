{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Risk.Approval (ApprovalLevel (..), ApprovalDecision (..), ApprovalReport (..), Pending, AwaitingAnalyst, 
  AwaitingManager, AwaitingCreditCommittee, Approved, Rejected, ApprovalCase, ApprovalEvaluation (..), 
  ApprovalTransition (CompleteAnalystReview, CompleteManagerReview, CompleteCreditCommitteeReview, RejectAnalystReview, 
  RejectManagerReview, RejectCreditCommitteeReview), beginApproval, evaluateApprovalCase, approvalEvaluationToReport, 
  applyTransition, completeAnalystReview, completeManagerReview, completeCreditCommitteeReview, rejectAnalystReview, 
  rejectManagerReview, rejectCreditCommitteeReview, approvalCaseReport, evaluateApproval, renderApprovalReport, 
  renderApprovalReportWithTopPositions, renderApprovalReportWithConcentration, requiredLevelForViolation) where

import Data.Text (Text)
import qualified Data.Text as T

import Domain.AssetClassification (AssetClass (..))
import Domain.CustomerProfile (CustomerProfile (..))
import Risk.Concentration (ConcentrationRequest, ConcentrationResult, SomeTopPositions)
import Risk.Report (PortfolioReport (..), renderReport, renderReportWithConcentration, renderReportWithTopPositions)
import Risk.RiskLevel (RiskLevel (..))
import Risk.Violation (Violation (..))

-- As alcadas possuem uma ordem pq, quando varias violacoes existem, preciso considerar a maior autoridade exigida por elas
data ApprovalLevel
  = Automatic
  | Analyst
  | Manager
  | CreditCommittee
  deriving (Eq, Show, Ord, Enum, Bounded)

-- A decisao fica separada da alcada pq exigir uma revisao em determinado nivel nao significa que a carteira foi rejeitada necessariamente
data ApprovalDecision
  = AutoApproved
  | RequiresManualReview ApprovalLevel
  | RejectedByPolicy
  deriving (Eq, Show)

data ApprovalReport = ApprovalReport
  { originalReport :: PortfolioReport
  , approvalDecision :: ApprovalDecision
  , requiredLevel :: ApprovalLevel
  }
  deriving (Eq, Show)

-- Os estados nao carregam dados pq existem só pra representar, no tipo de ApprovalCase, em qual etapa o caso se encontra
data Pending

data AwaitingAnalyst

data AwaitingManager

data AwaitingCreditCommittee

data Approved

data Rejected

-- O estado faz parte do tipo pra que só operacoes validas pra aquela etapa possam receber o caso
-- E estou mantendo o construtor oculto pra impedir que outros modulos escolham estados sem passar pelas transicoes certinhas
newtype ApprovalCase state = ApprovalCase PortfolioReport
  deriving (Eq, Show)

-- A avaliacao precisa escolher um estado em runtime pq essa decisao depende das violacoes encontradas no relatorio
data ApprovalEvaluation
  = AutomaticallyApproved (ApprovalCase Approved)
  | AnalystReviewRequired (ApprovalCase AwaitingAnalyst)
  | ManagerReviewRequired (ApprovalCase AwaitingManager)
  | CreditCommitteeReviewRequired (ApprovalCase AwaitingCreditCommittee)
  | PolicyRejected (ApprovalCase Rejected)
  deriving (Eq, Show)

-- As transicoes iniciais ficam internas pq só a avaliacao do relatorio deve decidir pra qual estado um caso pendente sera encaminhado
data ApprovalTransition from to where
  AutoApprove :: ApprovalTransition Pending Approved
  RequireAnalystReview :: ApprovalTransition Pending AwaitingAnalyst
  RequireManagerReview :: ApprovalTransition Pending AwaitingManager
  RequireCreditCommitteeReview :: ApprovalTransition Pending AwaitingCreditCommittee
  RejectByPolicy :: ApprovalTransition Pending Rejected
  CompleteAnalystReview :: ApprovalTransition AwaitingAnalyst Approved
  RejectAnalystReview :: ApprovalTransition AwaitingAnalyst Rejected
  CompleteManagerReview :: ApprovalTransition AwaitingManager Approved
  RejectManagerReview :: ApprovalTransition AwaitingManager Rejected
  CompleteCreditCommitteeReview :: ApprovalTransition AwaitingCreditCommittee Approved
  RejectCreditCommitteeReview :: ApprovalTransition AwaitingCreditCommittee Rejected

beginApproval :: PortfolioReport -> ApprovalCase Pending
beginApproval = ApprovalCase

approvalCaseReport :: ApprovalCase state -> PortfolioReport
approvalCaseReport (ApprovalCase report) = report

transition :: ApprovalCase from -> ApprovalCase to
transition (ApprovalCase report) = ApprovalCase report

-- Usando o gadt pra garantir que a transicao recebida parte exatamente do estado atual e leva só a um destino permitido
applyTransition :: ApprovalTransition from to -> ApprovalCase from -> ApprovalCase to
applyTransition approvalTransition approvalCase =
  case approvalTransition of
    AutoApprove -> transition approvalCase
    RequireAnalystReview -> transition approvalCase
    RequireManagerReview -> transition approvalCase
    RequireCreditCommitteeReview -> transition approvalCase
    RejectByPolicy -> transition approvalCase
    CompleteAnalystReview -> transition approvalCase
    RejectAnalystReview -> transition approvalCase
    CompleteManagerReview -> transition approvalCase
    RejectManagerReview -> transition approvalCase
    CompleteCreditCommitteeReview -> transition approvalCase
    RejectCreditCommitteeReview -> transition approvalCase

-- A classificacao usa só o relatorio pq as regras de compliance ja foram avaliadas antes da entrada aqui
evaluateApprovalCase :: ApprovalCase Pending -> ApprovalEvaluation
evaluateApprovalCase pending
  | required == Automatic = AutomaticallyApproved (applyTransition AutoApprove pending)
  | required >= CreditCommittee && riskLevel report == High = PolicyRejected (applyTransition RejectByPolicy pending)
  | required == Analyst = AnalystReviewRequired (applyTransition RequireAnalystReview pending)
  | required == Manager = ManagerReviewRequired (applyTransition RequireManagerReview pending)
  | required == CreditCommittee = CreditCommitteeReviewRequired (applyTransition RequireCreditCommitteeReview pending)
  | otherwise = AutomaticallyApproved (applyTransition AutoApprove pending)
  where
    report :: PortfolioReport
    report = approvalCaseReport pending

    required :: ApprovalLevel
    required = requiredLevelForReport report

-- Convertendo o resultado tipado pra ApprovalReport pq a camada de apresentacao precisa de uma representacao comum independentemente do 
-- estado
approvalEvaluationToReport :: ApprovalEvaluation -> ApprovalReport
approvalEvaluationToReport evaluation =
  case evaluation of
    AutomaticallyApproved approvedCase ->
      ApprovalReport
        { originalReport = approvalCaseReport approvedCase
        , approvalDecision = AutoApproved
        , requiredLevel = Automatic
        }
    AnalystReviewRequired analystCase -> manualReview Analyst analystCase
    ManagerReviewRequired managerCase -> manualReview Manager managerCase
    CreditCommitteeReviewRequired committeeCase ->
      manualReview CreditCommittee committeeCase
    PolicyRejected rejectedCase ->
      ApprovalReport
        { originalReport = approvalCaseReport rejectedCase
        , approvalDecision = RejectedByPolicy
        , requiredLevel = CreditCommittee
        }

-- Cada funcao aceita só casos da alcada correspondente pra que uma revisao nao possa ser concluida pelo nivel errado
completeAnalystReview :: ApprovalCase AwaitingAnalyst -> ApprovalCase Approved
completeAnalystReview = applyTransition CompleteAnalystReview

completeManagerReview :: ApprovalCase AwaitingManager -> ApprovalCase Approved
completeManagerReview = applyTransition CompleteManagerReview

completeCreditCommitteeReview :: ApprovalCase AwaitingCreditCommittee -> ApprovalCase Approved
completeCreditCommitteeReview = applyTransition CompleteCreditCommitteeReview

-- A rejeicao tb preserva a alcada de origem pra impedir que um caso de outro nivel, ou ja finalizado, seja rejeitado por essa operacao
rejectAnalystReview :: ApprovalCase AwaitingAnalyst -> ApprovalCase Rejected
rejectAnalystReview = applyTransition RejectAnalystReview

rejectManagerReview :: ApprovalCase AwaitingManager -> ApprovalCase Rejected
rejectManagerReview = applyTransition RejectManagerReview

rejectCreditCommitteeReview :: ApprovalCase AwaitingCreditCommittee -> ApprovalCase Rejected
rejectCreditCommitteeReview = applyTransition RejectCreditCommitteeReview

evaluateApproval :: PortfolioReport -> ApprovalReport
evaluateApproval =
  approvalEvaluationToReport
    . evaluateApprovalCase
    . beginApproval

requiredLevelForReport :: PortfolioReport -> ApprovalLevel
requiredLevelForReport report =
  maximum (Automatic : map requiredLevelForViolation (violations report))

manualReview :: ApprovalLevel -> ApprovalCase state -> ApprovalReport
manualReview level approvalCase =
  ApprovalReport
    { originalReport = approvalCaseReport approvalCase
    , approvalDecision = RequiresManualReview level
    , requiredLevel = level
    }

requiredLevelForViolation :: Violation -> ApprovalLevel
requiredLevelForViolation (MinimumDiversificationNotMet _ _) = Analyst
requiredLevelForViolation (AssetAllocationExceeded _ _ _) = Analyst
requiredLevelForViolation (SectorExposureExceeded _ _ _) = Manager
requiredLevelForViolation (AssetClassExposureExceeded PrivateCredit _ _) = CreditCommittee
requiredLevelForViolation (AssetClassExposureExceeded _ _ _) = Manager
requiredLevelForViolation (CustomerProfileCryptoExceeded _ _ _) = Manager
requiredLevelForViolation (CustomerCreditRiskExposureExceeded _ _ _) = Manager
requiredLevelForViolation (AssetClassExposureExceededForProfile PrivateCredit Conservative _ _) = CreditCommittee
requiredLevelForViolation (AssetClassExposureExceededForProfile _ _ _ _) = Manager

-- Reaproveitando a renderizacao do relatorio original pq essa camada precisa adicionar só as infos relacionadas ao fluxo
renderApprovalReport :: ApprovalReport -> Text
renderApprovalReport approvalReport =
  renderApprovalReportFrom approvalReport (renderReport (originalReport approvalReport))

-- O Top N altera só a forma de apresentar a carteira, por isso a decisao de aprovacao pode reutilizar o mesmo resultado ja calculado
renderApprovalReportWithTopPositions :: ApprovalReport -> SomeTopPositions -> Text
renderApprovalReportWithTopPositions approvalReport positions =
  renderApprovalReportFrom
    approvalReport
    (renderReportWithTopPositions (originalReport approvalReport) positions)

renderApprovalReportWithConcentration :: ApprovalReport -> ConcentrationRequest mode -> ConcentrationResult mode -> Text
renderApprovalReportWithConcentration approvalReport request positions =
  renderApprovalReportFrom
    approvalReport
    (renderReportWithConcentration (originalReport approvalReport) request positions)

renderApprovalReportFrom :: ApprovalReport -> Text -> Text
renderApprovalReportFrom approvalReport renderedReport =
  case T.lines renderedReport of
    header : spacer : customerLine : statusLine : riskLine : rest ->
      T.unlines
        ( [ header
          , spacer
          , customerLine
          , statusLine
          , riskLine
          , "Approval Decision: " <> renderApprovalDecision (approvalDecision approvalReport)
          , "Required Level: " <> renderApprovalLevel (requiredLevel approvalReport)
          ]
            ++ rest
        )
    _ ->
      renderedReport

renderApprovalDecision :: ApprovalDecision -> Text
renderApprovalDecision AutoApproved = "AutoApproved"
renderApprovalDecision (RequiresManualReview level) =
  "RequiresManualReview (" <> renderApprovalLevel level <> ")"
renderApprovalDecision RejectedByPolicy = "RejectedByPolicy"

renderApprovalLevel :: ApprovalLevel -> Text
renderApprovalLevel Automatic = "Automatic"
renderApprovalLevel Analyst = "Analyst"
renderApprovalLevel Manager = "Manager"
renderApprovalLevel CreditCommittee = "CreditCommittee"