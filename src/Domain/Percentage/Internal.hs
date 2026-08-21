-- Helpers usados só internamente quando o valor nao vem de uma entrada externa, mas sim do proprio codigo ou de algum 
-- calculos que ja esya protegido pelas validacoes do dominio
-- Estou mantendo esses helpers separados pq, nesses casos, uma falha seria um erro de implementacao
module Domain.Percentage.Internal (clampCalculatedPercentage, percentageLiteral) where

import Domain.Percentage (Percentage, clampPercentage, mkPercentage)

percentageLiteral :: Double -> Percentage
percentageLiteral value =
  case mkPercentage value of
    Right percentage -> percentage
    Left err -> error ("invalid internal percentage literal: " <> show err)

-- Nao propago o Either pq as entradas desses calculos ja foram validadas antes. Se ainda assim o calculo produzir um valor 
-- invalido, isso quer dizer uma quebra de uma premissa interna do motor mesmo
clampCalculatedPercentage :: Double -> Percentage
clampCalculatedPercentage value =
  case clampPercentage value of
    Right percentage -> percentage
    Left err -> error ("non-finite internal percentage calculation: " <> show err)
