module Init where

import qualified Data.ByteString as B

data KattisProblem
  = ProblemId Integer
  | ProblemName B.ByteString

retrieveProblemId :: KattisProblem -> ConnEnv Integer
retrieveProblemId p@(ProblemId _) = return p
retrieveProblemId (ProblemName _) = undefined

retrieveTestFiles :: KattisProblem -> ConnEnv [B.ByteString]
retrieveTestFiles = undefined

-- initialize may fail for a variety of reasons
-- the specific problem and a helper string should be encoded, if failing.
-- otherwise there is no specific return value
initializeProblem :: KattisProblem -> Bool -> Bool -> ConnEnv ()
initializeProblem problem mkDir retrieveTests = do
  createDirectoryIfMissing "<problem>"
  setCurrentDirectory ""
  createDirectoryIfMissing ".sofie"
  return ()
