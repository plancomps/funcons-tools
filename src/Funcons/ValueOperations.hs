
module Funcons.ValueOperations (
  execRewrites, evalOperation, 
  IException(..), IE(..), isIn, 
  ) where

import Funcons.EDSL (library)
import Funcons.Types
import Funcons.MSOS
import Funcons.Patterns (isIn)
import Funcons.Exceptions (IException(..), IE(..))
import Funcons.RunOptions
import Funcons.Core.Manual

evalOperation :: Funcons -> Either IE [Values]
evalOperation f =
  case execRewrites (rewriteFuncons f) of
    Left exc            -> Left exc
    Right (ValTerm vs)  -> Right vs
    Right _             -> error "rewriteToTerm: did you provide a value operation?"

execRewrites :: Rewrite a -> (Either IE a)
execRewrites rew = case e_exc_rewritten of
    Left (_, _, exc)  -> Left exc
    Right r           -> Right r
  where 
    (e_exc_rewritten, _, _) = runRewrite rew reader state
    reader = RewriteReader (libUnions [Funcons.EDSL.library
                                      ,Funcons.Core.Manual.library])
                    emptyTypeRelation defaultRunOptions undefined undefined
    state  = RewriteState

