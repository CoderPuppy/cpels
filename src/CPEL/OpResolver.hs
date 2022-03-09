module CPEL.OpResolver where

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Foldable (foldl')
import Control.Monad (join)
import Control.Applicative (liftA2)
import Data.Traversable (for)

import CPEL.Types
import CPEL.CST1 qualified as CST
import CPEL.Trie qualified as Trie

data ElabM a
instance Functor ElabM
instance Applicative ElabM
instance Monad ElabM
data Tm
data Tp
type SynTac = ElabM (Tm, Tp)
type ChkTac = Tp -> ElabM Tm

getPrecedence :: ElabM (Trie.Trie NamePart [S.Set Name])
getPrecedence = undefined
synOp ::
	[(Spanned Name, Spanned ChkTac)] ->
	Spanned Name ->
	Maybe (Spanned Idx) ->
	SynTac
synOp = undefined
synAp :: Spanned SynTac -> Spanned ChkTac -> SynTac
synAp = undefined
chkSyn :: SynTac -> ChkTac
chkSyn = undefined

w1 :: [Spanned CST.Operand] -> SynTac
w1 ops = do
	env <- getPrecedence
	let
		go :: [a] -> Spanned CST.Operand -> ElabM [[a]]
		go stack op = fmap join $ sequence [
				do
					
			]
	states <- foldl'
		((. ((fmap join .) . flip for . flip go)) . (>>=))
		(pure [[]]) ops
	error "TODO"

w2 :: CST.Exp -> SynTac
w2 = error "TODO"
