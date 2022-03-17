module CPEL.CST where

import CPEL.Types

data Var = Var {
	_varName :: Spanned Name,
	_varIdx :: Maybe (Spanned Idx)
} deriving (Show)
data Exp
	= EOp [Spanned Operand]
	| ELet [Spanned Decl] (Spanned Exp)
	| ELam [Spanned Clause]
	| ESig [Spanned Decl]
	| ELabel (Spanned Name)
	| EInaccessible (Spanned Exp)
	| ETyped (Spanned Exp) (Spanned Exp)
	| EArrow (Maybe (Spanned Name)) (Spanned Exp) (Spanned Exp)
	deriving (Show)
data Decl
	= DSig (Spanned Name) (Spanned Exp)
	| DClause [Spanned Operand] (Spanned ClauseCont)
	| DOpen (Spanned Exp)
	| DOperator [(Bool, Spanned Name)]
	deriving (Show)
data ClauseBody
	= CBExp (Spanned Exp)
	| CBSplit [Spanned Clause]
	deriving (Show)
data ClauseCont = ClauseCont {
	_clauseContBody :: Spanned ClauseBody,
	_clauseContDecls :: [Spanned Decl]
} deriving (Show)
data Clause = Clause {
	_clauseContPats :: [Spanned Exp],
	_clauseCont :: Spanned ClauseCont
} deriving (Show)
type Operand = Either Var Exp
