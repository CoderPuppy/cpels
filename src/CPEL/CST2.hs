module CPEL.CST2 where

import CPEL.Types (Spanned, Name, Idx)

data Var = Var {
	_varName :: Spanned Name,
	_varIdx :: Maybe (Spanned Idx)
} deriving (Show)
data Exp
	= EVar Var
	| EOp [(Spanned Name, Spanned Exp)] (Spanned Name) (Maybe (Spanned Idx))
	| ELet [Spanned Decl] (Spanned Exp)
	| ELam [Spanned Clause]
	| ERec [Spanned Decl]
	| ELabel (Spanned Name)
	| EInaccessible (Spanned Exp)
	| ETyped (Spanned Exp) (Spanned Exp)
	| EArrow (Maybe (Spanned Name)) (Spanned Exp) (Spanned Exp)
	deriving (Show)
data Decl
	= DSig (Spanned Name) (Spanned Exp)
	| DClause (Spanned Exp) (Spanned ClauseCont)
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
