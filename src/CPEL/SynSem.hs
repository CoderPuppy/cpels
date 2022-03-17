module CPEL.SynSem where

import CPEL.Types

data Term
	= Var Idx
	| Meta MetaVar
	| Ap Term Term
	| Lam Term
	| Pi Term Term
	| Universe

data Function
	= Closure [Val] Term
	| Builtin (Val -> Val)
data Val
	= VVar Lvl [Val]
	| VMeta MetaVar [Val]
	| VLam Function
	| VPi Val Function
	| VUniverse

eval :: [Val] -> Term -> Val
eval env = \case
	Var i -> env !! i
	Meta mv -> error "TODO"
	Ap f a -> vApp (eval env f) (eval env a)
	Lam body -> VLam $ Closure env body
	Pi at rt -> VPi (eval env at) (Closure env rt)
	Universe -> VUniverse

exec :: Function -> Val -> Val
exec (Closure env body) a = eval (a:env) body
exec (Builtin f) a = f a

vApp :: Val -> Val -> Val
vApp (VVar v as) a = VVar v (a:as)
vApp (VMeta mv as) a = VMeta mv (a:as)
vApp (VLam clo) a = exec clo a

quoteSpine :: Lvl -> Term -> [Val] -> Term
quoteSpine l head [] = head
quoteSpine l head (a:as) = Ap (quoteSpine l head as) (quote l a)

quote :: Lvl -> Val -> Term
quote l = \case
	VVar v sp -> quoteSpine l (Var (l - v - 1)) sp
	VMeta mv sp -> quoteSpine l (Meta mv) sp
	VLam body -> Lam $ quote (l + 1) $ exec body $ VVar l []
	VPi at rt -> Pi (quote l at) $ quote (l + 1) $ exec rt $ VVar l []
	VUniverse -> Universe
