module CPEL.OpResolver where

import Prelude hiding (sequence)

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (sequence)
import Data.Either.Combinators (fromRight')
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Data.Bifunctor hiding (first, second)

import CPEL.Types
import CPEL.CST qualified as CST
import CPEL.Trie qualified as Trie

-- | Expressions, generalized over base expressions
data Expr e
	-- | Embedded base expression
	= EBase e
	-- | Application expression
	| EAp (Spanned (Expr e)) (Spanned (Expr e))
	-- | Operator expression (including plain names)
	| EOp
		-- | Leading name part, may be empty in postfixy operators
		(Spanned NamePart)
		-- | Sub-expressions and following name parts
		--
		-- The last name part may be empty in prefixy operators
		(List0 (Spanned (Expr e), Spanned NamePart))
		-- | Variable-scoped De Bruijn index
		(Maybe (Spanned Idx))
	deriving (Show, Functor)

-- | Environment of available operators, mapped to precedence information
type Env = Trie.Trie NamePiece (List0 (S.Set Name))

-- | Parser stack
type Stack e = List0 (Frame e)
-- | Parser frame
type Frame e = Either (OpFrame e) (ExpFrame e)
-- | Operator frame, represents an operator being parsed
data OpFrame e = OpFrame
	-- | Leading name part
	{ _ofrHead :: Spanned NamePart
	-- | Filled holes
	, _ofrParts :: List0 (Spanned (Expr e), Spanned NamePart)
	-- | Extra name pieces
	, _ofrRest :: NamePart
	-- | Variable-scoped De Bruijn index
	, _ofrIndex :: Maybe (Spanned Idx)
	}
-- | Expression frame, represents an embedded expression
data ExpFrame e = ExpFrame (Spanned (Expr e))
-- | Slice through the parser stack
data Slice e = Slice
	-- | The operator at the top-level of `_slExp`
	{ _slExpName :: Maybe Name
	-- | Inner expression
	, _slExp :: Spanned (Expr e)
	-- | Remaining stack
	, _slStack :: Stack e
	}

-- | Extract the operator name from an `OpFrame`
ofrName :: OpFrame e -> Name
ofrName OpFrame{..} = concat
	[ snd _ofrHead
	, reverse _ofrParts >>= snd . snd
	, _ofrRest ]

-- | Compute the index of the current hole in an `OpFrame`
ofrPlace :: OpFrame e -> Int
ofrPlace OpFrame{..} = length _ofrHead + sum (fmap (length . snd) _ofrParts)

-- | Check if an operator is allowed in a hole in another
allows :: Env -> Name -> Int -> Name -> Bool
allows env outer i inner =
	S.member inner $ (!! i) $ fromJust $ Trie.lookup outer env

-- | Build an operator expression with a span
buildOp ::
	Spanned NamePart ->
	List0 (Spanned (Expr e), Spanned NamePart) ->
	Maybe (Spanned Idx) ->
	Spanned (Expr e)
buildOp head parts index =
	(
		Span (spanStart $ fst head) $
			maybe id (max . spanEnd . fst) index $
			case parts of
				[] -> spanEnd $ fst head
				parts -> spanEnd $ fst $ snd $ last parts,
		EOp head parts index
	)

-- | Start a slice
startSlice :: Stack e -> Maybe (Slice e)
startSlice (Right (ExpFrame e):stack)
	= Just Slice
		{ _slExpName = Nothing
		, _slExp = e
		, _slStack = stack }
startSlice _ = Nothing

-- | Extend a slice by moving a frame into the expression
extendSlice :: Env -> Slice e -> Maybe (Slice e)
-- Prevent disallowed operators in the last hole
extendSlice env Slice
	{ _slExpName = Just nameE
	, _slStack = Left ofr@OpFrame { _ofrRest = [""] }:_ }
	| not $ allows env nameOp (length nameOp - 2) nameE
	= Nothing
	where nameOp = ofrName ofr
-- Fill the last hole of a prefixy operator
extendSlice env Slice
	{ _slExp = e@(Span _ pos, _)
	, _slStack = Left ofr@OpFrame{ _ofrRest = [""] }:stack }
	= Just Slice
		{ _slExpName = Just $ ofrName ofr
		, _slExp = buildOp
			(_ofrHead ofr)
			((e, (Span pos pos, [""])):_ofrParts ofr)
			(_ofrIndex ofr)
		, _slStack = stack }
-- Build an application
extendSlice env Slice
	{ _slExp = a
	, _slStack = Right (ExpFrame f):stack }
	= Just Slice
		{ _slExpName = Nothing
		, _slExp = (
			Span (spanStart $ fst f) (spanEnd $ fst a),
			EAp f a
		)
		, _slStack = stack }
extendSlice _ _ = Nothing

-- | Enumerate all valid slices
slices :: Env -> Stack e -> [Slice e]
slices env = maybeToList . startSlice >=> go
	where go slice = slice:maybe [] go (extendSlice env slice)

-- | Compute the last valid slice
lastSlice :: Env -> Stack e -> Maybe (Slice e)
lastSlice env = fmap go . startSlice
	where go slice = maybe slice go $ extendSlice env slice

-- | Push an expression frame to the stack
--
-- Merges into an existing `ExpFrame` to build an application
pushExpFr :: ExpFrame e -> Stack e -> Stack e
pushExpFr (ExpFrame e) (Right (ExpFrame f):st) =
	Right (ExpFrame (Span (spanStart $ fst f) (spanEnd $ fst e), EAp f e)):st
pushExpFr efr st = Right efr:st

-- | Push an operation frame to the stack
--
-- Collapses a completed frame (`_ofrRest = []`) into an `ExpFrame`
pushOpFr :: OpFrame e -> Stack e -> Stack e
pushOpFr OpFrame{ _ofrRest = [], .. } =
	pushExpFr $ ExpFrame $ buildOp _ofrHead _ofrParts _ofrIndex
pushOpFr ofr = (Left ofr:)

-- | Process a single operand
processOperand ::
	Env -> Stack e -> Spanned (Either CST.Var e) -> Either String [Stack e]
processOperand env stack op = fmap join $ sequenceA $ fmap ($ (stack, op)) rules
	where rules =
		[ \case -- start op
			(stack, (_, Left (CST.Var part idx))) -> pure do
				(rest, _) <- Trie.toList $ Trie.descend (snd part) env
				pure $ flip pushOpFr stack $ OpFrame
					{ _ofrHead = part
					, _ofrParts = []
					, _ofrRest = rest
					, _ofrIndex = idx }
			_ -> pure empty

		, \case -- continue op
			(stack, (_, Left (CST.Var (span, part) idx)))
				-- find the innermost incomplete operator
				| Just sl@Slice{ _slStack = Left ofr@OpFrame{ _ofrRest = rest }:stack' }
					<- lastSlice env stack
				, Just rest' <- stripPrefix part rest
				-- ensure the inner expression is allowed inside this operator
				, maybe True (allows env (ofrName ofr) (ofrPlace ofr)) $ _slExpName sl
				-> do
					-- check there is at most one variable index per operator
					idx' <- case (idx, _ofrIndex ofr) of
						(Just _, Just _) -> Left "kjfdsa"
						(a, b) -> pure $ a <|> b
					pure $ pure $ flip pushOpFr stack' $ ofr
						{ _ofrParts = (:_ofrParts ofr) $
								(_slExp sl, (span, part))
						, _ofrRest = drop (length part) rest
						, _ofrIndex = idx' }
			_ -> pure empty

		, \case -- start postfixy op
			(stack, (_, Left (CST.Var part idx))) -> pure do
				-- at any point in the stack
				sl <- slices env stack
				-- for any postfixy operator
				(rest, allowed:_) <- Trie.toList $ Trie.descend ("":snd part) env
				-- which is allowed at that point
				guard $ maybe True (flip S.member allowed) $ _slExpName sl

				let pos = spanStart $ fst $ _slExp sl
				pure $ flip pushOpFr (_slStack sl) $ OpFrame
					{ _ofrHead = (Span pos pos, [""])
					, _ofrParts = [(_slExp sl, part)]
					, _ofrRest = rest
					, _ofrIndex = idx }
			_ -> pure empty

		, \case -- embed expression
			(stack, (span, Right e)) ->
				pure $ pure $ pushExpFr (ExpFrame (span, EBase e)) stack
			_ -> pure empty
		]

-- | Process an operator (`CST.EOp`)
buildOps :: Env -> [Spanned (Either CST.Var e)] -> Either String [Spanned (Expr e)]
buildOps env ops = do
	stacks <- foldl
		(\stacks op ->
			stacks >>= fmap join . sequenceA . fmap (flip (processOperand env) op))
		(pure []) ops
	pure do
		Slice { _slExp = e, _slStack = [] } <- mapMaybe (lastSlice env) stacks
		pure e
