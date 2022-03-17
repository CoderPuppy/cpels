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

data Syn s c
	= SynOp
		(Spanned NamePart)
		[(Spanned (Chk s c), Spanned NamePart)]
		(Maybe (Spanned Idx))
	| SynExp s
	| SynAp (Spanned (Syn s c)) (Spanned (Chk s c))
	deriving (Show)
instance Bifunctor Syn where
	bimap ms mc (SynOp head parts index) = SynOp head (fmap (first $ fmap $ bimap ms mc) parts) index
	bimap ms mc (SynExp s) = SynExp $ ms s
	bimap ms mc (SynAp f a) = SynAp (fmap (bimap ms mc) f) (fmap (bimap ms mc) a)
data Chk s c
	= ChkExp c
	| ChkSyn (Syn s c)
	deriving (Show)
instance Bifunctor Chk where
	bimap ms mc (ChkExp c) = ChkExp $ mc c
	bimap ms mc (ChkSyn s) = ChkSyn $ bimap ms mc s

type Env = Trie.Trie NamePiece [S.Set Name]
data OpFrame s c = OpFrame
	{ _ofrHead :: Spanned NamePart
	, _ofrParts :: List0 (Spanned (Chk s c), Spanned NamePart)
	, _ofrRest :: Either NamePart (List0 (Spanned (Chk s c)))
	, _ofrIndex :: Maybe (Spanned Idx)
	}
data ExpFrame s c = ExpFrame
	{ _efrHead :: Spanned (Syn s c)
	, _efrArgs :: List0 (Spanned (Chk s c))
	}
type Frame s c = Either (OpFrame s c) (ExpFrame s c)
type Stack s c = List0 (Frame s c)
data Slice s c = Slice
	{ _slExpName :: Maybe Name
	, _slExp :: Spanned (Syn s c)
	, _slStack :: Stack s c
	}

ofrName :: OpFrame s c -> Name
ofrName OpFrame{..} = concat
	[ snd _ofrHead
	, reverse _ofrParts >>= snd . snd
	, either id (const []) _ofrRest ]

ofrSliceName :: OpFrame s c -> Maybe Name
ofrSliceName OpFrame{ _ofrParts = [], _ofrRest = rest }
	| either null (const True) rest = Nothing
ofrSliceName ofr = Just $ ofrName ofr

ofrPlace :: OpFrame s c -> Int
ofrPlace OpFrame{..} = length _ofrHead + sum (fmap (length . snd) _ofrParts)

allows :: Env -> Name -> Int -> Name -> Bool
allows env outer i inner =
	S.member inner $ (!! i) $ fromJust $ Trie.lookup outer env

buildAps ::
	Spanned (Syn s c) ->
	List0 (Spanned (Chk s c)) ->
	Spanned (Syn s c)
buildAps = foldr \a@(Span _ e, _) f@(Span s _, _) ->
	(Span s e, SynAp f a)

buildOpFr :: OpFrame s c -> Spanned (Syn s c)
buildOpFr OpFrame{..} = buildAps core $ fromRight' _ofrRest
	where
		core = (
				Span (spanStart $ fst _ofrHead) $
					maybe id (max . spanEnd . fst) _ofrIndex $
					case _ofrParts of
						[] -> spanEnd $ fst _ofrHead
						parts | (_, (Span _ e, _)) <- last parts -> e,
				SynOp _ofrHead _ofrParts _ofrIndex
			)

buildFrame :: Frame s c -> Spanned (Syn s c)
buildFrame (Left ofr) = buildOpFr ofr
buildFrame (Right (ExpFrame head args)) = buildAps head args

startSlice :: Stack s c -> Maybe (Slice s c)
startSlice (Left ofr@OpFrame{ _ofrRest = Right args, .. }:stack)
	= Just Slice
		{ _slExpName = ofrSliceName ofr
		, _slExp = buildOpFr ofr
		, _slStack = stack }
startSlice (Right (ExpFrame f args):stack)
	= Just Slice
		{ _slExpName = Nothing
		, _slExp = buildAps f args
		, _slStack = stack }
startSlice _ = Nothing

extendSlice :: Env -> Slice s c -> Maybe (Slice s c)
extendSlice env Slice
	{ _slExpName = Just nameE
	, _slStack = Left ofr@OpFrame { _ofrRest = Left [""] }:_ }
	| not $ allows env nameOp (length nameOp - 2) nameE
	= Nothing
	where nameOp = ofrName ofr
extendSlice env Slice
	{ _slExp = e@(Span _ pos, _)
	, _slStack = Left ofr@OpFrame{ _ofrRest = Left [""] }:stack }
	= Just Slice
		{ _slExpName = Just $ ofrName ofr
		, _slExp = buildOpFr $ ofr
				{ _ofrParts =
						(second ChkSyn e, (Span pos pos, [""])):_ofrParts ofr
				, _ofrRest = Right [] }
		, _slStack = stack }
extendSlice env Slice
	{ _slExp = e
	, _slStack = Left ofr@OpFrame{ _ofrRest = Right args }:stack }
	= Just Slice
		{ _slExpName = ofrSliceName ofr
		, _slExp = buildOpFr $ ofr
				{ _ofrRest = Right $ second ChkSyn e:args }
		, _slStack = stack }
extendSlice env Slice
	{ _slExp = e
	, _slStack = Right (ExpFrame f args):stack }
	= Just Slice
		{ _slExpName = Nothing
		, _slExp = buildAps f args
		, _slStack = stack }
extendSlice _ _ = Nothing

slices :: Env -> Stack s c -> [Slice s c]
slices env = maybeToList . startSlice >=> go
	where go slice = slice:maybe [] go (extendSlice env slice)

lastSlice :: Env -> Stack s c -> Maybe (Slice s c)
lastSlice env = fmap go . startSlice
	where go slice = maybe slice go $ extendSlice env slice

processOperand :: Env -> Stack s c -> Spanned (Either CST.Var (s, c)) -> Either String [Stack s c]
processOperand env stack op = fmap join $ sequenceA $ fmap ($ (stack, op)) rules
	where rules =
		[ \case --start op
			(stack, (_, Left (CST.Var part idx))) -> pure do
				(rest, _) <- Trie.toList $ Trie.descend (snd part) env
				pure $ (:stack) $ Left OpFrame
					{ _ofrHead = part
					, _ofrParts = []
					, _ofrRest = case rest of
							[] -> Right []
							rest -> Left rest
					, _ofrIndex = idx }
			_ -> pure empty

		, \case -- continue op
			(lastSlice env -> Just sl@Slice
					{ _slStack = Left ofr@OpFrame{ _ofrRest = Left rest }:stack }
				, (_, Left (CST.Var (span, part) idx)))
				| Just rest' <- stripPrefix part rest
				, maybe True (allows env (ofrName ofr) (ofrPlace ofr)) $ _slExpName sl
				-> do
					idx' <- case (idx, _ofrIndex ofr) of
						(Just _, Just _) -> Left "kjfdsa"
						(a, b) -> pure $ a <|> b
					pure $ pure $ (:stack) $ Left $ ofr
						{ _ofrParts = (:_ofrParts ofr) $
								(second ChkSyn (_slExp sl), (span, part))
						, _ofrRest = case drop (length part) rest of
								[] -> Right []
								rest' -> Left rest'
						, _ofrIndex = idx' }
			_ -> pure empty

		, \case -- start postfixy op
			(stack, (_, Left (CST.Var part idx))) -> pure do
				sl <- slices env stack
				(rest, allowed:_) <- Trie.toList $ Trie.descend ("":snd part) env
				guard $ maybe True (flip S.member allowed) $ _slExpName sl
				let pos = spanStart $ fst $ _slExp sl
				pure $ (:stack) $ Left OpFrame
					{ _ofrHead = (Span pos pos, [""])
					, _ofrParts = [(second ChkSyn (_slExp sl), part)]
					, _ofrRest = case rest of
							[] -> Right []
							rest -> Left rest
					, _ofrIndex = idx }
			_ -> pure empty

		, \case -- embed expression
			(stack@(Left OpFrame{ _ofrRest = Left _ }:_), (span, Right (syne, _))) ->
				pure $ pure $ Right (ExpFrame (span, SynExp syne) []):stack
			(Left ofr@OpFrame{ _ofrRest = Right args }:stack, (span, Right (_, chke))) ->
				pure $ pure $ (:stack) $ Left $ ofr
					{ _ofrRest = Right $ (span, ChkExp chke):args }
			(Right (ExpFrame f args):stack, (span, Right (_, chke))) ->
				pure $ pure $ Right (ExpFrame f ((span, ChkExp chke):args)):stack
		]

buildOps :: Env -> [Spanned (Either CST.Var (s, c))] -> Either String [Spanned (Syn s c)]
buildOps env ops = do
	stacks <- foldl
		(\stacks op ->
			stacks >>= fmap join . sequenceA . fmap (flip (processOperand env) op))
		(pure []) ops
	pure do
		Slice { _slExp = e, _slStack = [] } <- mapMaybe (lastSlice env) stacks
		pure e
