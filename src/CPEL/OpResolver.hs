module CPEL.OpResolver where

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Foldable
import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Maybe
import Data.Either.Combinators (fromRight')
import Control.Arrow
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT))
import Control.Monad.Trans.Class (lift)
import Data.List

import CPEL.Types
import CPEL.CST1 qualified as CST
import CPEL.Trie qualified as Trie

data ElabM a
instance Functor ElabM
instance Applicative ElabM
instance Monad ElabM
instance MonadFail ElabM
data Tm
data Tp
data Tac a = Tac
	{ _tacName :: String
	, _tacImpl :: a
	}
instance Show (Tac a) where
	showsPrec d (Tac name _) = showParen (d > 10) $
		showString "Tac " . showsPrec 11 name
type SynTac = Tac (ElabM (Tm, Tp))
type ChkTac = Tac (Tp -> ElabM Tm)

getPrecedence :: ElabM (Trie.Trie NamePiece [S.Set Name])
getPrecedence = undefined
synOp ::
	Spanned NamePart ->
	[(Spanned ChkTac, Spanned NamePart)] ->
	Maybe (Spanned Idx) ->
	SynTac
synOp = undefined
synAp :: Spanned SynTac -> Spanned ChkTac -> SynTac
synAp = undefined
chkSyn :: SynTac -> ChkTac
chkSyn = undefined

type Env = Trie.Trie NamePiece [S.Set Name]
data OpFrame = OpFrame
	{ _ofrHead :: Spanned NamePart
	, _ofrParts :: List0 (Spanned ChkTac, Spanned NamePart)
	, _ofrRest :: Either NamePart (List0 (Spanned ChkTac))
	, _ofrIndex :: Maybe (Spanned Idx)
	} deriving (Show)
type Frame = Either OpFrame (Spanned SynTac, List0 (Spanned ChkTac))
type Stack = List0 Frame
data Slice = Slice
	{ _slExpName :: Maybe Name
	, _slExp :: Spanned SynTac
	, _slStack :: Stack
	} deriving (Show)

ofrName :: OpFrame -> Name
ofrName OpFrame{..} = concat
	[ snd _ofrHead
	, reverse _ofrParts >>= snd . snd
	, either id (const []) _ofrRest ]

ofrSliceName :: OpFrame -> Maybe Name
ofrSliceName OpFrame{ _ofrParts = [], _ofrRest = rest }
	| either null (const True) rest = Nothing
ofrSliceName ofr = Just $ ofrName ofr

ofrPlace :: OpFrame -> Int
ofrPlace OpFrame{..} = length _ofrHead + sum (fmap (length . snd) _ofrParts)

allows :: Env -> Name -> Int -> Name -> Bool
allows env outer i inner =
	S.member inner $ (!! i) $ fromJust $ Trie.lookup outer env

synAps :: Spanned SynTac -> List0 (Spanned ChkTac) -> Spanned SynTac
synAps = foldr \a@(Span _ e, _) f@(Span s _, _) -> (Span s e, synAp f a)

synOpFr :: OpFrame -> Spanned SynTac
synOpFr OpFrame{..} = synAps core $ fromRight' _ofrRest
	where
		core = (
				Span (spanStart $ fst _ofrHead) $
					maybe id (max . spanEnd . fst) _ofrIndex $
					case _ofrParts of
						[] -> spanEnd $ fst _ofrHead
						parts | (_, (Span _ e, _)) <- last parts -> e,
				synOp _ofrHead _ofrParts _ofrIndex
			)

synFrame :: Frame -> Spanned SynTac
synFrame = either synOpFr (uncurry synAps)

startSlice :: Stack -> Maybe Slice
startSlice (Left ofr@OpFrame{ _ofrRest = Right args, .. }:stack)
	= Just Slice
		{ _slExpName = ofrSliceName ofr
		, _slExp = synOpFr ofr
		, _slStack = stack }
startSlice (Right (f, args):stack)
	= Just Slice
		{ _slExpName = Nothing
		, _slExp = synAps f args
		, _slStack = stack }
startSlice _ = Nothing

extendSlice :: Env -> Slice -> Maybe Slice
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
		, _slExp = synOpFr $ ofr
				{ _ofrParts =
						(second chkSyn e, (Span pos pos, [""])):_ofrParts ofr
				, _ofrRest = Right [] }
		, _slStack = stack }
extendSlice env Slice
	{ _slExp = e
	, _slStack = Left ofr@OpFrame{ _ofrRest = Right args }:stack }
	= Just Slice
		{ _slExpName = ofrSliceName ofr
		, _slExp = synOpFr $ ofr
				{ _ofrRest = Right $ second chkSyn e:args }
		, _slStack = stack }
extendSlice env Slice
	{ _slExp = e
	, _slStack = Right (f, args):stack }
	= Just Slice
		{ _slExpName = Nothing
		, _slExp = synAps f args
		, _slStack = stack }
extendSlice _ _ = Nothing

slices :: Env -> Stack -> [Slice]
slices env = maybeToList . startSlice >=> go
	where go slice = slice:maybe [] go (extendSlice env slice)

lastSlice :: Env -> Stack -> Maybe Slice
lastSlice env = fmap go . startSlice
	where go slice = maybe slice go $ extendSlice env slice

processOperand :: Stack -> Spanned CST.Operand -> ElabM [Stack]
processOperand stack op = getPrecedence >>=
		fmap join . sequenceA . fmap ($ (stack, op)) . rules
	where rules env =
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
						(Just _, Just _) -> fail "kjfdsa"
						(a, b) -> pure $ a <|> b
					pure $ pure $ (:stack) $ Left $ ofr
						{ _ofrParts = (:_ofrParts ofr) $
								(second chkSyn (_slExp sl), (span, part))
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
					, _ofrParts = [(second chkSyn (_slExp sl), part)]
					, _ofrRest = case rest of
							[] -> Right []
							rest -> Left rest
					, _ofrIndex = idx }
			_ -> pure empty

		, \case -- embed expression
			(stack@(Left OpFrame{ _ofrRest = Left _ }:_), (span, Right e)) ->
				pure $ pure $ Right ((span, synExp e), []):stack
			(Left ofr@OpFrame{ _ofrRest = Right args }:stack, (span, Right e)) ->
				pure $ pure $ (:stack) $ Left $ ofr
					{ _ofrRest = Right $ (span, chkExp e):args }
			(Right (f, args):stack, (span, Right e)) ->
				pure $ pure $ Right (f, (span, chkExp e):args):stack
		]

synExp :: CST.Exp -> SynTac
synExp (CST.EOp ops) = Tac "op" $ do
	stacks <- foldl
		(\stacks op ->
			stacks >>= fmap join . sequenceA . fmap (flip processOperand op))
		(pure []) ops
	env <- getPrecedence
	let exps = do
		Slice { _slExp = e, _slStack = [] } <- mapMaybe (lastSlice env) stacks
		pure e
	case exps of
		[e] -> _tacImpl $ snd e
		_ -> fail $ show exps

chkExp :: CST.Exp -> ChkTac
chkExp = error "TODO"
