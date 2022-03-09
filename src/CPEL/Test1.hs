module CPEL.Test1 where

import Prelude hiding ((.), id)

import Data.Text qualified as T
import Data.Set qualified as S
import Data.List
import Control.Arrow
import Control.Category
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Foldable
import Debug.Trace

import CPEL.Trie qualified as Trie

type List0 a = [a]
type List1 a = [a]
type List2 a = [a]

data E1
	= E1Op (List1 (Either (List1 T.Text) E1))
	| E1Atom T.Text
	deriving (Show)

data E2
	= E2Op (List1 T.Text) (List0 (E2, List1 T.Text))
	| E2Ap E2 E2
	| E2Atom T.Text
	deriving (Show)

type SynTac = E2
type ChkTac = E2

type Env = Trie.Trie T.Text (List0 (S.Set (List2 T.Text)))

data OpFrame = OpFrame {
	_ofrHead :: List1 T.Text,
	_ofrParts :: List0 (ChkTac, List1 T.Text),
	_ofrRest :: Either (List1 T.Text) (List0 ChkTac)
} deriving (Show)
type Frame = Either OpFrame (SynTac, List0 ChkTac)
type Stack = List0 Frame

ofrName :: OpFrame -> List1 T.Text
ofrName OpFrame{..} = _ofrHead ++ (_ofrParts >>= snd) ++ either id (const []) _ofrRest

ofrPlace :: OpFrame -> Int
ofrPlace OpFrame{..} = length _ofrHead + sum (fmap (length . snd) _ofrParts)

allowedIn :: Env -> List1 T.Text -> List1 T.Text -> Int -> Bool
allowedIn env inner outer i = S.member inner $ (!! i) $ fromJust $ Trie.lookup outer env

close :: Env -> Stack -> [((Maybe (List1 T.Text), SynTac), Stack)]
close env = runKleisli $
		Kleisli go .
		first (Kleisli start) .
		Kleisli (maybeToList . uncons)
	where
		start :: Frame -> [(Maybe (List1 T.Text), SynTac)]
		start (Left ofr@OpFrame{ _ofrRest = Right args, .. })
			= pure (
				case _ofrParts of
					[] -> Nothing
					_ -> Just $ ofrName ofr,
				foldr (flip E2Ap) (E2Op _ofrHead _ofrParts) args
			)
		start (Right (head, args)) =
			pure (Nothing, foldr (flip E2Ap) head args)
		start _ = empty

		go ::
			((Maybe (List1 T.Text), E2), Stack) ->
			[((Maybe (List1 T.Text), E2), Stack)]
		go st = st:maybe [] go (work st)

		work ::
			((Maybe (List1 T.Text), E2), Stack) ->
			Maybe ((Maybe (List1 T.Text), E2), Stack)
		work
			( (Just nameE, e)
			, (Left ofr@OpFrame{ _ofrRest = Left [""], .. }:stack))
			| not $ allowedIn env nameE nameOp (length nameOp - 2)
			= Nothing
			where nameOp = ofrName ofr
		work
			( (_, e)
			, (Left ofr@OpFrame{ _ofrRest = Left [""], .. }:stack))
			= Just
				( (Just $ ofrName ofr, E2Op _ofrHead $ (e, [""]):_ofrParts)
				, stack)
		work
			( (_, e)
			, (Left ofr@OpFrame{ _ofrRest = Right args, .. }:stack))
			= Just
				(
					(
						case _ofrParts of
							[] -> Nothing
							_ -> Just $ ofrName ofr
					, foldr (flip E2Ap) (E2Op _ofrHead _ofrParts) (e:args))
				, stack)
		work
			( (_, e)
			, (Right (head, args):stack))
			= Just ((Nothing, foldr (flip E2Ap) head (e:args)), stack)
		work _ = Nothing

w1 :: Env -> Stack -> Either (List1 T.Text) E1 -> [Stack]
w1 env = curry $ runReaderT $ join $ lift
	[ ReaderT \case -- start op
		(stack, Left part) -> do
			(rest, _) <- Trie.toList $ Trie.descend part env
			pure $ (:stack) $ Left OpFrame {
					_ofrHead = part,
					_ofrParts = [],
					_ofrRest = case rest of
						[] -> Right []
						rest -> Left rest
				}
		_ -> empty
	, ReaderT \case -- continue op
			(close env -> places@(_:_), Left name)
				| ((nameE, e), Left ofr@OpFrame{ _ofrRest = Left rest }:stack) <- last places
				, Just rest' <- stripPrefix name rest
				, case nameE of
					Nothing -> True
					Just nameE -> allowedIn env nameE (ofrName ofr) (ofrPlace ofr)
				->
					pure $ (:stack) $ Left $ ofr {
						_ofrRest = case rest' of
							[] -> Right []
							rest' -> Left rest'
					}
			_ -> empty
	, ReaderT \case -- start postfixy op
		(stack, Left part) -> do
			((nameE, e), stack) <- close env stack
			(rest, allowed:_) <- Trie.toList $ Trie.descend ("":part) env
			guard $ case nameE of
				Nothing -> True
				Just nameE -> S.member nameE allowed
			pure $ (:stack) $ Left OpFrame {
					_ofrHead = [""],
					_ofrParts = [(e, part)],
					_ofrRest = case rest of
						[] -> Right []
						rest -> Left rest
				}
		_ -> empty
	, ReaderT \case -- embed expression
		(stack@(Left OpFrame{ _ofrRest = Left _ }:_), Right e) ->
			pure $ Right (w2 env e, []):stack
		(Left ofr@OpFrame{ _ofrRest = Right args }:stack, Right e) ->
			pure $ (:stack) $ Left $ ofr {
					_ofrRest = Right $ w2 env e:args
				}
		(Right (head, args):stack, Right e) ->
			pure $ Right (head, w2 env e:args):stack
		_ -> empty
	]

w2 :: Env -> E1 -> E2
w2 env = \case
	E1Op ops -> let
			opts = do
				((_, e), []) <- foldlM (w1 env) [] ops >>= close env
				pure e
		in case opts of
			[e] -> e
			_ -> error $ show opts
	E1Atom name -> E2Atom name
