module CPEL.Trie where

import Prelude hiding (null)

import Data.Functor.Const
import Data.Functor.Identity
import Data.Map qualified as M
import Data.Maybe

data Trie k v = Trie {
	root :: Maybe v,
	successors :: M.Map k (Trie k v)
} deriving (Show, Functor, Foldable, Traversable)

empty :: Trie k v
empty = Trie Nothing M.empty

null :: Trie k v -> Bool
null (Trie {..}) = isNothing root && M.null successors

elimNull :: Trie k v -> Maybe (Trie k v)
elimNull t = if null t then Nothing else Just t

at :: (Functor f, Ord k) => [k] -> (Trie k v -> f (Trie k v)) -> Trie k v -> f (Trie k v)
at [] f t = f t
at (k:ks) f (Trie {..}) = fmap (Trie root) $ M.alterF
	(fmap elimNull .
		maybe
			(fmap
				(foldr
					(\k g -> Trie Nothing . M.singleton k . g)
					id ks)
				(f empty))
			(at ks f))
	k successors

alter :: Ord k => [k] -> (Trie k v -> Trie k v) -> Trie k v -> Trie k v
alter ks f = runIdentity . at ks (Identity . f)

singleton :: Ord k => [k] -> v -> Trie k v
singleton ks v = alter ks (const $ Trie (Just v) M.empty) empty

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup ks = root . getConst . at ks Const

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert ks v = alter ks (\t -> t { root = Just v })

descend :: Ord k => [k] -> Trie k v -> Trie k v
descend ks = getConst . at ks Const

toList :: Trie k v -> [([k], v)]
toList (Trie {..}) = fmap ([],) (maybeToList root) ++ do
	(k, t') <- M.toList successors
	(ks, v) <- toList t'
	pure (k:ks, v)

fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldl (\t (ks, v) -> insert ks v t) empty
