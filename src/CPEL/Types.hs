module CPEL.Types (module CPEL.Types, Span(..), Pos(..)) where

import Data.Text qualified as T
import FlatParse.Stateful (Span(..), Pos(..))

-- | List with length ≥ 0
type List0 a = [a]
-- | List with length ≥ 1
type List1 a = [a]
-- | List with length ≥ 2
type List2 a = [a]

-- | Type with an attached span
type Spanned a = (Span, a)

-- | Piece of a name without any underscores
--
-- May be empty, for example the `NamePart` @_!@ would consist of an empty piece and
-- a piece containing @!@.
type NamePiece = T.Text
-- | Partial name - Underscore separated `NamePiece`s
--
-- These separate the sub-expressions in operator expressions
type NamePart = List1 NamePiece
-- | Full name
type Name = NamePart
-- | De Bruijn Index
type Idx = Int
-- | De Bruijn level
type Lvl = Int
-- | Metavariable index
type MetaVar = Int

spanStart :: Span -> Pos
spanStart (Span s _) = s
spanEnd :: Span -> Pos
spanEnd (Span _ e) = e
