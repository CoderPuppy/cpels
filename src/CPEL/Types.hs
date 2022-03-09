module CPEL.Types (module CPEL.Types, Span(..), Pos(..)) where

import Data.Text qualified as T
import FlatParse.Stateful (Span(..), Pos(..))

type List0 a = [a]
type List1 a = [a]
type List2 a = [a]

type Spanned a = (Span, a)

type NamePiece = T.Text
type NamePart = List1 NamePiece
type Name = NamePart
type Idx = Int

spanStart :: Span -> Pos
spanStart (Span s _) = s

spanEnd :: Span -> Pos
spanEnd (Span _ e) = e
