module CPEL.Types where

import Data.Text qualified as T
import FlatParse.Stateful (Span)

type Spanned a = (Span, a)

type NamePart = T.Text
type Name = [NamePart]
type Idx = Int
