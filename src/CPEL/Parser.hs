module CPEL.Parser where

import Prelude hiding (exp)

import Control.Arrow
import Control.Monad
import Data.Char qualified as C
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import FlatParse.Stateful ((<|>))
import FlatParse.Stateful qualified as FP

import CPEL.Types
import CPEL.CST

data Error = Error {
	errStartPos :: FP.Pos,
	errEndPos :: Maybe FP.Pos,
	errMsg :: String
} deriving (Show)
data State = State {
	allowLongWhere :: {-# UNPACK #-} !Bool,
	allowClause :: {-# UNPACK #-} !Bool
} deriving (Show)
type Parser = FP.Parser Error

guard :: Bool -> Parser ()
guard = flip when FP.empty . not

cut :: Parser a -> String -> Parser a
cut body msg = do
	pos <- FP.getPos
	FP.cut body $ Error pos Nothing msg

spanned :: Parser a -> Parser (Spanned a)
spanned = flip FP.spanned ((pure .) . flip (,))

isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

ws1 :: Parser ()
ws1 = FP.many_ $ FP.satisfy \c -> C.isSpace c && not (isNewline c)

comment :: Parser ()
comment = $(FP.string "--") <* FP.many_ (FP.satisfy_ $ not . isNewline)

newline :: Parser Int
newline = do
	ws1; FP.optional_ comment
	let go = do
		FP.satisfy_ isNewline
		len <- FP.spanned ws1 $ \_ (FP.Span (FP.Pos s) (FP.Pos e)) -> pure $ s - e
		FP.optional_ comment
		FP.optioned go pure (pure len)
	len <- go
	FP.put len
	pure len

ws2 :: Parser ()
ws2 = do
	oldIndent <- FP.ask
	FP.many_ do
		when (oldIndent < 0) FP.empty
		newIndent <- newline
		when (newIndent <= oldIndent) FP.empty
	ws1

entries :: Parser a -> Parser [a]
entries entry = do
	first <- entry
	rest <- FP.many do
		ws2
		FP.some_ do
			$(FP.string ";")
			ws2
		entry
	pure $ first:rest

indentedBlock :: Parser a -> Parser [a]
indentedBlock entry = do
	indent <- FP.ask
	first <- entries entry
	rest <- FP.many do
		newIndent <- newline
		when (newIndent < indent) FP.empty
		when (newIndent > indent) do
			pos <- FP.getPos
			FP.err $ Error pos Nothing "too much indentation"
		entries entry
	pure $ join $ first:rest

indented :: Parser a -> Parser a
indented body = do
	oldIndent <- FP.ask
	when (oldIndent < 0) FP.empty
	newIndent <- newline
	when (newIndent <= oldIndent) FP.empty
	FP.local (const newIndent) body

delimitedBlock :: Parser a -> Parser [a]
delimitedBlock entry = do
	$(FP.string "{")
	ws2
	es <- FP.optioned (entries entry) pure (pure [])
	ws2
	cut $(FP.string "}") $ "expected `}`"
	pure es

block :: Bool -> (Bool -> Parser a) -> Parser [a]
block delimited entry = do
	ws1
	if delimited
	then delimitedBlock (entry True)
	else
			delimitedBlock (entry True) <|>
			indented (indentedBlock (entry False)) <|>
			entries (entry False)

clauseBlock :: State -> (State -> Parser a) -> Parser [a]
clauseBlock st entry =
	block (not $ allowClause st) \delimited ->
		entry $ st {
				allowLongWhere = allowLongWhere st || delimited,
				allowClause = allowClause st || delimited
			}

isIdentChar :: Char -> Bool
isIdentChar c = not $ C.isSpace c || any (c ==) ("(){}.;" :: String)

identEnd :: Parser ()
identEnd = FP.fails $ FP.satisfy_ isIdentChar

ident :: Parser Name
ident = do
	FP.fails $(FP.string "--")
	fmap (T.split (== '_') . T.decodeUtf8) $ FP.byteStringOf $
		FP.spanned (FP.some_ $ FP.satisfy_ isIdentChar) $ \() span ->
		FP.fails $ FP.inSpan span do
			$(FP.switch [| case _ of
				"where" -> pure ()
				"let" -> pure ()
				"in" -> pure ()
				"λ" -> pure ()
				"rec" -> pure ()
				":" -> pure ()
				"=" -> pure ()
				"open" -> pure ()
				"→" -> pure ()
				"operator" -> pure ()
				|])
			FP.eof

decl :: State -> Parser Decl
decl st = declOpen st <|> declHeaded st <|> declOperator st

declOpen :: State -> Parser Decl
declOpen st = do
	$(FP.string "open"); identEnd
	ws2
	e <- cut (spanned $ exp st) $ "expected an expression"
	pure $ DOpen e

declHeaded :: State -> Parser Decl
declHeaded st = do
	posStart <- FP.getPos
	head <- FP.local (const -1) $ op $ st {
			allowLongWhere = False, -- this isn't really necessary
			allowClause = False
		}
	let
		declSig = do
			$(FP.string ":"); identEnd
			(span, name) <- case head of
				[(spanV, Left (Var (spanN, name) Nothing))] ->
					pure (spanV, name)
				_ -> do
					pos <- FP.getPos
					FP.err $ Error posStart (Just pos) $
						"type declaration can only have a simple head"
			ws2
			fmap (DSig (span, name)) $ spanned $ exp st

		declClause = fmap (DClause head) $ spanned $ clauseCont st
	cut (declSig <|> declClause) $
		"expected the rest of a headed declaration: either `:` expression OR clause"

declOperator :: State -> Parser Decl
declOperator st = do
	$(FP.string "operator"); identEnd
	fmap DOperator $ FP.some do
		ws2
		fmap (False,) (spanned ident) <|> do
			$(FP.string "(")
			ws2
			n <- spanned ident
			ws2
			$(FP.string ")")
			pure (True, n)

clauseCont :: State -> Parser ClauseCont
clauseCont st = do
	body <- spanned $ clauseBody st
	ds <- FP.optioned
		(do
			if allowLongWhere st then ws2 else ws1
			$(FP.string "where"); identEnd
			FP.optioned
				(clauseBlock st $ spanned . decl)
				pure (pure []))
		pure (pure [])
	pure $ ClauseCont body ds

clauseBody :: State -> Parser ClauseBody
clauseBody st = clauseBodyExp st <|> clauseBodySplit st

clauseBodyExp :: State -> Parser ClauseBody
clauseBodyExp st = do
	ws2
	$(FP.string "="); identEnd
	ws2
	e <- cut (spanned $ exp st) $ "expected an expression"
	pure $ CBExp e

clauseBodySplit :: State -> Parser ClauseBody
clauseBodySplit st = fmap CBSplit $ clauseBlock st $ spanned . clause

clause :: State -> Parser Clause
clause st = do
	head <- fmap
		(fmap \case
			(span, Left var) -> (span, EOp [(span, Left var)])
			(span, Right exp) -> (span, exp))
		(FP.local (const -1) $ op $ st {
				allowLongWhere = False,
				allowClause = False
			})
	body <- cut (spanned $ clauseCont st) $ "expected a clause"
	pure $ Clause head body

op :: State -> Parser [Spanned Operand]
op st = FP.some $ spanned (operand st) <* ws2

operand :: State -> Parser Operand
operand st = fmap Right (exp1 st) <|> fmap Left var

expParen :: State -> Parser (Spanned Exp)
expParen st = do
	$(FP.string "(")
	e <- spanned $ exp $ st {
			allowLongWhere = True,
			allowClause = True
		}
	cut $(FP.string ")") $ "expected `)`"
	pure e

expLet :: State -> Parser Exp
expLet st = do
	$(FP.string "let"); identEnd
	ds <- block False \_ -> spanned $ decl $ st {
			allowLongWhere = True,
			allowClause = True
		}
	$(FP.string "in"); identEnd
	body <- spanned $ exp st
	pure $ ELet ds body

expLam :: State -> Parser Exp
expLam st = do
	$(FP.string "λ"); identEnd
	body <- cut
		(clauseBlock st $ spanned . clause)
		$ "expected lambda body (a block of clauses)"
	pure $ ELam body

expRec :: State -> Parser Exp
expRec st = do
	$(FP.string "rec"); identEnd
	body <- cut
		(clauseBlock st $ spanned . decl)
		$ "expected record body (a block of declarations)"
	pure $ ERec body

expDot :: State -> Parser Exp
expDot st = do
	$(FP.string ".")
	fmap ELabel (spanned ident) <|> fmap EInaccessible (expParen st)

exp1 :: State -> Parser Exp
exp1 st =
	fmap snd (expParen st) <|>
	expLet st <|>
	expLam st <|>
	expRec st <|>
	expDot st

var :: Parser Var
var = do
	name <- spanned ident
	pure $ Var name Nothing

exp2 :: State -> Parser Exp
exp2 st = flip fmap (op st) \case
	[(span, Right exp)] -> exp
	ops -> EOp ops

expArrow :: State -> Parser Exp
expArrow st = do
	$(FP.string "(")
	ws2
	name <- spanned ident
	ws2
	$(FP.string ":"); identEnd
	ws2
	aty <- cut
		(spanned $ exp $ st { allowLongWhere = True, allowClause = True })
		$ error "TODO"
	ws2
	cut $(FP.string ")") "expected `)`"
	ws2
	cut ($(FP.string "→") *> identEnd) "expected `→`"
	ws2
	rty <- cut (spanned $ exp st) $ error "TODO"
	pure $ EArrow (Just name) aty rty

exp3 :: State -> Parser Exp
exp3 st = expArrow st <|> do
	head <- spanned $ exp2 st
	FP.optioned
		(do
			ws2
			$(FP.string "→"); identEnd
			ws2
			spanned $ exp3 st)
		(\rty -> do
			pure $ EArrow Nothing head rty)
		(pure $ snd head)

exp :: State -> Parser Exp
exp = exp3
