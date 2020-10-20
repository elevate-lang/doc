{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Parsec hiding (label)
import Text.Parsec.Char
import Data.Functor.Identity
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.RawString.QQ
import Data.List as List
import Data.Comp.Multi
import AST
import Id
import qualified Label as L

type Parser m u a = ParsecT String u m a

type PresSig = Pres :+: UnknownPres

type TypeSig = Type :+: Row :+: UnknownType :+: RecVariantType :+: IdScheme :+: Scheme PresSig :+: SchemeInst

type PatSig = RecordPat :+: AppPat :+: Pat :+: MatchAllPat

type ExprSig = Expr :+: FunDef PresSig TypeSig :+: TypeDef TypeSig :+: RecordOps :+: LabelExpr LabelAsLit :+: Match PatSig ComplexPat

type FullSig = Expr :+: FunDef PresSig TypeSig :+: RecDef PresSig TypeSig :+: TypeDef TypeSig :+: RecordOps :+: LabelExpr LabelAsFun :+: Match PatSig ComplexPat

parens :: (Monad m) => Parser m u a -> Parser m u a
parens p = char '(' *> many space *> p <* many space <* char ')'

braces :: (Monad m) => Parser m u a -> Parser m u a
braces p = char '{' *> many space *> p <* many space <* char '}'

brackets :: (Monad m) => Parser m u a -> Parser m u a
brackets p = char '[' *> many space *> p <* many space <* char ']'

angles :: (Monad m) => Parser m u a -> Parser m u a
angles p = char '<' *> many space *> p <* many space <* char '>'

spaced :: (Monad m) => Parser m u a -> Parser m u a
spaced p = many space *> p <* many space

spaced1 :: (Monad m) => Parser m u a -> Parser m u a
spaced1 p = many1 space *> p <* many1 space

keywords :: Set.Set Id
keywords = Set.fromList (map strId ["type", "let", "in", "match", "with", "forall", "lam"])

initUpperId :: (Monad m) => Parser m u Id
initUpperId = do
  name <- (:) <$> upper <*> many alphaNum
  if Set.member (strId name) keywords then 
    unexpected (name ++ "is a keyword")
  else return (strId name)

initLowerId :: (Monad m) => Parser m u Id
initLowerId = do
  name <- (:) <$> (lower <|> char '\\') <*> many alphaNum
  if Set.member (strId name) keywords then 
    unexpected ("error: " ++ name ++ " is a keyword")
  else return (strId name)

label :: (Monad m) => Parser m u L.Label
label = (L.label . getName) <$> initUpperId

num :: (Monad m) => Parser m u L.Label
num = L.label <$> many1 digit

termId :: (Monad m) => Parser m u Id
termId = initLowerId <|> (strId <$> string "_")

typeId :: (Monad m) => Parser m u Id
typeId = initLowerId

rowId :: (Monad m) => Parser m u Id
rowId = initLowerId

typeSchemeId :: (Monad m) => Parser m u Id
typeSchemeId = initUpperId

patternId :: (Monad m) => Parser m u Id
patternId = initLowerId

pres :: (Pres :<: p, Monad m) => Parser m u (Fix p PRES)
pres = ((iLack <$ (char '~' *> many space)) <|> pure iPres) <*> 
  (braces (label `sepBy` (try $ spaced (char ','))))

row :: (Monad m) => Parser m u (Fix TypeSig RowKind)
row = (iIdRow <$> rowId) <|> (iEmptyRow <$ char '*') <|>
  (iExtendRow <$> ((,) <$> (label <* spaced (char ':')) <*> 
    type_ <* spaced (char '|')) <*> row)

forall :: (Monad m) => Parser m u ([Id], [(Id, Fix PresSig PRES)])
forall = string "forall" *> many space *> 
  ((,) <$> many (typeId <* many space) <*> 
    many (brackets ((,) <$> rowId <*> 
      option iUnknownPres (spaced (char ':') *> pres)) <* many space)) <* spaced (char '.')

forallTypeScheme :: (Monad m) => Parser m u (Fix TypeSig SchemeKind)
forallTypeScheme = uncurry iScheme <$> option ([], []) (try forall) <*> type_

idScheme :: (IdScheme :<: t, Monad m) => Parser m u (Fix t SchemeKind)
idScheme = iIdScheme <$> typeSchemeId

typeScheme :: (Monad m) => Parser m u (Fix TypeSig SchemeKind)
typeScheme = forallTypeScheme <|> idScheme

recVariantType :: (Monad m) => Parser m u (Fix TypeSig TypeKind)
recVariantType = iRecVariantType <$> typeId <* spaced1 (string "as") <*> angles row

atomicType :: (Monad m) => Parser m u (Fix TypeSig TypeKind)
atomicType = (iSchemeInst <$> 
  (idScheme <|> try (parens forallTypeScheme)) <*> 
  (many space *> many (atomicType <* many space)) <*> many (brackets row <* many space)) <|>
  (iUnknownType <$ char '_') <|> parens type_ <|> try recVariantType <|> (iIdType <$> try typeId) <|> 
  (iVariantType <$> angles row) <|> (iRecordType <$> braces row)

type_ :: (Monad m) => Parser m u (Fix TypeSig TypeKind)
type_ = chainr1 atomicType (iFunType <$ try (spaced (string "->")))

recordPattern :: (Monad m) => Parser m u (Fix PatSig ComplexPat)
recordPattern = iRecordPat <$> braces (
  ((,) <$> label <* spaced (char ':') <*> pattern) `sepBy` 
  (try $ spaced (char '|')))

atomicPattern :: (Monad m) => Parser m u (Fix PatSig ComplexPat)
atomicPattern = (iLabelPat <$> (label <|> num)) <|>
  parens pattern <|> (iIdPat <$> patternId) <|> (iMatchAllPat <$ char '_') <|>
  recordPattern

pattern :: (Monad m) => Parser m u (Fix PatSig ComplexPat)
pattern = try (iAppPat <$> label <* many space <*> atomicPattern) <|> atomicPattern

recordForm :: (Monad m) => Parser m u [(L.Label, Fix ExprSig EXPR)]
recordForm = braces (((,) <$> label <* spaced (char ':') <*> term) `sepBy` (try $ spaced (char '|')))

atomicTerm :: (Monad m) => Parser m u (Fix ExprSig EXPR)
atomicTerm = parens term <|> (iLabelLit <$> label) <|> (iAppExpr <$> (iLabelLit <$> num) <*> pure (iRecordCons [])) <|>
  (iIdExpr <$> termId) <|> (iRecordCons <$> recordForm)

recordTerm :: (Monad m) => Parser m u (Fix ExprSig EXPR)
recordTerm = atomicTerm >>= recordOps
  where 
    recordOps t = fieldAccess t <|> fieldRemove t <|> 
      recordMod t <|> recordExt t <|> return t
    
    fieldAccess t = do
      l <- char '.' *> label
      recordOps (iFieldAccess t l)
    
    fieldRemove t = do
      l <- try (string ".-") *> label
      recordOps (iFieldRemove t l)

    recordMod t = do
      r <- char '.' *> recordForm
      recordOps (iRecordMod t r)

    recordExt t = do
      r <- try (string ".+") *> recordForm
      recordOps (iRecordExt t r)

    
appTerm :: (Monad m) => Parser m u (Fix ExprSig EXPR)
appTerm = chainl1 (try (many space *> recordTerm)) (iAppExpr <$ lookAhead (many1 space))

typeDef :: (Monad m) => Parser m u (Fix ExprSig EXPR)
typeDef = iTypeDef <$> 
  (try (string "type" *> many1 space) *> typeSchemeId <* spaced (char '=')) <*>
  (typeScheme <* spaced (string "in")) <*>
  term

funDefComp :: (Monad m) => Parser m u (Fix ExprSig EXPR)
funDefComp = do
  name <- try (string "let" *> many1 space) *> termId <* many space
  isCons <- char ':' *> option False (True <$ char '!') <* many space
  (tv, rv) <- option ([], []) (try forall)
  (ps, pts) <- unzip <$> many (parens ((,) <$> termId <* spaced (char ':') <*> type_) <* spaced (string "->"))
  rt <- (type_ <* spaced (char '='))
  body <- (term <* spaced (string "in"))
  e <- term
  let t = (tv, rv, List.foldr iFunType rt pts)
      f = List.foldr iLamExpr body ps
  return $ iFunDef name isCons t f e

funDefSimp :: (Monad m) => Parser m u (Fix ExprSig EXPR)
funDefSimp = do
  name <- try (string "let" *> many1 space) *> termId <* many space
  ps <- many (spaced termId)
  spaced (char '=')
  body <- term <* spaced (string "in")
  e <- term
  let t = ([], [] :: [(Id, Fix PresSig PRES)], iUnknownType :: Fix TypeSig TypeKind)
      f = List.foldr iLamExpr body ps
  return $ iFunDef name False t f e

funDef :: (Monad m) => Parser m u (Fix ExprSig EXPR)
funDef = try funDefComp <|> funDefSimp

lamDef :: (Monad m) => Parser m u (Fix ExprSig EXPR)
lamDef = flip (List.foldr iLamExpr) <$> 
  (try (string "lam") *> many space *> many1 termId <* spaced (char '=')) <*> term

match :: (Monad m) => Parser m u (Fix ExprSig EXPR)
match = iMatch <$>
  (try (string "match" *> many1 space) *> term <* spaced (string "with")) <*>
  angles (((,) <$> pattern <* spaced (string "=>") <*> term) `sepBy` (try $ spaced (char '|')))

term :: (Monad m) => Parser m u (Fix ExprSig EXPR)
term = typeDef <|> funDef <|> lamDef <|> match <|> appTerm

program :: (Monad m) => Parser m u (Fix ExprSig EXPR)
program = spaced term <* eof

testRun :: Parser Identity () a -> String -> Either ParseError a
testRun p s = parse (p <* eof) "" s

example :: String
example = [r|
type Result = forall a b. <Success: a | Failure: b | *> in
type Strategy = 
  forall p q. p -> Result q Nat in
let id: Strategy p p = lam x = Success x in
let fail: Strategy p q = lam x = Failure 0 in
let flatMap: (x: Result a b) -> 
             (f: a -> Result c b) -> Result c b =
  match x with <
    Success a => f a
  | Failure b => Failure b
  > in
let seq: (fs: Strategy p q) -> 
         (ss: Strategy q r) -> Strategy p r =
  lam x = flatMap (fs x) ss in
type Primitive = forall [p]. <Map: {*} | p> in
type Type = t as <*> in
type Expr = forall [p]. e as <
  Id: {Name: Nat | Type: Type | *}
| Lam: {Param: Nat | Body: e | Type: Type | *}
| App: {Fun: e | Arg: e | Type: Type | *}
| Primitive: Primitive[p] | *> in
_
|]

fusionExample :: String
fusionExample = [r|
let fusion: Strategy
  <App: {Fun: <App: {Fun: <Primitive: <Map: {*} | r6> | r5> | Arg: f | r4} | r3> | Arg: <App: {Fun: <App: {Fun: <Primitive: <Map: {*} | r12> | r11> | Arg: g | r10} | r9> | Arg: x | r8} | r7> | r1} | r0>
  <App: {Fun: <App: {Fun: <Primitive: <Map: {*} | h5> | h4> | Arg: <Lam: {Param: Nat | Body: <App: {Fun: f | Arg: <App: {Fun: g | Arg: <Id: {Name: Nat | h13} | h12> | h11} | h10> | h9} | h8> | h7} | h6> | h3} | h2> | Arg: x | h1} | h0> = 
  lam x = match x with <
    App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: App {Fun: App {Fun: Primitive Map | Arg: g} | Arg: x}} => 
    Success (App {Fun: App {Fun: Primitive Map | Arg: Lam {Param: 0 | Body: App {Fun: f | Arg: App {Fun: g | Arg: Id {Name: 0}}}}} | Arg: x})
  | _ => Failure 1
  > in _
|]

{-
class (HTraversable g, Monad m) => ASTParser f g m where
  parseCoalg :: CoalgM (ParsecT String () m) g (Fix f)
  parseAST :: NatM (ParsecT String () m) (Fix f) (Fix g)
  parseAST = anaM parseCoalg

instance (Monad m, HTraversable g, Pres :<: g) => ASTParser Pres g m where
  parseCoalg (Term p) = case p of
    Pres {} -> inj <$> parser
    Lack {} -> inj <$> parser
    where parser :: ParsecT String () m (Pres (Fix Pres) PRES)
          parser = ((Lack <$ (char '~' *> many space)) <|> pure Pres) <*> 
            (braces (label `sepBy` (try $ spaced (char ','))))

instance (Monad m, HTraversable g, UnknownPres :<: g) => ASTParser UnknownPres g m where
  parseCoalg (Term p) = case p of
    UnknownPres -> inj <$> parser
    where parser :: ParsecT String () m (UnknownPres (Fix UnknownPres) PRES)
          parser = pure UnknownPres

instance (Monad m, HTraversable g, Row :<: g) => ASTParser Row g m where
  parseCoalg (Term p) = case p of
    IdRow {} -> inj <$> parser
    EmptyRow -> inj <$> parser
    ExtendRow {} -> inj <$> parser
    where parser :: ParsecT String () m (Row (Fix Row) RowKind)
          parser = (iIdRow <$> rowId) <|> (iEmptyRow <$ char '*') <|>
            (iExtendRow <$> ((,) <$> (label <* spaced (char ':')) <*> 
            type_ <* spaced (char '|')) <*> pure iEmptyRow)
-}
