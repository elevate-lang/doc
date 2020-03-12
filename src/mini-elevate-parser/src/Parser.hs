{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Text.Parsec hiding (label)
import Text.Parsec.Char
import Data.Functor.Identity
import Data.Set as Set
import Data.Map.Strict as Map
import Text.RawString.QQ
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Data.List

type Id = String

type Label = Id

data Lack = Lack [Label] | NegLack [Label] deriving (Show, Eq, Ord)

data Row = RowId Id | EmptyRow | Extend (Label, Type) Row {-| Join Row Row | Meet Row Row-} deriving (Show, Eq, Ord)

data TypeScheme = TypeSchemeId Id | TypeScheme [Id] [(Id, Lack)] Type deriving (Show, Eq, Ord)

data Type = 
  TypeId Id | 
  FunType Type Type | 
  VariantType Row | RecordType Row | RecVariantType Id Row | 
  TypeInst TypeScheme [Type] [Row] deriving (Show, Eq, Ord)

data PatternField = PatternField Pattern | RecordField [(Label, Pattern)] deriving (Show, Eq, Ord)

data Pattern = PatternId Id | PatternApp Label PatternField deriving (Show, Eq, Ord)

data Term = 
  Label Label |
  TermId Id | App Term Term |
  TypeDef Id TypeScheme Term |
  FunDef Id Bool [Id] [(Id, Lack)] [(Id, Type)] Type Term Term |
  Record [(Label, Term)] | FieldAccess Term Label |
  Match Term [(Pattern, Term)] deriving (Show, Eq, Ord)

type Parser m u a = ParsecT String u m a

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
keywords = Set.fromList ["type", "let", "in", "match", "with", "forall", "lam"]

initUpperId :: (Monad m) => Parser m u Id
initUpperId = do
  name <- (:) <$> upper <*> many alphaNum
  if Set.member name keywords then 
    unexpected (name ++ "is a keyword")
  else return name

initLowerId :: (Monad m) => Parser m u Id
initLowerId = do
  name <- (:) <$> (lower <|> char '_' <|> char '\\') <*> many alphaNum
  if Set.member name keywords then 
    unexpected ("error: " ++ name ++ " is a keyword")
  else return name

label :: (Monad m) => Parser m u Label
label = initUpperId

num :: (Monad m) => Parser m u Label
num = many1 digit

termId :: (Monad m) => Parser m u Id
termId = initLowerId

typeId :: (Monad m) => Parser m u Id
typeId = initLowerId

rowId :: (Monad m) => Parser m u Id
rowId = initLowerId

typeSchemeId :: (Monad m) => Parser m u Id
typeSchemeId = initUpperId

patternId :: (Monad m) => Parser m u Id
patternId = initLowerId

{-
atomicRow :: (Monad m) => Parser m u Row
atomicRow = parens row <|> (RowId <$> rowId) <|> (EmptyRow <$ char '#') <|>
  (Extend <$> ((,) <$> (label <* spaced (char ':')) <*> 
    type_ <* spaced (char '|')) <*> atomicRow)

meetRow :: (Monad m) => Parser m u Row
meetRow = chainl1 atomicRow (Meet <$ try (spaced (string "/\\")))

row :: (Monad m) => Parser m u Row
row = chainl1 meetRow (Join <$ try (spaced (string "\\/")))
-}

row :: (Monad m) => Parser m u Row
row = (RowId <$> rowId) <|> (EmptyRow <$ char '*') <|>
  (Extend <$> ((,) <$> (label <* spaced (char ':')) <*> 
    type_ <* spaced (char '|')) <*> row)

lack :: (Monad m) => Parser m u Lack
lack = ((NegLack <$ (char '~' *> many space)) <|> pure Lack) <*> 
  (braces (label `sepBy` (try $ spaced (char ','))))

forall :: (Monad m) => Parser m u ([Id], [(Id, Lack)])
forall = string "forall" *> many space *> 
  ((,) <$> many (typeId <* many space) <*> 
    many (brackets ((,) <$> rowId <*> 
      option (Lack []) (spaced (char ':') *> lack)) <* many space)) <* spaced (char '.')

forallTypeScheme :: (Monad m) => Parser m u TypeScheme
forallTypeScheme = uncurry TypeScheme <$> option ([], []) (try forall) <*> type_

typeScheme :: (Monad m) => Parser m u TypeScheme
typeScheme = forallTypeScheme <|> (TypeSchemeId <$> typeSchemeId)

recVariantType :: (Monad m) => Parser m u Type
recVariantType = RecVariantType <$> typeId <* spaced1 (string "as") <*> angles row

atomicType :: (Monad m) => Parser m u Type
atomicType = (TypeInst <$> 
  ((TypeSchemeId <$> typeSchemeId) <|> try (parens forallTypeScheme)) <*> 
  (many space *> many (atomicType <* many space)) <*> many (brackets row <* many space)) <|>
  parens type_ <|> try recVariantType <|> (TypeId <$> try typeId) <|> 
  (VariantType <$> angles row) <|> (RecordType <$> braces row)

type_ :: (Monad m) => Parser m u Type
type_ = chainr1 atomicType (FunType <$ try (spaced (string "->")))

patternField :: (Monad m) => Parser m u PatternField
patternField = (RecordField <$> braces (((,) <$> label <* spaced (char ':') <*> pattern) `sepBy` 
  (try $ spaced (char '|')))) <|>
  (PatternField <$> (parens pattern <|> (PatternId <$> patternId) <|> 
    (PatternApp <$> (label <|> num) <*> pure (RecordField []))))

pattern :: (Monad m) => Parser m u Pattern
pattern = (PatternApp <$> label <* many space <*> (patternField <|> pure (RecordField []))) <|>
  (PatternApp <$> num <*> pure (RecordField [])) <|>
  (PatternId <$> patternId)

recordTerm :: (Monad m) => Parser m u Term
recordTerm = Record <$> braces (((,) <$> label <* spaced (char ':') <*> term) `sepBy` (try $ spaced (char '|')))

atomicTerm :: (Monad m) => Parser m u Term
atomicTerm = parens term <|> (Label <$> label) <|> (App <$> (Label <$> num) <*> pure (Record [])) <|>
  (TermId <$> termId) <|> recordTerm

accessTerm :: (Monad m) => Parser m u Term
accessTerm = atomicTerm >>= fieldAccess
  where fieldAccess t = do {
    l <- char '.' *> label;
    fieldAccess (FieldAccess t l)} <|> return t

appTerm :: (Monad m) => Parser m u Term
appTerm = chainl1 (try (many space *> accessTerm)) (App <$ lookAhead (many1 space))

typeDef :: (Monad m) => Parser m u Term
typeDef = TypeDef <$> 
  (try (string "type" *> many1 space) *> typeSchemeId <* spaced (char '=')) <*>
  (typeScheme <* spaced (string "in")) <*>
  term

funDef :: (Monad m) => Parser m u Term
funDef = uncurry <$> (FunDef <$>
  (try (string "let" *> many1 space) *> termId <* many space) <*>
  (char ':' *> option False (True <$ char '!') <* many space)) <*>
  option ([], []) (try forall) <*>
  many (parens ((,) <$> termId <* spaced (char ':') <*> type_) <* spaced (string "->")) <*>
  (type_ <* spaced (char '=')) <*>
  (term <* spaced (string "in")) <*>
  term

lamDef :: (Monad m) => Parser m u Term
lamDef = uncurry <$> (FunDef <$>
  (try (string "lam") *> pure "" <* many space) <*>
  (char ':' *> option False (True <$ char '!') <* many space)) <*>
  option ([], []) (try forall) <*>
  many (parens ((,) <$> termId <* spaced (char ':') <*> type_) <* spaced (string "->")) <*>
  (type_ <* spaced (char '=')) <*> term <*> pure (TermId "")

match :: (Monad m) => Parser m u Term
match = Match <$>
  (try (string "match" *> many1 space) *> term <* spaced (string "with")) <*>
  angles (((,) <$> pattern <* spaced (string "=>") <*> term) `sepBy` (try $ spaced (char '|')))

term :: (Monad m) => Parser m u Term
term = typeDef <|> funDef <|> lamDef <|> match <|> appTerm

program :: (Monad m) => Parser m u Term
program = spaced term <* eof

testRun :: Parser Identity () a -> String -> Either ParseError a
testRun p s = parse (p <* eof) "" s

escape :: String -> String
escape = concatMap (\case
  '_' -> "\\_"
  x -> [x])

type Indent = Int

renderLabel :: Label -> String
renderLabel l = "\\label{" ++ l ++ "}"

renderTermId :: Id -> String
renderTermId i = "\\var{" ++ escape i ++ "}"

renderTypeId :: Id -> String
renderTypeId i = "\\tvar{" ++ escape i ++ "}"

renderTypeSchemeId :: Id -> String
renderTypeSchemeId i = "\\tsvar{" ++ i ++ "}"

renderRowId :: Id -> String
renderRowId i = "\\rvar{" ++ escape i ++ "}"

renderPatternId :: Id -> String
renderPatternId i = "\\pvar{" ++ escape i ++ "}"

renderLack :: Lack -> String
renderLack (Lack ls) = "\\lack{" ++ intercalate "\\lackSep" (renderLabel <$> ls) ++ "}"
renderLack (NegLack ls) = "\\negLack{" ++ intercalate "\\lackSep" (renderLabel <$> ls) ++ "}"

renderRow = renderRow' 0

renderRow' :: Int -> Row -> String
renderRow' _ (RowId i) = renderRowId i
renderRow' _ EmptyRow = "\\emptyRow"
renderRow' _ (Extend (l, t) r) = "\\extendRow{" ++ renderLabel l ++ "}{" ++ 
  renderType t ++ "}{" ++ renderRow' 2 r ++ "}"
{-
renderRow' p (Join r1 r2) = 
  let str = "\\joinRow{" ++ renderRow' 0 r1 ++ "}{" ++ 
            renderRow' 0 r2 ++ "}"
  in if p >= 1 then "(" ++ str  ++ ")" else str
renderRow' p (Meet r1 r2) = 
  let str = "\\meetRow{" ++ renderRow' 1 r1 ++ "}{" ++ 
            renderRow' 1 r2 ++ "}"
  in if p >= 2 then "(" ++ str ++ ")" else str
-}

renderRowIdLack :: (Id, Lack) -> String
renderRowIdLack (i, Lack []) = renderRowId i
renderRowIdLack (i, l) = "\\rvarLack{" ++ renderRowId i ++ "}{" ++ renderLack l ++ "}"

renderForall :: [Id] -> [(Id, Lack)] -> String
renderForall [] [] = ""
renderForall ts [] = "\\forallType{" ++ 
  intercalate "\\forallTypeSep" (((\i -> "\\forallTypeWrap{" ++ i ++ "}") . renderTypeId) <$> ts) ++ "}"
renderForall [] rs = "\\forallRow{" ++ 
  intercalate "\\forallRowSep" (((\i -> "\\forallRowWrap{" ++ i ++ "}") . renderRowIdLack) <$> rs) ++ "}"
renderForall ts rs = renderForall ts [] ++ renderForall [] rs

renderTypeScheme :: TypeScheme -> String
renderTypeScheme (TypeSchemeId i) = renderTypeSchemeId i
renderTypeScheme (TypeScheme ts rs t) = renderForall ts rs ++ renderType t

renderType = renderType' False

renderType' :: Bool -> Type -> String
renderType' _ (TypeId i) = renderTypeId i
renderType' w (FunType t1 t2) = 
  let str = "\\funType{" ++ renderType' True t1 ++ "}{" ++ renderType t2 ++ "}"
  in if w then "(" ++ str ++ ")" else str
renderType' _ (VariantType r) = "\\variantType{" ++ renderRow r ++ "}"
renderType' _ (RecordType r) = "\\recordType{" ++ renderRow r ++ "}"
renderType' _ (RecVariantType i r) = "\\recVariantType{" ++ renderTypeId i ++ "}{" ++ renderRow r ++ "}"
renderType' _ (TypeInst ts t r) = 
  let tsStr = case ts of
        TypeScheme _ _ _ -> "(" ++ renderTypeScheme ts ++ ")"
        _ -> renderTypeScheme ts
  in flip (Data.List.foldl' (\acc now -> "\\instRow{" ++ acc ++ "}{" ++ renderRow now ++ "}")) r $ 
     Data.List.foldl' (\acc now -> "\\instType{" ++ acc ++ "}{" ++ renderType' True now ++ "}") tsStr t

renderLabelPattern :: (Label, Pattern) -> String
renderLabelPattern (l, p) = "\\labelPattern{" ++ renderLabel l ++ "}{" ++ renderPattern p ++ "}"

renderPattern :: Pattern -> String
renderPattern (PatternId i) = renderPatternId i
renderPattern (PatternApp l (RecordField [])) = renderLabel l
renderPattern (PatternApp l (RecordField r)) = "\\patternAppRecord{" ++ renderLabel l ++ "}{" ++
  intercalate "\\patternAppRecordSep" (renderLabelPattern <$> r) ++ "}"
renderPattern (PatternApp l (PatternField p)) = 
  let pStr = case p of
        PatternApp l (RecordField []) -> renderLabel l
        PatternId i -> renderPatternId i
        _ -> "(" ++ renderPattern p ++ ")"
  in "\\patternAppPattern{" ++ renderLabel l ++ "}{" ++ pStr ++ "}"

renderFunDefArg :: (Id, Type) -> String
renderFunDefArg (i, t) = "\\funDefArg{" ++ renderTermId i ++ "}{" ++ renderType t ++ "}"

renderRecordField :: Indent -> (Label, Term) -> String
renderRecordField n (l, e) = "\\recordField{" ++ renderLabel l ++ "}{" ++ renderTerm n e ++ "}"

renderCase :: Indent -> (Pattern, Term) -> String
renderCase n (p, e) = "\\case{" ++ renderPattern p ++ "}{" ++ renderTerm n e ++ "}"

renderTerm n e = renderTerm' False n e

renderTerm' :: Bool -> Indent -> Term -> String
renderTerm' _ _ (App (Label l) (Record [])) = renderLabel l
renderTerm' _ _ (Label l) = renderLabel l
renderTerm' _ _ (TermId i) = renderTermId i
renderTerm' w n (App e1 e2) = 
  let funStr = case e1 of
        TypeDef {} -> renderTerm' True n e1
        FunDef {} -> renderTerm' True n e1
        Match {} -> renderTerm' True n e1
        _ -> renderTerm n e1
      str = "\\app{" ++ funStr ++ "}{" ++ renderTerm' True n e2 ++ "}"
  in if w then "(" ++ str ++ ")" else str
renderTerm' w n (TypeDef i ts e) = 
  let indent = if w then n + 1 else n
      str = "\\typeDef{" ++ renderTypeSchemeId i ++ "}{" ++ renderTypeScheme ts ++ "}"
      oldIndentStr = concat $ replicate n "\\quad"
      indentStr = concat $ replicate indent "\\quad"
  in if w then "(\\\\&" ++ indentStr ++ str ++ renderTerm indent e ++ "\\\\&" ++ oldIndentStr ++ ")" 
     else "\\\\&" ++ indentStr ++ str ++ renderTerm indent e
renderTerm' w n (FunDef "" force absT absR args tr f (TermId "")) =
  let indent = if w then n + 1 else n
      findent = case f of
        TypeDef {} -> indent + 1
        FunDef {} -> indent + 1
        _ -> indent
      str = (if force then "\\lamDefForce{" else "\\lamDef{") ++
        renderForall absT absR ++ concat (renderFunDefArg <$> args) ++ renderType tr ++ "}{" ++ 
        renderTerm findent f ++ "}"
      oldIndentStr = concat $ replicate n "\\quad"
      indentStr = concat $ replicate indent "\\quad"
  in if w then "(\\\\&" ++ indentStr ++ str ++ "\\\\&" ++ oldIndentStr ++ ")"
     else "\\\\&" ++ indentStr ++ str
renderTerm' w n (FunDef i force absT absR args tr f e) =
  let indent = if w then n + 1 else n
      findent = case f of
        TypeDef {} -> indent + 1
        FunDef {} -> indent + 1
        _ -> indent
      str = (if force then "\\funDefForce{" else "\\funDef{") ++ renderTermId i ++ "}{" ++
        renderForall absT absR ++ concat (renderFunDefArg <$> args) ++ renderType tr ++ "}{" ++ 
        renderTerm findent f ++ "}"
      oldIndentStr = concat $ replicate n "\\quad"
      indentStr = concat $ replicate indent "\\quad"
  in if w then "(\\\\&" ++ indentStr ++ str ++ renderTerm indent e ++ "\\\\&" ++ oldIndentStr ++ ")"
     else "\\\\&" ++ indentStr ++ str ++ renderTerm indent e
renderTerm' _ n (Record fs) = 
  let indent = n
      oldIndentStr = concat $ replicate n "\\quad"
      indentStr = concat $ replicate indent "\\quad"
      fieldStr = if length fs == 1 then renderRecordField indent (head fs)
        else "\\\\&" ++ indentStr ++ "\\quad" ++
          intercalate ("\\\\&" ++ indentStr ++ "\\recordSepBreak") (renderRecordField (indent + 1) <$> fs) ++
          "\\\\&" ++ indentStr
  in "\\record{" ++ fieldStr ++ "}"
renderTerm' _ n (FieldAccess e l) = "\\fieldAccess{" ++ renderTerm' True n e ++ "}{" ++ renderLabel l ++ "}"
renderTerm' w n (Match e cs) = 
  let indent = n + 1
      oldIndentStr = concat $ replicate n "\\quad"
      indentStr = concat $ replicate indent "\\quad"
      caseStr = if length cs == 1 then renderCase indent (head cs)
        else "\\\\&" ++ indentStr ++ "\\quad" ++ 
          intercalate ("\\\\&" ++ indentStr ++ "\\matchSepBreak") (renderCase (indent + 1) <$> cs) ++ 
          "\\\\&" ++ indentStr
      str = "\\\\&" ++ indentStr ++ "\\match{" ++ renderTerm indent e ++ "}{" ++ caseStr ++ "}"
  in if w then "(" ++ str ++ "\\\\&" ++ oldIndentStr ++ ")" else str

renderProgram :: Term -> String
renderProgram e = "\\begin{align*}" ++ renderTerm 0 e ++ "\\end{align*}"

toLaTeX :: String -> IO ()
toLaTeX s = case parse (program <* eof) "" s of
  Right p -> putStrLn (renderProgram p)
  Left e -> print e

example :: String
example = [r|
type Result = forall a b. <Success: a | Failure: b | *> in
type Strategy = 
  forall [p] [q]. <p> -> Result <q> Nat in
let id: Strategy[p][p] = 
  lam: (x: <p>) -> <Success: <p> | r> = Success x in
let fail: Strategy[p][r] = 
  lam: (x: <p>) -> <Failure: Nat | u> = Failure 0 in
let flatMap: (x: Result a b) -> 
             (f: a -> Result c b) -> Result c b =
  match x with <
    Success a => f a
  | Failure b => Failure b
  > in
let seq: (fs: Strategy[p][q]) -> 
         (ss: Strategy[q][r]) -> Strategy[p][r] =
  lam: (x: <p>) -> Result <r> Nat = 
      flatMap (fs x) (lam: (x: <q>) -> Result <r> Nat = ss x) in
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
  [App: {Fun: <App: {Fun: <Primitive: <Map: {*} | r6> | r5> | Arg: f | r4} | r3> | Arg: <App: {Fun: <App: {Fun: <Primitive: <Map: {*} | r12> | r11> | Arg: g | r10} | r9> | Arg: x | r8} | r7> | r1} | r0]
  [App: {Fun: <App: {Fun: <Primitive: <Map: {*} | h5> | h4> | Arg: <Lam: {Param: Nat | Body: <App: {Fun: f | Arg: <App: {Fun: g | Arg: <Id: {Name: Nat | h13} | h12> | h11} | h10> | h9} | h8> | h7} | h6> | h3} | h2> | Arg: x | h1} | h0] = 
  lam: (x: <_>) -> Result <_> Nat = match x with <
    App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: App {Fun: App {Fun: Primitive Map | Arg: g} | Arg: x}} => 
    Success (App {Fun: App {Fun: Primitive Map | Arg: Lam {Param: 0 | Body: App {Fun: f | Arg: App {Fun: g | Arg: Id {Name: 0}}}}} | Arg: x})
  | _ => Failure 1
  > in _
|]
