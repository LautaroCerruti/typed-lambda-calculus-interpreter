{
module Parse where
import Common 
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Def
%name parseStmts Defs
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='     { TEquals }
    ':'     { TColon }
    '\\'    { TAbs }
    '.'     { TDot }
    ','     { TComma }
    '('     { TOpen }
    ')'     { TClose }
    '->'    { TArrow }
    ZERO      { TZero }
    LET     { TLet }
    IN      { TIn }
    VAR     { TVar $$ }
    TYPEE   { TTypeE }
    TYPEU   { TTypeU }
    TYPEP   { TTypeP }
    TYPEN   { TTypeN }
    DEF     { TDef }
    AS      { TAs }
    UNIT    { TUnit }
    FST     { TFst }
    SND     { TSnd }
    SUC     { TSuc }
    R       { TR }
    

%right VAR
%left '=' 
%right '->'
%left as
%right '\\' '.' LET IN
%left R

%%

Def     :  Defexp                      { $1 }
        |  Exp	                       { Eval $1 }
Defexp  : DEF VAR '=' Exp              { Def $2 $4 } 

Exp     :: { LamTerm }
        : '\\' VAR ':' Type '.' Exp    { LAbs $2 $4 $6 }
        | LET VAR '=' Exp IN Exp       { LLet $2 $4 $6 }
        | AsAbs                        { $1 }
        
AsAbs   :: { LamTerm }
        : AsAbs AS Type                { LAs $1 $3 }
        | Funs                         { $1 }

Funs    :: { LamTerm }
        : FST Funs                     { LFst $2 }
        | SND Funs                     { LSnd $2 }
        | R Atom Atom Atom             { LRec $2 $3 $4 }
        | Nexp                         { $1 }

Nexp    :: { LamTerm }
        : SUC Nexp                     { LSuc $2 }
        | NAbs                         { $1 } 

NAbs    :: { LamTerm }
        : NAbs Atom                    { LApp $1 $2 }
        | Atom                         { $1 }

Atom    :: { LamTerm }
        : VAR                          { LVar $1 }  
        | UNIT                         { LUnit }
        | ZERO                         { LZero }
        | '(' Exp ',' Exp ')'          { LPair $2 $4 }
        | '(' Exp ')'                  { $2 }

Type    : TYPEE                        { EmptyT }
        | TYPEU                        { UnitT }
        | TYPEN                        { NatT }
        | Type '->' Type               { FunT $1 $3 }
        | '(' Type ',' Type ')'        { PairT $2 $4 }
        | '(' Type ')'                 { $2 }

Defs    : Defexp Defs                  { $1 : $2 }
        |                              { [] }
     
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "L??nea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TTypeE
               | TTypeU
               | TTypeP
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TColon
               | TArrow
               | TComma
               | TFst
               | TSuc
               | TSnd
               | TR
               | TUnit
               | TZero
               | TTypeN
               | TEquals
               | TLet
               | TIn
               | TAs
               | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "L??nea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    ('0':cs) -> cont TZero cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    (',':cs) -> cont TComma cs
                    unknown -> \line -> Failed $ 
                     "L??nea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("E",rest)    -> cont TTypeE rest
                              ("Nat",rest)     -> cont TTypeN rest
                              ("Unit",rest)    -> cont TTypeU rest
                              ("unit",rest)    -> cont TUnit rest
                              ("def",rest)  -> cont TDef rest
                              ("let",rest)  -> cont TLet rest
                              ("fst",rest)  -> cont TFst rest
                              ("snd",rest)  -> cont TSnd rest
                              ("in",rest)  -> cont TIn rest
                              ("suc",rest) -> cont TSuc rest
                              ("as",rest)  -> cont TAs rest
                              ("R",rest)   -> cont TR rest
                              (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs     
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
