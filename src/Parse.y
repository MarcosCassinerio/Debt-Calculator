{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Inter
%name parseStmts Defs

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '['             { TOpen }
    ']'             { TClose }
    ','             { TComma }
    VAR             { TVar $$ }
    VAL             { TVal $$ }
    DEFINEP         { TDefineP }
    DEFINEG         { TDefineG }
    DEBTP           { TDebtP }
    DEBTG           { TDebtG }
    EXPENSE         { TExpense }
    CALCULATE       { TCalculate }
    CALCULATEALL    { TCalculateAll }
    REGISTRY        { TRegistry }
    MEMBERS         { TMembers }

%%

Def     :: { Exp }
        :  DEFINEP VAR                 { DefineP $2 }
        |  DEFINEG VAR '[' Names ']'   { DefineG $2 $4 }
        |  DEBTP VAR VAL               { DebtP $2 $3 }
        |  DEBTG VAR VAR VAL           { DebtG $2 $3 $4 }
        |  EXPENSE VAR VAL             { Expense $2 $3}
        
Names   :: { [String] }
        : VAR ',' Names                { $1 : $3 }
        | VAR                          { [$1] }

Defs    : Def Defs                     { Def $1 : $2 }
        |                              { [] }

Inter   : Op                           { Eval $1 }

Op      :: { Exp }
        : CALCULATE VAR                { Calculate $2 }
        | CALCULATEALL                 { CalculateAll }
        | REGISTRY VAR                 { Registry $2 }
        | MEMBERS VAR                  { Members $2 }
     
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
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
           | TVal Int
           | TOpen
           | TClose
           | TComma
           | TDefineP
           | TDefineG
           | TDebtP
           | TDebtG
           | TExpense
           | TCalculate
           | TCalculateAll
           | TRegistry
           | TMembers
           | TEOF
           deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  	->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                          | isDigit c -> lexNum (c:cs)
                    ('[':cs) 	-> cont TOpen cs
                    (']':cs) 	-> cont TClose cs
                    (',':cs) 	-> cont TComma cs
                    unknown 	-> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("DEFINEP",rest)    	-> cont TDefineP rest
                              ("DEFINEG",rest)  	-> cont TDefineG rest
                              ("DEBTP",rest)  		-> cont TDebtP rest
                              ("DEBTG",rest)   		-> cont TDebtG rest
                              ("EXPENSE",rest)          -> cont TExpense rest
                              ("CALCULATE",rest)  	-> cont TCalculate rest
                              ("CALCULATEALL",rest)	-> cont TCalculateAll rest
                              ("REGISTRY",rest)         -> cont TRegistry rest
                              ("MEMBERS", rest)         -> cont TMembers rest
                              (var,rest)    		-> cont (TVar var) rest
                          lexNum cs = let (num,rest) = span isDigit cs 
                                      in cont (TVal (read num)) rest
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
}
