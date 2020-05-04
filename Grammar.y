{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }

%token 
  "+"         { TokenOp _ "+" }
  "-"         { TokenOp _ "-" }          
  "*"         { TokenOp _ "*" }
  "/"         { TokenOp _ "/" }
  "**"        { TokenOp _ "**" }
  "<"         { TokenCompareOp _ "<" }
  ">"         { TokenCompareOp _ ">" }
  "<="        { TokenCompareOp _ "<=" }
  ">="        { TokenCompareOp _ ">=" }
  "=="        { TokenCompareOp _ "==" }
  "!="        { TokenCompareOp _ "!=" }
  "&&"        { TokenCompareOp _ "&&" }
  "||"        { TokenCompareOp _ "||" }
  ident       { TokenIdent _ $$ }
  intLit      { TokenIntLit _ $$ } 
  stringLit   { TokenStringLit _ $$}
  "int"       { TokenInt _ }
  "string"    { TokenString _ }
  "bool"      { TokenBool _ }
  "="         { TokenEquals _ }
  "not"       { TokenNegate _ }         
  "("         { TokenLeftParen _ }
  ")"         { TokenRightParen _ }
  "["         { TokenLeftBracket _ }
  "]"         { TokenRightBracket _ }
  "{"         { TokenLeftBrace _ }
  "}"         { TokenRightBrace _ }
  ";"         { TokenSemiColon _ }
  ","         { TokenColon _}
  "if"        { TokenIf _ }
  "else"      { TokenElse  _}
  "true"      { TokenTrue  _}
  "false"     { TokenFalse _ }
  "while"     { TokenWhile _ }
  "print"     { TokenPrint _}
  "println"   { TokenPrintLine _}
  "read"      { TokenRead _}
  "get"       { TokenGet _}
  "consume"  { TokenConsume _}
  "stream"    { TokenStream _}
  "length"    { TokenLength _}
  "null"      { TokenNull _}  

%nonassoc "if" "while"
%nonassoc "else"
%nonassoc "print" "println" "int" "bool" "string" "null"
%right "="
%left "&&" "||"
%right ">" "<" ">=" "<=" "==" "!="
%left "+" "-" 
%left "*" "/"
%left "**"  
%left "not"

 

%% 

Program : 
        StatementList { Program $1 }

Statement : "if" "(" Expr ")" "{" StatementList "}" "else" "{" StatementList "}"  { StatementIfElse $3 $6 $10 }
          | "if" "(" Expr ")" "{" StatementList "}" { StatementIf $3 $6 }
          | "while" "(" Expr ")" "{" StatementList "}"                { StatementWhile $3 $6 }
          | "print" "(" Expr ")" ";"                      { StatementPrint $3 }
          | "println" "(" Expr ")" ";"                      { StatementPrintLine $3 }
          | ident "=" Expr ";"                            { StatementAssign $1 $3 }
          | ident "[" Expr "]" "=" Expr ";"             { StatementArrayAssign $1 $3 $6}
          | VarDeclr                                     { StatementVarDeclr $1}
          | ArrayDeclr                                   { StatementArrayDeclr $1}
		  | "consume" "(" Expr ")"  ";"				{ StatementConsume $3}

VarDeclr : Type ident ";"           { VarDeclrOnly $1 $2 }
         | Type ident "=" Expr ";"  { VarDeclrAssign $1 $2 $4 }

ExprList : ExprList "," Expr { ($1 ++ [$3]) }
         | Expr              { [$1] }
         | {- empty -}       { [] }

ArrayDeclr : Type "[" "]" ident ";" { ArrayDeclrOnly $1 $4 }
           | Type "[" "]" ident "=" Expr ";" { ArrayDeclrAssign $1 $4 $6 }

StatementList : Statement StatementList  { ([$1] ++ $2) }
              | {- empty -}               { [] }

Expr : "not" Expr             { ExprNot $2 }
     | Expr "**" Expr         { ExprOp $1 Power $3 } 
     | Expr "/" Expr          { ExprOp $1 Divide $3 }
     | Expr "*" Expr          { ExprOp $1 Multiply $3 }
     | Expr "+" Expr          { ExprOp $1 Plus $3 }
     | Expr "-" Expr          { ExprOp $1 Minus $3 }
     | Expr "<=" Expr         { ExprCompareOp $1 LessOrEqualThan $3 } 
     | Expr "<" Expr          { ExprCompareOp $1 LessThan $3 } 
     | Expr ">" Expr          { ExprCompareOp $1 GreaterThan $3 } 
     | Expr ">=" Expr         { ExprCompareOp $1 GreaterOrEqualThan $3 } 
     | Expr "==" Expr         { ExprCompareOp $1 Equals $3 }
     | Expr "!=" Expr         { ExprNot (ExprCompareOp $1 Equals $3) } 
     | Expr "&&" Expr         { ExprCompareOp $1 And $3 } 
     | Expr "||" Expr         { ExprCompareOp $1 Or $3 }
     | "(" Expr ")"           { ExprExpr $2} 
     | "[" ExprList "]"       { ExprArrayAssign $2 } 
     | ident "[" Expr "]"     { ExprArrayValue $1 $3 } 
     | intLit                 { ExprInt $1 } 
     | ident                  { ExprIdent $1 } 
     | "true"                 { ExprBool True }
     | "false"                { ExprBool False }
     | stringLit              { ExprString $1 }
     | "read" "(" ")"         { ExprRead ExprNothing }
     | "read" "(" Expr ")"    { ExprRead $3 }
     | "get" "(" ")"          { ExprGet ExprNothing }
     | "get" "(" Expr ")"     { ExprGet $3 }
     | "stream" "(" Expr ")"  { ExprStream $3 }
     | "length" "(" Expr ")"  { ExprLength $3 }
     | "null"                 { ExprNothing }

Type : "bool"          { TypeBool }
     | "int"           { TypeInt }
     | "string"        { TypeString }

{ 
parseError :: [Token] -> a
parseError [] = error " Unknown parse error"
parseError (x:xs) = error ("Parse error at line:column " ++ (token_posn x))

data Program 
    = Program StatementList
      deriving (Show, Eq)

data VarDeclr
    = VarDeclrOnly Type Ident
    | VarDeclrAssign Type Ident Expr
    deriving (Show, Eq)

data ArrayDeclr
    = ArrayDeclrOnly Type Ident
    | ArrayDeclrAssign Type Ident Expr
    deriving (Show,Eq)

data Statement
    = Statement String
    | StatementIfElse Expr StatementList StatementList
    | StatementIf Expr StatementList
    | StatementWhile Expr StatementList
    | StatementPrint Expr
    | StatementPrintLine Expr
    | StatementAssign Ident Expr
    | StatementArrayAssign Ident Expr Expr
    | StatementVarDeclr VarDeclr
    | StatementArrayDeclr ArrayDeclr
	| StatementConsume Expr
    | StatementError
    deriving (Show, Eq)

type StatementList = [Statement]

type ExprList = [Expr]

data Expr 
    = ExprString String 
    | ExprNot Expr 
    | ExprOp Expr Op Expr 
    | ExprCompareOp Expr CompareOp Expr 
    | ExprInt Int 
    | ExprBool Bool 
    | ExprIdent Ident 
    | ExprExpr Expr
    | ExprArrayAssign ExprList 
    | ExprArrayValue Ident Expr
    | ExprRead Expr
    | ExprGet Expr
    | ExprStream Expr
    | ExprLength Expr
    | ExprEmpty
    | ExprError
    | ExprNothing
    deriving (Show, Eq)

data Type 
    =   TypeInt
    |   TypeBool
    |   TypeString 
    |   TypeIntArray
    |   TypeBoolArray
    |   TypeStringArray
    |   TypeEmpty
    deriving (Show,Eq) 

data Op
    =  Minus 
    |  Plus 
    |  Multiply 
    |  Divide
    |  Power 
    deriving (Show, Eq)

data CompareOp
    =  GreaterThan 
    |  LessThan 
    |  LessOrEqualThan 
    |  GreaterOrEqualThan 
    |  Equals
    |  And
    |  Or 
    deriving (Show, Eq)

type Ident = String
type IntegerLit = Int


} 