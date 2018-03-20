{module Imp_Parse where
import Imp_Lex
import Imp_AbsSyntax
}

-- the parser implements a function
--
--           imp_parse :: [Token] -> Com
--
-- that converts a list of tokens to the abstract syntax tree of a command

%name imp_parse Com 
%tokentype { Token }
%error { parseError }

%token

--declaration of terminal symbols

--
-- [[DECLARE YOUR TERMINAL SYMBOLS HERE]]
--

        if                  {KEY "if"}
        then                {KEY "then"}
        else                {KEY "else"}
        while               {KEY "while"}
        do                  {KEY "do"}
        skip                {KEY "skip"}
        '~'                 {OP "=="}
        '<'                 {OP "<"}
        '+'                 {OP "+"}
        '-'                 {OP "-"}
        '*'                 {OP "*"}
        ':='                {OP ":="}
        '&&'                {OP "&&"}
        '||'                {OP "||"}
        '!'                 {UOP "!"}
        '('                 {PUNC "("}
        ')'                 {PUNC ")"}
        '{'                 {PUNC "{"}
        '}'                 {PUNC "}"}
        ';'                 {PUNC ";"}
        num                 {NUM $$}
        boolean             {BOOLEAN $$}
        loc                 {LOC $$}


-- precedence and associativity declarations, lowest precedence first

--
-- [[MAKE PRECEDENCE AND ASSOCIATIVITY DECLARATIONS HERE]]
--

%left '||'
%left '&&'
%right '!'
%nonassoc '~' '<' ':='
%left '+' '-'
%left '*'
%left else
%left do
%left ';'

  
%%

-- the grammar

--
-- [[DEFINE YOUR GRAMMAR AND ITS ACTIONS HERE]]
--


Com :   loc ':=' AExp               {Assign ($1, $3)}
    |   if BExp then Com else Com   {Cond ($2, $4, $6)}
    |   Com ';' Com                 {Seq ($1, $3)}
    |   skip                        {Skip}
    |   while BExp do Com           {While ($2, $4)}
    |   '{' Com '}'                 {$2}
        

AExp :  num                         {Num $1} 
    |   loc                         {Loc $1}
    |   AExp '+' AExp               {Add ($1, $3)}
    |   AExp '-' AExp               {Minus ($1, $3)}
    |   AExp '*' AExp               {Times ($1, $3)}
    |   '(' AExp ')'                {$2}
    

BExp :  boolean                     {Boolean $1}
    |   AExp '~' AExp               {Equal ($1, $3)}
    |   AExp '<' AExp               {LessThan ($1, $3)}
    |   BExp '&&' BExp              {And ($1, $3)}
    |   BExp '||' BExp              {Or ($1, $3)}
    |   '!' BExp                    {Not ($2)}
    |   '(' BExp ')'                {$2}

     
{
  
parseError :: [Token] -> a
parseError _ = error "Imp parse error"



}

