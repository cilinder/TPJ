
{module Imp_Lex where }

%wrapper "basic"

-- character classes

$digit      = [0-9]
$lower      = [a-z]
$upper      = [A-Z]
$symb       = [\!\@\#\$\%\^\&\-\+\=\/\<\>\~\:\;\.\?\/\\\~\'\"\|\[\]]

:-

-- lexical classes

--
-- [[IMPLEMENT YOUR LEXICAL CLASSES HERE]]
--

    $white+                                                     ;      -- whitespace
    if | then | else | Integer | Bool | skip | while | do       {\s -> KEY s}
    True | False                                                {\s -> BOOLEAN (read s)}
    $digit+                                                     {\s -> NUM (read s)}
    \=\= | \< | \+ | \- | \* | \&\& | \|\| | \:\=               {\s -> OP s}
    \!                                                          {\s -> UOP s}
    \( | \) | \; | \{ | \}                                      {\s -> PUNC s}
    [$upper][$lower $upper $digit]*                             {\s -> LOC s}
    $symb+                                                      {\_ -> UNKNOWNSYM}



{

-- The Token type - the action for each lexical class has type String -> Token

data Token =
    KEY String |
    OP String |
    UOP String |
    PUNC String |
    BOOLEAN Bool |
    NUM Integer |
    LOC String |
    UNKNOWNSYM
    deriving Show
    
-- The lexer implements a function
--        imp_lex :: String -> [Token]
--
-- which converts a string containing Imp program text into a list of tokens

imp_lex = alexScanTokens

}
