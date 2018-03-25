module Imp_Parser where
import Imp_Lex
import Imp_State
import Imp_Parse


-- parseImp "file.imp" s 
--
-- lexes and parses the Imp program text in file.imp and then
-- executes it starting in state s

parseImp filename s = do
    progtext <- readFile filename
    let lexed   = imp_lex progtext
        parsed  = imp_parse lexed
        in print parsed
      
      
      
      
