
module Imp_Evaluator where
import Imp_AbsSyntax
import Imp_State


evalCom :: State -> Com -> State
evalBExp :: State -> BExp -> Bool
evalAExp :: State -> AExp -> Integer


--------------------------------
-- Evaluate command
--------------------------------

-- loc := aexp
evalCom state (Assign (loc, aexp)) =  
      let n = evalAExp state aexp in update state loc n
--    let val = evalAExp state aexp
--        in case val of 
--            (Integer n) -> update state loc n
--            _ -> error "Runtime type error"
    

-- if bexp then com1 else com2
evalCom state (Cond (bexp, com1, com2)) = 
    let d = evalBExp state bexp in 
        if d 
            then evalCom state com1 
            else evalCom state com2
    -- in case val of
        -- (Boolean True) -> evalCom state com1
        -- (Boolean False) -> evalCom state com2
        -- _ -> error "Runtime type error"

-- com1 ; com2
evalCom state (Seq (com1, com2)) = 
    let state' = evalCom state com1
    in evalCom state' com2
    

-- skip
evalCom state (Skip) = state

-- while bexp do com
evalCom state (While (bexp, com)) = 
    let d = evalBExp state bexp in 
        if d
            then let state' = evalCom state com in evalCom state' (While (bexp, com))
            else state
    -- in case d of
        -- (Boolean False) -> state
        -- (Boolean True) ->
            -- let state' = evalCom state com
            -- in evalCom state' (While (bexp com))

--------------------------------------
-- Evaluate boolean expression
--------------------------------------


evalBExp state (Boolean b) = b

evalBExp state (Equal (aexp1, aexp2)) = 
    let m = evalAExp state aexp1
        n = evalAExp state aexp2
    in m==n
    -- let val1 = evalAExp state aexp1
        -- val2 = evalAExp state aexp2
    -- in case (val1, val2) of
        -- (Num m, Num n) -> Boolean (m==n)
        -- _ -> error "Runtime type error"

evalBExp state (LessThan (aexp1, aexp2)) =
    let m = evalAExp state aexp1
        n = evalAExp state aexp2
    in m<n
    -- let val1 = evalAExp state aexp1
        -- val2 = evalAExp state aexp2
    -- in case (val1, val2) of
        -- (Num m, Num n) -> Boolean (m<n)
        -- _ -> error "Runtime type error"


evalBExp state (And (bexp1, bexp2)) =
    let d1 = evalBExp state bexp1 in if d1 then evalBExp state bexp2 else False
    -- let val1 = evalBExp state bexp1
        -- val2 = evalBExp state bexp2
    -- in case (val1, val2) of
        -- (Boolean True, Boolean b) -> (Boolean b)
        -- (Boolean False, _) -> (Boolean False)
        -- _ -> error "Runtime type error"

        
evalBExp state (Or (bexp1, bexp2)) =
    let d1 = evalBExp state bexp1 in if d1 then True else evalBExp state bexp2
    -- let val1 = evalBExp state bexp1
        -- val2 = evalBExp state bexp2
    -- in case (val1, val2) of
        -- (Boolean True, _) -> (Boolean True)
        -- (Boolean False, Boolean b) -> (Boolean b)
        -- _ -> error "Runtime type error"
        
        
evalBExp state (Not bexp) =
    let d = evalBExp state bexp in not d
    -- let val = evalBExp state bexp
    -- in case val of
        -- (Boolean b) -> (Boolean (not b))
        -- _ -> error "Runtime type error"

        
--------------------------------------
-- Evaluate arithmetic expression
--------------------------------------


evalAExp state (Loc loc) = valof state loc


evalAExp state (Num n) = n


evalAExp state (Add (aexp1, aexp2)) =
    let n1 = evalAExp state aexp1
        n2 = evalAExp state aexp2
    in n1+n2
    -- let val1 = evalAexp state aexp1
        -- val2 = evalAexp state aexp2
    -- in case (val1, val2) of
        -- (Num n1, Num n2) -> Num (n1+n2)
        -- _ -> error "Runtime type error"
        

evalAExp state (Minus (aexp1, aexp2)) =
    let n1 = evalAExp state aexp1
        n2 = evalAExp state aexp2
    in n1-n2
    -- let val1 = evalAexp state aexp1
        -- val2 = evalAexp state aexp2
    -- in case (val1, val2) of
        -- (Num n1, Num n2) -> Num (n1-n2)
        -- _ -> error "Runtime type error"
    

evalAExp state (Times (aexp1, aexp2)) =
    let n1 = evalAExp state aexp1
        n2 = evalAExp state aexp2
    in n1*n2    
    -- let val1 = evalAexp state aexp1
        -- val2 = evalAexp state aexp2
    -- in case (val1, val2) of
        -- (Num n1, Num n2) -> Num (n1*n2)
        -- _ -> error "Runtime type error"
    

--
-- [[DEFINE EVALUATION FUNCTIONS FOR:
--
--     *  ARITHMETIC EXPRESSIONS
--     *  BOOLEAN  EXPRESSIONS
--     *  COMMANDS
--
--   WITH THE TYPES SPECIFIED ABOVE]]
--
