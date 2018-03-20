{-# OPTIONS_GHC -w #-}
module Imp_Parse where
import Imp_Lex
import Imp_AbsSyntax
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,103) ([2624,4224,0,1024,8192,0,0,8,12288,28,3072,7,0,10496,16896,0,768,31744,0,49216,0,49152,112,12288,28,0,0,0,0,0,128,12,656,1056,0,322,1792,0,8192,20,0,41984,2048,1,28864,0,7216,31744,16,49152,4,0,16384,32778,16,2048,5,16896,1,20608,0,5152,0,1288,0,0,0,0,256,0,64,0,28,0,7,0,512,0,0,0,0,4096,0,0,16384,32768,0,1052,10496,16896,0,512,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_imp_parse","Com","AExp","BExp","if","then","else","while","do","skip","'~'","'<'","'+'","'-'","'*'","':='","'&&'","'||'","'!'","'('","')'","'{'","'}'","';'","num","boolean","loc","%eof"]
        bit_start = st * 30
        bit_end = (st + 1) * 30
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..29]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (7) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (12) = happyShift action_6
action_0 (24) = happyShift action_7
action_0 (29) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (29) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (18) = happyShift action_18
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (26) = happyShift action_17
action_3 (30) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (21) = happyShift action_11
action_4 (22) = happyShift action_12
action_4 (27) = happyShift action_13
action_4 (28) = happyShift action_14
action_4 (29) = happyShift action_15
action_4 (5) = happyGoto action_9
action_4 (6) = happyGoto action_16
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (21) = happyShift action_11
action_5 (22) = happyShift action_12
action_5 (27) = happyShift action_13
action_5 (28) = happyShift action_14
action_5 (29) = happyShift action_15
action_5 (5) = happyGoto action_9
action_5 (6) = happyGoto action_10
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_4

action_7 (7) = happyShift action_4
action_7 (10) = happyShift action_5
action_7 (12) = happyShift action_6
action_7 (24) = happyShift action_7
action_7 (29) = happyShift action_2
action_7 (4) = happyGoto action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (25) = happyShift action_34
action_8 (26) = happyShift action_17
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (13) = happyShift action_29
action_9 (14) = happyShift action_30
action_9 (15) = happyShift action_31
action_9 (16) = happyShift action_32
action_9 (17) = happyShift action_33
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (11) = happyShift action_28
action_10 (19) = happyShift action_23
action_10 (20) = happyShift action_24
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (21) = happyShift action_11
action_11 (22) = happyShift action_12
action_11 (27) = happyShift action_13
action_11 (28) = happyShift action_14
action_11 (29) = happyShift action_15
action_11 (5) = happyGoto action_9
action_11 (6) = happyGoto action_27
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (21) = happyShift action_11
action_12 (22) = happyShift action_12
action_12 (27) = happyShift action_13
action_12 (28) = happyShift action_14
action_12 (29) = happyShift action_15
action_12 (5) = happyGoto action_25
action_12 (6) = happyGoto action_26
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_7

action_14 _ = happyReduce_13

action_15 _ = happyReduce_8

action_16 (8) = happyShift action_22
action_16 (19) = happyShift action_23
action_16 (20) = happyShift action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (7) = happyShift action_4
action_17 (10) = happyShift action_5
action_17 (12) = happyShift action_6
action_17 (24) = happyShift action_7
action_17 (29) = happyShift action_2
action_17 (4) = happyGoto action_21
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (22) = happyShift action_20
action_18 (27) = happyShift action_13
action_18 (29) = happyShift action_15
action_18 (5) = happyGoto action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (15) = happyShift action_31
action_19 (16) = happyShift action_32
action_19 (17) = happyShift action_33
action_19 _ = happyReduce_1

action_20 (22) = happyShift action_20
action_20 (27) = happyShift action_13
action_20 (29) = happyShift action_15
action_20 (5) = happyGoto action_46
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_3

action_22 (7) = happyShift action_4
action_22 (10) = happyShift action_5
action_22 (12) = happyShift action_6
action_22 (24) = happyShift action_7
action_22 (29) = happyShift action_2
action_22 (4) = happyGoto action_45
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (21) = happyShift action_11
action_23 (22) = happyShift action_12
action_23 (27) = happyShift action_13
action_23 (28) = happyShift action_14
action_23 (29) = happyShift action_15
action_23 (5) = happyGoto action_9
action_23 (6) = happyGoto action_44
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (21) = happyShift action_11
action_24 (22) = happyShift action_12
action_24 (27) = happyShift action_13
action_24 (28) = happyShift action_14
action_24 (29) = happyShift action_15
action_24 (5) = happyGoto action_9
action_24 (6) = happyGoto action_43
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (13) = happyShift action_29
action_25 (14) = happyShift action_30
action_25 (15) = happyShift action_31
action_25 (16) = happyShift action_32
action_25 (17) = happyShift action_33
action_25 (23) = happyShift action_42
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (19) = happyShift action_23
action_26 (20) = happyShift action_24
action_26 (23) = happyShift action_41
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_18

action_28 (7) = happyShift action_4
action_28 (10) = happyShift action_5
action_28 (12) = happyShift action_6
action_28 (24) = happyShift action_7
action_28 (29) = happyShift action_2
action_28 (4) = happyGoto action_40
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (22) = happyShift action_20
action_29 (27) = happyShift action_13
action_29 (29) = happyShift action_15
action_29 (5) = happyGoto action_39
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (22) = happyShift action_20
action_30 (27) = happyShift action_13
action_30 (29) = happyShift action_15
action_30 (5) = happyGoto action_38
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (22) = happyShift action_20
action_31 (27) = happyShift action_13
action_31 (29) = happyShift action_15
action_31 (5) = happyGoto action_37
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (22) = happyShift action_20
action_32 (27) = happyShift action_13
action_32 (29) = happyShift action_15
action_32 (5) = happyGoto action_36
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (22) = happyShift action_20
action_33 (27) = happyShift action_13
action_33 (29) = happyShift action_15
action_33 (5) = happyGoto action_35
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_6

action_35 _ = happyReduce_11

action_36 (17) = happyShift action_33
action_36 _ = happyReduce_10

action_37 (17) = happyShift action_33
action_37 _ = happyReduce_9

action_38 (15) = happyShift action_31
action_38 (16) = happyShift action_32
action_38 (17) = happyShift action_33
action_38 _ = happyReduce_15

action_39 (15) = happyShift action_31
action_39 (16) = happyShift action_32
action_39 (17) = happyShift action_33
action_39 _ = happyReduce_14

action_40 (26) = happyShift action_17
action_40 _ = happyReduce_5

action_41 _ = happyReduce_19

action_42 _ = happyReduce_12

action_43 (19) = happyShift action_23
action_43 _ = happyReduce_17

action_44 _ = happyReduce_16

action_45 (9) = happyShift action_47
action_45 (26) = happyShift action_17
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (15) = happyShift action_31
action_46 (16) = happyShift action_32
action_46 (17) = happyShift action_33
action_46 (23) = happyShift action_42
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (7) = happyShift action_4
action_47 (10) = happyShift action_5
action_47 (12) = happyShift action_6
action_47 (24) = happyShift action_7
action_47 (29) = happyShift action_2
action_47 (4) = happyGoto action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (26) = happyShift action_17
action_48 _ = happyReduce_2

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (LOC happy_var_1))
	 =  HappyAbsSyn4
		 (Assign (happy_var_1, happy_var_3)
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Cond (happy_var_2, happy_var_4, happy_var_6)
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq (happy_var_1, happy_var_3)
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (Skip
	)

happyReduce_5 = happyReduce 4 4 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While (happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyTerminal (NUM happy_var_1))
	 =  HappyAbsSyn5
		 (Num happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyTerminal (LOC happy_var_1))
	 =  HappyAbsSyn5
		 (Loc happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Add (happy_var_1, happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Minus (happy_var_1, happy_var_3)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Times (happy_var_1, happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyTerminal (BOOLEAN happy_var_1))
	 =  HappyAbsSyn6
		 (Boolean happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (Equal (happy_var_1, happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (LessThan (happy_var_1, happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (And (happy_var_1, happy_var_3)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Or (happy_var_1, happy_var_3)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Not (happy_var_2)
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	KEY "if" -> cont 7;
	KEY "then" -> cont 8;
	KEY "else" -> cont 9;
	KEY "while" -> cont 10;
	KEY "do" -> cont 11;
	KEY "skip" -> cont 12;
	OP "==" -> cont 13;
	OP "<" -> cont 14;
	OP "+" -> cont 15;
	OP "-" -> cont 16;
	OP "*" -> cont 17;
	OP ":=" -> cont 18;
	OP "&&" -> cont 19;
	OP "||" -> cont 20;
	UOP "!" -> cont 21;
	PUNC "(" -> cont 22;
	PUNC ")" -> cont 23;
	PUNC "{" -> cont 24;
	PUNC "}" -> cont 25;
	PUNC ";" -> cont 26;
	NUM happy_dollar_dollar -> cont 27;
	BOOLEAN happy_dollar_dollar -> cont 28;
	LOC happy_dollar_dollar -> cont 29;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 30 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
imp_parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Imp parse error"
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc4868_0\\ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
