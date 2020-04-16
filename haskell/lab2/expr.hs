-- Niklas Lundberg
-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

import Data.Char


-- Defines the data type EXPR.
data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)


-- Parses a String into an EXPR.
parse :: String -> EXPR
parse = fst . buildexpr
  where
    -- Checks if the first Char x dosen`t have the propery p.
    notfirst :: (Char -> Bool) -> (EXPR, String) -> Bool
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    -- Builds a Const EXPR until the first non number is encounterd.
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        -- Adds the next digit to the already parsed number * 10. So 34 = 34 and not 7.
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    -- Builds a Var EXPR until the first non letter is encounterd.
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        -- Adds the next letter to the already parsed Word.
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    -- Builds an EXPR of any type, where Op + or - is at the top of the AST.
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        -- Creates a EXPR of type OP + or -.
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    -- Builds an EXPR of any type except Op + or -, where Op * or / is at the top.
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        -- Creates a EXPR of type Op * or /.
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    -- Builds an EXPR of type Const or Var. Also throws error if it fails.
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"


-- Unparses an EXPR back into a String.
unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"


-- Evaluates an EXPR into a Float using an Enviorment that defines the variables.
eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" e) env = sin (eval e env) -- MyCode
eval (App "cos" e) env = cos (eval e env) -- MyCode
eval (App "log" e) env = log (eval e env) -- MyCode
eval (App "exp" e) env = exp (eval e env) -- MyCode


-- Differentiates an EXPR symbolically with respect to a var.
-- Var -> EXPR -> Result
diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = simplify (Op "+" (diff v e1) (diff v e2))
diff v (Op "-" e1 e2) = simplify (Op "-" (diff v e1) (diff v e2))
diff v (Op "*" e1 e2) =
  simplify (Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2)))
diff v (Op "/" e1 e2) =
  simplify (Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2))
diff v (App "sin" e) = simplify (Op "*" (diff v e) (App "cos" e)) -- My Code.
diff v (App "cos" e) = simplify (Op "*" (diff v e) (App "sin" e)) -- My Code.
diff v (App "log" e) = simplify (Op "/" (Const 1) (diff v e)) -- My Code.
diff v (App "exp" e) = simplify (Op "*" (App "exp" e) (diff v e)) -- My Code.
diff _ _ = error "can not compute the derivative"


-- Simplifies an expresion.
simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App id e) = App id (simplify e) -- My Code.

