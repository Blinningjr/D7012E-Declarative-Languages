-- Niklas Ludnerg
module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip | -- My code.
    Body [Statement] | -- My code.
    While Expr.T Statement | -- My code.
    Read String | -- My code.
    Write Expr.T -- My code.
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

parseIf = (accept "if" -# (Expr.parse #- require "then")) # 
 ((parse #- require "else") # parse) >-> buildIf -- My code.
buildIf (e, (ls, rs)) = If e ls rs -- My code.

parseSkip = accept "skip" # require ";" >-> buildSkip -- My code.
buildSkip (_, _) = Skip -- My code.

parseBody = accept "begin" -# parseUntil "end" >-> buildBody -- My code.
buildBody l = Body l -- My code.

parseUntil v = (accept "end" -# return []) ! (parse # parseUntil v >-> cons) -- My code.
cons (a, b) = a:b

parseWhile = accept "while" -# (Expr.parse # (require "do" -# parse)) >-> buildWhile -- My code.
buildWhile (e, s) = While e s -- My code.

parseRead = accept "read" -# word #- require ";" >-> buildRead -- My code.
buildRead v = Read v

parseWrite = accept "write" -# Expr.parse #- require ";" >-> buildWrite -- My code.
buildWrite e = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ input = input -- My code.
exec (Assignment cs e: stnts) dict input = 
    exec stnts (Dictionary.insert (cs, (Expr.value e dict)) dict) input -- My code.
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Skip: stnts) dict input = exec stnts dict input -- My code.
exec (Body body: stnts) dict input = exec (body ++ stnts) dict input
exec (While e stmt: stmts) dict input -- My code.
 | Expr.value e dict > 0 = exec (stmt:(While e stmt):stmts) dict input
 | otherwise = exec stmts dict input
exec (Read cs: stmts) dict (x:xs) = exec stmts (Dictionary.insert (cs, x) dict) xs -- My code. 
exec (Write e: stmts) dict input = exec stmts dict ((Expr.value e dict):input) -- My code.


instance Parse Statement where
  parse = parseWhile ! parseIf ! parseBody ! parseRead ! parseWrite !
           parseSkip ! assignment -- My code.
 
  toString (Assignment ident val) = ident ++ " := " ++ Expr.toString val ++ "\n"
  toString (If cond lhs rhs) = "if " ++ Expr.toString cond ++ " then\n" ++
   toString lhs ++ "else\n" ++ toString rhs
  toString (Skip) = "skip;\n"
  toString (Body stnts) = "begin\n" ++ foldr (++) "" (map toString stnts) ++ "end\n"
  toString (While cond stnt) = "while " ++ Expr.toString cond ++ " do\n" ++ toString stnt
  toString (Read cs) = "read " ++ cs ++ ";\n"
  toString (Write e) = "write " ++ Expr.toString e ++ ";\n"

