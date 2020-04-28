-- Niklas Lundberg
module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- My code.
instance Parse T where
  parse = iter Statement.parse >-> Program -- My code.
  toString (Program stnts) = foldr (++) "" (map Statement.toString stnts)

exec (Program stmts) input = Statement.exec stmts Dictionary.empty input -- My code.

