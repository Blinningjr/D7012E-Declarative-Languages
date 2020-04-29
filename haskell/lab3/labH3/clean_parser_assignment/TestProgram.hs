{- Test for Program -}
module TestProgram where

import Program
p, p1, p2 :: Program.T
p = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")
p2 = fromString  ("\
\read n;\
\s := 0;\
\repeat\
\  begin\
\    s := s + n;\
\    n := n - 1;\
\  end\
\until (0-n)+1;\
\write s;")

sp = putStr (toString p)

sp1 = putStr (toString p1)

sp2 = putStr (toString p2)

rp = Program.exec p [3,16]

rp1 = Program.exec p1 [1024, 2]

rp2 = Program.exec p2 [5]

