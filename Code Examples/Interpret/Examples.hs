
module Examples where

import AST

{-
f(x) = x+1
f 2
-}
example1
  = Prog
    [Fundef "f" ["x"] (Op Plus (Var "x") (Num 1))]
    (Fun "f" [Num 2])

{-
let fact(n) = if n==1 then 1 else n*fact(n-1)
in fact 5
-}
example2
  = Prog
    [Fundef "fact" ["n"]
      (IfElse (Op Eq (Var "n") (Num 1))
       (Num 1)
       (Op Mult (Var "n") (Fun "fact" [Op Minus (Var "n") (Num 1)]))
      )]
    (Fun "fact" [Num 5])

{-
let gcd(a,b) = if b==0 then a else gcd(b, a%b)
in gcd 36 15
-}
example3
  = Prog
    [Fundef "gcd" ["a","b"]
     (IfElse (Op Eq (Var "b") (Num 0))
      (Var "a")
      (Fun "gcd" [Var "b", Op Mod (Var "a") (Var "b")]))
    ]
    (Fun "gcd" [Num 36, Num 15])


example4
  = Prog [Fundef "gcd" ["a", "b"]
          (Let  "r" (Num 0)
          (Seq
            (While (Op Neq (Var "b") (Num 0))
             (Seq
              (Seq
               (Assign "r" (Op Mod (Var "a") (Var "b")))
               (Assign "a" (Var "b")))
               (Assign "b" (Var "r"))))
            (Var "a")))]
    (Fun "gcd" [Num 36, Num 15])


example5
  = Prog []
     (Let "n" (Fun "scani" [])
      (Fun "printi" [Op Mult (Var "n") (Var "n")]))
    
example6
  = Prog
    [Fundef "fact" ["n"]
     (Let "r" (Num 1)
      (Seq
       ((While (Op Lt (Num 0) (Var "n") )
         (Seq
          (Assign "r" (Op Mult (Var "n") (Var "r")))
          (Assign "n" (Op Plus (Var "n") (Num (-1)))))
        )
       )
       (Var "r")))]
    (Fun "fact" [Num 5])



{-

let f(x,y) = x + y
in let t = 0
   in f((t:=t+1;t), 1)

let f(x,y) = x + y
in let t = 0
   in f(t, (t:=t+1;1))

-}

example8
  = Prog
    [Fundef "f" ["x","y"] (Op Plus (Var "x") (Var "y"))]
    (Let "t" (Num 0)
    (Fun "f" [ Var "t",
               (Seq (Assign "t" (Op Plus (Var "t") (Num 1)))
               (Num 1)) ]))
    
example7
  = Prog
    [Fundef "f" ["x","y"] (Op Plus (Var "x") (Var "y"))]
    (Let "t" (Num 0)
    (Fun "f" [ (Seq (Assign "t" (Op Plus (Var "t") (Num 1)))
               (Var "t")),
               (Num 1)]))
    

{-
let a = 0
in (a:=a+1;a) + (a:=a*2;a)
-}

example9
  = Prog []
    (Let "a" (Num 0)
     (Op Plus
      (Seq (Assign "a" (Op Plus (Var "a") (Num 1)))
       (Var "a"))
      (Seq (Assign "a" (Op Mult (Var "a") (Num 2)))
       (Var "a"))))


{-
let a = 0
in (a:=a+1;a*2)
-}
example10
  = Prog []
    (Let "a" (Num 0)
     (Seq
      (Assign "a" (Op Plus (Var "a") (Num 1)))
      (Op Mult (Var "a") (Num 2))))
