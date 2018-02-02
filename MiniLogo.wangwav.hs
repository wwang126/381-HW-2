module MiniLogo where

import Prelude hiding (Num)

--Wavelet Wang
--wangwav
--HW2
--Feb-1-18

--Type Defs
type Num = Int
type Var = String
type Macro = String
type Prog = [Cmd]

--Define Basic MiniLogo syntax
--Define Expressions
data Expr = V Var
        | N Num
        | Add Expr Expr
        deriving (Show,Eq)

--Define the pen mode
data Mode = Up
        | Down
        deriving(Show,Eq)
--Define Cmd
data Cmd = Pen Mode
        | Move(Expr, Expr)
        | Call Macro[Expr]
        | Define Macro [Var] Prog
        deriving(Show,Eq)

-- Define Line Macro
-- Concrete Syntax --
--      define line (x1,y1,x2,y2){
--          pen up;
--          move (x1,y1);
--          pen down;
--          move(x2,y2);
--      }
-- Passes information to actual functions
line = [Define "line" ["x1","y1","x2","y2"]
    [Pen Up, Move(V "x1",V "y1"),
    Pen Down, Move(V "x2", V "y2")]]
-- Define Nix Macro
-- Concrete Syntax --
--      define nix (x,y,w,h){
--          line(x, y, x+w, y+h);
--          line(x+w, y, x, y+h);
--      }
nix = [Define "nix" ["x","y","w","h"]
    [Call "line" [V "x", V "y",Add (V "x") (V "w") , Add (V "y") (V "h") ] ,
    Call "line" [Add (V "x") (V "y") , V "y", V "x" , Add (V "y") (V "h") ]]]
-- Define Steps Macro

-- Define macros function

-- Define pretty function
