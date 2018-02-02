module MiniLogo where

import Prelude hiding (Num)
import Data.List

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

    -- Define Line Macro --
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


    -- Define Nix Macro --
-- Concrete Syntax --
--      define nix (x,y,w,h){
--          line(x, y, x+w, y+h);
--          line(x+w, y, x, y+h);
--      }
nix = [Define "nix" ["x","y","w","h"]
    [Call "line" [V "x", V "y",Add (V "x") (V "w") , Add (V "y") (V "h") ] ,
    Call "line" [Add (V "x") (V "y") , V "y", V "x" , Add (V "y") (V "h") ]]]

    -- Define Steps function --
steps :: Int -> Prog
--At zero steps don't draw anything
steps 0 = []
--As steps increase, keep adding on line segments starting from the top.
--So basically you start at (i,i) and work backwards from there.
steps i = steps (i-1) ++ [ Call "line" [N (i-1), N (i-1), N(i-1), N i],
                            Call "line" [ N (i-1), N i, N i, N i]]



    -- Define macros function --
macros :: Prog->[Macro]
-- Exit case
macros [] = []
-- Go to macro to search for called macros
macros (Define macs _ progs : z) = macs : macros progs ++ macros z
-- Check for macro inside named macro
macros (Call macs _ : z) = macs : macros z
-- Finally Print out macro s
macros (x : z) = macros z

    -- Define pretty function --
-- Call with putStrLn
pretty :: Prog -> String
pretty [] = ""
--Print pen up/down commands
pretty (Pen Up:expr) = "\tpen up;\n" ++ pretty expr
pretty (Pen Down:expr) = "\tpen up;\n" ++ pretty expr
--Print move commands
pretty (Move (x, y):expr) = "\tmove (" ++ prettyE x ++ ", " ++ prettyE y ++
                            ");\n" ++ pretty expr
--Break up list of expressions
pretty (Call x lexpr:expr) = "\t" ++ x ++ "(" ++
                            intercalate ", " (map prettyE lexpr) ++
                            ");\n" ++ pretty expr
pretty (Define m vs p:ps) = "define " ++ m ++ "(" ++ intercalate ", " vs ++
                            "){\n" ++ pretty p ++ "}; " ++ pretty ps

-- Define helper expression for pretty that prettys expressions
prettyE :: Expr -> String
prettyE (V x) = x
--Convert integers into strings
prettyE (N x) = (show x)
--Convert Add expressions
prettyE (Add x y) = prettyE x ++ " + " ++ prettyE y


--  Extra Credit --

    --Define optE Function--
optE :: Expr -> Expr
-- If it finds an add function add the values together
optE (Add (N a) (N b)) = N (a + b)
-- Otherwise do nothing
optE x = x

    --Define optP Function--
optP :: Prog -> Prog
--Default case
optP [] = []
--If it encounters move
optP (Move (x,y) : z) = Move(optE x, optE y): optP z
--If it encounters Call
optP (Call x args : z) = Call x (callHelper args ) : optP z
--If it enounters Define
optP (Define w x y : z) = Define w x (optP y): optP z
--For anthying else, like pen up/pen down.
optP (x : z) = x : optP z


-- Breaks call list
callHelper :: [Expr] -> [Expr]
callHelper [] = []
callHelper (x : y) = optE x : callHelper y

--Test Case for optP
test = [Define "test" ["x1","y1","x2","y2"] [Move(V "x1", Add(N 1) (N 2)), Move(V "x2", V "y2")]]
