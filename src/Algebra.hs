module Algebra where

import Model
import Data.Foldable (Foldable(fold))
import Data.List (group)
import Control.Applicative (Alternative)


-- Exercise 5
--From introduction to chater seven we find each recursive datatype should have a fold
--Each constructor has a have a corresponding function
--Our recursive datatypes are are Program which has multiple ocmmands,
--Alt which can have Pat and Cmds
--
-- First let's  define the type aliases for your algebra
type ProgramAlgebra a = [a] -> a
type RuleAlgebra a = Identifier -> [a] -> a
type CmdAlgebra a = 
    ( a                     --Go
    , a                     --Take
    , a                     --Mark
    , a                     --NothingCmd
    , Dir -> a              --Turn
    , Dir -> [a] -> a       --Case
    , Identifier -> a       --Call
    )

type AltAlgebra a = Pat -> [a] -> a

--Folds
foldProgram :: ProgramAlgebra a -> RuleAlgebra a -> CmdAlgebra a -> AltAlgebra a -> Program -> a
foldProgram pAlg rAlg cAlg aAlg = pAlg. map(foldRule rAlg cAlg aAlg)

foldRule :: RuleAlgebra a -> CmdAlgebra a -> AltAlgebra a -> Rule -> a
foldRule rAlg cAlg aAlg (Rule identifier cmds) =
    rAlg identifier (map(foldCmd cAlg aAlg) cmds)

foldCmd :: CmdAlgebra a -> AltAlgebra a -> Cmd -> a
foldCmd cAlg@(go, take, mark, nothing, turn, case_, call) aAlg cmd = 
    case cmd of
        Go -> go
        Take -> take
        Mark -> mark
        NothingCmd -> nothing
        Turn dir -> turn dir
        Case dir alts -> case_ dir (map (foldAlt aAlg cAlg) alts)
        Call ident -> call ident

foldAlt :: AltAlgebra a -> CmdAlgebra a-> Alt -> a
foldAlt aAlg cAlg (Alt pat cmds) = aAlg pat $ map(foldCmd cAlg aAlg) cmds



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined