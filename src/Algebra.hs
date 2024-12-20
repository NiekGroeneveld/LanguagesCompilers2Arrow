module Algebra where

import Model
import Data.Foldable (Foldable(fold))
import Data.List (group)
import Control.Applicative (Alternative)


-- Exercise 5



type ProgramAlgebra p = (PAlgebra p,    
                        RuleAlgebra p, 
                        CmdsAlgebra p, 
                        CommandAlgebra p,  
                        AltsAlgebra p, 
                        AltAlgebra p, 
                        PatternAlgebra p, 
                        DirectionAlgebra p
                        )

foldProgram :: ProgramAlgebra p -> Program -> p
foldProgram = undefined




type PAlgebra p = [Rule] -> p
type RuleAlgebra r = (Identifier -> Cmds -> r)
type CmdsAlgebra cs = [Cmd]
type AltsAlgebra a = [Alt]
type AltAlgebra a = (Pat -> Cmds -> a)

type CommandAlgebra c = (c          --Go
                        ,c          --Take
                        ,c          --Mark
                        ,c          --Nothing
                        ,Dir -> c   --Turn
                        ,Dir -> Alts -> c   --Case
                        ,Identifier -> c    --Identifier
                    )

foldCmd :: CommandAlgebra a -> Cmd -> a
foldCmd (go, take, mark, nothing, turn, case', call) = f where
    f Go = go
    f Take = take
    f Mark = mark
    f NothingCmd = nothing
    f (Turn d) = turn d             --should do something with these
    f (Case d alts) = case' d alts  --should do something here
    f (Call s) = call s --ye


type PatternAlgebra p = ( p --empty
                        , p --lambda
                        , p --debris
                        , p --asteroid
                        , p --boundary
                        , p --wildcard
                        )
foldPattern :: PatternAlgebra a -> Pat -> a
foldPattern (empty, lambda, debris, asteroid, boundary, wildcard) = f where
    f EmptyPat = empty
    f LambdaPat = lambda
    f DebrisPat = debris
    f AsteroidPat = asteroid
    f BoundaryPat = boundary
    f WildcardPat = wildcard


type DirectionAlgebra d = (d, d, d)
foldDir :: DirectionAlgebra d -> Dir -> d
foldDir (left, right, front) = f where
    f RightDir = right
    f LeftDir = left
    f FrontDir = front


-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined