
module Model where

-- Exercise 1
data Token    = TArrow            -- "->"
    | TDot              -- "."
    | TComma            -- ","
    | TGo               -- "go"
    | TTake             -- "take"
    | TMark             -- "mark"
    | TNothing          -- "nothing"
    | TTurn             -- "turn"
    | TCase             -- "case"
    | TOf               -- "of"
    | TEnd              -- "end"
    | TLeft             -- "left"
    | TRight            -- "right"
    | TFront            -- "front"
    | TSemicolon        -- ";"
    | TEmpty            -- "Empty"
    | TLambda           -- "Lambda"
    | TDebris           -- "Debris"
    | TAsteroid         -- "Asteroid"
    | TBoundary         -- "Boundary"
    | TWildcard         -- "_"
    | TIdentifier String     -- Represents an identifier
    deriving (Show, Eq)


-- Exercise 2
--A program is a list of rules
type Program = [Rule]

-- A rule couples an identifier with a sequence of commands
data Rule = Rule Identifier Cmds deriving Show

type Cmds = [Cmd]

data Cmd 
    = Go                --Move forward
    | Take              --Pick up the item
    | Mark              --Leave a lambda at the current position
    | NothingCmd        --Do Nothing
    | Turn Dir          --Turn in a Direction
    | Case Dir Alts     --Case analysis on a direciton
    | Call Identifier   --Call antoher rule
    deriving Show 

-- Direction in which Arrow can turn or Sens
data Dir = LeftDir | RightDir | FrontDir deriving Show

-- Alternatives for a case construct
type Alts = [Alt] 

data Alt = Alt Pat Cmds deriving Show

--Patterns for case analysis
data Pat
    = EmptyPat          --Empty pattern
    | LambdaPat         --Lambda pattern
    | DebrisPat         --Debris pattern
    | AsteroidPat       --Asteroid pattern
    | BoundaryPat       --Boundary pattern
    | WildcardPat       --Wildcard pattern
    deriving Show   

--Identifier for naming rules
type Identifier = String




