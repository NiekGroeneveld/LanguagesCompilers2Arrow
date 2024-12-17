module Model where
import GHC.Generics (DecidedStrictness)
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
    | TIdent String     -- Represents an identifier
    deriving (Show, Eq)


-- Exercise 2
--A program is a list of rules
data Program = Program [Rule] deriving Show

-- A rule couples an identifier with a sequence of commands
data Rule = Rule Ident [Cmd] deriving Show

data Cmd 
    = Go                --Move forward
    | Take              --Pick up the item
    | Mark              --Leave a lambda at the current position
    | NothingCmd        --Do Nothing
    | Turn Dir          --Turn in a Direction
    | Case Dir [Alt]    --Case analysis on a direciton
    | Call Ident        --Call antoher rule
    deriving Show 

-- Direction in which Arrow can turn or Sens
data Dir = LeftDir | RightDir | FrontDir deriving Show

-- Alternatives for a case construct
data Alt = Alt Pat [Cmd] deriving Show

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
type Ident = String




