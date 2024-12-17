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
    | TIdent String     -- Represents an identifier
    deriving (Show, Eq)


-- Exercise 2
data Program = Program deriving Show
