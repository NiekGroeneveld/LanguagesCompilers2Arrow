module Interpreter where

import ParseLib

import Data.Map (Map, findMax, lookup)
import qualified Data.Map as L

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace s =
  let size = findSize s
      spaceArray = [printrow s size size | size <- [0..size]]
  in  concat spaceArray 


printrow :: Space -> Int -> Int -> String
printrow s y size =
  let row = [printpos s (x,y) | x <- [0..size]]
  in  concat row

printpos :: Space -> Pos -> String
printpos s pos  = case Data.Map.lookup pos s  of
  Just Empty    -> "."
  Just Lambda   -> "\\"
  Just Debris   -> "%"
  Just Asteroid -> "O"
  Just Boundary -> "#"
  Nothing       -> "error"

findSize :: Space -> Int
findSize s = fst $ fst (findMax s) 


-- These three should be defined by you
type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment spaceString = undefined


-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


