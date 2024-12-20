{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module Algebra where

import Model
import Data.Foldable (Foldable(fold))
import Control.Applicative (Alternative)
import Data.Sequence (Seq(Empty))
import Data.List

-- Exercise 5
--From introduction to chater seven we find each recursive datatype should have a fold
--Each constructor has a have a corresponding function
--Our recursive datatypes are are Program which has multiple ocmmands,
--Alt which can have Pat and Cmds
--

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
foldProgram pAlg rAlg cAlg aAlg = pAlg. map (foldRule rAlg cAlg aAlg)

foldRule :: RuleAlgebra a -> CmdAlgebra a -> AltAlgebra a -> Rule -> a
foldRule rAlg cAlg aAlg (Rule identifier cmds) =
    rAlg identifier (map (foldCmd cAlg aAlg) cmds)

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
foldAlt aAlg cAlg (Alt pat cmds) = aAlg pat $ map (foldCmd cAlg aAlg) cmds



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram p = validateNonCalledIdentifiers p
                && validateSetsOfAlternatives p
                && noDoubles p
                && elementStart p

--Helperfunction for non called indentifiers
getRuleNames :: Program -> [Identifier]
getRuleNames = map (\(Rule ident _) -> ident)

getCalledRules :: Cmds -> [Identifier]
getCalledRules = concatMap getCalledRulesCmd
    where
        getCalledRulesCmd :: Cmd -> [Identifier]
        getCalledRulesCmd cmd = case cmd of
            Call ident -> [ident]
            Case _ alts -> concatMap (\(Alt _ cmds) -> getCalledRules cmds) alts
            _ -> []

--
validateNonCalledIdentifiers :: Program -> Bool
validateNonCalledIdentifiers program =
    let definedRules = getRuleNames program
        calledRules = foldProgram
            concat                      --ProgramAlgebra (combine all called rules)
            (\_ cmds -> concat cmds)    --RuleAlgebra     (combine all called rules in a rule)
            (  []                       --Go
            ,  []                       --Take
            ,  []                       --Mark
            ,  []                       --NothingCmd
            , const []                  --Turn
            , \_ alts -> concat alts    --Case has alternatives that can call have identifiers
            , \ident -> [ident]         --Call, returns the identifier    
            )
            (\_ cmds -> concat cmds)    --AltAlgebra
            program
    in all (`elem` definedRules) calledRules


--Step2 of Validation, all cases must have 5 choices or a wildcard
isCompletePatternSet :: [Pat] -> Bool
isCompletePatternSet pats
    | WildcardPat `elem` pats = True
    | otherwise = EmptyPat `elem` pats
                && LambdaPat `elem` pats
                && DebrisPat `elem` pats
                && AsteroidPat `elem` pats
                && BoundaryPat `elem` pats

getPatternsFromAlts :: Alts -> [Pat]
getPatternsFromAlts = map (\(Alt pat _) -> pat)


--This one doesn't work because of the type signature fault made in the fold. Don't know how to fix
validateSetsOfAlternatives :: Program -> Bool
validateSetsOfAlternatives = foldProgram
    and                         --All must be true so all rules must give true so and operator
    (\_ cmds -> and cmds)       --RuleAlgebra, also all commands must satisfy
    (True                       --Go
    ,True                       --Take
    ,True                       --Mark
    ,True                       --NothingCmd
    , const True                --Turn
    , \(Case dir alts) ->
        let patterns = [p | Alt p _ <- alts]
        in isCompletePatternSet patterns     --Case
    , const True                 --Call
    )
    (\pat cmds ->
        let currentCase = isCompletePatternSet [pat]
        in currentCase && and cmds)       --AltAlgebra

noDoubles :: Program -> Bool
noDoubles p = let rules = getRuleNames p
            in length rules == length (nub rules)

elementStart :: Program -> Bool
elementStart p = "start" `elem` getRuleNames p


testProgram :: Program
testProgram = [Rule "start" [Turn RightDir,Go,Turn LeftDir,Call "firstArg"],Rule "turnAround" [Turn RightDir,Turn RightDir],Rule "return" [Case FrontDir [Alt BoundaryPat [NothingCmd],Alt WildcardPat [Go,Call "return"]]],Rule "firstArg" [Case LeftDir [Alt LambdaPat [Go,Call "firstArg",Mark,Go],Alt WildcardPat [Call "turnAround",Call "return",Turn LeftDir,Go,Go,Turn LeftDir,Call "secondArg"]]],Rule "secondArg" [Case LeftDir [Alt LambdaPat [Go,Call "secondArg",Mark,Go],Alt WildcardPat [Call "turnAround",Call "return",Turn LeftDir,Go,Turn LeftDir]]]]

testPatternList :: Alts
testPatternList = [Alt LambdaPat [Go,Call "secondArg",Mark,Go],Alt WildcardPat [Call "turnAround",Call "return",Turn LeftDir,Go,Turn LeftDir]]

testProgram1 :: Program
testProgram1 =
    [ Rule "complete"
        [ Case FrontDir
            [ Alt EmptyPat [Go]
            , Alt LambdaPat [Take]
            , Alt DebrisPat [Mark]
            , Alt AsteroidPat [NothingCmd]
            , Alt BoundaryPat [Turn LeftDir]
            ]
        ]
    ]