{
module Lexer where

import Model
}

%wrapper "basic"

$digit =        [0-9]               --digits
$alpha =        [a-zA-Z]            --Letters
$ident =        [a-zA-Z0-9\+\-]    --Patternmatch for the identifierstring

tokens :-
  --handle whitespace and comments
  $white+            ;  --handle whitespaces
  "--".*             ; --handle comments

  --handle the tokens
  \-\>              {\s -> TArrow }
  \.                {\s -> TDot}
  \,                {\s -> TComma}
  go                {\s -> TGo}
  take              {\s -> TTake}
  mark              {\s -> TMark}
  nothing           {\s -> TNothing}
  turn              {\s -> TTurn}
  case              {\s -> TCase}
  of                {\s -> TOf}
  end               {\s -> TEnd}
  left              {\s -> TLeft}
  right             {\s -> TRight}
  front             {\s -> TFront}
  \;                {\s -> TSemicolon}
  Empty             {\s -> TEmpty}
  Lambda            {\s -> TLambda}
  Debris            {\s -> TDebris}
  Asteroid          {\s -> TAsteroid}
  Boundary          {\s -> TBoundary}
  \_                {\s -> TWildcard}
  $ident+           {\s -> TIdentifier s}



