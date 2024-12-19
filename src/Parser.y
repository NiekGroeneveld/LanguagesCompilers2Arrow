{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  "->"        { TArrow  }
  '.'         { TDot    }
  ','         { TComma  }
  go          { TGo     }
  take        { TTake   }
  mark        { TMark   }
  nothing     { TNothing}
  turn        { TTurn   }
  case        { TCase   }
  of          { TOf     }
  end         { TEnd    }
  left        { TLeft   }
  right       { TRight  }
  front       { TFront  }
  ';'         { TSemicolon}
  Empty       { TEmpty  }
  Lambda      { TLambda }
  Debris      { TDebris }
  Asteroid    { TAsteroid}
  Boundary    { TBoundary}
  '_'         { TWildcard}
  Identifier  { TIdentifier $$}




%%

Program : Program '.' Rule           {$3 : $1}
        | Rule '.'                   {[$1]}
     
Rule : Identifier "->" Cmds          {Rule $1 $3}

Cmds : Cmds ',' Cmd                  {$3 : $1}
     | Cmd                           {[$1]} 

Cmd : go                          {Go}
    | take                        {Take}
    | mark                        {Mark}
    | nothing                     {Nothing}
    | turn Dir                    {Turn $2}
    | case Dir of Alts            {Case $2 $4}
    | Identifier                  {Call $1}

Dir : left                        {LeftDir}
    | right                       {RightDir}
    | front                       {FrontDir}


Alts : Alts ';' Alt end           {$3 : $1}
      |Alt                        {[$1]}

Alt : Pat "->" Cmds               {Alt $1 $3}

Pat : Empty                       {EmptyPat}
    | Lambda                      {LambdaPat}
    | Debris                      {DebrisPat}
    | Asteroid                    {AsteroidPat}
    | Boundary                    {BoundaryPat}
    | '_'                         {WildcardPat}




{
happyError _ = error "parse error"
}