{
module Parser where

import Lexer
import qualified AST
}

%name parse 
%tokentype { Token }
%error { parseError }

%token 
    -- Reserverd words
    beginWorld               { TKbeginWorld _ }
    endWorld                 { TKendWorld _ }
    World                    { TKWorld _ }
    Wall                     { TKwall _ }
    from                     { TKfrom _ }
    to                       { TKto _ }
    ObjectType               { TKObjectType _ }
    of                       { TKof _ }
    color                    { TKcolor _ }
    red                      { TKred _ }
    blue                     { TKblue _ }
    mangenta                 { TKmangenta _ }
    cyan                     { TKcyan _ }
    green                    { TKgreen _ }
    yellow                   { TKyellow _ }
    Place                    { TKPlace _ }
    at                       { TKat _ }
    basket                   { TKbasket _ }
    in                       { TKin _ }
    Start                    { TKStart _ }
    heading                  { TKheading _ }
    Basket                   { TKBasket _ }
    capacity                 { TKcapacity _ }
    with                     { TKwith _ }
    initial                  { TKinitial _ }
    value                    { TKvalue _ }
    Goal                     { TKGoal _ }
    is                       { TKis _ }
    Final                    { TKFinal _ }
    goal                     { TKgoal _ }
    willy                    { TKwilly _ }
    objects                  { TKobjects _ }
    and                      { TKand _ }
    or                       { TKor _ }
    not                      { TKnot _ }
    beginTask                { TKbeginTask _ }
    on                       { TKon _ }
    endTask                  { TKendTask _ }
    if                       { TKif _ }
    else                     { TKelse _ }
    then                     { TKthen _ }
    repeat                   { TKrepeat _ }
    times                    { TKtimes _ }
    while                    { TKwhile _ }
    do                       { TKdo _ }
    begin                    { TKbegin _ }
    end                      { TKend _ }
    define                   { TKdefine _ }
    as                       { TKas _ }
    move                     { TKmove _ }
    turnRight                { TKturnRight _ }
    turnLeft                 { TKturnLeft _ }
    pick                     { TKpick _ }
    drop                     { TKdrop _ }
    set                      { TKset _ }
    clear                    { TKclear _ }
    flip                     { TKflip _ }
    terminate                { TKterminate _ }
    found                    { TKfound _ }
    carrying                 { TKcarrying _ }
    frontClear               { TKfrontClear _ }
    leftClear                { TKleftClear _ }
    rightClear               { TKrightClear _ }
    lookingNorth             { TKlookingNorth _ }
    lookingEast              { TKlookingEast _ }
    lookingSouth             { TKlookingSouth _ }
    lookingWest              { TKlookingWest _ }
    north                    { TKnorth _ }
    south                    { TKsouth _ }
    east                     { TKeast _ }
    west                     { TKwest _ }

    -- Boolean
    Boolean                  { TKBoolean _ }
    true                     { TKtrue _ }
    false                    { TKfalse _ }

    -- Integers
    Int                      { TKInt _ _ }

    -- Identifiers
    Id                       { TKId _ _ }

    -- Symbols
    '('                      { TKopenBracket _ }
    ')'                      { TKcloseBracket _ }
    ';'                      { TKsemicolon _ }

-- Precedence list
%left and or  
%nonassoc not

-- TO DO

%% -- Grammar

BLOCK :: { [AST.BLOCK] }
BLOCK      : BLOCK WORLD                                { $2:$1 }
           | BLOCK TASK                                 { $2:$1 }
           | WORLD                                      { [$1] }
           | TASK                                       { [$1] }


WORLD :: { AST.BLOCK }
WORLD      : beginWorld Id INSTRS endWorld              { AST.WORLD (tokenPos $1) $2 (reverse $3) }
           | beginWorld Id endWorld                     { AST.WORLD (tokenPos $1) $2 [] }
           | beginWorld Id SC endWorld                  { AST.WORLD (tokenPos $1) $2 [] }
           | beginWorld Id SC INSTRS endWorld           { AST.WORLD (tokenPos $1) $2 (reverse $4) }

INSTRS :: { [AST.INSTR] }
INSTRS     : INSTRS INSTR                               { $2:$1 }
           | INSTRS ';'                                 { $1 }
           | INSTR                                      { [$1] }

INSTR :: { AST.INSTR }
INSTR      : World Int Int ';'                          { AST.WORLDSIZE (tokenPos $1) $2 $3 }
           | Wall DIRECTION from Int Int to Int Int ';' { AST.WALL (tokenPos $1) $2 $4 $5 $7 $8  }        
           | ObjectType Id of color COLORVAL ';'        { AST.OBJECTTYPE (tokenPos $1) $2 $5 }
           | Place Int of Id at Int Int ';'             { AST.PLACEAT (tokenPos $1) $2 $4 $6 $7 }
           | Place Int of Id in basket ';'              { AST.PLACEIN (tokenPos $1) $2 $4 }
           | Start at Int Int heading DIRECTION ';'     { AST.STARTAT (tokenPos $1) $3 $4 $6 }
           | Basket of capacity Int ';'                 { AST.BASKET (tokenPos $1) $4 }
           | Boolean Id with initial value true ';'     { AST.BOOLEAN (tokenPos $1) $2 $6 }
           | Boolean Id with initial value false ';'    { AST.BOOLEAN (tokenPos $1) $2 $6 }
           | Goal Id is GOALTEST ';'                    { AST.GOALIS (tokenPos $1) $2 $4 }
           | Final goal is FINALGOAL ';'                { AST.FINALIS (tokenPos $1) $4 }

DIRECTION :: { Token }
DIRECTION  : north                                      { $1 }
           | east                                       { $1 }
           | south                                      { $1 }
           | west                                       { $1 }

COLORVAL :: { Token }
COLORVAL   : red                                        { $1 }
           | blue                                       { $1 }
           | mangenta                                   { $1 }
           | cyan                                       { $1 }
           | green                                      { $1 }
           | yellow                                     { $1 }

GOALTEST :: { AST.GOALTEST }
GOALTEST   : willy is at Int Int                        { AST.WILLYISAT (tokenPos $1) $4 $5 }
           | Int Id objects in Basket                   { AST.OBJECTSIN (tokenPos $1) $1 $2 }
           | Int Id objects at Int Int                  { AST.OBJECTSAT (tokenPos $1) $1 $2 $5 $6 }

FINALGOAL :: { AST.FINALGOAL } 
FINALGOAL  : FINALGOAL and FINALGOAL1                   { AST.FGAND (tokenPos $2) $1 $3 }
           | FINALGOAL or FINALGOAL1                    { AST.FGOR (tokenPos $2) $1 $3 }
           | FINALGOAL1                                 { $1 }

FINALGOAL1 :: { AST.FINALGOAL } 
FINALGOAL1 : not FINALGOAL                              { AST.FGNOT (tokenPos $1) $2 }
           | '(' FINALGOAL ')'                          { $2 }
           | Id                                         { AST.FGID (tokenPos $1) $1 }
           | true                                       { AST.FGTOF (tokenPos $1) $1 }
           | false                                      { AST.FGTOF (tokenPos $1) $1 }



TASK :: { AST.BLOCK }
TASK       : beginTask Id on Id TASKINSTRS endTask      { AST.TASK (tokenPos $1) $2 $4 (reverse $5) }
           | beginTask Id on Id endTask                 { AST.TASK (tokenPos $1) $2 $4 [] }
           | beginTask Id on Id SC endTask              { AST.TASK (tokenPos $1) $2 $4 [] }
           | beginTask Id on Id SC TASKINSTRS endTask   { AST.TASK (tokenPos $1) $2 $4 (reverse $6) }


TASKINSTRS :: { [AST.TASKINSTR] }
TASKINSTRS : TASKINSTRS TASKINSTR                       { $2:$1 }
           | TASKINSTRS ';'                             { $1 }
           | TASKINSTR                                  { [$1] }

TASKINSTR :: { AST.TASKINSTR }
TASKINSTR : if BOOLTEST then TASKINSTR ';'              { AST.IF (tokenPos $1) $2 $4 }
          | if BOOLTEST then TASKINSTR else TASKINSTR';'{ AST.IFELSE (tokenPos $1) $2 $4 $6 }
          | repeat Int times TASKINSTR                  { AST.REPEAT (tokenPos $1) $2 $4 }
          | while BOOLTEST do TASKINSTR                 { AST.WHILE (tokenPos $1) $2 $4 }
          | begin TASKINSTRS end                        { AST.BEGIN (tokenPos $1) (reverse $2) }
          | define Id as TASKINSTR                      { AST.DEFINE (tokenPos $1) $2 $4 }
          | move ';'                                    { AST.MOVE (tokenPos $1) }
          | turnLeft ';'                                { AST.TURNLEFT (tokenPos $1) }
          | turnRight ';'                               { AST.TURNRIGHT (tokenPos $1) }
          | pick Id ';'                                 { AST.PICK (tokenPos $1) $2 }
          | drop Id ';'                                 { AST.DROP (tokenPos $1) $2 }
          | set Id ';'                                  { AST.SET (tokenPos $1) $2 }
          | set Id to true';'                           { AST.SETTO (tokenPos $1) $2 $4 }
          | set Id to false';'                          { AST.SETTO (tokenPos $1) $2 $4 }
          | clear Id ';'                                { AST.CLEAR (tokenPos $1) $2 }
          | flip Id ';'                                 { AST.FLIP (tokenPos $1) $2 }
          | terminate ';'                               { AST.TERMINATE (tokenPos $1) }
          | Id ';'                                      { AST.INSTRID (tokenPos $1) $1 }

BOOLTEST :: { AST.TEST }
BOOLTEST  : BOOLTEST1                                   { $1 }
          | BOOLTEST and BOOLTEST1                      { AST.TESTAND (tokenPos $2) $1 $3 }
          | BOOLTEST or BOOLTEST1                       { AST.TESTOR (tokenPos $2) $1 $3 }
    
BOOLTEST1 :: { AST.TEST }
BOOLTEST1 : not BOOLTEST                                { AST.TESTNOT (tokenPos $1) $2 }
          | '(' BOOLTEST ')'                            { $2 }
          | Id                                          { AST.TESTID (tokenPos $1) $1 }
          | frontClear                                  { AST.FRONTCLEAR (tokenPos $1) }
          | leftClear                                   { AST.LEFTCLEAR (tokenPos $1) }
          | rightClear                                  { AST.RIGHTCLEAR (tokenPos $1) }
          | lookingNorth                                { AST.LOOKNORTH (tokenPos $1) }
          | lookingEast                                 { AST.LOOKEAST (tokenPos $1) }
          | lookingSouth                                { AST.LOOKSOUTH (tokenPos $1) }
          | lookingWest                                 { AST.LOOKWEST (tokenPos $1) }
          | found '(' Id ')'                            { AST.FOUND (tokenPos $1) $3 }
          | carrying '(' Id ')'                         { AST.CARRYING (tokenPos $1) $3 }
          | true                                        { AST.TESTTOF (tokenPos $1) $1 }
          | false                                       { AST.TESTTOF (tokenPos $1) $1 }

SC: SC ';'                              { $1 }
          | ';'                                         { [] }
-- Program



{

-- Error function
parseError :: [Token] -> a
parseError []     = error "Error al final del archivo"
parseError (tk:tks) = error $ "Error en la linea " ++ (getLine' tk) ++ " y colummna " ++ (getColumn' tk)
    where getLine' tok   = show $ fst $ tokenPos tok 
          getColumn' tok = show $ snd $ tokenPos tok   	

}