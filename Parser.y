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
%left not
%left and or 

-- TO DO

%% -- Grammar

BLOCK :: { [AST.BLOCK] }
BLOCK      : BLOCK WORLD                              { $2:$1 }
           | WORLD                                    { [$1] }

WORLD :: { AST.BLOCK }
WORLD      : beginWorld Id INSTRS endWorld            { AST.WORLD $1 $2 (reverse $3) }
           | beginWorld Id endWorld                   { AST.WORLD $1 $2 [] }

INSTRS :: { [AST.INSTR] }
INSTRS     : INSTRS INSTR                             { $2:$1 }
           | INSTRS ';'                               { $1 }
           | INSTR                                    { [$1] }

INSTR :: { AST.INSTR }
INSTR      : World Int Int ';'                        { AST.WORLDSIZE $1 $2 $3 }
           | Wall DIRECTION from Int Int to Int Int ';'     { AST.WALL $1 $2 $4 $5 $7 $8  }        
           | ObjectType Id of color COLORVAL ';'      { AST.OBJECTTYPE $1 $2 $5 }
           | Place Int of Id at Int Int ';'           { AST.PLACEAT $1 $2 $4 $6 $7 }
           | Place Int of Id in basket ';'            { AST.PLACEIN $1 $2 $4 }
           | Start at Int Int heading DIRECTION ';'   { AST.STARTAT $1 $3 $4 $6 }
           | Basket of capacity Int ';'               { AST.BASKET $1 $4 }
           | Boolean Id with initial value true ';'   { AST.BOOLEAN $1 $2 $6 }
           | Boolean Id with initial value false ';'  { AST.BOOLEAN $1 $2 $6 }
           | Goal Id is GOALTEST ';'                  { AST.GOALIS $1 $2 $4 }
           | Final goal is FINALGOAL ';'              { AST.FINALIS $1 $4 }

DIRECTION :: { Token }
DIRECTION  : north                                    { $1 }
           | east                                     { $1 }
           | south                                     { $1 }
           | west                                     { $1 }

COLORVAL :: { Token }
COLORVAL   : red                                      { $1 }
           | blue                                     { $1 }
           | mangenta                                 { $1 }
           | cyan                                     { $1 }
           | green                                    { $1 }
           | yellow                                   { $1 }

GOALTEST :: { AST.GOALTEST }
GOALTEST   : willy is at Int Int                      { AST.WILLYISAT $1 $4 $5 }
           | Int Id objects in Basket                 { AST.OBJECTSIN $1 $1 $2 }
           | Int Id objects at Int Int                { AST.OBJECTSAT $1 $1 $2 $5 $6 }

FINALGOAL :: { AST.FINALGOAL } 
FINALGOAL  : FINALGOAL and FINALGOAL1                 { AST.FGAND $2 $1 $3 }
           | FINALGOAL or FINALGOAL1                  { AST.FGOR $2 $1 $3 }
           | FINALGOAL1                               { $1 }

FINALGOAL1 :: { AST.FINALGOAL } 
FINALGOAL1 : not FINALGOAL                            { AST.FGNOT $1 $2 }
           | '(' FINALGOAL ')'                        { $2 }
           | Id                                       { AST.FGID $1 $1 }

-- Program



{

-- Error function
parseError :: [Token] -> a
parseError []     = error "Error al final del archivo"
parseError (tk:tks) = error $ "Error en la linea " ++ (getLine' tk) ++ " y colummna " ++ (getColumn' tk)
    where getLine' tok   = show $ fst $ tokenPos tok 
          getColumn' tok = show $ snd $ tokenPos tok   	

}