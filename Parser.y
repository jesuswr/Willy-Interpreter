{
module Parser where

import Lexer
}

%name parse 
%tokentype { Token }
%error { parseError }

%token 
    -- Reserverd words
    beginWorld               { TKbeginWorld _ }
    endWorld                 { TKendWorld _ }
    World                    { TKWorld _ }
    wall                     { TKwall _ }
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
    '{'                      { TKopenBracket _ }
    '}'                      { TKcloseBracket _ }
    ';'                      { TKsemicolon _ }

-- Precedence list

-- TO DO

%% -- Grammar

-- Program



{

-- Error function
parseError :: [Token] -> a
parseError []     = error "Error al final del archivo"
parseError tk:tks = error % "Error en la linea " ++ getLine' tk ++ " y colummna " ++ getColumn' tk
    where getLine' tok   = show $ fst $ tokenPos tok 
          getColumn' tok = show $ snd $ tokenPos tok   	

}