{
module Lexer where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]


tokens :-
    -- spaces
<0>    [$white ]+              { skip }

   -- comments
<0>    \-\-.*                      { skip }

    -- reserved words
<0>    begin\-world                { pushTK TKbeginWorld }
<0>    end\-world                  { pushTK TKendWorld }
<0>    World                       { pushTK TKWorld }
<0>    Wall                        { pushTK TKwall }
<0>    from                        { pushTK TKfrom }
<0>    to                          { pushTK TKto }
<0>    Object\-type                { pushTK TKObjectType }
<0>    of                          { pushTK TKof }
<0>    color                       { pushTK TKcolor }
<0>    red                         { pushTK TKred }
<0>    blue                        { pushTK TKblue }
<0>    mangenta                    { pushTK TKmangenta }
<0>    cyan                        { pushTK TKcyan }
<0>    green                       { pushTK TKgreen }
<0>    yellow                      { pushTK TKyellow }
<0>    Place                       { pushTK TKPlace }
<0>    at                          { pushTK TKat }
<0>    basket                      { pushTK TKbasket }
<0>    in                          { pushTK TKin }
<0>    Start                       { pushTK TKStart }
<0>    heading                     { pushTK TKheading }
<0>    Basket                      { pushTK TKBasket }
<0>    capacity                    { pushTK TKcapacity }
<0>    with                        { pushTK TKwith }
<0>    initial                     { pushTK TKinitial }
<0>    value                       { pushTK TKvalue }
<0>    Goal                        { pushTK TKGoal }
<0>    is                          { pushTK TKis }
<0>    Final                       { pushTK TKFinal }
<0>    goal                        { pushTK TKgoal }
<0>    willy                       { pushTK TKwilly }
<0>    objects                     { pushTK TKobjects }
<0>    and                         { pushTK TKand }
<0>    or                          { pushTK TKor }
<0>    not                         { pushTK TKnot }
<0>    begin\-task                 { pushTK TKbeginTask }
<0>    on                          { pushTK TKon }
<0>    end\-task                   { pushTK TKendTask }
<0>    if                          { pushTK TKif }
<0>    else                        { pushTK TKelse }
<0>    then                        { pushTK TKthen }
<0>    repeat                      { pushTK TKrepeat }
<0>    times                       { pushTK TKtimes }
<0>    while                       { pushTK TKwhile }
<0>    do                          { pushTK TKdo }
<0>    begin                       { pushTK TKbegin }
<0>    end                         { pushTK TKend }
<0>    define                      { pushTK TKdefine }
<0>    as                          { pushTK TKas }
<0>    move                        { pushTK TKmove }
<0>    turn\-right                 { pushTK TKturnRight }
<0>    turn\-left                  { pushTK TKturnLeft }
<0>    pick                        { pushTK TKpick }
<0>    drop                        { pushTK TKdrop }
<0>    set                         { pushTK TKset }
<0>    clear                       { pushTK TKclear }
<0>    flip                        { pushTK TKflip }
<0>    terminate                   { pushTK TKterminate }
<0>    found                       { pushTK TKfound }
<0>    carrying                    { pushTK TKcarrying }
<0>    front\-clear                { pushTK TKfrontClear }
<0>    left\-clear                 { pushTK TKleftClear }
<0>    right\-clear                { pushTK TKrightClear }
<0>    looking\-north              { pushTK TKlookingNorth }
<0>    looking\-east               { pushTK TKlookingEast }
<0>    looking\-south              { pushTK TKlookingSouth }
<0>    looking\-west               { pushTK TKlookingWest }
<0>    north                       { pushTK TKnorth }
<0>    east                        { pushTK TKeast }
<0>    south                       { pushTK TKsouth }
<0>    west                        { pushTK TKwest }

    -- Boolean
<0>    Boolean                     { pushTK TKBoolean }
<0>    true                        { pushTK TKtrue }
<0>    false                       { pushTK TKfalse }

    -- numbers
<0>    $digit+                     { pushInt }

    -- identifiers
<0>    $digit[$alphaNum \_]+       { pushError }
<0>    [$alpha \_][$alphaNum \_]*  { pushId }

    -- symbols
<0>    \(                          { pushTK TKopenBracket }
<0>    \)                          { pushTK TKcloseBracket }
<0>    \;                          { pushTK TKsemicolon }

    -- block comment
<0>    \{\{                        { andBegin skip blockComment } -- If {{ is found, move to 'blockComment' startCode
<blockComment>   \{\{              { pushError }
<blockComment>   \}\}              { andBegin skip 0 }            -- If }} is found, return to initial startCode
<blockComment>   .                 { skip }
<blockComment>   \n                { skip }

    -- error
<0>    \}\}                        { pushError }
<0>    .                           { pushError }

{

-- The Token type
data Token =
    TKbeginWorld {tokenPos :: (Int,Int) }                |
    TKendWorld {tokenPos :: (Int,Int) }                  |
    TKWorld {tokenPos :: (Int,Int) }                     |
    TKwall {tokenPos :: (Int,Int) }                      |
    TKfrom {tokenPos :: (Int,Int) }                      |
    TKto {tokenPos :: (Int,Int) }                        |
    TKObjectType {tokenPos :: (Int,Int) }                |
    TKof {tokenPos :: (Int,Int) }                        |
    TKcolor {tokenPos :: (Int,Int) }                     |
    TKred {tokenPos :: (Int,Int) }                       |
    TKblue {tokenPos :: (Int,Int) }                      |
    TKmangenta {tokenPos :: (Int,Int) }                  |
    TKcyan {tokenPos :: (Int,Int) }                      |
    TKgreen {tokenPos :: (Int,Int) }                     |
    TKyellow {tokenPos :: (Int,Int) }                    |
    TKPlace {tokenPos :: (Int,Int) }                     |
    TKat {tokenPos :: (Int,Int) }                        |
    TKbasket {tokenPos :: (Int,Int) }                    |
    TKin {tokenPos :: (Int,Int) }                        |
    TKStart {tokenPos :: (Int,Int) }                     |
    TKheading {tokenPos :: (Int,Int) }                   |
    TKBasket {tokenPos :: (Int,Int) }                    |
    TKcapacity {tokenPos :: (Int,Int) }                  |
    TKwith {tokenPos :: (Int,Int) }                      |
    TKinitial {tokenPos :: (Int,Int) }                   |
    TKvalue {tokenPos :: (Int,Int) }                     |
    TKGoal {tokenPos :: (Int,Int) }                      |
    TKis {tokenPos :: (Int,Int) }                        |
    TKFinal {tokenPos :: (Int,Int) }                     |
    TKgoal {tokenPos :: (Int,Int) }                      |
    TKwilly {tokenPos :: (Int,Int) }                     |
    TKobjects {tokenPos :: (Int,Int) }                   |
    TKand {tokenPos :: (Int,Int) }                       |
    TKor {tokenPos :: (Int,Int) }                        |
    TKnot {tokenPos :: (Int,Int) }                       |
    TKbeginTask {tokenPos :: (Int,Int) }                 |
    TKon {tokenPos :: (Int,Int) }                        |
    TKendTask {tokenPos :: (Int,Int) }                   |
    TKif {tokenPos :: (Int,Int) }                        |
    TKelse {tokenPos :: (Int,Int) }                      |
    TKthen {tokenPos :: (Int,Int) }                      |
    TKrepeat {tokenPos :: (Int,Int) }                    |
    TKtimes {tokenPos :: (Int,Int) }                     |
    TKwhile {tokenPos :: (Int,Int) }                     |
    TKdo {tokenPos :: (Int,Int) }                        |
    TKbegin {tokenPos :: (Int,Int) }                     |
    TKend {tokenPos :: (Int,Int) }                       |
    TKdefine {tokenPos :: (Int,Int) }                    |
    TKas {tokenPos :: (Int,Int) }                        |
    TKmove {tokenPos :: (Int,Int) }                      |
    TKturnRight {tokenPos :: (Int,Int) }                 |
    TKturnLeft {tokenPos :: (Int,Int) }                  |
    TKpick {tokenPos :: (Int,Int) }                      |
    TKdrop {tokenPos :: (Int,Int) }                      |
    TKset {tokenPos :: (Int,Int) }                       |
    TKclear {tokenPos :: (Int,Int) }                     |
    TKflip {tokenPos :: (Int,Int) }                      |
    TKterminate {tokenPos :: (Int,Int) }                 |
    TKfound {tokenPos :: (Int,Int) }                     |
    TKcarrying {tokenPos :: (Int,Int) }                  |
    TKfrontClear {tokenPos :: (Int,Int) }                |
    TKleftClear {tokenPos :: (Int,Int) }                 |
    TKrightClear {tokenPos :: (Int,Int) }                |
    TKlookingNorth {tokenPos :: (Int,Int) }              |
    TKlookingEast {tokenPos :: (Int,Int) }               |
    TKlookingSouth {tokenPos :: (Int,Int) }              |
    TKlookingWest {tokenPos :: (Int,Int) }               |
    TKnorth {tokenPos :: (Int,Int) }                     |
    TKsouth {tokenPos :: (Int,Int) }                     |
    TKeast {tokenPos :: (Int,Int) }                      |
    TKwest {tokenPos :: (Int,Int) }                      |

    TKBoolean {tokenPos :: (Int,Int) }                   |
    TKtrue {tokenPos :: (Int,Int) }                      |
    TKfalse {tokenPos :: (Int,Int) }                     |

    TKInt {tokenPos :: (Int,Int) , getValue :: Int }     |
    TKId {tokenPos :: (Int,Int) , getStr :: String }     |

    TKopenBracket {tokenPos :: (Int,Int) }               |
    TKcloseBracket {tokenPos :: (Int,Int) }              |
    TKsemicolon {tokenPos :: (Int,Int) }                 |

    TKerror {tokenPos :: (Int,Int) , getChar :: Char}    |
    TKcommentEOFError                                    |
    TKEOF
    deriving(Eq)

-- Here is defined how the Token type is an instance of Show typeclass
instance Show Token where
    show ( TKbeginWorld (l,c) )       = "TKbeginWorld(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKendWorld (l,c) )         = "TKendWorld(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKWorld (l,c) )            = "TKWorld(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKwall (l,c) )             = "TKwall(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKfrom (l,c) )             = "TKfrom(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKto (l,c) )               = "TKto(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKObjectType (l,c) )       = "TKObjectType(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKof (l,c) )               = "TKof(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKcolor (l,c) )            = "TKcolor(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKred (l,c) )              = "TKred(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKblue (l,c) )             = "TKblue(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKmangenta (l,c) )         = "TKmangenta(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKcyan (l,c) )             = "TKcyan(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKgreen (l,c) )            = "TKgreen(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKyellow (l,c) )           = "TKyellow(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKPlace (l,c) )            = "TKPlace(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKat (l,c) )               = "TKat(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKbasket (l,c) )           = "TKbasket(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKin (l,c) )               = "TKin(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKStart (l,c) )            = "TKStart(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKheading (l,c) )          = "TKheading(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKBasket (l,c) )           = "TKBasket(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKcapacity (l,c) )         = "TKcapacity(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKwith (l,c) )             = "TKwith(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKinitial (l,c) )          = "TKinitial(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKvalue (l,c) )            = "TKvalue(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKGoal (l,c) )             = "TKGoal(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKis (l,c) )               = "TKis(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKFinal (l,c) )            = "TKFinal(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKgoal (l,c) )             = "TKgoal(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKwilly (l,c) )            = "TKwilly(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKobjects (l,c) )          = "TKobjects(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKand (l,c) )              = "TKand(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKor (l,c) )               = "TKor(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKnot (l,c) )              = "TKnot(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKbeginTask (l,c) )        = "TKbeginTask(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKon (l,c) )               = "TKon(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKendTask (l,c) )          = "TKendTask(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKif (l,c) )               = "TKif(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKelse (l,c) )             = "TKelse(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKthen (l,c) )             = "TKthen(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKrepeat (l,c) )           = "TKrepeat(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKtimes (l,c) )            = "TKtimes(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKwhile (l,c) )            = "TKwhile(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKdo (l,c) )               = "TKdo(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKbegin (l,c) )            = "TKbegin(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKend (l,c) )              = "TKend(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKdefine (l,c) )           = "TKdefine(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKas (l,c) )               = "TKas(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKmove (l,c) )             = "TKmove(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKturnRight (l,c) )        = "TKturnRight(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKturnLeft (l,c) )         = "TKturnLeft(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKpick (l,c) )             = "TKpick(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKdrop (l,c) )             = "TKdrop(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKset (l,c) )              = "TKset(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKclear (l,c) )            = "TKclear(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKflip (l,c) )             = "TKflip(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKterminate (l,c) )        = "TKterminate(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKfound (l,c) )            = "TKfound(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKcarrying (l,c) )         = "TKcarrying(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKfrontClear (l,c) )       = "TKfrontClear(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKleftClear (l,c) )        = "TKleftClear(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKrightClear (l,c) )       = "TKrightClear(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKlookingNorth (l,c) )     = "TKlookingNorth(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKlookingEast (l,c) )      = "TKlookingEast(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKlookingSouth (l,c) )     = "TKlookingSouth(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKlookingWest (l,c) )      = "TKlookingWest(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKnorth (l,c) )            = "TKnorth(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKsouth (l,c) )            = "TKsouth(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKeast (l,c) )             = "TKeast(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKwest (l,c) )             = "TKwest(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "

    show ( TKBoolean (l,c) )          = "TKBoolean(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKtrue (l,c) )             = "TKtrue(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKfalse (l,c) )            = "TKfalse(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    
    show ( TKInt (l,c) num )          = "TKInt(" ++ show num ++ ", linea=" ++ show l ++", columna=" ++ show c ++") " 
    show ( TKId (l,c) str )           = "TKId(\"" ++ str ++ "\", linea=" ++ show l ++", columna=" ++ show c ++") "

    show ( TKopenBracket (l,c) )      = "TKopenBracket(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKcloseBracket (l,c) )     = "TKcloseBracket(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "
    show ( TKsemicolon (l,c) )        = "TKsemicolon(linea=" ++ show l ++ ", columna=" ++ show c ++ ") "

    show ( TKerror (l,c) char )       = "Caraceter ilegal " ++ show char ++ " encontrado en linea " ++ show l ++ ", columna " ++ show c ++ "."
    show ( TKcommentEOFError )        = "Error: se alcanzo el final del archivo antes de cerrar un comentario de bloque."


-- Definition needed by Alex
alexEOF :: Alex Token
alexEOF = return TKEOF

-- push Token functions
pushTK :: ((Int, Int) -> Token) -> AlexInput -> Int -> Alex Token
pushTK tok ( (AlexPn _ l c ) , _ , _ , _ ) len = return ( tok (l,c) )

pushInt :: AlexInput -> Int -> Alex Token
pushInt ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKInt (l,c) ( read $ take len str ) )

pushId :: AlexInput -> Int -> Alex Token
pushId ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKId (l,c) ( take len str) )

pushError :: AlexInput -> Int -> Alex Token
pushError ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKerror (l,c) ( head str ) )

-- Scanner 
scanner :: String -> Either String [Token]
scanner str = 
    let loop = do
        tok <- alexMonadScan
        startCode <- alexGetStartCode
        if (tok == TKEOF )
        then do
                if ( startCode == 0 )
                    then do 
                    return []
                else do
                    return [TKcommentEOFError]
        else do toks <- loop
                return (tok:toks)
    in  auxF( runAlex str loop )  

-- Function that helps returning the error message or the list of tokens
auxF :: ( Either String [Token] ) -> Either String [Token]
auxF (Left str) = Left str
auxF (Right toks)
       | length errorList > 0 = Left $ strError errorList  
       | otherwise  = Right toks
       where errorList = [x | x <- toks, isError x]

-- Function to make a string with all the errors in the token list
strError :: [Token] -> String
strError errorList = unlines $ map (show) errorList

-- Function to check if a given token represents an error
isError :: Token -> Bool
isError (TKerror _ _) = True
isError (TKcommentEOFError) = True
isError _ = False

}
