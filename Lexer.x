{
module Lexer where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]

tokens :-
    -- spaces
<0>    [$white # \n]+             { skip }

   -- comments
<0>    \-\-.*                      { skip }

    -- end line
<0>    \n                         { pushTK TKendLine }

    -- reserved words
<0>    begin\-world                { pushTK TKbeginWorld }
<0>    end\-world                  { pushTK TKendWorld }
<0>    World                       { pushTK TKWorld }
<0>    wall                        { pushTK TKwall }
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
<0>    capacity                    { pushTK TKcapacity }
<0>    Boolean                     { pushTK TKBoolean }
<0>    true                        { pushTK TKtrue }
<0>    false                       { pushTK TKfalse }
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
<0>    begin\-work                 { pushTK TKbeginWork }
<0>    on                          { pushTK TKon }
<0>    end\-work                   { pushTK TKendWork }
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

   -- numbers and identifiers
<0>    $digit+                     { pushInt }
<0>    [$alpha \_][$alphaNum \_]*  { pushId }

    -- symbols
<0>    \(                          { pushTK TKopenBracket }
<0>    \)                          { pushTK TKcloseBracket }
<0>    \;                          { pushTK TKsemicolon }

    -- block comment
<0>    \{\{                        { andBegin skip blockComment }
<blockComment>   \{\{              { pushError }
<blockComment>   \}\}\n            { andBegin skip 0 }
<blockComment>   \}\}              { andBegin skip 0 }
<blockComment>   [.\n]             { skip }

    -- error
<0>    \}\}                        { pushError }
<0>    .                           { pushError }

{

data Token =
    TKendLine {tokenPos :: (Int,Int) }                   |

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
    TKcapacity {tokenPos :: (Int,Int) }                  |
    TKBoolean {tokenPos :: (Int,Int) }                   |
    TKtrue {tokenPos :: (Int,Int) }                      |
    TKfalse {tokenPos :: (Int,Int) }                     |
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
    TKbeginWork {tokenPos :: (Int,Int) }                 |
    TKon {tokenPos :: (Int,Int) }                        |
    TKendWork {tokenPos :: (Int,Int) }                   |
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

    TKInt {tokenPos :: (Int,Int) , getValue :: Int }     |
    TKId {tokenPos :: (Int,Int) , getStr :: String }     |

    TKopenBracket {tokenPos :: (Int,Int) }               |
    TKcloseBracket {tokenPos :: (Int,Int) }              |
    TKsemicolon {tokenPos :: (Int,Int) }                 |

    TKerror {tokenPos :: (Int,Int) , getChar :: Char}    |
    TKcommentEOFError                                    |
    TKEOF
    deriving(Eq,Show)

alexEOF :: Alex Token
alexEOF = return TKEOF

pushTK :: ((Int, Int) -> Token) -> AlexInput -> Int -> Alex Token
pushTK tok ( (AlexPn _ l c ) , _ , _ , _ ) len = return ( tok (l,c) )

pushInt :: AlexInput -> Int -> Alex Token
pushInt ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKInt (l,c) ( read $ take len str ) )

pushId :: AlexInput -> Int -> Alex Token
pushId ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKId (l,c) ( take len str) )


pushError :: AlexInput -> Int -> Alex Token
pushError ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKerror (l,c) ( head str ) )

runAlexScan s = runAlex s $ alexMonadScan   

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
                return ([tok] ++ toks)
    in runAlex str loop  
}