{
module Lexer
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]

tokens :-
	-- spaces
	[$white # \\n]*			;

	-- comments
	\-\-.*					;
	
	-- end line
	\\n 					{\(AlexPn _ l c) str -> TKendLine}

	-- reserved words
	begin\-world				{\(AlexPn _ l c) str -> TKbeginWorld l c}
	end\-world					{\(AlexPn _ l c) str -> TKendWorld l c}
	World 						{\(AlexPn _ l c) str -> TKWorld l c}
	wall						{\(AlexPn _ l c) str -> TKwall l c}
	from						{\(AlexPn _ l c) str -> TKfrom l c}
	to 							{\(AlexPn _ l c) str -> TKto l c}
	Object\-type				{\(AlexPn _ l c) str -> TkObjectType l c}
	of							{\(AlexPn _ l c) str -> TKof l c}
	color						{\(AlexPn _ l c) str -> TKcolor l c}
	red 						{\(AlexPn _ l c) str -> TKred l c}
	blue						{\(AlexPn _ l c) str -> TKblue l c}
	mangenta					{\(AlexPn _ l c) str -> TKmangenta l c}
	cyan						{\(AlexPn _ l c) str -> TKcyan l c}
	green						{\(AlexPn _ l c) str -> TKgreen l c}
	yellow						{\(AlexPn _ l c) str -> TKyellow l c}
	Place						{\(AlexPn _ l c) str -> TKPlace l c}
	at							{\(AlexPn _ l c) str -> TKat l c}
	basket						{\(AlexPn _ l c) str -> TKbasket l c}
	in							{\(AlexPn _ l c) str -> TKin l c}
	Start						{\(AlexPn _ l c) str -> TKStart l c}
	heading						{\(AlexPn _ l c) str -> TKheading l c}
	capacity					{\(AlexPn _ l c) str -> TKcapacity l c}
	Boolean						{\(AlexPn _ l c) str -> TKBoolean l c}
	true						{\(AlexPn _ l c) str -> TKtrue l c}
	false						{\(AlexPn _ l c) str -> TKfalse l c}
	with						{\(AlexPn _ l c) str -> TKwith l c}
	initial						{\(AlexPn _ l c) str -> TKinitial l c}
	value						{\(AlexPn _ l c) str -> TKvalue l c}
	Goal						{\(AlexPn _ l c) str -> TKGoal l c}
	is							{\(AlexPn _ l c) str -> TKis l c}
	Final						{\(AlexPn _ l c) str -> TKFinal l c}
	goal 						{\(AlexPn _ l c) str -> TKgoal l c}
	willy						{\(AlexPn _ l c) str -> TKwilly l c}
	objects						{\(AlexPn _ l c) str -> TKobjects l c}
	and							{\(AlexPn _ l c) str -> TKand l c}
	or							{\(AlexPn _ l c) str -> TKor l c}
	not							{\(AlexPn _ l c) str -> TKnot l c}
	begin\-work					{\(AlexPn _ l c) str -> TKbeginWork l c}
	on							{\(AlexPn _ l c) str -> TKon l c}
	end\-work					{\(AlexPn _ l c) str -> TKendWork l c}
	if							{\(AlexPn _ l c) str -> TKif l c}
	else 						{\(AlexPn _ l c) str -> TKelse l c}
	then 						{\(AlexPn _ l c) str -> TKthen l c}
	repeat						{\(AlexPn _ l c) str -> TKrepeat l c}
	times						{\(AlexPn _ l c) str -> TKtimes l c}
	while						{\(AlexPn _ l c) str -> TKwhile l c}
	do							{\(AlexPn _ l c) str -> TKdo l c}
	begin 						{\(AlexPn _ l c) str -> TKbegin l c}
	end 						{\(AlexPn _ l c) str -> TKend l c}
	define						{\(AlexPn _ l c) str -> TKdefine l c}
	as							{\(AlexPn _ l c) str -> TKas l c}
	move						{\(AlexPn _ l c) str -> TKmove l c}
	turn\-right					{\(AlexPn _ l c) str -> TKturnRight l c}
	turn\-left					{\(AlexPn _ l c) str -> TKturnLeft l c}
	pick						{\(AlexPn _ l c) str -> TKpick l c}
	drop						{\(AlexPn _ l c) str -> TKdrop l c}
	set							{\(AlexPn _ l c) str -> TKset l c}
	clear						{\(AlexPn _ l c) str -> TKclear l c}
	flip						{\(AlexPn _ l c) str -> TKflip l c}
	terminate					{\(AlexPn _ l c) str -> TKterminate l c}
	found						{\(AlexPn _ l c) str -> TKfound l c}
	carrying					{\(AlexPn _ l c) str -> TKcarrying l c}
	front\-clear				{\(AlexPn _ l c) str -> TKfrontClear l c}
	left\-clear					{\(AlexPn _ l c) str -> TKleftClear l c}
	right\-clear				{\(AlexPn _ l c) str -> TKrightClear l c}
	looking\-north				{\(AlexPn _ l c) str -> TKlookingNorth l c}
	looking\-east				{\(AlexPn _ l c) str -> TKlookingEast l c}
	looking\-south				{\(AlexPn _ l c) str -> TKlookingSouth l c}
	looking\-west				{\(AlexPn _ l c) str -> TKlookingWest l c}

	-- numbers and identifiers
	$digit+						{\(AlexPn _ l c) str -> TKInt (read str) l c}
	$alphaNum[$alphaNum \_]*	{\(AlexPn _ l c) str -> TKId str l c}

	-- symbols
	\(							{\(AlexPn _ l c) str -> TKopenBracket l c}
	\)							{\(AlexPn _ l c) str -> TKcloseBracket l c}
	\;							{\(AlexPn _ l c) str -> TKsemicolon l c}

	-- error
	.							{\(AlexPn _ l c) str -> TKerror (head str) l c}

{

data token =
	TKendLine int int 		|

	TKbeginWorld Int Int 	|
	TKendWorld Int Int 		|
	TKWorld Int Int 		|
	TKwall Int Int 			|
	TKfrom Int Int 			|
	TKto Int Int 			|
	TkObjectType Int Int 	|
	TKof Int Int 			|
	TKcolor Int Int 		|
	TKred Int Int 			|
	TKblue Int Int 			|
	TKmangenta Int Int 		|
	TKcyan Int Int 			|
	TKgreen Int Int 		|
	TKyellow Int Int 		|
	TKPlace Int Int 		|
	TKat Int Int 			|
	TKbasket Int Int 		|
	TKin Int Int 			|
	TKStart Int Int 		|
	TKheading Int Int 		|
	TKcapacity Int Int 		|
	TKBoolean Int Int 		|
	TKtrue Int Int 			|
	TKfalse Int Int 		|
	TKwith Int Int 			|
	TKinitial Int Int 		|
	TKvalue Int Int 		|
	TKGoal Int Int 			|
	TKis Int Int 			|
	TKFinal Int Int 		|
	TKgoal Int Int 			|
	TKwilly Int Int 		|
	TKobjects Int Int 		|
	TKand Int Int 			|
	TKor Int Int 			|
	TKnot Int Int 			|
	TKbeginWork Int Int 	|
	TKon Int Int 			|
	TKendWork Int Int 		|
	TKif Int Int 			|
	TKelse Int Int 			|
	TKthen Int Int 			|
	TKrepeat Int Int 		|
	TKtimes Int Int 		|
	TKwhile Int Int 		|
	TKdo Int Int 			|
	TKbegin Int Int 		|
	TKend Int Int 			|
	TKdefine Int Int 		|
	TKas Int Int 			|
	TKmove Int Int 			|
	TKturnRight Int Int 	|
	TKturnLeft Int Int 		|
	TKpick Int Int 			|
	TKdrop Int Int 			|
	TKset Int Int 			|
	TKclear Int Int 		|
	TKflip Int Int 			|
	TKterminate Int Int 	|
	TKfound Int Int 		|
	TKcarrying Int Int 		|
	TKfrontClear Int Int 	|
	TKleftClear Int Int 	|
	TKrightClear Int Int 	|
	TKlookingNorth Int Int 	|
	TKlookingEast Int Int 	|
	TKlookingSouth Int Int 	|
	TKlookingWest Int Int 	|

	TKInt Int Int Int 		|
	TKId String Int Int 	|

	TKopenBracket Int Int 	|
	TKcloseBracket Int Int 	|
	TKsemicolon Int Int 	|

	Tkerror Char Int Int 


	
}