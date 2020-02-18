
%% -- Grammar

BLOCKS		: BLOCKS WORLD								{ BLOCKS $2 }
			| BLOCKS TASK								{ BLOCKS $2 }
			| WORLD										{ $1 }
			| TASK										{ $1 }

WORLD		: beginWorld Id INSTRS endWorld				{ WORLD $2 $3 }

INSTRS 		: {- empty -}								{ INSTRS [] }
			| INSTRS INSTR 								{ INSTRS $ $2 : $1 }
			| INSTRS ';' 								{ INSTRS $1 }

INSTR 		: World Int Int ';'							{  }
			| Wall Int from Int Int to Int Int ';'		{  }		
			| ObjectType Id of color red ';'			{  }
			| ObjectType Id of color blue ';'			{  }
			| ObjectType Id of color magenta ';'		{  }	
			| ObjectType Id of color cyan ';'			{  }
			| ObjectType Id of color green ';'			{  }
			| ObjectType Id of color yellow ';'			{  }
			| Place Int of Id at Int Int ';' 			{  }
			| Place Int of Id in basket ';'				{  }
			| Start at Int Int heading north ';'		{  }
			| Start at Int Int heading east ';'			{  }
			| Start at Int Int heading south ';'		{  }
			| Start at Int Int heading west ';'			{  }
			| Basket of capacity Int ';'				{  }
			| Boolean Id with initial value true ';'	{  }
			| Boolean Id with initial value false ';'	{  }
			| Goal Id is GOALTEST ';'					{  }
			| Final goal is FINALGOAL ';'				{  }


GOALTEST 	: willy is at Int Int 						{  }
		 	| Int Id objects in Basket 					{  }
		 	| Int Id objects at Int Int 				{  }

 
FINALGOAL 	: FINALGOAL and FINALGOAL1 					{  }
		  	| FINALGOAL or FINALGOAL1 					{  }
		  	| FINALGOAL1 								{  }

FINALGOAL1 	: not FINALGOAL 							{  }
		   	| '(' FINALGOAL ')' 						{  }
		   	| Id 										{  }

TASK		: beginTask Id on Id TINSTRS endTask 		{  }

TINSTRS		: {- empty -} 								{  }
			| TINSTRS TINSTR 							{  }
			| TINSTRS ';' 								{  }

TINSTR 		: if BOOLTEST then TINSTR 					{  }
			| if BOOLTEST then TINSTR else TINSTR 		{  }
			| repeat Int times TINSTR 					{  }
			| while BOOLTEST do TINSTR 					{  }
			| begin TINSTRS end 						{  }
			| define Id as TINSTR 						{  }
			| move ';' 									{  }
			| turnLeft ';' 								{  }
			| turnRight ';' 							{  }
			| pick Id ';' 								{  }
			| drop Id ';' 								{  }
			| set Id ';' 								{  }
			| set Id to true';' 						{  }
			| set Id to false';' 						{  }
			| clear Id ';' 								{  }
			| flip Id ';' 								{  }
			| terminate ';' 							{  }

BOOLTEST 	: BOOLTEST1									{  }
		 	| BOOLTEST and BOOLTEST1					{  }
		 	| BOOLTEST or BOOLTEST1						{  }
		

BOOLTEST1 	: not BOOLTEST 								{  }
		 	| '(' BOOLTEST ')' 							{  }
		 	| Id 										{  }
		 	| frontClear 								{  }
		 	| leftClear 								{  }
		 	| rightClear 								{  }
		 	| lookingNorth 								{  }
		 	| lookingEast 								{  }
		 	| lookingSouth 								{  }
		 	| lookingWest 								{  }
		 	| found '(' Id ')' 							{  }
		 	| carrying '(' Id ')' 						{  }



Exp  : let var '=' Exp in Exp	{ Let $2 $4 $6 }
	 | Exp1						{ Exp1 $1}

Exp1 : Exp1 '+' Term			{ Plus $1 $3 }
	 | Exp1 '-' Term			{ Minus $1 $3}
	 | Term						{ Term $1 }

Term : Term '*' Factor			{ Times $1 $3 }
	 | Term '/' Factor			{ Div $1 $3 }
	 | Factor					{ Factor $1 }

Factor
	 : int 						{ Int $1 }
	 | var 						{ Var $1 }
	 | '(' Exp ')'				{ Brack $2 }
