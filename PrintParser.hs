module PrintParser where

import Control.Monad.State
import AST
import Lexer

data PrintState = PrintState{ output :: [String] , blockN :: Int } deriving(Show)

type MyPrintStateM a = StateT PrintState IO a


printParser :: [BLOCK] -> MyPrintStateM ()
printParser [] = do 
    (PrintState str int ) <- get
    io $ putStr (unlines $ reverse str) 
    return ()
printParser (x:xs) = do
    printWorld x
    printParser xs

incBlockN :: MyPrintStateM ()
incBlockN = do
    (PrintState str int ) <- get
    put (PrintState str (int+1) )
    return ()


printWorld :: BLOCK -> MyPrintStateM ()
printWorld (WORLD _ _ _) = do
    incBlockN
    return ()
printWorld (TASK (x,y) taskId onWorld taskInstrs) = do
    incBlockN
    (PrintState str int ) <- get
    put (PrintState ((bNum++show(int)):wId:tId:str) int )
    printInstrBlock 4 taskInstrs
    where
        tId  = (getStr taskId) ++ ":"
        wId  = "  mundo: " ++ (getStr onWorld)
        bNum = "    identificador de bloque: "


printInstrBlock :: Int -> [TASKINSTR] -> MyPrintStateM ()
printInstrBlock spaces [] = return ()
printInstrBlock spaces (x:xs) = do
    printInstr spaces x
    printInstrBlock spaces xs


printInstr :: Int -> TASKINSTR -> MyPrintStateM ()
printInstr spaces (IF (x,y) test tInst) = do
    (PrintState str int) <- get
    put (PrintState (cd:cond:str) int)
    printGuard (spaces+4) test
    (PrintState str' int') <- get
    put (PrintState (inst:str') int')
    where
        cond = replicate spaces ' ' ++ "CONDICIONAL IF:"
        cd   = replicate (spaces+2) ' ' ++ "condicion:"
        inst = replicate (spaces+2) ' ' ++ "instruccion:"

printInstr spaces (IFELSE (x,y) test tInst0 tInst1) = do
    (PrintState str int) <- get
    put (PrintState (cd:cond:str) int)
    printGuard (spaces+4) test
    (PrintState str' int') <- get
    put (PrintState (inst:str') int')
    printInstr (spaces+4) tInst0
    (PrintState str'' int'') <- get
    put (PrintState (inst2:str'') int'')
    printInstr (spaces+4) tInst1
    where
        cond  = replicate spaces ' ' ++ "CONDICIONAL IF/ELSE:"
        cd    = replicate (spaces+2) ' ' ++ "condicion:"
        inst  = replicate (spaces+2) ' ' ++ "instruccion if :"
        inst2 = replicate (spaces+2) ' ' ++ "instruccion else :"

printInstr spaces (REPEAT (x,y) n tInst) = do
    (PrintState str int) <- get
    put (PrintState (it:nt:nv:cr:str) int)
    printInstr (spaces+4) tInst
    where
        cr = replicate spaces ' ' ++ "CICLO REPEAT:"
        nv = replicate (spaces+2) ' ' ++ "numero de ciclos:"
        nt = replicate (spaces+4) ' ' ++ (show $ getValue n)
        it = replicate (spaces+2) ' ' ++ "instruccion:"

printInstr spaces (WHILE (x,y) test tInst) = do
    (PrintState str int) <- get
    put(PrintState (gc:cw:str) int)
    printGuard (spaces+4) test
    (PrintState str' int') <- get
    put(PrintState (it:str') int')
    printInstr (spaces+4) tInst
    where 
        cw = replicate spaces ' ' ++ "CICLO WHILE:"
        gc = replicate (spaces+2) ' ' ++ "condicion:"
        it = replicate (spaces+2) ' ' ++ "instruccion:"

printInstr spaces (BEGIN (x,y) tInsts) = do
    (PrintState str int) <- get
    put(PrintState (is:(ib++(show (int+1))):bi:str) int)
    incBlockN
    printInstrBlock (spaces+4) tInsts
    where
        bi = replicate spaces ' ' ++ "BLOQUE BEGIN: "
        ib = replicate (spaces+2) ' ' ++ "identificador de bloque: "
        is = replicate (spaces+2) ' ' ++ "instrucciones:"

printInstr spaces (MOVE (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "move"

printInstr spaces (TURNLEFT (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "turnleft"

printInstr spaces (TURNRIGHT (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "turnright"

printInstr spaces (PICK (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "pick"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)      

printInstr spaces (DROP (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)   
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "drop" 
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (SET (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "set"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (SETTO (x,y) id tof) = do
    (PrintState str int) <- get
    put(PrintState (tf:vn:is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "set"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)
        vn = replicate (spaces+4) ' ' ++ "valor nuevo:"
        tf = replicate (spaces+6) ' ' ++  (show tof)

printInstr spaces (FLIP (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "flip"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (CLEAR (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "clear"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (TERMINATE (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "terminate"

printInstr spaces (INSTRID (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (st:li:str) int)
    where
        li = replicate spaces ' ' ++ "LLAMADA A INSTRUCCION:"
        st = replicate (spaces+2) ' ' ++ (getStr id)


printGuard :: Int -> TEST -> MyPrintStateM ()
printGuard spaces (TESTTOF (x,y) val) = do
    (PrintState str int) <- get
    put(PrintState (tof:str) int)
    where 
        tof = replicate spaces ' ' ++ show val
printGuard spaces (TESTAND (x,y) test0 test1) = do
    (PrintState str int) <- get
    put(PrintState (li:disy:str) int)
    printGuard (spaces+4) test0
    (PrintState str' int') <- get
    put(PrintState (ld:str') int')
    printGuard (spaces+4) test1
    where
        disy = replicate spaces ' ' ++ "DISYUNCION:"
        li = replicate (spaces+2) ' ' ++ "lado izquiedo:"
        ld = replicate (spaces+2) ' ' ++ "lado derecho:"

printGuard spaces (TESTOR (x,y) test0 test1) = do
    (PrintState str int) <- get
    put(PrintState (li:conj:str) int)
    printGuard (spaces+4) test0
    (PrintState str' int') <- get
    put(PrintState (ld:str') int')
    printGuard (spaces+4) test1
    where
        conj = replicate spaces ' ' ++ "CONJUNCION:"
        li = replicate (spaces+2) ' ' ++ "lado izquiedo:"
        ld = replicate (spaces+2) ' ' ++ "lado derecho:"

printGuard spaces (TESTNOT (x,y) test) = do
    (PrintState str int) <- get
    put(PrintState (exp:neg:str) int)
    printGuard (spaces+4) test
    where
        neg = replicate spaces ' ' ++ "NEGACION:"
        exp = replicate (spaces+2) ' ' ++ "expresion:"

printGuard spaces (TESTID (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (idStr:ident:str) int)
    where
        ident = replicate spaces ' ' ++ "IDENTIFICADOR: "
        idStr = replicate (spaces+2) ' ' ++ (getStr id)

printGuard spaces (FRONTCLEAR (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "front-clear"

printGuard spaces (LEFTCLEAR (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "left-clear"
        
printGuard spaces (RIGHTCLEAR (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "right-clear"

printGuard spaces (LOOKNORTH (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-north"

printGuard spaces (LOOKEAST (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-east"

printGuard spaces (LOOKSOUTH (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-south"

printGuard spaces (LOOKWEST (x,y) ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-west"

printGuard spaces (FOUND (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (io:ob:f:eb:str) int)
    where
        eb = replicate spaces ' ' ++ "EXPRESION BOOLEANA:"
        f  = replicate (spaces+2) ' ' ++ "found"
        ob = replicate (spaces+4) ' ' ++ "identificador objeto:"
        io = replicate (spaces+6) ' ' ++ (getStr id)

printGuard spaces (CARRYING (x,y) id) = do
    (PrintState str int) <- get
    put(PrintState (io:ob:f:eb:str) int)
    where
        eb = replicate spaces ' ' ++ "EXPRESION BOOLEANA:"
        f  = replicate (spaces+2) ' ' ++ "carrying"
        ob = replicate (spaces+4) ' ' ++ "identificador objeto:"
        io = replicate (spaces+6) ' ' ++ (getStr id)


io :: IO a -> MyPrintStateM a
io = liftIO