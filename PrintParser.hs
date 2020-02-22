module PrintParser where

import Control.Monad.State
import AST
import Lexer

data PrintState = PrintState{ output :: [String] , blockN :: Int } deriving(Show)

type MyPrintStateM a = StateT PrintState IO a


printParser :: [BLOCK] -> MyPrintStateM ()
printParser [] = do 
    (PrintState str int ) <- get
    io $ putStr (unlines $ (taskStr:reverse str)) 
    return ()
    where
        taskStr = "TAREAS:"
printParser (x:xs) = do
    printBlock x
    printParser xs

incBlockN :: MyPrintStateM ()
incBlockN = do
    (PrintState str int ) <- get
    put (PrintState str (int+1) )
    return ()


printBlock :: BLOCK -> MyPrintStateM ()
printBlock (WORLD _ _ _) = do
    incBlockN
    return ()
printBlock (TASK _ taskId onWorld []) = do
    incBlockN
    (PrintState str int ) <- get
    put (PrintState ((bNum++show(int)):ins:wId:tId:str) int )
    where
        tId  = (getStr taskId) ++ ":"
        wId  = "  mundo: " ++ (getStr onWorld)
        ins  = "  bloque de instrucciones: sin instrucciones."
        bNum = "    identificador de bloque: "
printBlock (TASK _ taskId onWorld taskInstrs) = do
    incBlockN
    (PrintState str int ) <- get
    put (PrintState ((bNum++show(int)):ins:wId:tId:str) int )
    printInstrBlock 4 taskInstrs
    where
        tId  = (getStr taskId) ++ ":"
        wId  = "  mundo: " ++ (getStr onWorld)
        ins  = "  bloque de instrucciones:"
        bNum = "    identificador de bloque: "


printInstrBlock :: Int -> [TASKINSTR] -> MyPrintStateM ()
printInstrBlock spaces [] = return ()
printInstrBlock spaces (x:xs) = do
    printInstr spaces x
    printInstrBlock spaces xs


printInstr :: Int -> TASKINSTR -> MyPrintStateM ()
printInstr spaces (IF _ test tInst) = do 
    incBlockN -- IF have his own scope
    (PrintState str int) <- get
    put (PrintState (cd:(bNum++show(int)):cond:str) int)
    printGuard (spaces+4) test
    (PrintState str' int') <- get
    put (PrintState (inst:str') int')
    printInstr (spaces+4) tInst
    where
        cond = replicate spaces ' ' ++ "CONDICIONAL IF:"
        bNum = replicate (spaces+2) ' ' ++ "identificador de bloque: "
        cd   = replicate (spaces+2) ' ' ++ "condicion:"
        inst = replicate (spaces+2) ' ' ++ "instruccion:"

printInstr spaces (IFELSE _ test tInst0 tInst1) = do
    (PrintState str int) <- get
    put (PrintState (cd:cond:str) int)
    printGuard (spaces+4) test
    incBlockN -- IF have his own scope
    (PrintState str' int') <- get
    put (PrintState ((bNum++show(int')):inst:str') int')
    printInstr (spaces+4) tInst0
    incBlockN -- ELSE have his own scope
    (PrintState str'' int'') <- get
    put (PrintState ((bNum++show(int'')):inst2:str'') int'')
    printInstr (spaces+4) tInst1
    where
        cond  = replicate spaces ' ' ++ "CONDICIONAL IF/ELSE:"
        bNum = replicate (spaces+2) ' ' ++ "identificador de bloque: "
        cd    = replicate (spaces+2) ' ' ++ "condicion:"
        inst  = replicate (spaces+2) ' ' ++ "instruccion if :"
        inst2 = replicate (spaces+2) ' ' ++ "instruccion else :"

printInstr spaces (REPEAT _ n tInst) = do
    incBlockN -- REPEAT have his own scope
    (PrintState str int) <- get
    put (PrintState (it:nt:nv:(bNum++show(int)):cr:str) int)
    printInstr (spaces+4) tInst
    where
        cr = replicate spaces ' ' ++ "CICLO REPEAT:"
        bNum = replicate (spaces+2) ' ' ++ "identificador de bloque: "
        nv = replicate (spaces+2) ' ' ++ "numero de ciclos:"
        nt = replicate (spaces+4) ' ' ++ (show $ getValue n)
        it = replicate (spaces+2) ' ' ++ "instruccion:"

printInstr spaces (WHILE _ test tInst) = do
    incBlockN -- WHILE have his own scope
    (PrintState str int) <- get
    put(PrintState (gc:(bNum++show(int)):cw:str) int)
    printGuard (spaces+4) test
    (PrintState str' int') <- get
    put(PrintState (it:str') int')
    printInstr (spaces+4) tInst
    where 
        cw = replicate spaces ' ' ++ "CICLO WHILE:"
        bNum = replicate (spaces+2) ' ' ++ "identificador de bloque: "
        gc = replicate (spaces+2) ' ' ++ "condicion:"
        it = replicate (spaces+2) ' ' ++ "instruccion:"

printInstr spaces (BEGIN _ []) = do 
    (PrintState str int) <- get
    put(PrintState (is:(ib++(show (int+1))):bi:str) int)
    incBlockN -- BEGIN have his own scope
    where
        bi = replicate spaces ' ' ++ "BLOQUE BEGIN: "
        ib = replicate (spaces+2) ' ' ++ "identificador de bloque: "
        is = replicate (spaces+2) ' ' ++ "instrucciones: sin instrucciones."

printInstr spaces (BEGIN _ tInsts) = do
    (PrintState str int) <- get
    put(PrintState (is:(ib++(show (int+1))):bi:str) int)
    incBlockN -- BEGIN have his own scope
    printInstrBlock (spaces+4) tInsts
    where
        bi = replicate spaces ' ' ++ "BLOQUE BEGIN: "
        ib = replicate (spaces+2) ' ' ++ "identificador de bloque: "
        is = replicate (spaces+2) ' ' ++ "instrucciones:"

printInstr _ (DEFINE _ _ tInst) = do
    incBlockN  -- DEFINE have his own scope
    traverseDefineInstr tInst

printInstr spaces (MOVE _ ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "move"

printInstr spaces (TURNLEFT _ ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "turnleft"

printInstr spaces (TURNRIGHT _ ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "turnright"

printInstr spaces (PICK _ id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "pick"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)      

printInstr spaces (DROP _ id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)   
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "drop" 
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (SET _ id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "set"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (SETTO _ id tof) = do
    (PrintState str int) <- get
    put(PrintState (tf:vn:is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "set"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)
        vn = replicate (spaces+4) ' ' ++ "valor nuevo:"
        tf = replicate (spaces+6) ' ' ++  (show tof)

printInstr spaces (FLIP _ id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "flip"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (CLEAR _ id) = do
    (PrintState str int) <- get
    put(PrintState (is:io:it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "clear"
        io = replicate (spaces+4) ' ' ++ "identificador:"
        is = replicate (spaces+6) ' ' ++  (getStr id)

printInstr spaces (TERMINATE _ ) = do
    (PrintState str int) <- get
    put(PrintState (it:ip:str) int)
    where
        ip = replicate spaces ' ' ++ "INSTRUCCION PRIMITIVA:"
        it = replicate (spaces+2) ' ' ++ "terminate"

printInstr spaces (INSTRID _ id) = do
    (PrintState str int) <- get
    put(PrintState (st:li:str) int)
    where
        li = replicate spaces ' ' ++ "LLAMADA A INSTRUCCION:"
        st = replicate (spaces+2) ' ' ++ (getStr id)


printGuard :: Int -> TEST -> MyPrintStateM ()
printGuard spaces (TESTTOF _ val) = do
    (PrintState str int) <- get
    put(PrintState (tof:str) int)
    where 
        tof = replicate spaces ' ' ++ show val
printGuard spaces (TESTAND _ test0 test1) = do
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

printGuard spaces (TESTOR _ test0 test1) = do
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

printGuard spaces (TESTNOT _ test) = do
    (PrintState str int) <- get
    put(PrintState (exp:neg:str) int)
    printGuard (spaces+4) test
    where
        neg = replicate spaces ' ' ++ "NEGACION:"
        exp = replicate (spaces+2) ' ' ++ "expresion:"

printGuard spaces (TESTID _ id) = do
    (PrintState str int) <- get
    put(PrintState (idStr:ident:str) int)
    where
        ident = replicate spaces ' ' ++ "IDENTIFICADOR: "
        idStr = replicate (spaces+2) ' ' ++ (getStr id)

printGuard spaces (FRONTCLEAR _ ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "front-clear"

printGuard spaces (LEFTCLEAR _ ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "left-clear"
        
printGuard spaces (RIGHTCLEAR _ ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "right-clear"

printGuard spaces (LOOKNORTH _ ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-north"

printGuard spaces (LOOKEAST _ ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-east"

printGuard spaces (LOOKSOUTH _ ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-south"

printGuard spaces (LOOKWEST _ ) = do
    (PrintState str int) <- get
    put(PrintState (fc:bp:str) int)
    where
        bp = replicate spaces ' ' ++ "BOOLEANO PRIMITIVO:"
        fc = replicate (spaces+2) ' ' ++ "looking-west"

printGuard spaces (FOUND _ id) = do
    (PrintState str int) <- get
    put(PrintState (io:ob:f:eb:str) int)
    where
        eb = replicate spaces ' ' ++ "EXPRESION BOOLEANA:"
        f  = replicate (spaces+2) ' ' ++ "found"
        ob = replicate (spaces+4) ' ' ++ "identificador objeto:"
        io = replicate (spaces+6) ' ' ++ (getStr id)

printGuard spaces (CARRYING _ id) = do
    (PrintState str int) <- get
    put(PrintState (io:ob:f:eb:str) int)
    where
        eb = replicate spaces ' ' ++ "EXPRESION BOOLEANA:"
        f  = replicate (spaces+2) ' ' ++ "carrying"
        ob = replicate (spaces+4) ' ' ++ "identificador objeto:"
        io = replicate (spaces+6) ' ' ++ (getStr id)


io :: IO a -> MyPrintStateM a
io = liftIO


traverseDefineInstrs :: [TASKINSTR] -> MyPrintStateM ()
traverseDefineInstrs [] = return ()
traverseDefineInstrs (tInst:tInsts) = do
    traverseDefineInstr tInst
    traverseDefineInstrs tInsts


traverseDefineInstr :: TASKINSTR -> MyPrintStateM ()
traverseDefineInstr (BEGIN _ tInsts) = do
    incBlockN
    traverseDefineInstrs tInsts

traverseDefineInstr (DEFINE _ _ tInst) = do
    incBlockN
    traverseDefineInstr tInst

traverseDefineInstr  (IF _ _ tInst) = do 
    incBlockN
    traverseDefineInstr tInst

traverseDefineInstr  (IFELSE _ _ tInst0 tInst1) = do
    incBlockN
    traverseDefineInstr tInst0
    traverseDefineInstr tInst1

traverseDefineInstr  (REPEAT _ _ tInst) = do
    incBlockN
    traverseDefineInstr tInst
    
traverseDefineInstr  (WHILE _ _ tInst) = do
    incBlockN
    traverseDefineInstr tInst

traverseDefineInstr _ = return ()