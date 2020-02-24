
module PrintSymTable where

import AST
import Lexer
import qualified Data.Map as Hash
import qualified SymTable as ST
import PrintParser as PP
import Control.Monad.State
--import Data.Functor.Identity

--type MyPrintStateM a = State PrintState a

printSymTable :: ST.SymTable -> String 
printSymTable symT = unlines $ map (getSymString') (Hash.toList symT)


getSymString' :: (String, [ST.SymValue]) -> String
getSymString' (id, [symVal]) = getSymStrings id [symVal] 

getSymStrings :: String -> [ST.SymValue] -> String
getSymStrings id [s]    = getSymString id s
getSymStrings id (s:ss) = (getSymString id s) ++ "\n"
                          ++ (getSymStrings id ss)

getSymString :: String -> ST.SymValue -> String
getSymString id (ST.World _ _ decBlock idBlock descW (wx,wy) baskSz objInBsk (x,y) willyDir) =
  id ++ ":\n"
  ++ "  tipo: mundo\n"
  ++ "  identificador de bloque: " ++ (show idBlock) ++ "\n"
  ++ "  tamanio: " ++ (show x) ++ (show y) ++ "\n"
  ++ "  muros:\n" ++ (printWall 4 descW)
  ++ "  posicion de willy: " ++ (show wx) ++ (show wy) ++ "\n"
  ++ "  tamanio de la cesta: " ++ (show baskSz) ++ "\n"
  ++ "  objetos en la cesta: " ++ (printBasketObj 4 objInBsk) ++ "\n"
  ++ "  direccion de willy: " ++ willyDir ++ "\n"

getSymString id (ST.ObjectType _ _ defBlock color) =
  id ++ ":\n"
  ++ "  tipo: Objeto\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  color del objeto: " ++ color ++ "\n"

getSymString id (ST.WBoolean _ _ defBlock value) =
  id ++ ":\n"
  ++ "  tipo: booleano\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  valor: " ++ (show value) ++ "\n"

getSymString id (ST.Goal _ _ defBlock goalTest) =
  id ++ ":\n"
  ++ "  tipo: goal\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  expresion: " ++ (printGoalTest 4 goalTest) ++ "\n"

getSymString id (ST.Instruction _ _ defBlock numBlock inst) =
  id ++ ":\n"
  ++ "  tipo: instrucción\n"
  ++ "  bloque de declaracion: " ++ (show defBlock) ++ "\n"
  ++ "  identificador de bloque: " ++ (show numBlock) ++ "\n"
  ++ "  AST asociado:\n" ++ (printInstr' 4 inst defBlock) -- Se podra rehusar la funcion de PrintParser? sino que ladilla

getSymString id (ST.Task _ _ _ numBlock onWorld) =
  id ++ ":\n"
  ++ "  tipo: tarea\n"
  ++ "  mundo asociado: " ++ onWorld ++ "\n"
  ++ "  identificador de bloque: " ++ (show numBlock) ++ "\n"



printWall :: Int -> ST.WorldDesc -> String
printWall spaces wdesc = " jeje "

printBasketObj :: Int -> [String] -> String
printBasketObj spaces xs = " jaja "

printGoalTest :: Int -> GOALTEST -> String
printGoalTest spaces gt = " jiji "

printInstr' :: Int -> TASKINSTR -> Int -> String
printInstr' spaces inst currentScope = unlines $ reverse result
  where (PrintState result _) = do 
        (PrintState str int) <- get
        put (PrintState str currentScope)
        printInstr spaces inst
        (PrintState str' int') <- get
        str'
        --return (reverse str')

{-
incBlockN :: MyPrintStateM ()
incBlockN = do
    (PrintState str int ) <- get
    put (PrintState str (int+1) )
    return ()


-- AYUDA AQUI CON MONADS PARA USAR ESTA BROMA XD XD XD
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

        -}