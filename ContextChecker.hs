{-
An Interpreter for the subject "Traductores e Interpretadores" (Translators and Interpreters) 
of the Simon Bolivar University (USB).
  
  Authors:
  
  Neil Villamizar  15-11523
  
  Jesus Wahrman    15-11540
-}

module ContextChecker where

import AST
import Lexer
import Control.Monad.State
import qualified Data.Map as Hash
import SymTable

-- Receives the list of blocks and returns a string in case of error
-- or the symbols table otherwise
createSymTable :: [BLOCK] -> MyStateM (Either String SymTable)
createSymTable [] = do
  (MySymState symT stck err nB ) <- get
  return $ case err of
    []   -> Right symT
    str  -> Left $ unlines $ reverse err  
createSymTable (x:xs) = do
  insertBlock x 
  createSymTable xs

-- Receives a BLOCK, checks its context and inserts it in the table
insertBlock :: BLOCK -> MyStateM ()
insertBlock (WORLD (l,c) (TKId p s) instrs) = do
  (MySymState symT stck err nB ) <- get
  case existId s symT [0] (isToW) of
    True -> do 
      put(MySymState symT stck (em:err) nB)
      insertBlock (WORLD (l,c) (TKId p $ (show $ length err) ++ s) instrs)  
    False -> do 
      insertWorld (l,c) s
      insertWIBlock s instrs
      popScope
      (MySymState symT stck err nB ) <- get
      case worldHasFG s symT of 
        True  -> return() 
        False -> put(MySymState symT stck (em2:err) nB) 
    where
      em    = "Error: redefinicion de " ++ s ++ " en la linea "
              ++ show l ++ " y columna " ++ show c
      em2    = "Error: no se definio un Final Goal para " ++ s ++ " en la linea "
              ++ show l ++ " y columna " ++ show c        
      isToW x = isTask x || isWorld x

insertBlock (TASK (l,c) (TKId tP tId) (TKId wP wId) tasks) = do
  (MySymState symT stck err nB ) <- get
  case existId tId symT [0] (isToW) of
    True  -> do -- exists
      put(MySymState symT stck (em1:err) nB)
      insertBlock (TASK (l,c) (TKId tP $ (show $ length err) ++ tId) (TKId wP wId) tasks)  
    False ->
      case existId wId symT [0] isWorld of
        False -> do
          put(MySymState symT (-1:stck) (em2:err) nB)
          pushScope
          (MySymState symT stck err nB ) <- get
          let val = Task (l,c) tId 0 nB wId 
          insToTable tId val
          insertTIBlock tasks
          popScope
          popScope
        True  -> do
          let wSc = getWorldScope wId symT
          put(MySymState symT (wSc:stck) err nB)
          pushScope
          (MySymState symT stck err nB ) <- get
          let val = Task (l,c) tId 0 nB wId 
          insToTable tId val
          insertTIBlock tasks
          popScope
          popScope
  where
      em1    = "Error: redefinicion de " ++ tId ++ " en la linea "
              ++ show l ++ " y columna " ++ show c
      em2    = "Error: el mundo dado no existe, en la linea "
              ++ show l ++ " y columna " ++ show c
      isToW x = isTask x || isWorld x

-- Receives a block of task instructions and calls a function to check them
insertTIBlock :: [TASKINSTR] -> MyStateM()
insertTIBlock []     = return()
insertTIBlock (x:xs) = do
  insertTInst x 
  insertTIBlock xs

-- Receives a task instruction, checks its context and inserts a value in 
-- the table if needed
insertTInst :: TASKINSTR -> MyStateM()
insertTInst (IF (l,c) guard instr ) = do
  validTaskTest guard
  pushScope 
  insertTInst instr
  popScope

insertTInst (IFELSE (l,c) guard instr1 instr2) = do
  validTaskTest guard
  pushScope 
  insertTInst instr1
  popScope
  pushScope 
  insertTInst instr2
  popScope

insertTInst (REPEAT (l,c) n instr) = do
  pushScope 
  insertTInst instr
  popScope

insertTInst (WHILE (l,c) guard instr) = do
  validTaskTest guard
  pushScope 
  insertTInst instr
  popScope

insertTInst (BEGIN (l,c) instrs) = do
  pushScope 
  insertTIBlock instrs
  popScope

insertTInst (DEFINE (l,c) (TKId _ id) instr) = do
  (MySymState symT (st:stck) err nB ) <- get
  case notExistId id symT (st:stck) of
    True  -> do
      pushScope
      let val = Instruction (l,c) id st (nB+1) instr
      insToTable id val
      insertTInst instr
      popScope
    False -> do
      pushScope
      (MySymState symT stck err nB ) <- get
      put(MySymState symT stck (em1:err) nB)
      insertTInst instr
      popScope
  where 
    em1   = "Error: redefinicion de " ++ id ++ " en la linea "
            ++ show l ++ " y columna " ++ show c 

insertTInst (MOVE _) = return()

insertTInst (TURNLEFT _) = return()

insertTInst (TURNRIGHT _) = return()

insertTInst (PICK (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isObject of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un objeto, en la linea "
          ++ show l ++ " y columna " ++ show c

insertTInst (DROP (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isObject of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un objeto, en la linea "
          ++ show l ++ " y columna " ++ show c

insertTInst (SET (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isBoolean of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un booleano, en la linea "
          ++ show l ++ " y columna " ++ show c

insertTInst (SETTO (l,c) (TKId _ id) bool) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isBoolean of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un booleano, en la linea "
          ++ show l ++ " y columna " ++ show c

insertTInst (CLEAR (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isBoolean of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un booleano, en la linea "
          ++ show l ++ " y columna " ++ show c

insertTInst (FLIP (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isBoolean of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un booleano, en la linea "
          ++ show l ++ " y columna " ++ show c

insertTInst (TERMINATE _) = return()

insertTInst (INSTRID (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case existId id symT stck isInstruction of
    True  -> return()
    False -> put(MySymState symT stck (em:err) nB)
  where
    em = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c



-- Receives the id of the world and a block of world instructions 
-- and calls a function to check them
insertWIBlock :: String -> [INSTR] -> MyStateM ()
insertWIBlock id []     = return ()
insertWIBlock id (x:xs) = do
  insertWInst id x
  insertWIBlock id xs 


-- Receives a world instruction, checks its context and inserts a value in 
-- the table if needed
insertWInst :: String -> INSTR -> MyStateM ()
insertWInst id (WORLDSIZE (l,c) cols rows)     = do
  (MySymState symT stck err nB ) <- get
  case cols'*rows' of 
    0         -> do
      put(MySymState symT stck (em:err) nB)
    otherwise -> 
      case getWSize id symT of
        (1,1)     -> do
          updWorldSize id (cols',rows')
        otherwise -> do
          put(MySymState symT stck (em':err) nB)
  where 
    cols' = getValue cols
    rows' = getValue rows
    em    = "Error: las filas y columnas no pueden ser 0 o negativo. En la linea "
             ++ show l ++ " y columna " ++ show c 
    em'   = "Error: no se puede definir el tamano del mundo 2 veces. En la linea "
             ++ show l ++ " y columna " ++ show c

insertWInst id (WALL (l,c) dir c1 r1 c2 r2) = do
  (MySymState symT stck err nB ) <- get
  case x1 * y1 * x2 * y2 of
    0         -> do
      put (MySymState symT stck (em:err) nB)
    otherwise -> 
      case checkWall x1 y1 x2 y2 direction id symT of
        1 -> do put (MySymState symT stck (em1:err) nB)
        2 -> do put (MySymState symT stck (em2:err) nB)
        3 -> do put (MySymState symT stck (em3:err) nB)
        0 -> do updWorldWall id x1 y1 x2 y2 direction
  where
    direction = show dir
    x1 = getValue c1
    y1 = getValue r1
    x2 = getValue c2
    y2 = getValue r2
    em  = "Error: las filas y columnas no pueden ser 0 o negativo. En la linea "
           ++ show l ++ " y columna " ++ show c
    em1 = "Error: fila o columna fuera de los limites del mundo. En la linea "
           ++ show l ++ " y columna " ++ show c
    em2 = "Error: Wall: la direccion \"" ++ show dir ++ "\" no corresponde con las posiciones dadas."
           ++ " En la linea " ++ show l ++ " y columna " ++ show c
    em3 = "Error: no se pueden colocar paredes sobre una posicion donde hay objetos o esta willy. En la linea "
           ++ show l ++ " y columna " ++ show c

insertWInst id (OBJECTTYPE (l,c) oId color) = do
  (MySymState symT stck err nB ) <- get
  case not $ notExistId oId' symT stck of
    True      -> put(MySymState symT stck (em:err) nB)
    otherwise -> do
      let val = ObjectType (l,c) oId' nB (show color)
      insToTable oId' val
  where
    em = "Error: redefinicion de " ++ (getStr oId) ++ " en la linea "
          ++ show l ++ " y columna " ++ show c
    oId' = getStr oId

insertWInst id (PLACEAT (l,c) n obj col row) = do
  (MySymState symT stck err nB ) <- get
  case n' of
    0         -> put(MySymState symT stck (em:err) nB)
    otherwise -> case checkPlaceAt id objId (col',row') symT stck of
                   1 -> put(MySymState symT stck (em1:err) nB)
                   2 -> put(MySymState symT stck (em2:err) nB)
                   3 -> put(MySymState symT stck (em3:err) nB)
                   4 -> put(MySymState symT stck (em4:err) nB)
                   5 -> put(MySymState symT stck (em5:err) nB)
                   6 -> put(MySymState symT stck (em6:err) nB)
                   0 -> do
                    placeObject id objId n' (col',row')
  where
    n'    = getValue n
    objId = getStr obj
    col'  = getValue col
    row'  = getValue row
    em    = "Error: no se pueden colocar 0 objetos, en la linea " 
            ++ show l ++ " y columna " ++ show c
    em1   = "Error: el identificador no existe, en la linea "
            ++ show l ++ " y columna " ++ show c
    em2   = "Error: identificador fuera de alcance, en la linea "
            ++ show l ++ " y columna " ++ show c
    em3   = "Error: el identificador no es de objeto, en la linea "
            ++ show l ++ " y columna " ++ show c
    em4   = "Error: la casilla se sale del mundo, en la linea "
            ++ show l ++ " y columna " ++ show c
    em5   = "Error: hay una pared en donde se esta intentando colocar el objeto, en la linea "
            ++ show l ++ " y columna " ++ show c
    em6   = "Error: no se puede colocar en fila o columna 0, en la linea "
            ++ show l ++ " y columna " ++ show c

insertWInst id (BASKET (l,c) n) = do
  (MySymState symT stck err nB ) <- get
  case n' of
    0         -> put(MySymState symT stck (em:err) nB) 
    otherwise ->
      case Hash.lookup id symT of
        Nothing      -> return() 
        Just listVal -> do
          let world = filter isWorld listVal !! 0
          case basketSize world of 
            1         -> do 
              let newWorld = world{basketSize = n'}
              put(MySymState (Hash.insert id (updateWListVal newWorld listVal) symT) stck err nB)
            otherwise -> 
              put (MySymState symT stck (em1:err) nB)
  where 
    n'  = getValue n
    em  = "Error: no se puede definir basket de capacidad 0, en la linea "
          ++ show l ++ " y columna " ++ show c
    em1 = "Error: no se puede definir la capacidad de basket mas de 1 vez, en la linea "
          ++ show l ++ " y columna " ++ show c

insertWInst id (PLACEIN (l,c) n obj) = do
  (MySymState symT stck err nB ) <- get
  case checkPlaceIn id objId n' symT stck of
    1 -> put (MySymState symT stck (em1:err) nB)
    2 -> put (MySymState symT stck (em2:err) nB)
    3 -> put (MySymState symT stck (em3:err) nB)
    4 -> put (MySymState symT stck (em4:err) nB)
    5 -> put (MySymState symT stck (em5:err) nB)
    0 -> insertObject id objId n' symT
  where
    n'    = getValue n
    objId = getStr obj
    em1 = "Error: no se puede colocar 0 objetos en basket, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2 = "Error: el identificador no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em3 = "Error: identificador fuera de alcance, en la linea "
          ++ show l ++ " y columna " ++ show c     
    em4 = "Error: el identificador no es de un objeto, en la linea "
          ++ show l ++ " y columna " ++ show c              
    em5 = "Error: no hay suficiente espacio en basket, en la linea "
          ++ show l ++ " y columna " ++ show c   

insertWInst id (STARTAT (l,c) col row dir) = do
  (MySymState symT stck err nB ) <- get
  case validStart id (col',row') symT of
    1 -> put (MySymState symT stck (em1:err) nB)
    2 -> put (MySymState symT stck (em2:err) nB)
    3 -> put (MySymState symT stck (em3:err) nB)
    0 -> updStartPos id (col',row') dir'
  where
    col' = getValue col
    row' = getValue row
    dir' = show dir
    em1 = "Error: la columna o fila 0 no es valida, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2 = "Error: la casilla se sale del mundo, en la linea "
          ++ show l ++ " y columna " ++ show c     
    em3 = "Error: hay una pared en la casilla donde se quiere poner a willy, en la linea "
          ++ show l ++ " y columna " ++ show c  

insertWInst id (GOALIS (l,c) gId gTest) = do
  (MySymState symT stck err nB ) <- get
  case notExistId gId' symT stck of
    False -> put (MySymState symT stck (em1:err) nB)
    True  -> 
      case validTest id gTest symT stck of
        1 -> put (MySymState symT stck (em2:err) nB)
        2 -> put (MySymState symT stck (em3:err) nB)
        3 -> put (MySymState symT stck (em4:err) nB)
        4 -> put (MySymState symT stck (em5:err) nB)
        0 -> do
          let val = (Goal (l,c) gId' nB gTest)
          insToTable gId' val
  where 
    gId' = getStr gId
    em1  = "Error: redefinicion de " ++ gId' ++ ". Id dado ya estaba en uso, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: la columna o fila 0 no es valida, en la linea "
          ++ show l ++ " y columna " ++ show c
    em3  = "Error: la casilla dada se sale de los limites del mundo, en la linea "
          ++ show l ++ " y columna " ++ show c
    em4  = "Error: no existe ningun objeto con el id dado, en la linea "
          ++ show l ++ " y columna " ++ show c
    em5  = "Error: el id de objeto dado pertenece a otro tipo, en la linea "
          ++ show l ++ " y columna " ++ show c


insertWInst id (BOOLEAN (l,c) boolId boolValue) = do
  (MySymState symT stck err nB ) <- get
  case not $ notExistId boolId' symT stck of
    True      -> put(MySymState symT stck (em:err) nB)
    otherwise -> do
      let val = WBoolean (l,c) boolId' nB (getBool boolValue)
      insToTable boolId' val
  where
    boolId' = getStr boolId
    em = "Error: redefinicion de " ++ boolId' ++ " en la linea "
          ++ show l ++ " y columna " ++ show c
          
insertWInst id (FINALIS (l,c) fGoal) = do
  (MySymState symT stck err nB ) <- get
  case worldHasFG id symT of
    True  -> do
      put(MySymState symT stck (em:err) nB)
      validFinalGoal fGoal
    False -> do
      validFinalGoal fGoal
      (MySymState symT stck err' nB ) <- get
      case length err' == length err of
        False -> return()
        True  -> updFinalGoal id fGoal
  where 
    em = "Error: no se pueden definir dos Final Goal en la linea "
          ++ show l ++ " y columna " ++ show c


-- Receives a FINALGOAL and checks if its valid
validFinalGoal :: FINALGOAL -> MyStateM()
validFinalGoal (FGAND (l,c) left right) = do
  validFinalGoal left
  validFinalGoal right

validFinalGoal (FGOR (l,c) left right) = do
  validFinalGoal left
  validFinalGoal right

validFinalGoal (FGNOT (l,c) exp) = do
  validFinalGoal exp

validFinalGoal (FGID (l,c) id) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id' symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id' symT stck isGoal || existId id' symT stck isBoolean of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    id' = getStr id
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de Booleano o Goal, en la linea "
          ++ show l ++ " y columna " ++ show c


-- Receives a TEST and checks if its valid
validTaskTest :: TEST -> MyStateM()
validTaskTest (TESTAND (l,c) left right) = do
  validTaskTest left
  validTaskTest right

validTaskTest (TESTOR (l,c) left right) = do
  validTaskTest left
  validTaskTest right

validTaskTest (TESTNOT (l,c) exp) = do
  validTaskTest exp

validTaskTest (TESTID (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isBoolOrGuard of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    isBoolOrGuard x = isBoolean x || isGoal x
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de Booleano o Goal, en la linea "
          ++ show l ++ " y columna " ++ show c

validTaskTest (FOUND (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isObject of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un objeto, en la linea "
          ++ show l ++ " y columna " ++ show c

validTaskTest (CARRYING (l,c) (TKId _ id)) = do
  (MySymState symT stck err nB ) <- get
  case not $ existId id symT stck isTrue of
    True  -> put (MySymState symT stck (em1:err) nB)
    False ->
      case existId id symT stck isObject of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> return()
  where 
    em1  = "Error: el id dado no existe, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: el id dado no es de un objeto, en la linea "
          ++ show l ++ " y columna " ++ show c
          
validTaskTest (TESTTOF _ bool) = return ()
validTaskTest (FRONTCLEAR _ ) = return ()
validTaskTest (LEFTCLEAR _ ) = return ()
validTaskTest (RIGHTCLEAR _ ) = return ()
validTaskTest (LOOKNORTH _ ) = return ()
validTaskTest (LOOKEAST _ ) = return ()
validTaskTest (LOOKSOUTH _ ) = return ()
validTaskTest (LOOKWEST _ ) = return ()


-- Helpful functions

-- Receives the position and the id of the world and inserts it 
-- to the table
insertWorld :: Pos -> String -> MyStateM ()
insertWorld p id = do
  (MySymState symT (st:sts) err nB ) <- get
  let val      = World p id st (nB+1) Hash.empty (1,1) 1 [] (1,1) "north" None
  insToTable id val
  pushScope


-- Receives an id and a SymValue and inserts it to the table
insToTable :: String -> SymValue -> MyStateM ()
insToTable id val = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id symT of
    Nothing -> put(MySymState (Hash.insert id [val] symT) stck err nB)
    Just xs -> put(MySymState (Hash.insert id (val:xs) symT) stck err nB)


-- Pushes a new scope on the stack of the state
pushScope :: MyStateM ()
pushScope = do
  (MySymState symT stck err nB ) <- get
  put(MySymState symT ((nB+1):stck) err (nB+1))


-- Pops a scope from the stack
popScope :: MyStateM ()
popScope = do
  (MySymState symT (x:xs) err nB ) <- get
  put(MySymState symT xs err nB)


-- Receives the id of the world and the new size and updates the size
-- in the table,
updWorldSize :: String -> (Int,Int) -> MyStateM ()
updWorldSize id (c,r) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id symT of
    Nothing -> return() 
    Just listVal -> do
      let oldWorld = filter isWorld listVal !! 0 
      let newWorld = oldWorld{size=(c,r)} 
      put(MySymState (Hash.insert id (updateWListVal newWorld listVal) symT) stck err nB)
      

-- Gets the id of the world and the table and returns the size of the world
getWSize :: String -> SymTable -> (Int,Int)
getWSize id symT = 
  case Hash.lookup id symT of
    Just listVal -> 
      size $ filter isWorld listVal !! 0
    _ -> (1,1)


-- Receives the x and y start coordinates and end coordinates of the wall,
-- the direction of the wall, the id of the world, the table and returns
-- 0 in case that the wall is valid and x>0 in case of error
checkWall :: Int -> Int -> Int -> Int -> String -> String -> SymTable -> Int
checkWall x1 y1 x2 y2 dir worldId symT
  | x1 > xlim || x2 > xlim                          = 1 -- a cell is out of the world
  | y1 > ylim || y2 > ylim                          = 1 -- a cell is out of the world
  | dir == "north" && (x1 /= x2 || y2 > y1)         = 2 -- the direction is wrong
  | dir == "south" && (x1 /= x2 || y2 < y1)         = 2 -- the direction is wrong
  | dir == "east"  && (x1 > x2 || y2 /= y1)         = 2 -- the direction is wrong
  | dir == "west"  && (x1 < x2 || y2 /= y1)         = 2 -- the direction is wrong
  | not $ clearForWall x1 y1 x2 y2 dir worldId symT = 3 -- theres something in the cell
  | otherwise                                       = 0 -- no problem
  where (xlim,ylim) = getWSize worldId symT


-- Receives the x and y start coordinates and end coordinates of the wall,
-- the direction of the wall, the id of the world, the table and returns
-- True in case that the cells are clear or False otherwise
clearForWall :: Int -> Int -> Int -> Int -> String -> String -> SymTable -> Bool
clearForWall x y finalx finaly dir worldId symT
  | not $ emptyCell x y worldId symT = False
  | x == finalx && y == finaly       = True
  | dir == "north"         = clearForWall x (y-1) finalx finaly dir worldId symT
  | dir == "south"         = clearForWall x (y+1) finalx finaly dir worldId symT
  | dir == "east"          = clearForWall (x+1) y finalx finaly dir worldId symT
  | dir == "west"          = clearForWall (x-1) y finalx finaly dir worldId symT


-- Receives the x and y coordinates of the cell,
-- the id of the world, the table and returns True if the cell
-- is doesnt contain an object or willy or False otherwise 
emptyCell :: Int -> Int -> String -> SymTable -> Bool
emptyCell x y worldId symT = case Hash.lookup worldId symT of
  Just listVal ->  do
    let world = filter isWorld listVal !! 0
    case Hash.lookup (x,y) $ desc world of
      Just (Objects _)     -> False
      _ -> (x,y) /= willyIsAt world 
  _ -> False


-- Receives the id of the world, the x and y start coordinates 
-- and end coordinates of the wall, the direction of the wall and
-- adds the wall to the world description
updWorldWall :: String -> Int -> Int -> Int -> Int -> String -> MyStateM ()
updWorldWall worldId x1 y1 x2 y2 dir = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup worldId symT of
    Just listVal -> do
      let oldWorld = filter isWorld listVal !! 0
      let newWorld =  oldWorld{desc=(Hash.insert (x1,y1) (Wall) $ desc oldWorld)} 
      put (MySymState (Hash.insert worldId (updateWListVal newWorld listVal) symT) stck err nB)
      if (x1==x2 && y1==y2) then do return ()
      else if (dir == "north") then updWorldWall worldId x1 (y1-1) x2 y2 dir
      else if (dir == "south") then updWorldWall worldId x1 (y1+1) x2 y2 dir
      else if (dir == "east" ) then updWorldWall worldId (x1+1) y1 x2 y2 dir
      else                          updWorldWall worldId (x1-1) y1 x2 y2 dir
    _  -> return ()


-- Receives the id of the world, id of the object, position to place the object,
-- the table and the stack of scopes and returns 0 in case of a valid place at or
-- x>0 in case of an error
checkPlaceAt :: String -> String -> Pos -> SymTable -> [Int] -> Int
checkPlaceAt worldId objId (col,row) symT scope
  | notExists                                  = 1 -- the id doesnt exist
  | not $ existId objId symT scope isTrue      = 2 -- the id is not in the current scopes
  | not $ existId objId symT scope isObject    = 3 -- the id isnt from an object
  | colLim < col || rowLim < row               = 4 -- the cell is out of the world
  | not $ cellWithoutWall col row worldId symT = 5 -- theres a wall in the cell
  | col*row == 0                               = 6 -- cant place objects at col or row 0
  | otherwise                                  = 0 -- no problem
  where (colLim,rowLim) = getWSize worldId symT
        notExists = case Hash.lookup objId symT of
          Nothing -> True
          _       -> False


-- Receives x and y coordinates of a cell, the world id, the table
-- and returns true in case that there isnt a wall in that cell or
-- false otherwise
cellWithoutWall :: Int -> Int -> String -> SymTable -> Bool
cellWithoutWall x y worldId symT = case Hash.lookup worldId symT of
  Just listVal -> do
    let world = filter isWorld listVal !! 0
    case Hash.lookup (x,y) $ desc world of
      Just Wall -> False  -- wall, then return false
      _         -> True -- no wall, then return true
  _ -> False


-- Receives the world id, object id, the number of objects and the cell
-- to place them and it places the number of objects in the world description
placeObject :: String -> String -> Int -> Pos -> MyStateM()
placeObject worldId objId n (c,r) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup worldId symT of
    Just listVal -> do
      let world = filter isWorld listVal !! 0
      case Hash.lookup (c,r) $ desc world of
        Just (Objects map) -> -- there already are objects in the cell
          case Hash.lookup objId map of
            Just m -> do -- there already are objects of the same type in the cell
              let v  = Hash.insert objId (m+n) map
              let v' = Hash.insert (c,r) (Objects v) $ desc world
              let newWorld = world{desc=v'}
              put(MySymState (Hash.insert worldId (updateWListVal newWorld listVal) symT) stck err nB)
            Nothing -> do -- there are not objects of the same type in the cell
              let v  = Hash.insert objId n map
              let v' = Hash.insert (c,r) (Objects v) $ desc world 
              let newWorld = world{desc=v'}
              put(MySymState (Hash.insert worldId (updateWListVal newWorld listVal) symT) stck err nB)
        Nothing -> do -- there are not objects in the cell
          let v  = Hash.insert objId n Hash.empty
          let v' = Hash.insert (c,r) (Objects v) $ desc world 
          let newWorld = world{desc=v'}
          put(MySymState (Hash.insert worldId (updateWListVal newWorld listVal) symT) stck err nB)


-- Receives the id of the world, id of the object,number of objects,
-- the table and the stack of scopes and returns 0 in case of a valid place at or
-- x>0 in case of an error
checkPlaceIn :: String -> String -> Int -> SymTable -> [Int] -> Int
checkPlaceIn worldId objId n symT scope
  | n == 0                                     = 1 -- cant place 0 objects
  | notExists                                  = 2 -- the object id doesnt exist
  | not $ existId objId symT scope isTrue      = 3 -- obj id isnt in the current scopes
  | not $ existId objId symT scope isObject    = 3 -- the obj id isnt from an object
  | basketCap worldId symT < n                 = 5 -- not enough space in the basket
  | otherwise                                  = 0 -- no problem
  where notExists = case Hash.lookup objId symT of
          Nothing -> True
          _       -> False


-- Receives the world id and the table and returns the capacity of 
-- the basket of that world
basketCap :: String -> SymTable -> Int
basketCap wId symT =
  case Hash.lookup wId symT of 
    Just listVal -> do
      let world = filter isWorld listVal !! 0
      basketSize world - (length $ objectsInB world)
    otherwise -> 0 -- Este nunca pasa


-- Receives the world id and the table and returns the scope number of that world
getWorldScope :: String -> SymTable -> Int
getWorldScope wId symT =
  case Hash.lookup wId symT of 
    Just listVal -> do
      let world = filter isWorld listVal !! 0
      numBlock world 
    otherwise -> 0   


-- Receives the world id and the table and returns true if a final goal was
-- defined for that world or false otherwise
worldHasFG :: String -> SymTable -> Bool
worldHasFG wId symT =
  case Hash.lookup wId symT of 
    Just listVal -> do
      let world = filter isWorld listVal !! 0
      case finalG world of
        None      -> False
        otherwise -> True
    otherwise -> False   


-- Receives the id of the world and the FINALGOAL and updates the world
-- with that FINALGOAL
updFinalGoal :: String -> FINALGOAL -> MyStateM()
updFinalGoal wId fg = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup wId symT of
    Just listVal -> do
      let oldWorld = filter isWorld listVal !! 0
      let newWorld = oldWorld{finalG = FinalG fg}
      put(MySymState (Hash.insert wId (updateWListVal newWorld listVal) symT) stck err nB)
    Nothing -> return() 


-- Receives the world id, object id, number of objects and the table
-- and inserts that number of objects in the basket of that world
insertObject :: String -> String -> Int -> SymTable -> MyStateM()
insertObject wId oId n symT = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup wId symT of
    Just listVal -> do
      let oldWorld = filter isWorld listVal !! 0
      let newObjInB = (replicate n oId) ++ (objectsInB oldWorld)
      let newWorld = oldWorld{objectsInB=newObjInB}
      put(MySymState (Hash.insert wId (updateWListVal newWorld listVal) symT) stck err nB)
    Nothing -> return() 


-- Receives the world id, the position and the table and return 0 in case of 
-- a valid start position or x>0 otherwise
validStart :: String -> Pos -> SymTable -> Int
validStart wId (col,row) symT 
  | col*row == 0                           = 1 -- cant have col or row as 0
  | colLim < col || rowLim < row           = 2 -- the position is out of the world
  | not $ cellWithoutWall col row wId symT = 3 -- there is a wall in that position
  | otherwise                              = 0 -- no problem
  where (colLim,rowLim) = getWSize wId symT


-- Receives the world id, the position and the direction of willy and 
-- updates willy start position and direction
updStartPos :: String -> Pos -> String -> MyStateM()
updStartPos wId pos dir = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup wId symT of
    Just listVal -> do
      let oldWorld = filter isWorld listVal !! 0
      let newWorld = oldWorld{willyIsAt = pos, willyDirection = dir}
      put(MySymState (Hash.insert wId (updateWListVal newWorld listVal) symT) stck err nB)
    Nothing -> return() 


-- Receives the world id, a GOALTEST, the table and the stack of scopes
-- and returns 0 in case of a valid test or x>0 otherwise
validTest :: String -> GOALTEST -> SymTable -> [Int] -> Int
validTest wId (WILLYISAT (l,c) col row) symT stck
  | col'*row' == 0                         = 1 -- cant have col or row as 0
  | colLim < col' || rowLim < row'         = 2 -- the position is out of the world
  | otherwise                              = 0 -- no problem
  where
    col' = getValue col
    row' = getValue row
    (colLim,rowLim) = getWSize wId symT

validTest wId (OBJECTSIN (l,c) n oId) symT stck
  | not $ existId oId' symT stck isTrue   = 3 -- the id doesnt exist
  | not $ existId oId' symT stck isObject = 4 -- the id isnt from an object
  | otherwise                             = 0 -- no problem
  where 
    n'   = getValue n
    oId' = getStr oId

validTest wId (OBJECTSAT (l,c) n oId col row) symT stck
  | col'*row' == 0                        = 1 -- cant have col or row as 0
  | colLim < col' || rowLim < row'        = 2 -- the position is out of the world
  | not $ existId oId' symT stck isTrue   = 3 -- the id doesnt exist
  | not $ existId oId' symT stck isObject = 4 -- the id isnt from an object
  | otherwise                             = 0 -- no problem
  where
    col'            = getValue col
    row'            = getValue row
    (colLim,rowLim) = getWSize wId symT
    n'              = getValue n
    oId'            = getStr oId


-- Receives the id of the SymValue, the table, the stack of scopes and a function
-- that tells if the SymValue is from the wanted type and returns true if there exists 
-- a SymValue with that id in the scopes or false otherwise
existId :: String -> SymTable -> [Int] -> (SymValue -> Bool) -> Bool 
existId id symT [] _ = False
existId id symT (scope:scopes) isX = 
  case Hash.lookup id symT of
    Nothing -> False -- theres nothing with that id
    Just listOfValues -> -- list of values with that id in ALL scopes
      case scopeBelongs scope $ filter isX listOfValues of -- check for scope in stack
        False -> existId id symT scopes isX -- if its not, look with the next scope
        True  -> True -- if it is return true


-- Receives an scope, a list of SymValues and returns true if there is a SymValue
-- with that scope or false otherwise
scopeBelongs :: Int -> [SymValue] -> Bool  
scopeBelongs scope [] = False
scopeBelongs scope (val:vals)
  | defBlock val == scope = True
  | otherwise             = scopeBelongs scope vals


-- Receives a SymValue id, the table, the stack of scopes and returns
-- true if the id is available for use or false otherwise
notExistId :: String -> SymTable -> [Int] -> Bool 
notExistId id symT []     = True
notExistId id symT scopeStack@(scope:scopes) = 
  case Hash.lookup id symT of
    Nothing -> True 
    Just listOfValues ->  usableIDforDeclare scope wScope listOfValues
  where wScope = scopeStack !! (length scopeStack - 2)


-- Receives the current scope, the scope of the world and a list of SymValues
-- and returns true if the id is available or false otherwise
usableIDforDeclare :: Int -> Int -> [SymValue] -> Bool 
usableIDforDeclare currentScope worldScope [] = True
usableIDforDeclare currentScope worldScope (val:vals)
  | defBlock val == currentScope                = False 
  | isBoolean val && defBlock val == worldScope = False
  | isGoal val && defBlock val == worldScope    = False
  | isObject val  && defBlock val == worldScope = False
  | otherwise = usableIDforDeclare currentScope worldScope vals


-- Receives a SymValue and returns true if its a goal or false otherwise
isGoal :: SymValue -> Bool
isGoal Goal{} = True
isGoal _      = False


-- Receives a SymValue and returns true if its a boolean or false otherwise
isBoolean :: SymValue -> Bool
isBoolean WBoolean{} = True
isBoolean _          = False


-- Receives a SymValue and returns true if its an object or false otherwise
isObject :: SymValue -> Bool
isObject ObjectType{} = True
isObject _            = False


-- Receives a SymValue and returns true if its a world or false otherwise
isWorld :: SymValue -> Bool
isWorld World{} = True
isWorld _       = False


-- Receives a SymValue and returns true if its an instruction or false otherwise
isInstruction :: SymValue -> Bool
isInstruction Instruction{} = True
isInstruction _             = False


-- Receives a SymValue and returns true if its a task or false otherwise
isTask :: SymValue -> Bool
isTask Task{} = True
isTask _      = False


-- Receives a SymValue and returns true always
isTrue :: SymValue -> Bool
isTrue _ = True

-- Receives the new value of the world and a list of SymValues with 
-- the same id as the world and inserts the new world in it
updateWListVal :: SymValue -> [SymValue] -> [SymValue]
updateWListVal newWorld (val:vals)
  | isWorld val = newWorld:vals
  | otherwise    = val:updateWListVal newWorld vals