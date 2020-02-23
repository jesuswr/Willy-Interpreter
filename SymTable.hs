module SymTable where

import AST
import Lexer
import Control.Monad.State
import qualified Data.Map as Hash

type Pos = (Int,Int)

type WorldDesc = Hash.Map Pos WorldElements 

type ObjectsInCell = Hash.Map String Int

data WorldElements = Wall | Objects{ map :: ObjectsInCell} deriving(Show)

data SymValue = World{ startPos :: Pos , id :: String 
                     , defBlock :: Int , numBlock :: Int  
                     , desc :: WorldDesc , willyIsAt :: Pos 
                     , basketSize :: Int , objectsInB :: [String]
                     , size :: Pos , willyDirection :: String
                     }
              | ObjectType{ startPos :: Pos , id :: String 
                          , defBlock :: Int , color :: String 
                          }
              | WBoolean{startPos :: Pos , id :: String 
                        , defBlock :: Int , value :: Bool
                        }
              | Goal{ startPos :: Pos , id :: String 
                    , defBlock :: Int , test :: GOALTEST 
                    }
              | Instruction{ startPos :: Pos , id :: String 
                           , defBlock :: Int , inst :: TASKINSTR
                           }
              | Task{ startPos :: Pos , id :: String 
                    , defBlock :: Int , onWorld :: String }
              deriving(Show)

type SymTable = Hash.Map String [SymValue]

data MySymState = MySymState{ symTable :: SymTable , stack :: [Int] , error :: [String] 
                            , nBlock :: Int }

type MyStateM a = StateT MySymState IO a


io :: IO a -> MyStateM a
io = liftIO


createSymTable :: [BLOCK] -> MyStateM (Either String SymTable)
createSymTable [] = do
  (MySymState symT stck err nB ) <- get
  io $ print symT
  io $ putStr $ unlines $ reverse err
  io $ print stck

  return $ case err of
    []   -> Right symT
    str  -> Left $ unlines $ reverse err  
createSymTable (x:xs) = do
  insertBlock x 
  createSymTable xs


insertBlock :: BLOCK -> MyStateM ()
insertBlock (WORLD (l,c) (TKId p s) instrs) = do
  (MySymState symT stck err nB ) <- get
  case existsWoTId symT s of
    True -> do -- Si existe
      put(MySymState symT stck (em:err) nB)
      insertBlock (WORLD (l,c) (TKId p $ (show $ length err) ++ s) instrs)  
      io $ print $ length err
    False -> do -- No existe
      insertWorld (l,c) s
      insertWIBlock s instrs
      popScope

    where
      em    = "Error: redefinicion de " ++ s ++ " en la linea "
              ++ show l ++ " y columna " ++ show c
-- FALTA HACER LO DE ARRIBA PARA TASK, PERO PRIMERO TODO EL WORLD


insertWIBlock :: String -> [INSTR] -> MyStateM ()
insertWIBlock id []     = return ()
insertWIBlock id (x:xs) = do
  insertWInst id x
  insertWIBlock id xs 


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
    em2   = "Error: el identificador no es de objeto, en la linea "
            ++ show l ++ " y columna " ++ show c
    em3   = "Error: la casilla se sale del mundo, en la linea "
            ++ show l ++ " y columna " ++ show c
    em4   = "Error: hay una pared en donde se esta intentando colocar el objeto, en la linea "
            ++ show l ++ " y columna " ++ show c
    em5   = "Error: no se puede colocar en fila o columna 0, en la linea "
            ++ show l ++ " y columna " ++ show c

insertWInst id (BASKET (l,c) n) = do
  (MySymState symT stck err nB ) <- get
  case n' of
    0         -> put(MySymState symT stck (em:err) nB) -- caso nuevo tamano igual a 0
    otherwise ->
      case Hash.lookup id symT of
        Nothing -> return() --Este nunca pasa
        Just ((World pos wId defB numB desc w baskS objInB siz willyDir):xs) -> 
          case baskS of 
            1         -> do -- Caso tamano viejo igual a 1, cambiar
              let val = (World pos wId defB numB desc w n' objInB siz willyDir)
              put(MySymState (Hash.insert id (val:xs) symT) stck err nB)
            otherwise ->  -- Caso tamano viejo distinto de 1, error
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
    0 -> insertObject id objId n' symT

  where
    n'    = getValue n
    objId = getStr obj
    em1 = "Error: no se puede colocar 0 objetos en basket, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2 = "Error: el identificador no existe, en la linea "
          ++ show l ++ " y columna " ++ show c     
    em3 = "Error: el identificador no es de un objeto, en la linea "
          ++ show l ++ " y columna " ++ show c              
    em4 = "Error: no hay suficiente espacio en basket, en la linea "
          ++ show l ++ " y columna " ++ show c   

insertWInst id (STARTAT (l,c) col row dir) = do
  (MySymState symT stck err nB ) <- get
  case validStart id (col',row') symT of
    1 -> put (MySymState symT stck (em1:err) nB)
    2 -> put (MySymState symT stck (em1:err) nB)
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
  case existId gId' symT stck isGoal of 
    True  -> put (MySymState symT stck (em1:err) nB)
    False -> 
      case notExistId gId' symT stck of
        False -> put (MySymState symT stck (em2:err) nB)
        True  -> 
          case validTest id gTest symT stck of
            1 -> put (MySymState symT stck (em3:err) nB)
            2 -> put (MySymState symT stck (em4:err) nB)
            3 -> put (MySymState symT stck (em5:err) nB)
            4 -> put (MySymState symT stck (em6:err) nB)
            0 -> do
              let val = (Goal (l,c) gId' nB gTest)
              insToTable gId' val
  where 
    gId' = getStr gId
    em1  = "Error: ya existe un Goal con el id dado, en la linea "
          ++ show l ++ " y columna " ++ show c
    em2  = "Error: ya existe un tipo de dato con el id dado, en la linea "
          ++ show l ++ " y columna " ++ show c
    em3  = "Error: la columna o fila 0 no es valida, en la linea "
          ++ show l ++ " y columna " ++ show c
    em4  = "Error: la casilla dada se sale de los limites del mundo, en la linea "
          ++ show l ++ " y columna " ++ show c
    em5  = "Error: no existe ningun objeto con el id dado, en la linea "
          ++ show l ++ " y columna " ++ show c
    em6  = "Error: el id de objeto dado pertenece a otro tipo, en la linea "
          ++ show l ++ " y columna " ++ show c


insertWInst id (BOOLEAN (l,c) boolId boolValue) = do
  (MySymState symT stck err nB ) <- get
  case not (notExistId boolId' symT stck) || (id==boolId') of
    True      -> put(MySymState symT stck (em:err) nB)
    otherwise -> do
      let val = WBoolean (l,c) boolId' nB (getBool boolValue)
      insToTable boolId' val

  where
    boolId' = getStr boolId
    em = "Error: redefinicion de " ++ boolId' ++ " en la linea "
          ++ show l ++ " y columna " ++ show c
          

insertWInst id (FINALIS _ fGoal) = do
  validFinalGoal fGoal


validFinalGoal :: FINALGOAL -> MyStateM()
validFinalGoal (FGAND (l,c) left right) = do
  validFinalGoal left
  validFinalGoal right
validFinalGoal (FGOR (l,c) left right) = do
  validFinalGoal left
  validFinalGoal right
validFinalGoal (FGNOT (l,c) exp) = do
  validFinalGoal exp
validFinalGoal (FGTOF (l,c) val) = return()
validFinalGoal (FGID (l,c) id) = do
  (MySymState symT stck err nB ) <- get
  case notExistId id' symT stck of
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


-- FUNCIONES GENERALES----------------------------

existsWoTId :: SymTable -> String -> Bool
existsWoTId sT id = 
  case Hash.lookup id sT of
    Nothing   -> False
    Just xs   -> existsWoTId' xs 

  where
    existsWoTId' []     = False
    existsWoTId' (x:xs) = 
      case isWorldOrTask x of
        True  -> True
        False -> existsWoTId' xs

    isWorldOrTask (World _ _ _ _ _ _ _ _ _ _) = True
    isWorldOrTask (Task _ _ _ _)              = True
    isWorldOrTask _                           = False 


insertWorld :: Pos -> String -> MyStateM ()
insertWorld p id = do
  (MySymState symT (st:sts) err nB ) <- get
  let val      = World p id st (nB+1) Hash.empty (1,1) 1 [] (1,1) "north"
  insToTable id val
  pushScope


insToTable :: String -> SymValue -> MyStateM ()
insToTable id val = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id symT of
    Nothing -> put(MySymState (Hash.insert id [val] symT) stck err nB)
    Just xs -> put(MySymState (Hash.insert id (val:xs) symT) stck err nB)


pushScope :: MyStateM ()
pushScope = do
  (MySymState symT stck err nB ) <- get
  put(MySymState symT ((nB+1):stck) err (nB+1))

popScope :: MyStateM ()
popScope = do
  (MySymState symT (x:xs) err nB ) <- get
  put(MySymState symT xs err nB)

updWorldSize :: String -> Pos -> MyStateM ()
updWorldSize id (c,r) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id symT of
    Nothing -> return() --Este nunca pasa
    Just ((World p id dB nB h w b l _ d):xs) -> do
      let val = (World p id dB nB h w b l (c,r) d)
      put(MySymState (Hash.insert id (val:xs) symT) stck err nB)
      
getWSize :: String -> SymTable -> Pos
getWSize id symT = 
  case Hash.lookup id symT of
    Just ((World p id dB nB h w c l size _):xs) -> size

isUsedWId :: String -> SymTable -> [Int] -> Bool
isUsedWId objId symT []     = False
isUsedWId objId symT (x:[]) = False 
isUsedWId objId symT (x:xs) = 
  case Hash.lookup objId symT of
    Nothing -> isUsedWId objId symT xs
    Just ys -> 
      case objIdBelongs x ys of
        False -> isUsedWId objId symT xs
        True  -> True 

objIdBelongs :: Int -> [SymValue] -> Bool
objIdBelongs _ []                        = False
objIdBelongs y ((ObjectType _ _ x _):xs) = y == x
objIdBelongs y ((WBoolean _ _ x _):xs)   = y == x
objIdBelongs y ((Goal _ _ x _):xs)       = y == x
objIdBelongs _ _                         = False

checkWall :: Int -> Int -> Int -> Int -> String -> String -> SymTable -> Int
checkWall x1 y1 x2 y2 dir worldId symT
  | x1 > xlim || x2 > xlim                          = 1
  | y1 > ylim || y2 > ylim                          = 1
  | dir == "north" && (x1 /= x2 || y2 > y1)         = 2
  | dir == "south" && (x1 /= x2 || y2 < y1)         = 2
  | dir == "east"  && (x1 > x2 || y2 /= y1)         = 2
  | dir == "west"  && (x1 < x2 || y2 /= y1)         = 2
  | not $ clearForWall x1 y1 x2 y2 dir worldId symT = 3
  | otherwise                                       = 0
  where (xlim,ylim) = getWSize worldId symT


clearForWall :: Int -> Int -> Int -> Int -> String -> String -> SymTable -> Bool
clearForWall x y finalx finaly dir worldId symT
  | not $ emptyCell x y worldId symT = False
  | x == finalx && y == finaly       = True
  | dir == "north"         = clearForWall x (y-1) finalx finaly dir worldId symT
  | dir == "south"         = clearForWall x (y+1) finalx finaly dir worldId symT
  | dir == "east"          = clearForWall (x+1) y finalx finaly dir worldId symT
  | dir == "west"          = clearForWall (x-1) y finalx finaly dir worldId symT

emptyCell :: Int -> Int -> String -> SymTable -> Bool
emptyCell x y worldId symT = case Hash.lookup worldId symT of
  Just ((World _ _ _ _ h p _ _ _ _ ):xs) -> case Hash.lookup (x,y) h of
    Just (Objects _) -> False
    _          -> (x,y) /= p -- Si no es un objeto y no es willy ret true
  _ -> False

updWorldWall :: String -> Int -> Int -> Int -> Int -> String -> MyStateM ()
updWorldWall worldId x1 y1 x2 y2 dir = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup worldId symT of
    Just ((World p id dB nB h w b l po d):xs) -> do
      let val = (World p id dB nB (Hash.insert (x1,y1) (Wall) h) w b l po d)
      put (MySymState (Hash.insert worldId (val:xs) symT) stck err nB)
      if (x1==x2 && y1==y2) then do return ()
      else if (dir == "north") then updWorldWall worldId x1 (y1-1) x2 y2 dir
      else if (dir == "south") then updWorldWall worldId x1 (y1+1) x2 y2 dir
      else if (dir == "east" ) then updWorldWall worldId (x1+1) y1 x2 y2 dir
      else                          updWorldWall worldId (x1-1) y1 x2 y2 dir
    _                                        -> return ()


checkPlaceAt :: String -> String -> Pos -> SymTable -> [Int] -> Int
checkPlaceAt worldId objId (col,row) symT scope
  | not $ isUsedWId objId symT scope           = 1 -- El id no existe
  | not $ isObjId   objId symT scope           = 2 -- El id no es de un objeto
  | colLim < col || rowLim < row               = 3 -- La casilla no entra en el mundo
  | not $ cellWithoutWall col row worldId symT = 4 -- La casilla esta ocupada por pared
  | col*row == 0                               = 5 -- no se puede colocar en 0
  | otherwise                                  = 0 -- No hay problema
  where (colLim,rowLim) = getWSize worldId symT

  

isObjId :: String -> SymTable -> [Int] -> Bool
isObjId objId symT []     = False
isObjId objId symT (x:[]) = False 
isObjId objId symT (x:xs) = 
  case Hash.lookup objId symT of
    Nothing -> isObjId objId symT xs
    Just ys -> 
      case isObjId' x ys of
        False -> isObjId objId symT xs
        True  -> True 

isObjId' :: Int -> [SymValue] -> Bool
isObjId' _ []                        = False
isObjId' y ((ObjectType _ _ x _):xs) = y == x
isObjId' _ _                         = False

cellWithoutWall :: Int -> Int -> String -> SymTable -> Bool
cellWithoutWall x y worldId symT = case Hash.lookup worldId symT of
  Just ((World _ _ _ _ h p _ _ _ _ ):xs) -> 
    case Hash.lookup (x,y) h of
      Just Wall -> False
      _         -> True -- Si no es una pared ret true
  _ -> False


placeObject :: String -> String -> Int -> Pos -> MyStateM()
placeObject worldId objId n (c,r) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup worldId symT of
    Just ((World pos id defB numB desc w baskS objInB siz willyDir):xs) -> 
      case Hash.lookup (c,r) desc of
        Just (Objects map) -> 
          case Hash.lookup objId map of
            Just m -> do 
              io $ print "hola1"
              let v  = Hash.insert objId (m+n) map
              let v' = Hash.insert (c,r) (Objects v) desc 
              let val = World pos id defB numB v' w baskS objInB siz willyDir
              put(MySymState (Hash.insert worldId (val:xs) symT) stck err nB)
            Nothing -> do
              io $ print "hola2"
              let v  = Hash.insert objId n map
              let v' = Hash.insert (c,r) (Objects v) desc 
              let val = World pos id defB numB v' w baskS objInB siz willyDir
              put(MySymState (Hash.insert worldId (val:xs) symT) stck err nB)
        Nothing -> do
          let v  = Hash.insert objId n Hash.empty
          let v' = Hash.insert (c,r) (Objects v) desc 
          let val = World pos id defB numB v' w baskS objInB siz willyDir
          put(MySymState (Hash.insert worldId (val:xs) symT) stck err nB)

checkPlaceIn :: String -> String -> Int -> SymTable -> [Int] -> Int
checkPlaceIn worldId objId n symT scope
  | n == 0                                     = 1
  | not $ isUsedWId objId symT scope           = 2 -- El id no existe
  | not $ isObjId   objId symT scope           = 3 -- El id no es de un objeto
  | basketCap worldId symT < n                 = 4 -- No hay suf espacio en basket
  | otherwise                                  = 0

basketCap :: String -> SymTable -> Int
basketCap wId symT =
  case Hash.lookup wId symT of 
    Just ((World pos id defB numB desc w baskS objInB siz willyDir):xs) -> 
      baskS - (length objInB)
    otherwise -> 0 -- Este nunca pasa

insertObject :: String -> String -> Int -> SymTable -> MyStateM()
insertObject wId oId n symT = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup wId symT of
    Just ((World pos id defB numB desc w baskS objInB siz willyDir):xs) -> do
      let newObjInB = (replicate n oId) ++ objInB
      let val = (World pos id defB numB desc w baskS newObjInB siz willyDir)
      put(MySymState (Hash.insert wId (val:xs) symT) stck err nB)
    Nothing -> return() -- Nunca pasa

validStart :: String -> Pos -> SymTable -> Int
validStart wId (col,row) symT 
  | col*row == 0                           = 1 -- col o row es 0
  | colLim < col || rowLim < row           = 2 -- La pos no esta en el mundo
  | not $ cellWithoutWall col row wId symT = 3 -- Hay una pared
  | otherwise                              = 0
  where (colLim,rowLim) = getWSize wId symT

updStartPos :: String -> Pos -> String -> MyStateM()
updStartPos wId pos dir = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup wId symT of
    Just ((World pos' id defB numB desc w baskS objInB siz willyDir):xs) -> do
      let val = (World pos' id defB numB desc pos baskS objInB siz dir)
      put(MySymState (Hash.insert wId (val:xs) symT) stck err nB)
    Nothing -> return() -- Nunca pasa


validTest :: String -> GOALTEST -> SymTable -> [Int] -> Int
validTest wId (WILLYISAT (l,c) col row) symT stck
  | col'*row' == 0                         = 1
  | colLim < col' || rowLim < row'         = 2
  | otherwise                              = 0
  where
    col' = getValue col
    row' = getValue row
    (colLim,rowLim) = getWSize wId symT

validTest wId (OBJECTSIN (l,c) n oId) symT stck
  | notExistId oId' symT stck             = 3
  | not $ existId oId' symT stck isObject = 4
  | otherwise                             = 0
  where 
    n'   = getValue n
    oId' = getStr oId

validTest wId (OBJECTSAT (l,c) n oId col row) symT stck
  | col'*row' == 0                        = 1
  | colLim < col' || rowLim < row'        = 2
  | notExistId oId' symT stck             = 3
  | not $ existId oId' symT stck isObject = 4
  | otherwise                             = 0
  where
    col'            = getValue col
    row'            = getValue row
    (colLim,rowLim) = getWSize wId symT
    n'              = getValue n
    oId'            = getStr oId



-- id que tal, tabla, stack de scopes, funcion para filtrar
existId :: String -> SymTable -> [Int] -> (SymValue -> Bool) -> Bool -- Para llamadas
existId id symT [] _ = False
existId id symT (scope:scopes) isX = 
  case Hash.lookup id symT of
    Nothing -> False  -- Si no esta en la Symbol table no lo puedo llamar
    Just listOfValues ->        -- lista de valores que se pueden llamar con el ID en el programa en general
      case scopeBelongs scope $ filter isX listOfValues of -- quiero checkear si el scope esta en la lista
        False -> existId id symT scopes isX -- si no esta pruebo con el siguiente scope
        True  -> True 

scopeBelongs :: Int -> [SymValue] -> Bool  -- Recorro la lista checkeando
scopeBelongs scope [] = False
scopeBelongs scope (val:vals)
  | defBlock val == scope = True     -- los SymValue que busques con isX deben tener atributo defB
  | otherwise             = scopeBelongs scope vals


-- id que tal, tabla, stack de scopes
notExistId :: String -> SymTable -> [Int] -> Bool -- Para declaraciones 
notExistId id symT []     = True
notExistId id symT scopeStack@(scope:scopes) = 
  case Hash.lookup id symT of
    Nothing -> True  -- Si no esta en la Symbol table lo puedo usar para declarar
    Just listOfValues ->  usableIDforDeclare scope wScope listOfValues
  where wScope = scopeStack !! (length scopeStack - 2)

usableIDforDeclare :: Int -> Int -> [SymValue] -> Bool  -- Recorro la lista checkeando
usableIDforDeclare currentScope worldScope [] = True
usableIDforDeclare currentScope worldScope (val:vals)
  | defBlock val == currentScope                = False
  | isBoolean val && defBlock val == worldScope = False
  | isObject val  && defBlock val == worldScope = False
  | otherwise = usableIDforDeclare currentScope worldScope vals


isGoal :: SymValue -> Bool
isGoal Goal{} = True
isGoal _      = False

isBoolean :: SymValue -> Bool
isBoolean WBoolean{} = True
isBoolean _          = False

isObject :: SymValue -> Bool
isObject ObjectType{} = True
isObject _            = False

isWorld :: SymValue -> Bool
isWorld World{} = True
isWorld _       = False

isInstruction :: SymValue -> Bool
isInstruction Instruction{} = True
isInstruction _             = False

isTask :: SymValue -> Bool
isTask Task{} = True
isTask _      = False

{- getOldWorld = filter isWorld listVal !! 0 -}
updateWListVal :: SymValue -> [SymValue] -> [SymValue]
updateWListVal newWorld (val:vals)
  | isWorld val = newWorld:vals
  | otherise    = val:updateWListVal newWorld vals