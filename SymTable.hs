module SymTable where

import AST
import Lexer
import Control.Monad.State
import qualified Data.Map as Hash

type Pos = (Int,Int)

type WorldDesc = Hash.Map Pos WorldElements 

data WorldElements = Wall | Object{ objId :: String } deriving(Show)

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
  case (isUsedWId oId' symT stck) || (id==oId') of
    True      -> put(MySymState symT stck (em:err) nB)
    otherwise -> do
      let val = ObjectType (l,c) oId' nB (show color)
      insToTable oId' val

  where
    em = "Error: redefinicion de " ++ (getStr oId) ++ " en la linea "
          ++ show l ++ " y columna " ++ show c
    oId' = getStr oId















-- FUNCIONES GENERALES

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


insertWorld :: (Int,Int) -> String -> MyStateM ()
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

updWorldSize :: String -> (Int,Int) -> MyStateM ()
updWorldSize id (c,r) = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id symT of
    Nothing -> return() --Este nunca pasa
    Just ((World p id dB nB h w b l _ d):xs) -> do
      let val = (World p id dB nB h w b l (c,r) d)
      put(MySymState (Hash.insert id (val:xs) symT) stck err nB)
      
getWSize :: String -> SymTable -> (Int,Int)
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
    Just (Object s) -> False
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

