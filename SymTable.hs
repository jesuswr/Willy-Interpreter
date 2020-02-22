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
                     , size :: Pos
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
    []  -> Left $ unlines $ reverse err
    str -> Right symT
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

    isWorldOrTask (World _ _ _ _ _ _ _ _ _) = True
    isWorldOrTask (Task _ _ _ _)            = True
    isWorldOrTask _                         = False 


insertWorld :: (Int,Int) -> String -> MyStateM ()
insertWorld p id = do
  (MySymState symT (st:sts) err nB ) <- get
  let val      = World p id st (nB+1) Hash.empty (1,1) 1 [] (1,1)
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
    Just ((World p id dB nB h w b l _):xs) -> do
      let val = (World p id dB nB h w b l (c,r))
      put(MySymState (Hash.insert id (val:xs) symT) stck err nB)
      
getWSize :: String -> SymTable -> (Int,Int)
getWSize id symT = 
  case Hash.lookup id symT of
    Just ((World p id dB nB h w c l size):xs) -> size

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
