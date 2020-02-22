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
      return()
    False -> do -- No existe
      insertWorld (l,c) s
      (MySymState symT stck err nB ) <- get
      return()

    where
      em    = "Error: redefinicion de " ++ s ++ " en la linea "
              ++ show l ++ " y columna " ++ show c


















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

    isWorldOrTask (World _ _ _ _ _ _ _ _) = True
    isWorldOrTask (Task _ _ _ _)          = True
    isWorldOrTask _                       = False 


insertWorld :: (Int,Int) -> String -> MyStateM ()
insertWorld p id = do
  (MySymState symT stck err nB ) <- get
  let val      = World p id nB (nB+1) Hash.empty (1,1) 1 []
  insToTable id val
  pushScope


insToTable :: String -> SymValue -> MyStateM ()
insToTable id val = do
  (MySymState symT stck err nB ) <- get
  case Hash.lookup id symT of
    Nothing -> put(MySymState (Hash.insert id [val] symT) ((nB+1):stck) err (nB+1))
    Just xs -> put(MySymState (Hash.insert id (val:xs) symT) ((nB+1):stck) err (nB+1))


pushScope :: MyStateM ()
pushScope = do
  (MySymState symT stck err nB ) <- get
  put(MySymState symT ((nB+1):stck) err (nB+1))