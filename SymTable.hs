module SymTable where

import AST
import Lexer
import Control.Monad.State
import qualified Data.Map as Hash

type Pos = (Int,Int)

-- Data type to save the elements of the world, walls or objects
type WorldDesc = Hash.Map Pos WorldElements 

-- Data type to save the objects in a cell, because you can have more than
-- one type of object
type ObjectsInCell = Hash.Map String Int

-- Data type to represent the elements of the world, can be a wall or objects
data WorldElements = Wall | Objects{ map :: ObjectsInCell} deriving(Show)

-- Data type to represent the final goal of the world
data FinalGoal = None | FinalG{ goal :: FINALGOAL } deriving(Show)

-- Data type to represent the values that the table can contain
data SymValue = World{ startPos :: Pos , id :: String 
                     , defBlock :: Int , numBlock :: Int  
                     , desc :: WorldDesc , willyIsAt :: Pos 
                     , basketSize :: Int , objectsInB :: [String]
                     , size :: Pos , willyDirection :: String
                     , finalG :: FinalGoal
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
                           , defBlock :: Int , numBlock :: Int , inst :: TASKINSTR
                           }
              | Task{ startPos :: Pos , id :: String 
                    , defBlock :: Int , numBlock :: Int, onWorld :: String 
                    }
              deriving(Show)

-- Data type of the table, a hash from String to a list of SymValues
type SymTable = Hash.Map String [SymValue]

-- Data type to create the symbols table, using a LeBlanc-Cook implementation
data MySymState = MySymState{ symTable :: SymTable , stack :: [Int] , error :: [String] 
                            , nBlock :: Int }

type MyStateM a = State MySymState a