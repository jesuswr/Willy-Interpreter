module AST where
import Lexer

data BLOCK = WORLD{ worldStartPos :: (Int,Int) , worldID :: Token , worldInstrs :: [INSTR] } 
           | TASK{ taskStartPos :: (Int,Int) , taskID :: Token , onW :: Token  
                  , taskInstrs :: [TASKINSTR] }
           deriving(Show)

data INSTR = WORLDSIZE{ instrStartPos :: (Int,Int) , instrColumns :: Token , instrRows :: Token }
           | WALL{ instrStartPos :: (Int,Int) , instrDirection :: Token 
                 , instrFirstX :: Token , instrFirstY :: Token 
                 , instrSecondX :: Token , instrSecondY :: Token 
                 }
           | OBJECTTYPE{ instrStartPos :: (Int,Int) , instrId :: Token , instrColor :: Token }
           | PLACEAT{ instrStartPos :: (Int,Int) , instrAmount :: Token , instrId :: Token 
                    , instrColumn :: Token , instrRow :: Token 
                    }
           | PLACEIN{ instrStartPos :: (Int,Int) , instrAmount :: Token , instrId :: Token }
           | STARTAT{ instrStartPos :: (Int,Int) , instrColumn :: Token , instrRow :: Token 
                    , instrDirection :: Token 
                    }
           | BASKET{ instrStartPos :: (Int,Int) , instrCapacity :: Token }
           | BOOLEAN{ instrStartPos :: (Int,Int) , instrId :: Token , instrValue :: Token }
           | GOALIS{ instrStartPos :: (Int,Int) , instrId :: Token , instrCondition :: GOALTEST }
           | FINALIS{ instrStartPos :: (Int,Int) , instrFinalGoal :: FINALGOAL }
           deriving(Show)

data GOALTEST = WILLYISAT{ gtStartPos :: (Int,Int) , gtColumn :: Token , gtRow :: Token }
              | OBJECTSIN{ gtStartPos :: (Int,Int) , gtAmount :: Token , gtId :: Token }
              | OBJECTSAT{ gtStartPos :: (Int,Int) , gtAmount :: Token , gtId :: Token 
                         , gtColumn :: Token , gtRow :: Token
                         }
              deriving(Show)


data FINALGOAL = FGAND{ fgStartPos :: (Int,Int) , fgLeftExp :: FINALGOAL 
                      , fgRightExp :: FINALGOAL 
                      }
               | FGOR{ fgStartPos :: (Int,Int) , fgLeftExp :: FINALGOAL 
                      , fgRightExp :: FINALGOAL 
                      }
               | FGNOT{ fgStartPos :: (Int,Int) , fgExp :: FINALGOAL }
               | FGID{ fgStartPos :: (Int,Int) , fgId :: Token }
               | FGTOF{ fgStartPos :: (Int,Int) , fgVal :: Token }
               deriving(Show)

data TASKINSTR = IF{ tiPos :: (Int,Int) , guard :: TEST , instr :: TASKINSTR }
               | IFELSE{ tiPos :: (Int,Int) , guard :: TEST , instr :: TASKINSTR 
                       , instr2 :: TASKINSTR 
                       }
               | REPEAT{ tiPos :: (Int,Int) , times :: Token , instr :: TASKINSTR  }
               | WHILE{ tiPos :: (Int,Int) , guard :: TEST , instr :: TASKINSTR  }
               | BEGIN{ tiPos :: (Int,Int) , instrs :: [TASKINSTR]  }
               | DEFINE{ tiPos :: (Int,Int) , tiId :: Token , instr :: TASKINSTR  }
               | MOVE{ tiPos :: (Int,Int) }
               | TURNLEFT{ tiPos :: (Int,Int) }
               | TURNRIGHT{ tiPos :: (Int,Int) }
               | PICK{ tiPos :: (Int,Int) , tiId :: Token }
               | DROP{ tiPos :: (Int,Int) , tiId :: Token }
               | SET{ tiPos :: (Int,Int) , tiId :: Token }
               | SETTO{ tiPos :: (Int,Int) , tiId :: Token , tof :: Token }
               | CLEAR{ tiPos :: (Int,Int) , tiId :: Token }
               | FLIP{ tiPos :: (Int,Int) , tiId :: Token }
               | TERMINATE{ tiPos :: (Int,Int) }
               | INSTRID{ tiPos :: (Int,Int) , tiId :: Token }
               deriving(Show)

data TEST = TESTAND{ testPos :: (Int,Int) , leftTest :: TEST , rightTest :: TEST }
          | TESTOR{ testPos :: (Int,Int) , leftTest :: TEST , rightTest :: TEST }
          | TESTNOT{ testPos :: (Int,Int) , testExpr :: TEST }
          | TESTID{ testPos :: (Int,Int) , testId :: Token }
          | FRONTCLEAR{ testPos :: (Int,Int) }
          | LEFTCLEAR{ testPos :: (Int,Int) }
          | RIGHTCLEAR{ testPos :: (Int,Int) }
          | LOOKNORTH{ testPos :: (Int,Int) }
          | LOOKEAST{ testPos :: (Int,Int) }
          | LOOKSOUTH{ testPos :: (Int,Int) }
          | LOOKWEST{ testPos :: (Int,Int) }
          | FOUND{ testPos :: (Int,Int) , testId :: Token }
          | CARRYING{ testPos :: (Int,Int) , testId :: Token }
          | TESTTOF{ testPos :: (Int,Int) , testVal :: Token }
          deriving(Show)
