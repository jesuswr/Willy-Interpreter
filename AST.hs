module AST where
import Lexer

data BLOCK = WORLD{ worldStartPos :: Token , worldID :: Token , worldInstrs :: [INSTR] } deriving(Show)


data INSTR = WORLDSIZE{ instrStartPos :: Token , instrColumns :: Token , instrRows :: Token }
           | WALL{ instrStartPos :: Token , instrDirection :: Token 
                 , instrFirstX :: Token , instrFirstY :: Token 
                 , instrSecondX :: Token , instrSecondY :: Token 
                 }
           | OBJECTTYPE{ instrStartPos :: Token , instrId :: Token , instrColor :: Token }
           | PLACEAT{ instrStartPos :: Token , instrAmount :: Token , instrId :: Token 
                    , instrColumn :: Token , instrRow :: Token 
                    }
           | PLACEIN{ instrStartPos :: Token , instrAmount :: Token , instrId :: Token }
           | STARTAT{ instrStartPos :: Token , instrColumn :: Token , instrRow :: Token 
                    , instrDirection :: Token 
                    }
           | BASKET{ instrStartPos :: Token , instrCapacity :: Token }
           | BOOLEAN{ instrStartPos :: Token , instrId :: Token , instrValue :: Token }
           | GOALIS{ instrStartPos :: Token , instrId :: Token , instrCondition :: GOALTEST }
           | FINALIS{ instrStartPos :: Token , instrFinalGoal :: FINALGOAL }
           deriving(Show)

data GOALTEST = WILLYISAT{ gtStartPos :: Token , gtColumn :: Token , gtRow :: Token }
              | OBJECTSIN{ gtStartPos :: Token , gtAmount :: Token , gtId :: Token }
              | OBJECTSAT{ gtStartPos :: Token , gtAmount :: Token , gtId :: Token 
                         , gtColumn :: Token , gtRow :: Token
                         }
              deriving(Show)


data FINALGOAL = FGAND{ fgStartPos :: Token , fgLeftExp :: FINALGOAL 
                      , fgRightExp :: FINALGOAL 
                      }
               | FGOR{ fgStartPos :: Token , fgLeftExp :: FINALGOAL 
                      , fgRightExp :: FINALGOAL 
                      }
               | FGNOT{ fgStartPos :: Token , fgExp :: FINALGOAL }
               | FGID{ fgStartPos :: Token , fgId :: Token }
               deriving(Show)
