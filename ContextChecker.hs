module ContextChecker where

import AST

-- Function to traverse a  sequence of blocks
dfsCheckBLOCKS :: [BLOCK] -> IO ()
dfsCheckBLOCKS []             = return ()
dfsCheckBLOCKS (block:blocks) = do
    dfsCheckBLOCK block
    dfsCheckBLOCKS blocks

-- Function to traverse block
dfsCheckBLOCK :: BLOCK -> IO ()
dfsCheckBLOCK world@(WORLD (x,y) worldId worldInsts) = do
    putStrLn " "
    putStrLn $ "Enter world block: " ++ (show world)
    dfsCheckINSTRS worldInsts
    putStrLn "Exit block."
    putStrLn " "
dfsCheckBLOCK task@(TASK (x,y) taskId onWorld taskInstrs ) = do
    putStrLn " "
    putStrLn $ "Enter task block: " ++ (show task)
    dfsCheckTASKINSTRS taskInstrs
    putStrLn "Exit block."
    putStrLn " "


dfsCheckINSTRS :: [INSTR] -> IO ()
dfsCheckINSTRS []             = return ()
dfsCheckINSTRS (instr:instrs) = do
    dfsCheckINSTR instr
    dfsCheckINSTRS instrs


dfsCheckINSTR :: INSTR -> IO ()
dfsCheckINSTR inst@(WORLDSIZE (x,y) c r) = do
    putStrLn " "
    putStrLn $ "world size instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(WALL (x,y) dir x1 y1 x2 y2) = do
    putStrLn " "
    putStrLn $ "Wall instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(OBJECTTYPE (x,y) id color) = do
    putStrLn " "
    putStrLn $ "Object-type instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(PLACEAT (x,y) n id c r) = do
    putStrLn " "
    putStrLn $ "PLace at instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(PLACEIN (x,y) n id) = do
    putStrLn " "
    putStrLn $ "Place in instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(STARTAT (x,y) c r dir) = do
    putStrLn " "
    putStrLn $ "Start at instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(BASKET (x,y) cap) = do
    putStrLn " "
    putStrLn $ "Basket capacity instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(BOOLEAN (x,y) id val) = do
    putStrLn " "
    putStrLn $ "Boolean instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckINSTR inst@(GOALIS (x,y) id goalTest) = do
    putStrLn " "
    putStrLn $ "Goal-is instruccion: :" ++ (show inst) ++ "Enter GOALTEST instruccion: " 
    dfsCheckGOALTEST goalTest
    putStrLn "Exit GOALTEST."
    putStrLn " "
dfsCheckINSTR inst@(FINALIS (x,y) finalGoal) = do
    putStrLn " "
    putStrLn $ "Final-is instruccion: :" ++ (show inst) ++ "Enter FINALGOAL instruccion: " 
    dfsCheckFINALGOAL finalGoal
    putStrLn "Exit FINALGOAL."
    putStrLn " "


dfsCheckGOALTEST :: GOALTEST -> IO ()
dfsCheckGOALTEST cond@(WILLYISAT (x,y) c r) = do
    putStrLn " "
    putStrLn $ "Wlly_is_at condicion: " ++ (show cond)
    putStrLn " "
dfsCheckGOALTEST cond@(OBJECTSIN (x,y) n id) = do
    putStrLn " "
    putStrLn $ "Objects_in condicion: " ++ (show cond)
    putStrLn " "
dfsCheckGOALTEST cond@(OBJECTSAT (x,y) n id c r) = do
    putStrLn " "
    putStrLn $ "Objects_at condicion: " ++ (show cond)
    putStrLn " "


dfsCheckFINALGOAL :: FINALGOAL -> IO ()
dfsCheckFINALGOAL cond@(FGAND (x,y) finalGoal0 finalGoal1) = do
    putStrLn " "
    putStrLn "Enter FINALGOAL condicion: " 
    dfsCheckFINALGOAL finalGoal0
    putStrLn "Exit FINALGOAL."
    putStrLn $ " AND " ++ (show cond) 
    putStrLn "Enter FINALGOAL condicion: " 
    dfsCheckFINALGOAL finalGoal1
    putStrLn "Exit FINALGOAL."
    putStrLn " "
dfsCheckFINALGOAL cond@(FGOR (x,y) finalGoal0 finalGoal1) = do
    putStrLn " "
    putStrLn "Enter FINALGOAL condicion: " 
    dfsCheckFINALGOAL finalGoal0
    putStrLn "Exit FINALGOAL."
    putStrLn $ " OR " ++ (show cond) 
    putStrLn "Enter FINALGOAL condicion: " 
    dfsCheckFINALGOAL finalGoal1
    putStrLn "Exit FINALGOAL."
    putStrLn " "
dfsCheckFINALGOAL cond@(FGNOT (x,y) finalGoal) = do
    putStrLn " "
    putStrLn " NOT "
    putStrLn $ "Enter FINALGOAL condicion: " ++ (show cond) 
    dfsCheckFINALGOAL finalGoal
    putStrLn "Exit FINALGOAL."
    putStrLn " "
dfsCheckFINALGOAL cond@(FGID (x,y) id) = do
    putStrLn " "
    putStrLn $ "FG-ID condicion: " ++ (show cond) 
    putStrLn " "
dfsCheckFINALGOAL cond@(FGTOF (x,y) val) = do
    putStrLn " "
    putStrLn $ "FG-ToF condicion: " ++ (show cond) 
    putStrLn " "


dfsCheckTASKINSTRS :: [TASKINSTR] -> IO ()
dfsCheckTASKINSTRS []             = return ()
dfsCheckTASKINSTRS (instr:instrs) = do
    dfsCheckTASKINSTR instr
    dfsCheckTASKINSTRS instrs


dfsCheckTASKINSTR :: TASKINSTR -> IO ()
dfsCheckTASKINSTR inst@(IF (x,y) test tInst) = do
    putStrLn " "
    putStrLn $ "Enter IF instruccion: " ++ (show inst)
    putStrLn " if cond:"
    dfsCheckTEST test
    dfsCheckTASKINSTR tInst
    putStrLn "Exit IF"
    putStrLn " "
dfsCheckTASKINSTR inst@(IFELSE (x,y) test tInst0 tInst1) = do
    putStrLn " "
    putStrLn $ "Enter IFELSE instruccion: " ++ (show inst)
    putStrLn " if cond: "
    dfsCheckTEST test
    putStrLn " then: "
    dfsCheckTASKINSTR tInst0
    putStrLn " else: "
    dfsCheckTASKINSTR tInst1
    putStrLn "Exit IF"
    putStrLn " "
dfsCheckTASKINSTR inst@(REPEAT (x,y) n tInst) = do
    putStrLn " "
    putStrLn $ "Enter REPEAT instruccion: " ++ (show inst)
    dfsCheckTASKINSTR tInst
    putStrLn "Exit REPEAT"
    putStrLn " "
dfsCheckTASKINSTR inst@(WHILE (x,y) test tInst) = do
    putStrLn " "
    putStrLn $ "Enter WHILE instruccion: " ++ (show inst)
    putStrLn " WHILE cond:"
    dfsCheckTEST test
    dfsCheckTASKINSTR tInst
    putStrLn "Exit WHILE"
    putStrLn " "
dfsCheckTASKINSTR inst@(BEGIN (x,y) tInsts) = do
    putStrLn " "
    putStrLn $ "Enter BEGIN instruccion: " ++ (show inst)
    dfsCheckTASKINSTRS tInsts
    putStrLn "Exit BEGIN (end)"
    putStrLn " "
dfsCheckTASKINSTR inst@(DEFINE (x,y) id tInst) = do
    putStrLn " "
    putStrLn $ "Enter DEFINE instruccion: " ++ (show inst)
    dfsCheckTASKINSTR tInst
    putStrLn "Exit DEFINE"
    putStrLn " "
dfsCheckTASKINSTR inst@(MOVE (x,y) ) = do
    putStrLn " "
    putStrLn $ "MOVE instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(TURNLEFT (x,y) ) = do
    putStrLn " "
    putStrLn $ "TURNLEFT instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(TURNRIGHT (x,y) ) = do
    putStrLn " "
    putStrLn $ "TURNRIGHT instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(PICK (x,y) id) = do
    putStrLn " "
    putStrLn $ "PICK instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(DROP (x,y) id) = do
    putStrLn " "
    putStrLn $ "DROP instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(SET (x,y) id) = do
    putStrLn " "
    putStrLn $ "SET instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(SETTO (x,y) id tof) = do
    putStrLn " "
    putStrLn $ "SETTO instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(CLEAR (x,y) id) = do
    putStrLn " "
    putStrLn $ "CLEAR instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(FLIP (x,y) id) = do
    putStrLn " "
    putStrLn $ "FLIP instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(TERMINATE (x,y) ) = do
    putStrLn " "
    putStrLn $ "TERMINATE instruccion: " ++ (show inst)
    putStrLn " "
dfsCheckTASKINSTR inst@(INSTRID (x,y) id) = do
    putStrLn " "
    putStrLn $ "INSTRID instruccion: " ++ (show inst)
    putStrLn " "


dfsCheckTEST :: TEST -> IO ()
dfsCheckTEST cond@(TESTAND (x,y) test0 test1) = do
    putStrLn " "
    putStrLn "Enter TEST condicion: " 
    dfsCheckTEST test0
    putStrLn "Exit TEST."
    putStrLn $ " AND "  ++ (show cond) 
    putStrLn "Enter TEST condicion: " 
    dfsCheckTEST test1
    putStrLn "Exit TEST."
    putStrLn " "
dfsCheckTEST cond@(TESTOR (x,y) test0 test1) = do
    putStrLn " "
    putStrLn "Enter TEST condicion: " 
    dfsCheckTEST test0
    putStrLn "Exit TEST."
    putStrLn $ " OR "  ++ (show cond) 
    putStrLn "Enter TEST condicion: " 
    dfsCheckTEST test1
    putStrLn "Exit TEST."
    putStrLn " "
dfsCheckTEST cond@(TESTNOT (x,y) test) = do
    putStrLn " "
    putStrLn " NOT "
    putStrLn $ "Enter TESTNOT condicion: " ++ (show cond) 
    dfsCheckTEST test
    putStrLn "Exit TESTNOT."
    putStrLn " "
dfsCheckTEST cond@(TESTID (x,y) id) = do
    putStrLn " "
    putStrLn $ "TESTID condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(FRONTCLEAR (x,y) ) = do
    putStrLn " "
    putStrLn $ "FRONTCLEAR condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(LEFTCLEAR (x,y) ) = do
    putStrLn " "
    putStrLn $ "LEFTCLEAR condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(RIGHTCLEAR (x,y) ) = do
    putStrLn " "
    putStrLn $ "RIGHTCLEAR condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(LOOKNORTH (x,y) ) = do
    putStrLn " "
    putStrLn $ "LOOKNORTH condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(LOOKEAST (x,y) ) = do
    putStrLn " "
    putStrLn $ "LOOKEAST condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(LOOKSOUTH (x,y) ) = do
    putStrLn " "
    putStrLn $ "LOOKSOUTH condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(LOOKWEST (x,y) ) = do
    putStrLn " "
    putStrLn $ "LOOKWEST condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(FOUND (x,y) id) = do
    putStrLn " "
    putStrLn $ "FOUND condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(CARRYING (x,y) id) = do
    putStrLn " "
    putStrLn $ "CARRYING condicion: " ++ (show cond)
    putStrLn " "
dfsCheckTEST cond@(TESTTOF (x,y) val) = do
    putStrLn " "
    putStrLn $ "TESTTOF condicion: " ++ (show cond)
    putStrLn " "
