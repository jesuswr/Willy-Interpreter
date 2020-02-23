
-- id que tal, tabla, stack de scopes, funcion para filtrar
existId :: String -> SymTable -> [Int] -> (Symvalue -> Bool) -> Bool -- Para llamadas
existId ID symT [] _ = False
existId ID symT (scope:scopes) isX = 
  case Hash.lookup ID symT of
    Nothing -> False  -- Si no esta en la Symbol table no lo puedo llamar
    Just listOfValues ->        -- lista de valores que se pueden llamar con el ID en el programa en general
      case scopeBelongs scope $ filter isX listOfValues of -- quiero checkear si el scope esta en la lista
        False -> existId ID symT scopes -- si no esta pruebo con el siguiente scope
        True  -> True 

scopeBelongs :: Int -> [SymValue] -> Bool  -- Recorro la lista checkeando
scopeBelongs scope [] = False
scopeBelongs scope (val:vals)
	| defB val == scope = True     -- los Symvalue que busques con isX deben tener atributo defB
	| otherwise             = scopeBelongs scope vals


------------------------------------------------------------------------------------------------------
--///////////////////////////////////////////////////////////////////////////////////////////////////
------------------------------------------------------------------------------------------------------

-- id que tal, tabla, stack de scopes
notExistId :: String -> SymTable -> [Int] -> Bool -- Para declaraciones 
notExistId ID symT []     = True
notExistId ID symT scopeStack@(scope:scopes) = 
  case Hash.lookup ID symT of
    Nothing -> True  -- Si no esta en la Symbol table lo puedo usar para declarar
    Just listOfValues ->  usableIDforDeclare scope wScope listOfValues
  where wScope = scopeStack !! (length scopeStack - 2)

usableIDforDeclare :: Int -> Int -> [SymValue] -> Bool  -- Recorro la lista checkeando
usableIDforDeclare currentScope worldScope [] = True
usableIDforDeclare currentScope worldScope (val:vals)
	| defB val == currentScope                = False
	| isBoolean val && defB val == worldScope = False
	| isObject val  && defB val == worldScope = False
	| otherwise = usableIDforDeclare currentScope worldScope vals


--///////////////////////////////////////////////////////////////////////////


insertWInst id (BOOLEAN (l,c) boolId boolValue) = do
  (MySymState symT stck err nB ) <- get
  case (isUsedWId boolId' symT stck) || (id==boolId') of
    True      -> put(MySymState symT stck (em:err) nB)
    otherwise -> do
      let val = WBoolean (l,c) boolId' nB (show boolValue)
      insToTable boolId' val

  where
    boolId' = getStr boolId
    em = "Error: redefinicion de " ++ boolId' ++ " en la linea "
          ++ show l ++ " y columna " ++ show c



