##### Jesus Wahrman 15-11540
##### Neil Villamizar 15-11523 

# Informe Fase 2

Para este proyecto utilizamos el lenguaje de programación funcional Haskell. En específico para esta fase usamos la herramienta "Happy", que recibe el archivo "Parser.y" y genera el "Parser.hs" que usamos en nuestro main "Willy.hs" para hacer el análisis sintáctico. El main primero llama al Lexer de la entrega anterior y la lista de tokens que esta devuelve es suministrada al Parser para realizar el correspondiente análisis sintáctico. Luego con el AST (Árbol abstracto de sintaxis) generado por el Parser se procede a realizar el chequeo de contexto usando una tabla de símbolos.


En el archivo Parser.y se define el nombre del parser (con este nombre se llama al parser desde el  
main), el tipo de los token a recibir (el definido en la fase previa) y el nombre del error. En este caso son respectivamente: parse, Token y parseError. 


Luego se definen las palabras asociadas a cada token de la entrega anterior. Estas se usaran en la definición de las gramáticas. Se definen de la siguiente forma:
palabra   { Token }


Posteriormente se definen las precedencias y asociatividades a usar en caso de ambigüedades. Esto ayuda a Happy a generar un parser de una gramática sin ambigüedades. En nuestro caso las precedencias usadas fueron:

%nonassoc then

%nonassoc else

%left and or
  
%nonassoc not

Las precedencias del "then" y del "else" ayudan a solucionar el problema ["dangling else ambiguity"](https://en.wikipedia.org/wiki/Dangling_else).


Posteriormente se definen todas las reglas de la gramática que genera el lenguaje Willy\*. Estas van creando el AST, usando el tipo de dato definido en el archivo AST.hs. Finalmente se define la función de error a usar por parte del parser. Las reglas son de la forma:

S : t1 t2 ... tk   { E }
 
Donde S es un símbolo no terminal y ti es un símbolo terminal o no terminal, y E es la expresión que se genera. Con estas expresiones se va creando nuestro AST. Se evitó usar recursiones a la derecha al definir las reglas de la gramática usada. 


En el main, luego de parsear, se llama al context checker, usando evalState, a esta se le pasa la función "createSymTable" y el estado inicial "initTableState". La primera toma como input el output del parser y realiza el chequeo de contexto usando una tabla de símbolos definida en "SymTable.hs" y el estado inicial "initTableState". 


La tabla de símbolos usada consta de un mapa (Data.Map de Haskell) de símbolos a listas de "symValue" o datos correspondientes al identificador. Cada lista tiene los datos de varios elementos del programa que usan el mismo identificador así como el identificador del scope donde fue declarado. Esto es posible ya que en "scopes" distintos se pueden repetir identificadores. Además cuenta con una Pila de scopes (de sus identificadores) para saber en cualquier momento que scopes están "activos". Es decir, en esos scopes activos pueden haber identificadores al alcance de los elementos en el scope actual. 


Va recorriendo el AST en preorder y va metiendo los símbolos declarados en el programa en Willy\* en la tabla de símbolos. Al hacer esto revisa que el identificador no haya sido usado previamente en el mismo scope. Además debe revisar que no haya sido usado en declaraciones de booleanos u objetos en el mundo correspondiente (si se está en una tarea, ya que estos son accesibles desde las tareas). Al mismo tiempo va verificando que las llamadas a identificadores correspondan con algún identificador que este previamente en la tabla que este en los scopes activos según la pila de scopes y que sea del tipo esperado por la llamada. 


Finalmente, si no hubo errores en ninguno de los procesos anteriores, se procede a imprimir por salida estándar siguiendo el formato de salida del proyecto. El cual consta de dos partes. En la primera se imprime una representación del AST (esto no toma en cuenta las declaraciones) que incluye los datos relacionados con cada nodo. Y la segunda parte del formato que imprime todos los símbolos contenidos en la tabla de símbolos y de nuevo todos los datos relacionados con dichos símbolos.

Nota: para correr el proyecto usar el comando "make", y luego el comando "willy \< Willy\* program file \>"
