##### Jesus Wahrman 15-11540  
##### Neil Villamizar 15-11523 

# Informe Fase 2

Para este proyecto utilizamos el lenguaje de programacion funcional Haskell. En especifico para esta fase usamos la herramienta "Happy", que recibe el archivo "Paser.y" y genera el "Parser.hs" que usamos en nuestro main "Willy.hs" para hacer el analisis sintáctico. El main primero llama al Lexer de la entrega anterior y la lista de tokens que esta devuelve es suministrada al Parser para realizar el correspondiente análisis sintáctico. Luego con el AST (Árbol abstracto de sintaxis) generado por el Parser se procede a realizar el chequeo de contexto usando una tabla de simbolos.

En el archivo Parser.y  se define el nombre del parser (con este nombre se llama al parser desde el  main), el tipo de los token a recibir (el definido en la fase previa) y el nombre del error. En este caso son respectivamente: parse, Token y parseError. 

Luego se definen las palabras asociadas a cada token de la entrega anterior. Estas se usaran en la definición de las gramaticas. Se definen de la siguiente forma:
palabra   { Token }

Posteriormente se definen las precedencias y asociatividades a usar en caso de ambigüedades. Esto ayuda a Happy a generar un parser de una gramatica sin ambigüedades. En nuestro caso las precedencias usadas fueron:
%nonassoc then
%nonassoc else
%left and or  
%nonassoc not

Las precedencias del "then" y del "else" ayudan a solucionar el problema ["dangling else ambiguity"](https://en.wikipedia.org/wiki/Dangling_else).

Posteriormente se definen todas las reglas de la gramatica que genera el lenguaje Willy\*. Estas van creando el AST, usando el tipo de dato definido en el archivo AST.hs. Finalmente se define la funcion de error a usar por parte del parser. Las reglas son de la forma:

S : t1 t2 ... tk  { E } 

Donde S es un simbolo no terminal y ti es un simbolo terminal o no terminal, y E es la expresión que  se genera. Con estas expresiones se va creando nuestro AST. Se evito usar recursiones a la derecha al definir las reglas de la gramatica usada. 


En el main, luego de parsear, se llama al context checker, 


Finalmente tenemos una funcion que recibe el arreglo de tokens y devuelve un arreglo de strings, esto es para imprimir los tokens en el formato pedido en el proyecto. Desde el Main se recibe el nombre del archivo a interpretar, se chequea la validez del input y la existencia del archivo, para luego invocar al scanner con dicho archivo y mostrar la salida correspondiente por el standart ouput.
