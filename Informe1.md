##### Jesus Wahrman 15-11540  
##### Neil Villamizar 15-11523   
# Informe Fase 1 
Para este proyecto utilizamos Haskell. En especifico para esta fase usamos la herramienta Alex, que recibe el archivo Lexer.x y genera el Lexer.hs que usamos en nuestro main para hacer el analisis lexicografico.

En el archivo Lexer.x primero definimos el wrapper a usar, en este caso usamos 'monad', ya que este nos daba la opcion de usar 'startCodes', que son para indicar que regex usar dependiendo del startCode en el que estes, en nuestro caso usamos 2: uno para los comentarios en bloque (commentBlock) y otro para lo demas (0). Esto nos ayudo mucho ya que dentro de los comentarios en bloque pueden haber errores, como por ejemplo encontrar otro {{, de esta forma si encontramos esa expresion dentro del commentBlock startCode, podemos hacer push de un error, cosa que sin el monad wrapper era imposible.

Seguidamente definimos algunas macros para mayor comodidad y legibilidad en la siguiente parte, estas fueron: digit (digitos), alpha (letras) y alphaNum (letras y digitos).

Luego se encuentran las definiciones de las regex que son de tipo:
> <startCode>      regex       { action }

Donde el startCode es lo que explicamos antes, regex es una expresion regular y action es la accion que realizara alex si hace match con esa regex.

Y luego viene una parte del codigo en la que Alex solo copia y pega nuestro codigo en el que el genera. Aqui definimos el tipo de dato Token, en el que todos menos uno tienen un par de enteros que representa linea y columna, y algunos tienen algo mas como: TKInt tiene el entero que representa, TKId tiene el string y TKError el caracter en el que se encontro el error. El unico que no tiene la posicion es el TKcommentEOFError, ya que este error da cuando se abren llaves para un comentario de bloque pero nunca se cierran, por lo cual tiene su propio mensaje de error y no necesita de la posicion.
Luego de esto viene una instacia de Token sobre la clase Show para que cada Token definido tenga su string respectivo.
Despues viene una definicion de alexEOF, que es necesaria para que Alex pueda ejecutarse correctamente, ya que cuando llega a el final del archivo devuelve esta funcion, que en nuestro caso, retorna un token que representa el fin del archivo.
Seguidamente vienen 4 funciones que son las acciones cuando se hace match con alguna regex, son: pushTK, pushInt, pushId y pushError. Son distintas ya que los tokens que representan tienen distintos valores.
A continuacion viene la funcion scanner, que devuelve un Either String [Token], esto es un Left String si encontro errores o un Right Token si no. Esta funcion con la ayuda de Alex va leyendo el input dado y lo va matcheando con las regex correspondientes, el scanner se detiene cuando Alex devuelve el TKEOF del que hablamos antes, en este caso revisa en que startCode nos encontramos, si estamos en el 0 no hay problema y devuelve una lista vacia, pero si estamos en commentBlock, significa que no hemos cerrado el comentario de bloque por lo que devuelve una lista que contiene TKcommentEOFError. Al final de la ejecucion el scanner puede devolver 2 cosas, si encontro errores en el arreglo de Tokens devuelve un Left String donde el String son los mensajes de error, o si no encuentra errores devuelve un Right [Token], donde [Token] es la lista de los tokens conseguidos. Luego hay 3 funciones que son usadas para ayudar al scanner, la primera auxF que recibe Either String [Token] y y si hay errores en la lista de tokens devuelve el string a imprimir con los errores y si no devuelve el mismo arreglo de tokens. Esta funcion a su vez usa las otras 2, isError que es para verificar si un Token es de error y strError que es para armar el String con todos los errores encontrados.

Finalmente tenemos una funcion que recibe el arreglo de tokens y devuelve un arreglo de strings, esto es para imprimir los tokens en el formato pedido en el proyecto.