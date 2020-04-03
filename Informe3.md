##### Jesus Wahrman 15-11540
##### Neil Villamizar 15-11523 

# Informe Fase 2

Para este proyecto utilizamos el lenguaje de programación funcional Haskell. En específico para esta fase usamos dos estructuras de fases anteriores, el AST creado por el Parser para encontrar la tarea y la tabla de simbolos de la fase del chequeo de contexto.

Inicialmente recorremos el nivel mas alto del AST buscando la tarea a ejecutar, si no existe es un error, y si la encontramos, buscamos el scope de su mundo y lo insertamos al stack de scopes, luego empilamos un nuevo scope para el tarea y empezamos a recorrer el AST de dicha tarea, utilizando la tabla de simbolos como sea necesario, ya sea para introducir funciones definidas por el usuario o para obtener valores como booleanos definidos en el mundo.

Al finalizar todas las instrucciones de la tarea o al llegar a una instruccion terminate, revisamos si el final goal se cumple, en caso de ser asi el programa fue exitoso, en caso contrario no lo fue.

Adicionalmente se imprime el mundo con una descripcion de las casillas, la basket de willy y la direccion a la que esta viendo willy, lo hicimos con un formato como leyenda, ya que mapear objetos a caracteres y ponerlos en las casillas podia presentar varios problemas, como por ejemplo que hacer si hay 10 tipos distintos de objetos en la misma casilla, seria muy dificil entender el mapa. El formato de leyenda nos permite asignar un numero a cada casilla y luego decir que hay en cada casilla facilmente, ademas podemos marcar las casillas con paredes con una X facilmente y la casilla en la que esta Willy con una W. De forma que el mapa queda bastante limpio.

## Programas de prueba

### create_spiral.willy

Este programa lo que hace es recorrer el mapa en forma de espiral y dejando objetos a medida que va haciendo el recorrido, se detiene cuando ya no tiene a donde ir y el objetivo fue logrado si en todas las casillas hay un objeto. Este programa esta basado en el siguiente problema:

[Circular Matrix (Construct a matrix with numbers 1 to m*n in spiral way)](https://www.geeksforgeeks.org/circular-matrix-construct-a-matrix-with-numbers-1-to-mn-in-spiral-way/)