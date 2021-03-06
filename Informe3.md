##### Jesus Wahrman 15-11540
##### Neil Villamizar 15-11523 

# Informe Fase 3

Para este proyecto utilizamos el lenguaje de programación funcional Haskell. EN esta fase del proyecto de concluyó la implementacion del interpretador para el lenguaje imperativo Willy\*. En específico para esta fase usamos dos estructuras de fases anteriores, el AST creado por el Parser para encontrar la tarea y la tabla de simbolos de la fase del chequeo de contexto.

Inicialmente recorremos el nivel mas alto del AST buscando la tarea a ejecutar, si no existe es un error, y si la encontramos, buscamos el scope de su mundo y lo insertamos al stack de scopes, luego empilamos un nuevo scope para el tarea y empezamos a recorrer el AST de dicha tarea, utilizando la tabla de simbolos como sea necesario, ya sea para introducir funciones definidas por el usuario o para obtener valores como booleanos definidos en el mundo.

Al finalizar todas las instrucciones de la tarea o al llegar a una instruccion terminate, revisamos si el final goal se cumple, en caso de ser asi el programa fue exitoso, en caso contrario no lo fue.

Adicionalmente se imprime el mundo con una descripcion de las casillas, la basket de willy y la direccion a la que esta viendo willy. Se hizo con un formato con leyenda, ya que mapear objetos a caracteres y ponerlos en las casillas podia presentar varios problemas, como por ejemplo que hacer si hay 10 tipos distintos de objetos en la misma casilla, seria muy dificil entender el mapa. El formato de leyenda nos permite asignar un numero a cada casilla y luego decir que hay en cada casilla facilmente, ademas podemos marcar las casillas con paredes con una X facilmente y la casilla en la que esta Willy con una W. De forma que el mapa queda bastante limpio.

Para los Colores en la terminal se uso una libreria de haskell "System.Console.ANSI" la cual se instala con el comando:
```console
user@machine:~$ cabal install ansi-terminal
```
Esta libreria incluye el tipo "Color", y funciones para elegir el color de salida, limpiar la pantalla, limpiar la linea, mover la posision del cursor, cambiar el titulo de la terminal entre otras.

En el proyecto se implementeron 3 modos de ejecucion. El modo automatico, el modo automatico cada X segundos, y el modo manual.

El modo automatico corre todo el interpretador sin parar y muestra el resultado final por salida estandard.

El modo automatico cada X segundos usa la libreria "Control.Concurrent" para con la funcion threadDelay hacer esperar al hilo por los X segundos que deseamos. Ademas se implemento una barra de progreso del tiempo para 

El modo manual avanza al presionar cualquier tecla que tenga codigo ASCII, ademas si se presiona la tcla 'e', se termina el programa.

## Programas de prueba

### create_spiral.willy

Este programa lo que hace es recorrer el mapa en forma de espiral y dejando objetos a medida que va haciendo el recorrido, se detiene cuando ya no tiene a donde ir y el objetivo fue logrado si en todas las casillas hay un objeto. Este programa esta basado en el siguiente problema:

[Circular Matrix (Construct a matrix with numbers 1 to m*n in spiral way)](https://www.geeksforgeeks.org/circular-matrix-construct-a-matrix-with-numbers-1-to-mn-in-spiral-way/)

### follow_path.willy

En este programa el mapa tiene un camino desde un inicio hasta un final marcado por objetos y Willy lo recorre hasta llegar al final que esta marcado por un objeto especial, es importante que en cada momento Willy solo sea capaz de tomar un camino para que funcione. El programa se detiene cuando Willy encuentra el objeto que marca el final y el final goal es que este en la casilla final.

### pick_all.willy

En este programa willy recorre todo el mapa recogiendo oro y luego debe poner todo el oro recogido en la posicion 1 1, el final goal es que no haya oro en las casillas que no sean la 1 1 y que todo el oro del mapa este en 1 1.

### xor.willy

Este programa usa el mapa como si estuviesemos viendo cada fila como un entero y calcula el xor de los enteros que estan en las dos primeras filas y lo coloca en la tercera. Aqui el final goal siempre es true ya que no hay forma de revisar el xor. Este problema esta inspirado en el problema de que tienes un arreglo en el que todos los elementos estan repetidos, menos uno, la mejor solucion para esto es calcular el xor de todos los elementos del arreglo y el resultado de esto es la respuesta por las propiedades del xor.

### lenght_is_power_of_two.willy

Este programa verifica si un string de 0's pertenece a ![equation](http://www.sciweavers.org/tex2img.php?eq=%20%5Cbig%5C%7B%200%5E%7B%202%5E%7Bn%7D%20%7D%20%3A%20n%20%20%5Cgeq%200%20%5Cbig%5C%7D%20&bc=White&fc=Black&im=jpg&fs=12&ff=arev&edit=0). Usa el mundo de willy como representacion del string. Realiza iteraciones del string de izquierda a derecha tachando cada segundo cero que encuentra (que no haya sido tachado anteriormente). Si al hacer esto, queda una cantidad de  ceros impar mayor a 1, entonces rechaza. Si al hacer esto queda un solo cero sin tachar entonces acepta. Hay dos tareas distintas. En una el string pertenece y en la otra no. Este programa esta basado en la maquina de Turing que se encuentra como ejemplo 3.7 en el capitulo 3 del libro "Introduction to the theory of Computation" de Michael Sipser. 

### error programs

Finalmente hicimos 4 programas que demuestran todas las clases de errores en nuestro interpretador de Willy*: lexicograficos, parseo, contexto o ejecucion (hay tres tareas distintas en el ultimo).