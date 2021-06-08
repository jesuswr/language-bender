
# Language Bender

**Language Bender** es un lenguaje de programación imperativo no orientado a objetos, basado en Avatar The Last Airbender, para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722).  
 
## Autores:
- Luis Diaz 15-10420 
- Neil Villamizar 15-11523
- Jesús Wahrman 15-11540

### Inferencia de tipos
Si no se especifica el tipo de alguna función o variable, el compilador intentará inferirlo. Para inferir el tipo de una función se inspecciona su cuerpo, no sus llamadas.

### Basado en expresiones
El lenguaje esta basado en expresiones. Esto es, todas las instruccions producen un valor. Para esto, Doe se basa en las siguientes reglas:
1) Los distintos caminos de ejecución de un bloque de instrucciones retornan el mismo tipo de dato. El caso contrario es un error de compilación.
2) Un bloque de instrucciones (delimitado por '.-' y '-.') retorna lo que retorne la última sub expresión, o el último ```burst``` / ```to be continued``` en caso del control de flujo de los ciclos, o `this story comes to an end` en el caso de las funciones.
3) Existe un símbolo especial que hace que una expresión retorne ```()``` (el tipo unitario). Por ahora el “~” al final de una expresión, la forzara a retornar el tipo unitario.
4) Las expresiones que tienen caminos no definidos, retornan ```()``` por defecto (Ejemplo: un ```if``` sin un ```otherwise```).
5) Adicionalmente, el tipo unitario ```()``` no es utilizable como un valor válido para una variable o función ni como anotación de tipo.

  **Nota:** Las instrucciones `burst` y `to be continued` son instrucciones especiales que no evalúan a un tipo concreto, en su lugar cambian el control del flujo de los ciclos. Cuando un camino de ejecución contiene una instrucción de este tipo, no se le considera para inferir el tipo de la expresión, sino el tipo de retorno del ciclo que lo contiene. Esto ocurre porque no tiene sentido evaluar una expresión cuyo valor es imposible de utilizar, como ocurre en estos casos. Así, permitimos patrones bastante útiles como:
```
while(lightning master) .-
  bender x is if (error()) 
             break
          otherwise 
             f().
           
  -- do something with x
-.
```
En este ejemplo, x tiene el tipo de retorno de f, mientras que el ciclo while tiene tipo de retorno (). 
  
### Identificadores
Es toda palabra que comienza con una letra minuscula o un guion bajo (```_```), está formada posteriormente por caracteres alfanuméricos y guiones bajos (```_```) y no es una palabra reservada por el lenguaje.

### Declaraciones
Para declarar variables tenemos:
1. Para declarar una variable sin inicializar y con tipo concreto: ```bender <id> of <T>```
2. Para declarar una variable inicializada y con tipo concreto: ```bender <id> of <T> is <expresion>```
3. Para declarar una variable inicializada y con tipo inferido: ```bender <id> is <expresion>```

Para declarar constantes se precede la declaración por ```eternal```. Por ejemplo:
```
eternal bender <id> of <T> is <expresion>
eternal bender <id> is <expresion>
``` 
Las variables declaradas constantes deben ser siempre inicializadas. Para asignar condicionalmente distintos posibles valores a una constante podemos usar el siguiente patrón:
```
eternal bender x is if (X) f() otherwise g();
```
Las variables declaradas constantes son inmutables. Eso incluye los campos de tipos compuestos.    
Cualquier declaración devuelve el tipo unitario.

### Asignación 
Asocia un valor a una variable. Es una expresión, como todo en el lenguaje. La asignación debe respetar tipos compatibles y retorna el valor asignado al identificador izquierdo:
```<id> is <expresion>```

### Modelo Por Valor y Referencias en el Lenguaje
El lenguaje tiene modelo por valor. Es decir, la asignacion asocia una variable con un valor concreto copiandolo. Sin embargo se cuenta con la posibilidad de tener tipos de referencia a otras variables:
```
    bender x is 1.
    bender y is reincarnation of x. -- Referencia a x
```
En este ejemplo, ```y``` se usa como cualquier otra variable, pero sus cambios se hacen efectivos en ```x``` también. Además, no se puede declarar una variable de referencia sin inicializarla a no ser que sea en el argumento de una función:
```
    bender y is reincarnation; -- Error de compilación
```

### Apuntadores 
Solo hay apuntadores al heap, los cuales se denotan como ```<T> art```. El valor ```an apprentice``` es un valor especial que es compatible con cualquier tipo de apuntador. Se usa ```born as <T> member``` para crear un elemento en el heap, ```<id> has died``` para eliminarlo, y no existe aritmética de apuntadores.
```
bender x of water art is born as water member.
x has died.
``` 


### Tipos de Datos

#### Entero
Números enteros del tamaño del registro (```air```). Almacenados en complemento 2.

#### Flotante
Números flotantes de 32 bits (```water``` ). Siguen el estándar [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754#:~:text=The%20IEEE%20Standard%20for%20Floating,and%20Electronics%20Engineers%20(IEEE).&text=Many%20hardware%20floating-point%20units%20use%20the%20IEEE%20754%20standard.)

#### Booleano
Tipo booleano con dos posibles valores: ```lightning master``` o ```fire master``` (```fire```). Cuentan con sus propios operadores:
- ``` and ```: Retorna ```lightning master``` si ambos argumentos valen ```lightning master```. De otra forma retorna ```fire master```.
- ``` or ```: Retorna ```lightning master``` si alguno de los argumentos vale ```lightning master```. De otra forma retorna ```fire master```.
- ``` not ```: Retorna ```lightning master``` si ambos argumentos tienen el mismo valor. De otra forma retorna ```fire master```.


#### Char
Tipo de dato que representa un caracter (```earth```). Codificados en formato ascii.

#### String
Alias para un arreglo de caracteres (```metal```).

#### Arreglo
Los arreglos tienen tamaño constante de elaboración y se declaran como:
```
bender <id> of <element> nation since <size> years.
bender <id> of <element> nation since <size> years is master of <exp0>, <exp1>, ..., <expk>.
```
Los arreglos soportan indexación, es la única operación que soportan:
```
bender toph of earth nation since 42 years is master of aang, aang_jr, aang_jr_jr.
disciple <i> of toph // evalua al elemento i del arreglo toph. 
```
Para acceder al elemento `i` del arreglo `toph` es necesario que `i` esté en el rango `[0,Size)`. De lo contrario, es un error de ejecución

#### Struct
tipo producto o record:
 ```element <id> is compound by <tag0> skill of <T0>, ...``` 
 donde ```tagi``` de tipo ```Ti``` es un atributo del struct.
Para acceder a un atributo `x0` de un struct `s`, usamos:
```using <id>'s <tag> skill```

Para crear un struct, usamos la sintaxis: 
```
element lava is compound by super_fire skill of fire, bedrock skill of earth.
...
bender aang is learning lava control using <exp0>, <exp1>,...
```

#### Union
tipo suma o variante: 
```
energy <id> allows <tag0> technique of <T0> bending, ...
``` 
Para crear una instancia de una union usamos:
```
bender <id> is learning <union_id>'s <tag> technique from <exp>
```
El tipo de una instancia de la union es la unión en sí misma, para acceder a un posible valor, se usa el nombre de esa etiqueta. Por ejemplo:
```
using <var_name>'s <elem_tag> technique

```
Es un error de ejecución que se le solicite un valor de un tipo distinto al tipo actualmente almacenado para la unión.  

Para consultar por el tipo de una union usamos la sintaxis:
```trying <var_name>'s <elem_tag> technique``` 
Por ejemplo:   
```
if (tryng u MyFloat technique)
   logn(using u's MyFloat technique).
else -- if not float, it's int
   using u's MyInt technique % 2.
```

#### Aritmetica
Sólo entre flotantes, entre enteros y entre ambos (en este último caso se retorna flotante). Usando los operadores: 
- ``` and then ``` para la suma.
- ``` but ``` para la resta.
- ``` and thus ``` para la multiplicación.
- ``` besides ``` para la división.
- `left` módulo, solo para enteros

#### Comparaciones
Para tipos enteros, flotantes y booleanos:
- ``` is less than ```: Retorna ```lightning master``` si el argumento izquierdo es menor que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is less or equal than ```: Retorna ```lightning master``` si el argumento izquierdo es menor o igual que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is greater than ```: Retorna ```lightning master``` si el argumento izquierdo es mayor que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is greater or equal than ```: Retorna ```lightning master``` si el argumento izquierdo es mayor o igual que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is equal to ```: Retorna ```lightning master``` si el argumento izquierdo es igual que el derecho. Además, para tipos compuestos compara los valores de forma profunda.

### Instrucciones y Estructuras de Control de Flujo

#### Comentarios
Los comentarios omiten una linea del codigo a partir del comentario. Para esto se usa ``` -- ```.
```
-- Esto es un comentario
let x = 1; -- A partir de aqui es un comentario
-- 'let x = 1' de arriba si es codigo
```

#### Secuenciación y bloque de instrucciones
El bloque de instrucciones es posiblemente vacío y retorna el valor de la última instrucción. La secuenciación se expresa con el símbolo . entre cada expresión. Si se desea descartar el tipo de retorno de la última expresión y reemplazarlo por el tipo unitario, se utiliza el símbolo “~”:
```
.-
    x besides 2.
    y and then 4.
    x
-.
```
Esta expresión retorna el valor de x. Por otro lado: 
```        
.-
    x besides 2.
    y and then 4.
    x.
    ~
-.
```
Esta expresión retorna el tipo unitario.


#### If
Como en lenguajes tradicionales: 
```
if (<bool_exp>) <exp> otherwise <exp>
```
La instrucción también se puede evaluar, y la evaluación de la expresión principal debe tener el mismo tipo que la expresión secundaria del caso por defecto. En caso de no tener expresión secundaria, el tipo de retorno sera el tipo unitario.

#### For (acotado)
Las iteraciones acotadas permiten repetir una secuencia no vacía de instrucciones por una cantidad fija de iteraciones, conocida antes de la primera iteración.
La sintaxis para una iteración acotada es la siguiente:
```
opening <step> <id> chakras from <start> to <end> <exp>
```
En `<exp>` no se permitira cambiar la variable de iteracion ni declarar una variable con el mismo nombre.

#### While
Como en lenguajes tradicionales: 
```
while <bool_exp> doing <exp>
```

#### Control de Flujo para Ciclos
Se tienen instrucciones ```burst``` y ```to be continued``` que permiten terminar un ciclo o una iteración prematuramente y respectivamente. Ambas instrucciones pueden recibir un parámetro adicional para retornar valores, al mismo estilo del ```this story comes to an end```:
```
while (X) { 
    if (Y)
        burst 10.
    i is i and then 1
}
```
Así, la expresión de iteración puede retornar valores mediante su control de flujo.

### Sub-Rutinas

#### Funciones 
Todas las funciones retornan algo:
```
book <book_id> of <type_name> about <arg0_type> bender <arg0_id>, ... : <expr>
```
#### Procedimientos
Son funciones que siempre retornan (), en otras palabras, retorna el tipo unit () y por lo tanto no puede ser utilizado como una subexpresión (a excepción de los bloques de instrucciones). Es imposible anotarles tipo de retorno como a las funciones, puesto que nunca retornan algo más que ().
```        
travel <travel_id> made by <arg0_type> bender <arg0_id>, ... : <expr>
```
Sus tipos de argumento también son anotables.

#### Orden de Ejecución
Existe un procedimiento especial ```main``` que es el punto de partida del programa. Toda variable que esté declarada fuera de la función main debe ser constante en tiempo de compilación. 

#### Sobre Funciones y Procedimientos

###### Pasaje de Argumentos
Todos los argumentos se pasan por valor, el pasaje de parámetros por referencia se logra mediante los tipos de referencia . Esto es, cuando se llama a una función o procedimiento ```F``` que tiene un paramentro `x` por referencia, con argumento ```z```, dicha función declara internamente a la variable ```x``` de la siguiente manera:
```
bender x is reincarnation of z.
``` 
###### Default Arguments
podemos definir valores por defecto a los argumentos de las funciones, por ejemplo: 
``` 
book desert_travel of earth with air bender aang is 73, earth bender toph is 'z':
```
Los argumentos con valores por defecto tienen que ser los últimos (más a la derecha) en la firma.

### I/O
Se provee una función especial `to_str(a)` tal que puede convertir cualquier objeto reducible a tipos básicos en un string
Las operaciones de IO básicas son:
* `open(filename: str, mode: str) -> File` - Apertura de archivos. Abre el archivo de nombre `filename` con el modo definido según el string `mode`. El formato del string `mode` se provee más adelante.
* `readf(file: File) -> str` - lectura de un archivo, lee todo cuando sea posible y lo retorna en un string.
* `printf(file: File, content: str)` - función especial que recibe un archivo `file`, un string `content` y escribe el string en el archivo según se especificó en su modo de apertura.

El objeto `File` es un tipo de dato especial usado para representar archivos, contiene información importante internamente como el string de modo y el file descriptor del archivo abierto.

También existen las funciones especiales `read` y `print` que son equivalentes a `printf` y `readf` con el archivo sustituido por `stdout` y `stdin` respectivamente.
