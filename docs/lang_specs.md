
# Language Bender

**Language Bender** es un lenguaje de programación imperativo no orientado a objetos, basado en Avatar The Last Airbender, para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722).  

 
## Autores:
- Luis Diaz 15-10420 
- Neil Villamizar 15-11523
- Jesús Wahrman 15-11540

### Inferencia de tipos
Si no se especifica el tipo de alguna variable, el compilador intentará inferirlo. Para inferir el tipo de una variable se inspecciona su valor, por lo que al declarar una variable se le debe dar un valor inicial o especificar el tipo.

### Basado en expresiones
El lenguaje esta basado en expresiones. Esto es, todas las instruccions producen un valor. Para esto, language bender se basa en las siguientes reglas:
1) Los distintos caminos de ejecución de un bloque de instrucciones retornan el mismo tipo de dato. El caso contrario es un error de compilación.
2) Un bloque de instrucciones (delimitado por '.-' y '-.') retorna lo que retorne la última sub expresión de algún camino, o algún ```burst``` / ```to be continued``` en caso del control de flujo de los ciclos, o ```this story comes to an end``` en el caso de las funciones.
3) Existe un operador especial que hace que una expresión retorne ```()``` (el tipo unitario). El operador “\~” al final de una expresión, la forzara a retornar el tipo unitario luego de evaluarla.
4) Las expresiones que tienen caminos no definidos, deben retornar ```()``` (Ejemplo: un ```if``` sin un ```otherwise```).
5) Adicionalmente, el tipo unitario ```()``` no es utilizable como un valor válido para una variable o función ni como anotación de tipo.


### Identificadores
Es toda palabra que comienza con una letra minuscula o un guion bajo (```_```), está formada posteriormente por caracteres alfanuméricos y guiones bajos (```_```) y no es una palabra reservada por el lenguaje.

### Declaraciones
Para declarar variables tenemos:
1. Para declarar una variable sin inicializar y con tipo concreto: ```bender <id> of <T>```
2. Para declarar una variable inicializada y con tipo concreto: ```bender <id> of <T> is <expresion>```
3. Para declarar una variable inicializada y con tipo inferido: ```bender <id> is <expresion>```
  
Cualquier declaración devuelve el tipo unitario.

### Asignación 
Asocia un valor a una variable. Es una expresión, como todo en el lenguaje. La asignación debe respetar tipos compatibles y retorna el valor asignado al identificador izquierdo:
```
<id> is <expresion> -- asigna <expresion> a la variable <id>, y retorna <expresion>
```

### Modelo Por Valor y Referencias en el Lenguaje
El lenguaje tiene modelo por valor. Es decir, la asignacion asocia una variable con un valor concreto copiandolo. Sin embargo, existe el pasaje por referencia en los argumentos de funciones y procedimientos.


### Apuntadores 
Solo hay apuntadores al heap, los cuales se denotan como ```<T> art```. El valor ```an apprentice``` es un valor especial que es compatible con cualquier tipo de apuntador. Se usa ```born as <T> member``` para crear un elemento en el heap, ```<id> has died``` para eliminarlo, y no existe aritmética de apuntadores. Se usa ```artwork``` para dereferenciar.
```
bender x of water art is born as water member.
x is an apprentice.
x has died.
artwork of x really is 42. -- se debe agregar 'really' al asignar despues de dereferenciar
``` 


### Tipos de Datos

#### Entero
Números enteros del tamaño del registro (el keyword para enteros es ```air```). Almacenados en complemento 2.

#### Flotante
Números flotantes de 32 bits (el keyword para flotantes es ```water``` ). Siguen el estándar [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754#:~:text=The%20IEEE%20Standard%20for%20Floating,and%20Electronics%20Engineers%20(IEEE).&text=Many%20hardware%20floating-point%20units%20use%20the%20IEEE%20754%20standard.)

#### Booleano
Tipo booleano con dos posibles valores: ```lightning master``` (true) o ```fire master``` (false) (el keyword para booleano es ```fire```). Cuentan con sus propios operadores:
- ``` and ```: Retorna ```lightning master``` si ambos argumentos valen ```lightning master```. De otra forma retorna ```fire master```.
- ``` or ```: Retorna ```lightning master``` si alguno de los argumentos vale ```lightning master```. De otra forma retorna ```fire master```.
- ``` not ```: Retorna ```lightning master``` si el argumento tiene valor ```fire master```. De otra forma retorna ```fire master```.


#### Char
Tipo de dato que representa un caracter (el keyword para char es ```earth```). Codificados en formato ascii.

#### String
Alias para un arreglo de caracteres (el keyword para strings es ```metal```). La sintaxis para declarar un string es la siguiente:
```
bender <id> of metal with <size> purity. -- <size> es el tamaño del string 
```

#### Arreglo
Los arreglos tienen tamaño constante de elaboración y se declaran como:
```
bender <id> of <element> nation since <size> years.                                                      -- ^ Sin inicializar
bender <id> of <element> nation since <size> years is master of <exp0>, <exp1>, ..., <expk> right now.   -- ^ Inicializado
```
Los arreglos soportan indexación, es la única operación que soportan:
```
disciple <index_exp> of <array_exp>.              -- ^ evalua al elemento <index_exp> del arreglo <array_exp>. 
disciple <index_exp> of <array_exp> really is 42. -- ^ Para asignar a una casilla del arreglo se debe usar la asignacion con el keyword 'really'
```
Ejemplo:
```
bender toph of earth nation since 3 years is master of aang, aang_jr, aang_jr_jr right now.
disciple 1 of toph -- ^ evalua al elemento en la posición 1 del arreglo toph. 
```
Para acceder al elemento `<index_exp>` del arreglo `<array_exp>` es necesario que `<index_exp>` esté en el rango `[0,Size)`. De lo contrario, es un error de ejecución.

#### Struct
tipo producto o record. Para declarar un nuevo struct:
 ```
 element <str_id> is compound by <tag0> skill of <T0>, ...
 ``` 
 donde ```tagi``` de tipo ```Ti``` es un atributo del struct.  

Para crear un struct, usamos la siguiente sintaxis:
```
bender <var_id> is learning <str_id> control using <exp0>, <exp1>, ..., <expn> right now
```
Para acceder a un atributo `<tag>` de un struct `<struct_exp>`, usamos:
```
using <struct_exp>'s <tag> skill
```
Para modificar un atributo `<tag>` de un struct `<struct_exp>`, usamos:
```
<struct_exp>'s <tag> is <expr>
```

Ejemplos: 
```
element lava is compound by super_fire skill of fire, bedrock skill of earth.
bender aang is learning lava control using fire master, 'b' right now.
bender zuko is using aang's super_fire skill.
aang's bedrock is 'r'.
```

#### Union
tipo suma o variante. Para declarar una nueva union: 
```
energy <union_id> allows <tag0> technique of <T0> bending, ...
``` 
Para crear una instancia de una union usamos:
```
bender <var_id> is learning <union_id>'s <tag> technique from <exp>
```
El tipo de una instancia de la union es la unión en sí misma, para acceder a un posible valor, se usa el nombre de esa etiqueta. Por ejemplo:
```
using <union_expr>'s <tag> technique

```
Es un error de ejecución que se le solicite un valor de un tipo distinto al tipo actualmente almacenado para la unión.  

Para consultar por el tipo de una union usamos la sintaxis:
```
trying <union_expr>'s <tag> technique
``` 
Por ejemplo:   
```
if tryng u MyFloat technique:
   in logn book with using u's MyFloat technique...
otherwise -- if not float, it's int
   using u's MyInt technique left 2.
```

#### Cero de un Tipo

El cero de un tipo es un valor para cada tipo que se retorna cuando se necesita el valor de un loop vacio (un loop que no llega a iterar). Dichos valores son los siguientes:
- Para enteros, el valor cero es ```0```.
- Para flotantes, el valor cero es ```0.0```.
- Para booleanos, el valor cero es ```false```.
- Para caracteres, el valor cero es ```$```.
- Para arreglos, el valor cero es un arreglo vacio.
- Para apuntadores, el valor cero es ```an apprentice```.
- Para strutcs y uniones, el valor cero es indeterminado.


#### Aritmetica
Sólo entre un par de flotantes; o entre un par de enteros. Usando los operadores: 
- ``` and then ``` para la suma.
- ``` but ``` para la resta.
- ``` and thus ``` para la multiplicación.
- ``` besides ``` para la división.
- ```left``` para el módulo, solo para enteros

#### Comparaciones
Para tipos enteros, flotantes y booleanos:
- ``` is less than ```: Retorna ```lightning master``` si el argumento izquierdo es menor que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is less or equal than ```: Retorna ```lightning master``` si el argumento izquierdo es menor o igual que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is greater than ```: Retorna ```lightning master``` si el argumento izquierdo es mayor que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is greater or equal than ```: Retorna ```lightning master``` si el argumento izquierdo es mayor o igual que el derecho. Para booleanos toma ```lightning master``` como ```1``` y ```fire master``` como ```0```.
- ``` is equal to ```: Retorna ```lightning master``` si el argumento izquierdo es igual que el derecho, de lo contrario retorna ```fire master```. Además, para tipos compuestos compara los valores de forma profunda.
- ``` is not equal to ```: Retorna ```fire master``` si el argumento izquierdo es igual que el derecho, de lo contrario retorna ```lightning master```. Además, para tipos compuestos compara los valores de forma profunda.

### Instrucciones y Estructuras de Control de Flujo

#### Comentarios
Los comentarios omiten una linea del codigo a partir del comentario. Para esto se usa ``` -- ```.
```
-- Esto es un comentario
bender x is 1. -- A partir de aqui es un comentario
-- Otro comentario. -- Sigue siendo el mismo comentario xd
```

#### Secuenciación y bloque de instrucciones
El bloque de instrucciones es posiblemente vacío y retorna el valor de la última instrucción. La secuenciación se expresa con el símbolo ```.``` entre cada expresión. Si se desea descartar el tipo de retorno de la última expresión y reemplazarlo por el tipo unitario, se utiliza el operador “\~”. Este es un operador unitario que toma una expresión como argumento, la evalúa, y retorna el tipo unitario. Este operador asocia a izquierda por lo que se usa al final de una expresión.
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
    x ~
-.
```
Esta expresión retorna el tipo unitario.


#### If
Como en lenguajes tradicionales: 
```
if <bool_exp>: <exp1> otherwise <exp2>
if <bool_exp>: <exp1>. otherwise <exp2> -- ^ El punto en <expr1> es opcional.  
```
La instrucción también se puede evaluar, y la evaluación de la expresión principal debe tener el mismo tipo que la expresión secundaria del caso por defecto. En caso de no tener expresión secundaria, el tipo de retorno sera el tipo unitario.
```
if <bool_exp>: <exp>                    -- ^ Retorna el tipo unitario
```

#### For (acotado)
Las iteraciones acotadas permiten repetir una secuencia no vacía de instrucciones por una cantidad fija de iteraciones, conocida antes de la primera iteración.
La sintaxis para una iteración acotada es la siguiente:
```
opening <step> of <it_id> chakras from <start> to <end> : <exp>
```
En `<exp>` no se permitira cambiar la variable de iteración `<it_id>` ni declarar una variable con el mismo nombre. `<step>`, `<start>` y `<end>` son expresiones de tipo entero (`air`) que indican la cantidad de incremento o decremento por iteración, el valor inicial de la variable de iteración y una cota para dicha variable respectivamente. Los valores que toma `<it_id>` estan en el rango `[<start>, <end>)` y se debe cumplir `<start> <= <end>`,  en caso contrario (`(<end>, <start>]`) no itera. Ejemplo:
```
opening 1 of aang chakras from 0 to 7:
    aang.
```
En este ejemplo la variable de iteración `aang` toma los valores `0`, `1`, `2`, `3`, `4`, `5` y `6`. El For es una expresión y retorna el valor de `<exp>` en la ultima iteración. Por lo que el ejemplo anterior evaluaría a `6`. En caso de no iterar, el for retorna el 'cero' del tipo.

#### While
Como en lenguajes tradicionales: 
```
while <bool_exp> doing: <exp>
```
Ejemplo:
```
bender i is 0.
while i is less than n doing:
    i is i and then 1.
```
En este ejemplo se incrementa la variable `i` hasta que alcanza el valor `n`. El While es una expresión y retorna el valor de `<exp>` en la ultima iteración. En caso de no iterar, devuelve el valor cero del tipo. En el ejemplo anterior evaluaría a `n`.

#### Control de Flujo para Ciclos
Se tienen instrucciones ```burst <expr>``` y ```to be continued <expr>``` que permiten terminar un ciclo o una iteración prematuramente y respectivamente. Ambas instrucciones reciben un parámetro para retornar valores. Al terminar un ciclo con ```burst <expr>```, el valor del ciclo será ```<expr>```. Al terminar una iteración con ```to be continued <expr>``` esta evalua ```<expr>```, útil en caso de que sea la última iteración. El tipo del cuerpo del ciclo debe ser el mismo de ```<expr>```. Adicionalmente ```burst ~``` y ```to be continued ~``` retornan tipo unitario. Ejemplo: 
```
bender X of air.
bender i of air is 0.
X is while i is less than 100 doing :
.-
    if i is equal to 42:
        burst 42 ~ .      -- ~(burst 42) evalua el busrt 42 y retorna (), por lo que el tipo del if queda '()' y el del while 'air'. 
    i is i and then 1
-.
```
Así, la expresión de iteración puede retornar valores mediante su control de flujo. Notemos en el ejemplo ```burst 42 ~```. El operador \~ tiene menor precedencia que el burst, por lo que la expresión nos queda asociada asi ((burst 42)\~). El if tendra tipo unitario ya que no tiene segunda expresión y la primera retorna () gracias al operador \~. El ciclo while tendra tipo entero ya que tanto la última expresión del bloque como el burst (que puede ser evaluado por \~) son de tipo entero.

### Sub-Rutinas

#### Funciones 
Para declarar una función se usa la siguiente sintaxis:
```
book <book_id> of <type_name> about <arg0_type> bender <arg0_id>, ... , <argn_type> bender <argn_id> : <expr>   -- tipo <type_name>
book <book_id> of <type_name> : <expr>                                                                          -- sin argumentos 
```
Una función retorna un resultado del tipo de `<expr>`. Este se debe indicar mediante `<type_name>` en la firma de la función. Los parametros de la función estan separados por comas, tienen identificador `<argi_id>` y tipo `<argi_type>`.
La expresión ```this story comes to an end <expr_>``` puede usarse dentro del cuerpo de una función para finalizar la ejecución de la misma y hacer que la funcion retorne el valor de `<expr_>`, el cual debe ser del mismo tipo que del cuerpo `<expr>`.
Ejemplo:
```
book desert_trip of earth with air bender aang, earth bender toph: 
.-
    if aang is greater than 0:
        toph
    otherwise
        'F'
-.
```
#### Procedimientos
Son funciones que siempre retornan (), en otras palabras, retornan el tipo unitario. Es imposible anotarles tipo de retorno como a las funciones, puesto que nunca retornan algo más que (). Para declarar un procedimiento se usa la siguiente sintaxis:
```        
travel <travel_id> made by <arg0_type> bender <arg0_id>, ... , <argn_type> bender <argn_id> : <expr> 
travel <travel_id> : <expr>     -- procedimiento sin argumentos
```
Los parametros del procedimiento estan separados por comas, tienen identificador `<argi_id>` y tipo `<argi_type>`. Adicionalmente se puede usar la expresión ```this story comes to an end ~``` dentro del cuerpo de un procedimiento para finalizar la ejecución del mismo, por supuesto retornando ().
Ejemplo:
```
travel sea_odyssey made by air bender aang, water bender katara : 
.-
    aang -- got lost at sea,
    and then katara. -- but she found him
    -- and they scape together. So
    this story comes to an end ~
-.
```

#### Llamadas a Sub-Rutinas

Para llamar a una función se usa la siguiente sintaxis:
```
in <f_name> book with <expr1>, ..., <exprn> "..."
<f_name> book with <expr1>, ..., <exprn> "..."     -- el 'in' es opcional
```
Donde `<f_name>` es el identificador de la función, y ```<expr1>, ..., <exprn>``` son los argumentos. Ejemplo:
```
 -- a la variable iroh se le asigna el resultado de la función desert_trip con argumentos aang y toph.
bender iroh is in desert_trip book with aang, toph ...  
```
Para llamar a un procedimiento se usa la siguiente sintaxis:
```
in <proc_name> travel with <expr1>, ..., <exprn> "..."
<proc_name> travel with <expr1>, ..., <exprn> "..."     -- el 'in' es opcional
```
Donde `<proc_name>` es el identificador del procedimiento, y ```<expr1>, ..., <exprn>``` son los argumentos. Ejemplo:
```
-- Se llama al procedimiento sea_odyssey con argumentos aang y katara.
sea_odyssey travel with aang, katara ...
```
#### Orden de Ejecución
Existe un procedimiento especial ```main``` que es el punto de partida del programa. Sin embargo, las variables globales ejecutan su inicializacion antes de llamar al main.

#### Sobre Funciones y Procedimientos

###### Pasaje de Argumentos

Los argumentos se pasan por valor, pero tambien existe el pasaje de parámetros por referencia.
En la declaración de una función o procedimiento, un argumento por referencia se indica mediante la siguiente sintaxis: ``` <arg_type> reincarnation bender <arg_id> ```. Ejemplo:
```
-- yakun es un parametro por referencia
book set_t0_0 of water with water reincarnation bender yakun:
.-
    yakun is 0.0 -- se modifica la variable pasada como argumento
-.
```

### I/O
Existen funciones para la entrada y la salida para los tipos basicos:
- ```printair``` para imprimir un entero al stdout.
- ```printwater``` para imprimir un flotante al stdout.
- ```printearth``` para imprimir un caracter al stdout.
- ```printfire``` para imprimir un booleano al stdout.
- ```readair``` para leer un entero del stdin.
- ```readwater``` para leer un flotante del stdin.
- ```readearth``` para leer un caracter del stdin.
- ```readfire``` para leer un booleano del stdin.