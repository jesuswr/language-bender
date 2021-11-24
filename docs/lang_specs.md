
# Language Bender

**Language Bender** es un lenguaje de programación imperativo no orientado a objetos, basado en Avatar The Last Airbender, para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722).  

 
## Autores:
- Luis Diaz 15-10420 
- Neil Villamizar 15-11523
- Jesús Wahrman 15-11540

### Inferencia de tipos
Si no se especifica el tipo de alguna función o variable, el compilador intentará inferirlo. Para inferir el tipo de una función se inspecciona su cuerpo, no sus llamadas.

### Basado en expresiones
El lenguaje esta basado en expresiones. Esto es, todas las instruccions producen un valor. Para esto, language bender se basa en las siguientes reglas:
1) Los distintos caminos de ejecución de un bloque de instrucciones retornan el mismo tipo de dato. El caso contrario es un error de compilación.
2) Un bloque de instrucciones (delimitado por '.-' y '-.') retorna lo que retorne la última sub expresión de algún camino, o algún ```burst``` / ```to be continued``` en caso del control de flujo de los ciclos, o ```this story comes to an end``` en el caso de las funciones.
3) Existe un operador especial que hace que una expresión retorne ```()``` (el tipo unitario). El operador “\~” al final de una expresión, la forzara a retornar el tipo unitario luego de evaluarla.
4) Las expresiones que tienen caminos no definidos, retornan ```()``` por defecto (Ejemplo: un ```if``` sin un ```otherwise```).
5) Adicionalmente, el tipo unitario ```()``` no es utilizable como un valor válido para una variable o función ni como anotación de tipo.


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
eternal bender x is if C: f() otherwise g()
```
Las variables declaradas constantes son inmutables. Eso incluye los campos de tipos compuestos.    
Cualquier declaración devuelve el tipo unitario.

### Asignación 
Asocia un valor a una variable. Es una expresión, como todo en el lenguaje. La asignación debe respetar tipos compatibles y retorna el valor asignado al identificador izquierdo:
```
<id> is <expresion> -- asigna <expresion> a la variable <id>, y retorna <expresion>
```

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
Solo hay apuntadores al heap, los cuales se denotan como ```<T> art```. El valor ```an apprentice``` es un valor especial que es compatible con cualquier tipo de apuntador. Se usa ```born as <T> member``` para crear un elemento en el heap, ```<id> has died``` para eliminarlo, y no existe aritmética de apuntadores. Para dereferenciar se usa ```artwork of``` y para asignar luego de dereferenciar ```really is```.
```
bender x of water art is born as water member.
artwork of x really is 3.14.
bender y is artwork of x.
x is an apprentice.
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
- ``` not ```: Retorna ```lightning master``` si el argumento vale ```fire master```. De otra forma retorna ```fire master```.


#### Char
Tipo de dato que representa un carácter (```earth```). Codificados en formato ascii.

#### String
Alias para un arreglo de carácteres (```metal```). Con la siguiente sintaxis:
```
bender <id> of metal <size> purity
```
Donde ```<size>``` es el tamaño del string o arreglo de carácteres.

#### Arreglo
Los arreglos tienen tamaño constante de elaboración y se declaran como:
```
bender <id> of <element> nation since <size> years.                                                      -- ^ Sin inicializar
bender <id> of <element> nation since <size> years is master of <exp0>, <exp1>, ..., <expk> right now.   -- ^ Inicializado
```
Los arreglos soportan indexación, es la única operación que soportan:
```
disciple <index_exp> of <array_exp> -- ^ evalua al elemento <index_exp> del arreglo <array_exp>. 
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

#### Aritmetica
Sólo entre flotantes, entre enteros y entre ambos (en este último caso se retorna flotante). Usando los operadores: 
- ``` and then ``` para la suma.
- ``` but ``` para la resta.
- ``` and thus ``` para la multiplicación.
- ``` besides ``` para la división.
- `left` módulo, solo para enteros

#### Comparaciones
Para tipos enteros, flotantes y booleanos:
- ``` is less than ```: Retorna ```lightning master``` si el argumento izquierdo es menor que el derecho. Para booleanos toma ```lightning master``` como mayor a ```fire master```.
- ``` is less or equal than ```: Retorna ```lightning master``` si el argumento izquierdo es menor o igual que el derecho. Para booleanos toma ```lightning master``` como mayor a ```fire master```.
- ``` is greater than ```: Retorna ```lightning master``` si el argumento izquierdo es mayor que el derecho. Para booleanos toma ```lightning master``` como mayor a ```fire master```.
- ``` is greater or equal than ```: Retorna ```lightning master``` si el argumento izquierdo es mayor o igual que el derecho. Para booleanos toma ```lightning master``` como mayor a ```fire master```.
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
En `<exp>` no se permitira cambiar la variable de iteración `<it_id>`. `<step>`, `<start>` y `<end>` son expresiones de tipo entero (`air`) que indican la cantidad de incremento o decremento por iteración, el valor inicial de la variable de iteración y una cota para dicha variable respectivamente. Los valores que toma `<it_id>` estan en el rango `[<start>, <end>)` si `<start> <= <end>`, `(<end>, <start>]` en el caso contrario. Ejemplo:
```
opening 1 of aang chakras from 7 to 0:
    aang.
```
En este ejemplo la variable de iteración `aang` toma los valores `7`, `6`, `5`, `4`, `3`, `2` y `1`. El For es una expresión y retorna el valor de `<exp>` en la ultima iteración. Por lo que el ejemplo anterior evaluaría a `1`.

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
En este ejemplo se incrementa la variable `i` hasta que alcanza el valor `n`. El While es una expresión y retorna el valor de `<exp>` en la ultima iteración. En el ejemplo anterior evaluaría a `n`.

#### Control de Flujo para Ciclos
Se tienen instrucciones ```burst <expr>``` y ```to be continued <expr>``` que permiten terminar un ciclo o una iteración prematuramente y respectivamente. Ambas instrucciones reciben un parámetro para retornar valores. Al terminar un ciclo con ```burst <expr>```, el valor del ciclo será ```<expr>```. Al terminar una iteración con ```to be continued <expr>``` esta evalua ```<expr>```, útil en caso de que sea la última iteración. El tipo del cuerpo del ciclo debe ser le mismo de ```<expr>```. Adicionalmente ```burst ~``` y ```to be continued ~``` retornan tipo unitario. Ejemplo: 
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
book <book_id> about <arg0_type> bender <arg0_id>, ... , <argn_type> bender <argn_id> : <expr>                  -- tipo inferido
book <book_id> of <type_name> : <expr>                                                                          -- sin argumentos
book <book_id> : <expr>                                                                                         -- sin arg. y tipo inferido 
```
Una función retorna un resultado del tipo de `<expr>`. Este se puede indicar mediante `<type_name>` en la firma de la función, de lo contrario el tipo sera inferido. Los parametros de la función estan separados por comas, tienen identificador `<argi_id>` y tipo `<argi_type>`.
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
Existe un procedimiento especial ```main``` que es el punto de partida del programa. Toda variable que esté declarada fuera de la función main debe ser constante en tiempo de compilación. 

#### Sobre Funciones y Procedimientos

###### Pasaje de Argumentos
Todos los argumentos se pasan por valor, el pasaje de parámetros por referencia se logra mediante los tipos de referencia . Esto es, cuando se llama a una función o procedimiento ```F``` que tiene un paramentro `x` por referencia, con argumento ```z```, dicha función declara internamente a la variable ```x``` de la siguiente manera:
```
bender x is reincarnation of z.
``` 
En la declaración de una función o procedimiento, un argumento por referencia se indica mediante el tipo de referencia mediante la siguiente sintaxis: ``` <arg_type> reincarnation bender <arg_id> ```. Ejemplo:
```
-- yakun es un parametro por referencia
book set_t0_0 of water with water reincarnation bender yakun:
.-
    yakun is 0.0 -- se modifica la variable pasada como argumento
-.
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
