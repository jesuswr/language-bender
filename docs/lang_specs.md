# Doe-Lang

**Doe-Lang** es un lenguaje de programación imperativo no orientado a objetos, cuya temática no ha sido definida aun, para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722).  
 
## Autores:
- Luis Diaz 15-10420 
- Neil Villamizar 15-11523
- Jesús Wahrman 15-11540

### Inferencia de tipos
Si no se especifica el tipo de alguna función o variable, el compilador intentará inferirlo. Para inferir el tipo de una función se inspecciona su cuerpo, no sus llamadas.

### Basado en expresiones
El lenguaje esta basado en expresiones. Esto es, todas las instruccions producen un valor. Para esto, Doe se basa en las siguientes reglas:
1) Los distintos caminos de ejecución de un bloque de instrucciones retornan el mismo tipo de dato. El caso contrario es un error de compilación.
2) Un bloque de instrucciones (delimitado por '{' y '}') retorna lo que retorne la última sub expresión, o el último ```break``` / ```continue``` en caso del control de flujo de los ciclos, o `return` en el caso de las funciones.
3) Existe un símbolo especial que hace que una expresión retorne ```()``` (el tipo unitario). Por ahora el “.” al final de una expresión, la forzara a retornar el tipo unitario.
4) Las expresiones que tienen caminos no definidos, retornan ```()``` por defecto (Ejemplo: un ```if``` sin un ```else```).
5) Adicionalmente, el tipo unitario ```()``` no es utilizable como un valor válido para una variable o función ni como anotación de tipo.

  **Nota:** Las instrucciones `break` y `continue` son instrucciones especiales que no evalúan a un tipo concreto, en su lugar cambian el control del flujo de los ciclos. Cuando un camino de ejecución contiene una instrucción de este tipo, no se le considera para inferir el tipo de la expresión, sino el tipo de retorno del ciclo que lo contiene. Esto ocurre porque no tiene sentido evaluar una expresión cuyo valor es imposible de utilizar, como ocurre en estos casos. Así, permitimos patrones bastante útiles como:
```
while(true) {
  let x = if (error()) 
             break
          else 
             f();
           
  // do something with x
} 
```
En este ejemplo, x tiene el tipo de retorno de f, mientras que el ciclo while tiene tipo de retorno (). 
  
### Identificadores
Es toda palabra que comienza con una letra, está formada posteriormente por caracteres alfanuméricos y guiones bajos (```_```) y no es una palabra reservada por el lenguaje.

### Declaraciones
Para declarar variables tenemos:
1. Para declarar una variable sin inicializar y con tipo concreto: ```let x: Tipo```
2. Para declarar una variable inicializada y con tipo concreto: ```let x: Tipo = Valor```
3. Para declarar una variable inicializada y con tipo inferido: ```let x = <expresion>```

Para declarar constantes se precede la declaración por ```const```. Por ejemplo:
```
const x = Valor
const x : Tipo = Valor
``` 
Las variables declaradas constantes deben ser siempre inicializadas. Para asignar condicionalmente distintos posibles valores a una constante podemos usar el siguiente patrón:
```
const x = if (X) f() else g();
```
Las variables declaradas constantes son inmutables. Eso incluye los campos de tipos compuestos.    
Cualquier declaración devuelve el tipo unitario.

### Asignación 
Asocia un valor a una variable. Es una expresión, como todo en el lenguaje. La asignación debe respetar tipos compatibles y retorna el valor asignado al identificador izquierdo:
```identificador  = expresión```

### Modelo Por Valor y Referencias en el Lenguaje
El lenguaje tiene modelo por valor. Es decir, la asignacion asocia una variable con un valor concreto copiandolo. Sin embargo se cuenta con la posibilidad de tener tipos de referencia a otras variables:
```
    let x = 1;
    let y = &x; // Referencia a x
```
En este ejemplo, ```y``` se usa como cualquier otra variable, pero sus cambios se hacen efectivos en ```x``` también. Además, no se puede declarar una variable de referencia sin inicializarla a no ser que sea en el argumento de una función:
```
    let z : & int; // Error de compilación
```

### Apuntadores 
Solo hay apuntadores al heap, los cuales se denotan como ``` Type * ```. El valor ```null``` es un valor especial que es compatible con cualquier tipo de apuntador. Se usa ```new T``` para crear un elemento en el heap, ```delete identificador``` para eliminarlo, y no existe aritmética de apuntadores.


### Tipos de Datos

#### Entero
Números enteros del tamaño del registro (```int```). Almacenados en complemento 2.

#### Flotante
Números flotantes de 32 y 64 bits (```float``` y ```double```). Siguen el estándar [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754#:~:text=The%20IEEE%20Standard%20for%20Floating,and%20Electronics%20Engineers%20(IEEE).&text=Many%20hardware%20floating-point%20units%20use%20the%20IEEE%20754%20standard.)

#### Booleano
Tipo booleano con dos posibles valores: ```true``` o ```false``` (```bool```). Cuentan con sus propios operadores:
- ``` and ```: Retorna ```true``` si ambos argumentos valen ```true```. De otra forma retorna ```false```.
- ``` or ```: Retorna ```true``` si alguno de los argumentos vale ```true```. De otra forma retorna ```false```.
- ``` not ```: Retorna ```true``` si ambos argumentos tienen el mismo valor. De otra forma retorna ```false```.


#### Char
Tipo de dato que representa un caracter. Codificados en formato ascii.

#### String
Alias para un arreglo de caracteres (```string```).

#### Arreglo
Los arreglos tienen tamaño constante de elaboración y se declaran como:
```
let a : T[Size];
let a = {a0,a1,a2};
```
Los arreglos soportan indexación, es la única operación que soportan:
```
let x : T[Size] = ...;
x[i] // evalua al elemento i del arreglo x. 
```
Para acceder al elemento `i` del arreglo `x` es necesario que `i` esté en el rango `[0,Size)`. De lo contrario, es un error de ejecución

#### Struct
tipo producto o record: ```struct <struct_name> {x0 : T0, ..., xk : Tk}``` donde ```xi``` de tipo ```Ti``` es un atributo del struct.
Para acceder a un atributo `x0` de un struct `s`, usamos el clásico operador punto: `s.x0`

Para crear un struct, usamos la sintaxis: `<struct_name>{expr0, ... , exprk}`. Por ejemplo:
```
struct S {a: int, b: float}
...
let s = S{1, 2.0};
```

#### Union
tipo suma o variante: ```union <union_name> = <tag_0> T0 | ... | <tag_k> Tk``` donde ```|``` separan los distintos tipos que puede tomar la union. Por ejemplo:
```
union U { MyFloat: float, MyInt: int }
```
Para crear una instancia de una union usamos la sintaxis `<union_typename>::<union_tag_name>{expr0, ... , exprk}`. Por ejemplo:  
```
let u = U::MyFloat{2.0}
```

El tipo de una instancia de la union es la unión en sí misma, para acceder a un posible valor, se usa el nombre de esa etiqueta. Por ejemplo:

```
let f : float = u.MyFloat
```
Es un error de ejecución que se le solicite un valor de un tipo distinto al tipo actualmente almacenado para la unión.  

Para consultar por el tipo de una union usamos la sintaxis `<instance_name>.<tag_name>?`. Por ejemplo:   
```
if (u.MyFloat?)
   logn(u.MyFloat).
else // if not float, it's int
   u.MyInt % 2.
```

#### Aritmetica
Sólo entre flotantes, entre enteros y entre ambos (en este último caso se retorna flotante). Usando los operadores: 
- ``` + ``` para la suma.
- ``` - ``` para la resta.
- ``` * ``` para la multiplicación.
- ``` / ``` para la división.
- `%` módulo, solo para enteros

#### Comparaciones
Para tipos enteros, flotantes y booleanos:
- ``` < ```: Retorna ```true``` si el argumento izquierdo es menor que el derecho. Para booleanos toma ```true``` como ```1``` y ```false``` como ```0```.
- ``` <= ```: Retorna ```true``` si el argumento izquierdo es menor o igual que el derecho. Para booleanos toma ```true``` como ```1``` y ```false``` como ```0```.
- ``` > ```: Retorna ```true``` si el argumento izquierdo es mayor que el derecho. Para booleanos toma ```true``` como ```1``` y ```false``` como ```0```.
- ``` >= ```: Retorna ```true``` si el argumento izquierdo es mayor o igual que el derecho. Para booleanos toma ```true``` como ```1``` y ```false``` como ```0```.
- ``` == ```: Retorna ```true``` si el argumento izquierdo es igual que el derecho. Además, para tipos compuestos compara los valores de forma profunda.

### Instrucciones y Estructuras de Control de Flujo

#### Comentarios
Los comentarios omiten una linea del codigo a partir del comentario. Para esto se usa ``` // ```.
```
// Esto es un comentario
let x = 1; // A partir de aqui es un comentario
// 'let x = 1' de arriba si es codigo
```

#### Secuenciación y bloque de instrucciones
El bloque de instrucciones es posiblemente vacío y retorna el valor de la última instrucción. La secuenciación se expresa con el símbolo ; entre cada expresión. Si se desea descartar el tipo de retorno de la última expresión y reemplazarlo por el tipo unitario, se utiliza el símbolo “.”:
```
{
    x+2;
    y*4;
    f();
    x
}
```
Esta expresión retorna el valor de x. Por otro lado: 
```        
{
    x+2;
    y*4;
    f();
    x.
}
```
Esta expresión retorna el tipo unitario.


#### If
Como en lenguajes tradicionales: 
```
if (expresión booleana) expresión else expresión
```
La instrucción también se puede evaluar, y la evaluación de la expresión principal debe tener el mismo tipo que la expresión secundaria del caso por defecto. En caso de no tener expresión secundaria, el tipo de retorno sera el tipo unitario.

#### For
Como en lenguajes tradicionales: 
```
for ( posible declaración e inicialización; condición booleana; expresión de incremento ) expresión
```

#### ForEach
Repetición determinada: 
```
for(variable_para_iterar : iterable) expresión
```

#### While
Como en lenguajes tradicionales: 
```
while (condición booleana) expresión
```

#### Control de Flujo para Ciclos
Se tienen instrucciones ```break``` y ```continue``` que permiten terminar un ciclo o una iteración prematuramente y respectivamente. Ambas instrucciones pueden recibir un parámetro adicional para retornar valores, al mismo estilo del return:
```
while (X) { 
    if (Y)
        break 10;
    i = i + 1
}
```
Así, la expresión de iteración puede retornar valores mediante su control de flujo.

### Sub-Rutinas

#### Funciones 
Todas las funciones retornan algo, y pueden tener los tipos de retorno inferidos de su cuerpo:
```
fn f(x: int) {
    2*x
}
```
Esa función es de tipo ```(int) -> int```. El tipo de los valores de entrada debe ser anotado.

#### Procedimientos
Son funciones que siempre retornan (), en otras palabras, retorna el tipo unit () y por lo tanto no puede ser utilizado como una subexpresión (a excepción de los bloques de instrucciones). Es imposible anotarles tipo de retorno como a las funciones, puesto que nunca retornan algo más que ().
```        
proc f(x) {
    print(x + 2)
}
```
Sus tipos de argumento también son anotables.

#### Orden de Ejecución
Existe un procedimiento especial ```proc main()``` que es el punto de partida del programa. Toda variable que esté declarada fuera de la función main debe ser constante en tiempo de compilación. 

#### Sobre Funciones y Procedimientos

###### Pasaje de Argumentos
Todos los argumentos se pasan por valor, el pasaje de parámetros por referencia se logra mediante los tipos de referencia ```&T```. Esto es, cuando se llama a una función o procedimiento ```F``` de firma ```F(x : &T)```, con argumento ```z```, dicha función declara internamente a la variable ```x : &T``` de la siguiente manera:
```
let x : &T = &z;
```
###### Sintaxis de Declaración

- Funciones:
```
func <name>([arg_list]) [-> <output_type>] expr
```
- Procedimientos:
```
proc <name>([arg_list]) expr
```
 
###### Default Arguments
podemos definir valores por defecto a los argumentos de las funciones, por ejemplo: 
``` 
f(x : int, y : int = 4, z : int = -1)
```
Los argumentos con valores por defecto tienen que ser los últimos (más a la derecha) en la firma.

#### I/O
Se provee una función especial `to_str(a)` tal que puede convertir cualquier objeto reducible a tipos básicos en un string
Las operaciones de IO básicas son:
* `open(filename: str, mode: str) -> File` - Apertura de archivos. Abre el archivo de nombre `filename` con el modo definido según el string `mode`. El formato del string `mode` se provee más adelante.
* `readf(file: File) -> str` - lectura de un archivo, lee todo cuando sea posible y lo retorna en un string.
* `printf(file: File, content: str)` - función especial que recibe un archivo `file`, un string `content` y escribe el string en el archivo según se especificó en su modo de apertura.

El objeto `File` es un tipo de dato especial usado para representar archivos, contiene información importante internamente como el string de modo y el file descriptor del archivo abierto.

También existen las funciones especiales `read` y `print` que son equivalentes a `printf` y `readf` con el archivo sustituido por `stdout` y `stdin` respectivamente.
