# Doe-Lang

**Doe-Lang** es un lenguaje de programación imperativo no orientado a objetos, cuya temática no ha sido definida aun, para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722).  
 
## Autores:
- Luis Diaz 15-10420 
- Neil Villamizar 15-11523
- Jesús Wahrman 15-11540

### Inferencia de tipos
Si no se especifica el tipo de alguna función o variable, el compilador intentará inferirlo. Para inferir el tipo de una función se ve el cuerpo de esta, no el lugar en el que se usa.

### Basado en expresiones

### Identificadores
Es toda palabra que comienza con una letra minúscula, está formada posteriormente por letras, números y guiones bajos (`_`) y no es una palabra reservada por el lenguaje.

### Declaraciones
Para declarar variables tenemos:
1. Para declarar una variable sin inicializar y con tipo inferido: let x
2. Para declarar una variable sin inicializar y con tipo concreto: let x: Tipo
3. Para declarar una variable inicializada y con tipo concreto: let x: Tipo = Valor

Para declarar constantes tenemos ?.
La declaración devuelve el tipo unitario.


### Tipos de datos
#### Entero
Números enteros del tamaño del registro (int)
#### Flotante
Números flotantes de 32 y 64 bits (float y double)
#### Booleano
True o false (bool)
#### String
Alias para un arreglo de caracteres (string)
#### Arreglo
#### Struct
#### Union


### Instrucciones y estructuras de control de flujo
#### Aritmetica
#### For
#### ForEach
#### While
#### If
