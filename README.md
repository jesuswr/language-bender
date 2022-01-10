 </p align="center">
<img src="https://github.com/jesuswr/language-bender/blob/develop/avatarcover.jpg" />

<p align="center">

# Language Bender

![ok](https://img.shields.io/badge/made-with%20love-green)

**Language Bender** es un lenguaje de programación imperativo no orientado a objetos, basado en *Avatar The Last Airbender*, para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722).  
 
## Autores:
- Luis Diaz 15-10420 
- Neil Villamizar 15-11523
- Jesús Wahrman 15-11540

## Especificación del Lenguaje:

Puedes encontrar la especificación del lenguaje en español [aquí](docs/lang_specs.md).
 
 
## Instalación
 
Para instalar, usar el makefile mediante el comando:
```
make
```
Se debe tener ya instalado ```stack```, ```bison```, ```flex```, ```g++``` y ```java```.

## Ejecución
 
 Para ejecutar el compilador usar el siguiente comando:
 ```
 stack exec -- lbend <file> [flags]
 ```
 donde ```<file>``` es el archivo a compilar. Debe tener extensión ```.bend```.
 La descripción de los flags disponibles se puede obtener al usar el flag ```--help```.
 Se debe ejecutar en el subdirectorio ```/language-bender/language-bender```.
