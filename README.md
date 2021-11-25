# Práctica 2: Tableaux y Formas Normales

Deben completar las funciones faltantes de los módulos `Logic.Tableaux` y `Logic.Normal`.

Una descripción de los ejercicios y los puntajes está en el archivo `practica2.pdf`.

## Datos del equipo

Integrantes:

* 
* 
* 
* 

## Dependendencias

Antes de empezar a programar, hay que descargar las dependencias del proyecto.

```sh
stack setup
```

## Pruebas

Su práctica debe pasar todas las pruebas

```sh
stack test
```

Para solo ejecutar las pruebas reacionadas a `<nombre>`, se puede usar

``` sh
stack test --test-arguments='--match "<nombre>"'
```

**Nota**: inicialmente, la práctica solo tiene ejemplos en los comentarios.
Pero este comando corre las pruebas unitarias. Por lo que usar este comando no va a tener ningún efecto.

También puede ser útil usar `ghci` en su código para probar sus funciones de forma interactiva

``` sh
stack ghci
```

## Compilar la práctica

Una vez que terminen la implementación, verifiquen que compile

```sh
stack build
```
