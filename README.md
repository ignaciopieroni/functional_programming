# Organizaciones 

**Paradigma Funcional**

Se desea procesar un conjunto de organizaciones de diferentes industrias para poder realizar diferentes consultas sobre ellas.
De cada organización se conoce su nombre, su sitio web, el país donde fue fundada, una breve descripción, el año en el que fue fundada, la industria a la que pertenece y su cantidad de empleados actual.
El objetivo final es poder realizar el análisis sobre un archivo [CSV](https://es.wikipedia.org/wiki/Valores_separados_por_comas) (llamado `organizaciones.csv`).
Por suerte otro equipo programo un modulo llamado [Utiles](/Utiles.hs) con varias funciones utilitarias para leer archivos y parsear los campos del mismo. Por lo que solamente deberemos preocuparnos de modelar los tipos de datos y escribir las funciones necesarias para realizar el análisis.

## Tareas a realizar

1.⁠ ⁠Modelar el tipo de dato `Organizacion`. Tener en cuenta todos los campos mencionados.

2.⁠ ⁠Escribir una función `organizacionMasVieja`, que dada una lista de organizaciones devuelva la organización que primero se fundó.

3.⁠ ⁠⁠Escribir una función que dadas 2 organizaciones, devuelva la suma de los empleados de ambas.

4.⁠ ⁠⁠Utilizar la función definida en el punto anterior para escribir una función `totalDeEmpleados`, que dada una lista de organizaciones devuelva la sumatoria de todos los empleados de las organizaciones de la lista. 

5.⁠ ⁠⁠Escribir una versión alternativa, sin utilizar la función del punto 3 y utilizando composición de funciones.

6.⁠ ⁠Escribir una función que devuelva la organización del sector de plásticos ("Plastics" en el archivo) con mayor cantidad de empleados y que haya sido fundada luego de 1960. Utilizar composición de funciones.

7.⁠ ⁠⁠Escribir una función llamada `ampliarOrganizacion` que dada una organización y un número, aumente la cantidad de empleados de esa organización en esa cantidad.

8.⁠ ⁠⁠Utilizando la función del punto anterior escribir una función `ampliarOrganizaciones` que dada una lista de organizaciones, aumente la cantidad de empleados de todas ellas en 10%.

9. Dada una lista de organizaciones, se desea averiguar si la mayoria de las organizaciones cumple con estos criterios:
    - Organizaciones con una cantidad par de empleados.
    - Organizaciones cuya primer palabra del nombre tenga más de una candidad dada de caracteres.
    - Organizaciones fundadas despues del 2000 y cuya primera palabra del nombre sea la que se indica.
          
Definir una unica funcion que permita tomar el criterio por parametro y realice la tarea correspondiente.
Dar diferentes ejemplos de consulta, en uno de ellos utilizando una expresiones lambda que exprese un nuevo criterio inventado. Justificar al utilidad del concepto de aplicacion parcial.

10. Dada una lista de organizaciones, verificar que no hay organizaciones repetidas. 

Importante: Evaluar las funciones definidas en los puntos anteriores usando la lista con todas las organizaciones que surja del archivo `organizaciones.csv`.
