# DSL-debt-calculator

## Introducción
Este es el trabajo final de la materia `Analisis de Lenguajes de Programacion`. El mismo consiste de un DSL (lenguaje de dominio especifico) para la definicion de personas, grupos entre personas y donde se pueden cargar gastos tanto propios como ajenos. Luego se puede hacer una consulta de las deudas finales resumidas, tanto a nivel global, en un grupo o entre 2 personas. 
El lenguaje sobre el que fue programado es haskell.

### Manual de uso

El uso de este proyecto consiste en 2 partes. Inicialmente esta la carga de un archivo de deudas, este deberá tener una extensión .dbt. Luego, se podrá hacer uso del interprete para correr las [acciones](#interprete).
Una vez teniendo todo descargado, se deberá correr el comando `stack run`. Este tomara por defecto el archivo Default.dbt situado en Ejemplos y generara las deudas adecuadas.
También, al correr ese comando, se entrara en el interprete.
Antes de hablar sobre cada uno de estos ítem por separado, se presentara la gramática con la que se trabajo.

* `digit ::= ‘0’|‘1’|...|‘9’`
* `val ::= digit | digit val`
<br>
* `letter ::= ‘a’|‘b’|...|‘z’`
* `var ::= letter |letter var`
<br>
* `names ::= var ‘,’ names |var`
<br>
* `def ::= ‘DEF IN EP ’ var`
* `|‘DEFINEG’ var ‘[’ names ‘]’`
* `|‘DEBTP ’ var val`
* `|‘DEBTG’ var var val`
* `|‘EXPENSE’ var val`
<br>
* `op ::= ‘CALCULATE’ var`
* `|‘CALCULATEALL’`
* `|‘REGISTRY’ var`
* `|‘MEMBERS’ var`


### Archivos de deuda

El archivo que se carga en un inicio esta pensado para que se carguen únicamente definiciones y gastos. Y estos se escribirán de la manera que lo muestra la gramática def. Estos se correrán en el orden en el que fueron escritos, así que si se genera un gasto de una persona, previamente se tendría que haber definida a esta. 

A continuación se explicara como se unas las operaciones:
* `DEFINEP nombre`: Crea una persona con el nombre dado.
* `DEFINEG nombre participantes`: Crea un grupo con el nombre y participantes dados (además de uno mismo).
* `DEBTP nombre monto`: Crea un gasto entre `nombre` y uno mismo pagado por el otro con un monto dado.
* `DEBTG nombre grupo monto`: Crea un gasto en el grupo pagado por `nombre` con un monto dado.
* `EXPENSE nombre monto`: En este caso, `nombre` puede ser de una persona o un grupo. Allí se crea un gasto pagado por uno mismo con el monto dado.

### Interprete

En el interprete se podrán escribir comandos que sirvan para obtener datos a partir de lo cargado en el archivo. Estas son, entre otros, los que se ven en la gramática op. 

A continuación se explicara como se unas las operaciones:
* `CALCULATE nombre`: Calcula las deudas finales sobre la persona o grupo `nombre`.
* `CALCULATEALL`: Calcula las deudas finales sobre todas las personas y grupos.
* `REGISTRY nombre`: Devuelve las operaciones hechas sobre la persona o grupo `nombre`.
* `MEMBERS nombre`: Devuelve los participantes del grupo `nombre` o una lista con ese mismo elemento de ser `nombre` una persona.

Adicionalmente están los siguientes comandos:
* `:load`: Carga un nuevo archivo `.odt` eliminando los datos generados por el archivo anterior.
* `:browse`: Muestra los nombre de los grupos y personas.
* `:print`: Imprime el entorno.
* `:quit`: Sale del interprete.
* `:help`: Muestra la lista de comandos.
