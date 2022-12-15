# Postwork Sesión 1 Equipo 14

#### Objetivo

"El Postwork tiene como objetivo que practiques los comandos básicos aprendidos 
durante la sesión, de tal modo que sirvan para reafirmar el conocimiento. Recuerda 
que la programación es como un deporte en el que se debe practicar, habrá caídas, 
pero lo importante es levantarse y seguir adelante. Éxito"

#### Requisitos
#- Concluir los retos
#- Haber estudiado los ejemplos durante la sesión

#### Desarrollo

"El siguiente postwork, te servirá para ir desarrollando habilidades como si se 
tratara de un proyecto que evidencie el progreso del aprendizaje durante el módulo, 
sesión a sesión se irá desarrollando.
A continuación aparecen una serie de objetivos que deberás cumplir, es un ejemplo 
real de aplicación y tiene que ver con datos referentes a equipos de la liga española 
de fútbol (recuerda que los datos provienen siempre de diversas naturalezas), en 
este caso se cuenta con muchos datos que se pueden aprovechar, explotarlos y generar 
análisis interesantes que se pueden aplicar a otras áreas. Siendo así damos paso a las instrucciones:" 
  
"1. Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 de 
la primera división de la liga española: https://www.football-data.co.uk/spainm.php"

#El archivo que se descargo tiene el nombre de: SP1.csv


"2. Importa los datos a R como un Dataframe. NOTA: No olvides cambiar tu dirección 
de trabajo a la ruta donde descargaste tu archivo"

"En Files entro a la carpeta donde se encuentra el archivo SP1.csv y fijo el lugar de trabajo"

#Validar ruta trabajo.
getwd() 

# Importamos los datos a R a un Dataframe con la función read.csv(). 
sp1 <- read.csv("SP1.csv") 

# Verificamos estructura de dataframe.
str(sp1)  
head(sp1)

"3. Del dataframe que resulta de importar los datos a `R`, extrae las columnas
que contienen los números de goles anotados por los equipos que jugaron en casa
(FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG);
guárdalos en vectores separados"

# Se crea el vector con los goles anotados por equipos locales.
goles.locales<-sp1$FTHG 
is.vector(goles.locales) 
# Se crea el vector con los goles anotados por equipos visitantes.
goles.visitantes <-sp1$FTAG 
is.vector(goles.visitantes) 

"4. Consulta cómo funciona la función `table` en `R`.
Para ello, puedes ingresar los comandos `help(table)` o
`?table` para leer la documentación"


?table
# Se crea una tabla que nos da la frecuencia absoluta.

# Se crea tabla de frecuencia absoluta con los vectores anteriores.
goles.partido <- table(goles.locales,goles.visitantes) 

# Se visualiza la matriz de frecuencia de goles.locales vs goles.visitantes
View(goles.partido)


"5. Responde a las siguientes preguntas:
a) ¿Cuántos goles tuvo el partido con mayor empate?"

# Ordenamos la frecuencia de forma descendente e identificamos que:
# Para los partidos con mayor frecuencia (49 veces) es el empate 1-1
# Que suman 98 goles en total.
# Así mismo el empate que tuvo mayor cantidad de goles fue el 4-4 (1 vez).



"b) ¿En cuántos partidos ambos equipos empataron 0 a 0?"

#Se ordena la columna goles.locales de forma ascendente y se identifica que:
# En 33 partidos empataron 0-0

"c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar
que el equipo visitante (AG) metiera un solo gol?"

#Se ordena la columna goles.locales de forma descendente y se identifica que:
# El partido fue: 6-0 con una frecuencia de 1.



