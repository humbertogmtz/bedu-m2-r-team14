######################Equipo 14 Postwork Sesión 5##########################################

"OBJETIVO
Realizar inferencia estadística para extraer información de la muestra que sea 
contrastable con la población.

REQUISITOS
Haber desarrollado los postworks anteriores
Cubrir los temas del prework
Replicar los ejemplos de la sesión

DESARROLLO
El data frame iris contiene información recolectada por Anderson sobre 50 
flores de 3 especies distintas (setosa, versicolor y virginca),
incluyendo medidas en centímetros del largo y ancho del sépalo así como de
los pétalos.

Estudios recientes sobre las mismas especies muestran que:

i) En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es 
igual a 5.7 cm

ii) En promedio, el ancho del pétalo de la especie virginica (Petal.Width)
es menor a 2.1 cm

iii) En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más 
grande que el promedio del largo del pétalo de la especie versicolor.

iv) En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.

Utilizando pruebas de inferencia estadística, concluye si existe evidencia
suficiente para concluir que los datos recolectados por Anderson están en 
línea con los nuevos estudios.

Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el
planteamiento de hipótesis adecuado y concluye."

df <- iris
View(df)

#Al no conocer la desviación estandar de la poblacion debemos utilizar la 
#desviacion estandar de la muestra.
#Por lo cual el estadistico de prueba distribuye como una t de student.

#Realizamos pruebas de inferencia estadística,
#con pruebas t para contrastar las hipótesis de los promedios.

#Si pvalue >= significancia (0.01) NO RECHAZO HIPOTESIS NULA Ho
# Si pvalue < significancia (0.01) RECHAZO HIPOTESIS NULA


"1) En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es 
igual a 5.7 cm"

#PLANTEAMIENTO DE HIPOTESIS
#Ho: prom_sepal_length == 5.7 cm
#Ha: prom_sepal_lenght =! 5.7 cm

#CONTRASTE DE DOS COLAS
t.test(x=df[df$Species == "setosa", "Sepal.Length"], 
       alternative = 'two.sided', mu=5.7)
# p-value < 2.2e-16

#NIVEL DE CONFIANZA 99% IMPLICA 0.01 DE SIGNIFICANCIA
#CONTRASTE DE DOS COLAS: SIGNIFICANCIA SE DIVIDE ENTRE 2 
# significancia = 0.01

#ENTONCES
#p-value < significancia

# A un nivel de confianza del 99% existe evidencia estadística para rechazar 
# la hipótesis nula; es decir los datos datos de Anderson muestran que el 
# promedio del largo de la especie Setosa es =! 5.7 cm.
# Por lo tanto estos datos no estan en  linea con este nuevo estudio!


"2) En promedio, el ancho del pétalo de la especie virginica (Petal.Width)
es menor a 2.1 cm"

#PLANTEAMIENTO DE HIPOTESIS
#Ho: prom_petal_width >= 2.1 cm
#Ha: prom_petal_width < 2.1 cm

#CONTRASTE DE COLA INFERIOR
t.test(x=df[df$Species == "virginica", "Petal.Width"], 
       alternative = 'less', mu=2.1)
#p-value = 0.03132
# significancia = 0.01

# ENTONCES
# p-value > significancia

# A un nivel de confianza del 99% no existe evidencia estadística para rechazar 
# la hipótesis nula; es decir los datos datos de Anderson muestran que el 
# promedio del ancho del petalo de la especie Virginica es >= 2.1 cm.
# Por lo tanto estos datos no estan en linea con este nuevo estudio!


"3) En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más 
grande que el promedio del largo del pétalo de la especie versicolor."

#Tenemos una variable para dos grupos.
#Verificamos si las desviaciones estandar son igual o diferentes.

#PLANTEAMIENTO DE HIPOTESIS
#Ho: varianzas iguales
#Ha: varianzas diferentes

#CONTRASTE DE DOS COLAS
var.test(df[df$Species == "virginica", "Petal.Length"], 
         df[df$Species == "versicolor", "Petal.Length"], 
         ratio = 1, alternative = "two.sided")
#p-value = 0.2637
#significancia = 0.01

# ENTONCES
# p-value > significancia

# A un nivel de confianza del 99% no existe evidencia estadística para rechazar 
# la hipótesis nula; es decir los datos datos de Anderson muestran que:
#las varianzas de los grupos son iguales.


#PLANTEAMIENTO DE HIPOTESIS
#Ho: prom_petal_length_virginica -  prom_petal_length_versicolor == 1.1
#Ha: prom_petal_length_virginica - prom_petal_length_versicolor  =! 1.1

#CONTRASTE DE DOS COLAS
t.test(x = df[df$Species == "virginica", "Petal.Length"],
       y = df[df$Species == "versicolor", "Petal.Length"],
       alternative = "two.sided",
       mu = 1.1, var.equal = TRUE)
# p-value = 0.06405
#significancia = 0.01

# ENTONCES
# p-value > significancia

# A un nivel de confianza del 99% no existe evidencia estadística para rechazar 
# la hipótesis nula; es decir los datos datos de Anderson muestran que:
# prom_petal_length_virginica - prom_petal_length_versicolor == 1.1 cm
#¡Los datos estan en linea con este nuevo estudio!


"4) En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies."

#PLANTEAMIENTO DE HIPOTESIS:
#Ho: prom_sepal_width_setosa = prom_sepal_width_virginica = prom_sepal_width_versicolor
#Ha: al menos una es diferente.

# BOXPLOT PARA VER GRAFICAMENTE LO QUE SUCEDE
boxplot(Sepal.Width ~ Species,
        data = df)
#Graficamente podemos concluir que en promedio son diferentes los anchos del sepalo.

#Para comparar la media de una variable considerando dos o mas grupos de factor
#usamos el análisis de varianza (ANOVA)

anova <- aov(Sepal.Width ~ Species,
             data = df)
summary(anova)
#p-value < 2e-16
#significancia = 0.01

# ENTONCES
# p-value < significancia

# A un nivel de confianza del 99% existe evidencia estadística para rechazar 
# la hipótesis nula; es decir los datos datos de Anderson muestran que:
# al menos en una de las especies hay diferencia en el promedio del ancho del
# sepalo.
#Por lo tanto: los datos !Los datos no estan en linea con este nuevo estudio!


#####CONCLUSION########################

# LOS ESTUDIOS i), ii) y iv)  MOSTRARON QUE
# CON UN NC 99% EXISTE EVIDENCIA ESTADISTICA PARA CONCLUIR QUE:
# LOS DATOS DE ANDERSON !NO ESTAN EN LINEA CON ESTOS NUEVOS ESTUDIOS!

# EL ESTUDIO iii) MOSTRO QUE
# CON UN NC 99% EXISTE EVIDENCIA ESTADISTICA PARA CONCLUIR QUE:
# LOS DATOS DE ANDERSON !SI ESTAN EN LINEA CON ESTE ESTUDIO!
