######Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México#######
library(dplyr)
library(DescTools)
library(ggplot2)
library(moments)
install.packages("table1") 
library(table1)
install.packages("vcd") 
library(vcd)


"OBJETIVO
-Realizar un análisis estadístico completo de un caso
-Publicar en un repositorio de Github el análisis y el código empleado
REQUISITOS
-Haber realizado los works y postworks previos
-Tener una cuenta en Github o en RStudioCloud
DESARROLLO
Un centro de salud nutricional está interesado en analizar estadísticamente y 
probabilísticamente los patrones de gasto en alimentos saludables y no saludables 
en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene
recursos financieros extrar al ingreso y en si presenta o no inseguridad alimentaria.
Además, está interesado en un modelo que le permita identificar los determinantes 
socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012)
levantada por el Instituto Nacional de Salud Pública en México. La mayoría de las
personas afirman que los hogares con menor nivel socioeconómico tienden a gastar
más en productos no saludables que las personas con mayores niveles socioeconómicos
y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad 
alimentaria.

La base de datos contiene las siguientes variables:

-nse5f (Nivel socieconómico del hogar): 1 Bajo, 2 Medio bajo, 3 Medio, 4 Medio alto, 5 Alto
-area (Zona geográfica): 0 Zona urbana, 1 Zona rural
-numpeho (Número de persona en el hogar)
-refin (Recursos financieros distintos al ingreso laboral): 0 no, 1 sí
-edadjef (Edad del jefe/a de familia)
-sexoje (Sexo del jefe/a de familia): 0 Hombre, 1 Mujer
-añosedu (Años de educación del jefe de familia)
-ln_als (Logarítmo natural del gasto en alimentos saludables)
-ln_alns (Logarítmo natural del gasto en alimentos no saludables)
-IA (Inseguridad alimentaria en el hogar): 0 No presenta IA, 1 Presenta IA"


"1) Plantea el problema del caso"

# Con base a los estudios realizados en la encuesta nacional de salud y nutrición
# en México (2012), se plantea el siguiente caso:

# i) Establecer si existe correlación entre los gastos en alimentos
#----saludables ( o no saludables) con respecto al nivel socio económico,
# ---así mismo si hay recursos financieros extras y  si existe inseguridad alimentaria.

 
# ii) Determinar las posibles causas de la inseguridad alimentaria (IA).

# iii) Sera verdad que los hogares con menor nivel socioeconómico tienden a gastar
#------más en productos no saludables que las personas con mayores niveles 
#------socioeconómicos por lo que presentan cierta inseguridad alimentaria.



"2) Realiza un análisis descriptivo de la información"

#--------------------- Leer base de datos. ------------------------------------#

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")


#--------------------- Revisar datos faltantes.--------------------------------#


str(df)
#-- 40809 observables --#
sum(complete.cases(df))
#-- 20280 casos completos --#

#-------------------- Indexar sólo filas completas-----------------------------#

# No podemos asegurar que los valores con NA den certeza sobre el resto de la 
# información por lo cual se omiten para el presente caso.

df.clean <- df[complete.cases(df),]
str(df.clean)
View(df.clean)

#-- 20280 observables --#

#-------------- Revisar estructura de las variables.---------------------------#


#--------- Transformar variables a su tipo y escala correspondiente.-----------#

# Variable nse5f (Nivel socieconómico del hogar)
#Cualitativa ordinal: 1 Bajo, 2 Medio bajo, 3 Medio, 4 Medio alto, 5 Alto

df.clean$nse5f <- factor(df.clean$nse5f, labels = c("Bajo", "MedioBajo","Medio",
                                                    "MedioAlto", "Alto"))
str(df.clean$nse5f)


# Variable refin (Recursos financieros distintos al ingreso laboral) 
# Cualitativa nominal con niveles: 0 = NO, 1 = SI.
df.clean$refin <- factor(df.clean$refin, labels = c("No", "Si"))
str(df.clean$refin)


# Variable IA (Inseguridad alimentaria en el hogar)
# Cualitativa nominal con niveles: 0 No presenta IA, 1 Presenta IA"
df.clean$IA <- factor(df.clean$IA, labels = c("No", "Si"))
str(df.clean$IA)

df.clean$area <- factor(df.clean$area, labels = c("Urbana", "Rural"))
str(df.clean$area)

df.clean$sexojef <- factor(df.clean$sexojef, labels = c("Hombre", "Mujer"))
str(df.clean$sexojef)

#Resumen de las variables
summary(df.clean)

#-------------Variables dependientes-------------------------------------------#

#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)

#-------------Medidas de tendencia central.....................................#

# Gastos alimentos saludables
promedio.ln_als <- mean(df.clean$ln_als)
#--6.191992
mediana.ln_als <- median(df.clean$ln_als)
#--6.27382
moda.ln_als <- Mode(df.clean$ln_als)
#--6.3099

#INTERPRETACION
# MODA > MEDIANA > MEDIA
#LA DISTRIBUCION ESTA SESGADA A LA IZQUIERDA.

skewness(df.clean$ln_als)
# = -1.191836 < 0 por lo tanto hay sesgo a la izquierda

#Gastos alimentos no saludables
promedio.ln_alns <- mean(df.clean$ln_alns)
#-- =4.1188
media.ln_alns <- median(df.clean$ln_alns)
#-- =4.0073
moda.ln_alns <- Mode(df.clean$ln_alns)
#-- =3.4011

#INTERPRETACION
# MODA < MEDIANA < MEDIA
#LA DISTRIBUCION ESTA SESGADA A LA DERECHA.

skewness(df.clean$ln_alns)
# = 0.2477 > 0 ligero sesgo a la derecha.

#---------------Medidas de dispersión central----------------------------------#

#Gasto alimenticio saludable.
ds.ln_als <- sd(df.clean$ln_als)
# = 0.6885
# Tenemos una desviación estandar del 10% con respecto a la media de los datos

#Gasto alimenticio no saludable.
ds.ln_alns <- sd(df.clean$ln_alns)
# = 1.0414

#----------Tablas de frecuencia para las variables-----------------------------#

# Tabla de porcentajes de las variables
table1::table1(~IA + nse5f + refin + ln_alns + ln_als + area + numpeho +
                 edadjef + sexojef + añosedu, 
               data = df.clean, na.rm = TRUE, digits = 1, format.number = TRUE)

# Variables vs IA
table1::table1(~ nse5f + refin + ln_alns + ln_als + area + numpeho +
                 edadjef + sexojef + añosedu | IA, data = df.clean, 
               na.rm = TRUE, digits = 1, format.number = TRUE)

# IA vs nse5f
table1::table1(~IA | nse5f, data = df.clean, na.rm = TRUE, digits = 1, format.number = TRUE) 

#CALCULEMOS EL NUMERO DE CLASES NECESARIOS PARA EL HISTOGRAMA
# SE USARA LA REGLA DE STURGES K = 1+3.3LOG(N)
k=ceiling(1+(3.3*log10(length(df.clean$ln_als))))
# k = 16
k1=ceiling(1+(3.3*log10(length(df.clean$ln_alns))))
# k = 16

h <- ggplot(df.clean, aes(x = ln_als)) +               
  geom_histogram(bins = k, color = "Blue", alpha = 0.5)
# En la gráfica se observa sesgo a la izquierda.

h1 <- ggplot(df.clean, aes(x = ln_alns)) +               
  geom_histogram(bins = k, color = "Blue", alpha = 0.5)
# En la gráfica se observa un ligero sesgo a la derecha.

#--------Comportamiento del gasto alimenticio por clase socio económica--------#

#Gasto saludable
b1 <- ggplot(df.clean, aes( y=ln_als, x=nse5f)) + geom_boxplot()

#Gasto no saludable
b2 <- ggplot(df.clean, aes( y=ln_alns, x=nse5f)) + geom_boxplot()

#En ambos boxplot se observa que a mayor nivel socio economico (nse5f) 
# hay mayor gasto tanto en alimento saludable como en alimento no saludable.

#---Comportamiento del gasto alimenticio por recursos financieros adicionales--#

# Gasto saludable.
b3 <- ggplot(df.clean, aes( y=ln_als, x=refin)) + geom_boxplot()

#Gasto no saludable.
b4 <- ggplot(df.clean, aes( y=ln_alns, x=refin)) + geom_boxplot()
#En ambos boxplot se observa que no influye en el gasto alimentisio saludable 
# y no saludable que la persona reciva o no recurso financiero adicional.


#--Comportamiento del gasto alimenticio por si presenta o no inseguridad alimentaria--#

# Gasto saludable.

b5 <- ggplot(df.clean, aes( y=ln_als, x=IA)) + geom_boxplot()
b6 <- ggplot(df.clean, aes( y=ln_alns, x=IA)) + geom_boxplot()
#En ambos boxplot se observa que en cualquier grupo (inseguridad o no inseguridad alimentaria)
#el gasto en alimentos no presenta una diferencia significativa.

"3) Calcula probabilidades que nos permitan entender el problema en México"

"El 71.1% de la población Mexicana presenta inseguridad alimentaria, si se selecciona
un grupo de 100 personas"#Distribución de las probabilidades

set.seed(14)

binom <- rbinom(n = 10000, size = 100, prob = 0.711)
barplot(table(binom) / length(binom),
        main = "Distribucion Binomial",
        xlab = "# de personas seleccionadas")
        
"¿Cuál es la probabilidad de que 50 personas o  mas de clase alta  presente
inseguridad alimentaria?"
pbinom(q = 70, size = 100, prob = 0.497, lower.tail = FALSE)
# = 1.2384e-05
"¿Cuál es la probabilidad de que 50 personas de clase media  presente
inseguridad alimentaria?"
pbinom(q = 70, size = 100, prob = 0.76, lower.tail = FALSE )
# = 89.91%
"¿Cuál es la probabilidad de que 50 personas de clase baja  presente
inseguridad alimentaria?"
pbinom(q = 70, size = 100, prob = 0.86, lower.tail = FALSE )
# = 99.99%


"Plantea hipótesis estadísticas y concluye sobre ellas para entender 
el problema en México"

#La mayoría de las personas afirman que los hogares con menor nivel socioeconómico 
#tienden a gastar más en productos no saludables que las personas con 
#mayores niveles socioeconómicos


"Planteamiento de hipótesis:

Ho: prom_ln_alns_baja <= prom_ln_alns_alta 

Ha: prom_ln_alns_baja > prom_ln_alns_alta"

var.test(df.clean[df.clean$nse5f == "Bajo", "ln_alns"], 
         
         df.clean[df.clean$nse5f == "Alto", "ln_alns"], 
         
         ratio = 1, alternative = "two.sided")
#p-value = 6.199e-13 < significancia (0.05)
#A un nivel de confianza del 95% existe evidencia estadística
#para rechazar la hipotesis nula.
#Por lo tanto las varianzas son diferentes.



t.test(x = df.clean[df.clean$nse5f == "Bajo", "ln_alns"], y = df.clean[df.clean$nse5f == "Alto", "ln_alns"],
       
       alternative = "greater",
       
       mu = 0, var.equal = FALSE)
#p-value = 1 > significancia (0.05)
#A un nivel de confianza del 95% no existe evidencia estadística
#para rechazar la hipotesis nula.
#Por lo tanto: prom_ln_alns_baja <= prom_ln_alns_alta.




#----------------------------------------------------------------------------------------------------------------

#El promedio de gasto en alimentos no saludables en familias que presentan IA
#es mayor que en familias que no tienen IA.

"Planteamiento de hipótesis:

Ho: prom_ln_alns_IA <= prom_ln_alns_NIA 
Ha: prom_ln_alns_IA > prom_ln_alns_NIA"

var.test(df.clean[df.clean$IA == "Si", "ln_alns"], 
         
         df.clean[df.clean$IA == "No", "ln_alns"], 
         
         ratio = 1, alternative = "two.sided")
#p-value = 1.75e-07 < significancia (0.05)
#A un nivel de confianza del 95% existe evidencia estadística
#para rechazar la hipotesis nula.
#Por lo tanto las varianzas son diferentes.


t.test(x = df.clean[df.clean$IA == "Si", "ln_alns"], 
       y = df.clean[df.clean$IA == "No", "ln_alns"],
       alternative = "greater",
       mu = 0, var.equal = FALSE)
#p-value = 1 > significancia (0.05)
#A un nivel de confianza del 95% no existe evidencia estadística
#para rechazar la hipotesis nula.
#Por lo tanto: prom_ln_alns_IA <= prom_ln_alns_NIA .

"El promedio de gasto en alimentos no saludables en familias cuando el jefe
de familia es mujer es mayor que donde el jefe de familia es hombre."



"Planteamiento de hipótesis:
Ho: prom_ln_alns_m <= prom_ln_alns_h 
Ha: prom_ln_alns_m > prom_ln_alns_h"
#SI pvalue >= significancia NO RECHAZO HIPOTESIS NULA Ho
# SI pvalue < significancia RECHAZO HIPOTESIS NULA

#Prueba de varianzas
var.test(df.clean[df.clean$sexojef == "Mujer", "ln_alns"], 
         
         df.clean[df.clean$sexojef == "Hombre", "ln_alns"], 
         
         ratio = 1, alternative = "two.sided")
#p-value = 0.377 > significancia (0.05)
#A un nivel de confianza del 95% no existe evidencia estadística
#para rechazar la hipotesis nula.
#Por lo tanto las varianzas son iguales.

t.test(x = df.clean[df.clean$sexojef == "Mujer", "ln_alns"],
       y = df.clean[df.clean$sexojef == "Hombre", "ln_alns"],
       
       alternative = "greater",
       
       mu = 0, var.equal = TRUE)
#p-value = 1 > significancia (0.05)
#A un nivel de confianza del 95% existe evidencia estadística
#para rechazar la hipotesis nula.
#Por lo tanto: prom_ln_alns_M > prom_ln_alns_H.



#----Modelo para determinar factores sobre Inseguridad Alimentaria-------------#

"Dado que la variable dependiente es cualitativa y el número de predictores es 
amplio se usa un modelo de regresión logística múltiple."


#--------Modelo 1 (todas las variables)-----------------------------#

"En este caso se van a emplear como posibles predictores todas las variables
categóricas y la variable cualitativa ln_alns.
En el caso de la variable cuantitativa ln_als, no se tomara en cuenta debido 
a que como vimos en el boxplot tiene muchos outliers esta variable, lo que puede 
complicar bastante la creación de estos modelos además de que en el data set que
se emplea, esta variable es muy asimetrica."

# LAS Bi CONTRASTAN LAS SIGUIENTES HIPOTESIS
#Ho: Bi = 0
#Ha: Bi =! 0

#SI pvalue >= significancia NO RECHAZO HIPOTESIS NULA Ho
# SI pvalue < significancia RECHAZO HIPOTESIS NULA

"Generamos modelo a través de la función glm con las variables"
logistic.1 <- glm(IA ~ nse5f + area + numpeho + refin + edadjef + sexojef +
                    añosedu + ln_alns, 
                  data = df.clean, family = binomial)

"Validación de Betas y eliminación de variables no significativas a un nivel de
confianza del 95%"

#Observemos el p-value de las Betas.
summary(logistic.1)
#ACI = 22152

# Beta (edadjef)
# p-value = 0.3577 > significancia (0.1, 0.05, 0.01)
# A CUALQUIER NIVEL DE CONFIANZA EXISTE EVIDENCIA ESTADISTICA 
#PARA NO RECHAZAR HIPOTESIS NULA
#POR LO TANTO Beta(edadjef) = 0

"Se determina edadjef como variable no significativa
y se ejecuta nuevamente el modelo."


#---------------Modelo logistico 2 (quitando edadjef)-------------------------#

logistic.2 <- update(logistic.1, ~.- edadjef)


"Validación de Betas y eliminación de variables no significativas a un nivel de
confianza del 95%"

#Observamos el p-value de las Betas.
summary(logistic.2)
#AIC: 22151

# Beta ( area )
# p-value = 0.049 < significancia (0.05)
# AUN NIVEL DE CONFIANZA DE 95%
# EXISTE EVIDENCIA ESTADISTICA PARA RECHAZAR HIPOTESIS NULA
#POR LO TANTO Beta( area) =! 0

"Comparación de clasificación predicha y observaciones"

"Predicciones"
"Para este estudio se va a emplear un threshold de 0.5"
predicciones <- ifelse(test = logistic.2$fitted.values > 0.5, yes = "Si", no = "No")

"Matriz de confusión"
matriz_confusion <- addmargins(table(df.clean$IA, predicciones))
# Labeling
names(dimnames(matriz_confusion)) <- c("True status", "Prediction")
colnames(matriz_confusion) <- c("Fail", "Success", "Total")
rownames(matriz_confusion) <- c("Fail", "Success", "Total")
matriz_confusion

library(vcd)
mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

falso.negativo <- matriz_confusion[2,1]
verdadero.negativos <- matriz_confusion[1,1]
verdadero.positivo <- matriz_confusion[2,2]
total.muestra <- matriz_confusion[3,3]

#precicion
(verdadero.negativos + verdadero.positivo) / total.muestra 
#0.7333 (73.33%)
"El modelo es capaz de clasificar correctamente 73.33% de
las observaciones de entrenamiento."

#porcentaje de falsos negativos
falso.negativo / (falso.negativo + verdadero.positivo)
#0.0703 (7.03 %)

"El modelo atinó a los datos reales en un 73.33% y acorde al gráfico el mayor
número de errores se focaliza en: NO presenta IA"

"Como siguientes pasos podría aplicarse algún método de ajuste al modelo para 
mejorarlo, por ejemplo: Seleccionar otro threshold puede mejorar la exactitud 
del modelo. Pero eso queda fuera de los objetivos del trabajo."

#---------------CONCLUSIONES--------------------------------------------------#

"Determinación de coeficientes e interpretación"

round(exp(coef(logistic.2)),4)

"1) Mientras mayor sea la clase social se tiene menos probabilidad de presentar IA"
"2) Las familias de clase Media Baja tienen menor probabilidad de presentar
IA que los de clase Baja"
"3) Las familias de clase Media tienen menor probabilidad de presentar IA que los de clase Baja"
"4) Las familias de clase Media Alta tienen menor probabilidad de presentar IA que los de clase Baja"
"5) Las familias de clase Alta tienen menor probabilidad de presentar IA que los de clase Baja"
"6) La probabilidad de IA en area rural es menor que en area urbana"
"7) A mayor número de personas en el hogar, mayor probabilidad de presentar IA"
"8) Hay mayor probabilidad de presentar IA si las familias cuentan con recursos financieros adicionales"
"9) Hay mayor probabilidad de presentar IA si la mujer funge como jefe de familia"
"10) Hay menor probabilidad de presentar IA si el jefe de familia cuenta con mas años de educación"
"11) Hay menor probabilidad de presentar IA si aumentamos el gasto en alimentos no saludable."









#------NOTA EXTRA----------------------------------------------------------#

#Comparación de clasificación predicha y observaciones > 0.45

predicciones <- ifelse(test = logistic.2$fitted.values > 0.45, yes = "Si", no = "No")

matriz_confusion <- addmargins(table(df.clean$IA, predicciones))
# Labeling
names(dimnames(matriz_confusion)) <- c("True status", "Prediction")
colnames(matriz_confusion) <- c("Fail", "Success", "Total")
rownames(matriz_confusion) <- c("Fail", "Success", "Total")
matriz_confusion

falso.negativo <- matriz_confusion[2,1]
verdadero.negativos <- matriz_confusion[1,1]
verdadero.positivo <- matriz_confusion[2,2]
total.muestra <- matriz_confusion[3,3]

#precicion
(verdadero.negativos + verdadero.positivo) / total.muestra 
#0.7327 (73.27%)

#porcentaje de falsos negativos
falso.negativo / (falso.negativo + verdadero.positivo)
#0.038 (3.87 %)

#Comparación de clasificación predicha y observaciones > 0.55

predicciones <- ifelse(test = logistic.2$fitted.values > 0.55, yes = "Si", no = "No")

matriz_confusion <- addmargins(table(df.clean$IA, predicciones))
# Labeling
names(dimnames(matriz_confusion)) <- c("True status", "Prediction")
colnames(matriz_confusion) <- c("Fail", "Success", "Total")
rownames(matriz_confusion) <- c("Fail", "Success", "Total")
matriz_confusion

falso.negativo <- matriz_confusion[2,1]
verdadero.negativos <- matriz_confusion[1,1]
verdadero.positivo <- matriz_confusion[2,2]
total.muestra <- matriz_confusion[3,3]

#precicion
(verdadero.negativos + verdadero.positivo) / total.muestra 
#0.7290 (72.90%)

#porcentaje de falsos negativos
falso.negativo / (falso.negativo + verdadero.positivo)
#0.10 (10.88 %)
