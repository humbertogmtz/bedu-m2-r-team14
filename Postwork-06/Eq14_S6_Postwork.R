"Desarrollo
Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las ventas de un producto 
particular, y el conjunto de datos con el que disponemos son datos de publicidad que consisten en las ventas
de aquel producto en 200 diferentes mercados, junto con presupuestos de publicidad para el producto en cada uno 
de aquellos mercados para tres medios de comunicación diferentes: TV, radio, y periódico. 

No es posible para nuestro cliente incrementar directamente las ventas del producto. 

Por otro lado, ellos pueden controlar el gasto en publicidad para cada uno de los tres medios de comunicación. 

Por lo tanto, si determinamos que hay una asociación entre publicidad y ventas, 

entonces podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, y así
indirectamente incrementar las ventas.

En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado para predecir las
ventas sobre la base de los tres presupuestos de medios de comunicación. 

Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y 
elija el modelo más adecuado siguiendo los procedimientos vistos

Considera:
  
Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)"

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")

library(dplyr)
library(ggplot2)

#Validamos estructura de la información
str(adv)
summary(adv)
head(adv)
#La variable dependiente es cuantitativa, por lo tanto, usamos un modelo de regresión lineal, adicional,
#todos los datos están completos.


#Validamos correlación entre variables
adv.select <- select(adv,Sales,TV,Radio, Newspaper)
round(cor(adv.select),4)

#La correlación entre las ventas y radio (.3496) y newspaper (.1580), es baja; mientras que con TV es alta (.9012)

pairs(~ Sales + TV + Radio + Newspaper, 
      data = adv.select, gap = 0.4, cex.labels = 1.5)

#Observamos una relación aproximadamente lineal entre la variable Sales y TV, con lo cual, podemos ajustar
#a un modelo de regresión lineal simple con la función lm
# Y = beta0 + beta1*TV + beta2*Radio + beta3*Newspaper + e


"Estimación por mínimos cuadrados"

#Modelo 1: considerando todas las variables

attach(adv)

ml1 <- lm(Sales ~ TV + Radio + Newspaper)
ml1
summary(ml1)

#Por cada incremento unitario en TV las ventas se incrementan en .05
#Por cada incremento unitario en Radio las ventas se incrementan en .1
#Por cada incremento unitario en Newspaper las ventas se incrementan en .0003

#Valido B's para hacerlo poblacional

#Planteamiento de Hipotesis
#Ho: B=0
#Ha: B=!0

#Dado que los p-values son muy pequeños para intercepto, TV y Radio se rechaza
#la Ho a todos los niveles de significancia, es decir, B es diferente de cero

#Dado que el p-value es mayor para todos los niveles de significancia en Newspaper
#entonces no rechazo Ho, es decir, B3=0

#Este modelo me esta explicando las variaciones al .9026 acorde al Multiple R-squared con una R-ajustada de .9011


#Validación de errores

StanRes3 <- rstandard(ml1)


par(mfrow = c(2, 2))

plot(TV, StanRes3, ylab = "Residuales Estandarizados")
plot(Radio, StanRes3, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes3, ylab = "Residuales Estandarizados")

#Graficamente se observa que los errores NO tienen correlación significativa con las variables

qqnorm(StanRes3)
qqline(StanRes3) #agregar línea de referencia

#Los residuos se ditribuyen como una normal

dev.off()

shapiro.test(StanRes3)

"Ho: La variable distribuye como una normal
Ha: La variable no distribuye como una normal"

#A un nivel de confianza de 90%, el p-value (0.001339) es menor al nivel de significancia es .1, en cuyo caso rechazo Ho, es decir, la variable no se
#distribuye como una normal


#A un nivel de confianza de 95%, el p-value (0.001339) es menor al nivel de significancia es .05, en cuyo caso rechazo Ho, es decir, la variable no se
#distribuye como una normal


#A un nivel de confianza de 99%, el p-value (0.001339) es menor al nivel de significancia es .01, en cuyo caso rechazo Ho, es decir, la variable no se
#distribuye como una normal

#----------------------------------------------------------------------------------------------------------------------------------

#Modelo 2: Eliminamos Newspaper

ml3 <- update(ml1, ~.- Newspaper)
ml3
summary(ml3)

#Este modelo me explica al .9026 con una R-ajustaada de .9016, sin cambio significativo vs el modelo 1

#Validación de B's

#Planteamiento de Hipotesis
#Ho: B=0
#Ha: B=!0

# Los p-value para todas las variables son menores a todos los niveles de significancia, con lo cual, rechazo Ho, es decir, 
#Todas las B's son diferentes de cero.


#Validación de errores

StanRes5 <- rstandard(ml3)


par(mfrow = c(2, 2))

plot(TV, StanRes5, ylab = "Residuales Estandarizados")
plot(Radio, StanRes5, ylab = "Residuales Estandarizados")

#Graficamente se observa que los errores NO tienen correlación significativa con las variables

qqnorm(StanRes3)
qqline(StanRes3) #agregar línea de referencia

#Los residuos se ditribuyen como una normal

dev.off()

shapiro.test(StanRes5)

"Ho: La variable distribuye como una normal
Ha: La variable no distribuye como una normal"

#A un nivel de confianza de 90%, el nivel de significancia es .1 siendo menor el p-value, en cuyo caso rechazo Ho, 
#es decir, la variable no se distribuye como una normal


#A un nivel de confianza de 95%, el nivel de significancia es .05 siendo menor el p-value, en cuyo caso rechazo Ho, 
#es decir, la variable no se distribuye como una normal


#A un nivel de confianza de 99%, el nivel de significancia es .01, siendo menor el p-value, en cuyo caso rechazo Ho, 
#es decir, la variable no se distribuye como una normal


#-----------------------------------------------------------------------------------------------------------------------------

#Modelo 3: Eliminamos Radio y Newspaper

ml2 <- update(ml1, ~.- Radio - Newspaper)
ml2
summary(ml2)

#Este modelo me explica al .8122 con una R-ajustada de .8112

#Validación de B's

#Planteamiento de Hipotesis
#Ho: B=0
#Ha: B=!0

# Los p-value para todas las variables son menores a todos los niveles de significancia, con lo cual, rechazo Ho, es decir, 
#Todas las B's son diferentes de cero.


#Validación de errores

StanRes4 <- rstandard(ml2)


par(mfrow = c(2, 2))

plot(TV, StanRes4, ylab = "Residuales Estandarizados")

#Graficamente se observa que los errores NO tienen correlación significativa con las variables

qqnorm(StanRes4)
qqline(StanRes4) #agregar línea de referencia

#Mis residuos se ditribuyen como una normal

dev.off()

shapiro.test(StanRes4)

"Ho: La variable distribuye como una normal
Ha: La variable no distribuye como una normal"

#A un nivel de confianza de 90%, el nivel de significancia es .1 siendo mayor el p-value, en cuyo caso no rechazo Ho, 
#es decir, la variable se distribuye como una normal


#A un nivel de confianza de 95%, el nivel de significancia es .05 siendo mayor el p-value, en cuyo caso no rechazo Ho, 
#es decir, la variable se distribuye como una normal


#A un nivel de confianza de 99%, el nivel de significancia es .01, siendo mayor el p-value en cuyo caso no rechazo Ho, 
#es decir, la variable se distribuye como una normal



#-----------------------------------------------------------------------------------------------------------------------------

#Conclusión 

#Se sugiere se tome el modelo 3, ya que cuenta con un nivel aceptable de explicación y se cumplen todos los supuestos
#y pruebas de hipótesis.













