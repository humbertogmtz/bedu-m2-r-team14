# Postwork Sesión 4. Equipo 14.
"Utilizando la variable total_intl_charge de la base de datos telecom_service.csv de la sesión 3, 
realiza un análisis probabilístico. Para ello, debes determinar la función de distribución de 
probabilidad que más se acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas 
descriptivas o técnicas de visualización."

library(DescTools)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
str(df)
var = df$total_intl_charge
summary(df)
# No hay datos faltantes

# Calcular media y desviacion estandar
media <- mean(var)
mediana <- median(var)
moda <- Mode(var)
# Como media = mediana = moda, los datos se ajustan a una distribución normal

ds <- sd(var)

# Graficar para observar la distribución de probabilidad de los datos
barplot(table(var) / length(var),
        main = "Distribucion de probabilidad",
        xlab = "total_intl_charge (USD)")

# Se observa una distribución Normal


"Una vez que hayas seleccionado el modelo, realiza lo siguiente:

1. Grafica la distribución teórica de la variable aleatoria total_intl_charge"
x <- seq(-3.5, 3.5, 0.01)*ds + media
y <- dnorm(x, mean = media, sd = ds) 

plot(x, y, type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal", 
     sub = expression(paste(mu == 2.76, " y ", sigma == 0.75)))

curve(dnorm(x, mean = media, sd = ds ), from = 0, to = 6, 
            col='blue', main = "DISTRIBUCION DE PROBABILIDAD NORMAL",
            ylab = "FRECUENCIA RELATIVA", xlab = "TOTAL DE CARGO INTERNACIONAL \n mu = 2.76  sigma = 0.75")


"2. ¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?"
pnorm(q = 1.85, mean = media, sd = ds) 
# = 0.1125002

"3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?"
pnorm(q = 3, mean = media, sd = ds, lower.tail = FALSE) 
# = 0.3773985
  
"4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 
2.35usd y 4.85 usd?"
pnorm(q = 4.85, mean = media, sd = ds) - pnorm(q = 2.35, mean = media, sd = ds)
# = 0.7060114
  
"5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más 
alto que podría esperar?"
# Si se busca encontrar el total de cargos internacionales de los valores altos
# se toma la cola superior
qnorm(p = 0.48, mean = media, sd = ds, lower.tail = FALSE) 
# = 2.802386 USD

# Pero si se busca encontrar el total de cargos internacionales de los valores bajos
# se toma la cola inferior
qnorm(p = 0.48, mean = media, sd = ds, lower.tail = TRUE)
# = 2.726777 USD

"6. ¿Cuáles son los valores del total de cargos internacionales que dejan exactamente 
al centro el 80% de probabilidad?"
qnorm(p = 0.1, mean = media, sd = ds); qnorm(p = 0.9, mean = media, sd = ds)
# = 1.798583 USD y 3.73058 USD
