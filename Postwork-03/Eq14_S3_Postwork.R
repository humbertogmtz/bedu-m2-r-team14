# Postwork Sesion 3. Equipo 14
"Objetivo. Realizar una análisis descriptivo de las variables de un dataframe"

library(DescTools)
library(ggplot2)
library(moments)


"Utilizando el dataframe boxp.csv realiza el siguiente análisis descriptivo.
No olvides excluir los missing values y transformar las variables a su tipo 
y escala correspondiente."

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
class(df$Categoria)
str(df)
summary(df) 
#Existen 24 NA's en Mediciones

#Se eliminan los renglones que tienen esos valores
df <- na.omit(df)

#Transformar las variables
df$Categoria <- factor(df$Categoria)
df$Grupo <- factor(df$Grupo)

"1. Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`"
summary(df$Mediciones)
Mode(df$Mediciones)
#Media = 62.88
#Mediana = 49.30
#Moda = 23.3

"2.Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?"
# De acuerdo a las medidas de tendencia central, Media > Mediana > Moda
# Por lo tanto existe un sesgo hacia la derecha

# Se calcula el sesgo para comprobar el resultado
skewness(df$Mediciones)
# sesgo > 0, por lo tanto si hay sesgo hacia la derecha
"3. Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`"
sd(df$Mediciones) 
#Desviacion estandar = 53.77
#Lo que indica una alta dispersion de los datos
cuartiles <- quantile(df$Mediciones, probs = c(0.25, 0.5, 0.75))
#El 25% de las Mediciones son menores a 23.4
#El 50% de las Mediciones se encuentran por debajo de 49.3
#El 75% de las Mediciones son menores a 82.8

"4. Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?"

# Se usa la regla de Sturges K = 1+3.3LOG(N), para obtener el npuemro de bins
ks=ceiling(1+(3.3*log10(length(df$Mediciones))))
# ks = 11

ggplot(df, aes(x = Mediciones, fill = Categoria)) +
  geom_histogram(bins = ks, color = "Blue", alpha = 0.5) +
  labs(title = "Histograma de Mediciones por Categoria", 
       y = "Frecuencia")


#Se observa que la mayoria de las mediciones en las 3 categorias se encuentran en valores bajos
#siendo la categoria C1 la que mas aporta, seguido de la C2
# y todas contribuyen de igual manera con valores mayores

"5. Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría."

ggplot(df, aes(x = Categoria, y = Mediciones, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Boxplot de Distribuciones por categoria y grupo")

"¿Consideras que hay diferencias entre categorías?"
# No hay mucha diferencia entre categorias
"¿Los grupos al interior de cada categoría podrían estar generando el sesgo?"
# El grupo 0 es el que posee una mayor cantidad de datos atipicos, por lo que se puede
#inferir que es el grupo que genera el sesgo