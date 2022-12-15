# Postwork Sesión 2 Equipo 14.

#### Objetivo

"- Conocer algunas de las bases de datos disponibles en `R`
- Observar algunas características y manipular los DataFrames con `dplyr`
- Realizar visualizaciones con `ggplot`
#### Requisitos

1. Tener instalado R y RStudio
2. Haber realizado el prework y estudiado los ejemplos de la sesión."

#### Desarrollo

"1) Inspecciona el DataSet iris disponible directamente en la librería de ggplot. 
Identifica las variables que contiene y su tipo, asegúrate de que no hayan datos
faltantes y que los datos se encuentran listos para usarse."

#Llamamos a la libreria ggplot2 y dplyr
library(ggplot2)  
library(dplyr)  

# Validamos estructura del dataset iris.
str(iris)
# 150 obs. con 5 var.
head(iris)

# Aseguramos que no haya datos faltantes con la funcion:
sum(complete.cases(iris))  
# 150 casos completos
# No hay datos faltantes.

"2) Crea una gráfica de puntos que contenga `Sepal.Lenght` en el eje horizontal, 
`Sepal.Width` en el eje vertical, que identifique `Species` por color y que el tamaño 
de la figura está representado por `Petal.Width`. 
Asegúrate de que la geometría contenga `shape = 10` y `alpha = 0.5`."


g <- ggplot(iris, aes(x = Sepal.Length, y=Sepal.Width, color = Species,
                    size = Petal.Width)) + 
     geom_point(shape=10, alpha=0.5) 
g
"3) Crea una tabla llamada `iris_mean` que contenga el promedio de todas las variables 
agrupadas por `Species`."

iris.mean <- iris %>%
  group_by(Species) %>%
  summarize_all(mean)
 
View(iris.mean)

"4) Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar 
los promedios en la visualización. Asegúrate que el primer argumento de la geometría 
sea el nombre de tu tabla y que los parámetros sean `shape = 23`, `size = 4`, 
`fill = 'black'` y `stroke = 2`. También agrega etiquetas, temas y los cambios 
necesarios para mejorar tu visualización."


g <- g + geom_point(data = iris.mean,
                    shape = 23, size = 4, fill = 'black', stroke = 2)

#Ajusto la región a graficar.
g <- g + scale_y_continuous(limits = c(2, 4.5),
                            breaks = seq(2,4.5, 0.5)) +
  scale_x_continuous(breaks = seq(4, 8, 0.5))

# Renombrar las etiquetas de variable de species.
g <- g + scale_color_discrete("SPECIES", 
                              labels = c("Setosa", "Versicolor","Virginica"))

# Agregar título y nombre a los ejes
g <- g + labs(title = "FLOWERS FROM SPECIES OF IRIS",
              x = "SEPAL LENGHTH",
              y = "SEPAL WIDHT") + 
  theme_classic()


# Guardar la gráfica
ggsave("iris_plot.jpg", plot = g)




