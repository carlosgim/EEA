----#librerias----
library(ggplot2)
library(stringr)
library(gridExtra)


----#carga archivo----
salarios <- read.csv("E:/Maestria/EEA/salarios.csv", sep=";",stringsAsFactors = FALSE)


----#Ejercicio 1----
#salarios a numericos

salarios_v2 <- apply(salarios,2, function (x) gsub(pattern = "$",replacement = "",x,fixed = TRUE))
salarios_v2 <- apply(salarios_v2,2, function (x) gsub(pattern = ".",replacement = "",x,fixed = TRUE))
salarios_v2 <- as.data.frame(salarios_v2)
salarios_v2$salini <- as.numeric(salarios_v2$salini)
salarios_v2$salario <- as.numeric(salarios_v2$salario)
salarios_v2$id <- as.numeric(salarios_v2$id)
salarios_v2$expprev <- as.numeric(salarios_v2$expprev)


#a. Hacer gráficos de nube de puntos para la variable ‘salario’ versus las otras variables.

g1 <- ggplot(salarios_v2,mapping = aes(x=salario,y=salini))+geom_point()
g2 <- ggplot(salarios_v2,mapping = aes(x=salario,y=expprev))+geom_point()

grid.arrange(g1,g2)
