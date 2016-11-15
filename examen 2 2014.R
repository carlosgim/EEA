#segundo parcial 2014

----#librerias----
library(caret)
library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(CombMSC)
library(ggplot2)
library(caret)
library(car)
library(ellipse)
library(QuantPsyc)
library(leaps)
library(xlsx)
library(dummies)
options(scipen = 9999)

---#carga de datos----

Sick <- read.delim("E:/Maestria/EEA/Sick.txt")

----#1.1.1-----

'1.1.1 Separe las poblaciones en entrenamiento y validación en forma aleatoria con los %

que le fuera indicado (genere una nueva semilla aleatoria varias veces e indique el valor de

la semilla en el software que utilizó). Indique que cantidad de casos quedaron para cada

ambiente.'

Sick

set.seed( 12454 )
sick_inTraining <- createDataPartition( Sick$clase, p = .70, list = FALSE)
sick_dataset_training <- Sick[ sick_inTraining,]
sick_dataset_testing  <- Sick[-sick_inTraining,]


dim(sick_dataset_training)
dim(sick_dataset_testing)

----#1.1.2-----

'Formule el mejor modelo posible de regresión logística de acuerdo a la consigna y

preséntelo. Se debe entregar el modelo (con la asignación de las categorías originales

correspondientes que el software entregó para cada una de las variables dummy)'
str(Sick)
cont_dumm <- dummy(Sick$sex,drop = TRUE)[,1]
sick_sn <- cbind(Sick,cont_dumm)
sick_sn$sex <- NULL

sick.m1 <- glm(clase~., family=binomial, data = sick_dataset_training)
summary(sick.m1)

sick.m2 <- step(sick.m1,direction="backward",trace=FALSE)

summary(sick.m2)
