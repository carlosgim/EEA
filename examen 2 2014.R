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
library(pROC)
options(scipen = 9999)

---#carga de datos----

Sick <- read.delim("E:/Maestria/EEA/Sick.txt")

----#1.1.1-----

'1.1.1 Separe las poblaciones en entrenamiento y validación en forma aleatoria con los %

que le fuera indicado (genere una nueva semilla aleatoria varias veces e indique el valor de

la semilla en el software que utilizó). Indique que cantidad de casos quedaron para cada

ambiente.'
Sick$clase <- as.factor(Sick$clase)
summary(Sick)

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

----#1.1.3----

'Calcular el AUC (Curva ROC) en entrenamiento y validación indicando el total de

casos de cada una de las clases.'


sick_dataset_training.m1 <- glm(clase~ sex+sick+pregnant+query_hypothyroid+hypopituitary+psych+T3+TT4+T4U+referral_source, family=binomial, data = sick_dataset_training)
pred.testing <- predict(sick_dataset_training.m1, sick_dataset_testing[,-24], type="response")
pred.testing <- ifelse(pred.testing >= .5, 1,0)
table("predicho"=pred.testing, "observado"=sick_dataset_testing[,"clase"])

probpred <- predict(sick_dataset_training.m1, sick_dataset_testing, type=c("response"))
g <- roc(clase ~ probpred, data = sick_dataset_testing)
plot(g, col="red") 


probpred.training <- predict(sick_dataset_training.m1, type=c("response"))
g.training <- roc(clase ~ probpred.training, data = sick_dataset_training)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )

---#1.1.4----

'Selecciones el 20% de los pacientes en el ambiente de validación de acuerdo a la

siguiente lógica. Entregue los resultados indicados:

 Al azar e indique la cantidad de pacientes que se encontraban enfermos

 Utilizando el modelo desarrollado en el punto 1.1.2 e indique la cantidad de

pacientes que se encontraban enfermos.'

set.seed( 12454 )
sick_inTraining <- createDataPartition( sick_dataset_testing$clase, p = .20, list = FALSE)
sick_dataset_testing_sample  <- sick_dataset_testing[sick_inTraining,]

summary(as.factor(sick_dataset_testing_sample$clase))
