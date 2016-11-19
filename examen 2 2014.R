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
library(funModeling)
library(VIM)
library(mice)
library(ResourceSelection)
library(lmtest)#goodness of fit
library(pscl) #pseudo R2
options(scipen = 9999)

---#carga de datos----

Sick <- read.delim("E:/Maestria/EEA/Sick.txt")
Sick <- read.csv("C:/Users/Pablo/Google Drive/Maestria/EEA/Clase 12/Sick.csv", sep=";")

rownames(Sick) <- Sick$caso
Sick$caso <- NULL
----#1.1.1-----

'1.1.1 Separe las poblaciones en entrenamiento y validación en forma aleatoria con los %

que le fuera indicado (genere una nueva semilla aleatoria varias veces e indique el valor de

la semilla en el software que utilizó). Indique que cantidad de casos quedaron para cada

ambiente.'

#Ver faltantes
Sick$clase <- as.factor(Sick$clase)
Sick$sex <- as.factor(Sick$sex)
summary(Sick)

mydatastatus <- df_status(Sick)

aggr_plot <- VIM::aggr(Sick, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#imputar faltantes

tempData <- mice(Sick[,-2],m=5,maxit=50,meth='norm.predict',seed=500)
summary(tempData)
densityplot(tempData)

Sick_completo <- complete(tempData)
Sick_completo$sex <- Sick$sex

Sick <- Sick[,!names(Sick) %in% mydatastatus$variable[mydatastatus$p_zeros==100]]
Sick <- Sick[,!names(Sick) %in% mydatastatus$variable[mydatastatus$p_na==100]]

md.pattern(Sick)

set.seed( 12454 )
sick_inTraining <- createDataPartition( Sick_completo$clase, p = .70, list = FALSE)
sick_dataset_training <- Sick_completo[ sick_inTraining,]
sick_dataset_testing  <- Sick_completo[-sick_inTraining,]


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
pR2(sick.m2) #ver mcfadden

----#1.1.3----

'Calcular el AUC (Curva ROC) en entrenamiento y validación indicando el total de

casos de cada una de las clases.'


sick_dataset_training.m1 <- glm(clase~ sex+sick+pregnant+query_hypothyroid+hypopituitary+psych+T3+TT4+T4U+referral_source, family=binomial, data = sick_dataset_training)

pred.testing <- predict(sick.m2, sick_dataset_testing[,!colnames(sick_dataset_testing) %in% c("clase")], type="response")
pred.testing <- ifelse(pred.testing >= .5, 1,0)
table("predicho"=pred.testing, "observado"=sick_dataset_testing[,"clase"])


probpred <- predict(sick.m2, sick_dataset_testing, type=c("response"))
g <- roc(clase ~ probpred, data = sick_dataset_testing)
plot(g, col="red",legacy.axes=TRUE) 

roc
probpred.training <- predict(sick.m2,sick_dataset_training, type=c("response"))
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
'3 pacientes enfermos'

pred.testing.sample <- predict(sick.m2, sick_dataset_testing_sample, type="response")
pred.testing.sample <- ifelse(pred.testing.sample >= .5, 1,0)
summary(as.factor(pred.testing.sample))

'predice 2 pacientes'

---#1.1.5-----

'•	Indicar en cuanto sería el impacto en modificar una unidad de por lo menos una variable continua del modelo. '

summary(sick.m2)
confint(sick.m2)

(exp(-6.62431)-1)*100

'•	Indicar si hay puntos incluyentes con COOK.'

par(mfrow=c(1,2))
plot(sick.m2,which=4)
plot(sick.m2,which=5)

par(mfrow=c(1,1))

'•	Indicar que método de selección de variables se utilizó y explicar su funcionamiento.'

'utilice backward, y luego con las variables de ese modelo elegí las más significativas para ver si mejoraba el modelo'

'•	Mostrar el estadístico de Hosmer-Lemeshow en el último paso del modelo.'

hl <- hoslem.test(sick.m2$y,fitted(sick.m2),g=10) #g determina la cantidad de grupos en los que se divide el dataset
hl #p-value 0.006, es significativo por lo que tengo evidencia de que el modelo se ajusta mal

cbind(hl$expected,hl$observed)
#muestra la probabilidad partida en 1, la cantidad de esperados 0 y 1 y cuantos hay en realidad.
'                  yhat0          yhat1 y0 y1
[2.22e-16,1.68e-06] 66.99998  0.00002051153 67  0
(1.68e-06,1.46e-05] 66.99957  0.00043252282 67  0
(1.46e-05,7.06e-05] 65.99766  0.00233700533 66  0
(7.06e-05,0.000185] 66.99195  0.00805191413 67  0
(0.000185,0.000523] 66.97689  0.02311118010 67  0
(0.000523,0.00145]  65.93686  0.06314267208 65  1
(0.00145,0.00366]   66.83987  0.16012862068 66  1
(0.00366,0.0133]    65.50951  0.49048529211 65  1
(0.0133,0.085]      64.54733  2.45266989359 67  0
(0.085,0.996]       36.20038 30.79962038820 36 31'

'•	Proponer una comparación de modelos: 
Mod1: TSH, T3, sick y TT4 
Mod2: TSH, T3, sick, TT4 y queryhypo, queryhiper
Que cuenten cómo se hace la comparación usando devianzas. 
'
mod1 <- glm(clase ~ TSH+T3+sick+TT4,data = sick_dataset_training,family = 'binomial' )
mod2 <- glm(clase ~ TSH+T3+sick+TT4+query_hypothyroid+query_hyperthyroid,data = sick_dataset_training,family = 'binomial' )


summary(mod1)
summary(mod2)
pchisq(deviance(mod1),deviance(mod2),lower=F)
lrtest(mod1, mod2)


anova(mod1,mod2,test = "Chisq")

#esto dice que a pesar de que el modelo dos tiene menor deviance, no hay una mejora significativa, por lo que preferible quedarse con el modelo 1 que es más simple.

barplot(cbind(modelo1=AIC(mod1),modelo2=AIC(mod2)),ylab="AIC")

pchisq(deviance(mod1),df.residual(mod1),lower=F)


----#KS----
#load library
library(ROCR)

#score test data set
pred<-prediction(probpred,sick_dataset_testing$clase)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#this code builds on ROCR library by taking the max delt
#between cumulative bad and good rates being plotted by
#ROCR
max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])


#otra manera

ks.test(x = test$prediccion[test$clase == 1], y = test$prediccion[test$clase == 0])
