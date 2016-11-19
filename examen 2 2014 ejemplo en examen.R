----#examen----


----#librerias----
library(subselect)
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

----#1.0 levantar dataset----

Sick <- read.csv("C:/Users/Pablo/Google Drive/Maestria/EEA/Clase 12/Sick.csv", sep=";")

rownames(Sick) <- Sick$caso
Sick$caso <- NULL  

mydatastatus <- df_status(Sick)

aggr_plot <- VIM::aggr(Sick, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


Sick <- Sick[,!names(Sick) %in% mydatastatus$variable[mydatastatus$p_zeros==100]]
Sick <- Sick[,!names(Sick) %in% mydatastatus$variable[mydatastatus$p_na==100]]

#completar nulos

tempData <- mice(Sick,m=5,maxit=50,meth='norm.predict',seed=500)
summary(tempData)
densityplot(tempData)
Sick_completo <- complete(tempData)
unique(Sick_completo$sex)
Sick_completo$sex <- ifelse(Sick_completo$sex>=0.5,1,0)
---#1.1.1----

Sick_completo$clase <- as.factor(Sick_completo$clase)
Sick_completo$sex <- as.factor(Sick_completo$sex)

set.seed( 12454 )
sick_inTraining <- createDataPartition( Sick_completo$clase, p = .70, list = FALSE)
sick_dataset_training <- Sick_completo[ sick_inTraining,]
sick_dataset_testing  <- Sick_completo[-sick_inTraining,]

dim(sick_dataset_training)
dim(sick_dataset_testing)


---#1.1.2----

sick.m1 <- glm(clase~., family=binomial, data = sick_dataset_training)
summary(sick.m1)

sick.m2 <- step(sick.m1,direction="backward",trace=TRUE)

summary(sick.m2)

---#1.1.3----


pred.testing <- predict(sick.m2, sick_dataset_testing, type="response")
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

g.training


----#1.1.4----

set.seed( 12454 )
sick_inTraining <- createDataPartition( sick_dataset_testing$clase, p = .20, list = FALSE)
sick_dataset_testing_sample  <- sick_dataset_testing[sick_inTraining,]

table(sick_dataset_testing_sample$clase)

pred.testing.sample <- predict(sick.m2, sick_dataset_testing_sample, type="response")
pred.testing.sample <- ifelse(pred.testing.sample >= .5, 1,0)
summary(as.factor(pred.testing.sample))

sample_completo <- cbind.data.frame(sick_dataset_testing_sample,pred.testing.sample)

----#1.1.5----

exp(-6.76911)


par(mfrow=c(1,2))
plot(sick.m2,which=4)
plot(sick.m2,which=5)

par(mfrow=c(1,1))

#metodo de wald
probfit <- glm(sex ~ FL + RW + CL + CW  + lFL + lRW + lCL + lCW,
               crabs,family=binomial(link=probit))
probresults <-eleaps(pHmat$mat,kmin=3,kmax=3,nsol=5,criterion="Wald",H=pHmat$H,
                     r=1,tolsym=1E-10)
lHmat <- glmHmat(sick.m2) 

wald.coef(lHmat$mat,H=lHmat$H,sick.m2$subsets,tolsym=1e-06)


#homer-Lemes

hl <- hoslem.test(sick.m2$y,fitted(sick.m2),g=10) #g determina la cantidad de grupos en los que se divide el dataset
hl #p-value 0.006, es significativo por lo que tengo evidencia de que el modelo se ajusta mal

mod.3 <- glm(clase ~ T3,sick_dataset_testing,family = "binomial")
hl.2 <- hoslem.test(mod.3$y,fitted(mod.3),g=10) #g determina la cantidad de grupos en los que se divide el dataset
hl.2 #p-value 0.006, es significativo por lo que tengo evidencia de que el modelo se ajusta mal

#•	Proponer una comparación de modelos: 
mod1 <- glm(clase ~ TSH+T3+sick+TT4,data = sick_dataset_training,family = 'binomial' )
mod2 <- glm(clase ~ TSH+T3+sick+TT4+query_hypothyroid+query_hyperthyroid,data = sick_dataset_training,family = 'binomial' )

summary(mod1)
summary(mod2)
pchisq(deviance(mod1),deviance(mod2),lower=F)
lrtest(mod1, mod2)


anova(mod1,mod2,test = "Chisq")

#•	Calcular el valor de AIC en dos modelos no anidades y usarlo para comparación de modelos.
AIC(mod1)
AIC(mod2)
barplot(cbind(modelo1=AIC(mod1),modelo2=AIC(mod2)),ylab="AIC")
