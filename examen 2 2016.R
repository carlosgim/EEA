----#libraries----

library(foreign)
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

---#carga---
  
telco <- foreign::read.spss('C:/Users/Pablo/Google Drive/Maestria/EEA/Examen2-EEA2016/parcial2/telco_parcial2.sav',to.data.frame = TRUE)

rownames(telco) <- telco$caso
telco$caso <- NULL  

telco$ESTADO_C <- factor(telco$ESTADO_C)
telco$NIVEL_ED <- factor(telco$NIVEL_ED)
telco$GENERO <- factor(telco$GENERO)
telco$Sev_LLama <- factor(telco$Sev_LLama)
telco$ALQUILER <- factor(telco$ALQUILER)
telco$Ser_Tjta <- factor(telco$Ser_Tjta)
telco$MENSAJES <- factor(telco$MENSAJES)
telco$INTERNET <- factor(telco$INTERNET)
telco$IDENTIFI <- factor(telco$IDENTIFI)
telco$DESVIO_D <- factor(telco$DESVIO_D)
telco$LLAMADAA <- factor(telco$LLAMADAA)
telco$FACTURAC <- factor(telco$FACTURAC)

my_data <- df_status(telco)


#1.1

set.seed( 12454 )
telco_inTraining <- createDataPartition( telco$ROTACION, p = .70, list = FALSE)
telco_dataset_training <- telco[ telco_inTraining,]
telco_dataset_testing  <- telco[-telco_inTraining,]

dim(telco_dataset_training)

dim(telco_dataset_testing)
summary(telco)

#1.2

telco.m1 <- glm(ROTACION ~., family=binomial, data = telco_dataset_training)
summary(telco.m1)

telco.m2 <- step(telco.m1,direction="backward",trace=TRUE)

summary(telco.m2)

#1.3

probpred <- predict(telco.m2, telco_dataset_testing, type=c("response"))
g <- roc(ROTACION ~ probpred, data = telco_dataset_testing)
plot(g, col="red",legacy.axes=TRUE) 

g
probpred.training <- predict(telco.m2,telco_dataset_training, type=c("response"))
g.training <- roc(ROTACION ~ probpred.training, data = telco_dataset_training)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )

g.training


#1.4

sample_indice <- sample(1:nrow(telco_dataset_testing),round(nrow(telco_dataset_testing)*0.3))
sample_telco  <- telco_dataset_testing[sample_indice,]

library(dplyr)
telco_dataset_testing[,30] <- NULL
modelo_prob <- telco_dataset_testing %>% arrange( desc(probpred))
modelo_prob <- modelo_prob[1:90,]
summary(modelo_prob$ROTACION)

#1.5
entrega <- cbind.data.frame(caso=row.names(telco_dataset_testing),Set_datos="Validación",Clase=telco_dataset_testing$ROTACION,prob=telco_dataset_testing$probpred)
write.csv(entrega,file="archivo_validacion_tempone_1_5.txt",row.names = FALSE)
entrega_train <- cbind.data.frame(caso=row.names(telco_dataset_training),Set_datos="Entrenamiento",Clase=telco_dataset_training$ROTACION,prob=probpred.training)
write.csv(entrega_train,file="archivo_entrenamiento_tempone_1_5.txt",row.names = FALSE)

#2

mod.3 <- glm(ROTACION ~ MESES_CO+EDAD_EN_+ALQUILER+GENERO+Lin_mult+NIVEL_ED, family=binomial, data = telco)
summary(mod.3)

#2.1
exp(-0.044519)

#2.2
exp(1.021130)

#2.3

par(mfrow=c(1,2))
plot(mod.3,which=4)
plot(mod.3,which=5)

par(mfrow=c(1,1))

#2.4

modelo_1 <- glm(ROTACION ~ MESES_CO+ALQUILER, family=binomial, data = telco)
modelo_2 <- glm(ROTACION ~ MESES_CO+EDAD_EN_+ALQUILER+GENERO+Lin_mult, family=binomial, data = telco)

summary(modelo_1)
summary(modelo_2)

anova(modelo_1,modelo_2,test = "Chisq")

#2.5

AIC(modelo_1)
AIC(modelo_2)
barplot(cbind(modelo1=AIC(modelo_1),modelo2=AIC(modelo_2)),ylab="AIC",col="red")


barplot(cbind(modelo1=stats::BIC(modelo_1),modelo2=stats::BIC(modelo_2)),ylab="BIC",col="red")


stats::BIC(modelo_1)
stats::BIC(modelo_2)

#2.6

#homer-Lemes

hl.1 <- hoslem.test(modelo_1$y,fitted(modelo_1),g=10) #g determina la cantidad de grupos en los que se divide el dataset
hl.1 #p-value 0.7215, es significativo por lo que tengo evidencia de que el modelo se ajusta mal

hl.2 <- hoslem.test(modelo_2$y,fitted(modelo_2),g=10) #g determina la cantidad de grupos en los que se divide el dataset
hl.2  #p-value 0.9708, es significativo por lo que tengo evidencia de que el modelo se ajusta mal
