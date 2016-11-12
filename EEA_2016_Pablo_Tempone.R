----#librerias-----


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

---#carga de datos-----

autos_parcial <- read.table("C:/Users/Pablo/Google Drive/Maestria/EEA/Examen1-EEA2016/coches_EEA2016.dat",header = TRUE,sep = "!",dec = ",")


----#parte 1------

#En primer lugar desarrolle un modelo de regresión que relacione la variable de respuesta con la variable MOTOR. 

autos_parcial_sn <- autos_parcial[!is.na(autos_parcial$consumo),]

fit.autos.1 <- lm(consumo ~ motor,data = autos_parcial_sn)

summary(fit.autos.1)


#1.b.	Diseñe un gráfico que incluya la recta y el intervalo de confianza del 95% para la media (con línea punteada o similar). ¿Cuántos puntos quedan fuera de este intervalo?

mpi <- cbind(autos_parcial_sn, predict(fit.autos.1, interval = "prediction"))


ggplot(data = autos_parcial_sn,mapping = aes(x=motor,y=consumo))+geom_point()+geom_smooth(method = "lm")+stat_ellipse()+
geom_line(data=mpi,aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(data=mpi,aes(y=upr), color = "red", linetype = "dashed")

#1.d.	¿El intervalo de confianza del 95% hallado es más amplio que el intervalo de confianza para la media del 99%?  
mpi <- cbind(autos_parcial_sn, predict(fit.autos.1, interval = "prediction"))


ggplot(data = autos_parcial_sn,mapping = aes(x=motor,y=consumo))+geom_point()+geom_smooth(method = "lm",level=0.99)+stat_ellipse()
  geom_line(data=mpi,aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(data=mpi,aes(y=upr), color = "red", linetype = "dashed")

#1.f.	¿Cuál es valor estimado de CONSUMO para un valor de MOTOR igual a 0? ¿Confía Ud. en el resultado obtenido?
  
  predict(fit.autos.1,data.frame(motor=0),interval="confidence")
  
#1.g.	Estudie la existencia de outliers y de puntos de influencia. ¿Coinciden? (para esto es altamente recomendable agregar una variable id con una secuencia numérica)
  
plot(fit.autos.1,which=3)

par(mfrow=c(1,2))
plot(fit.autos.1,which=4)
plot(fit.autos.1,which=5)

------#parte 2------

#En segundo lugar, desarrolle un modelo de regresión que relacione la variable de respuesta con las seis variables predictoras continuas. 

autos_parcial_sn$origen <- factor(autos_parcial_sn$origen)

fit.autos.2 <- lm(consumo ~ .,data = autos_parcial_sn[,1:7])

#2.a.	¿Qué variables son significativas con un 5% de error? 

summary(fit.autos.2)


#2.b.	¿Cómo compara el impacto de las variables en el modelo? ¿Cuál es la variable 'más importante' en el modelo? ¿Y la 'menos importante'?

lm.beta(fit.autos.2) #betas estandarizados
confint(fit.autos.2)
varImp(fit.autos.2)

#2.c.	Verifique gráficamente los supuestos de normalidad y de homocedasticidad

plot(fit.autos.2,which=2) #puntos bastante alineados, parece haber normalidad.

residuos <- rstandard(fit.autos.2)
valores.ajustados <- fitted(fit.autos.2)
plot(valores.ajustados, residuos)

#2.d.	¿Existe multicolinealidad en el modelo? Indique 3 opciones para detectarla y explique los resultados obtenidos. 

vif.autos.1 <- vif(fit.autos.2) # variance inflation factors
barplot(vif.autos.1,ylab="VIF")

vifstep(autos_parcial_sn[,c(1:7)],th=10)

vifcor(autos_parcial_sn[,c(1:7)],th=0.9)

#evaluar los vif mayores a 9 o 10
forw.pva.1$qr

corrplot((cor(autos_parcial_sn[,c(1:7)],use="complete.obs")),method = "square",addCoef.col = TRUE)

autov.auto.1 <- FactoMineR::PCA(autos_parcial_sn[,c(1:7)])

autov.auto.1$var$cor
max(autov.auto.1$eig$eigenvalue)/min(autov.auto.1$eig$eigenvalue)

#2.g.	Se necesita utilizar regresión backward pero incluyendo la variable MOTOR obligatoriamente. En esta situación, ¿cuál es el modelo resultante? (considere una probabilidad de salida de 0,25) 

min.model.autos.1 = lm(consumo ~ motor,data = autos_parcial_sn[,c(1:7)])

step.autos.1 <-stepAIC(fit.autos.2, direction="backward", scope=list(lower=min.model.autos.1, upper=fit.autos.2))

summary(step.autos.1)


#2.h.	¿Cuál es el mejor modelo considerando dos variables predictoras? ¿Y considerando tres variables predictoras? Utilice los criterios de R2 y RMSE para responder. 

# All Subsets Regression

leaps.autos.2 <-regsubsets(consumo ~ .,data =autos_parcial_sn[,c(1:7)],nvmax = 2)
# view results
summary(leaps.autos.2)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
par(mfrow=c(1,3))

plot(leaps.f1.2,scale="adjr2")
plot(leaps.f1.2,scale="Cp")
plot(leaps.f1.2,scale="r2")


summary(leaps.autos.2)

leaps.autos.2$rss
#con 3 variables
leaps.autos.3 <-regsubsets(consumo ~ .,data =autos_parcial_sn[,c(1:7)],nvmax = 3)

leaps.autos.3$call
summary(leaps.autos.3)
leaps.autos.3
plot(leaps.autos.3,scale="adjr2")
plot(leaps.autos.3,scale="Cp")
plot(leaps.autos.3,scale="r2")


-----#parte 3-----

#En tercer lugar desarrolle un modelo de regresión que relacione la variable de respuesta con las seis variables predictoras continuas más la variable nominal que indica el origen al que pertenece el automóvil. Para esto considere como categoría base a Japón (código =3).

cont_dumm <- dummy(autos_parcial_sn$origen,drop = TRUE)[,-c(3,4)]
autos_parcial_sn <- cbind(autos_parcial_sn,cont_dumm)

autos_v2 <- autos_parcial_sn[,c(1:7,9,10)]

fit.autos.3 <- lm(consumo ~ .,data = autos_v2)

#3.a.	¿Cambiaron las variables significativas respecto del modelo de regresión múltiple? ¿Y qué puede decir de la variable nominal que se agregó?

summary(fit.autos.3)
summary(fit.autos.2)
summary(fit.autos.1)

modelo1 <- summary(fit.autos.1)[9]
modelo2 <- summary(fit.autos.2)[9]
modelo3 <- summary(fit.autos.3)[9]

barplot(cbind(modelo1,modelo2,modelo3),ylab="R-ajustado")


barplot(cbind(modelo1=AIC(fit.autos.1),modelo2=AIC(fit.autos.2),modelo3=AIC(fit.autos.3)),ylab="AIC")

barplot(cbind(modelo1=stats::BIC(fit.autos.1),modelo2=stats::BIC(fit.autos.2),modelo3=stats::BIC(fit.autos.3)),ylab="BIC")


stats::BIC(fit.autos.1)
stats::BIC(fit.autos.2)
stats::BIC(fit.autos.3)

Cp(fit.autos.3)
