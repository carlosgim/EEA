-----#librerias----

library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(CombMSC)
library(ggplot2)
library(caret)

options(scipen = 9999)

-----#carga de datos------

PVA97 <- read.table("/home/pablo/Documentos/EEA/EEA/1º Parcial - 2015/PVA97_EEA2015.csv",header = TRUE,sep = "|",dec = ",")

-----#punto 1------

#1.a. ¿Cuál es la ecuación resultante?

fit.pva.1 <- lm(TARGET_D ~ DemAge,data = PVA97)
summary(fit.pva.1)
confint(fit.pva.1) #valores de confianza
#ecuación <- target_d=18.24977-0.04231*DemAge

#1.b. ¿Cuál sería el valor de la variable dependiente si la edad fuera igual a 98 años?
#Incluya al menos un argumento a favor o en contra del uso del modelo para realizar predicción. 

'A favor es que la variable parece ser buena predictora del monto, en contra que el R2 es muy bajo,
con lo que la regresión no ajusta bien a la realidad.'
plot(PVA97$DemAge,PVA97$TARGET_D)
abline(fit.pva.1,col="red")

predict(fit.pva.1,data.frame(DemAge=98),se.fit = TRUE)

'valor predicho 14.10365'

#1.c. Diseñe un gráfico que incluya la recta y el intervalo de confianza del 95% para la media (con línea punteada o similar). 

m1 <- ggplot(data = PVA97,aes(x=DemAge,y=TARGET_D))+geom_point(shape=1)
m1+stat_smooth(method = lm,fill = "grey50", alpha = 1,level = 0.95,colour="red")

#1.d. ¿Sería más amplio o más angosto el gráfico de los intervalos si pensamos en un 90% de confianza?
m1 <- ggplot(data = PVA97,aes(x=DemAge,y=TARGET_D))+geom_point(shape=1)
m1+stat_smooth(method = lm,fill = "grey50", alpha = 1,level = 0.90,colour="red")

'intervalo mas amplio'

#1.e. ¿Cuál es valor estimado de Target_D para una edad de 69 años? Incluya los intervalos de confianza del 95% para la media y para los individuos.

predict(fit.pva.1,data.frame(DemAge=69),interval="confidence")

'   fit      lwr      upr
1 15.33056 14.88495 15.77618'


----#punto 2----

#A continuación desarrolle un modelo de regresión que relacione la variable de respuesta con las 12 variables predictoras continuas. Para esto utilice la técnica forward de selección de variables. 
min.model = lm(TARGET_D ~ 1,data = PVA97[,c(2:11,14:16)])
fit.pva.2 <- lm(TARGET_D ~ .,data = PVA97[,c(2:11,14:16)])

# Stepwise foward Regression
forw.pva.1 <-stepAIC(min.model, direction="forward", scope=list(lower=min.model, upper=fit.pva.2))

summary(forw.pva.1)
forw.pva.1$anova # display results 
anova(forw.pva.1)
#intervalos de confianza
confint(forw.pva.1)

#variables mas importantes

varImp(forw.pva.1)

#1.f.	¿El modelo es significativo? Si es significativo
#1.g.	¿Qué variables son significativas con un 5% de error? ¿Y con un 1% de error?

anova(forw.pva.1)

'con 1% son todas significativas, con 5% no es sign gifttimelast'

#1.h.	¿Cómo compara el impacto de las variables independientes en el modelo? ¿Cuál es la variable 'más importante' en el modelo? ¿Y la 'menos importante'?
varImp(forw.pva.1)

#1.i.	¿Existe multicolinealidad en el modelo? Indique al menos 2 opciones para detectarla y explique los resultados obtenidos. 

vif.forward <- vif( PVA97[,c(2:11,14:16)]) # variance inflation factors
vif.forward

vifstep(PVA97[,c(2:11,14:16)],th=9)

vifcor(PVA97[,c(2:11,14:16)],th=0.9)

#evaluar los vif mayores a 9 o 10
forw.pva.1$qr

corrplot((cor(PVA97[,c(2:11,14:16)])),co)


