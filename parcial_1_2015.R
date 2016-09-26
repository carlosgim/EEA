-----#librerias----

library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(CombMSC)
library(ggplot2)
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