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
library(car)
library(ellipse)
library(QuantPsyc)
library(leaps)
library(xlsx)
options(scipen = 9999)

-----#carga de datos------

Formula1 <- read.table("C:/Users/Pablo/Google Drive/Maestria/EEA/eea2013/Formula1_2013.csv",header = TRUE,sep = ";",dec = ",",stringsAsFactors = FALSE)
Formula1$RPM <- as.numeric(Formula1$RPM)*1000

-----#punto 1------

#En primer lugar desarrolle un modelo de regresión que relacione la variable de respuesta con la potencia del motor. 

#a.	¿Cuál es la ecuación resultante?

fit.f1.1 <- lm(KMPORH ~ KW,data = Formula1)

summary(fit.f1.1)

#b.	Diseñe un gráfico que incluya la recta y el intervalo de confianza del 95% para la media (con línea punteada o similar). ¿Cuántos puntos quedan fuera de este intervalo?

plot(KMPORH ~ KW,data = Formula1)
abline(fit.f1.1,col='red')

ggplot(data = Formula1,mapping = aes(x=KW,y=KMPORH))+geom_point()+geom_smooth(method = "lm")+stat_ellipse()

#c.	¿Estos intervalos de confianza son siempre simétricos? 
'simetricos entre si'

#a.	¿Cuál es valor estimado de KMPORH para un valor de KW igual a 360? Incluya el intervalo de confianza del 95% para la media.

predict(fit.f1.1,data.frame(KW=360),interval="confidence")

#b.	¿Cuál es valor estimado de KW para un valor de KMPORH igual a 300? Incluya el intervalo de confianza del 95% para la media.
predict(fit.f1.1,data.frame(KW=300),interval="confidence")

#c.	Realice un gráfico de los residuos (en el eje vertical) contra los valores predichos (en el eje horizontal). ¿Encuentra algún comportamiento?
plot(fit.f1.1,which=3)

#d.	¿Encuentra valores anómalos importantes mediante la distancia de Cook? ¿Y utilizando DFBetas?
