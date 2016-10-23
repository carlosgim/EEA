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
library(dummies)
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

#otro ejemplo con ggplot

# cbind the predictions to mtcars
mpi <- cbind(Formula1, predict(fit.f1.1, interval = "prediction"))
ggplot(data = Formula1,mapping = aes(x=KW,y=KMPORH))+geom_point()+geom_smooth(method = "lm")+stat_ellipse()+geom_ribbon(data=mpi,aes(ymin = lwr, ymax = upr),
                                                                                                                        fill = "blue", alpha = 0.2)
ggplot(data = Formula1,mapping = aes(x=KW,y=KMPORH))+geom_point()+geom_smooth(method = "lm")+stat_ellipse()+
  geom_line(data=mpi,aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(data=mpi,aes(y=upr), color = "red", linetype = "dashed")
                                                                                                                        
# Grafico de dispersion y recta
plot(KMPORH ~ KW,data = Formula1)
abline(fit.f1.1)

# Intervalos de confianza de la respuesta media: ic es una matriz con tres
# columnas: la primera es la prediccion, las otras dos son los extremos
# del intervalo
ic <- predict(fit.f1.1, Formula1, interval = "confidence")
lines(Formula1$KW, ic[, 2], lty = 2)
lines(Formula1$KW, ic[, 3], lty = 2)

# Intervalos de prediccion
ic <- predict(fit.f1.1, Formula1, interval = "prediction")
lines(Formula1$KW, ic[, 2], lty = 2, col = "red")
lines(Formula1$KW, ic[, 3], lty = 2, col = "red")


#c.	¿Estos intervalos de confianza son siempre simétricos? 
'simetricos entre si'

#a.	¿Cuál es valor estimado de KMPORH para un valor de KW igual a 360? Incluya el intervalo de confianza del 95% para la media.

predict(fit.f1.1,data.frame(KW=360),interval="confidence")

#b.	¿Cuál es valor estimado de KW para un valor de KMPORH igual a 300? Incluya el intervalo de confianza del 95% para la media.
predict(fit.f1.1,data.frame(KW=300),interval="confidence")

#c.	Realice un gráfico de los residuos (en el eje vertical) contra los valores predichos (en el eje horizontal). ¿Encuentra algún comportamiento?
plot(fit.f1.1,which=3)

#d.	¿Encuentra valores anómalos importantes mediante la distancia de Cook? ¿Y utilizando DFBetas?
par(mfrow=c(1,2))
plot(fit.f1.1,which=4)
plot(fit.f1.1,which=5)

#e.	¿Cuál es la relación entre el valor del test t para la variable predictora y el valor F del ANOVA?
anova(fit.f1.1)
'son tests que expresan cosas similares cuando hay una sola variable predictora'

-----#parte 2-----

#En segundo lugar desarrolle un modelo de regresión que relacione la variable de respuesta con las cuatro variables predictoras continuas

fit.f1.2 <- lm(data = Formula1[,1:5],KMPORH ~ .)

#f.	Compare el ajuste de este modelo con el primer modelo obtenido. 

summary(fit.f1.2)
summary(fit.f1.1)

'tiene un mejor R2 ajustado'

#g.	¿Qué variables son significativas con un 5% de error? 

summary(fit.f1.2)

#h.	¿Cómo compara el impacto de las variables en el modelo? ¿Cuál es la variable 'más importante' en el modelo? ¿Y la 'menos importante'?
lm.beta(fit.f1.2) #betas estandarizados
confint(fit.f1.2)
varImp(fit.f1.2)

#i.	¿Existe multicolinealidad en el modelo? Indique dos opciones para detectarla y explique los resultados obtenidos. 

vif.forward.f1 <- vif(fit.f1.2) # variance inflation factors
vif.forward.f1

vifstep(Formula1[,2:5],th=9)

vifcor(Formula1[,2:5],th=0.9)

#evaluar los vif mayores a 9 o 10
forw.pva.1$qr

corrplot((cor(Formula1[,2:5])),method = "square",addCoef.col = TRUE)

autov.forw <- FactoMineR::PCA(Formula1[,1:5])

autov.forw$var$cor
max(autov.forw$eig$eigenvalue)/min(autov.forw$eig$eigenvalue)


'Si existe multicolinealidad'

#j.	¿Qué opciones conoce para resolver la multicolinealidad? Indique por lo menos dos de ellas.

'eliminar variable con colinealidad y componentes principales'

#k.	¿Cuál es la relación entre VIF y Tolerancia?
'índice de tolerancia = 1/VIFi'

#l.	¿Cuál es el mejor modelo considerando dos variables predictoras? ¿Y considerando tres variables predictoras? Considere el criterio Bayesiano y el de Akaike (o el de Akaike corregido) para reponder.

# All Subsets Regression

leaps.f1.2 <-regsubsets(KMPORH ~ .,data = Formula1[,1:5],nvmax = 2)
# view results
summary(leaps.f1.2)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps.f1.2,scale="adjr2")
plot(leaps.f1.2,scale="Cp")

#con 3 variables
leaps.f1.3 <-regsubsets(KMPORH ~ .,data = Formula1[,1:5],nvmax = 3)

leaps.f1.3$call
summary(leaps.f1.3)
leaps.f1.3
plot(leaps.f1.3,scale="adjr2")
plot(leaps.f1.3,scale="Cp")

coef(leaps.f1.3,1:3)

vcov(leaps.f1.3,3)

cor(vcov(leaps.f1.3,3))

#akaike
lm.all.1 <- lm(KMPORH ~ KW+KG, data = Formula1[, 1:5])
lm.all.2 <- lm(KMPORH ~ KW+KG+HP, data = Formula1[, 1:5])

AIC(lm.all.1)
AIC(lm.all.2)

stats::BIC(lm.all.1)
stats::BIC(lm.all.2)


----#parte 3-----

#En tercer lugar desarrolle un modelo de regresión que relacione la variable de respuesta con las cuatro variables predictoras continuas más la variable que indica el continente al que pertenece el automóvil. Para esto considere como categoría base a América (código=0).
Formula1$CONTINENTE <- factor(Formula1$CONTINENTE)
cont_dumm <- dummy(Formula1$CONTINENTE,drop = TRUE)[,-1]
Formula1 <- cbind(Formula1,cont_dumm)

Formula1_v2 <- Formula1[,c(1:5,7,8)]

fit.f1.3 <- lm(KMPORH ~ .,data = Formula1_v2)

#m.	Compare el ajuste de este modelo con los modelos anteriores. 

summary(fit.f1.3)

#n.	¿Qué variables son significativas? 
'HP           -1.1223231   0.4921595  -2.280   0.03138 *  
KW            1.7136336   0.5703257   3.005   0.00597 ** 
  
KG           -0.6961001   0.1076440  -6.467 0.0000009 ***
  
CONTINENTE2  -3.2594395   1.1566591  -2.818   0.00931 ** '


