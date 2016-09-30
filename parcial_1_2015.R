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
ellipse(PVA97$DemAge,PVA97$TARGET_D)
predict(fit.pva.1,data.frame(DemAge=98),se.fit = TRUE)
g1 <- ggplot(PVA97,mapping = aes(x=DemAge,y=TARGET_D))+geom_point()+geom_smooth(method = "lm")+stat_ellipse()
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
lm.beta(forw.pva.1) #betas estandarizados
confint(forw.pva.1, level = 0.9 )
'con 1% son todas significativas, con 5% no es sign gifttimelast'

#1.h.	¿Cómo compara el impacto de las variables independientes en el modelo? ¿Cuál es la variable 'más importante' en el modelo? ¿Y la 'menos importante'?
varImp(forw.pva.1)

#1.i.	¿Existe multicolinealidad en el modelo? Indique al menos 2 opciones para detectarla y explique los resultados obtenidos. 

vif.forward <- vif( PVA97[,c(2:11,14:16)]) # variance inflation factors
vif.forward

vifstep(PVA97[,c(3:11,14:16)],th=9)

vifcor(PVA97[,c(3:11,14:16)],th=0.9)

#evaluar los vif mayores a 9 o 10
forw.pva.1$qr

corrplot((cor(PVA97[,c(3:11,14:16)])),method = "square",addCoef.col = TRUE)

autov.forw <- FactoMineR::PCA(PVA97[,c(3:11,14:16)])

autov.forw$var$cor
max(autov.forw$eig$eigenvalue)/min(autov.forw$eig$eigenvalue)


#1.j.	Utilice un método gráfico para analizar el cumplimiento del supuesto de homocedasticidad. 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(forw.pva.1)
# plot studentized residuals vs. fitted values
spreadLevelPlot(forw.pva.1)
plot(forw.pva.1) #ver 3er grafico

#1.k.	Incluya ahora una regresión stepwise para comparar la calidad del ajuste de los 3 modelos obtenidos (la regresión lineal simple más los 2 modelos de selección de variables

step.pva.1 <-stepAIC(fit.pva.2)

summary(step.pva.1)
AIC(step.pva.1)
summary(fit.pva.1)
AIC(fit.pva.1)
summary(forw.pva.1)
AIC(forw.pva.1)
anova(fit.pva.1,forw.pva.1,step.pva.1)

#1.l.	¿Cuál sería el mejor modelo considerando 3 variables predictoras? ¿Y considerando 4 variables predictoras? Indique qué criterio utiliza para esta elección.
step.pva.1 <-stepAIC(fit.pva.2,scope = list())

# All Subsets Regression

leaps<-regsubsets(TARGET_D ~ .,data = PVA97[,c(2:11,14:16)],nbest=1,nvmax = 3)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
subsets(leaps, statistic="rsq") 

leaps<-regsubsets(TARGET_D ~ .,data = PVA97[,c(2:11,14:16)],nvmax = 4)
# view results
summary(leaps)
# models are ordered by the selection statistic.
plot(leaps,scale="r2")

#1.m.	Desarrolle entonces un nuevo modelo de regresión stepwise que incluya obligatoriamente la variable correspondiente a la edad. Compare el resultado obtenido con la regresión stepwise realizada anteriormente.
min.model.2 = lm(TARGET_D ~ DemAge,data = PVA97[,c(2:11,14:16)])

step.pva.2 <-stepAIC(fit.pva.2, direction="both", scope=list(lower=min.model.2, upper=fit.pva.2))

