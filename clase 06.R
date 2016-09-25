-----#librerias-----
library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(combs)
library(CombMSC)

options(scipen = 9999)

----#ejemplo autos----

fit.1.c6 <- lm(consumo ~ motor+cv+acel+cilindr,data = autos)
  
fit.1.c6

summary(fit.1.c6)

# Stepwise foward Regression
forw.1.6 <- stepAIC(fit.1.c6, direction="forward",steps = 3, k=2)
summary(forw.1.6)
forw.1.6$anova # display results 

forw.2.6 <- step(fit.1.c6, direction="forward")
summary(forw.2.6)

# Stepwise backward Regression
back.1.6 <- stepAIC(fit.1.c6, direction="backward")
summary(back.1.6)
back.1.6$anova # display results 

# Stepwise backward Regression
step.1.6 <- stepAIC(fit.1.c6, direction="both")
summary(step.1.6)
step.1.6$anova # display results 

Cp(fit.1.c6)


-----#ejemplo examen----



