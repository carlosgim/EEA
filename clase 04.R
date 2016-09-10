----#library----
library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif


----#modelo hamilton-----
P103_Hamilton_coma <- read.delim2("C:/Users/Pablo/Google Drive/Maestria/EEA/Clase 04/P103_Hamilton_coma.txt")
#con x1
hamilton.fit.1 <- lm(data=P103_Hamilton_coma, Y~X1)

plot(hamilton.fit.1) 

plot(data=P103_Hamilton_coma, Y~X1)
abline(hamilton.fit.1)
summary(hamilton.fit.1)
#rcuadrado cercano a 0, no explica

#con x2
hamilton.fit.2 <- lm(data=P103_Hamilton_coma, Y~X2)

plot(hamilton.fit.2) 

plot(data=P103_Hamilton_coma, Y~X2)
abline(hamilton.fit.2)
summary(hamilton.fit.2)
#rcuadrado 0.1, explica más

#multiple
hamilton.fit <- lm(data=P103_Hamilton_coma, Y~.)

summary(hamilton.fit)

plot(P103_Hamilton_coma)

#ver correlacion

corrplot(cor(P103_Hamilton_coma),addCoef.col = TRUE)

s3d <- scatterplot3d(P103_Hamilton_coma,pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")
s3d$plane3d(hamilton.fit)

plot3d(P103_Hamilton_coma, col="red", size=3) 

---#ejemplo autos----

autos <- read.spss("C:/Users/Pablo/Google Drive/Maestria/EEA/Clase 01/coches.sav",to.data.frame=TRUE)

autos.fit.1 <- lm(data=autos, consumo~cv+peso+cilindr+acel)

plot(autos.fit.1) 

summary(autos.fit.1)

#fit2: potencia si esta correlacionada con las otras

autos.fit.2 <- lm(data=autos, consumo~peso+cilindr+acel+cv)
autos$cilindr <- as.numeric(autos$cilindr)
plot(autos.fit.2) 

summary(autos.fit.2)

# Evaluate Collinearity
vif.autos <- vif(autos[,c(3,4,5,8)]) # variance inflation factors
sqrt(vif.autos$VIF) > 2 # problem?

vifstep(autos[,c(3,4,5,8)],th=8)

vifcor(autos[,c(3,4,5,8)],th=0.9)
