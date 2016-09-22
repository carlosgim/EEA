-----#librerias-----

library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif

---#import file-----

telefonia <- read.csv("~/Documentos/EEA/repasoParcial1/telefonia.csv", dec=",", stringsAsFactors=FALSE)

---#regresion---
  
fit.tel.1 <- lm(root4_ingreso ~ largmes+locmes+tarjmes+innacmes,data = telefonia) 
  
summary(fit.tel.1)

#las variables locmes y tarjmes no son buenas predictoras. Tiene un R cuadrado bajo

plot(fit.tel.1)

# Evaluate Collinearity
vif.telefonia <- vif(telefonia[,c(9:12)]) # variance inflation factors
vif.telefonia
sqrt(vif.telefonia$VIF) > 2 # problem?
