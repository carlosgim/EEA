-----#librerias-----

library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(FactoMineR)

---#import file-----

telefonia <- read.csv("~/Documentos/EEA/repasoParcial1/telefonia.csv", dec=",", stringsAsFactors=FALSE)

---#regresion---
  
fit.tel.1 <- lm(ingreso ~ largmes+locmes+tarjmes+innacmes,data = telefonia) 
  
summary(fit.tel.1)

fit.tel.1$qr
#las variables locmes y tarjmes no son buenas predictoras. Tiene un R cuadrado bajo

plot(fit.tel.1)

# Evaluate Collinearity
vif.telefonia <- vif(telefonia[,c(9:12)]) # variance inflation factors
vif.telefonia
sqrt(vif.telefonia$VIF) > 2 # problem?


---#ejercicio 3----
fit.tel.2 <- lm(ingreso ~ .,data = telefonia[,c(2,4:14)]) 
summary(fit.tel.2)

fit.tel.3 <- lm(root4_ingreso ~ .,data = telefonia[,c(3:5,7:14)]) 

summary(fit.tel.3)

fit.tel.1$call

# Evaluate Collinearity
vif.telefonia.3 <- vif(telefonia[,c(3:5,7:14)]) # variance inflation factors
vif.telefonia.3
#evaluar los vif mayores a 9 o 10
fit.tel.3$qr

View(data.frame((cor(telefonia[,c(4:5,7:14)]))))

corrplot((cor(telefonia[,c(4:5,7:14)])))

eigen(as.matrix(telefonia[,c(3:5,7:14)]))

autov <- FactoMineR::PCA(telefonia[,c(3:5,7:14)])

autov$var$cor
max(autov$eig$eigenvalue)/min(autov$eig$eigenvalue)
#si es mayor a 100 hay multicolinealidad moderada, si es mayor a 1000 multicolinealidad extrema

#se metio como raiz cuadrada para que sea lineal, porque la distribucion es mas simetrica, el log hubiese funcionado igual
plot(density(telefonia$root4_ingreso))
plot(density(telefonia$ingreso))
#ver para los supuestos los residuos

#incluir los residuos
plot(fit.tel.2)
plot(fit.tel.3)

#los residuos tienen q no estar agrupados

