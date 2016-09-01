----#librerias----
library(ggplot2)
library(stringr)
library(gridExtra)
library(funModeling)

options(scipen = 999) #deshabilitar notacion cientifica
----#carga archivo----
salarios <- read.csv("E:/Maestria/EEA/salarios.csv", sep=";",stringsAsFactors = FALSE)

salarios <- read.csv("~/Documentos/EEA/salarios.csv", sep=";", stringsAsFactors=FALSE)



----#Ejercicio 1----
#salarios a numericos
salarios$edad <- as.numeric(salarios$edad)
salarios_v2 <- apply(salarios,2, function (x) gsub(pattern = "$",replacement = "",x,fixed = TRUE))
salarios_v2 <- apply(salarios_v2,2, function (x) gsub(pattern = ".",replacement = "",x,fixed = TRUE))
salarios_v2 <- apply(salarios_v2,2, function (x) as.numeric(x))
salarios_v2 <- as.data.frame(salarios_v2)


summary(salarios_v2)
funModeling::df_status(salarios_v2)

#a. Hacer gráficos de nube de puntos para la variable ‘salario’ versus las otras variables.

g1 <- ggplot(salarios_v2,mapping = aes(x=salini,y=salario))+geom_point()+scale_y_continuous(labels = scales::comma)
g2 <- ggplot(salarios_v2,mapping = aes(x=expprev,y=salario))+geom_point()+scale_y_continuous(labels = scales::comma)

grid.arrange(g1,g2)

#b. Hallar la recta de mínimos cuadrados para cada caso.

fit.1 <- lm(data = salarios_v2,salario~salini)
fit.2 <- lm(data = salarios_v2,salario~expprev)

summary(fit.1)
summary(fit.2)


#c. Graficar el diagrama de dispersión y la recta de ajuste.

l1 <- g1+theme_bw() + 
  geom_smooth(method=lm) # Add linear regression line 

l2 <- g2+theme_bw() + 
  geom_smooth(method=lm) # Add linear regression line 

grid.arrange(l1,l2)

#d. Examinar los residuos y graficarlos.

summary(fit.1)

'Residual standard error: 16.86 on 472 degrees of freedom
Multiple R-squared:  0.3739,	Adjusted R-squared:  0.3726 '

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.1)  # Plot the model information

summary(fit.2)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.2)

salarios_v2$predicted_mod_1 <- predict(fit.1, type="response")   # Save the predicted values
salarios_v2$residuals_mod_1 <- residuals(fit.1, type="response") # Save the residual values

#e. Obtener la tabla de ANOVA para cada caso.

anova(fit.1) ## tendré un análisis de varianza
anova(fit.2)

# GRAFICO con distancia de los residuos
ggplot(salarios_v2, aes(x =salini , y = salario)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = salini, yend = predicted_mod_1), alpha = .2) +  # alpha to fade lines
  geom_point() +
 # geom_point(aes(y = predicted_mod_1), shape = 1) +
  theme_bw()  # Add theme for cleaner look


#f. Calcular e interpretar R2.

summary(fit.1)$r.squared

summary(fit.2)$r.squared

#g. ¿Se puede afirmar que hay una regresión significativa entre las variables estudiadas en cada caso?

'si con el primer modelo'

#h. Estudiar la correlación entre las variables.

cor(salarios_v2[,c(5,6)],use = "pairwise.complete.obs")
                
cor(salarios_v2[,c(5,8)],use = "pairwise.complete.obs")
