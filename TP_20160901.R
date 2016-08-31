----#librerias----
library(ggplot2)
library(stringr)
library(gridExtra)
library(funModeling)


----#carga archivo----
salarios <- read.csv("E:/Maestria/EEA/salarios.csv", sep=";",stringsAsFactors = FALSE)


----#Ejercicio 1----
#salarios a numericos

salarios_v2 <- apply(salarios,2, function (x) gsub(pattern = "$",replacement = "",x,fixed = TRUE))
salarios_v2 <- apply(salarios_v2,2, function (x) gsub(pattern = ".",replacement = "",x,fixed = TRUE))
salarios_v2 <- as.data.frame(salarios_v2)
salarios_v2$salini <- as.numeric(salarios_v2$salini)
salarios_v2$salario <- as.numeric(salarios_v2$salario)
salarios_v2$id <- as.numeric(salarios_v2$id)
salarios_v2$expprev <- as.numeric(salarios_v2$expprev)

summary(salarios_v2)
funModeling::df_status(salarios_v2)

#a. Hacer gráficos de nube de puntos para la variable ‘salario’ versus las otras variables.

g1 <- ggplot(salarios_v2,mapping = aes(x=salario,y=salini))+geom_point()
g2 <- ggplot(salarios_v2,mapping = aes(x=salario,y=expprev))+geom_point()

grid.arrange(g1,g2)

#b. Hallar la recta de mínimos cuadrados para cada caso.

fit.1 <- lm(data = salarios_v2,salario~salini)
fit.2 <- lm(data = salarios_v2,salario~expprev)

summary(fit.1)
summary(fit.2)


#c. Graficar el diagrama de dispersión y la recta de ajuste.

g1+theme_bw() + 
  geom_smooth(method=lm) # Add linear regression line 

g2+theme_bw() + 
  geom_smooth(method=lm) # Add linear regression line 

#d. Examinar los residuos y graficarlos.

summary(fit.1)

'Residual standard error: 44.03 on 472 degrees of freedom
Multiple R-squared:  0.3739,	Adjusted R-squared:  0.3726 '

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.1)  # Plot the model information

salarios_v2$predicted_mod_1 <- predict(fit.1, type="response")   # Save the predicted values
salarios_v2$residuals_mod_1 <- residuals(fit.1, type="response") # Save the residual values


ggplot(salarios_v2, aes(x = salario, y = salini)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = salario, yend = predicted_mod_1), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted_mod_1), shape = 1) +
  theme_bw()  # Add theme for cleaner look
