#ejemplo de http://analisisydecision.es/regresion-ridge-o-contraida-con-r/

# The following dataset is from Hastie, Tibshirani and Friedman (2009), from a study
# by Stamey et al. (1989) of prostate cancer, measuring the correlation between the level
# of a prostate-specific antigen and some covariates. The covariates are
#
# * lcavol  : log-cancer volume
# * lweight : log-prostate weight
# * age     : age of patient
# * lbhp    : log-amount of benign hyperplasia
# * svi     : seminal vesicle invasion
# * lcp     : log-capsular penetration
# * gleason : Gleason Score, check http://en.wikipedia.org/wiki/Gleason_Grading_System
# * pgg45   : percent of Gleason scores 4 or 5
#
# And lpsa is the response variable, log-psa.

----#librerias----
library(curl)
library(RCurl)
library(car)
library(MASS)

---#carga de datos----

url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data"
download.file(url = url,destfile = "prostate.data")
cancer <- read.table("prostate.data", header=TRUE)
str(cancer)

entreno = subset(cancer,train=="TRUE")
modelo_mco <- lm(lpsa~ . , data=entreno[,-10])
summary(modelo_mco)

vif(modelo_mco)
modelo_contraida <- lm.ridge(lpsa ~ ., data=entreno[,-10], lambda = seq(0,10,0.1))
plot(seq(0,10,0.1), modelo_contraida$GCV, main="Busqueda lambda por GCV", type="l",
     xlab=expression(lambda), ylab="GCV")


----#otro ejemplos---
  
  fit = lm(lpsa~.,data=cancer)
summary(fit)

#usando ridge regression

lam = seq(0,10000,len=5000)
ridgefits = lm.ridge(lpsa~.,data=cancer,lam=lam)
plot(range(lam), range(ridgefits$coef),type='n')
for(i in 1:nrow(ridgefits$coef)) {lines(lam,ridgefits$coef[i,])}
    
plot(ridgefits)

#otro ejemplo

library(MASS)
library(lattice)
library(GGally)


head(longley)
pairs(longley)
ggpairs(longley,columns = colnames(longley[,-1]))

names(longley)[1] <- "y"
fit <- lm.ridge(y ~ ., longley, lambda = seq(0.001, .05, .001))
summary(fit)
library(broom)
td <- tidy(fit) #muestra cada valor de lambda y su valor aproximado
head(td) 

g <- glance(fit) #funcion glance permite ver el mejor lambda segun varios metodos.
g

library(ggplot2)
ggplot(td, aes(lambda, estimate, color = term)) + geom_line()


# plot of GCV versus lambda
ggplot(td, aes(lambda, GCV)) + geom_line() +
  geom_vline(xintercept = g$lambdaGCV, col = "red", lty = 2)
