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
