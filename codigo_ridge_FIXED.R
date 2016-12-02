
library(MASS)
library(glmnet)
library(ggplot2)
library(tidyr)

# Regresion para XRSS

longley.scale <- as.data.frame(sapply(longley, scale), col.names = names(longley))

longley.scale.m <- model.matrix(Employed ~ ., longley.scale)

set.seed(224217)
cv.out <- cv.glmnet(x = longley.scale.m, y = longley.scale$Employed, standardize = F, alpha = 0, nfolds = 4, lambda = seq(0,0.010,0.001))
tabla <- data.frame(cv.out$cvm*4, cv.out$lambda)
qplot(data = tabla, x = cv.out$lambda, y = cv.out$cvm * 4, geom = 'line', xlab = 'lambda', ylab = 'RSS', main = 'Cross-Validation RSS') + geom_vline(xintercept = cv.out$lambda.min, col = 'blue')


# Regresion para GCV


fit <- lm.ridge(Employed ~ . , data = longley.scale, lambda = seq(0,0.010,0.001))
lambda.min <- fit$lambda[(which.min(fit$GCV))]
print(coef(fit), digits = 3)

qplot(x = fit$lambda, y = fit$GCV, geom= 'line', ylab = 'GCV', xlab = 'lambda', main = 'GCV vs lambda') + geom_vline( xintercept = lambda.min, col = 'blue')



# Traza de Ridge

tabla <- as.data.frame(coef(fit))
tabla$lambda <- row.names(tabla)
row.names(tabla) <- NULL
names(tabla)[1] <- 'Intercept'

nuevo <- gather(data = tabla, key = terms, value = Estimate, Intercept:Year)
nuevo$lambda <- as.numeric(nuevo$lambda)
nuevo$terms <- as.factor(nuevo$terms)

qplot(data = subset(nuevo, subset = terms != 'Intercept'), x = lambda, y = Estimate, geom = 'line', col = terms)



