# Importo librerías (ISLR incluye el dataset Hitters)

if(!require("ISLR")) install.packages("ISLR")

if(!require("caret")) install.packages("caret")

# Breve inspección del dataset Hitters

head(Hitters)

str(Hitters)

# Defino la estructura del experimento

fitControl <- trainControl(method = "repeatedcv",
                           
                           number = 5, repeats = 3)

# Valores que pruebo de labda

gridLambda <- expand.grid(lambda = c(0.0001,0.001,0.01,0.1,1,10,
                                     
                                     100,1000))

# Corro el experimento

lassoFit <- train(Salary ~ ., data = Hitters, method = "ridge",
                  
                  trControl = fitControl,
                  
                  tuneGrid = gridLambda,
                  
                  preProcess = c("center", "scale"))

# Imprimo el experimento

print(lassoFit)

plot(lassoFit)


# Importo librerías (ISLR incluye el dataset Hitters)

if(!require("ISLR")) install.packages("ISLR")

if(!require("caret")) install.packages("caret")

# Breve inspección del dataset Hitters

head(Hitters)

str(Hitters)

# Defino la estructura del experimento

fitControl <- trainControl(method = "repeatedcv",
                           
                           number = 5, repeats = 3)

# Valores que pruebo de labda

gridLambda <- expand.grid(lambda = c(0.0001,0.001,0.01,0.1,1,10,
                                     
                                     100,1000))

# Corro el experimento

lassoFit <- train(Salary ~ .^2, data = Hitters, method = "ridge",
                  
                  trControl = fitControl,
                  
                  tuneGrid = gridLambda,
                  
                  preProcess = c("center", "scale"))

# Imprimo el experimento

print(lassoFit)

plot(lassoFit)
