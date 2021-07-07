rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)


# Pacotes -----------------------------------------------------------------

library(kernlab) # contem a base de spam e os modelos.
library(dplyr)
library(caret) # tem a funcao que separa em treino e teste mantendo as prop.
library(epiDisplay) # Gerar tabelas com mais informacoes e gera grafico

# Base de dados -----------------------------------------------------------

data(spam) # Base rotulada

# Separando a base --------------------------------------------------------

set.seed(150)

inTrain <- caret::createDataPartition(y = spam$type, p = .8,
                                      list = F )

## Treino

training <- spam[inTrain,]

## Teste

testing <- spam[-inTrain,]

# Bootstrap ---------------------------------------------------------------
## Faremos primeiro usando a funcao train()

set.seed(150) # Temos que definir a semente

ctrl <- caret::trainControl(method = "boot", number = 10) # number eh o 
# o numero de reamostragem

model_bootstrap <- caret::train(type~., data = training,
                                method = "glm", trControl = ctrl)
# Foi usado regressao(glm) mas nao muda nada pois nao tem
# hiperparametros

# O modelo criado:

model_bootstrap

# O valor da accuracy/kappa em cada reamostragem:

model_bootstrap$resample # Com base nisso é feita a media

## Bootstrap sem o boot

set.seed(150)

ctrl1 <- caret::trainControl(method = "none")

model_none <- caret::train(type~., data = training,
                             method = "glm", trControl = ctrl1)

## Comparacao entre os modelos com e sem bootstrap

model_bootstrap$finalModel$coefficients -
  model_none$finalModel$coefficients

## Botstrap para selecao de hiperparametros
set.seed(150)

ctrl2 <- caret::trainControl(method = "boot")

model_bootstrap2 <- train(type~., data = training,
                          method = "knn", trControl = ctrl2)

model_bootstrap2

## Bootstrap sem a funcao train()
set.seed(150)
folds <- caret::createResample(y = training$type, time = 10,
                               list = F)
View(folds)


# k-fold ------------------------------------------------------------------
set.seed(150)
ctrl3 <- caret::trainControl(method = "cv", number = 10)
tng <- expand.grid(k = c(3, 4, 5, 7, 10))

model_kfold <- caret::train(type~.,data = training,
                            method = "knn", trControl = ctrl3,
                            tuneGrid = tng)
model_kfold

# Accuracy em cada reamostragem

model_kfold$resample

## K-fold sem a funcao train()

set.seed(150)

folds_traning <- caret::createFolds(training$type, k = 10,
                                    list = T, returnTrain = T)

summary(folds_traning)

# Treino

training_kfold01 <- spam[folds_traning$Fold01, ]

# Teste

testing_kfold01 <- spam[-folds_traning$Fold01, ]


# k-fold repetido ---------------------------------------------------------

set.seed(150)

ctrl4 <- caret::trainControl(method = "repeatedcv", number = 10,
                             repeats = 3)

model_repeatedcv <- caret::train(type~., data = training, 
                                 method = "rpart",
                                 trControl = ctrl4)

model_repeatedcv
