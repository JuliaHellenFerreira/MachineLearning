rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)


# Pacotes -----------------------------------------------------------------

library(kernlab) 
library(dplyr)
library(caret)
library(epiDisplay)
library(ISLR)
library(kernlab)
library(ModelMetrics)

# Base de dados -----------------------------------------------------------

data(Wage)

data(spam)

# nearZeroVar -------------------------------------------------------------
## Queremos usar o metodo de "VAriancia zero ou quase zero":

caret::nearZeroVar(Wage, saveMetrics = T)
epiDisplay::tab1(Wage$region)

## Quremos remover as variaveis que zeroVar

nzv <- caret::nearZeroVar(Wage, saveMetrics = F, names = T)
Wage_nzv <- dplyr::select(Wage, -nzv)

## Troca dos pontos de corte
nzv2 <- caret::nearZeroVar(Wage,saveMetrics = T,
                    freqCut = 5)

epiDisplay::tab1(Wage$race)

# Vamos conferir: freqRatio  8.464164 > 5

2480/293

# percUnique  0.1333333 < 10

4*100/3000


# Aplicacao ---------------------------------------------------------------

set.seed(150)

inTrain <- caret::createDataPartition(y = Wage$wage,
                                      p = .8,
                                      list = F)
## Treino

training <- Wage[inTrain,]

## Teste

testing <- Wage[-inTrain,]

## Testando o modelo

ctrl <- caret::trainControl(method = "boot", number = 10,
                            preProcOptions = list(freqCut = 95/5,
                                                  uniqueCut = 10))



modelFit <- caret::train(wage~., data = training, method ="lm",
                         trControl = ctrl, preProces = "nzv")

modelFit$preProcess$method$remove

## Aplicando na amostra teste

pred_boot <- stats::predict(modelFit, testing)

caret::postResample(pred_boot, testing$wage)

# Correlacao --------------------------------------------------------------

## Para calcular a correlcao entre as variaveis quantitativas usamos

descrCor <- stats::cor(spam[1:57])

## Resumo da correlacao (por ser simetrica usaremos a parte superior 
## da matriz)

summary(descrCor[upper.tri(descrCor)])

## Para saber quais variaveis tem maior correlacao

highCor <- caret::findCorrelation(descrCor, cutoff = 0.75,
                       names = T)

spam2 <- dplyr::select(spam, -highCor)

# Unindo ------------------------------------------------------------------

set.seed(150)

inTrain1 <- caret::createDataPartition(y = spam$type,
                                      p = .8,
                                      list = F)
## Treino

training1 <- spam[inTrain,]

## Teste

testing1 <- spam[-inTrain,]

## Testando o modelo

ctrl1 <- caret::trainControl(method = "boot", number = 10,
                            preProcOptions = list(freqCut = 95/5,
                                                  uniqueCut = 10))



modelFit1 <- caret::train(type~., data = training1, method ="glm",
                         trControl = ctrl1, preProces = c("nzv", "corr"))

modelFit1$preProcess$method$remove

## Vamos aplicar na amostra

pred_boot1 <- stats::predict(modelFit1, testing1)

ModelMetrics::confusionMatrix(pred_boot1,testing1$type)
