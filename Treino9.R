rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)

# Removendo dependencias lineares -----------------------------------------

testData2 <- matrix(0, nrow=6, ncol=6)
testData2[,1] <- c(1, 1, 1, 1, 1, 1)
testData2[,2] <- c(1, 1, 1, 0, 0, 0)
testData2[,3] <- c(0, 0, 0, 1, 1, 1)
testData2[,4] <- c(1, 0, 0, 1, 0, 0)
testData2[,5] <- c(0, 1, 0, 0, 1, 0)
testData2[,6] <- c(0, 0, 1, 0, 0, 1)

dl <- caret::findLinearCombos(testData2)

testData<- testData2[, -dl$remove]

## Funcao preProcess

Wage <- ISLR::Wage

set.seed(150)
inTrain <- caret::createDataPartition(y = Wage$wage,
                                      p=0.75, list=F) 

#Separamos linhas para amostra treino
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

#Criar pre processamento
tratamento<-caret::preProcess(training,
                              method = c("nzv","corr"),
                              freqCut = 95/5, uniqueCut = 10, 
                              cutoff = .75)



## Aplicando pre processamento
training_pp<-predict(tratamento,training)

## Na amostra TESTE devemos aplicar o mesmo pre processamento
testing_pp<-predict(tratamento,testing)

