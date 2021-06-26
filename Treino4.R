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

# Preparando a separacao
set.seed(150)
inTraining <- caret::createDataPartition(y = spam$type, p = 0.75, list = F)

# Treino
training <- spam[inTraining,]

# Testing

testing <- spam[-inTraining,]

# Verificando se foi sepaerado corretamente:

epiDisplay::tab1(spam$type) # OK
epiDisplay::tab1(testing$type) # OK
epiDisplay::tab1(training$type) # OK

# Escolha do modelo  ------------------------------------------------------

# O modelo escolhido doi glm:

modelFit <- caret::train(type ~ ., data = training, method = "glm" )

modelFit # Accuracy = 0.9187845

# Usando a base teste -----------------------------------------------------

# Usando o teste
prediction <- stats::predict(modelFit, newdata = testing)

# Comparando as bases

caret::confusionMatrix(prediction, testing$type) # Accuracy = 0.9304,
# Sensitivity = 0.9613, Specificity = 0.8830

# Se eu quiser mudar a classe basta mudar um argumento

caret::confusionMatrix(prediction,testing$type, positive="spam") 
# Accuracy = 0.9304, Sensitivity = 0.8830, Specificity = 0.9613 


