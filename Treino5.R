rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)


# Pacotes -----------------------------------------------------------------

library(kernlab) # contem a base de spam e os modelos.
library(dplyr)
library(caret) # tem a funcao que separa em treino e teste mantendo as prop.
library(epiDisplay) # Gerar tabelas com mais informacoes e gera grafico

# Base de dados -----------------------------------------------------------

data("faithful") # Base rotulada

# Separando a base --------------------------------------------------------

set.seed(444)
inTraining <- caret::createDataPartition(y = faithful$waiting, p = .5, list = F)

# Treino

training <- faithful[inTraining,]

# Teste

testing <- faithful[-inTraining,]


# DESAFIO -----------------------------------------------------------------

# par(mfrow=c(1,3))
# 
# plot1 <- boxplot(faithful$waiting)
# plot2 <-boxplot(testing$waiting)
# plot3 <-boxplot(training$waiting)
# 
# ?createDataPartition # For numeric y, the sample is split into groups
# sections based on percentiles and sampling is done within these subgroups.

# Escolha do modelo -------------------------------------------------------

# O modelo escolido foi "lm"

modelFit <- train(waiting ~ eruptions, data = faithful, method = "lm")
modelFit # RMSE = 5.873226, Rsquared = 0.8163251, MAE = 4.745002  
summary(modelFit) # Resumo do modelo 


# Linha de regresao -------------------------------------------------------

# Com ggplot

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Base faithful")

# Sem ggplot

# plot(training$waiting~training$eruptions,ylab="Waiting",
#      xlab="Eruption")
# 
# abline(modelFit$finalModel, col = "red", lwd = 3)

# Usando a base teste -----------------------------------------------------

# Estimar o erro out of sample

prediction <- stats::predict(modelFit, newdata = testing)

caret::postResample(prediction, testing$waiting) # Tivemos resultados melhores


# Comparativo -------------------------------------------------------------

par(mfrow=c(1,2))

plot1 <- ggplot(training, aes(x = waiting, y = eruptions))+
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Base training")


plot2 <- ggplot(testing, aes(x = waiting, y = eruptions))+
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Base testing")

gridExtra::grid.arrange(plot1 , plot2, ncol=2)


# plot(waiting ~ eruptions, data= training ,ylab="Waiting",
#      xlab="Eruption",main="Amostra Treino")
# abline(modelFit$finalModel,col="red",lwd=3)
# 
# plot(waiting ~ eruptions, data= testing,ylab="Waiting",
#      xlab="Eruption",main="Amostra Teste")
# abline(modelFit$finalModel,col="red",lwd=3)



