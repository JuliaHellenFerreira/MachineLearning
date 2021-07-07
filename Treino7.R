rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)


# Pacotes -----------------------------------------------------------------

library(kernlab) # contem a base de spam e os modelos.
library(dplyr)
library(caret) # tem a funcao que separa em treino e teste mantendo as prop.
library(epiDisplay) # Gerar tabelas com mais informacoes e gera grafico

# Base de dados -----------------------------------------------------------

data(spam) # Base rotulada

# Comparacao entre os modelos ---------------------------------------------


ctrl <- caret::trainControl(method = "repeatedcv", number = 10,
                            repeats = 3)



# glm
{set.seed(150)
  t0 <- Sys.time()
  model_glm <- caret::train(type~., data = spam, method = "glm",
                            trControl = ctrl)
  glm_time <- Sys.time() - t0}

# svmLinear

{set.seed(150)
  t0 <- Sys.time()
  model_svmLinear <- caret::train(type~., data = spam, method = "svmLinear",
                            trControl = ctrl)
  svmLinear_time <- Sys.time() - t0}

# rpart

{set.seed(150)
  t0 <- Sys.time()
  model_rpart <- caret::train(type~., data = spam, method = "rpart",
                            trControl = ctrl)
  rpart_time <- Sys.time() - t0}

# knn

{set.seed(150)
  t0 <- Sys.time()
  model_knn <- caret::train(type~., data = spam, method = "knn",
                              trControl = ctrl)
  knn_time <- Sys.time() - t0}

# Comparando os resultados -----------------------------------------------

results <- resamples(list(Glm = model_glm,
                          SVM = model_svmLinear,
                          Rpart = model_rpart,
                          Knn = model_knn))
# Resumindo 

summary(results)

# Tempo de processamento

results$timings

# Visualizacao grafica:

barplot(results$timings$Everything,
        names.arg=rownames(results$timings))

# Outra forma

glm_time
svmLinear_time
rpart_time
knn_time

# Parametros graficos

scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))

# Boxplot comparativo da accuracy e kappa

bwplot(results, scales = scales)

# Densidade da accuracy

densityplot(results, scales = scales, pch = "|", auto.key = TRUE)

# Comportamento de cada fold

parallelplot(results)

# Comparando comportamento de cada fold em diferentes testes
xyplot(results, models=c("Glm", "SVM"))
xyplot(results, models=c("Knn", "Rpart"))

# Calcula diferença de accuracy/kappa entre modelos

diffs <- diff(results)

#Resumo e p-valor

summary(diffs)



