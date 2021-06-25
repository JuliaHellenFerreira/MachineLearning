rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)


# Pacotes -----------------------------------------------------------------

library(kernlab) # contem a base de spam
library(dplyr)
library(caret) # tem a funcao que separa em treino e teste mantendo as prop.
library(epiDisplay) # Gerar tabelas com mais informacoes e gera grafico

# Base de dados -----------------------------------------------------------

data(spam)

# Separando as bases ------------------------------------------------------

set.seed(150) # Semente

inTraning <- caret::createDataPartition(y = spam$type, p = 0.75, list = F) 
# Estou separando as linhas com mesmas proporcao de categorias e retornara 
# num vetor

Training <- spam[inTraning,] # Base Treino

Testing <- spam[-inTraning,] # Base Teste

## Vamos verificar as prop.

epiDisplay::tab1(spam$type, col = "black")
epiDisplay::tab1(Training$type, col = "red") 
epiDisplay::tab1(Testing$type, col = "green")
