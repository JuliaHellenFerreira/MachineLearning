# Machine Learning - Professor: Douglas Rodrigues
## Bibliotecas

library(kernlab) # Contém a base spam que estamos estudando;

library(dplyr) #c Manipulação dos dados;

library(caret) # Tem um função que permite separar em treino e teste a nossa amostra e mantem a proporção;

library(epiDisplay) # Gerar tabelas com informações básicas e gera graficos.

## Principais funções: 

* caret::createDataPartition(y = spam$type, p = 0.75, list = F) -> Separa as linhas com mesmas proporções de categorias e retornara num vetor;

* epiDisplay::tab1(spam$type, col = "black") -> Para verificar as prpoções. Devem ser iguais ao da base principal; 


