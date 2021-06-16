rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)


# Pacotes -----------------------------------------------------------------

library(kernlab)
library(dplyr)

# Base de dados -----------------------------------------------------------

data(spam)


## Fixamos a posicao na tabela aleatoria
set.seed(200)

## Sorteamos 50 observacoes do banco SPAM

sorteio <- sample(dim(spam)[1], size = 50)

small.spam <-spam[sorteio,]

#Vamos plotar os dados 

plot(small.spam$capitalAve[small.spam$type =="spam"],
     col = "red", ylim = c(0,20), xlim = c(1, 20),
     ylab = "capitalAve", pch = 19) # Pegando os pontos que sao spam

points(small.spam$capitalAve[small.spam$type=="nonspam"],
       col="blue", pch=19)

## Colocamos linhas para separar spam de nao spam

abline(h = 2.5, col = "black")

## Regra para minimizar o "in sample error":

regra1 <- small.spam %>% 
  select(capitalAve, type) %>% 
  arrange(capitalAve) %>% 
  mutate(predic = NA)

## Classificando

for (i in 1:nrow(small.spam)){
  if (regra1$capitalAve[i] <= 2.5){
    regra1$predic[i]<-"nonspam"
  }else{
      regra1$predic[i]<-"spam"
    }
}

table(regra1$predic,regra1$type)
table(regra1$predic,regra1$type)/dim(regra1)[1]

#in sample error: 32%
