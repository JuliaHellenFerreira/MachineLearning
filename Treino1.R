rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)


# Pacotes -----------------------------------------------------------------

library(kernlab)
library(dplyr)

# Base de dados -----------------------------------------------------------

data(spam)


# Analise -----------------------------------------------------------------

par(mfrow = c(1,2)) # Como organjizar os graficos

hist(spam$free[spam$type == "nonspam"],
     prob = T, col = "yellow", ylim = c(0,1),
     xlim = c(0,12), xlab="Frequencia (em %)",
     main = "Não SPAM - Palavra free") # Grafico com os emails que nao sao spam 

lines(density(spam$free[spam$type=="nonspam"]),
      col="blue", lwd=2) #Densidade estimada

hist(spam$free[spam$type == "spam"],
     prob = T,col = "gray",
     ylim = c(0,1),
     xlab = "Frequencia (em %)", main = "SPAM - Palavra free")

lines(density(spam$free[spam$type == "spam"]), col = "red", lwd = 2)

## Conclusao: Podemos obeservar que os emails que nao sao spam tem uma
## distribuicao perto do zero. Ja quando falamos de spam tem uma
## distribuicao ao longo do grafcio.

par(mfrow=c(1,1)) # Colocando o grafico em uma linha e coluna

plot(density(spam$free[spam$type=="nonspam"]),
     col="blue",lwd=2,
     xlab="Frequencia (em %)",
     main="Densidades da palavra free")

lines(density(spam$free[spam$type=="spam"]),
      col="red",lwd=2)

## Juntamos as densidades estimadas

abline(v = 0.5) # Escolhemos um valor que seja equilibrado paa ra abranger 
# spam de nao spam

predition <- ifelse(spam$free > 0.5,"spam","nonspam") # Separamos eles

table(predition,spam$type) # Vamos fazer uma tabela que reuna as informacoes

table(predition,spam$type)/length(spam$type) # Proporcao

## Taxa de acerto: 
paste0(round((0.58248207+0.11845251)*100), "%")
