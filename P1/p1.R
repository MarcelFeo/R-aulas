# Prova 01
# Alunos: Marcel Fernando Lobo de Féo - 12211BCC042
# Sergio Henrique - 12211BCC038
# Prova 01
# Alunos: Marcel Fernando Lobo de Féo - 12211BCC042
# Sergio Henrique - 12211BCC038

library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)

#____________________________________________________________________

# Exercício 01:

# Colocando as cartas no baralho

gerar_baralho <- function() {
 
  baralho <- c()
  
  for(i in 1:52) {
    
    if (i < 14) {
      baralho[i] <- "azul"
    } else if (i >= 14 & i <= 26) {
      baralho[i] <- "vermelha"
    } else if (i >= 26 & i <= 39) {
      baralho[i] <- "branca"
    } else if (i >= 39 & i <= 52) {
      baralho[i] <- "marrom"
    }
    
  }
  
  return(baralho)
}

# letra (a)

experimentoa <- function(n) {
  
  resultado <- c()
  
  for (i in 1:n) {
    
    baralho <- gerar_baralho()
    
    cartas <- sample(baralho, 3)
    qtd_azuis <- sum(cartas == "azul")
    
    if (qtd_azuis == 2) {
      resultado[i] <- 1
    } else {
      resultado[i] <- 0
    }
    
  } 
  
  return(mean(resultado))
}

# Interpretação do resultado: Temos que esse resultado significa a média de vezes que uma pessoa pode tirar duas vezes uma carta azul, pegando aleatoriamente 3 cartas do baralho.


# letra (b)
resultadosb <- c()

for (i in 1:100000) {
  qtd_sorteio <- 0
  total <- 0
  baralhob <- c(1:52)
  
  while(qtd_sorteio < 4) {
    
    sorteio <- sample(baralhob, 1, replace = TRUE)
    
    # Número 7 <- carta azul, Número 20 <- carta vermelha, Núemero 33 <- carta branca, Número 46 <- marrom
    if (sorteio == 7 | sorteio == 20 | sorteio == 33 | sorteio == 46) {
      qtd_sorteio <- qtd_sorteio + 1
    }
    
    total <- total + 1
  }  
  
  resultadosb[i] <- total
}

mean(resultadosb)

#____________________________________________________________________

# Exercício 02:

# letra (a)

dados <- read.table("churn.txt", sep = ";", header = TRUE)
dados <- dados[, -1]
dados <- dados[, -1]
dados <- dados[, -1]

#dados$Surname <- as.factor(dados$Surname)
#dados$Geography <- as.factor(dados$Geography)
#dados$Gender <- as.factor(dados$Gender)

summary(dados)

# letra (b)

dados <- dados[sample(nrow(dados)), ] # mistura os valores no vetor

n <- round(0.75*nrow(dados)) # pega 75% do vetor

treino <- dados[1:n,] # treinamento (80%)
teste <- dados[-(1:n),] # teste (20%)

modelo.arvore <- rpart(formula = Exited~ ., data = treino, method = "class")
rpart.plot(modelo.arvore, extra = 101)

previsao = predict(modelo.arvore, newdata = teste, type = "class")

mean(previsao == teste$Exited)



dados$Surname <- as.factor(dados$Surname)
dados$Geography <- as.factor(dados$Geography)
dados$Gender <- as.factor(dados$Gender)

summary(dados)

# letra (b)

dados <- dados[sample(nrow(dados)), ] # mistura os valores no vetor

n <- round(0.75*nrow(dados)) # pega 75% do vetor

treino <- dados[1:n,] # treinamento (75%)
teste <- dados[-(1:n),] # teste (20%)

modelo.arvore <- rpart(formula = Exited~ ., data = treino, method = "class")
rpart.plot(modelo.arvore, type = 2, extra = 104, fallen.leaves = TRUE)

previsao = predict(modelo.arvore, newdata = teste, type = "class")

mean(previsao == teste$Exited)

matriz <- confusionMatrix(previsao, teste$Exited)

matriz_confusao <- table(Previsão = previsao, Real = teste$Exited)
matriz_confusao 

# letra (c)

todos_dados <- read.table("churn.txt", sep = ";", header = TRUE)

cliente_franca <- subset(todos_dados, Geography == "France")
cliente_spain <- subset(todos_dados, Geography == "Spain")
cliente_germany <- subset(todos_dados, Geography == "Germany")

treino_do_modelo <- function(data, pais) {
  
  todos_dados <- todos_dados[sample(nrow(todos_dados)), ] # mistura os valores no vetor
  todos_dados <- todos_dados[, -1]
  todos_dados <- todos_dados[, -1]
  todos_dados <- todos_dados[, -1]
  
  n <- round(0.75*nrow(todos_dados)) # pega 75% do vetor
  
  treino <- todos_dados[1:n,] # treinamento (75%)
  teste <- todos_dados[-(1:n),] # teste (20%)
  
  modelo_arvore <- rpart(Exited ~ ., data = treino, method = "class")
  rpart.plot(modelo.arvore, type = 2, extra = 104, fallen.leaves = TRUE)
  
  previsao <- predict(modelo_arvore, teste, type = "class")
  
  matriz <- confusionMatrix(previsao, teste$Exited)

  accuracy <- matriz$overall["Accuracy"]
  print(paste("Acurácia de ", pais, " = ", round(accuracy, 4)))

  
  return(accuracy)
}

# Acurácias
franca <- treino_do_modelo(cliente_franca, "França")

espanha <- treino_do_modelo(cliente_espanha, "Espanha")

alemanha <- treino_do_modelo(cliente_alemanha, "Alemanha")


# Comentando os resultados da b e c: 

#Acurácia: Você pode comparar as acurácias entre os modelos de diferentes países para ver se há variações significativas. Isso pode indicar que o comportamento dos clientes varia entre países.
#Matriz de Confusão: A matriz de confusão para cada país permite entender onde o modelo está errando mais, por exemplo, se há muitos falsos positivos ou falsos negativos em um país específico.

