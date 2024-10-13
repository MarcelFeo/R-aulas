# Revisão

library(ggplot2)
library(rpart)
library(class)

# Exercício 01:

vitorias_juju <- c()
vitorias_jorel <- c()

for (i in 1:10000) {
  
  dinheiro_juju <- 18
  dinheiro_jorel <- 7
  
  while(dinheiro_jorel != 0 & dinheiro_juju != 0) {
    # 1 -> cara e 2 -> coroa
    lancamento_moeda <- sample(1:2, 1)
    
    
    if(lancamento_moeda == 1) {
      dinheiro_jorel <- dinheiro_jorel - 1
      dinheiro_juju <- dinheiro_juju + 1
      
      if (dinheiro_jorel <= 0) {
        vitorias_juju[i] <- 1
        vitorias_jorel[i] <- 0
      }
      
    } else {
      dinheiro_juju <- dinheiro_juju - 1
      dinheiro_jorel <- dinheiro_jorel + 1
      
      if (dinheiro_juju <= 0) {
        vitorias_jorel[i] <- 1
        vitorias_juju[i] <- 0
      }
    } 
  }
  
}

mean(vitorias_jorel)
mean(vitorias_juju)

# Exercício 02:

# experimento com todas figurinhas de chances iguais sem figurinhas especiais

figurinhas <- 1:30
resultados <- c()

for (i in 1:10000) {
  
  album <- sample(figurinhas, 1)
  
  while(length(unique(album)) < 30) {
    album <- c(album, sample(figurinhas, 1))
  }
  
  resultados[i] <- length(album) # quantas figurinhas foram compradas
  
}

# figurinha com chances diferentes de serem sorteadas

figurinhas <- 1:30
resultados <- c()

for (i in 1:10000) {
  
  album <- sample(figurinhas, size = 1, prob = c(10, rep(2, times = 29)))
  
  while(length(unique(album)) < 30) {
    album <- c(album, sample(figurinhas, 1, prob = c(10, rep(2, times = 29))))
  }
  
  resultados[i] <- length(album) # quantas figurinhas foram compradas
  
}

# Exercício 03:

chicago <- read.csv("chicago.csv", TRUE)
chicago <- chicago[, -1]
chicago$season <- as.factor(chicago$season)

View(chicago)

# letra a

# Quantas mortes provocadas por doenças cardiovasculares ocorreram no período do estudo?
total_cvd <- sum(chicago$cvd, na.rm = TRUE) # na.rm ignora os NA
total_cvd

# Em qual estação ocorreram mais e menos mortes por doenças cardiovasculares?
cvd_by_season <- aggregate(cvd ~ season, data = chicago, FUN = sum, na.rm = TRUE)
cvd_by_season

# Estação com mais mortes por doenças cardiovasculares
max_cvd_season <- cvd_by_season[which.max(cvd_by_season$cvd), ]
max_cvd_season

# Estação com menos mortes por doenças cardiovasculares
min_cvd_season <- cvd_by_season[which.min(cvd_by_season$cvd), ]
min_cvd_season

# (b) Em qual ano ocorreram mais mortes relacionadas a doenças respiratórias?

resp_by_year <- aggregate(resp ~ year, data = chicago_data, FUN = sum, na.rm = TRUE)
resp_by_year

# Ano com mais mortes relacionadas a doenças respiratórias
max_resp_year <- resp_by_year[which.max(resp_by_year$resp), ]
max_resp_year

# (c) Média de temperatura em cada uma das estações

temp_by_season <- aggregate(temp ~ season, data = chicago, FUN = mean, na.rm = TRUE)
temp_by_season

# Média da umidade relativa em cada uma das estações
rhum_by_season <- aggregate(rhum ~ season, data = chicago, FUN = mean, na.rm = TRUE)
rhum_by_season

# Boxplot para a variável temp em cada estação
boxplot(temp ~ season, data = chicago, main = "Boxplot da Temperatura por Estação", ylab = "Temperatura (°F)")


# letra d
chicago$season <- as.factor(chicago$season)

ggplot(chicago, aes(x = time, y = temp, color = season)) +
  geom_point() +
  labs(title = "Temperatura ao longo do tempo colorido por estação", 
       x = "Tempo", y = "Temperatura (°F)") +
  scale_color_manual(values = c("Spring" = "green", "Summer" = "red", "Autumn" = "orange", "Winter" = "blue"))


inverno <- chicago[chicago$season == "Winter"]

# Exercício 04:

# letra a

papagaio <- read.table("papagaio.txt", sep = ",", header = TRUE)

head(papagaio) # exibe as primeiras 6 linhas do conjunto de dados
tail(papagaio) # exibe as últimas 6 linhas do conjunto
str(papagaio)  # exibe a estrutura do conjunto de dados, mostrando o tipo de cada coluna (variável), o número de observações, e um exemplo de valores.
summary(papagaio) # exibe um resumo estatístico

# letra b

# Gráfico de barras para a quantidade de aves por espécie
ggplot(papagaio, aes(x = especie)) +
  geom_bar(fill = "yellow") +
  labs(title = "Quantidade de Papagaios-do-Mar por Espécie",
       x = "Espécie", y = "Quantidade")

# letra c

# Boxplot de envergadura por espécie
ggplot(papagaio_data, aes(x = especie, y = envergadura)) +
  geom_boxplot() +
  labs(title = "Envergadura por Espécie",
       x = "Espécie", y = "Envergadura (cm)")

# Gráfico de dispersão: peso vs envergadura, colorido por espécie
ggplot(papagaio_data, aes(x = envergadura, y = peso, color = especie)) +
  geom_point() +
  labs(title = "Peso vs Envergadura por Espécie",
       x = "Envergadura (cm)", y = "Peso (g)")

# letra d

# Criar a árvore de decisão
arvore_modelo <- rpart(especie ~ peso + tamanho + envergadura, data = papagaio)

# Exibir a árvore
print(arvore_modelo)

# Previsões usando o modelo
previsoes_arvore <- predict(arvore_modelo, papagaio, type = "class")

# Calcular a taxa de acerto
taxa_acerto_arvore <- mean(previsoes_arvore == papagaio$especie)
taxa_acerto_arvore

# letra e

# Preparar os dados para o KNN (removendo a coluna 'especie' para usar como preditora)
dados_treino <- papagaio[, c("peso", "tamanho", "envergadura")]
especies_treino <- papagaio$especie

# Aplicar KNN (k = 3, por exemplo)
previsoes_knn <- knn(train = dados_treino, test = dados_treino, cl = especies_treino, k = 3)

# Calcular a taxa de acerto do KNN
taxa_acerto_knn <- mean(previsoes_knn == papagaio$especie)
taxa_acerto_knn

