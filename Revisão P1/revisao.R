# Revisão

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

total_mortes <- sum(dados$death)

# letra d

library(ggplot2)

ggplot(data = chicago, aes(x = time, y = temp, fill = season)) +
  geom_point() +
  theme_minimal()

inverno <- chicago[chicago$season == "Winter"]



