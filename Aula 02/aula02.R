# AULA 02 - LAÇOS DE REPETIÇÃO E FUNÇÃO

# FOR
x <- 0
for(i in 1:20) {
  x <- x + i
}

print(x)

#==================================================================

# PROBLEMA DO ANIVERSÁRIO NO MESMO DIA

aniversarios <- sample(x = 1:365, size = 10, replace = TRUE)
aniversarios

duplicated(aniversarios) # VERIFICA SE TEM VALORES DUPLICADOS NO VETOR

any(duplicated(aniversarios)) # VERIFICA SE TEM ALGUM TRUE

resultados <- c() # VETOR VAZIO

for(j in 1:10000) {
  aniversarios <- sample(x = 1:365, size = 10, replace = T)
  resultados[j] <- any(duplicated(aniversarios))
}

mean(resultados)

# Para conseguir 100% deve ser uma sala com 366 alunos, pelo princípio das casas dos pombos

# CRIANDO UMA FUNÇÃO PARA O PROBLEMA
probabilidade_aniversario <- function(n) {
  for(j in 1:10000) {
    aniversarios <- sample(x = 1:365, size = n, replace = T)
    resultados[j] <- any(duplicated(aniversarios))
  }
  
  return(mean(resultados))
}

#==================================================================

# PROBLEMA DA MEGA-SENA

bilhete <- c(4, 5, 12, 43, 21, 34) # SIMULA UM BILHETE

semanas <- 0 # QTD DE SEMANAS
acertos <- 0 # QTD DE ACERTOS

while(acertos < 4) {
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
  acertos <- sum(bilhete %in% sorteio)
  semanas <- semanas + 1
}

semanas/52 # SEMANAS EM ANOS

#==================================================================
















