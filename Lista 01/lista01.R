# Lista 01
# Aluno: Marcel Fernando Lobo de Féo - 12211BCC042
#------------------------------------------------------------

# Exercício 01:

# (a)
a <- seq(10, 30, by = 1)
a

# (b)
b <- seq(30, 10, by = -1)
b

# (c)
c <- vetor <- c(seq(10, 30, by = 1), seq(30, 10, by = -1))
c

#------------------------------------------------------------

# Exercício 02:

# (a)
vet1 <- seq(2, 8, by = 2)
vet2 <- rep(vet1, times = 10)
vet2

# (b)
vet3 <- rep(vet1, times = 11)
vet4 <- vet3[seq(1, 41)]
vet4

#------------------------------------------------------------

# Exercício 03:

# (a)
n1 <- seq(20, 30, by = 1) 
r1 <- sum(n1^2 + 4*n1)
r1

# (b)
n2 <- seq(10, 20, by = 1)
r2 <- sum((3^n2)/n2 + (2^n2)/(n2^2))
r2

#------------------------------------------------------------

# Exercício 04:

sorteio <- sample(x = 1:100, size = 40, replace = TRUE)

# (a)
total_pares = sum(sorteio %% 2 == 0)

# (b)
maior_setenta = sum(sorteio > 70)

# (c)
for(i in 1:40) {
  if(sorteio[i] %% 2 != 0) {
    print(i)
  }
}

#------------------------------------------------------------

# Exercício 05:

joga_dado <- function() {
  acertos <- 0
  n <- 0
  
  while(acertos < 2) {
    dado <- sample(x = 1:6, size = 1, replace = TRUE)
    
    if(dado == 4) {
      acertos <- acertos + 1
    }
    
    n <- n + 1
  }

  return(n)
}

#------------------------------------------------------------

# Exercício 06:

quantidades <- c()

for(i in 1:10000) {
  num_lancamento <- joga_dado()
  quantidades[i] <- num_lancamento
}

# mean(quantidades) - A média que calculamos mostra a média de quantos lançamentos são necessários para que o número 4 seja obtido pela segunda vez durante uma sequência de lançamentos do dado.

#------------------------------------------------------------

# Exercício 07:

fibonacci <- function(n) {
  if (n < 3) {
    stop("O valor de entrada da função deve ser n >= 3.")
  }
  
  x <- c(1, 1)
  
  for (i in 3:n) { x[i] <- x[i-1] + x[i-2] }
  
  return(x)
}

#------------------------------------------------------------

# Exercício 08:

funcionarios <- c("Michael Scott", "Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton")

resultado_sorteio <- c()

for(i in 1:100000) {
  for(j in 1:5) {
    amigo_oculto <- sample(x = 1:5, size = 1, replace = TRUE)
    
    if(funcionarios[j] == funcionarios[amigo_oculto]) {
      resultado_sorteio[i] <- 0
    } else {
      resultado_sorteio[i] <- 1
    }
  }
}

mean(resultado_sorteio)

#------------------------------------------------------------

















