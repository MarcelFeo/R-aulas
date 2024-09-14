library(ggplot2)

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
num_sorteio <- 0

for(i in 1:100000) {
  amigo_oculto <- sample(x = 1:5, size = 5, replace = TRUE)
  certo <- 1
  
  for(j in 1:5) {
    if(j == amigo_oculto[j]) {
      certo <- 0
      break
    }
  }
  
  resultado_sorteio[i] <- ifelse(certo, 0, 1)
}

mean(resultado_sorteio == 0)

#------------------------------------------------------------

# Exercício 09:

resultados_jogos <- c()

for (i in 1:100000) {
  
  lancamento1 <- sample(x = 1:6, size = 1)
  lancamento2 <- sample(x = 1:6, size = 1)
  
  soma_dados <- lancamento1 + lancamento2
  nova_soma <- soma_dados
  
  if (soma_dados == 7 | soma_dados == 11) {
    cat(sprintf("%d + %d = %d. Você venceu!\n", lancamento1, lancamento2, soma_dados))
    resultados_jogos[i] <- 1
    
  } else if (soma_dados == 2 | soma_dados == 3 | soma_dados == 12) {
    cat(sprintf("%d + %d = %d. Você perdeu!\n", lancamento1, lancamento2, soma_dados))
    resultados_jogos[i] <- 0
    
  } else {
    repeat {
      lancamento1 <- sample(x = 1:6, size = 1)
      lancamento2 <- sample(x = 1:6, size = 1)
      nova_soma <- lancamento1 + lancamento2
      
      if (nova_soma == 7 | nova_soma == soma_dados) {
        break
      }
    }
    
    if (nova_soma == 7) {
      cat(sprintf("%d + %d = %d. Você perdeu!\n", lancamento1, lancamento2, nova_soma))
      resultados_jogos[i] <- 0
      
    } else {
      cat(sprintf("%d + %d = %d. Você venceu!\n", lancamento1, lancamento2, nova_soma))
      resultados_jogos[i] <- 1
    }
  }
}

mean(resultados_jogos)

#------------------------------------------------------------

# Exercício 10:

# letra a
funcao_a <- function(l) {

  chegou <- 0
  caiu <- 0
  while(chegou == 0 & caiu == 0) {
    if (l >= 20) {
      print("Luke está em casa!")
      chegou <- 1
    } else if (l < 0) {
      print("Luke caiu no precipício!")
      caiu <- 1
    } else {
      sorteio_moeda = sample(x = 1:2, size = 1) # 1 -> coroa e 2 -> cara
      
      if (sorteio_moeda == 1) {
        l = l - 1
      } else {
        l = l + 1
      } 
    }
    
  }  
}

# letra b

funcao_b <- function(l) {
  
  resultados_luke <- c()
  
  for (i in 1:10000) {
    chegou <- 0
    caiu <- 0
    l_teste <- l
    
    while(chegou == 0 & caiu == 0) {
      if (l_teste >= 20) {
        chegou <- 1
        resultados_luke[i] <- 1
      } else if (l_teste < 0) {
        caiu <- 1
        resultados_luke[i] <- 0
      } else {
        sorteio_moeda = sample(x = 1:2, size = 1) # 1 -> coroa e 2 -> cara
        
        if (sorteio_moeda == 1) {
          l_teste = l_teste - 1
        } else {
          l_teste = l_teste + 1
        } 
      }
      
    }    
  }
  
  return (mean(resultados_luke))
  
}

# letra c
resultados_grafico <- c()
  
for (i in 1:19) { resultados_grafico[i] <- funcao_b(i) }

dados <- data.frame(L = 1:19, Proporcao = resultados_grafico)

ggplot(dados, aes(x = 1:19, y = resultados_grafico)) +
  geom_line() +
  geom_point() +
  labs(x = "Posição Inicial", y = "Proporção")

#------------------------------------------------------------

# Exercício 11:

# letra a
link_passeio <- function(n) {
  passos <- sample(c("L", "R", "U", "D"), n, replace = TRUE)
  ponto <- c(0, 0)
  
  for (p in passos) {
    if (p == "L") {
      ponto[1] <- ponto[1] - 1
    } else if (p == "R") {
      ponto[1] <- ponto[1] + 1
    } else if (p == "U") {
      ponto[2] <- ponto[2] + 1
    } else if (p == "D") {
      ponto[2] <- ponto[2] - 1
    }
  }
  
  return(ponto)
}

ponto_final <- link_passeio(8)
sprintf("Posição final: (%d, %d)", ponto_final[1], ponto_final[2])

# letra b
