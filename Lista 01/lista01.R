# Lista 01
# Aluno: Marcel Fernando Lobo de Féo - 12211BCC042

library(ggplot2)

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

voltou_origem <- 0

for (i in 1:10000) {
  pf <- link_passeio(8)
  
  if (all(pf == c(0, 0))) {
    voltou_origem <- voltou_origem + 1
  }
}

proporcao <- voltou_origem / 10000
# A proporção mostra quantas vezes Link voltou ao ponto inicial depois de 8 passos, em 10.000 simulações. Se a proporção for baixa, significa que é difícil ele voltar à origem, já que seus movimentos são aleatórios e ele tende a se afastar do ponto de partida.

# letra c

verifica_passos <- function(n) {
  if (n %% 2 == 1) {
    return("Impossível retornar à origem depois de um número ímpar de passos")
  } else {
    voltou_origem <- 0
    
    for (i in 1:10000) {
      pf <- link_passeio(n)
      
      if (all(pf == c(0, 0))) {
        voltou_origem <- voltou_origem + 1
      }
    }
    
    proporcao <- voltou_origem / 10000
    
    return(paste("A proporção de vezes que Link retornou à origem após", n, "passos é:", proporcao))
  }
}
verifica_passos(4)

#------------------------------------------------------------

# Exercício 12:

vitoria_garnit <- 0

for (i in 1:10000) {
  
  steven <- c(0, 1, 0)
  garnit <- c(0, 0, 1)
  
  ultimos <- c(NA, NA, NA)
  
  while (TRUE) {
    lancamento <- sample(x = 0:1, size = 1) # 0 = coroa e 1 = cara
    
    # Atualiza os três últimos lançamentos
    ultimos <- c(ultimos[2:3], lancamento)
    
    if (!any(is.na(ultimos))) {
      if (all(ultimos == steven)) {
        print("steven")
        break
      }
      if (all(ultimos == garnit)) {
        vitoria_garnit <- vitoria_garnit + 1
        print("garnit")
        break
      }
    }
  }
}

media_vitoria <- vitoria_garnit / 10000

#------------------------------------------------------------

# Exercício 13:

dados_arquivo <- read.csv("dados.txt", sep = ";")

# letra a

ggplot(dados_arquivo, aes(x = Genero)) +
  geom_bar(fill = "lightblue") +
  labs(x = "Gênero")

# Maioria das vítimas eram mulheres

# letra b

ggplot(dados_arquivo, aes(x = Idade)) +
  geom_histogram(bins = 8, fill = "lightpink", , color = "darkred") +
  labs(x = "Idade")

# Maior parte das vítimas são de idades avançadas

# letra c

ggplot(dados_arquivo, aes(y = Idade)) +
  geom_boxplot(fill = "lightgreen") +
  labs(y = "Idade")

# A mediana das idades das vítimas está em torno dos 80 anos.

# letra d

ggplot(dados_arquivo, aes(x = LocalDaMorte)) +
  geom_bar(fill = "lightyellow") +
  labs(x = "Local da Morte")

# Maioria das vítimas morreu em casa

# letra e

ggplot(dados, aes(x = AnoDaMorte)) +
  geom_histogram(binwidth = 1, fill = "lightgray", color = "black") +
  labs(x = "Ano da Morte")

# As mortes se estão ao longo de algumas décadas, com picos nos anos 80 e 90. 

# letra f

# Com base nos dados observados anteriores, podemos ver alguns padrões, como a maioria das vítimas sendo do sexo feminino, com idades avançadas, maioria das mortes sendo em casa. Esse perfil pode indicar que Shipman focava em pacientes idosos, muitas vezes em sua própria casa.

#------------------------------------------------------------

# Exercício 14:

# letra a

dados_arquivo2 <- read.table("primatas.txt", sep=":", header=TRUE)
str(dados_arquivo2)
summary(dados_arquivo2)

# letra b

ggplot(dados_arquivo2, aes(x=especie)) +
  geom_bar() +
  labs(title="Bonobos e Chimpanzés", x="Espécie", y="Contagem")

ggplot(dados_arquivo2, aes(x=especie, fill=genero)) +
  geom_bar(position="dodge") +
  labs(title="Machos e Fêmeas por Espécie", x="Espécie", y="Contagem", fill="Gênero")

# letra c

bonobos <- subset(dados_arquivo2, especie == "bonobo")

ggplot(bonobos, aes(x=genero, y=peso, fill=genero)) +
  geom_boxplot() +
  labs(title="Machos e Fêmeas de Bonobos", x="Gênero", y="Peso")

chimpanzes <- subset(dados_arquivo2, especie == "chimpanze")

ggplot(chimpanzes, aes(x=genero, y=peso, fill=genero)) +
  geom_boxplot() +
  labs(title="Machos e Fêmeas de Chimpanzés", x="Gênero", y="Peso")

# letra d

femeas <- subset(dados_arquivo2, genero == "femea")

ggplot(femeas, aes(x=especie, y=peso, fill=especie)) +
  geom_boxplot() +
  labs(title="Fêmeas de Bonobos e Chimpanzés", x="Espécie", y="Peso")

machos <- subset(dados_arquivo2, genero == "macho")

ggplot(machos, aes(x=especie, y=peso, fill=especie)) +
  geom_boxplot() +
  labs(title="Machos de Bonobos e Chimpanzés", x="Espécie", y="Peso")

# letra e

# Com base nas representações gráficas, observa-se que existem diferenças marcantes entre machos e fêmeas em bonobos e chimpanzés. Nos chimpanzés, os machos geralmente possuem um peso maior em comparação às fêmeas. Nos bonobos, embora essa diferença também esteja presente, ela é menos pronunciada. Ao comparar as duas espécies, os chimpanzés, de maneira geral, apresentam maior peso do que os bonobos, tanto entre machos quanto entre fêmeas.

# letra f

indices <- sample(1:nrow(dados_arquivo2), 0.7 * nrow(dados_arquivo2))
treino <- dados_arquivo2[indices, ]
teste <- dados_arquivo2[-indices, ]

prever <- function(altura, peso, genero) {
  if (peso > 60) {
    return("chimpanzé")
  } else if (peso <= 60 & genero == "M") {
    if (tamanho > 120) {
      return("chimpanzé")
    } else {
      return("bonobo")
    }
  } else if (peso <= 60 & genero == "F") {
    if (tamanho > 110) {
      return("chimpanzé")
    } else {
      return("bonobo")
    }
  } else {
    return("bonobo")
  }
}

resultados_primatas <- c()
for (i in 1:nrow(teste)) {
  resultados_primatas[i] <- prever(teste$altura[i], teste$peso[i], teste$genero[i])
}

mean(teste$especie == resultados_primatas)

#------------------------------------------------------------