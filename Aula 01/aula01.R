# Aula 01 - Introdução

# Operações Básicas
2 + 2 # = 4 (soma)

7 - 2 # = 5 (subtração)

2 * 5 # = 10 (multiplicação)

6 / 2 # = 3 (divisão)

2**3 # = 8 (1ª forma - potenciação)

2^3 # = 8 (2ª forma - potenciação)

#================================================================================

# Criação de Objetos

x <- 5 * 2 # atribuindo ao objeto x o valor 5 * 2 = 10
x + 1 -> y # atribuindo ao objeto y o valor x + 1 = 11
y # = 11

#================================================================================

# Vetores

vet1 <- c(1, 2, 3)
vet2 <- c(TRUE, FALSE)
vet3 <- c(4, 5, 6)


vet1 + vet2 # vetores de tamanho difentes: comprimento do objeto maior não é                   múltiplo do comprimento do objeto menor. O R soma o primeiro                     elemento do vet1 com o elemento do vet2, depois soma o segundo do                vet1 com o segundo do vet2, como o vet1 tem mais um elemento o R                 pega o primeiro elemento do vet2 novamente e soma com o último                   elemento do vet1, se o vet1 tivesse um quarto elemento o R somaria               esse quarto elemento com o segundo elemento do vet2

vet1 + vet3 # [5, 7, 9]

# Obtendo uma posição específica do vetor

vet1[1] # 1
vet3[3] # 6
vet3[c(2,3)] # 5, 6

sum(vet1) # soma todos os valores dentro do vetor, nesse caso é 6

sum(vet1 < 2) # sum(soma uma condição), nesse caso quantos valores de vet1 são                 menores que 2

#================================================================================

# Funções

# class() - diz o tipo do objeto

word <- "palavra" # character

bool <- TRUE # logical

class(bool)

# mean() - média

test <- sample(x = 1:100, size = 100)

mean(test == 1)

# sample() - sorteia

# Qual a probabilidade de a soma de dois dados ser 3, considerando 10.000 lançamentos?

dado1 <- sample(x = 1:6, size = 10000, replace = TRUE)
dado2 <- sample(x = 1:6, size = 10000, replace = TRUE)

soma <- dado1 + dado2

mean(soma == 3) 

# sum(dado1 == 3) # Quantas vezes o lançamento do dado1 saiu o número3

#================================================================================

# Observações

# No R TRUE vale 1 e FALSE vale 0. Exemplos:

TRUE + TRUE # = 2
TRUE + FALSE # = 1
FALSE + FALSE # = 0
FALSE + TRUE # = 1

# Se fizer um sum com valores lógico ele vai contar quantos TRUE tem dentro do vetor. Exemplos:

vetTest <- c(TRUE, FALSE, TRUE)
sum(vetTest)

# Se fazer ?[nome da função] a ide abre a documentação da linguagem


















