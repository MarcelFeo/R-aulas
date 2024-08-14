# Aula 03 - Analise de dados

# dados do arquivo titanic.txt, primeira linha são nomes das colunas e os valores são separados por vírgula
dados <- read.table(file = "Aula 03/titanic.txt", header = TRUE, sep = ",")

# comando para mostrar tabela com os dados
View(dados)

# sobreescrevendo os dados excluindo a primeira coluna
dados <- dados[,-2]

# tirar coluna 1 e 12 do vetor
dados <- dados[, -c(1, 12)]

# transformando valores não númericos em fatores
dados$Survived <- as.factor(dados$Survived)
dados$Survived

dados$Sex <- as.factor(dados$Sex)
dados$Sex

dados$Pclass <- as.factor(dados$Pclass)
dados$Pclass

# resumo estatísticos dos dados
summary(dados)

# número de total de sobreviventes
s <- dados[,2]
total <- sum(s)
total

# primeira linha e quarta coluna
dados[1, 4]
# sexto e decimo passageiro
dados[c(6, 10),]

# analise das sobrevivencias dos homens e mulheres
homens <- dados[dados$Sex == "male",]
summary(homens)

table(homens$Survived)
barplot(table(homens$Survived))

mulheres <- dados[dados$Sex == "female",]
summary(mulheres)

table(mulheres$Survived)
barplot(table(mulheres$Survived))

# Quantos homens da terceira classe morreram?
homemtc <- homens[homens$Pclass == 3,]
summary(homemtc)

#=======================================================================

# Observações:

# exemplo: tirar coluna 1 e 12 do vetor
#dados <- dados[, -c(1, 12)]