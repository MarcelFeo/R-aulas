library(ggplot2)        # Carrega a biblioteca ggplot2 para criação de gráficos
library(palmerpenguins) # Carrega a biblioteca palmerpenguins, que contém o conjunto de dados dos pinguins
library(class)          # Carrega a biblioteca class, que contém funções para classificação, como o KNN

data(penguins)          # Carrega o conjunto de dados dos pinguins na variável 'penguins'
pinguins <- penguins    # Cria uma cópia do conjunto de dados na variável 'pinguins'

pinguins <- pinguins[, -c(2, 7, 8)] # Remove as colunas 2, 7 e 8 (ano, ilha, e código de anel) do conjunto de dados
pinguins <- na.omit(pinguins)       # Remove linhas com valores NA do conjunto de dados

n <- round(0.8 * nrow(pinguins))    # Calcula 80% do número total de linhas para definir o tamanho do conjunto de treino

indices_treino <- sample(1:nrow(pinguins), size = n, replace = FALSE) # Gera uma amostra aleatória de índices para o conjunto de treino

treino <- pinguins[indices_treino,] # Cria o conjunto de treino usando os índices gerados
teste <- pinguins[-indices_treino,] # Cria o conjunto de teste com as linhas que não estão no conjunto de treino

treino_padronizado <- scale(treino[, -1]) # Padroniza os dados do treino (exceto a primeira coluna, que contém as espécies)
teste_padronizado <- scale(teste[,-1])    # Padroniza os dados do teste (exceto a primeira coluna, que contém as espécies)

classe_treino <- treino$species # Armazena a coluna de espécies do conjunto de treino
classe_teste <- teste$species   # Armazena a coluna de espécies do conjunto de teste

modelo01 <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = 1) # Cria um modelo KNN com k = 1
mean(modelo01 == teste$species) # Calcula a taxa de acerto do modelo comparando as predições com as espécies reais do conjunto de teste

cor(treino[, -1]) # Calcula a matriz de correlação das variáveis no conjunto de treino (exceto a coluna de espécies)

taxa_acerto <- c() # Inicializa um vetor para armazenar a taxa de acerto para diferentes valores de k

for(k in 1:10) {   # Loop de 1 a 10 para testar diferentes valores de k
  modelo <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = k) # Cria um modelo KNN para cada valor de k
  taxa_acerto[k] <- mean(modelo == teste$species) # Calcula a taxa de acerto para cada valor de k e armazena no vetor taxa_acerto
}

df <- data.frame(k = 1:10, taxa_acerto) # Cria um dataframe com os valores de k e as respectivas taxas de acerto

ggplot(data = df, aes(x = k, y = taxa_acerto)) + # Cria um gráfico com ggplot2 usando k no eixo x e a taxa de acerto no eixo y
  geom_line()                                   # Adiciona uma linha ao gráfico para conectar os pontos
