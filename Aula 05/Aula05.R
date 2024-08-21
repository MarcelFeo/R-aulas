library(ggplot2)

set.seed(1711)
iris <- iris[sample(nrow(iris)),]
n <- round(0.8*nrow(iris))

treino <- iris[1:n,]
teste <- iris[-(1:n),]

ggplot(data = treino, mapping = aes(x = Species)) +
  geom_bar()

ggplot(treino, aes(x = Petal.Length)) +
  geom_histogram(bins = 20, fill = "lightblue") +
  theme_minimal()

ggplot(treino, aes(y = Petal.Length)) +
  geom_boxplot() +
  facet_wrap(~Species)

ggplot(treino, aes(y = Sepal.Length)) +
  geom_boxplot() +
  facet_wrap(~Species)

ggplot(treino, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point()

results <- c()

for(j in 1:nrow(teste)) {
  if(teste$Petal.Length[j] < 2.5) {
    results[j] <- "setosa"
  } else {
    if(teste$Petal.Length[j] < 1.75) {
      results[j] <- "versicolor"
    } else {
      results[j] <- "virginica"
    }
  }
}

results

teste$Species == results

mean(teste$Species == results)
