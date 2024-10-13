library(ggplot2)

femur <- read.csv("femur.csv", header=TRUE)

femur_male <- subset(femur, genero == "Male")
femur_female <- subset(femur, genero == "Female")

cor_male <- cor(femur_male$altura, femur_male$femur) # 90%
ggplot(femur_male, aes(x=altura, y=femur)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Relação altura e femur homens")

cor_female <- cor(femur_female$altura, femur_female$femur) # 77%
ggplot(femur_female, aes(x=altura, y=femur)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Relação altura e femur mulheres")

modelo_linear <- lm(data = femur_male, formula = altura ~ femur) # encontra melhor reta
