library(rvest) # raspagen
library(dplyr) # manipulação de dados
library(stringr) # manipulação de strings
library(geobr) # info geograficas
library(ggplot2)

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o"
html <- read_html(url)

tabelas <- html |>
  html_elements("table") |>
  html_table()

alfabetizacao <- tabelas[[3]]

alfabetizacao <- alfabetizacao[,c(2, 3)]
names(alfabetizacao) <- c("estado", "taxa")

parte1 <- str_replace_all(string = alfabetizacao$taxa, pattern = "," , replacement = ".")
parte2 <- str_replace_all(string = parte1, pattern = "%" , replacement = "")

parte_final <- as.numeric(parte2)
parte_final <- parte_final/100

alfabetizacao$taxa <- parte_final

mg <- read_state(code_state = "MG")

ggplot(data = mg) +
  geom_sf(fill = "lightblue") +
  theme_void()

municipios_mg <- read_municipality(code_muni = "MG")

ggplot(data = municipios_mg)+
  geom_sf() +
  theme_void()

estados <- read_state()

# ordenando por ordem alfabetica
order(estados$name_state)
estados <- estados[order(estados$name_state),]
alfabetizacao <- alfabetizacao[order(alfabetizacao$estado),]

# cria na tabela estado uma coluna taxa colando os valores de alfabetizacao$taxa
estados$taxa <- alfabetizacao$taxa

ggplot(data = estados, aes(fill = taxa)) +
  geom_sf() +
  scale_fill_gradient(high = "#3d4b3a", low = "#31f906")






View(alfabetizacao)
