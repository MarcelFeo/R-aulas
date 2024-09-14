library(httr)
library(rvest)
library(dplyr)
library(ggplot2)
library(tidytext)

url <- "https://blaze1.space/api/roulette_games/history?startDate=2024-08-14T16:45:04.506Z&endDate=2024-09-13T16:45:04.507Z&page=1"

GET(url)
dados <- content(GET(url), "text")

dados <- fromJSON(dados)
table(dados$records$color)

url_base <- "https://blaze1.space/api/roulette_games/history?startDate=2024-08-14T16:45:04.506Z&endDate=2024-09-13T16:45:04.507Z&page="

resultados <- c()

for (i in 1:50) {
  url <- paste0(url_base, i)
  dados <- content(GET(url), "text")
  dados <- fromJSON(dados)
  resultados <- c(resultados, dados$records$color)
}

resultados
prop.table(table(resultados))

url <- "https://www.letras.mus.br/fleetwood-mac/171854/"

html <- read_html(url)
html |>
  html_elements("h1") |>
  html_text2()

letra <- html |>
  html_elements("div.lyric") |>
  html_elements("p") |>
  html_text2() |>
  paste(collapse = " ")

letra <- data.frame(letra)

letra |>
  unnest_tokens(output = word, input = letra) |>
  count(word, sort = TRUE) |>
  head(n = 10) |>
  ggplot(aes(y = word, x = n)) +
  geom_col()
