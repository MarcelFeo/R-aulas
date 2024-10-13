library(rvest)
library(dplyr)
library(tidytext)
library(tidyr) # organizar um conjunto
library(ggplot2)
library(stopwords)
library(rvest) # raspar dados
library(stringr)
library(janeaustenr) # todos livros da jane austen

livros <- austen_books() # todos livros da jane austen

livros |>
  filter(book == "Emma") |>
  unnest_tokens(output = word, input = text) |>
  count(word, sort = TRUE) |>
  top_n(10)

stopwords_en <- data.frame(word = stopwords("en"))

livros |>
  filter(book == "Emma") |>
  unnest_tokens(output = word, input = text) |>
  anti_join(stopwords_en) |>
  count(word, sort = TRUE) |>
  top_n(10)

emma <- livros |>
  filter(book == "Emma")

capitulos <- str_detect(emma$text, "^CHAPTER [IVXLCDM]+")
capitulos <- cumsum(capitulos)
emma$capitulos <- capitulos
str(emma)

emma |>
  unnest_tokens(word, text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"), relationship = "many-to-many") |>
  count(capitulos, sentiment) |>
  spread(sentiment, n, fill = 0) |> # spread está dentro da biblioteca tidyr
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total)) +
  geom_col(fill = "lightblue") +
  theme_classic()

livros |>
  group_by(book) |>
  mutate(capitulos = cumsum(str_detect(text, regex("^chapter (\\d|[IVXCDLM])+", ignore_case = TRUE)))) |>
  ungroup() |>
  unnest_tokens(word, text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"), relationship = "many-to-many") |>
  count(book, capitulos, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book,)

