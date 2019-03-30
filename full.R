sourcesFolder <- 'исходники/'
source('import.data.R')
source('fix.mistakes.R')
source('stem.R')
require(text2vec)
require(magrittr)
require(dplyr)
stopwords <- c(stopwords(kind = 'en') , stopwords(kind = 'ru'))
like.data$stemmed <- like.data$stemmed %>%
  tolower %>%
  removeNumbers %>%
  removePunctuation %>%
  removeWords(words = stopwords)
dislike.data$stemmed <- dislike.data$stemmed %>%
  tolower %>%
  removeNumbers %>%
  removePunctuation %>%
  removeWords(words = stopwords)
feedback.data$stemmed <- feedback.data$stemmed %>%
  tolower %>%
  removeNumbers %>%
  removePunctuation %>%
  removeWords(words = stopwords)
breakdown.data$stemmed <- breakdown.data$stemmed %>%
  tolower %>%
  removeNumbers %>%
  removePunctuation %>%
  removeWords(words = stopwords)