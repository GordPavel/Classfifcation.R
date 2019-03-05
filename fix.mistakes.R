require(stringi)
require(jsonlite)
require(httr)
yandex.speller <-
  'http://speller.yandex.net/services/spellservice.json/checkTexts'
string.length <-
  function(str)
    (str %>% strsplit(split = '', fixed = T))[[1]] %>% length
Rcpp::sourceCpp('fixTexts.cpp')
apply.fixes <- function(result, texts) {
  intTexts <- texts %>% as.list %>% lapply(utf8ToInt)
  result <- result %>%
    lapply(function(fix) {
      fix %>% lapply(function(word) {
        if (word$s %>% length > 0)
          list(
            from = word$pos ,
            len = word$len ,
            correct = word$s[[1]] %>% utf8ToInt
          )
        else
          list(from = -1 ,
               len = -1 ,
               correct = NULL)
      })
    })
  fixTexts(result, intTexts) %>%
    lapply(intToUtf8) %>%
    as.character
}
fix.sentences <- function(texts) {
  formatted <- texts %>%
    sapply(
      function(text)
        text %>%
        stringr::str_replace_all(pattern = '\\s', replacement = '+') %>%
        as.vector %>%
        as.character
    ) %>% as.vector %>%
    paste(collapse = '&text=')
  url <- paste0(yandex.speller, '?text=' , formatted , '&lang=ru')
  result <- httr::content(GET(url))
  apply.fixes(result, texts)
}
yandex.request <-
  function(texts, batch.size = 100) {
    texts %>% tolower %>% split((seq(length(texts)) - 1) %/% batch.size) %>% lapply(fix.sentences) -> res
    Reduce(c, res)
  }
like.data$fixed <-
  yandex.request(like.data$preproceed) %>% as.character
dislike.data$fixed <-
  yandex.request(dislike.data$preproceed) %>% as.character
breakdown.data$fixed <-
  yandex.request(breakdown.data$preproceed) %>% as.character
feedback.data$fixed <-
  yandex.request(feedback.data$preproceed) %>% as.character

rm(yandex.speller,apply.fixes,fix.sentences,fixTexts,string.length,yandex.request)