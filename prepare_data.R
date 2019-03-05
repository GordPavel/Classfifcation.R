sourcesFolder <- 'исходники/'

require(foreign)
require(dplyr)
require(stringr)
require(magrittr)

all.data <-
  list.files(path = sourcesFolder , pattern = 'original\\d+.sav') %>%
  lapply(function(name)
    suppressMessages({
      read.spss(paste0(sourcesFolder, '/' , name),
                to.data.frame = T,
                reencode = 'UTF-8')
    }))

get.regex <- function(text, data.names) {
  paste0('^V(', paste0(
    c(
      data.names %>%
        filter(!grepl('_cod$', code)) %>%
        filter(grepl(text, tolower(get(
          'name'
        )), ignore.case = T)) %>%
        transmute(num = str_extract(code, '\\d{2}\\.\\d{1,2}'))
    )$num,
    collapse = '|'
  ), ')\\.?t')
}
get.data <-
  function(regex, data.names)
    all.data[[1]][, grep(regex, data.names$code, ignore.case = T)]

normalize.data <- function(frame) {
  frame <- data.frame(apply(frame, 2, function(col) {
    trimws(as.character(col))
  }))
  frame[frame == ''] <- NA
  frame <-
    frame[, !apply(frame, 2, function(col) {
      all(is.na(col))
    })]
  question.columns.seq <- seq(1, ncol(frame) - 1, 2)
  names(frame)[question.columns.seq] <- 'question'
  answer.columns.seq <- seq(2, ncol(frame), 2)
  names(frame)[answer.columns.seq] <- 'answer'
  na.omit(data.frame(
    question = c(as.character(unlist(frame[, question.columns.seq]))),
    answer = c(as.character(unlist(frame[, answer.columns.seq])))
  ))
}

data.names <-
  data.frame(code = names(all.data[[1]]),
             name = attributes(all.data[[1]])$variable.labels)

data <- lapply(all.data, function(frame) {
  list(
    like = get.regex('любите', data.names) %>% get.data(data.names) %>% normalize.data,
    dislike = get.regex('не нравится', data.names) %>% get.data(data.names) %>% normalize.data,
    breakdown = get.regex('неисправность', data.names) %>% get.data(data.names) %>% normalize.data,
    feedback = get.regex('претензи', data.names) %>% get.data(data.names) %>% normalize.data
  )
})

require(data.table)
require(tm)

preprocced.text <-
  function(texts)
    texts %>% removePunctuation %>% removeNumbers

delete.repeats <- function(frame , questionColumn = 'question') {
  frame %>% split(f = frame %>% pull(question) %>% tolower) %>%
    lapply(function(row)
      row[1,]) %>%
    rbindlist
}

like.data <- data %>%
  lapply(function(row)
    row$like) %>%
  rbindlist
like.data$preproceed <-
  like.data$question %>% as.character %>% preprocced.text
like.data <- delete.repeats(like.data)

dislike.data <-
  data %>%
  lapply(function(row)
    row$dislike) %>%
  rbindlist
dislike.data$preproceed <-
  dislike.data$question %>% as.character %>% preprocced.text
dislike.data <- delete.repeats(dislike.data)

breakdown.data <- data %>%
  lapply(function(row)
    row$breakdown) %>%
  rbindlist
breakdown.data$preproceed <-
  breakdown.data$question %>% as.character %>% preprocced.text
breakdown.data <- delete.repeats(breakdown.data)

feedback.data <- data %>%
  lapply(function(row)
    row$feedback) %>%
  rbindlist
feedback.data$preproceed <-
  feedback.data$question %>% as.character %>% preprocced.text
feedback.data <- delete.repeats(feedback.data)

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
  intTexts <- texts %>%
    as.list %>%
    lapply(utf8ToInt)
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
    texts %>%
      tolower %>%
      split((seq(length(texts)) - 1) %/% batch.size) %>%
      lapply(fix.sentences) -> res
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

my.stem <- function(texts) {
  temp.out.file.name <- suppressWarnings({
    tempfile('out', fileext = '.txt') %>% normalizePath
  })
  temp.in.file.name <- suppressWarnings({
    tempfile('in', fileext = '.txt') %>% normalizePath
  })
  out.file <-
    file(temp.out.file.name, encoding = 'UTF-8', open = 'w')
  texts %>% writeLines(out.file)
  close(out.file)
  system(paste0('mystem -wldc ', temp.out.file.name, ' ', temp.in.file.name))
  in.file <- file(temp.in.file.name, encoding = 'UTF-8', open = 'r')
  text <- readLines(in.file) %>% removePunctuation
  close(in.file)
  text
}
like.data$stemmed <- like.data$fixed %>% my.stem
dislike.data$stemmed <- dislike.data$fixed %>% my.stem
breakdown.data$stemmed <- breakdown.data$fixed %>% my.stem
feedback.data$stemmed <- feedback.data$fixed %>% my.stem