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
  frame <- frame[,!apply(frame, 2, function(col) {
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
      row[1, ]) %>%
    rbindlist
}
like.data <- data %>%
  lapply(function(row)
    row$like) %>% rbindlist
like.data$preproceed <-
  like.data$question %>% as.character %>% preprocced.text
like.data <- delete.repeats(like.data)
dislike.data <- data %>%
  lapply(function(row)
    row$dislike) %>% rbindlist
dislike.data$preproceed <-
  dislike.data$question %>% as.character %>% preprocced.text
dislike.data <- delete.repeats(dislike.data)
breakdown.data <- data %>%
  lapply(function(row)
    row$breakdown) %>% rbindlist
breakdown.data$preproceed <-
  breakdown.data$question %>% as.character %>% preprocced.text
breakdown.data <- delete.repeats(breakdown.data)
feedback.data <- data %>%
  lapply(function(row)
    row$feedback) %>% rbindlist
feedback.data$preproceed <-
  feedback.data$question %>% as.character %>% preprocced.text
feedback.data <- delete.repeats(feedback.data)
rm(
  all.data,
  data,
  data.names,
  delete.repeats,
  get.data,
  get.regex,
  normalize.data,
  preprocced.text
)