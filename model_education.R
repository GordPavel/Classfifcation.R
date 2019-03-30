require(tm)
require(magrittr)
require(dplyr)
require(data.table)
require(keras)
install_keras()

max.words <- 10000
maxlen <- 20

tokenizer <- text_tokenizer(num_words = max.words) %>% 
  fit_text_tokenizer(all.data$stemmed)

word.index <- tokenizer$word_index

embedding.dim <- ncol(word.embedding) - 1
word.embedding <- word.embedding %>% as.data.frame
embedding.matrix <- matrix(0,nrow = max.words,ncol = ncol(word.embedding) - 1)

for(word in names(word.index)) {
  index <- word.index[[word]]
  embedding.vector <-
    (word.embedding %>% filter(V1 == word))[1, 2:301] %>% as.numeric
  if (index < max.words && !(embedding.vector %>% is.na %>% any))
    embedding.matrix[index + 1 ,] <- embedding.vector
}

sequences <- texts_to_sequences(tokenizer,all.data$stemmed)
data <- pad_sequences(sequences,maxlen = maxlen)
labels <- ( all.data$cluster %>% as.numeric - 1 ) %>% to_categorical

test.indexes <- sample(1:nrow(all.data), size = nrow(all.data) * .3)
test.data <- data[test.indexes,]
test.labels <- labels[test.indexes,]

data <- data[-test.indexes,]
labels <- labels[-test.indexes,]

require(tfruns)
require(tfestimators)
validation.indexes <- sample(1:nrow(data), size = nrow(data) * .3)
x.education <- data[-validation.indexes, ]
y.education <- labels[-validation.indexes, ]
x.validation <- data[validation.indexes, ]
y.validation <- labels[validation.indexes, ]

FLAGS <- flags(
  flag_integer("lstm_units", 128),
  flag_numeric("lstm_recurrnt_dropout", .5),
  flag_numeric("lstm_dropout", .2),
  flag_integer("rnn_units", 64),
  flag_numeric("rnn_recurrnt_dropout", .5),
  flag_numeric("rnn_dropout", .7),
  flag_string("optimizer", 'rmsprop')
)

runs <- tuning_run(
  "run_model.R",
  flags = list(
    lstm_units = c(64,128),
    lstm_recurrnt_dropout = с(.1,.5,.9),
    lstm_dropout = с(.1,.5,.9),
    rnn_units = c(64,128),
    rnn_recurrnt_dropout = с(.1,.5,.9),
    rnn_dropout = с(.1,.5,.9),
    optimizer = c('SGD','RMSprop')
  )
)

accuracy <- model %>% evaluate(test.data, test.labels)
predictions <-
  model %>%
  predict(test.data)

predictions <- predictions %>%
  apply(MARGIN = 1, function(row)
    list(prob = max(row) , class = which.max(row))) %>%
  rbindlist

rm(
  data,
  tokenizer,
  embedding.matrix,
  embedding.dim,
  x_education,
  y_education,
  x_validation,
  y_validation,
  x_test,
  y_test,
  education.indexes,
  probabs,
  test.indexes,
  validation.indexes,
  split.proportionally,
  embedding.vector,
  index,
  max.words,
  maxlen,
  texts,
  indexes,
  word,
  sequences,
  word.index,
  labels
)