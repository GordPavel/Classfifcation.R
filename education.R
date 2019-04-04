load("~/социология/before_education.RData")
require(magrittr)
require(dplyr)
require(keras)
require(tfruns)
require(tfestimators)
install_keras()

runs <- tuning_run(
  "run_model.R",
  flags = list(
    lstm_units = c(64, 96, 128),
    lstm_recurrnt_dropout = c(.3,.5, .7),
    lstm_dropout = c(.3,.5, .7),
    rnn_units = c(64, 96, 128),
    rnn_recurrnt_dropout = c(.3,.5, .7),
    rnn_dropout = c(.3,.5, .7),
    optimizer = c('SGD', 'RMSprop')
  ),
  confirm = F,
  echo = F
)
save.image('~/социология/after_education.RData')