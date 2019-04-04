require(caret)
require(tictoc)

tic('One run')
history <- createFolds(1:nrow(data), k = 10) %>%
  lapply(function(validation.indexes) {
    x.education <- data[-validation.indexes, ]
    y.education <- labels[-validation.indexes, ]
    x.validation <- data[validation.indexes, ]
    y.validation <- labels[validation.indexes, ]
    
    model <- keras_model_sequential() %>%
      layer_embedding(input_dim = max.words,
                      output_dim = embedding.dim,
                      input_length = maxlen) %>%
      layer_lstm(units = FLAGS$lstm_units, 
                 return_sequences = T, 
                 recurrent_dropout = FLAGS$lstm_recurrnt_dropout ,
                 dropout = FLAGS$lstm_dropout) %>%
      layer_simple_rnn(units = FLAGS$rnn_units, 
                       recurrent_dropout = FLAGS$rnn_recurrnt_dropout, 
                       dropout = FLAGS$rnn_dropout) %>%
      layer_dense(units = ncol(labels), activation = "softmax") 
    model %>% get_layer(index = 1) %>%
      set_weights(list(embedding.matrix)) %>%
      freeze_weights
    model %>% compile(optimizer = FLAGS$optimizer,
                      loss = c('categorical_crossentropy'),
                      metrics = c("accuracy"))
    model %>%
      keras::fit(
        x.education,
        y.education,
        epochs = 25,
        batch_size = 100,
        validation_data = list(x.validation,y.validation),
        validation_split = .3,
        verbose = T
      )
  })
toc()