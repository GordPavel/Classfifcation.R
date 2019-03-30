require(keras)

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
history <- model %>%
  keras::fit(
    data,
    labels,
    epochs = 40,
    batch_size = 100,
    validation_data = list(data,labels),
    validation_split = .3,
    verbose = F
    )