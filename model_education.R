require(keras)
install_keras()

indexes <- sample(1:nrow(like.data), size = nrow(like.data) * .5)

x <- like.data$stemmed %>% one.hot.coding %>% as.matrix
y <- like.data$answer %>% as.numeric %>% to_categorical

x_train <- x[indexes, ]
y_train <- y[indexes, ]
x_val <- x[-indexes, ]
y_val <- y[-indexes, ]

model <- keras_model_sequential() %>%
  layer_embedding(
    input_dim = nrow(vocabulary) ,
    output_dim = ncol(vocabulary),
    input_length = ncol(x)
  ) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = ncol(y), activation = "softmax")

model %>% get_layer(model, index = 1) %>%
  set_weights(list(word_vectors %>% as.matrix)) %>%
  freeze_weights()

model %>% compile(optimizer = "rmsprop",
                  loss = "categorical_crossentropy",
                  metrics = c("accuracy"))

summary(model)

history <- model %>% fit(
  x_train,
  y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

plot(history)