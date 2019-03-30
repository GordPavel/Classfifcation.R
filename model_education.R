require(tm)
require(magrittr)
require(dplyr)
require(keras)
# install_keras()

texts <- like.data$stemmed
labels <- kmed$clusters %>% as.numeric %>% to_categorical

max.words <- 10000
maxlen <- 20
tokenizer <- text_tokenizer(num_words = max.words) %>% 
  fit_text_tokenizer(texts)

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

sequences <- texts_to_sequences(tokenizer,texts)
data <- pad_sequences(sequences,maxlen = maxlen)
indexes <- sample(1:nrow(data) , size = nrow(data) * .7)
x_train <- data[indexes,]
y_train <- labels[indexes,]
x_val <- data[-indexes,]
y_val <- labels[-indexes,]

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max.words,
                  output_dim = embedding.dim,
                  input_length = maxlen) %>%
  layer_lstm(units = 128, return_sequences = T, 
             recurrent_dropout = .2 ,
             dropout = .2) %>%
  layer_simple_rnn(units = 64) %>%
  layer_dense(units = ncol(labels), activation = "softmax") 
model %>% get_layer(index = 1) %>%
  set_weights(list(embedding.matrix)) %>%
  freeze_weights
model %>% compile(optimizer = "rmsprop",
                  loss = "categorical_crossentropy",
                  metrics = c("accuracy"))
history <- model %>%
  keras::fit(
    x_train,
    y_train,
    epochs = 20,
    batch_size = 64,
    validation_data = list(x_val, y_val),
    validation_split = .2
  )

plot(history)
