require(dplyr)
require(text2vec)
require(tm)
require(tictoc)
require(magrittr)
texts <- list.files('articles/', full.names = T) %>%
  sapply(function(file.name)
    readChar(file.name, file.info(file.name)$size))
texts.to.tokens <- function(texts) {
  texts %>%
    tolower %>%
    removeNumbers %>%
    removePunctuation %>%
    removeWords(words = c(stopwords(kind = 'en') , stopwords(kind = 'ru'))) %>%
    itoken_parallel(tokenizer = word_tokenizer , progressbar = F)
}
tic('Word embeddings preparing')
tokens <- texts %>% texts.to.tokens
vocabulary <- tokens %>% 
  create_vocabulary(ngram = c(1 , 4)) %>% 
  prune_vocabulary( term_count_min = 5 , term_count_max = 2000 )
vectorizer <- vocab_vectorizer(vocabulary)
tcm <- create_tcm(tokens, vectorizer)
glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocabulary, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
toc()
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)

one.hot.coding <- function( texts )
  texts %>% texts.to.tokens %>% create_dtm(vectorizer = vectorizer)
rm(texts,tcm,tokens,wv_context,wv_main,glove)