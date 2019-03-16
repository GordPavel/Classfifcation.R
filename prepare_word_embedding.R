word.embedding <-
  read.table(
    "word.model.txt",
    quote = "\"",
    comment.char = "",
    stringsAsFactors = F
  )
word.embedding$V1 <- gsub("(.+)_\\w+", '\\1' , word.embedding$V1)
stopwords <- stopwords('ru')
word.embedding <-
  word.embedding %>% filter(!(V1 %in% stopwords)) %>% filter(!grepl("[[:punct:]]" , V1))