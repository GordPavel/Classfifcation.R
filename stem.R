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
rm(my.stem)
like.data$stemmed <- like.data$fixed %>% my.stem
dislike.data$stemmed <- dislike.data$fixed %>% my.stem
breakdown.data$stemmed <- breakdown.data$fixed %>% my.stem
feedback.data$stemmed <- feedback.data$fixed %>% my.stem