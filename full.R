sourcesFolder <- 'исходники/'
source('import.data.R')
source('fix.mistakes.R')
source('stem.R')
require(zeallot)
require(data.table)

split.propartionally <-
  function(frame, answerColumn = 'answer' , coefs) {
    groups <- names(coefs)
    classes <- frame %>%
      split(frame %>% dplyr::pull(answerColumn)) %>%
      lapply(function(class)
        class %>% split(sample(
          groups ,
          size = nrow(class),
          prob = coefs ,
          replace = T
        )))
    groups %>% as.list %>% lapply(function(index)
      classes %>% lapply(function(class) {
        if (index %in% names(class))
          class[[index]]
      }) %>% rbindlist %>% sample)
  }
# Анализ текстов