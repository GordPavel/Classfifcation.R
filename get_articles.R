require(magrittr)
require(xml2)
require(data.table)
require(tm)
for (i in 2:47) {
  page <-
    read_html(paste0('http://www.avtomanual.com/articles/page/', i, '/'))
  articles <- page %>% xml_find_all(
    "/html/body/div[@id='container']/div/table/tr/td[@id='left']/div/div[@id='dle-content']/div[@class='shortstory']/div[@class='shorttitle']/*/a"
  )
  articles <- data.frame(
    title = articles %>% xml_attr('title'),
    link = articles %>% xml_attr('href'),
    stringsAsFactors = F
  )
  articles %>% purrrlyr::by_row(function(row) {
    title <- row$title
    link <- row$link
    page <- read_html(link)
    text <- page %>% xml_find_all(
      "/html/body/div[@id='container']/div/table/tr/td[@id='left']/div/div[@id='dle-content']/div[1]/div[1]/table/tr[1]/td/div[@class='fstory']/text()"
    ) %>% xml_text(trim = T) %>% paste0(collapse = ' ') %>% trimws(which = 'both')

    if (text != '') {
      temp.out.file.name <- suppressWarnings({
        tempfile('out', fileext = '.txt') %>% normalizePath
      })
      output.filename <-
        paste0('articles/' , uuid::UUIDgenerate() , '.txt')

      out.file <-
        file(temp.out.file.name,
             encoding = 'UTF-8',
             open = 'w')
      writeChar(text, out.file)
      close(out.file)

      system(paste0('mystem -wldc ', temp.out.file.name, ' ', output.filename))
    }
  })
}
rm(i,articles,page)