library(tidyverse)
library(stringr)
library(rvest)

html <- read_html("http://www.library.upenn.edu/cgi-bin/res/sr.cgi?fotlp=&community=77&resourcetype=1")

cast <- html_nodes(html, ".strongBrowseLink")
length(cast)
title <- html_text(cast)


cast <- html_nodes(html, ".browseLink")
cast <- cast[html_text(cast) == 'more']
length(cast)
id <- html_attr(cast, name = 'href') %>% str_extract('\\d+')



info <- list()

for(x in id) {
  
  Sys.sleep(3)
  
  print(x)
  url <- sprintf('http://www.library.upenn.edu/cgi-bin/res/fullview.cgi?resID=%s', x)
  
  html1 <- read_html(url)
  
  cast <- html_nodes(html1, 'div div')
  desc <- html_text(cast) %>% 
    .[str_detect(., 'Description:')] %>% 
    `if`(length(.) == 0, NA_character_, .) %>% 
    trimws()

  holding <- html_text(cast) %>% 
    .[str_detect(., 'Holdings:')] %>%
    `if`(length(.) == 0, NA_character_, .) %>%
    trimws()
  
  cast <- html_nodes(html1, 'strong~ .smallBlueonWhite')
  rel <- html_text(cast)
  
  cast <- html_nodes(html1, '.hanging:nth-child(1)')
  also <- html_text(cast) %>% 
    str_extract('\\n\\W+?Also Known as:.+?\\n') %>% 
    trimws()
  
  out <- list(desc = desc, holding = holding, rel = rel, also = also)
  
  info[[x]] <- out
}

dat <- tibble(title, 
              also = map_chr(info, 'also'), 
              desc = map_chr(info, 'desc'), 
              holding = map_chr(info, 'holding'), 
              rel = map_chr(info, ~paste0(.x$rel, collapse = ' | ')))
write_csv(dat, file.choose())
