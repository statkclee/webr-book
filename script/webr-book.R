library(tidyverse)
library(pdftools)

# 1. TOC 전처리 ----------------------
webr_toc <- pdftools::pdf_toc("data/webr.pdf")

webr_toc_lvl_01 <- pluck(webr_toc, "children")

## 1.1. 중제목
webr_toc_subtitle <- pluck(webr_toc_lvl_01, 1) %>% 
  as_tibble() %>% 
  unnest_longer(children) %>% 
  filter(children_id == "title") %>% 
  mutate(subtitle = map_chr(children, 1)) %>% 
  select(title, subtitle)

## 1.2. 소제목
get_subtitle <- function(chapter) {
  pluck(webr_toc_lvl_01, 1) %>% 
  as_tibble() %>% 
  unnest_longer(children) %>% 
  filter(row_number() == 2 * chapter) %>% 
  unnest(children) %>% 
  mutate(subsubtitle = map_chr(children, 1)) %>% 
  mutate(subtitle = webr_toc_subtitle$subtitle[chapter]) %>% 
  select(title, subtitle, subsubtitle)
}  

get_subtitle(3)

## 1.3. TOC 결합

webr_toc_tbl <- get_subtitle(1) %>% 
  bind_rows(get_subtitle(2)) %>% 
  bind_rows(get_subtitle(3)) 

webr_toc_tbl %>% 
  write_rds("data/webr_toc_tbl.rds")

# 2. 머리말 추출 ----------------------
webr_txt <- pdftools::pdf_text("data/webr.pdf")

webr_preface <- str_c(webr_txt[[2]], webr_txt[[3]]) %>% 
  str_remove_all(pattern = "\r\n")

webr_preface %>% 
  write_rds("data/webr_preface.rds")
