library(tidyverse)
library(pdftools)

# 1. TOC 전처리 ----------------------
webr_toc <- pdftools::pdf_toc(glue::glue("{here::here()}/data/webr.pdf"))

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
  write_rds(glue::glue("{here::here()}/data/webr_toc_tbl.rds"))

# 2. 머리말 추출 ----------------------
webr_txt <- pdftools::pdf_text(glue::glue("{here::here()}/data/webr.pdf"))

webr_preface <- str_c(webr_txt[[2]], webr_txt[[3]]) %>% 
  str_remove_all(pattern = "\r\n") %>% 
  str_remove_all(pattern = "(\n)")  %>% 
  str_remove_all(pattern = "\\\\")


webr_preface %>% 
  write_rds(glue::glue("{here::here()}/data/webr_preface.rds"))

# 2. PDF 이미지 추출 ---------------------

# brew install poppler
# mkdir images/
# pdfimages -all webr.pdf ./images/


# 3. Word 문서 ---------------------------

library(officer)

webr_docx <- officer::read_docx("data/webr.docx")
webr_content <- docx_summary(webr_docx) %>% 
  as_tibble()

ch01_start_idx <- which(webr_content$text == "제 1 장")
ch02_start_idx <- which(webr_content$text == "제 2 장")
ch03_start_idx <- which(webr_content$text == "제 3 장")

## 3.1. 1장 ------------------------------

ch01_raw <- webr_content %>% 
  mutate(text = str_remove(text, pattern = "\\d{15}")) %>% 
  filter(between(row_number(), ch01_start_idx, ch02_start_idx - 1)) %>% 
  filter(!text == "") %>% 
  select(style_name, text)

ch01_md <- ch01_raw %>% 
  filter(!str_detect(text, "제\\s?[0-9]\\s?장")) %>% 
  mutate(markdown = case_when(style_name == "heading 1"             ~ glue::glue("# {text} \n\n"),
                              str_detect(text, "제\\s?[0-9]\\s?절") ~ glue::glue("## {text} \n\n"),
                              style_name == "heading 3"             ~ glue::glue("### {text} \n\n"),
                              TRUE                                  ~ glue::glue("{text} \n\n"))) %>% 
  pull(markdown)

print(ch01_md)

## 3.2. 2장 ------------------------------

ch02_raw <- webr_content %>% 
  mutate(text = str_remove(text, pattern = "\\d{15}")) %>% 
  filter(between(row_number(), ch02_start_idx, ch03_start_idx - 1)) %>% 
  filter(!text == "") %>% 
  select(style_name, text)

ch02_md <- ch02_raw %>% 
  filter(!str_detect(text, "제\\s?[0-9]\\s?장")) %>% 
  slice(3:n()) %>%   
  mutate(markdown = case_when(style_name == "heading 1"             ~ glue::glue("# {text} \n\n"),
                              str_detect(text, "제\\s?[0-9]+\\s?절") ~ glue::glue("## {text} \n\n"),
                              style_name == "heading 3"             ~ glue::glue("### {text} \n\n"),
                              TRUE                                  ~ glue::glue("{text} \n\n"))) %>% 
  pull(markdown)

print(ch02_md)

## 3.3. 3장 ------------------------------

ch03_raw <- webr_content %>% 
  mutate(text = str_remove(text, pattern = "\\d{15}")) %>% 
  filter(between(row_number(), ch03_start_idx, n())) %>% 
  filter(!text == "") %>% 
  select(style_name, text)

ch03_md <- ch03_raw %>% 
  filter(!str_detect(text, "제\\s?[0-9]\\s?장")) %>% 
  slice(3:n()) %>% 
  mutate(markdown = case_when(style_name == "heading 1"             ~ glue::glue("# {text} \n\n"),
                              str_detect(text, "제\\s?[0-9]+\\s?절") ~ glue::glue("## {text} \n\n"),
                              style_name == "heading 3"             ~ glue::glue("### {text} \n\n"),
                              TRUE                                  ~ glue::glue("{text} \n\n"))) %>% 
  pull(markdown)

print(ch03_md)

