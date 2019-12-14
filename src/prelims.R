# prelims.R. This program was written by Steve Simon on 2019-06-11.

default_path <- "../data"

suppressMessages(suppressWarnings(library(base64enc)))
suppressMessages(suppressWarnings(library(curl)))
suppressMessages(suppressWarnings(library(knitr)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(rmarkdown)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(tidyverse)))
verbose <- TRUE
yaml_header <- "---"

"%0%" <- function(x, y) {paste0(x, y)}
"%1%" <- function(x, y) {paste0(x, "\n", y)}
"%2%" <- function(x, y) {paste0(x, "\n\n", y)}
"%b%" <- function(x, y) {paste0(x, ' ', y)}
"%p%" <- function(x, y) {paste0(x, '(', y, ')')}
"%q%" <- function(x, y) {paste0(x, '"', y, '"')}
"%s%" <- function(x, y) {paste0(x, "/", y)}

add_line_breaks <- function(s1, n=1) {
  v <- c(s1, rep("\n", n))
  paste0(v, collapse="")
}

add_quotes <- function(s1, s2) {
  paste0(s1, '"', s2, '"')
}

add_parentheses <- function(s1, s2) {
  paste0(s1, '(', s2, ')')
}

read_text <- function(fn, path=default_path, char_max=999999) {
  path %>%
    paste0("/") %>%
    paste0(fn) %>% 
    paste0(".txt") %>%
    readLines  -> text_lines
  text_lines %>% 
    paste0(collapse="\n") %>%
    return
}

remove_ch <- function(txt, ch, fixed_flag=FALSE) {
  gsub(ch, "", txt, fixed=fixed_flag)
}

select_name <- function(txt) {
  txt %>%
    grep('\\@', ., value=TRUE) %>%
    remove_ch('@') %>%
    remove_ch(',') %>%
    strsplit("\\{") %>%
    unlist %>%
    return
}

select_txt <- function(txt, lab) {
  txt %>%
    grep(lab, ., value=TRUE) %>%
    gsub("{{", "{", ., fixed=TRUE) %>%
    remove_ch('article{', fixed=TRUE) %>%
    remove_ch('article {', fixed=TRUE) %>%
    remove_ch('misc{', fixed=TRUE) %>%
    remove_ch(',.*?$', fixed=TRUE) %>%
    remove_ch('"', fixed=TRUE) %>%
    remove_ch('^\\s*') %>%
    remove_ch(paste0("^",lab)) %>%
    remove_ch('^\\{') %>%
    remove_ch('\\}') %>%
    remove_ch('\\s*$') %>%
    remove_ch(',$') -> selection
  if (is.null(selection)) return("")
  return(paste0(selection, collapse="; "))
}
