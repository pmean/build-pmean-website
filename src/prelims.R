# prelims.R. This program was written by Steve Simon on 2019-06-11.

default_path <- "../data"

suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(magrittr)))
verbose <- TRUE

add_line_breaks <- function(s1, n=1) {
  v <- c(s1, rep("\n", n))
  paste0(v, collapse="")
}

add_quoted_string <- function(s1, s2) {
  paste0(s1, '"', s2, '"')
}

build_citation <- function(txt_full) {
  txt_authors <- select_txt(txt_full, "author")
  txt_title <- select_txt(txt_full, "title")
  txt_authors %>%
    paste0(". ") %>%
    paste0(txt_title) %>%
    paste0(".") -> citation
  if (verbose) cat(citation)
  return(citation)
}

build_document_link <- function(txt_full) {
  txt_html <- select_txt(txt_full, "html")
  txt_pdf <- select_txt(txt_full, "pdf")
  html_flag <- !is.null(txt_html)
  pdf_flag  <- !is.null(txt_pdf)
  both_links <- html_flag & pdf_flag
  prefix_all <- "Available in "
  prefix_html <- ifelse(html_flag, "[html format](", "")
  postfix_html <- ifelse(html_flag, ")", "")
  conjunction <- ifelse(both_links, " and ", "")
  prefix_pdf <- ifelse(pdf_flag, "[pdf format](", "")
  postfix_pdf <- ifelse(pdf_flag, ").", ".")
  prefix_all %>%
    paste0(prefix_html) %>%
    paste0(txt_html) %>%
    paste0(postfix_html) %>%
    paste0(conjunction) %>%
    paste0(prefix_pdf) %>%
    paste0(txt_pdf) %>%
    paste0(postfix_pdf) -> txt_link
  if (verbose) {cat(txt_link)}
  return(txt_link)
}

build_image_link <- function(bib_files) {
  bib_files[1] %>%
    sub(".bib", ".png", .) -> png_names
  "![](" %>%
    paste0(bib_path) %>%
    paste0("/") %>%
    paste0(png_names) %>%
    add_line_breaks(2) -> image_link
  if (verbose) {cat(image_link)}
  return(image_link)
}

build_yaml_header <- function(txt_full) {
  my_name <- 'Steve Simon'
  txt_title <- select_txt(txt_full, "title")
  yaml_divider <- '---'
  yaml_divider %>%
    add_line_breaks %>%
    paste0('title: ') %>%
    add_quoted_string(txt_title) %>%
    add_line_breaks %>%
    paste0('author: ') %>%
    add_quoted_string(my_name) %>%
    add_line_breaks %>%
    paste0('output: html_document') %>%
    paste0('\n') %>%
    paste0(yaml_divider) %>%
    add_line_breaks(2) -> yaml_header
  if (verbose) cat(yaml_header)  
  return(yaml_header)
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

select_txt <- function(txt, lab) {
  txt %>%
    grep(lab, ., value=TRUE) %>%
    remove_ch('"', fixed=TRUE) %>%
    remove_ch('^\\s*') %>%
    remove_ch(paste0("^",lab)) %>%
    remove_ch(' = \\{') %>%
    remove_ch('\\}') %>%
    remove_ch('\\s*$') %>%
    remove_ch(',$') %>%
    return
}
