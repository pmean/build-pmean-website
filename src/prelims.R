# prelims.R. This program was written by Steve Simon on 2019-06-11.

default_path <- "../data"

suppressMessages(suppressWarnings(library(knitr)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(rmarkdown)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(tidyverse)))
verbose <- TRUE

"%0%" <- function(x, y) {paste0(x, y)}
"%1%" <- function(x, y) {paste0(x, "\n", y)}
"%2%" <- function(x, y) {paste0(x, "\n\n", y)}
"%s%" <- function(x, y) {paste0(x, "/", y)}

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
    paste0(".") %>%
    add_line_breaks(2) -> citation
  if (verbose) cat(citation)
  return(citation)
}

build_description <- function(txt_full) {
  select_txt(txt_full, "annote") %>%
    add_line_breaks(2) -> txt_annote
  if (verbose) cat(txt_annote)
  return(txt_annote)
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
    paste0(postfix_pdf) %>%
    add_line_breaks(2) -> txt_link
  if (verbose) {cat(txt_link)}
  return(txt_link)
}

build_index_page <- function(bib_files) {
  index_page <- build_index_yaml()
  for (i in 1:bib_count) {
    short_name <- select_name(txt_full[[i]])[2]
    md_path %>%
      paste0("/") %>%
      paste0(short_name) %>%
      paste0(".Rmd") -> long_name
    if (verbose) {cat(short_name); cat("\n\n")}
    description <- build_description(txt_full[[i]])
    txt_date <- select_txt(txt_full[[i]], "urldate")
    txt_title <- select_txt(txt_full[[i]], "title")
    index_page %>%
      paste0(txt_date) %>%
      paste0(". ") %>%
      paste0("[") %>%
      paste0("Recommended: ") %>%
      paste0(txt_title) %>%
      paste0("](") %>%
      paste0(short_name) %>%
      paste0(".html). ") %>%
      paste0(description) %>%
      add_line_breaks(2) -> index_page
  }
  write(index_page, paste0(md_path, '/index.Rmd'))
}

build_index_yaml <- function() {
  yaml_divider <- '---'
  yaml_divider %>%
    add_line_breaks %>%
    paste0('title: P.Mean Website (created 1997-12-22, reborn at this location 2008-06-21)') %>%
    add_line_breaks %>%
    paste0('output: html_document') %>%
    paste0('\n') %>%
    paste0(yaml_divider) %>%
    add_line_breaks(2) -> yaml_header
  if (verbose) cat(yaml_header)  
  return(yaml_header)
  
}

build_individual_pages <- function(bib_files) {
  for (i in 1:bib_count) {
    short_name <- select_name(txt_full[[i]])[2]
    md_path %>%
      paste0("/") %>%
      paste0(short_name) %>%
      paste0(".Rmd") -> long_name
    if (verbose) {cat(short_name); cat("\n\n")}
    yaml_header <- build_yaml_header(txt_full[[i]])
    description <- build_description(txt_full[[i]])
    image_link <- build_image_link(bib_files[i])
    document_link <- build_document_link(txt_full[[i]])
    citation <- build_citation(txt_full[[i]])
    write(yaml_header, long_name, append=FALSE)
    write(description, long_name, append=TRUE)
    write(image_link, long_name, append=TRUE)
    write(document_link, long_name, append=TRUE)
    write(citation, long_name, append=TRUE)
  }
}

build_image_link <- function(bib_files) {
  bib_files[1] %>%
    sub(".bib", ".png", .) -> png_names
  "![](" %>%
    paste0(bib_path) %>%
    paste0("/") %>%
    paste0(png_names) %>%
    paste0(')') %>%
    add_line_breaks(2) -> image_link
  if (verbose) {cat(image_link)}
  return(image_link)
}

build_yaml_header <- function(txt_full) {
  my_name <- 'Steve Simon'
  txt_title <- paste0("Recommended: ", select_txt(txt_full, "title"))
  yaml_divider <- '---'
  yaml_divider %>%
    add_line_breaks %>%
    paste0('title: ') %>%
    add_quoted_string(txt_title) %>%
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
    remove_ch('"', fixed=TRUE) %>%
    remove_ch('^\\s*') %>%
    remove_ch(paste0("^",lab)) %>%
    remove_ch(' = \\{') %>%
    remove_ch('\\}') %>%
    remove_ch('\\s*$') %>%
    remove_ch(',$') -> selection
  if (is.null(selection)) return("")
  return(paste0(selection, collapse="; "))
}
