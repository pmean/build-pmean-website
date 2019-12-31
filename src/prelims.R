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
"%s%" <- function(x, y) {paste0(x, "/", y)}

"%p%" <- function(x, y) {paste0(x, '(', y, ')')}
"%q%" <- function(x, y) {paste0(x, '"', y, '"')}
"%[%" <- function(x, y) {paste0(x, "[", y, "]")}

brack <- function(x) {"" %[% x}
paren <- function(x) {"" %p% x}

extract_field <- function(x, field_name) {
  str_subset(x, fixed(field_name, ignore_case=TRUE)) %>%
    str_remove(field_name) %>%
    str_remove_all(fixed('"')) %>%
    str_trim %>%
    str_c(collapse=", ") -> extracted_text
  if (length(extracted_text) == 0) {return("Not found")}
  return(extracted_text)
}

if (verbose) {
  tst <- c(
    "author: \"Steve Simon\"", 
    "date: \"2015-01-15\"", 
    "category: Statistics",
    "tags: \"Human side of statistics, Observational studies\"")
  tst %>% extract_field("tags: ") %>% print
  tst %>% extract_field("date: ") %>% print
  tst %>% extract_field("category: ") %>% print
  tst %>% extract_field("improper: ") %>% print
}

build_link <- function(x, p="../archive") {
  p %s% x %>% 
    tolower %>% 
    str_replace_all(" ", "-") %0% ".html" -> added_dashes
  link1 <- brack(x) %0% paren(added_dashes)
}

if (verbose) {
  tst %>% extract_field("category: ") %>% build_link %>% print
}

build_footer <- function(tag_txt, ctg_txt, dat_txt) {
  "This" %b% build_link(ctg_txt) %b% "was added to this website on" -> ctg_link
  
  dat_txt %>% str_sub(1, 7) -> mo
  dat_txt %>% str_sub(8, 10) -> da
  
  mo %>% build_link %0% da %0% "." -> dat_link
  
  tag_txt %>%
    strsplit(", ") %>%
    unlist %>%
    build_link %>%
    paste(collapse=", ") -> tag_link
  
  "You can find similar pages at" %b% tag_link %0% "." -> tag_link
  
  return(ctg_link %1% dat_link %1% tag_link)
}

if (verbose) {
  build_footer(
    extract_field(tst, "tags: "),
    extract_field(tst, "category: "),
    extract_field(tst, "date: ")
  ) %>% print
}

compare_dates <- function(path0, path1, pattern0="md", pattern1="md") {
  wb_root <- "c:/Users/steve/Dropbox/professional/web"
  
  f0_root <- wb_root %s% path0
  f1_root <- wb_root %s% path1
  
  f0_names <- list.files(path=f0_root, pattern="*." %0% pattern0)
  f1_names <- list.files(path=f1_root, pattern="*." %0% pattern1)

  f0_names %<>% str_remove(pattern0)
  f1_names %<>% str_remove(pattern1)
  
  # First, add files that are found in f0 but not f1.  
  changed_list <- setdiff(f0_names, f1_names)
  
  common_files <- intersect(f0_names, f1_names)
  for (i_file in common_files) {
    t0 <- file.info(f0_root %s% i_file %0% "." %0% pattern0)$mtime
    t1 <- file.info(f1_root %s% i_file %0% "." %0% pattern1)$mtime
      if (t1-t0 > 0) next
      changed_list %<>% append(i_file)
      if (verbose) {
        "\n" %0% t1 %b% t0 %b% str_remove(i_file, "^.*/") %>% cat
      }
  }
  return(changed_list)
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
