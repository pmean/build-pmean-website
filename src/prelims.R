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

# These functions join strings in various ways.

"%0%" <- function(x, y) {paste0(x, y)}
"%1%" <- function(x, y) {paste0(x, "\n", y)}
"%2%" <- function(x, y) {paste0(x, "\n\n", y)}
"%b%" <- function(x, y) {paste0(x, ' ', y)}
"%c%" <- function(x, y) {paste0(x, ',', y)}
"%s%" <- function(x, y) {paste0(x, "/", y)}

"%p%" <- function(x, y) {paste0(x, '(', y, ')')}
"%q%" <- function(x, y) {paste0(x, '"', y, '"')}
"%[%" <- function(x, y) {paste0(x, "[", y, "]")}

brack <- function(x) {"" %[% x}
paren <- function(x) {"" %p% x}
quote <- function(x) {"" %q% x}

# Test these functions
if (verbose) {
  x <- "abc"
  y <- "def"
  '\n"abc" %0% "def" produces: ' %1% x %0% y %>% cat
  '\n"abc" %1% "def" produces: ' %1% x %1% y %>% cat
  '\n"abc" %2% "def" produces: ' %1% x %2% y %>% cat

  '\n"abc" %b% "def" produces: ' %1% x %b% y %>% cat
  '\n"abc" %c% "def" produces: ' %1% x %c% y %>% cat
  '\n"abc" %s% "def" produces: ' %1% x %s% y %>% cat

  '\n"abc" %p% "def" produces: ' %1% x %p% y %>% cat
  '\n"abc" %q% "def" produces: ' %1% x %q% y %>% cat
  '\n"abc" %[% "def" produces: ' %1% x %[% y %>% cat
  
  '\n\nbrack("abc") produces: ' %1% brack(x) %>% cat
  '\nparen("abc") produces: ' %1% paren(x) %>% cat
}

# This function takes a topic, partial date,
# or category, and produces a link to the
# appropriate page.

extract_bibtex_field <- function(txt, lab, def="Not found") {
  txt %>% 
    str_replace("^.*@", "@")                  %>%          # see note above
    str_replace_all(fixed("{{"), "{")         %>%          # double bracket
    str_replace_all(fixed(" {"), "{")         %>%          # leading blank
    str_replace_all(fixed("},"), "}")         %>%          # trailing comma
    str_replace_all(fixed(" ="), "=")         %>%          # leading blank
    str_replace_all(fixed("= "), "=")         %>%          # trailing blank
    str_trim                                  %>%          # whitespace
    str_subset(fixed(lab, ignore_case=TRUE))  -> lin 
  if (length(lin) == 0) {return(def)}
  lin %>%
    str_remove_all('^@.*\\{'   )              %>%          # from @ to {
    str_remove_all(fixed('"'))                %>%          # quote marks
    str_remove_all("^.*?=")                   %>%          # label
    str_remove_all(",$")                      %>%          # trailing comma
    str_remove_all(fixed('{'))                %>%          # left curly bracket
    str_remove_all(fixed('}'))                %>%          # right curly bracket
    str_trim                                  -> selection
  return(paste0(selection, collapse="; "))
}

# Test this function

if (verbose) {
  '@Article{falsify-research,'                         %1%
    'annote = {An article about misconduct.},'           %1%
    'Author="Fanelli, Daniele",'                         %1%
    'mendeley-tags = {Ethics in research},'              %1%
    'url = {https://fakesite.com},'                      %1%
    'urldate = {2019-05-31},'                            %1%
    'Title="{How many scientists fabricate research?}",' %1%
    'Journal="PLoS ONE",'                                %1%
    'Year="2009",'                                       %1%
    'Volume="4",'                                        %1%
    'Number="5",'                                        %1%
    'Pages="e5738",'                                     %1%
    'Month="May"'                                        %1%
    '}'                                                  %>%
    str_split("\n")                                      %>%
    unlist                                               -> tst_bib
  
  tst_bib %>% extract_bibtex_field("@")        %0% "\n\n" %>% cat  
  tst_bib %>% extract_bibtex_field("urldate")  %0% "\n\n" %>% cat  
  tst_bib %>% extract_bibtex_field("improper") %0% "\n\n" %>% cat
}

# This function extracts information from
# a yaml header for a markdown file.

extract_yaml_field <- function(x, field_name) {
  str_subset(x, fixed(field_name, ignore_case=TRUE)) %>%
    str_trim %>%
    str_remove(",$") %>%
    str_remove(field_name) %>%
    str_remove_all(fixed('"')) %>%
    str_trim %>%
    str_c(collapse=", ") -> extracted_text
  if (length(extracted_text) == 0) {return("Not found")}
  return(extracted_text)
}

# Test this function
if (verbose) {
  tst <- c(
    "author: \"Steve Simon\"", 
    "date: \"2015-01-15\"", 
    "category: Statistics",
    "tags: \"Human side of statistics, Observational studies\"")
  tst %>% extract_yaml_field("tags: ") %>% print
  tst %>% extract_yaml_field("date: ") %>% print
  tst %>% extract_yaml_field("category: ") %>% print
  tst %>% extract_yaml_field("improper: ") %>% print
}



build_link <- function(x, p="../archive") {
  p %s% x %>% 
    tolower %>% 
    str_replace_all(" ", "-") %0% ".html" -> added_dashes
  link1 <- brack(x) %0% paren(added_dashes)
}

# Test this function
if (verbose) {
  tst <- c(
    "author: \"Steve Simon\"", 
    "date: \"2015-01-15\"", 
    "category: Statistics",
    "tags: \"Human side of statistics, Observational studies\"")
  tst %>% extract_yaml_field("category: ") %>% build_link %>% print
}

# This function builds a footer file with
# links to the topic, category, and month.

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
  tst <- c(
    "author: \"Steve Simon\"", 
    "date: \"2015-01-15\"", 
    "category: Statistics",
    "tags: \"Human side of statistics, Observational studies\"")
  build_footer(
    extract_yaml_field(tst, "tags: "),
    extract_yaml_field(tst, "category: "),
    extract_yaml_field(tst, "date: ")
  ) %>% print
}

# This function compares dates of files
# in two different directories.

compare_dates <- function(path0, path1, pattern0="md", pattern1="md") {
  f0_names <- list.files(path=path0, pattern="*." %0% pattern0)
  f1_names <- list.files(path=path1, pattern="*." %0% pattern1)

  f0_names %<>% str_remove(fixed("." %0% pattern0))
  f1_names %<>% str_remove(fixed("." %0% pattern1))
  
  if (verbose) {
    "\n" %0% length(f0_names) %b% "files found in" %b% path0 %>% cat
    "\n" %0% length(f1_names) %b% "files found in" %b% path1 %>% cat
    cat(" ")
    cat(paste(f1_names, collapse=", "))
    cat("\n")
  }
  
  # First, add files that are found in f0 but not f1.  
  changed_list <- setdiff(f0_names, f1_names)
  if (verbose & length(changed_list > 0)) {
    str_c(changed_list, " not found in ", path1, collapse="\n") %>% cat
  }
  
  # Compare dates for those files found in both f0 and f1
  
  common_files <- intersect(f0_names, f1_names)
  for (i_file in common_files) {
    t0 <- file.info(path0 %s% i_file %0% "." %0% pattern0)$mtime
    t1 <- file.info(path1 %s% i_file %0% "." %0% pattern1)$mtime
    if (t1-t0 > 0) next
    changed_list %<>% append(i_file)
    if (verbose) {
      "\n" %0% t1 %b% t0 %b% str_remove(i_file, "^.*/") %>% cat
    }
  }
  return(changed_list)
}

# This function identifies a  single line 
# in a bibtex file and strips off unneeded
# punctation.

# Note: Some of the bib files have unprintable
# junk characters at the very start.

# This function adds a leading zeros to digits less than 1,000

zzzpad <- function(x) {
  message_tail <- " in zzzpad may produce nonsensical results"
  if(any(x<0))           message("Note: Negative values", message_tail)
  if(any(x != trunc(x))) message("Note: Fractional values", message_tail)
  if(any(x>9999))        message("Note: Values > 9999", message_tail)
  case_when(
    x <   10 ~ paste0("000", x),
    x <  100 ~ paste0( "00", x),
    x < 1000 ~ paste0(  "0", x),
    TRUE     ~ as.character(x)
  ) %>% return
}

# Test this function
if (verbose) zzzpad(c(8:12, 98:102, 998:1002))
if (verbose) zzzpad(-3)
if (verbose) zzzpad(0.5)
if (verbose) zzzpad(9998:10003)

