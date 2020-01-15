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

# general functions

# These functions join strings in various ways.

"%0%" <- function(x, y) {paste0(x        , y)}
"%1%" <- function(x, y) {paste0(x, "\n"  , y)}
"%2%" <- function(x, y) {paste0(x, "\n\n", y)}
"%b%" <- function(x, y) {paste0(x, ' '   , y)}
"%c%" <- function(x, y) {paste0(x, ','   , y)}
"%C%" <- function(x, y) {paste0(x, ', '  , y)}
"%d%" <- function(x, y) {paste0(x, "."   , y)}
"%D%" <- function(x, y) {paste0(x, ". "  , y)}
"%s%" <- function(x, y) {paste0(x, "/"   , y)}
"%.%" <- function(x, y) {paste0(x, ". "  , y)}
"% %" <- function(x, y) {paste0(x, " "   , y)}

"%p%" <- function(x, y) {paste0(x, '(', y, ')')}
"%q%" <- function(x, y) {paste0(x, '"', y, '"')}
"%[%" <- function(x, y) {paste0(x, "[", y, "]")}

brack <- function(x) {"" %[% x}
paren <- function(x) {"" %p% x}
quote <- function(x) {"" %q% x}

br <- function(x, n=1) {x %0% str_c(rep("\n", n), collapse="")}

# Test these functions
if (verbose) {
  cat("\n\n***Testing various string infix operators***\n")
  x <- "abc"
  y <- "def"
  br("") %>% cat
  '"abc" %0% "def" produces: ' %1% x %0% y      %>% br    %>% cat
  '"abc" %1% "def" produces: ' %1% x %1% y      %>% br    %>% cat
  '"abc" %2% "def" produces: ' %1% x %2% y      %>% br(2) %>% cat
  
  '"abc" %b% "def" produces: ' %1% x %b% y      %>% br    %>% cat
  '"abc" %c% "def" produces: ' %1% x %c% y      %>% br    %>% cat
  '"abc" %C% "def" produces: ' %1% x %C% y      %>% br    %>% cat
  '"abc" %s% "def" produces: ' %1% x %s% y      %>% br    %>% cat
  '"abc" %.% "def" produces: ' %1% x %.% y      %>% br(2) %>% cat
  
  '"abc" %p% "def" produces: ' %1% x %p% y      %>% br    %>% cat
  '"abc" %q% "def" produces: ' %1% x %q% y      %>% br    %>% cat
  '"abc" %[% "def" produces: ' %1% x %[% y      %>% br(2) %>% cat
  
  'br("abc", 1) produces: '    %1% br("abc", 1) %>% br    %>% cat
  'br("abc", 2) produces: '    %1% br("abc", 2) %>% br(2) %>% cat
  
  'brack("abc") produces: '    %1% brack(x)     %>% br    %>% cat
  'paren("abc") produces: '    %1% paren(x)     %>% br    %>% cat
}

# This function takes a topic, partial date,
# or category, and produces a link to the
# appropriate page.

build_link <- function(x, p="../archive") {
  x %>% str_split(", ") %>% unlist -> y
  p %s% y %>% 
    tolower %>% 
    str_replace_all(" ", "-") %0% ".html" -> added_dashes
  brack(y) %0% paren(added_dashes) %>%
    str_c(collapse=", \n") %>%
    return
}

# Test this function
if (verbose) {
  cat("\n\nTesting build_link***\n")
  tst <- "Human side of statistics, Observational studies"
  tst %>% build_link %>% cat
}

# This function checks to see if a new
# file (f1) exists or if it is dated
# later than the original file (f0).

check_dates <- function(f0, f1) {
  t0 <- file.info(f0)$mtime
  t1 <- file.info(f1)$mtime
  skip_flag <- !is.na(t1) & (t1-t0 > 0) & !update_all
  if (skip_flag) {
    if (verbose) {"\n    Skipping  " %b% f1 %>% cat}
  }
  return(skip_flag)
}

# This function takes an entry from a bibtex
# file or a yaml header and removes unneeded
# punctuation (commas, quote marks, etc.)

remove_punctuation <- function(x) {
  x %>%
    str_trim                   %>%
    str_remove_all(fixed('"')) %>%
    str_remove_all(fixed("{")) %>%
    str_remove_all(fixed("}")) %>%
    str_remove    (",$"      ) %>%
    str_remove    ("^@"      ) %>%
    str_trim                   %>%
    return
}

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
if (verbose) {
  cat("\n\n***Testing zzzpad***\n")
  zzzpad(c(8:12, 98:102, 998:1002)) %>% print
}
# If you have the time and energy, check out
# these cases as well.
# if (verbose) zzzpad(-3)
# if (verbose) zzzpad(0.5)
# if (verbose) zzzpad(9998:10003)

# bib specific functions

# This function reads a bibtex file and creates
# an organized list of the information from 
# this file.

parse_bibtex <- function(tx, f0) {
  # Note: Some of the bib files have unprintable
  # junk characters at the very start.
  tx[1] %<>% str_remove("^.*?@")
  tx %>%
    str_subset("^\\}", negate=TRUE) %>%
    str_remove("[=\\{].*") %>%
    tolower %>%
    remove_punctuation %>% 
    str_remove("mendeley-") %>%
    str_replace("misc", "name") %>%
    str_replace("inproceedings", "name") %>%
    str_replace("book", "name") %>%
    str_replace("article", "name") -> field_names
  tx %>%
    str_subset("^\\}", negate=TRUE) %>%
    str_remove(".*?[=\\{]") %>%
    remove_punctuation %>%
    as.list  %>%
    set_names(field_names) %>%
    return
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
  
  cat("\n\n***Testing parse_bibtex***\n")
  tst_bib %>% parse_bibtex("dummy-file-name.bib") %>% print
}

modify_bib_fields <- function(f) {
  f$note <- f$annote
  # Modify format if not found
  if (f$format=="Not found") {
    f$format %<>%
      str_detect(regex("pdf$", ignore_case=TRUE)) %>%
      ifelse("pdf", "html")
  }
  f$format <- f$format %b% "format"
  
  # Build citation
  f$citation <- f$author %.% f$title %.% "Available in " %>% str_wrap(50) %1%
    brack(f$format) %p% f$url %0% "."
  
  # Modify long author lists 
  n_authors <- str_count(f$author, fixed(" and ", ignore_case=TRUE)) + 1
  if (n_authors > 2) {
    f$author %<>% str_replace(regex(" and .*", ignore_case=TRUE), " et al")}
  
  # Modify title
  f$title <- "Recommendation:" %b% f$title
  
  # Build source
  f$full_bib_name %>% 
    str_remove(bib_root) %>% 
    str_remove(fixed(".bib")) -> f$source
  
  # Build image file
  f$image <- str_remove(f$source, "^.*/") %0% ".png"
  
  # Build full file names
  f$full_body_name <- str_replace(f$full_bib_name, "bib$", "md")
  f$full_tail_name <- str_replace(f$full_bib_name, "bib$", "tail")
  f$full_link_name <- str_replace(f$full_bib_name, "bib$", "link")
  f$full_summ_name <- str_replace(f$full_bib_name, "bib$", "summ")
  
  f$category <- "Recomendation"
  f$month    <- str_sub(f$urldate, 1, 7)
  f$day      <- str_sub(f$urldate, 8, 10)
  
  f$blogdate <- f$urldate
  
  return(f)
}

flag_unused_bib_fields <- function(field_values, f0) {
  key_fields <- c(
    "annote",
    "author",
    "date",
    "format",
    "name",
    "tags",
    "title",
    "url",
    "urldate"
  )
  field_names <- names(field_values)
  unused_fields <- setdiff(key_fields, field_names)
  if (verbose) {"\nUnused fields:" %b% str_c(unused_fields, collapse=", ")}
  for (i_field in unused_fields) {
    field_values[[i_field]] <- "Not found"
  }
  
  field_values$full_bib_name <- f0
  field_values$modified <- 
    max(
      str_sub(file.info(f0)$mtime, 1, 10), 
      field_values$urldate
    )
  return(field_values)
}

# yaml specific functions

# This function reads a yaml header from a file
# and creates an organized list of the information
# from this file.

parse_yaml <- function(tx, f0) {
  yaml_lines <- str_which(tx, "---")
  if (length(yaml_lines) < 2) return("Two yaml dividers not found.")
  tx[2:(yaml_lines[2]-1)] %>%
    str_remove("\\:.*") %>%
    str_remove("mendeley-") %>%
    remove_punctuation %>% 
    str_replace("^date$", "blogdate") -> field_names
  tx[2:(yaml_lines[2]-1)] %>%
    str_remove(".*?\\:") %>%
    remove_punctuation %>%
    as.list  %>%
    set_names(field_names) -> field_values
  note_range <- (yaml_lines[2]+1):(yaml_lines[3]-1)
  field_values$note <- str_c(tx[note_range])
  field_values %>%
    return
}

# Test this function
if (verbose) {
  tst <- c(
    "---",
    "author: \"Steve Simon\"", 
    "date: \"2015-01-15\"", 
    "category: Statistics",
    "tags: \"Human side of statistics, Observational studies\"",
    "---",
    "Short text description",
    "<---more--->",
    "Additional text")
  cat("\n\n***Testing parse_yaml***\n")
  tst %>% parse_yaml %>% print
}

# This function updates information in yaml
# fields.

modify_yaml_fields <- function(f) {
  # Build full file names
  f$full_tail_name <- str_replace(f$full_post_name, "md$|Rmd$", "tail")
  f$full_link_name <- str_replace(f$full_post_name, "md$|Rmd$", "link")
  f$full_summ_name <- str_replace(f$full_post_name, "md$|Rmd$", "summ")
  
  f$month    <- str_sub(f$date, 1, 7)
  f$day      <- str_sub(f$date, 8, 10)
  
  f$blogdate <- f$date
  
  return(f)
}

flag_unused_yaml_fields <- function(field_values, f0) {
  key_fields <- c(
    "author",
    "category",
    "date",
    "tags",
    "title"
  )
  field_names <- names(field_values)
  unused_fields <- setdiff(key_fields, field_names)
  if (verbose) {"\nUnused fields:" %b% str_c(unused_fields, collapse=", ") %>% cat}
  for (i_field in unused_fields) {
    field_values[[i_field]] <- "Not found"
  }
  
  field_values$full_post_name <- f0
  field_values$modified <- 
    max(
      str_sub(file.info(f0)$mtime, 1, 10), 
      field_values$date
    )
  
  return(field_values)
}

# Functions for writing files

# This function writes the body of a markdown
# file associated with a bibtex recommendation.

write_body <- function(f) {
  new_tx <-
    "---"                             %1%
    "title: "    %q% f$title          %1%
    "author: "   %q% f$author         %1%
    "date: "     %q% f$urldate        %1%
    "category: " %q% "Recommendation" %1%
    "tags: "     %q% f$tags           %1%
    "source: "   %q% f$source         %1%
    "name: "     %q% f$name           %1%
    "output: "   %0% "html_document"  %1%
    "---"                             %2%
    
    str_wrap(f$note, 50)              %2%
    
    "<---More--->"                    %2%
    
    f$citation                        %2%
    
    "![]"        %p% f$image          %1%
    "\n"
  if (verbose) {"\n\n" %0% new_tx %>% cat}
  writeLines(new_tx, f$full_body_name)
  return(f)  
}

# This function produces a text file to be
# put at the end of a markdown file with 
# information and links to various 
# locations (tags, categories, dates).

write_tail <- function(f) {
  tail_tx <- 
    "This" %b% build_link(f$category)           %1%
    "was added to the website on"               %1%
    build_link(f$month)               %0% f$day %1%
    "and was last modified on"                  %1%
    f$modified                        %0% "."   %1%
    "You can find similar pages at"             %1%
    build_link(f$ta) %0% ".\n\n"
  
  if (str_detect(f$source, "pmean")) {
    tail_tx                                     %1%
      "An earlier version of this page appears" %1%
      "[here]" %p% f$source %0% ".\n\n"         -> tail_tx
  }
  
  if (verbose) {"\n\n" %0% tail_tx %>% cat}
  writeLines(tail_tx, f$full_tail_name)
  return(f)
}

# This function writes the names of various
# links (tags, category, date).

write_links <- function(f) {
  f$blogdate                            %1%
    f$month                             %1%
    f$category                          %1%
    str_replace_all(f$tags, ", ", "\n") -> link_tx
  
  if (verbose) {"\nLinks" %1% link_tx %>% cat}
  writeLines(link_tx, f$full_link_name)
  return(f)
}

write_summ <- function(f) {
  f$note %>% 
    str_c(collapse="\n") %>%
    str_remove("^\n") %>%
    str_remove("^\n")  -> summ_tx

  brack(f$title) %0% 
    paren("../blog" %s% f$name %0% ".html") %b% 
    summ_tx %1% 
    "\n"                                         -> summ_tx
  if (verbose) {cat(summ_tx)}  
  writeLines(summ_tx, f$full_summ_name)
  return(f)
}

if (verbose) {cat("\n\n***Completed testing***\n\n")}