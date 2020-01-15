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
"%.%" <- function(x, y) {paste0(x, ". ", y)}
"% %" <- function(x, y) {paste0(x, " ", y)}

"%p%" <- function(x, y) {paste0(x, '(', y, ')')}
"%q%" <- function(x, y) {paste0(x, '"', y, '"')}
"%[%" <- function(x, y) {paste0(x, "[", y, "]")}

brack <- function(x) {"" %[% x}
paren <- function(x) {"" %p% x}
quote <- function(x) {"" %q% x}

br <- function(x, n=1) {x %0% str_c(rep("\n", n), collapse="")}

# Test these functions
if (verbose) {
  x <- "abc"
  y <- "def"
  br("") %>% cat
  '"abc" %0% "def" produces: ' %1% x %0% y %>% br %>% cat
  '"abc" %1% "def" produces: ' %1% x %1% y %>% br %>% cat
  '"abc" %2% "def" produces: ' %1% x %2% y %>% br(2) %>% cat

  '"abc" %b% "def" produces: ' %1% x %b% y %>% br %>% cat
  '"abc" %c% "def" produces: ' %1% x %c% y %>% br %>% cat
  '"abc" %s% "def" produces: ' %1% x %s% y %>% br %>% cat
  '"abc" %.% "def" produces: ' %1% x %.% y %>% br %>% cat
  
  '"abc" %p% "def" produces: ' %1% x %p% y %>% br %>% cat
  '"abc" %q% "def" produces: ' %1% x %q% y %>% br %>% cat
  '"abc" %[% "def" produces: ' %1% x %[% y %>% br %>% cat
  
  'br("abc", 1) produces: ' %1% br("abc", 1) %>% br %>% cat
  'br("abc", 2) produces: ' %1% br("abc", 2) %>% br %>% cat

  'brack("abc") produces: ' %1% brack(x) %>% br %>% cat
  'paren("abc") produces: ' %1% paren(x) %>% br %>% cat
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

check_dates <- function(f0, f1) {
  t0 <- file.info(f0)$mtime
  t1 <- file.info(f1)$mtime
  skip_flag <- !is.na(t1) & (t1-t0 > 0) & !update_all
  if (skip_flag) {
    if (verbose) {"\n    Skipping  " %b% f1 %>% cat}
  }
  return(skip_flag)
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

# This function builds a link to a category,
# tag, or date page.

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
  tst <- c(
    "author: \"Steve Simon\"", 
    "date: \"2015-01-15\"", 
    "category: Statistics",
    "tags: \"Human side of statistics, Observational studies\"")
  tst %>% extract_yaml_field("tags: ") %>% build_link %>% cat
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

# This function reads a bibtex file and creates
# an organized list of the information from 
# this file.

parse_bibtex <- function(tx, f0) {
  # Note: Some of the bib files have unprintable
  # junk characters at the very start.
  tx %>%
    str_subset("^\\}", negate=TRUE) %>%
    str_remove("[=\\{].*") %>%
    str_remove("mendeley-") %>%
    remove_punctuation %>% 
    str_replace("misc", "name") %>%
    str_replace("article", "name") -> field_names
  tx %>%
    str_subset("^\\}", negate=TRUE) %>%
    str_remove(".*?[=\\{]") %>%
    remove_punctuation %>%
    as.list  %>%
    set_names(field_names) %>%
    return
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
    str_replace("misc", "name") %>%
    str_replace("article", "name") -> field_names
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
# If you have the time and energy, check out
# these cases as well.
# if (verbose) zzzpad(-3)
# if (verbose) zzzpad(0.5)
# if (verbose) zzzpad(9998:10003)

cat("\n\nCompleted testing\n\n")