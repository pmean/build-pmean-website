# step1-translate-bib-to-md.R
# written by Steve Simon
# created 2019-06-28

# This program finds all the bibtex files
# (extension .bib) in r1 and converts them
# to markdown format (extension .md) and
# stores them in md/r3.

# Step 1-1. Preliminaries

source(file="src/prelims.R")
if (!exists("verbose")) verbose <- TRUE
if (!exists("update_all")) update_all <- TRUE

# Step 1-2. Find the .md files

post_root <- "../source/posts"
yr_list <- list.dirs(bib_root, recursive=FALSE)

remove_punctuation <- function(x) {
  x %>%
    str_trim                   %>%
    str_remove_all(fixed("{")) %>%
    str_remove_all(fixed("}")) %>%
    str_remove    (",$"      ) %>%
    str_remove    ("^@"      ) %>%
    str_trim                   %>%
    return
}

parse_yaml <- function(tx, f0) {
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
    set_names(field_names) -> field_values
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

modify_fields <- function(f) {
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

  return(f)
}

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

write_tail <- function(f) {
  tail_tx <- 
    "This" %b% build_link(f$category)           %1%
    "was added to the website on"               %1%
    build_link(f$month)               %0% f$day %1%
    "and was last modified on"                  %1%
    f$modified                        %0% "."   %1%
    "You can find similar pages at"             %1%
    build_link(f$ta) %0% ".\n\n"
  
  if (verbose) {"\n\n" %0% tail_tx %>% cat}
  writeLines(tail_tx, f$full_tail_name)
  return(f)
}

write_links <- function(f) {
  f$month                               %1%
    f$urldate                           %1%
    f$category                          %1%
    str_replace_all(f$tags, ", ", "\n") %1%
    "\n"                                -> link_tx
  if (verbose) {print(link_tx)}
  writeLines(f$full_link_name)
  return(f)
}


write_everything <- function(f0) {
  f0 %>% str_replace("bib$", "md") -> f1
  if (check_dates(f0, f1)) return("Skipping" %b% f1)
  if (verbose)   {"\n    Working on" %b% f1 %>% cat}
  
  readLines(f0) %>%
    parse_bibtex(f0) %>%
    modify_fields %>%
    write_body %>%
    write_tail %>%
    write_links %>%
    return
}



for (i_yr in yr_list) {
  if (verbose) {"\nYear =" %b% i_yr %>% cat}
  mo_list <- list.dirs(path=i_yr, recursive=FALSE)
  for (i_mo in mo_list) {
    if (verbose) {"\n  Month =" %b% i_mo %>% cat}
    md_list <- list.files(i_mo, pattern="*.bib")
    for (i_md in md_list) {
      f <- write_everything(i_mo %s% i_md)
    }
  }
}

# Save everything.

save.image("data/step2.RData")
