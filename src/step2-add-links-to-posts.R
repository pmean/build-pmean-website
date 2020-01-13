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
yr_list <- list.dirs(post_root, recursive=FALSE)

remove_punctuation <- function(x) {
  x %>%
    str_trim                   %>%
    str_remove_all(fixed('"')) %>%
    str_trim                   %>%
    return
}

parse_yaml <- function(tx, f0) {
  yaml_lines <- str_which(tx, "---")
  print(yaml_lines)
  if (length(yaml_lines) < 2) return("Two yaml dividers not found.")
  tx[2:(yaml_lines[2]-1)] %>%
    str_remove("\\:.*") %>%
    str_remove("mendeley-") %>%
    remove_punctuation %>% 
    str_replace("misc", "name") %>%
    str_replace("article", "name") -> field_names
  print(field_names)
  tx[2:(yaml_lines[2]-1)] %>%
    str_remove(".*?\\:") %>%
    remove_punctuation %>%
    as.list  %>%
    set_names(field_names) -> field_values
  key_fields <- c(
    "author",
    "category",
    "date",
    "format",
    "name",
    "tags",
    "title"
  )
  
  
  unused_fields <- setdiff(key_fields, field_names)
  if (verbose) {"\nUnused fields:" %b% str_c(unused_fields, collapse=", ")}
  for (i_field in unused_fields) {
    field_values[[i_field]] <- "Not found"
  }

  field_values$full_post_name <- f0
  field_values$modified <- 
    max(
      str_sub(file.info(f0)$mtime, 1, 10), 
      field_values$urldate
    )
  
  return(field_values)
}

modify_fields <- function(f) {
  # Build source
  f$full_post_name %>% 
    str_remove(bib_root) %>% 
    str_remove(fixed(".md")) -> f$source
  
  # Build image file
  f$image <- str_remove(f$source, "^.*/") %0% ".png"
  
  # Build full file names
  f$full_body_name <- str_replace(f$full_post_name, "md$", "md")
  f$full_tail_name <- str_replace(f$full_post_name, "md$", "tail")
  f$full_link_name <- str_replace(f$full_post_name, "md$", "link")
  f$full_summ_name <- str_replace(f$full_post_name, "md$", "summ")
  
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
  f0 %>% str_replace("md$", "tail") -> f1
  if (check_dates(f0, f1)) return("Skipping" %b% f1)
  if (verbose)   {"\n    Working on" %b% f1 %>% cat}
  
  readLines(f0) %>%
    parse_yaml(f0) %>% print
    # modify_fields %>%
    # write_body %>%
    # write_tail %>%
    # write_links %>%
    # return
}



for (i_yr in yr_list) {
  if (verbose) {"\nYear =" %b% i_yr %>% cat}
  mo_list <- list.dirs(path=i_yr, recursive=FALSE)
  for (i_mo in mo_list) {
    if (verbose) {"\n  Month =" %b% i_mo %>% cat}
    md_list <- list.files(i_mo, pattern="*.md")
    for (i_md in md_list) {
      f <- write_everything(i_mo %s% i_md)
    }
  }
}

# Save everything.

save.image("data/step2.RData")
