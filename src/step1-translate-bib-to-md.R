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

# Step 1-2. Find the .bib files

bib_root <- "../source/bib"
yr_list <- list.dirs(bib_root, recursive=FALSE)

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

save.image("data/step1.RData")
