# step2-add-links-to-post.R
# written by Steve Simon
# created 2020-01-11

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

modify_fields <- function(f) {
  # Build full file names
  f$full_tail_name <- str_replace(f$full_post_name, "md$|Rmd$", "tail")
  f$full_link_name <- str_replace(f$full_post_name, "md$|Rmd$", "link")
  f$full_summ_name <- str_replace(f$full_post_name, "md$|Rmd$", "summ")
  
  f$month    <- str_sub(f$date, 1, 7)
  f$day      <- str_sub(f$date, 8, 10)

  return(f)
}

write_everything <- function(f0) {
  f0 %>% str_replace("md$|Rmd$", "tail") -> f1
  if (check_dates(f0, f1)) return("Skipping" %b% f1)
  if (verbose)   {"\n    Working on" %b% f1 %>% cat}
  
  readLines(f0) %>%
    parse_yaml(f0) %>% 
    modify_fields %>% 
    write_tail %>%
    write_links %>%
    print
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
