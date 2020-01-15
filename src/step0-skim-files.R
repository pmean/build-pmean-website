# step0-rename-bib-files.R
# written by Steve Simon
# created 2020-01-15

# This program finds all the bibtex files
# (extension .bib) in r1 and converts the
# names.
# Step 0-1. Preliminaries

source(file="src/prelims.R")
if (!exists("verbose")) verbose <- TRUE
if (!exists("update_all")) update_all <- TRUE

skim_bib_files <- function(field_header, dir_root="../source/bib", file_pattern="*.bib") {
  yr_list <- list.dirs(dir_root, recursive=FALSE)
  for (i_yr in yr_list) {
    if (verbose) {"\nYear =" %b% i_yr %>% cat}
    mo_list <- list.dirs(path=i_yr, recursive=FALSE)
    for (i_mo in sort(mo_list)) {
      if (verbose) {"\n  Month =" %b% i_mo %>% br %>% cat}
      file_list <- list.files(i_mo, pattern=file_pattern)
      for (i_file in file_list) {
        tx <- readLines(i_mo %s% i_file)
        fields <- parse_bibtex(tx, i_mo %s% i_file)
        if(verbose) "    " %0% i_file %C% fields[[field_header]] %>% br %>% cat
        if(is.null(fields[[field_header]])) print(fields)
      }
    }
  }
}

skim_yaml_files <- function(field_header, dir_root="../source/posts", file_pattern="*.md") {
  yr_list <- list.dirs(dir_root, recursive=FALSE)
  for (i_yr in yr_list) {
    if (verbose) {"\nYear =" %b% i_yr %>% cat}
    mo_list <- list.dirs(path=i_yr, recursive=FALSE)
    for (i_mo in sort(mo_list)) {
      if (verbose) {"\n  Month =" %b% i_mo %>% br %>% cat}
      file_list <- list.files(i_mo, pattern=file_pattern)
      for (i_file in file_list) {
        tx <- readLines(i_mo %s% i_file)
        fields <- parse_yaml(tx, i_mo %s% i_file)
        if(verbose) "    " %0% i_file %C% fields[[field_header]] %>% br %>% cat
        if(is.null(fields[[field_header]])) print(fields)
      }
    }
  }
}

skim_bib_files("name")
skim_bib_files("name", "../source/r1")
skim_yaml_files("tags")

# Save everything.

save.image("data/step0.RData")
