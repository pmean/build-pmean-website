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

# Step 0-2. Find the .bib files

r1_root <- "c:/Users/steve/Dropbox/professional/web/r1"

# Note: month "18-02" was causing problems
# and has been moved temporarily.


bib_root <- "../source/bib"
r1_root <- "../source/r1"
yr_list <- list.dirs(r1_root, recursive=FALSE)


field_names <- NULL
for (i_yr in yr_list) {
  if (verbose) {"\nYear =" %b% i_yr %>% cat}
  mo_list <- list.dirs(path=i_yr, recursive=FALSE)
  for (i_mo in mo_list) {
    if (verbose) {"\n  Month =" %b% i_mo %0% "\n\n" %>% cat}
    bib_list <- list.files(i_mo, pattern="*.bib")
    for (i_bib in bib_list) {
      tx <- readLines(i_mo %s% i_bib)
      fields <- parse_bibtex(tx, i_mo %s% i_bib)
      field_names %<>% append(names(fields))
      if(verbose) print(fields$name)
    }
  }
}

# Save everything.

save.image("data/step0.RData")
