# step0-rename-bib-files.R
# written by Steve Simon
# created 2020-01-15

# This program finds all the bibtex files
# (extension .bib) in a directory structure
# and converts the names.

source(file="src/prelims.R")
if (!exists("verbose")) verbose <- TRUE
if (!exists("update_all")) update_all <- TRUE

file_list <- build_file_list("../source/bib", "*.bib")
for (i_file in file_list) {
  tx <- readLines(i_file)
  fields <- parse_bibtex(tx, i_file)
  if(verbose) i_file %C% fields$name %>% br %>% cat
  if(is.null(fields[[field_header]])) print(fields)
}

# Save everything.

save.image("data/step0.RData")
