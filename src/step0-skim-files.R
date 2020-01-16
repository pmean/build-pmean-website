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

skim_bib_files("name")
skim_bib_files("name", "../source/r1")
skim_yaml_files("category")

# Save everything.

save.image("data/step0.RData")
