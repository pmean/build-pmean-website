# step1-translate-bib-to-md.R
# written by Steve Simon
# created 2019-06-28

# This program finds all the bibtex files
# (extension .bib) in r1 and converts them
# to markdown format (extension .md) and
# stores them in md/r3.

# Step 0-1. Preliminaries

source(file="src/prelims.R")
if (!exists("verbose")) verbose <- TRUE
if (!exists("update_all")) update_all <- TRUE

# Step 0-2. Find the .bib files

r1_root <- "c:/Users/steve/Dropbox/professional/web/r1"

# Note: month "18-02" was causing problems
# and has been moved temporarily.

month_list <- c(
  "19/05", 
  "19/06", 
  "19/07", 
  "19/10", 
  "19/11", 
  "19/12",
  "20/01"
)
all_names <- NULL
for (i_month in month_list) {
  r1_path <- r1_root %s% i_month
  all_names[[i_month]] <- list.files(path=r1_path, pattern="*.bib")
}

n_months <- sapply(all_names, length)
month_path <- rep(names(all_names), n_months)
all_names %>% 
  unlist %>% 
  unname %>% 
  str_remove(fixed(".bib")) -> word_names

half_names <- month_path %s% word_names
full_names <- r1_root %s% half_names %0% ".bib"
if (verbose) print(half_names)
n_names <- length(word_names)


## Step 0-3. Read each .bib file and add source

for (i in 1:n_names) {
  tx <- readLines(full_names[i])
  source_info <- extract_bibtex_field(tx, "source="        , "No source")
  if (source_info != "No source") next
  if (verbose) {cat("\n", half_names[i], sep="")}
  extra_line <- "  source = {" %0% half_names[i] %0% "},"
  tx %>% str_which(fixed("}")) %>% max -> end_line
  new_tx <- c(
    tx[1:(end_line-3)],
    extra_line,
    tx[(end_line-2):end_line],
    ""
  )
  writeLines(new_tx, full_names[i])
}

# Step 0-4. Read each md file and add source

b3_path <- "c:/Users/steve/Dropbox/professional/web/md/b3"
b3_names <- list.files(b3_path, pattern="*.md")
for (i_file in b3_names) {
  tx <- readLines(b3_path %s% i_file)
  source_info <- extract_yaml_field(tx, "source:")
  if (source_info != "Not found") next
  if (verbose) {cat("\n", i_file, sep="")}
  i_file %>% str_remove(".md$") -> j_file
  extra_line <- 'source: "http://blog.pmean.com/' %0% j_file %0% '/"'
  end_line <- length(tx)
  new_tx <- c(
    tx[1:3],
    extra_line,
    tx[4:end_line],
    ""
  )
  writeLines(new_tx, b3_path %s% j_file %0% ".md")
}

# Save everything.

save.image("data/step0.RData")
