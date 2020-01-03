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

r1_root <- "c:/Users/steve/Dropbox/professional/web/r1"
r3_path <- "c:/Users/steve/Dropbox/professional/web/md/r3"

# Note: month "18-02" was causing problems
# and has been moved temporarily.

month_list <- c(
  "19/05", 
  "19/06", 
  "19/07", 
  "19/10", 
  "19/11", 
  "19/12"
)
all_names <- NULL
for (i_month in month_list) {
  r1_path <- r1_root %s% i_month
  all_names[[i_month]] <- list.files(path=r1_path)
}

n_months <- sapply(all_names, length)
month_path <- rep(names(all_names), n_months)
all_names %>% unlist %>% unname -> word_names
half_names <- month_path %s% word_names
full_names <- r1_path %s% half_names %0% ".bib"
if (verbose) print(half_names)
n_names <- length(word_names)


## Step 1-3. Read each .bib file and convert to .md file

# First, set up the general structure.

# Create bib_info, a matrix of information about the individual .bib files.

bib_cats <- c(
  "fil",
  "ttl",
  "dat",
  "tag",
  "not",
  "url",
  "pdf",
  "nam",
  "png"
)
n_cats <- length(bib_cats)
bib_info <- matrix("<<empty>>", nrow=n_names, ncol=n_cats)
dimnames(bib_info)[[2]] <- bib_cats

# Populate bib_info

txt_original <- as.list(1:n_names)
for (i in 1:n_names) {
  if (verbose) {cat("\n", half_names[i], sep="")}
  tx <- readLines(full_names[i])
  txt_original[[i]]  <- tx

  bib_info[i, "fil"] <- half_names[i]
  bib_info[i, "ttl"] <- select_txt(tx, "title="        , "No title")
  bib_info[i, "dat"] <- select_txt(tx, "urldate="      , "No date")
  bib_info[i, "tag"] <- select_txt(tx, "mendeley-tags=", "No tags")
  bib_info[i, "not"] <- select_txt(tx, "annote="       , "No notes")
  bib_info[i, "url"] <- select_txt(tx, "url="          , "No html")
  bib_info[i, "pdf"] <- select_txt(tx, "pdf="          , "No pdf")
  bib_info[i, "nam"] <- select_txt(tx, "@"             , "No name")
  
  bib_info[i, "png"] <- r1_root %s% half_names[i] %0% ".png"
}

# Build md files

for (i in 1:n_names) {
  full_title <- 'Recommendation:' %b% bib_info[i, "ttl"]
  yaml_divider                                               %1%
    'title: '                    %q% full_title              %1%
    'author: '                   %q% "Steve Simon"           %1%
    'date: '                     %q% bib_info[i, "dat"]      %1%
    'category: '                 %0% 'Recommendation'        %1%
    'tags: '                     %q% bib_info[i, "tag"]      %1%
    'source: '                   %q% half_names[i]           %1%
    'output: '                   %0% 'html_document'         %1%
    yaml_divider                                             %2%

    bib_info[i, "not"]                                       %2%
    
    '<!---More--->'                                          %2%
    
    '![]'                        %p% bib_info[i, "png"]      %2%
    
    'Available in [html format]' %p% bib_info[i, "url"]      %0%
    ' or [PDF format]'           %p% bib_info[i, "pdf"]      %0%
    '.\n'                                                    -> md_file
  
  md_file %>%
    str_remove(fixed(" or [PDF format](No pdf)"))           %>%
    str_remove(fixed(" [html format](No html) or"))         -> md_file
  new_name <- r3_path %s% bib_info[i, "nam"] %0% ".md"
  if (verbose) {
    "Writing" %b% new_name           %b% ".\n" %>% cat
    "Writing" %b% bib_info[i, "png"] %b% ".\n\n" %>% cat
  }
  writeLines(md_file, new_name)
}

## Step 1-5. List incomplete areas

bib_df <- data.frame(bib_info, stringsAsFactors=FALSE)
bib_df %>%
  filter(tag=="No tags") %>%
  mutate(png=substr(png, 47, 99)) %>%
  mutate(png=sub(".png", "", png)) %>%
  select(tag, png)
bib_df %>%
  filter(tag=="") %>%
  mutate(png=substr(png, 47, 99)) %>%
  mutate(png=sub(".png", "", png)) %>%
  select(tag, png)
```


Save everything.

```{r save-everything}
save.image("../data/create-r3.RData")
```