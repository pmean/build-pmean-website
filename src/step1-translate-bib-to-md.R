#  step1-translate-bib-to-md.R
# written by Steve Simon
# created 2019-06-28

# This program finds all the bibtex files
# (extension .bib) in r1 and converts them
# to markdown format (extension .md) and
# stores them in md/r3.

# Step 1-1. Preliminaries

source(file="prelims.R")
verbose <- TRUE

# Step 1-2. Find the .bib files

r1_root <- "c:/Users/steve/Dropbox/professional/web/r1"

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
bib_names <- NULL
r1_names <- NULL
short_names <- NULL
for (i_month in month_list) {
  r1_path <- r1_root %s% i_month
  f0 <- list.files(path=r1_path, pattern="*.nbib")
  f1 <- list.files(path=r1_path, pattern="*.bib")
  f2 <- setdiff(f1, f0)
  f3 <- r1_path %s% f2
  f4 <- i_month %s% f2
  r1_names %<>% append(f2)
  bib_names %<>% append(f3)
  short_names %<>% append(f4)
  if (verbose) {
     "\n\nMonth =" %b% i_month %>% cat
     "\n" %0% paste(f2, collapse=", ") %>% cat
  }
}
short_names %<>% str_replace(fixed(".bib"), "")
if (verbose) print(bib_names)
n_names <- length(r1_names)
r3_path <- "c:/Users/steve/Dropbox/professional/web/md/r3"
```

## Step 1-3. Read each .bib file and convert to .md file

First, set up the general structure.

```{r structure}
yaml_divider <- '---'
marks <- c(
  "title = ",
  "urldate = ",
  "mendeley-tags = ",
  "annote = ",
  "url = ",
  "pdf = ",
  "@"
)
default_marks <- c(
  "No title",
  "1999-09-09",
  "Untagged",
  "No commentary",
  "No html link",
  "No PDF link",
  "No short name"
)
n_marks <- length(marks)
```

Create bib_info, a matrix of information about the individual .bib files.

```{r create-info}
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
```

Populate bib_info

```{r bib-functions}

read_bib <- function(fn, def="No data") {readLines(fn)}

# Some of the bib files have unprintable junk characters at the very start.
select_txt <- function(txt, lab, def) {
  txt %>% 
    str_replace("^.*@", "@")          %>%          # unprintable junk characters
    str_replace_all(fixed("{{"), "{") %>%          # double bracket
    str_replace_all(fixed(" {"), "{") %>%          # leading blank
    str_replace_all(fixed("},"), "}") %>%          # trailing comma
    str_replace_all(fixed(" ="), "=") %>%          # leading blank
    str_replace_all(fixed("= "), "=") %>%          # trailing blank
    str_subset(lab)                   -> lin 
  if (length(lin) == 0) {
    if (verbose) {cat(",", def)}
    return(def)
  }
  lin %>%
    str_remove_all('^@.*\\{'   ) %>%          # from @ to {
    str_remove_all(fixed('"'))   %>%          # quote marks
    str_remove_all("^.*?=")      %>%          # label
    str_remove_all(",$")         %>%          # trailing comma
    str_remove_all(fixed('{'))   %>%          # left curly bracket
    str_remove_all(fixed('}'))   %>%          # right curly bracket
    str_trim                     -> selection
  if (is.null(selection)) return(def)
  return(paste0(selection, collapse="; "))
}

```


```{r read-bib}
txt_original <- as.list(1:n_names)
for (i in 1:n_names) {
  if (verbose) {cat("\n", short_names[i], sep="")}
  tx <- readLines(bib_names[i])
  txt_original[[i]]  <- tx

  bib_info[i, "fil"] <- short_names[i]
  bib_info[i, "ttl"] <- select_txt(tx, "title="        , "No title")
  bib_info[i, "dat"] <- select_txt(tx, "urldate="      , "No date")
  bib_info[i, "tag"] <- select_txt(tx, "mendeley-tags=", "No tags")
  bib_info[i, "not"] <- select_txt(tx, "annote="       , "No notes")
  bib_info[i, "url"] <- select_txt(tx, "url="          , "No html")
  bib_info[i, "pdf"] <- select_txt(tx, "pdf="          , "No pdf")
  bib_info[i, "nam"] <- select_txt(tx, "@"             , "No name")
  
  bib_info[i, "png"] <- r1_root %s% short_names[i] %0% ".png"

}
```

```{r build-md}
for (i in 1:n_names) {
  full_title <- 'Recommendation:' %b% bib_info[i, "ttl"]
  yaml_divider                                               %1%
    'title: '                    %q% full_title              %1%
    'author: '                   %q% "Steve Simon"           %1%
    'date: '                     %q% bib_info[i, "dat"] %1%
    'category: '                 %0% 'Recommendation'        %1%
    'tags: '                     %q% bib_info[i, "tag"] %1%
    'output: '                   %0% 'html_document'         %1%
    yaml_divider                                             %2%

    bib_info[i, "not"]                                  %2%
    
    '<!---More--->'                                          %2%
    
    '![]'                        %p% bib_info[i, "png"] %2%
    
    'Available in [html format]' %p% bib_info[i, "url"] %0%
    ' or [PDF format]'           %p% bib_info[i, "pdf"] %0%
    '.\n'                                                    -> md_file
  
  md_file %>%
    sub(" or [PDF format](No pdf)",  "", ., fixed=TRUE)  %>%
    sub(" [html format](No html) or", "", ., fixed=TRUE) -> md_file
  new_name <- r3_path %s% bib_info[i, "nam"] %0% ".md"
  if (verbose) {
    "Writing" %b% new_name           %b% ".\n" %>% cat
    "Writing" %b% bib_info[i, "png"] %b% ".\n\n" %>% cat
  }
  writeLines(md_file, new_name)
}
```

## Step 1-5. List incomplete areas

```{r incomplete}
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