#  convert-bib.R
## written by Steve Simon
## created 2019-12-16

source(file="src/prelims.R", echo=FALSE)
verbose <- TRUE

## Step 1. Find the .bib files

r1_names <- NULL
r1_dirs <- NULL
r1_root <- "c:/Users/steve/Dropbox/professional/web/r1"
month_list <- c("19/05", "19/06", "19/07", "19/10", "19/11", "19/12")
for (i_month in month_list) {
  r1_path <- r1_root %s% i_month
  f0 <- list.files(path=r1_path, pattern="*.nbib")
  f1 <- list.files(path=r1_path, pattern="*.bib")
  fn <- setdiff(f1, f0)
  r1_names %<>% append(fn)
  r1_dirs %<>% append(rep(i_month, length(fn))) 
}
short_names <- r1_dirs %s% sub(".bib", "", r1_names)
bib_names <- r1_root %s% r1_dirs %s% r1_names
if (verbose) print(short_names)
n_files <- length(r1_names)
r3_path <- "c:/Users/steve/Dropbox/professional/web/r3"

## Step 2. Read each .bib file and convert to .md file
### First, set up the general structure.

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

### Create bib_info, a matrix of information about the individual .bib files.

bib_cats <- c(
  "fil",
  "nam",
  "ttl",
  "dat",
  "tag",
  "url",
  "pdf",
  "png",
  "not",
  "brf"
)
n_cats <- length(bib_cats)
bib_info <- matrix("<<empty>>", nrow=n_files, ncol=n_cats)
dimnames(bib_info)[[2]] <- bib_cats

### Clean bib file functions

read_bib <- function(fn) {readLines(r1_path %s% r1_names[i_file])}

strip_blanks <- function(x) {
  # Some of the files also have some silly garbage characters
  # before the @ sign on the first line of text.
  x %>%
    gsub("\\s", " " , .                ) %>%          # multiple blanks
    gsub("^*.@", "@", .                ) %>%          # @ silliness
    gsub("{{"  , "{", .    , fixed=TRUE) %>%          # double bracket
    gsub(" {"  , "{", .    , fixed=TRUE) %>%          # leading blank
    gsub(" ="  , "=", .    , fixed=TRUE) %>%          # leading blank
    gsub("= "  , "=", .    , fixed=TRUE) %>%          # trailing blank
    return
}

clean_line <- function(x, lab) {
  x%>%
    remove_ch('^.*@'      ) %>%          # from beginning to @
    remove_ch('^.*\\{'    ) %>%          # from beginning to {
    remove_ch(',.*?$'     ) %>%          # from comma to end
    remove_ch('\\"'       ) %>%          # quote marks
    remove_ch('^\\s*'     ) %>%          # leading spaces
    remove_ch("^" %0% lab ) %>%          # label
    remove_ch('^\\{'      ) %>%          # left curly bracket
    remove_ch('\\}'       ) %>%          # right curly bracket
    remove_ch('\\s*$'     ) %>%          # trailing blanks
    remove_ch(',$'        ) %>%          # trailing comma
    return
}

select_txt <- function(txt, lab, def) {
  txt %>% 
    strip_blanks %>%
    grep(lab, ., value=TRUE, ignore.case=TRUE) -> lin 
  if (length(lin) == 0) {
    if (verbose) {cat(",", def)}
    return(def)
  }
  lin %>% clean_line(lab) -> selection
  if (is.null(selection)) return("")
  return(paste0(selection, collapse="; "))
}

### Populate bib_info

txt_original <- as.list(1:n_files)
for (i_file in 1:n_files) {
  if (verbose) {cat("\n", short_names[i_file], sep="")}
  tx <- readLines(bib_names[i_file])
  txt_original[[i_file]]  <- tx

  bib_info[i_file, "fil"] <- short_names[i_file]
  bib_info[i_file, "ttl"] <- select_txt(tx, "title="        , "No title")
  bib_info[i_file, "dat"] <- select_txt(tx, "urldate="      , "No date")
  bib_info[i_file, "tag"] <- select_txt(tx, "mendeley-tags=", "No tags")
  bib_info[i_file, "not"] <- select_txt(tx, "annote="       , "No notes")
  bib_info[i_file, "url"] <- select_txt(tx, "url="          , "No html")
  bib_info[i_file, "pdf"] <- select_txt(tx, "pdf="          , "No pdf")
  bib_info[i_file, "nam"] <- select_txt(tx, "@"             , "No name")
  
  bib_info[i_file, "png"] <- sub("bib$", "png", bib_names[i_file])

}

### Build brief page

brief_template <-
  '[%ttl% (created %dat%)](../blog/%nam%.html) %not%'

for (i_file in 1:n_files) {
  brief_template %>%
    sub("%ttl%", bib_info[i_file, "ttl"], .) %>%
    sub("%dat%", bib_info[i_file, "dat"], .) %>%
    sub("%nam%", bib_info[i_file, "nam"], .) %>%
    sub("%not%", bib_info[i_file, "not"], .) -> bib_info[i_file, "brf"]
}

### Build main page

for (i_file in 1:n_files) {
  yaml_divider                                               %1%
    'title: '                    %q% bib_info[i_file, "ttl"] %1%
    'author: '                   %q% "Steve Simon"           %1%
    'date: '                     %q% bib_info[i_file, "dat"] %1%
    'category: '                 %0% 'Recommended'           %1%
    'tags: '                     %q% bib_info[i_file, "tag"] %1%
    'output: '                   %0% 'html_document'         %1%
    'source: '                   %0% bib_info[i_file, "fil"] %1%
    yaml_divider                                             %2%

    bib_info[i_file, "not"]                                  %2%
    
    '<!---More--->'                                          %2%
    
    '![]'                        %p% bib_info[i_file, "png"] %2%
    
    'Available in [html format]' %p% bib_info[i_file, "url"] %0%
    ' or [PDF format]'           %p% bib_info[i_file, "pdf"] %0%
    '.\n'                                                    -> md_file
  
  md_file %>%
    sub(" or [PDF format](No pdf)",  "", ., fixed=TRUE)  %>%
    sub(" [html format](No html) or", "", ., fixed=TRUE) -> md_file
  new_name <- r3_path %s% bib_info[i_file, "nam"] %0% ".md"
  if (verbose) {
    "\nStep" %b%
    i_file %b%
    ": writing" %b% 
    bib_info[i_file, "fil"] %b%
    "to" %b%
    bib_info[i_file, "nam"] %0%
    "." %>% cat
  }
  writeLines(md_file, new_name)
}

## Step 3. Produce an index

yaml_divider                                               %1%
  'title: '                    %q% 'All blog entries'      %1%
  'author: '                   %q% "Steve Simon"           %1%
  'date: '                     %q% bib_info[i_file, "dat"] %1%
  'output: '                   %0% 'html_document'         %1%
  yaml_divider                                             -> md_file_header

bib_info %>%
  data.frame(stringsAsFactors=FALSE) %>%
  arrange(desc(dat)) %>%
  pull(brf) %>%
  sub("../blog/", "", ., fixed=TRUE) %>%
  paste0("\n\nB-", n_files:1, ". ", .) %>%
  paste0(collapse="") -> md_file_body

  new_name <- r3_path %s% "index.md"
  if (verbose) {
    "\nWriting  index.md"
      "." %>% cat
  }
writeLines(paste0(md_file_header, md_file_body), new_name)

## Step 4. Add a footer

## Step 5. Convert to html

r4_path <- sub("/r3", "/r4", r3_path)
md_file <- r3_path %s% "index.md"
render(md_file, output_dir=r4_path)
for (i_file in 1:n_files) {
  md_file <- r3_path %s% bib_info[i_file, "nam"] %0% ".md"
  render(md_file, output_dir=r4_path)
}

## Step 6. List incomplete areas

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

## Save everything

save.image("data/convert-bib.RData")
