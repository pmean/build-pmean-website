#  step3-build-archive-files.R
## written by Steve Simon
## created 2019-12-21

## Step 3-0. Preliminaries

source(file="src/prelims.R", echo=FALSE)
wb_root <- "c:/Users/steve/Dropbox/professional/web"
summ_path <- wb_root %s% "md/summ"
link_path <- wb_root %s% "md/link"

## Step 3-1. Read files

summ_files <- list.files(summ_path, pattern="*.md") 
n_summ <- length(summ_files)
summ_tx <- NULL
summ_files %>% str_remove(fixed(".md")) -> summ_name 
for (i in 1:n_summ) {
  summ_tx[[summ_name[i]]] <- readLines(summ_path %s% summ_files[i])
}

link_files <- list.files(link_path, pattern="*.txt")
n_link <- length(link_files)
link_tx <- NULL
link_files %>% str_remove(fixed(".txt")) -> link_name 
for (i in 1:n_summ) {
  link_tx[[link_name[i]]] <- readLines(link_path %s% link_files[i])
}



## Step 3-2. List twenty most recent files

if (verbose) {"\nWriting index.md." %>% cat}

yaml_divider                                               %1%
  'title: '                    %q% 'Recent blog entries'   %1%
  'output: '                   %0% 'html_document'         %1%
  yaml_divider                                             -> md_file_header

n_files <- length(dat)
o <- rev(order(dat))
summ_tx[o][1:20] %>%
  paste0("\n\nB-", n_files:(n_files-19), ". ", .) %>%
  paste0(collapse="") -> md_file_body

new_name <- md_root %s% "archive" %s% "index.md"
writeLines(paste0(md_file_header, md_file_body), new_name)

## Step 2. Month by month archive

for (i_month in sort(unique(mnt))) {
  if (verbose) {"\nWriting" %b% i_month %0% "." %>% cat}

  yaml_divider                                     %1%
    'title: "Archive:' %b% i_month         %0% '"' %1%
    'output: '         %0% 'html_document'         %1%
    yaml_divider                                   -> md_file_header
  
  sb <- mnt==i_month
  n_sb <- sum(sb)
  if (verbose) {" " %0% n_sb %>% cat}
  summ_tx[sb] %>% 
    paste0("\n\nB-", n_sb:1, ". ", .) %>%
    paste0(collapse="") -> md_file_body
  
  new_name <- md_root %s% "archive" %s% i_month %0% ".md"
  writeLines(paste0(md_file_header, md_file_body), new_name)
}

## Step 3. Topic archive

tag %>% str_split(", ") %>% unlist %>% unique -> tag_list

for (i_tag in tag_list) {
  if (verbose) {"\nWriting" %b% i_tag %0% "." %>% cat}

  yaml_divider                                     %1%
    'title: "Archive:' %b% i_tag           %0% '"' %1%
    'output: '         %0% 'html_document'         %1%
    yaml_divider                                   -> md_file_header
  
  tag %>% str_detect(i_tag) -> sb
  n_sb <- sum(sb)
  if (verbose) {" " %0% n_sb %>% cat}
  summ_tx[sb] %>% 
    paste0("\n\nB-", n_sb:1, ". ", .) %>%
    paste0(collapse="") -> md_file_body
  
  new_name <- md_root %s% "archive" %s% str_replace(i_tag, " ", "-") %0% ".md"
  writeLines(paste0(md_file_header, md_file_body), new_name)
}

## Step 4. Category archive

for (i_ctg in sort(unique(ctg))) {
  if (verbose) {"\nWriting" %b% i_ctg %0% "." %>% cat}

  yaml_divider                                     %1%
    'title: "Archive:' %b% i_ctg           %0% '"' %1%
    'output: '         %0% 'html_document'         %1%
    yaml_divider                                   -> md_file_header
  
  sb <- ctg==i_ctg
  n_sb <- sum(sb)
  if (verbose) {" " %0% n_sb %>% cat}
  summ_tx[sb] %>% 
    paste0("\n\nB-", n_sb:1, ". ", .) %>%
    paste0(collapse="") -> md_file_body
  
  new_name <- md_root %s% "archive" %s% i_ctg %0% ".md"
  writeLines(paste0(md_file_header, md_file_body), new_name)
}

## Save everything

save.image("data/convert-md.RData")
