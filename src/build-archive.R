#  convert-md.R
## written by Steve Simon
## created 2019-12-21

## Step 0. Preliminaries

source(file="src/prelims.R", echo=FALSE)
load("data/summaries.RData")

## Step 1. List twenty most recent files

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
  yaml_divider                                               %1%
    'title: '                    %q% 'Archive:' %b% i_month  %1%
    'output: '                   %0% 'html_document'         %1%
    yaml_divider                                             -> md_file_header
  
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
  yaml_divider                                               %1%
    'title: '                    %q% 'Archive:' %b% i_tag    %1%
    'output: '                   %0% 'html_document'         %1%
    yaml_divider                                             -> md_file_header
  
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
  yaml_divider                                               %1%
    'title: '                    %q% 'Archive:' %b% i_month  %1%
    'output: '                   %0% 'html_document'         %1%
    yaml_divider                                             -> md_file_header
  
  sb <- ctg==i_ctg
  n_sb <- sum(sb)
  if (verbose) {" " %0% n_sb %>% cat}
  summ_tx[sb] %>% 
    paste0("\n\nB-", n_sb:1, ". ", .) %>%
    paste0(collapse="") -> md_file_body
  
  new_name <- md_root %s% "archive" %s% i_ctg %0% ".md"
  writeLines(paste0(md_file_header, md_file_body), new_name)
}

## Step 4. Convert to html

# r4_path <- sub("/r3", "/r4", r3_path)
# md_file <- r3_path %s% "index.md"
# render(md_file, output_dir=r4_path)
# for (i_file in 1:n_files) {
  # md_file <- r3_path %s% bib_info[i_file, "nam"] %0% ".md"
  # render(md_file, output_dir=r4_path)
# }

## Save everything

save.image("data/convert-md.RData")
