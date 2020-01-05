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
summ_files %>% str_remove(fixed(".md")) -> summ_names 
for (i in 1:n_summ) {
  summ_tx[[summ_names[i]]] <- readLines(summ_path %s% summ_files[i])
}

link_files <- list.files(link_path, pattern="*.txt")
n_link <- length(link_files)
link_tx <- NULL
link_files %>% str_remove(fixed(".txt")) -> link_names 
for (i in 1:n_link) {
  link_tx[[link_names[i]]] <- readLines(link_path %s% link_files[i])
}

if (verbose) {
  d_summ <- setdiff(link_names, summ_names)
  n <- length(d_summ)
  cat("\nThere were" %b% n %b% "files found in link but not found in summ. ")
  d_summ %>% str_c(collapse=", ") %>% cat

  d_link <- setdiff(summ_names, link_names)
  n <- length(d_link)
  cat("\nThere were" %b% n %b% "files files in summ but not found in link. ")
  d_link %>% str_c(collapse=", ") %>% cat
}

## Step 3-2. Order files in reverse chronological order.

dat <- rep("Date not found", n_link)
for (i in 1:n_link) {
  dat[i] <- link_tx[[i]][1]
  link_tx[[i]][1] <- str_sub(dat[i], 1, 7)
}
o <- order(dat, link_names)

dat[rev(o)]
most_recent <- link_name[rev(o)]

most_recent[1:20]

## Step 3-2. List twenty most recent files

if (verbose) {"\nWriting index.md." %>% cat}

yaml_divider                                               %1%
  'title: '                    %q% 'Recent blog entries'   %1%
  'output: '                   %0% 'html_document'         %1%
  yaml_divider                                             -> md_file_header

n_files <- length(most_recent)
md_file_body <- character(0)
for (i in 1:n_files) {
  i_file <- most_recent[i]
  j <- zzzpad(n_files+1-i)
  top_line <- "\n\n### B-" %0% j %0% "." %b% summ_tx[[i_file]][1] 
  md_file_body <- c(md_file_body, top_line, summ_tx[[i_file]][-1])
}
new_name <- md_root %s% "archive" %s% "index.md"
writeLines(c(md_file_header, md_file_body), new_name)

## Step 3-3. Build all other archive pages

n_lines <- sapply(link_tx, length)
name_vector <- rep(names(link_tx), n_lines)
link_tx %>% unlist %>% unname -> arch_vector
head(cbind(arch_vector, name_vector), 50)

arch_vector %>% unique %>% sort -> arch_list
if (verbose) print(arch_list)

for (i_arch in arch_list) {
  if (nchar(i_arch)==0) next
  sb <- name_vector[arch_vector==i_arch]
  sb_list <- intersect(most_recent, sb)
  n_sb <- length(sb_list)

  yaml_divider                                               %1%
    'title: '                    %q% i_arch                  %1%
    'output: '                   %0% 'html_document'         %1%
    yaml_divider                                             -> md_file_header

  md_file_body <- character(0)
  for (i in 1:n_sb) {
    i_file <- sb_list[i]
    j <- zzzpad(n_sb+1-i)
    top_line <- "\n\n### B-" %0% j %0% "." %b% summ_tx[[i_file]][1] 
    md_file_body <- c(md_file_body, top_line, summ_tx[[i_file]][-1])
  }
  new_name <- md_root %s% i_arch %0% ".md"
  if (verbose) "\nPreparing" %b% new_name %>% cat
  writeLines(c(md_file_header, md_file_body), new_name)
}


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

## Save everything

save.image("data/step3.RData")
