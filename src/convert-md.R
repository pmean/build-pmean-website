#  convert-md.R
## written by Steve Simon
## created 2019-12-21

## Step 0. Preliminaries

source(file="src/prelims.R", echo=FALSE)
verbose <- TRUE

extract_field <- function(x, field_name) {
  str_subset(x, fixed(field_name, ignore_case=TRUE)) %>%
    str_remove(field_name) %>%
    str_remove_all(fixed('"')) %>%
    str_trim %>%
    str_c(collapse=", ") -> extracted_text
  if (length(extracted_text==0)) return("not found")
    return
}

build_link <- function(x, p="../archive/") {
  link1 <- brack(x) %0% "(" %0% p %0% gsub(" ", "-", x) %0% ".html)"
}

## Step 1. Find the .md files

wb_root <- "c:/Users/steve/Dropbox/professional/web"


b3_root <- wb_root %s% "b3"
r3_root <- wb_root %s% "r3"
md_root <- wb_root %s% "md"

b3_names <- list.files(path=b3_root, pattern="*.md")

r3_names <- list.files(path=r3_root, pattern="*.md")

file_list <- c(b3_root %s% b3_names, r3_root %s% r3_names)
n_files <- length(file_list)

## Step 2. Read each .md file and extract information

### Initialize values

yaml_divider <- '---'
more_divider <- "<---More--->"

nam <- rep("no data", n_files)
ttl <- rep("no data", n_files)
dat <- rep("no data", n_files)
tag <- rep("no data", n_files)
ctg <- rep("no data", n_files)

orig_tx <- as.list(rep("no data", n_files))
head_tx <- as.list(rep("no data", n_files))
shrt_tx <- as.list(rep("no data", n_files))
full_tx <- as.list(rep("no data", n_files))

comp_tx <- as.list(rep("No data", n_files))
foot_tx <- as.list(rep("No data", n_files))
link_tx <- as.list(rep("No data", n_files))

### Read and extract

n_files <- 5
for (i in 1:n_files) {
  i_file <- file_list[i]
  tx <- readLines(i_file)
  
  tx %>% str_which(yaml_divider) %>% min %>% add(1)      -> div0
  tx %>% str_which(yaml_divider) %>% max %>% subtract(1) -> div1
  tx %>% str_which(yaml_divider) %>% max %>% add(1)      -> div2
  tx %>% str_which(more_divider) %>% max(length(tx))     -> div3
  tx %>% length                                          -> div4

  orig_tx[[i]] <- tx
  head_tx[[i]] <- tx[div0:div1]
  shrt_tx[[i]] <- tx[div2:div3]
  full_tx[[i]] <- tx[div2:div4]
  
  i_file %>% str_remove(fixed(".md")) %>% str_remove("^.*/") -> nam[i]
  tx %>% extract_field("title: "   ) -> ttl[i]
  tx %>% extract_field("date: "    ) -> dat[i]
  tx %>% extract_field("tags: "    ) -> tag[i]
  tx %>% extract_field("category: ") -> ctg[i]
}


ctg %>% tolower %>% str_replace("statistics", "blog entry") -> ctg
dat %>% str_sub(1, 7) -> mnt
dat %>% str_sub(8, 10) -> day

### Build new files

for (i in 1:n_files) {
  i_file <- file_list[i]

  tag[i] %>%
    strsplit(", ") %>%
    build_link %>%
    paste(collapse=", ") -> tag_link
    
  "This" %b% build_link(ctg[i]) %b% "was added to this website" %1%
    build_link(mnt[i]) %0% day[i] %0% "."                          %1%
    "You can find similar pages at" %s% tag_link %0% "."     -> foot_tx[[i]]

  build_link(nam[i_file], "../blog") %b% shrt_tx -> link_tx[[i]]
  
}

for (i in 1:n_files) {
  i_file <- file_list[i]

  yaml_divider                               %1%
    "title:" %s% ttl[i_file] %p% dat[i_file] %1%
    "output: html_document"                  %1%
    yaml_divider                             %2%
    
    full_tx[[i]]                             %2%
    
    foot_tx[[i]]                             -> comp_tx[[i]]
  
  # comp_tx[[i]] %>% writeLines(md_root %s% "blog" %s% nam[i_file] %0% ".md")
    
}

## Step 4. Produce an index

yaml_divider                                               %1%
  'title: '                    %q% 'All blog entries'      %1%
  'output: '                   %0% 'html_document'         %1%
  yaml_divider                                             -> md_file_header

n_files <- length(dat)
o <- rev(order(dat))
  paste0("\n\nB-", n_files:1, ". ", .) %>%
  paste0(collapse="") -> md_file_body

new_name <- md_root %s% "archive" %s% "index.md"
if (verbose) {"\nWriting  index.md" %>% "." %>% cat}
writeLines(paste0(md_file_header, md_file_body), new_name)

## Step 5. Convert to html

r4_path <- sub("/r3", "/r4", r3_path)
md_file <- r3_path %s% "index.md"
render(md_file, output_dir=r4_path)
for (i_file in 1:n_files) {
  md_file <- r3_path %s% bib_info[i_file, "nam"] %0% ".md"
  render(md_file, output_dir=r4_path)
}

## Save everything

save.image("data/convert-md.RData")
