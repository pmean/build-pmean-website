#  convert-md.R
## written by Steve Simon
## created 2019-12-21

## Step 0. Preliminaries

source(file="src/prelims.R", echo=FALSE)
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
more_divider <- "<!---More--->"

nam <- rep("no data", n_files)
ttl <- rep("no data", n_files)
dat <- rep("no data", n_files)
tag <- rep("no data", n_files)
ctg <- rep("no data", n_files)

orig_tx <- as.list(rep("no data", n_files))
head_tx <- as.list(rep("no data", n_files))
shrt_tx <- rep("no data", n_files)
full_tx <- rep("no data", n_files)
newh_tx <- rep("no data", n_files)

comp_tx <- rep("No data", n_files)
foot_tx <- rep("No data", n_files)
summ_tx <- rep("No data", n_files)

### Read and extract

for (i in 1:n_files) {
  i_file <- file_list[i]
  "\nExtracting information from" %b% i_file %0% "." %>% cat 
  tx <- readLines(i_file)
  
  tx %>% str_which(yaml_divider) %>% first  %>% add(1)          -> div0
  tx %>% str_which(yaml_divider) %>% nth(2) %>% subtract(1)     -> div1
  tx %>% str_which(yaml_divider) %>% nth(2) %>% add(1)          -> div2
  tx %>% str_which(more_divider) %>% first  %>% subtract(1)     -> div3
  tx %>% length                                                 -> div4
  
  div3 <- min(div3, div4, na.rm=TRUE)

  orig_tx[[i]] <- tx
  head_tx[[i]] <- tx[div0:div1]
  shrt_tx[i] <- str_c(tx[div2:div3], collapse="\n")
  full_tx[i] <- str_c(tx[div2:div4], collapse="\n")
  
  i_file %>% str_remove(fixed(".md")) %>% str_remove("^.*/") -> nam[i]
  tx %>% extract_field("title: "   ) -> ttl[i]
  tx %>% extract_field("date: "    ) -> dat[i]
  tx %>% extract_field("tags: "    ) -> tag[i]
  tx %>% extract_field("category: ") -> ctg[i]
}


ctg %>% tolower %>% str_replace("statistics", "blog entry") -> ctg
dat %>% str_sub(1, 7) -> mnt
dat %>% str_sub(8, 10) -> day

### Build pieces of new files

for (i in 1:n_files) {

  yaml_divider                     %1%
    "title:" %s% ttl[i] %p% dat[i] %1%
    "output: html_document"        %1%
    yaml_divider                   %>%
    str_c(collapse="\n")                       -> newh_tx[i]
  build_footer(tag[i], ctg[i], dat[i])         -> foot_tx[i]
  build_link(nam[i], "../blog") %b% shrt_tx[i] -> summ_tx[i]
  
}

for (i in 1:n_files) {
  fn <- md_root %s% "blog" %s% nam[i] %0% ".md"
  if (verbose) {"\nWriting" %b% fn %0% "." %>% cat}

  newh_tx[i] %2% full_tx[i] %2% foot_tx[i] -> comp_tx[i]
  comp_tx[i] %>% writeLines(fn)
}

## Step 4. Produce an index

if (verbose) {"\nWriting index.md." %>% cat}

yaml_divider                                               %1%
  'title: '                    %q% 'All blog entries'      %1%
  'output: '                   %0% 'html_document'         %1%
  yaml_divider                                             -> md_file_header

n_files <- length(dat)
o <- rev(order(dat)) 
summ_tx[o] %>%
  paste0("\n\nB-", n_files:1, ". ", .) %>%
  paste0(collapse="") -> md_file_body

new_name <- md_root %s% "archive" %s% "index.md"
writeLines(paste0(md_file_header, md_file_body), new_name)

## Step 5. Convert to html

# r4_path <- sub("/r3", "/r4", r3_path)
# md_file <- r3_path %s% "index.md"
# render(md_file, output_dir=r4_path)
# for (i_file in 1:n_files) {
  # md_file <- r3_path %s% bib_info[i_file, "nam"] %0% ".md"
  # render(md_file, output_dir=r4_path)
# }

## Save everything

save.image("data/convert-md.RData")
