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

## Step 1a. Search for files not needing updates

bl_root <- wb_root %s% "md/blog"

bl_files <- list.files(path=bl_root, pattern="*.md")

changed_list <- NULL
for (i in 1:length(bl_files)) {
  i_file <- bl_names[i]
  wb_file <- wb_names[i]
  for (i_file in file_list) {
  t0 <- file.info(i_file)$mtime
  if (!file.exists(wb_file)) {
    changed_list %<>% append(bl_files[i])
    if (verbose) "\n" %0% bl_files[i] %b% "not matched." %>% cat
    next
  }
  i_file %>%
    str_replace("^.*/", bl_root %0% "/") %>%
    file.info %>%
    pull(mtime) -> t1
  if (t1-t0 > 0) next
  changed_list %<>% append(i_file)
  if (verbose) {
    "\n" %0% t1 %b% t0 %b% str_remove(i_file, "^.*/") %>% cat
  }
}

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

  yaml_divider                               %1%
    'title: "' %0% ttl[i] %p% dat[i] %0% '"' %1%
    'output: html_document'                  %1%
    yaml_divider                             %>%
    str_c(collapse="\n")                       -> newh_tx[i]
  build_footer(tag[i], ctg[i], dat[i])         -> foot_tx[i]
  brack(ttl[i] %0% " " %p% dat[i]) %0%
    paren("../blog" %s% nam[i] %0% ".html")    -> summ_tx[i]
  
}

for (i in 1:n_files) {
  fn <- md_root %s% "blog" %s% nam[i] %0% ".md"
  if (verbose) {"\nWriting" %b% fn %0% "." %>% cat}

  newh_tx[i] %2% full_tx[i] %2% foot_tx[i] -> comp_tx[i]
  comp_tx[i] %>% writeLines(fn)
}

## Step 4. Save information for building an archive.

o <- rev(order(dat, ttl))

dat <- dat[o]
mnt <- mnt[o]
tag <- tag[o]
ctg <- ctg[o]
summ_tx <- summ_tx[o]

save(ctg, dat, mnt, tag, summ_tx, file="data/summaries.RData")

## Save everything

save.image("data/convert-md.RData")
