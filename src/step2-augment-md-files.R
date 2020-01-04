#  step2-augment-md-files.R
## written by Steve Simon
## created 2019-12-21

## Step 2-1. Preliminaries

source(file="src/prelims.R", echo=FALSE)
update_all <- TRUE

## Step 2-2. Find the .md files

wb_root <- "c:/Users/steve/Dropbox/professional/web"

b3_root <- wb_root %s% "md/b3"
r3_root <- wb_root %s% "md/r3"
bl_root <- wb_root %s% "md/blog"

b3_names <- compare_dates(b3_root, bl_root)
r3_names <- compare_dates(r3_root, bl_root)

if (update_all) {
  b3_root %>%
    list.files(pattern="*.md") %>%
    str_remove(fixed(".md")) -> b3_names
  r3_root %>%
    list.files(pattern="*.md") %>%
    str_remove(fixed(".md")) -> r3_names
}

file_list <- character(0)

if (length(b3_names) > 0) file_list %<>% append(b3_root %s% b3_names %0% ".md")
if (length(r3_names) > 0) file_list %<>% append(r3_root %s% r3_names %0% ".md")

n_files <- length(file_list)
if (n_files==0) {stop("No files need to be updated.")}

## Step 1a. Search for files not needing updates

bl_root <- wb_root %s% "md/blog"

bl_files <- list.files(path=bl_root, pattern="*.md")


## Step 2-3. Read each .md file and extract information

### Initialize values

yaml_divider <- '---'
more_divider <- "<!---More--->"
no_data_label <- "No data"

nam <- rep(no_data_label, n_files)
ttl <- rep(no_data_label, n_files)
dat <- rep(no_data_label, n_files)
tag <- rep(no_data_label, n_files)
ctg <- rep(no_data_label, n_files)

orig_tx <- as.list(rep(no_data_label, n_files))
head_tx <- as.list(rep(no_data_label, n_files))

shrt_tx <- rep(no_data_label, n_files)
full_tx <- rep(no_data_label, n_files)
newh_tx <- rep(no_data_label, n_files)

comp_tx <- rep(no_data_label, n_files)
foot_tx <- rep(no_data_label, n_files)
summ_tx <- rep(no_data_label, n_files)

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


ctg %<>% tolower
dat %>% str_sub(1, 7) -> mnt
dat %>% str_sub(8, 10) -> day

### Build pieces of new files

for (i in 1:n_files) {
  full_title <- ttl[i] %0% " " %p% dat[i]

  yaml_divider                                                  %1%
    'title: ' %q% full_title                                    %1%
    'output: html_document'                                     %1%
    yaml_divider                                                %>%
    str_c(collapse="\n")                                        -> newh_tx[i]
  
  build_footer(tag[i], ctg[i], dat[i])                          -> foot_tx[i]
  
  brack(full_title) %0% paren("../blog" %s% nam[i] %0% ".html") %1% 
    shrt_tx[i]                                                  %>%
    str_c(collapse="\n")                                        -> summ_tx[i]
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
