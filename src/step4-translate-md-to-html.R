#  build-html.R
## written by Steve Simon
## created 2019-12-21

## Step 0. Preliminaries

source(file="src/prelims.R", echo=FALSE)

wb_root <- "c:/Users/steve/Dropbox/professional/web"

ar_root <- wb_root %s% "md/archive"
ar_names <- list.files(path=ar_root, pattern="*.md")

html_path <- wb_root %s% "website/blog"

ar_names

bl_root <- wb_root %s% "md/blog"
bl_names <- list.files(path=bl_root, pattern="*.md")

bl_names

## Step 1a. Search for files not needing updates

bl_root <- wb_root %s% "md/blog"

bl_files <- list.files(path=bl_root, pattern="*.md")

bl_names <- bl_root %s% bl_files

wb_names <- wb_root %s% "website/blog" %s% str_replace(bl_files, fixed(".md"), ".html")

changed_list <- NULL
for (i in 1:length(bl_names)) {
  i_file <- bl_names[i]
  wb_file <- wb_names[i]
  if (!file.exists(wb_file)) {
    changed_list %<>% append(bl_files[i])
    if (verbose) "\n" %0% bl_files[i] %b% "not matched." %>% cat
    next
  }
  t0 <- file.info(i_file)$mtime
  t1 <- file.info(wb_file)$mtime
  if (t1-t0 > 0) next
  changed_list %<>% append(bl_files[i])
  if (verbose) {
    "\n" %0% t1 %b% t0 %b% str_remove(i_file, "^.*/") %>% cat
  }
}

## Step 4. Convert to html

for (i_file in changed_list) {
  md_file <- bl_root %s% i_file
  if (verbose) {
    "\nConverting" %b% md_file %>% cat
  }
  html_path <- wb_root %s% "website/blog"
  render(md_file, output_dir=html_path)
}

for (i_file in ar_names) {
  md_file <- ar_root %s% i_file
  if (verbose) {
    "/nConverting" %b% md_file %>% cat
  }
  html_path <- wb_root %s% "website/archive"
  render(md_file, output_dir=html_path)
}

## Save everything

save.image("data/convert-md.RData")
