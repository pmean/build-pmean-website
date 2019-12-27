#  convert-md.R
## written by Steve Simon
## created 2019-12-21

## Step 0. Preliminaries

source(file="src/prelims.R", echo=FALSE)

wb_root <- "c:/Users/steve/Dropbox/professional/web"

ar_root <- wb_root %s% "md/archive"

ar_names <- list.files(path=ar_root, pattern="*.md")

ar_names

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
