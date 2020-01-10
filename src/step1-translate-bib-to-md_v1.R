# step1-translate-bib-to-md.R
# written by Steve Simon
# created 2019-06-28

# This program finds all the bibtex files
# (extension .bib) in r1 and converts them
# to markdown format (extension .md) and
# stores them in md/r3.

# Step 1-1. Preliminaries

source(file="src/prelims.R")
if (!exists("verbose")) verbose <- TRUE
if (!exists("update_all")) update_all <- TRUE

# Step 1-2. Find the .bib files

bib_root <- "../md/bib"
yr_list <- list.dirs(bib_root, recursive=FALSE)

pull_bibtex <- function(tx) {
  fields <- as.list(rep("Missing", 8))
  names(fields) <- c("ti", "da", "ta", "na", "no", "fo", "ur", "au")
  tx    %>% extract_bibtex_field("title="       ) -> fields$ti
  tx    %>% extract_bibtex_field("urldate="     ) -> fields$da
  tx    %>% extract_bibtex_field("mendeley-tags") -> fields$ta
  tx[1] %>% extract_bibtex_field("@"            ) -> fields$na
  tx    %>% extract_bibtex_field("annote="      ) -> fields$no
  tx    %>% extract_bibtex_field("format="      ) -> fields$fo
  tx    %>% extract_bibtex_field("url="         ) -> fields$ur
  tx    %>% extract_bibtex_field("author="      ) -> fields$au
  return(fields)
}

check_dates <- function(f0, f1) {
  t0 <- file.info(f0)$mtime
  t1 <- file.info(f1)$mtime
  skip_flag <- !is.na(t1) & (t1-t0 < 0) & !update_all
  if (skip_flag) {
    if (verbose) {"\n    Skipping  " %b% f1 %>% cat}
  }
  return(skip_flag)
}

build_body <- function(f0) {
  f0 %>% str_replace("bib$", "md") -> f1
  if (check_dates(f0, f1)) return("Skipping" %b% f1)
  if (verbose)   {"\n    Working on" %b% f1 %>% cat}
  tx <- readLines(f0)
  f <- pull_bibtex(tx)
  
  # Modify title
  f$ti <- "Recommendation:" %b% f$ti
 
  # Modify long author lists 
  n_authors <- str_count(f$au, fixed(" and ", ignore_case=TRUE)) + 1
  if (n_authors > 2) {
    f$au %<>% str_replace(regex(" and .*", ignore_case=TRUE), " et al")}
  
  # Modify format if not found
  if (f$fo=="Not found") {
    f$fo %<>%
      str_detect(regex("pdf$", ignore_case=TRUE)) %>%
      ifelse("pdf", "html")
  }
  f0 %>% str_remove(bib_root) %>% str_remove(fixed(".bib")) -> f$so
  f$im <- str_remove(f$so, "^.*/") %0% ".png"
  f$ci <- f$au %.% f$ti %.% "Available in " %>% str_wrap(50) %1%
    brack(f$fo) %p% f$ur %0% "."
  
  new_tx <-
    "---"                             %1%
    "title: "    %q% f$ti             %1%
    "author: "   %q% f$au             %1%
    "date: "     %q% f$da             %1%
    "category: " %q% "Recommendation" %1%
    "tags: "     %q% f$ta             %1%
    "source: "   %q% f$so             %1%
    "name: "     %q% f$na             %1%
    "output: "   %0% "html_document"  %1%
    "---"                             %2%
    
    str_wrap(f$no, 50)                %2%
    
    "<---More--->"                    %2%
    
    f$ci                              %2%
    
    "![]"        %p% f$im             %1%
    "\n"
  if (verbose) {"\n\n" %0% new_tx %>% cat}
  writeLines(new_tx, f1)
}

for (i_yr in yr_list) {
  if (verbose) {"\nYear =" %b% i_yr %>% cat}
  mo_list <- list.dirs(path=i_yr, recursive=FALSE)
  for (i_mo in mo_list) {
    if (verbose) {"\n  Month =" %b% i_mo %>% cat}
    md_list <- list.files(i_mo, pattern="*.bib")
    for (i_md in md_list) {
      build_body(i_mo %s% i_md)
    }
  }
}

build_tail <- function(f0) {
  f0 %>% str_replace("bib$", "tail") -> f1
  if (check_dates(f0, f1)) {return("Skipping" %b% f1)}
  if (verbose)   {"\n    Working on" %b% f1 %>% cat}
  tx <- readLines(f0)
  f <- pull_bibtex(tx)
  f$ca <- "Recomendation"
  f$mo <- str_sub(f$da, 1, 7)
  f$dt <- str_sub(f$da, 8, 10)
  f$dm <- max(str_sub(t0, 1, 10), f$da)
  tail_tx <- 
    "This" %b% build_link(f$ca)                 %1%
    "was added to the website on"               %1%
    build_link(f$mo)                %0% f$dt    %1%
    "and was last modified on"                  %1%
    f$da                            %0% "."     %1%
    "You can find similar pages at"             %1%
    build_link(f$ta) %0% ".\n\n"
  
  if (verbose) {"\n\n" %0% tail_tx %>% cat}
  writeLines(tail_tx, f1)
  return(tx)
  }
for (i_yr in yr_list) {
  if (verbose) {"\nYear =" %b% i_yr %>% cat}
  mo_list <- list.dirs(path=i_yr, recursive=FALSE)
  for (i_mo in mo_list) {
    if (verbose) {"\n  Month =" %b% i_mo %>% cat}
    md_list <- list.files(i_mo, pattern="*.bib")
    for (i_md in md_list) {
      tx <- build_tail(i_mo %s% i_md)
    }
  }
}

# Save everything.

save.image("data/step1.RData")
