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
build_body <- function(f0) {
  f0 %>% str_replace("bib$", "md") -> f1
  t0 <- file.info(f0)$mtime
  t1 <- file.info(f1)$mtime
  if (!is.na(t1) & (t1-t0 < 0) & !update_all) {
    if (verbose) {"\n    Skipping  " %b% f1 %>% cat; next}
  }
  if (verbose)   {"\n    Working on" %b% f1 %>% cat}
  tx <- readLines(f0)
  tx    %>% extract_bibtex_field("title="       ) -> ti
  tx    %>% extract_bibtex_field("urldate="     ) -> da
  tx    %>% extract_bibtex_field("mendeley-tags") -> ta
  tx[1] %>% extract_bibtex_field("@"            ) -> na
  tx    %>% extract_bibtex_field("annote="      ) -> no
  tx    %>% extract_bibtex_field("format="      ) -> fo
  tx    %>% extract_bibtex_field("url="         ) -> ur

  tx    %>% extract_bibtex_field("author="      ) -> au
 
  # Modify long author lists 
  n_authors <- str_count(au, fixed(" and ", ignore_case=TRUE)) + 1
  if (n_authors > 2) {
    au <- str_replace(regex(" and .*", ignore_case=TRUE), " et al")}
  
  # Modify format if not found
  if (fo=="Not found") fo <- ifelse(str_detect(fo, regex("pdf$", ignore_case=TRUE)), "pdf", "html")
  fo <- fo % % "format"
  
  f0 %>% str_remove(bib_root) %>% str_remove(fixed(".bib")) -> so
  im <- str_remove(so, "^.*/") %0% ".png"
  ci <- au %.% ti %.% "Available in " %>% str_wrap(50) %1%
    brack(fo) %p% ur %0% "."
  
  new_tx <-
    "---" %1%
    "title: "    %q% ti               %1%
    "author: "   %q% au               %1%
    "date: "     %q% da               %1%
    "category: " %q% "Recommendation" %1%
    "tags: "     %q% ta               %1%
    "source: "   %q% so               %1%
    "name: "     %q% na               %1%
    "output: "   %0% "html_document"  %1%
    "---"                             %2%
    
    str_wrap(no, 50)                  %2%
    
    "<---More--->"                    %2%
    
    ci                                %2%
    
    "![]"        %p% im               %1%
    "\n"
  if (verbose) {"\n\n" %0% new_tx %>% cat}
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

# Save everything.

save.image("data/step1.RData")
