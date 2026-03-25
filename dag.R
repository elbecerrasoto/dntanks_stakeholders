#!/usr/bin/env Rscript
library(tidyverse)

llm_print <- function(obj, n = 10, clipboard = FALSE) {
  # 1. Take the head
  snip <- head(obj, n)
  
  # 2. Setup destination (Wayland clipboard vs Terminal)
  if (clipboard) {
    dest <- pipe("wl-copy")
    # This ensures the pipe closes even if the function errors out
    on.exit(close(dest))
  } else {
    dest <- "" # "" represents the standard output/terminal in R
  }
  
  # 3. Handle the output format
  if (is.data.frame(snip) || is.matrix(snip)) {
    # If it's a table, write as clean TSV
    write.table(as.data.frame(snip), file = dest, sep = "\t", 
                row.names = FALSE, quote = FALSE, na = "NA")
  } else {
    # If it's a vector, write raw lines (removes [1] and quotes)
    # as.character ensures factors or NAs don't break the call
    writeLines(as.character(snip), con = dest)
  }
}


profiles <- read_tsv("output/all_profile02.tsv")
tables <- readxl::read_excel("output/tabs_profile.xlsx")

readr::write_tsv(head(tables, 10), file = stdout())

llm_print(tables$LIST_TABLES_POINTED, n = 2, clipboard = T)
llm_print(names(tables), n = 100, clipboard = T)
llm_print(tables, n = 10, clipboard = T)

          