library(readxl)
library(tidyverse)
library(clipr)

x <- read_excel("output/all_profile02.tsv")

x |>
  filter(NAME_TABLE == "STG_OPPORTUNITY") |>
  pull(NAME_COLUMN) |>
  write.table()



x |>
  filter(NAME_TABLE == "STG_OPPORTUNITY") |>
  pull(NAME_COLUMN)