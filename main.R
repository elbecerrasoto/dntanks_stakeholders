#!/usr/bin/env Rscript
library(tidyverse)

# utils -------------------------------------------------------------------

tib2xlsx <- function(tib, path) {
  openxlsx::write.xlsx(tib,
             path,
             asTable = TRUE,
             tableStyle = "TableStyleMedium2")
}

truncate_tib <- function(tib) {
  TRUNCATE <- 1024
  mutate(tib, across(where(is.character), ~ str_trunc(., width = TRUNCATE)))
}

# read the data -----------------------------------------------------------

cols_focus <- read_tsv("input/01_cols_focus.tsv") |> truncate_tib()
tabs_focus <- read_tsv("input/02_tabs_focus.tsv") |> truncate_tib()

cols_focus <- cols_focus |> select(-PROFILED_AT)
tabs_focus <- tabs_focus |> select(-PROFILED_AT)

join2cols <- read_tsv("input/join2cols.tsv")
join2tabs <- read_tsv("input/join2tabs.tsv")

# join extra info ---------------------------------------------------------

cols_focus <- left_join(cols_focus, join2cols, join_by(NAME_TABLE, NAME_COLUMN))
tabs_focus <- left_join(tabs_focus, join2tabs, join_by(NAME_TABLE)) 

all_profile <- left_join(cols_focus, tabs_focus, join_by(NAME_TABLE, COUNT_ROWS))

# rearrange ---------------------------------------------------------------

all_profile <- all_profile |>
  relocate(NAME_TABLE,
           NAME_COLUMN,
           NAME_DISPLAY,
           SHOULD_MIGRATE_COL,
           IS_PK, IS_FK, 
           RATE_FILL, RATE_DOMINANT,
           RATE_ACTIVE_ROWS,
           VALUE_SAMPLED,
           LIST_VALUES, TYPE_DYNAMICS) |>
  arrange(desc(SHOULD_MIGRATE_COL),
          desc(IS_PK), desc(IS_FK),
          desc(RATE_FILL), RATE_DOMINANT, desc(RATE_ACTIVE_ROWS))

# write results -----------------------------------------------------------

all_profile |> tib2xlsx("output/all_profile.xlsx")
cols_focus |> tib2xlsx("output/cols_profile.xlsx")
tabs_focus |> tib2xlsx("output/tabs_profile.xlsx")
