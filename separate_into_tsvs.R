library(tidyverse)
library(readxl)

x <- read_excel("output/all_profile.xlsx")

RATE_FILL_CUTOFF <- 0.02
RATE_DOMINANT_CUTOFF <- 1.0

x <- x |>
  mutate(IS_KEY = IS_PK | IS_FK) |>
  relocate(IS_KEY, .after = SHOULD_MIGRATE_COL)

should_migrate_heuristic <- function(x) {
  x |>
    mutate(SHOULD_MIGRATE_COL = IS_KEY |
      (RATE_FILL < RATE_FILL_CUTOFF) |
      (RATE_DOMINANT < RATE_DOMINANT_CUTOFF))
}

arrange_crm_health <- function(x) {
  arrange(desc(IS_KEY),
          desc(RATE_FILL),
          RATE_DOMINANT)
}


x <- should_migrate_heuristic(x)
