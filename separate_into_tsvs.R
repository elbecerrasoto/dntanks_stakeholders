library(tidyverse)
library(readxl)
library(ggthemes)
library(tidyverse)
library(rlang)
library(scales)

OUTPUT <- "output/all_profile02.tsv"

RATE_FILL_CUTOFF <- 0.02
ENTROPY_CUTOFF <- 0.0

# Mechanism -----------------------------------------------------------------

suggest_cutoff_elbow <- function(.data, .column) {
  # 1. Prepare and sort the data
  vals <- .data |>
    filter(!is.na({{ .column }})) |>
    pull({{ .column }}) |>
    sort(decreasing = TRUE)

  n <- length(vals)
  indices <- 1:n

  # 2. Define the line connecting the first and last points
  # Line equation: Ax + By + C = 0
  # For points (x1, y1) and (x2, y2):
  # (y1 - y2)x + (x2 - x1)y + (x1y2 - x2y1) = 0

  x1 <- 1
  y1 <- vals[1]
  x2 <- n
  y2 <- vals[n]

  A <- y1 - y2
  B <- x2 - x1
  C <- (x1 * y2) - (x2 * y1)

  # 3. Calculate the distance from every point to that line
  distances <- abs(A * indices + B * vals + C) / sqrt(A^2 + B^2)

  # 4. Find the index with the maximum distance (the "elbow")
  elbow_index <- which.max(distances)
  suggested_value <- vals[elbow_index]

  # Return the value at the elbow
  return(suggested_value)
}

plot_elbow_cutoff <- function(data, column, cutoff = 0.20, title_suffix = "Metric") {
  # 1. Handle indirection with {{ }}
  # 2. Filter out keys to see the 'natural' distribution of data quality
  elbow_data <- data |>
    filter(!IS_KEY) |>
    arrange(desc({{ column }})) |>
    mutate(Rank = row_number())

  # Find the rank closest to the cutoff value for the vertical line
  # We use pull() to get the vector for the which.min calculation
  col_vector <- elbow_data |> pull({{ column }})
  cutoff_index <- which.min(abs(col_vector - cutoff))

  ggplot(elbow_data, aes(x = Rank, y = {{ column }})) +
    # FiveThirtyEight Style Base
    theme_fivethirtyeight() +
    geom_line(color = "#008fd5", linewidth = 1.5) +
    # The "Elbow" Cutoff Line
    geom_vline(xintercept = cutoff_index, linetype = "dashed", color = "#ff2700") +
    # Dynamic Annotation
    annotate(
      "text",
      x = cutoff_index + (max(elbow_data$Rank) * 0.05),
      y = cutoff + 0.05,
      label = paste0("Cutoff: ", scales::percent(cutoff)),
      color = "#ff2700",
      hjust = 0,
      fontface = "bold"
    ) +
    labs(
      title = paste("Elbow Plot:", title_suffix),
      subtitle = paste("Columns ranked by", rlang::as_label(rlang::enquo(column)), "(Excluding Keys)"),
      x = "Column Rank",
      y = "Value"
    ) +
    theme(
      axis.title = element_text(),
      panel.grid.major = element_line(color = "#e0e0e0")
    )
}


should_migrate_heuristic <- function(x) {
  x |>
    mutate(SHOULD_MIGRATE_COL = IS_KEY |
      (RATE_FILL < RATE_FILL_CUTOFF) |
      (ENTROPY > ENTROPY_CUTOFF))
}

arrange_crm_health <- function(x) {
  arrange(
    desc(IS_KEY),
    desc(RATE_FILL),
    RATE_DOMINANT
  )
}


# Policy --------------------------------------------------------------------

x <- read_excel("output/all_profile.xlsx")

x <- x |>
  mutate(ENTROPY = 1 - RATE_DOMINANT)

x <- x |>
  mutate(IS_KEY = IS_PK | IS_FK) |>
  relocate(IS_KEY, .after = SHOULD_MIGRATE_COL)

x <- should_migrate_heuristic(x)

plot_elbow_cutoff(x, RATE_FILL, cutoff = RATE_FILL_CUTOFF)
plot_elbow_cutoff(x, ENTROPY, cutoff = ENTROPY_CUTOFF)

suggest_cutoff_elbow(x, RATE_FILL)
suggest_cutoff_elbow(x, ENTROPY)



# Apply migration logic
y <- x |>
  mutate(
    SHOULD_MIGRATE_COL = case_when(
      IS_KEY ~ TRUE,
      (RATE_FILL >= RATE_FILL_CUTOFF) & (ENTROPY >= ENTROPY_CUTOFF) ~ TRUE,
      TRUE ~ FALSE
    ),
    Action = ifelse(SHOULD_MIGRATE_COL, "Migrate", "Archive"),
    Category = case_when(
      IS_KEY ~ "System Key (Auto-Migrate)",
      TRUE ~ Action
    )
  )



# Interesting Viz ----------------------------------------------------------

ggplot(y, aes(x = RATE_FILL, y = ENTROPY, color = Category)) +
  geom_point(alpha = 0.6, size = 3) +
  # Dynamic Cutoff Lines
  geom_vline(xintercept = RATE_FILL_CUTOFF, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_hline(yintercept = ENTROPY_CUTOFF, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_color_manual(values = c(
    "Archive" = "#ff2700", # Red
    "Migrate" = "#008fd5", # Blue
    "System Key (Auto-Migrate)" = "#77ab43" # Green
  )) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold")) +
  labs(
    title = "Migration Selection Framework",
    subtitle = paste0("Calculated cutoffs: Fill > ", percent(RATE_FILL_CUTOFF), 
                      " | Entropy > ", percent(ENTROPY_CUTOFF)),
    x = "Fill Rate (Completeness)",
    y = "Entropy (Information Variety)",
    color = "Decision"
  ) +
  annotate("label", x = 0.8, y = 0.1, label = "Dense but Monotonous\n(Check for Defaults)", size = 3) +
  annotate("label", x = 0.8, y = 0.9, label = "High Value Data\n(Rich & Complete)", size = 3, fill = "#008fd5", color = "white")



# write data --------------------------------------------------------------

x |> write_tsv(OUTPUT)
