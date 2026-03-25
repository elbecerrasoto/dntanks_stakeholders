library(tidyverse)
library(readxl)
library(ggthemes)
library(tidyverse)
library(rlang)

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

# x |>
#   write_tsv("output/all_profile02.tsv")

plot_elbow_cutoff(x, RATE_FILL, cutoff = RATE_FILL_CUTOFF)
plot_elbow_cutoff(x, ENTROPY, cutoff = ENTROPY_CUTOFF)

suggest_cutoff_elbow(x, RATE_FILL)
suggest_cutoff_elbow(x, ENTROPY)


if (F) {
  # Intresting Viz ----------------------------------------------------------




  library(ggthemes)

  # Create a quadrant identifier for the plot
  plot_data <- x |>
    mutate(
      Action = ifelse(SHOULD_MIGRATE_COL, "Migrate", "Archive"),
      Category = case_when(
        IS_KEY ~ "System Key (Auto-Migrate)",
        TRUE ~ Action
      )
    )

  ggplot(plot_data, aes(x = RATE_FILL, y = RATE_DOMINANT, color = Category)) +
    geom_point(alpha = 0.7, size = 3) +
    geom_vline(xintercept = RATE_FILL_CUTOFF, linetype = "dashed", color = "black") +
    geom_hline(yintercept = RATE_DOMINANT_CUTOFF, linetype = "dashed", color = "black") +
    scale_color_manual(values = c(
      "Archive" = "#ff2700",
      "Migrate" = "#008fd5",
      "System Key (Auto-Migrate)" = "#77ab43"
    )) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text()) +
    labs(
      title = "Which Columns Make the Cut?",
      subtitle = "Balancing data density against variability.",
      x = "Fill Rate (Higher is better)",
      y = "Dominance Rate (Lower is better)",
      color = "Decision"
    ) +
    annotate("text", x = 0.8, y = 0.2, label = "High Value Data\n(Dense & Varied)", fontface = "bold") +
    annotate("text", x = 0.1, y = 0.9, label = "Low Value Data\n(Sparse & Static)", fontface = "bold")


  library(dplyr)
  library(ggplot2)
  library(forcats)

  # Prepare data: order tables by their average fill rate
  genomic_data <- x |>
    group_by(NAME_TABLE) |>
    mutate(Avg_Table_Fill = mean(RATE_FILL, na.rm = TRUE)) |>
    ungroup() |>
    mutate(NAME_TABLE = fct_reorder(NAME_TABLE, Avg_Table_Fill, .desc = TRUE)) |>
    arrange(NAME_TABLE) |>
    mutate(Global_Index = row_number()) # Create continuous x-axis across all tables

  # Calculate midpoints for table labels on the x-axis
  axis_set <- genomic_data |>
    group_by(NAME_TABLE) |>
    summarize(center = mean(Global_Index))

  # Alternating colors for the "chromosomes" (tables)
  genomic_data <- genomic_data |>
    mutate(Color_Group = as.numeric(NAME_TABLE) %% 2)

  ggplot(genomic_data, aes(x = Global_Index, y = RATE_FILL)) +
    geom_jitter(aes(color = as.factor(Color_Group), size = RATE_DOMINANT), alpha = 0.6, width = 0.4) +
    scale_color_manual(values = c("#1f77b4", "#aec7e8")) + # Classic D3/Genomic alternating colors
    scale_size_continuous(range = c(4, 1), name = "Variance (Smaller = More Static)") +
    scale_x_continuous(label = axis_set$NAME_TABLE, breaks = axis_set$center) +
    geom_hline(yintercept = RATE_FILL_CUTOFF, linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      title = "Data Topology: The Manhattan Plot",
      subtitle = "Each cluster is a CRM Table. Points above the red line are candidates for migration.",
      x = "CRM Tables (Ordered by Overall Health)",
      y = "Column Fill Rate"
    ) +
    guides(color = "none") # Hide the alternating color legend
}
