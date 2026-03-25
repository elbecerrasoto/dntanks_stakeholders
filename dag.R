#!/usr/bin/env Rscript
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggthemes)

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
    write.table(as.data.frame(snip),
      file = dest, sep = "\t",
      row.names = FALSE, quote = FALSE, na = "NA"
    )
  } else {
    # If it's a vector, write raw lines (removes [1] and quotes)
    # as.character ensures factors or NAs don't break the call
    writeLines(as.character(snip), con = dest)
  }
}


tables <- readxl::read_excel("output/tabs_profile.xlsx")


# 1. Rename the column to something more standard
tables <- tables %>%
  rename(OUTWARD_CONNECTIONS = LIST_TABLES_POINTED)



# --- 1. PREP NODES ---
# Ensure no NAs and no duplicates in the table names
nodes <- tables %>%
  select(name = NAME_TABLE) %>%
  filter(!is.na(name), name != "NA", name != "") %>%
  distinct()

# --- 2. PREP EDGES (The "Strict Clean") ---
edges <- tables %>%
  # Rename the column as requested
  select(from = NAME_TABLE, to_list = OUTWARD_CONNECTIONS) %>%
  # Filter out real NAs and the literal string "NA" before expansion
  filter(!is.na(to_list), to_list != "NA", to_list != "") %>%
  separate_rows(to_list, sep = ",") %>%
  mutate(to = trimws(to_list)) %>%
  # Remove any rows where 'to' became empty (common with trailing commas)
  filter(!is.na(to), to != "", to != "NA") %>%
  select(from, to) %>%
  distinct()

# --- 3. BUILD THE GRAPH ---
# We use an inner join on edges to ensure 'to' exists in our 'nodes' list
# This prevents "vertex not found" or NA errors if a table points to a non-existent table
valid_edges <- edges %>%
  semi_join(nodes, by = c("from" = "name")) %>%
  semi_join(nodes, by = c("to" = "name"))

crm_graph <- tbl_graph(nodes = nodes, edges = valid_edges, directed = TRUE)


# Global threshold for easy tuning (Lower = more hubs. 0.80 = Top 20%)
HUB_QUANTILE_THRESHOLD <- 0.80

# 4. Calculate Network Metrics & Edge Properties
crm_graph <- crm_graph %>%
  activate(nodes) %>%
  mutate(
    COUNT_INWARD_CONNECTIONS = centrality_degree(mode = "in"),
    COUNT_OUTWARD_CONNECTIONS = centrality_degree(mode = "out"),
    score_hub = centrality_hub(),

    # Flag HUBs based on the global variable
    IS_HUB = score_hub >= quantile(score_hub, HUB_QUANTILE_THRESHOLD, na.rm = TRUE)
  ) %>%
  # Switch focus to edges to figure out which lines connect to a HUB
  activate(edges) %>%
  mutate(
    # .N() accesses the node data from within the edge context.
    # This checks if either the source (from) OR target (to) is a HUB.
    is_hub_edge = .N()$IS_HUB[from] | .N()$IS_HUB[to]
  )




# 6. Visualize (S-Tier D3 & FiveThirtyEight Inspired Version)

p <- ggraph(crm_graph, layout = "fr") +
  geom_edge_arc(
    aes(filter = !is_hub_edge),
    strength = 0.35, alpha = 0.05, edge_width = 0.2, color = "#8b8b8b",
    show.legend = FALSE
  ) +
  geom_edge_arc(
    aes(filter = is_hub_edge),
    strength = 0.35,
    arrow = arrow(length = unit(2.5, "mm"), type = "closed", angle = 20),
    start_cap = circle(4, "mm"),
    end_cap = circle(5, "mm"),
    alpha = 0.45, edge_width = 0.8, color = "#5a5a5a",
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(color = IS_HUB, size = COUNT_INWARD_CONNECTIONS),
    alpha = 0.9, show.legend = FALSE
  ) +
  geom_node_text(
    aes(label = ifelse(IS_HUB, name, NA), fontface = "bold"),
    repel = TRUE, force = 15, box.padding = 1.5, point.padding = 1,
    max.overlaps = Inf, size = 4.5, color = "#222222",
    na.rm = TRUE, show.legend = FALSE
  ) +
  scale_color_manual(values = c("FALSE" = "#30a2da", "TRUE" = "#fc4f30")) +
  scale_size_continuous(range = c(1.5, 10)) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#F0F0F0", color = NA)
  ) +
  labs(
    title = "CRM Schema Dependency Architecture",
    subtitle = "Highlighting central data hubs (red) and their inbound directional flow."
  )

# 2. Export to Letter Sized Horizontal PDF
# US Letter Landscape is 11 x 8.5 inches
ggsave(
  filename = "CRM_Schema_Architecture.pdf",
  plot = p,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in"
)
