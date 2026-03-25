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
    write.table(as.data.frame(snip), file = dest, sep = "\t", 
                row.names = FALSE, quote = FALSE, na = "NA")
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

ggraph(crm_graph, layout = 'fr') + 
  
  # --- LAYER 1: BACKGROUND EDGES (The Noise) ---
  # Faint, sweeping, no arrows. Just to provide structural context.
  geom_edge_arc(
    aes(filter = !is_hub_edge),   # Only draw non-hub connections here
    strength = 0.35,              # Increased curve for a more organic, hand-drawn feel
    alpha = 0.05,                 # Barely visible
    edge_width = 0.2,
    color = "#8b8b8b",
    show.legend = FALSE
  ) +
  
  # --- LAYER 2: FOREGROUND EDGES (The Signal) ---
  # Thicker, darker, distinctly curved, and equipped with directional arrows.
  geom_edge_arc(
    aes(filter = is_hub_edge),    # Only draw the critical Hub paths
    strength = 0.35,              # Match the sweeping curve
    arrow = arrow(length = unit(2.5, 'mm'), type = "closed", angle = 20),
    start_cap = circle(4, 'mm'),  # Prevents lines from starting inside the source node
    end_cap = circle(5, 'mm'),    # Stops the arrowhead right BEFORE the target node
    alpha = 0.45,                 # Bold enough to see, transparent enough to prevent blocking
    edge_width = 0.8,
    color = "#5a5a5a",            # Darker gray for contrast against the blue/red nodes
    show.legend = FALSE
  ) +
  
  # --- LAYER 3: THE NODES ---
  geom_node_point(
    aes(color = IS_HUB, size = COUNT_INWARD_CONNECTIONS),
    alpha = 0.9,
    show.legend = FALSE
  ) +
  
  # --- LAYER 4: THE LABELS (Extreme Repel) ---
  geom_node_text(
    aes(
      label = ifelse(IS_HUB, name, NA), 
      fontface = "bold"
    ),
    repel = TRUE,
    force = 15,                   # Multiplier for the repulsion physics (pushes labels far apart)
    box.padding = 1.5,            # Creates a larger invisible forcefield around the text box
    point.padding = 1,            # Forces the label to sit further away from its parent node
    max.overlaps = Inf,           # Forces ggrepel to NEVER hide a hub label
    size = 4.5,
    color = "#222222",
    na.rm = TRUE,
    show.legend = FALSE
  ) +
  
  # --- STYLING & THEME ---
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
    plot.background = element_rect(fill = "#F0F0F0", color = NA) # True 538 background color
  ) +
  labs(
    title = "CRM Schema Dependency Architecture",
    subtitle = "Highlighting central data hubs (red) and their inbound directional flow."
  )







