#!/usr/bin/env Rscript
library(tidyverse)
library(tidygraph)
library(ggraph)

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





# 4. Calculate Network Metrics inside the Graph
crm_graph <- crm_graph %>%
  mutate(
    COUNT_INWARD_CONNECTIONS = centrality_degree(mode = "in"),
    COUNT_OUTWARD_CONNECTIONS = centrality_degree(mode = "out"),
    # Calculate Hubs and Authorities
    hub_score = centrality_hub(),
    authority_score = centrality_authority(),
    # Flag as HUB if it's in the top 5% of hub scores (adjust threshold as needed)
    IS_HUB = hub_score > quantile(hub_score, 0.95, na.rm = TRUE),
    # Optional: Betweenness to find bridge tables
    betweenness = centrality_betweenness()
  )

# 5. Extract metrics and join back to your main tibble
node_metrics <- crm_graph %>% as_tibble()

final_tables <- tables %>%
  left_join(node_metrics, by = c("NAME_TABLE" = "name"))


  
  
  # 6. Visualize the DAG (Stakeholder & D3-Inspired Version)
  
  # Load ggthemes for the FiveThirtyEight aesthetic
  # install.packages("ggthemes")
  library(ggthemes)

# 'fr' (Fruchterman-Reingold) is a force-directed layout. 
# Unlike 'sugiyama' (which draws rigid, bipartite-like layers), 
# 'fr' acts like gravity: connected nodes pull together, unlinked nodes push apart.
# This gives it that organic, clustered D3.js feel.
ggraph(crm_graph, layout = 'fr') + 
  
  # 1. ROUNDED EDGES: Swap straight lines for gentle arcs. 
  # This softens the graph and makes complex web crossings easier on the eyes.
  geom_edge_arc(
    arrow = arrow(length = unit(1.5, 'mm'), type = "closed"), 
    alpha = 0.15,            # Highly transparent edges so they don't overpower nodes
    color = "#8b8b8b",       # Neutral grey for edges
    strength = 0.15          # A subtle curve (1.0 would be a full semi-circle)
  ) +
  
  # 2. NODES: We map size to incoming connections and color to the HUB flag.
  geom_node_point(
    aes(color = IS_HUB, size = COUNT_INWARD_CONNECTIONS),
    alpha = 0.9,
    show.legend = FALSE      # Drops the legends completely (Size & Color)
  ) +
  
  # 3. TEXT & CROWD CONTROL: ggrepel handles text overlapping automatically.
  # We make HUB nodes BOLD and slightly larger so they instantly grab attention.
  geom_node_text(
    aes(
      label = name,
      fontface = ifelse(IS_HUB, "bold", "plain") # Make Hubs stand out text-wise
    ),
    repel = TRUE,            # Prevents text collision
    size = 3.5,
    color = "#333333",       # Dark grey text (easier to read than pure black)
    max.overlaps = 15,       # If an area is overwhelmingly crowded, it safely hides minor labels
    show.legend = FALSE
  ) +
  
  # 4. SCALING & COLORS: FiveThirtyEight inspired palette
  # Regular nodes are a calm blue, Hubs are an aggressive alert red/orange.
  scale_color_manual(values = c("FALSE" = "#30a2da", "TRUE" = "#fc4f30")) +
  # Force the nodes to stay within a reasonable min/max size radius
  scale_size_continuous(range = c(2, 12)) + 
  
  # 5. THEME: Apply the clean, journalistic FiveThirtyEight theme
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",              # Double-ensure no legends appear
    panel.grid = element_blank(),          # Remove background grid lines
    axis.text = element_blank(),           # Hide axis coordinates
    axis.ticks = element_blank(),          # Hide axis ticks
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "#666666", margin = margin(b = 15))
  ) +
  
  # 6. TITLES: Speak directly to the stakeholder's interests
  labs(
    title = "CRM Schema Dependency Architecture",
    subtitle = "Highlighting central data hubs (red) and their inbound dependency volume."
  )
