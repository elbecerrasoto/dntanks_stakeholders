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

# 6. Visualize the DAG
# 'sugiyama' is a layout specifically designed to minimize edge crossings in DAGs
ggraph(crm_graph, layout = 'sugiyama') + 
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), alpha = 0.3, color = "gray50") +
  geom_node_point(aes(color = IS_HUB, size = COUNT_INWARD_CONNECTIONS)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "firebrick")) +
  theme_void() +
  labs(title = "CRM Table Dependency DAG",
       subtitle = "Red nodes indicate Hub tables; Size indicates incoming foreign keys") 