#!/usr/bin/env Rscript
# File: main_dag.R
# Description: Data processing and graph topology generation

library(tidyverse)
library(tidygraph)

# --- UTILS ---
llm_print <- function(obj, n = 10, clipboard = FALSE) {
  snip <- head(obj, n)
  if (clipboard) {
    dest <- pipe("wl-copy")
    on.exit(close(dest))
  } else {
    dest <- "" 
  }
  
  if (is.data.frame(snip) || is.matrix(snip)) {
    write.table(as.data.frame(snip), file = dest, sep = "\t", 
                row.names = FALSE, quote = FALSE, na = "NA")
  } else {
    writeLines(as.character(snip), con = dest)
  }
}

# --- DATA INGESTION ---
tables <- readxl::read_excel("output/tabs_profile.xlsx") %>%
  rename(OUTWARD_CONNECTIONS = LIST_TABLES_POINTED)

# --- 1. PREP NODES ---
nodes <- tables %>%
  select(name = NAME_TABLE) %>%
  filter(!is.na(name), name != "NA", name != "") %>%
  distinct()

# --- 2. PREP EDGES ---
edges <- tables %>%
  select(from = NAME_TABLE, to_list = OUTWARD_CONNECTIONS) %>%
  filter(!is.na(to_list), to_list != "NA", to_list != "") %>%
  separate_rows(to_list, sep = ",") %>%
  mutate(to = trimws(to_list)) %>%
  filter(!is.na(to), to != "", to != "NA") %>%
  select(from, to) %>%
  distinct()

# --- 3. BUILD THE GRAPH ---
valid_edges <- edges %>%
  semi_join(nodes, by = c("from" = "name")) %>%
  semi_join(nodes, by = c("to" = "name"))

crm_graph <- tbl_graph(nodes = nodes, edges = valid_edges, directed = TRUE)

# --- 4. NETWORK METRICS & FLAGS ---
HUB_QUANTILE_THRESHOLD <- 0.80

crm_graph <- crm_graph %>%
  activate(nodes) %>%
  mutate(
    COUNT_INWARD_CONNECTIONS = centrality_degree(mode = "in"),
    COUNT_OUTWARD_CONNECTIONS = centrality_degree(mode = "out"),
    score_hub = centrality_hub(),
    IS_HUB = score_hub >= quantile(score_hub, HUB_QUANTILE_THRESHOLD, na.rm = TRUE)
  ) %>%
  activate(edges) %>%
  mutate(
    is_hub_edge = .N()$IS_HUB[from] | .N()$IS_HUB[to]
  )

# --- 5. GENERATE VISUALIZATION ---
# Hand off the crm_graph object to the presentation script
cat("Graph computed successfully. Sourcing visual script...\n")
source("viz_schema.R")