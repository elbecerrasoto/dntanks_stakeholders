#!/usr/bin/env Rscript
# File: main_dag.R
# Description: Data processing, graph topology generation, and data export

library(tidyverse)
library(tidygraph)

# ==========================================
# PIPELINE CONFIGURATION
# ==========================================
HUB_QUANTILE_THRESHOLD  <- 0.75
AUTH_QUANTILE_THRESHOLD <- 0.90 

# Export Flags
SAVE_NETWORK_TSV <- TRUE
TSV_OUTPUT_PATH  <- "output/tabs_profile_network_info.tsv"

# --- UTILS ---
llm_print <- function(obj, n = 10, clipboard = FALSE) {
  snip <- head(obj, n)
  if (clipboard) {
    dest <- pipe("wl-copy")
    on.exit(close(dest))
  } else { dest <- "" }
  
  if (is.data.frame(snip) || is.matrix(snip)) {
    write.table(as.data.frame(snip), file = dest, sep = "\t", 
                row.names = FALSE, quote = FALSE, na = "NA")
  } else { writeLines(as.character(snip), con = dest) }
}

# ==========================================
# 1. DATA INGESTION & NODE/EDGE PREP
# ==========================================
tables <- readxl::read_excel("output/tabs_profile.xlsx") %>%
  rename(OUTWARD_CONNECTIONS = LIST_TABLES_POINTED)

nodes <- tables %>%
  select(name = NAME_TABLE, TYPE_TABLE) %>%
  filter(!is.na(name), name != "NA", name != "") %>%
  distinct(name, .keep_all = TRUE)

edges <- tables %>%
  select(from = NAME_TABLE, to_list = OUTWARD_CONNECTIONS) %>%
  filter(!is.na(to_list), to_list != "NA", to_list != "") %>%
  separate_rows(to_list, sep = ",") %>%
  mutate(to = trimws(to_list)) %>%
  filter(!is.na(to), to != "", to != "NA") %>%
  select(from, to) %>%
  distinct()

valid_edges <- edges %>%
  semi_join(nodes, by = c("from" = "name")) %>%
  semi_join(nodes, by = c("to" = "name"))

# ==========================================
# 2. BUILD GRAPH & CALCULATE METRICS
# ==========================================
crm_graph <- tbl_graph(nodes = nodes, edges = valid_edges, directed = TRUE)

crm_graph <- crm_graph %>%
  activate(nodes) %>%
  mutate(
    COUNT_INWARD_CONNECTIONS = centrality_degree(mode = "in"),
    COUNT_OUTWARD_CONNECTIONS = centrality_degree(mode = "out"),
    score_hub = centrality_hub(),
    score_auth = centrality_authority(),
    
    IS_HUB = score_hub >= quantile(score_hub, HUB_QUANTILE_THRESHOLD, na.rm = TRUE),
    IS_AUTH = score_auth >= quantile(score_auth, AUTH_QUANTILE_THRESHOLD, na.rm = TRUE),
    
    NODE_CATEGORY = case_when(
      IS_AUTH ~ "Authority",
      IS_HUB ~ "Hub",
      TRUE ~ "Standard"
    )
  ) %>%
  activate(edges) %>%
  mutate(
    is_important_edge = .N()$NODE_CATEGORY[from] != "Standard" | .N()$NODE_CATEGORY[to] != "Standard"
  )

# ==========================================
# 3. EXPORT METRICS TO TSV
# ==========================================
if (SAVE_NETWORK_TSV) {
  # Extract the newly calculated node data from the graph
  node_metrics <- crm_graph %>% 
    activate(nodes) %>% 
    as_tibble()
  
  # Join it back to the original full dataset
  tables_extended <- tables %>%
    left_join(node_metrics, by = c("NAME_TABLE" = "name"))
    # Optional: Reorganize columns so the new network stats are up front
    # relocate(TYPE_TABLE, NODE_CATEGORY, COUNT_INWARD_CONNECTIONS, COUNT_OUTWARD_CONNECTIONS, .after = NAME_TABLE)
  #!/usr/bin/env Rscript
  # File: main_dag.R
  # Description: Data processing, graph topology generation, and data export
  
  library(tidyverse)
  library(tidygraph)
  
  # ==========================================
  # PIPELINE CONFIGURATION
  # ==========================================
  HUB_QUANTILE_THRESHOLD  <- 0.75
  AUTH_QUANTILE_THRESHOLD <- 0.90 
  
  # Export Flags
  SAVE_NETWORK_TSV <- TRUE
  TSV_OUTPUT_PATH  <- "output/tabs_profile_network_info.tsv"
  
  # --- UTILS ---
  llm_print <- function(obj, n = 10, clipboard = FALSE) {
    snip <- head(obj, n)
    if (clipboard) {
      dest <- pipe("wl-copy")
      on.exit(close(dest))
    } else { dest <- "" }
    
    if (is.data.frame(snip) || is.matrix(snip)) {
      write.table(as.data.frame(snip), file = dest, sep = "\t", 
                  row.names = FALSE, quote = FALSE, na = "NA")
    } else { writeLines(as.character(snip), con = dest) }
  }
  
  # ==========================================
  # 1. DATA INGESTION & NODE/EDGE PREP
  # ==========================================
  tables <- readxl::read_excel("output/tabs_profile.xlsx") %>%
    rename(OUTWARD_CONNECTIONS = LIST_TABLES_POINTED)
  
  nodes <- tables %>%
    select(name = NAME_TABLE, TYPE_TABLE) %>%
    filter(!is.na(name), name != "NA", name != "") %>%
    distinct(name, .keep_all = TRUE)
  
  edges <- tables %>%
    select(from = NAME_TABLE, to_list = OUTWARD_CONNECTIONS) %>%
    filter(!is.na(to_list), to_list != "NA", to_list != "") %>%
    separate_rows(to_list, sep = ",") %>%
    mutate(to = trimws(to_list)) %>%
    filter(!is.na(to), to != "", to != "NA") %>%
    select(from, to) %>%
    distinct()
  
  valid_edges <- edges %>%
    semi_join(nodes, by = c("from" = "name")) %>%
    semi_join(nodes, by = c("to" = "name"))
  
  # ==========================================
  # 2. BUILD GRAPH & CALCULATE METRICS
  # ==========================================
  crm_graph <- tbl_graph(nodes = nodes, edges = valid_edges, directed = TRUE)
  
  crm_graph <- crm_graph %>%
    activate(nodes) %>%
    mutate(
      COUNT_INWARD_CONNECTIONS = centrality_degree(mode = "in"),
      COUNT_OUTWARD_CONNECTIONS = centrality_degree(mode = "out"),
      score_hub = centrality_hub(),
      score_auth = centrality_authority(),
      
      IS_HUB = score_hub >= quantile(score_hub, HUB_QUANTILE_THRESHOLD, na.rm = TRUE),
      IS_AUTH = score_auth >= quantile(score_auth, AUTH_QUANTILE_THRESHOLD, na.rm = TRUE),
      
      NODE_CATEGORY = case_when(
        IS_AUTH ~ "Authority",
        IS_HUB ~ "Hub",
        TRUE ~ "Standard"
      )
    ) %>%
    activate(edges) %>%
    mutate(
      is_important_edge = .N()$NODE_CATEGORY[from] != "Standard" | .N()$NODE_CATEGORY[to] != "Standard"
    )
  
  # ==========================================
  # 3. EXPORT METRICS TO TSV
  # ==========================================
  if (SAVE_NETWORK_TSV) {
    # Extract the newly calculated node data from the graph
    node_metrics <- crm_graph %>% 
      activate(nodes) %>% 
      as_tibble()
    
    # Join it back to the original full dataset
    tables_extended <- tables %>%
      left_join(node_metrics, by = c("NAME_TABLE" = "name"))
      # Optional: Reorganize columns so the new network stats are up front
      # relocate(TYPE_TABLE, NODE_CATEGORY, COUNT_INWARD_CONNECTIONS, COUNT_OUTWARD_CONNECTIONS, .after = NAME_TABLE)
    
    # Ensure the output directory exists#!/usr/bin/env Rscript
    # File: main_dag.R
    # Description: Data processing, graph topology generation, and data export
    
    library(tidyverse)
    library(tidygraph)
    
    # ==========================================
    # PIPELINE CONFIGURATION
    # ==========================================
    HUB_QUANTILE_THRESHOLD  <- 0.75
    AUTH_QUANTILE_THRESHOLD <- 0.90 
    
    # Export Flags
    SAVE_NETWORK_TSV <- TRUE
    TSV_OUTPUT_PATH  <- "output/tabs_profile_network_info.tsv"
    
    # --- UTILS ---
    llm_print <- function(obj, n = 10, clipboard = FALSE) {
      snip <- head(obj, n)
      if (clipboard) {
        dest <- pipe("wl-copy")
        on.exit(close(dest))
      } else { dest <- "" }
      
      if (is.data.frame(snip) || is.matrix(snip)) {
        write.table(as.data.frame(snip), file = dest, sep = "\t", 
                    row.names = FALSE, quote = FALSE, na = "NA")
      } else { writeLines(as.character(snip), con = dest) }
    }
    
    # ==========================================
    # 1. DATA INGESTION & NODE/EDGE PREP
    # ==========================================
    tables <- readxl::read_excel("output/tabs_profile.xlsx") %>%
      rename(OUTWARD_CONNECTIONS = LIST_TABLES_POINTED)
    
    nodes <- tables %>%
      select(name = NAME_TABLE, TYPE_TABLE) %>%
      filter(!is.na(name), name != "NA", name != "") %>%
      distinct(name, .keep_all = TRUE)
    
    edges <- tables %>%
      select(from = NAME_TABLE, to_list = OUTWARD_CONNECTIONS) %>%
      filter(!is.na(to_list), to_list != "NA", to_list != "") %>%
      separate_rows(to_list, sep = ",") %>%
      mutate(to = trimws(to_list)) %>%
      filter(!is.na(to), to != "", to != "NA") %>%
      select(from, to) %>%
      distinct()
    
    valid_edges <- edges %>%
      semi_join(nodes, by = c("from" = "name")) %>%
      semi_join(nodes, by = c("to" = "name"))
    
    # ==========================================
    # 2. BUILD GRAPH & CALCULATE METRICS
    # ==========================================
    crm_graph <- tbl_graph(nodes = nodes, edges = valid_edges, directed = TRUE)
    
    crm_graph <- crm_graph %>%
      activate(nodes) %>%
      mutate(
        COUNT_INWARD_CONNECTIONS = centrality_degree(mode = "in"),
        COUNT_OUTWARD_CONNECTIONS = centrality_degree(mode = "out"),
        score_hub = centrality_hub(),
        score_auth = centrality_authority(),
        
        IS_HUB = score_hub >= quantile(score_hub, HUB_QUANTILE_THRESHOLD, na.rm = TRUE),
        IS_AUTH = score_auth >= quantile(score_auth, AUTH_QUANTILE_THRESHOLD, na.rm = TRUE),
        
        NODE_CATEGORY = case_when(
          IS_AUTH ~ "Authority",
          IS_HUB ~ "Hub",
          TRUE ~ "Standard"
        )
      ) %>%
      activate(edges) %>%
      mutate(
        is_important_edge = .N()$NODE_CATEGORY[from] != "Standard" | .N()$NODE_CATEGORY[to] != "Standard"
      )
    
    # ==========================================
    # 3. EXPORT METRICS TO TSV
    # ==========================================
    if (SAVE_NETWORK_TSV) {
      # Extract the newly calculated node data from the graph
      node_metrics <- crm_graph %>% 
        activate(nodes) %>% 
        as_tibble()
      
      # Join it back to the original full dataset
      tables_extended <- tables %>%
        left_join(node_metrics, by = c("NAME_TABLE" = "name", "TYPE_TABLE" = "TYPE_TABLE"))
        # Optional: Reorganize columns so the new network stats are up front
        # relocate(TYPE_TABLE, NODE_CATEGORY, COUNT_INWARD_CONNECTIONS, COUNT_OUTWARD_CONNECTIONS, .after = NAME_TABLE)
      
      # Ensure the output directory exists
      if (!dir.exists(dirname(TSV_OUTPUT_PATH))) {
        dir.create(dirname(TSV_OUTPUT_PATH), recursive = TRUE)
      }
      
      # Write the TSV
      readr::write_tsv(tables_extended, TSV_OUTPUT_PATH)
      cat("Success: Network metrics saved to ->", TSV_OUTPUT_PATH, "\n")
    }
    
    # ==========================================
    # 4. GENERATE VISUALIZATION
    # ==========================================
    cat("Graph computed successfully. Sourcing visual script...\n")
    source("viz_schema.R")
    if (!dir.exists(dirname(TSV_OUTPUT_PATH))) {
      dir.create(dirname(TSV_OUTPUT_PATH), recursive = TRUE)
    }
    
    # Write the TSV
    readr::write_tsv(tables_extended, TSV_OUTPUT_PATH)
    cat("Success: Network metrics saved to ->", TSV_OUTPUT_PATH, "\n")
  }
  
  # ==========================================
  # 4. GENERATE VISUALIZATION
  # ==========================================
  cat("Graph computed successfully. Sourcing visual script...\n")
  source("viz_schema.R")
  # Ensure the output directory exists
  if (!dir.exists(dirname(TSV_OUTPUT_PATH))) {
    dir.create(dirname(TSV_OUTPUT_PATH), recursive = TRUE)
  }
  
  # Write the TSV
  readr::write_tsv(tables_extended, TSV_OUTPUT_PATH)
  cat("Success: Network metrics saved to ->", TSV_OUTPUT_PATH, "\n")
}

# ==========================================
# 4. GENERATE VISUALIZATION
# ==========================================
cat("Graph computed successfully. Sourcing visual script...\n")
source("viz_schema.R")