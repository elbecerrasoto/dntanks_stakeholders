# File: viz_schema.R
# Description: S-Tier network visualization (Tri-Color, Unambiguous Pointers)

library(ggraph)
library(ggthemes)
library(showtext)

# ==========================================
# CONFIGURATION PANEL
# ==========================================

# Colors (Adding Gold for Authorities)
CLR_HUB_NODE <- "#fc4f30" # 538 Red (Hubs point outward)
CLR_AUTH_NODE <- "#e5ae38" # 538 Gold (Authorities receive inward)
CLR_NORM_NODE <- "#30a2da" # 538 Blue (Standard nodes)
CLR_IMP_EDGE <- "#2b2b2b"
CLR_NORM_EDGE <- "#8b8b8b"
CLR_BG <- "#F4F4F4"

# Node Sizes
NODE_SIZE_MIN <- 2.0
NODE_SIZE_MAX <- 13.0

# Edge Physics
EDGE_CURVE <- 0.35
EDGE_IMP_WIDTH <- 0.9
EDGE_IMP_ALPHA <- 0.65
EDGE_NORM_WIDTH <- 0.2
EDGE_NORM_ALPHA <- 0.05

# Text & Repulsion Physics
LBL_SIZE <- 3.2
LBL_COLOR <- "#4a4a4a"
REP_FORCE <- 85 # Increased to push text far from each other
REP_BOX_PAD <- 5.5 # Increased to push text far from the nodes
REP_PT_PAD <- 0.1 # CRITICAL FIX: Almost 0 so the dotted line touches the dot
LBL_POINTER_CLR <- "#999999"

# ==========================================
# 1. TYPOGRAPHY SETUP
# ==========================================
font_add_google("Montserrat", "title_font")
font_add_google("Fira Mono", "mono_font")
showtext_auto()

# ==========================================
# 2. BUILD THE PLOT
# ==========================================
p_final <- ggraph(crm_graph, layout = "fr") +

  geom_edge_arc(
    aes(filter = !is_important_edge),
    strength = EDGE_CURVE, alpha = EDGE_NORM_ALPHA,
    edge_width = EDGE_NORM_WIDTH, color = CLR_NORM_EDGE,
    show.legend = FALSE
  ) +

  geom_edge_arc(
    aes(filter = is_important_edge),
    strength = EDGE_CURVE,
    arrow = arrow(length = unit(2.5, "mm"), type = "closed", angle = 20),
    start_cap = circle(4, "mm"), end_cap = circle(5, "mm"),
    alpha = EDGE_IMP_ALPHA, edge_width = EDGE_IMP_WIDTH, color = CLR_IMP_EDGE,
    show.legend = FALSE
  ) +

  # Color mapped to our new 3-tier category
  geom_node_point(
    aes(color = NODE_CATEGORY, size = COUNT_INWARD_CONNECTIONS),
    alpha = 0.9, show.legend = FALSE
  ) +

  geom_node_text(
    aes(
      # Print if it is NOT standard (i.e., it is a Hub or Authority)
      label = ifelse(NODE_CATEGORY != "Standard", gsub("^STG_", "", name), NA)
    ),
    family = "mono_font",
    repel = TRUE,
    force = REP_FORCE,
    box.padding = REP_BOX_PAD,
    point.padding = REP_PT_PAD, # This now allows the line to touch the node
    size = LBL_SIZE,
    color = LBL_COLOR,
    max.overlaps = Inf,
    max.iter = 100000,
    min.segment.length = 0,
    segment.color = LBL_POINTER_CLR,
    segment.size = 0.45,
    segment.linetype = "dashed",
    show.legend = FALSE
  ) +

  # 3-Color scale implementation
  scale_color_manual(values = c(
    "Standard" = CLR_NORM_NODE,
    "Hub" = CLR_HUB_NODE,
    "Authority" = CLR_AUTH_NODE
  )) +

  scale_size_continuous(range = c(NODE_SIZE_MIN, NODE_SIZE_MAX)) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(family = "title_font", face = "bold", size = 22, color = "#111111"),
    plot.subtitle = element_text(family = "title_font", size = 12, color = "#555555", margin = margin(b = 20)),
    plot.background = element_rect(fill = CLR_BG, color = NA)
  ) +
  labs(
    title = "CRM SCHEMA DEPENDENCY ARCHITECTURE",
    subtitle = "Strategic Mapping Hubs (Red), Master Records (Gold), and Referential Flow"
  )

# ==========================================
# 3. EXPORT
# ==========================================
cat("Rendering PDF...\n")
ggsave(
  filename = "CRM_Architecture_STier.pdf",
  plot = p_final,
  device = cairo_pdf,
  width = 11, height = 8.5, units = "in"
)
cat("Done! Saved as CRM_Architecture_STier.pdf\n")
