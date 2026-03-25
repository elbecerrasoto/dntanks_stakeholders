# File: viz_schema.R
# Description: S-Tier network visualization (Two-Page PDF, Frozen Coordinates)

library(ggraph)
library(ggthemes)
library(showtext)

# ==========================================
# CONFIGURATION PANEL
# ==========================================

# Colors - Topology (Page 1)
CLR_HUB_NODE <- "#fc4f30" # 538 Red
CLR_AUTH_NODE <- "#e5ae38" # 538 Gold
CLR_NORM_NODE <- "#30a2da" # 538 Blue

# Colors - Classification (Page 2)
CLR_CORE_NODE <- "#10a57a" # Emerald Green (Core)
CLR_MAP_NODE <- "#9b59b6" # Amethyst Purple (Mapping)

# Edge & Background Colors
CLR_IMP_EDGE <- "#2b2b2b"
CLR_NORM_EDGE <- "#8b8b8b"
CLR_BG <- "#F4F4F4"

# Physics & Aesthetics
NODE_SIZE_MIN <- 2.0
NODE_SIZE_MAX <- 13.0
EDGE_CURVE <- 0.35
EDGE_IMP_WIDTH <- 0.9
EDGE_IMP_ALPHA <- 0.65
EDGE_NORM_WIDTH <- 0.2
EDGE_NORM_ALPHA <- 0.05

# Text Physics
LBL_SIZE <- 3.2
LBL_COLOR <- "#4a4a4a"
REP_FORCE <- 85
REP_BOX_PAD <- 5.5
REP_PT_PAD <- 0.1
LBL_POINTER_CLR <- "#999999"

# ==========================================
# 1. TYPOGRAPHY SETUP
# ==========================================
font_add_google("Montserrat", "title_font")
font_add_google("Fira Mono", "mono_font")
showtext_auto()

# ==========================================
# 2. FREEZE THE LAYOUT (Crucial for Multi-Page)
# ==========================================
# We calculate the physics simulation ONCE so both pages perfectly align.
set.seed(42) # Ensures reproducibility
frozen_layout <- create_layout(crm_graph, layout = "fr")


# ==========================================
# 3. BUILD PAGE 1: TOPOLOGY
# ==========================================
p_topology <- ggraph(frozen_layout) +

  geom_edge_arc(aes(filter = !is_important_edge), strength = EDGE_CURVE, alpha = EDGE_NORM_ALPHA, edge_width = EDGE_NORM_WIDTH, color = CLR_NORM_EDGE, show.legend = FALSE) +
  geom_edge_arc(aes(filter = is_important_edge), strength = EDGE_CURVE, arrow = arrow(length = unit(2.5, "mm"), type = "closed", angle = 20), start_cap = circle(4, "mm"), end_cap = circle(5, "mm"), alpha = EDGE_IMP_ALPHA, edge_width = EDGE_IMP_WIDTH, color = CLR_IMP_EDGE, show.legend = FALSE) +

  geom_node_point(aes(color = NODE_CATEGORY, size = COUNT_INWARD_CONNECTIONS), alpha = 0.9, show.legend = FALSE) +

  geom_node_text(
    aes(label = ifelse(NODE_CATEGORY != "Standard", gsub("^STG_", "", name), NA)),
    family = "mono_font", repel = TRUE, force = REP_FORCE, box.padding = REP_BOX_PAD, point.padding = REP_PT_PAD, size = LBL_SIZE, color = LBL_COLOR, max.overlaps = Inf, max.iter = 100000, min.segment.length = 0, segment.color = LBL_POINTER_CLR, segment.size = 0.45, segment.linetype = "dashed", show.legend = FALSE
  ) +

  scale_color_manual(values = c("Standard" = CLR_NORM_NODE, "Hub" = CLR_HUB_NODE, "Authority" = CLR_AUTH_NODE)) +
  scale_size_continuous(range = c(NODE_SIZE_MIN, NODE_SIZE_MAX)) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none", panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    plot.title = element_text(family = "title_font", face = "bold", size = 22, color = "#111111"),
    plot.subtitle = element_text(family = "title_font", size = 12, color = "#555555", margin = margin(b = 20)),
    plot.background = element_rect(fill = CLR_BG, color = NA)
  ) +
  labs(title = "CRM SCHEMA DEPENDENCY ARCHITECTURE", subtitle = "Strategic Mapping Hubs (Red), Master Records (Gold), and Referential Flow")


# ==========================================
# 4. BUILD PAGE 2: CORE VS MAPPING
# ==========================================
p_classification <- ggraph(frozen_layout) +

  geom_edge_arc(aes(filter = !is_important_edge), strength = EDGE_CURVE, alpha = EDGE_NORM_ALPHA, edge_width = EDGE_NORM_WIDTH, color = CLR_NORM_EDGE, show.legend = FALSE) +
  geom_edge_arc(aes(filter = is_important_edge), strength = EDGE_CURVE, arrow = arrow(length = unit(2.5, "mm"), type = "closed", angle = 20), start_cap = circle(4, "mm"), end_cap = circle(5, "mm"), alpha = EDGE_IMP_ALPHA, edge_width = EDGE_IMP_WIDTH, color = CLR_IMP_EDGE, show.legend = FALSE) +

  # The magic change: color = TYPE_TABLE
  geom_node_point(aes(color = TYPE_TABLE, size = COUNT_INWARD_CONNECTIONS), alpha = 0.9, show.legend = FALSE) +

  # Keeping the same label logic so the text doesn't jump around between slides
  geom_node_text(
    aes(label = ifelse(NODE_CATEGORY != "Standard", gsub("^STG_", "", name), NA)),
    family = "mono_font", repel = TRUE, force = REP_FORCE, box.padding = REP_BOX_PAD, point.padding = REP_PT_PAD, size = LBL_SIZE, color = LBL_COLOR, max.overlaps = Inf, max.iter = 100000, min.segment.length = 0, segment.color = LBL_POINTER_CLR, segment.size = 0.45, segment.linetype = "dashed", show.legend = FALSE
  ) +

  # Applying the new Green/Purple palette mapping
  scale_color_manual(values = c("Core" = CLR_CORE_NODE, "Mapping" = CLR_MAP_NODE)) +
  scale_size_continuous(range = c(NODE_SIZE_MIN, NODE_SIZE_MAX)) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none", panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    plot.title = element_text(family = "title_font", face = "bold", size = 22, color = "#111111"),
    plot.subtitle = element_text(family = "title_font", size = 12, color = "#555555", margin = margin(b = 20)),
    plot.background = element_rect(fill = CLR_BG, color = NA)
  ) +
  labs(title = "CRM SCHEMA FUNCTIONAL CLASSIFICATION", subtitle = "Core Business Entities (Green) vs. Relational Mapping Tables (Purple)")


# ==========================================
# 5. EXPORT MULTI-PAGE PDF
# ==========================================
cat("Rendering Multi-Page PDF...\n")

# Open a PDF device
cairo_pdf("CRM_Architecture_STier.pdf", width = 11, height = 8.5, onefile = TRUE)

# Print pages in order
print(p_topology)
print(p_classification)

# Close the device to save the file
invisible(dev.off())

cat("Done! Saved two pages to CRM_Architecture_STier.pdf\n")
