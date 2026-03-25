# File: viz_schema.R
# Description: S-Tier network visualization with parameterized aesthetics

library(ggraph)
library(ggthemes)
library(showtext)

# ==========================================
# CONFIGURATION PANEL (Tweak these values)
# ==========================================

# Colors
CLR_HUB_NODE      <- "#fc4f30"    # 538 Red
CLR_NORM_NODE     <- "#30a2da"    # 538 Blue
CLR_HUB_EDGE      <- "#2b2b2b"    # Dark slate for core topology
CLR_NORM_EDGE     <- "#8b8b8b"    # Faint gray for background noise
CLR_BG            <- "#F4F4F4"    # Canvas background

# Node Sizes
NODE_SIZE_MIN     <- 2.0
NODE_SIZE_MAX     <- 13.0

# Edge Physics
EDGE_CURVE        <- 0.35         # 0 = straight line, 1 = semi-circle
EDGE_HUB_WIDTH    <- 0.9
EDGE_HUB_ALPHA    <- 0.65
EDGE_NORM_WIDTH   <- 0.2
EDGE_NORM_ALPHA   <- 0.05

# Text & Repulsion Physics
LBL_SIZE          <- 2.8
LBL_COLOR         <- "#6b6b6b"
REP_FORCE         <- 35           # Pushes labels away from each other
REP_BOX_PAD       <- 3.0          # Pushes labels further from the nodes (lengthens the line)
REP_PT_PAD        <- 1.5          # Minimum distance before the line starts
LBL_POINTER_CLR   <- "#b0b0b0"    # Slightly darker dotted line to stand out

# ==========================================
# 1. TYPOGRAPHY SETUP
# ==========================================
font_add_google("Montserrat", "title_font")   
font_add_google("Fira Mono", "mono_font")     
showtext_auto()

# ==========================================
# 2. BUILD THE PLOT
# ==========================================
p_final <- ggraph(crm_graph, layout = 'fr') + 
  
  # LAYER 1: BACKGROUND NOISE 
  geom_edge_arc(
    aes(filter = !is_hub_edge),
    strength = EDGE_CURVE, alpha = EDGE_NORM_ALPHA, 
    edge_width = EDGE_NORM_WIDTH, color = CLR_NORM_EDGE,
    show.legend = FALSE
  ) +
  
  # LAYER 2: THE TOPOLOGY 
  geom_edge_arc(
    aes(filter = is_hub_edge),
    strength = EDGE_CURVE,
    arrow = arrow(length = unit(2.5, 'mm'), type = "closed", angle = 20),
    start_cap = circle(4, 'mm'), end_cap = circle(5, 'mm'),
    alpha = EDGE_HUB_ALPHA, edge_width = EDGE_HUB_WIDTH, color = CLR_HUB_EDGE,
    show.legend = FALSE
  ) +
  
  # LAYER 3: THE NODES 
  geom_node_point(
    aes(color = IS_HUB, size = COUNT_INWARD_CONNECTIONS),
    alpha = 0.9, show.legend = FALSE
  ) +
  
  # LAYER 4: THE LABELS (Elongated Pointers)
  geom_node_text(
    aes(label = ifelse(IS_HUB, name, NA)),
    family = "mono_font",        
    repel = TRUE, 
    force = REP_FORCE, 
    box.padding = REP_BOX_PAD, 
    point.padding = REP_PT_PAD,
    size = LBL_SIZE, color = LBL_COLOR, 
    max.overlaps = Inf,
    max.iter = 100000,                      # Don't give up on placing labels
    min.segment.length = 0,                 # ALWAYS draw the pointer line
    segment.color = LBL_POINTER_CLR, 
    segment.size = 0.4, 
    segment.linetype = "dotted", 
    show.legend = FALSE
  ) +
  
  # STYLING & THEME 
  scale_color_manual(values = c("FALSE" = CLR_NORM_NODE, "TRUE" = CLR_HUB_NODE)) +
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
    subtitle = "Strategic Data Hubs (Red) and Inbound Referential Flow"
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