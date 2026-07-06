### Multiple selected rds (here maximum positive delta C-index or unchanged in the maximal number of survival metrics per cancer, nimproved_unchanged cases within the rds range df006 to df377)
# Vector of dfXXX .rds filenames from improved_unchanged_best_fullset.tsv 
# df_list_rds <- c(
#   "df377.rds", "df155.rds", "df008.rds", "df017.rds", "df368.rds",
#   "df161.rds", "df374.rds", "df160.rds", "df147.rds", "df305.rds", 
#   "df157.rds"
# )

### Multiple selected rds (here maximum positive delta C-index, nimproved case over 24 and within the rds range df054 to df377)
# df_list_rds <- c(
#   "df165.rds", "df166.rds", "df167.rds", "df174.rds", "df175.rds", "df176.rds",
#   "df183.rds", "df184.rds", "df185.rds", "df201.rds", "df202.rds", "df203.rds",
#   "df210.rds", "df211.rds", "df212.rds", "df219.rds", "df220.rds", "df221.rds",
#   "df237.rds", "df238.rds", "df239.rds", "df246.rds", "df247.rds", "df248.rds",
#   "df255.rds", "df256.rds", "df257.rds", "df309.rds", "df310.rds", "df311.rds",
#   "df318.rds", "df319.rds", "df320.rds", "df327.rds", "df328.rds", "df329.rds",
#   "df345.rds", "df346.rds", "df347.rds", "df354.rds", "df355.rds", "df356.rds",
#   "df363.rds", "df364.rds", "df365.rds"
# )


###############################################################################
# Construct full dfXXX vector (df006.rds → df377.rds)
###############################################################################

#### Plottin th whole st of dfXXX rds files
# Sequence of integers corresponding to dfXXX names
df_nums <- 6:377

# Format with zero-padding and append .rds
df_list_rds <- paste0("df", sprintf("%03d", df_nums), ".rds")

# Print in the requested format
cat("# Vector of dfXXX .rds filenames\n",
    "df_list_rds <- c(\n  \"",
    paste(df_list_rds, collapse = "\", \""),
    "\")\n",
    sep = "")

# Remove .rds extension safely
df_list <- gsub("\\.rds$", "", df_list_rds)

# Run lineage tracing on all targets
lineage_all <- trace_imputation_lineage(
  df_list,
  "imputation_block_table.tsv"
)

###############################################################################
# Combined lineage summary table for all dfXXX targets
###############################################################################

library(dplyr)
library(purrr)

lineage_summary_all <- purrr::imap_dfr(
  lineage_all,
  function(lineage_tbl, df_name) {
    
    # Ensure lineage_tbl is data frame
    lineage_tbl <- as.data.frame(lineage_tbl)
    
    # Define the REAL expected columns
    required_cols <- c(
      "step",
      "from_id",
      "to_id",
      "type",
      "block",
      "method",
      "module_function"
    )
    
    # Safety check
    missing_cols <- setdiff(required_cols, colnames(lineage_tbl))
    if (length(missing_cols) > 0) {
      stop(paste(
        "Missing expected columns in lineage table for", df_name, ":",
        paste(missing_cols, collapse = ", ")
      ))
    }
    
    # Build the combined summary for this dfXXX
    lineage_tbl %>%
      dplyr::mutate(
        target_df = df_name,
        root_df = dplyr::first(from_id),           # starting lineage df
        final_df = dplyr::last(to_id),             # should match target_df
        total_steps = max(step, na.rm = TRUE)
      ) %>%
      dplyr::select(
        target_df,
        step,
        from_id,
        to_id,
        type,
        block,
        method,
        module_function,
        root_df,
        final_df,
        total_steps
      )
  }
)

###############################################################################
# Display summary
###############################################################################

cat("\n==================== COMBINED LINEAGE SUMMARY ====================\n")
print(lineage_summary_all)

cat("\n==================== DISTINCT TARGETS ====================\n")
print(unique(lineage_summary_all$target_df))

cat("\n==================== TOTAL ROWS ====================\n")
print(nrow(lineage_summary_all))

write.csv(lineage_summary_all, "combined_lineage_summary.csv", row.names = FALSE)


###############################################################################
# Sankey Diagram of all lineage transitions
###############################################################################

library(dplyr)
library(plotly)

# lineage_summary_all must already exist

# ---------------------------------------------------------------------------
# 1. Build node list (unique df IDs)
# ---------------------------------------------------------------------------
nodes <- lineage_summary_all %>%
  select(from_id, to_id) %>%
  tidyr::pivot_longer(cols = everything(), values_to = "df_id") %>%
  distinct(df_id) %>%
  mutate(node_id = row_number() - 1)   # Plotly requires 0-based node indices

# ---------------------------------------------------------------------------
# 2. Build link list (edges)
# ---------------------------------------------------------------------------
links <- lineage_summary_all %>%
  transmute(
    source = nodes$node_id[match(from_id, nodes$df_id)],
    target = nodes$node_id[match(to_id, nodes$df_id)],
    value  = 1,   # equal weighting; can use method quality, delta gain, etc.
    label  = paste(type, method, sep = ": ")
  )

# ---------------------------------------------------------------------------
# 3. Plotly Sankey diagram
# ---------------------------------------------------------------------------
p_sankey <- plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = nodes$df_id,
    color = "#4B79A1",
    pad = 15,
    thickness = 15,
    line = list(color = "black", width = 0.5)
  ),
  link = list(
    source = links$source,
    target = links$target,
    value  = links$value,
    label  = links$label,
    color  = "rgba(200, 30, 30, 0.4)"  # translucent red links
  )
) %>%
  layout(
    title = "Sankey Diagram — Imputation Lineage Flow",
    font = list(size = 12)
  )

p_sankey


htmlwidgets::saveWidget(as_widget(p_sankey),
                        "sankey_lineage.html",
                        selfcontained = TRUE)

#####
#####
#####
#####
###############################################################################
# Dynamic Sankey Lineage Explorer — FIXED VERSION
# - Select ALL button
# - Edge labels update correctly
###############################################################################
### Notes:  rs selected to comply with these performace requirements:
### # Volcano-style plot — red label ONLY dfXXX meeting ALL conditions:
# (1) df_num between 198 and 341
# (2) mean_delta is the highest positive value within this df198–df341 range
# (3) n_improved >= 24

library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(purrr)

# lineage_summary_all must exist in global environment before running.

ui <- fluidPage(
  
  titlePanel("Dynamic Sankey — Imputation Lineage Explorer (Fixed)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # MULTI-SELECT TARGET LIST
      selectInput(
        "targets",
        "Select dfXXX Targets:",
        choices = sort(unique(lineage_summary_all$target_df)),
        selected = NULL,
        multiple = TRUE
      ),
      
      # SELECT ALL BUTTON
      actionButton("selectAll", "Select ALL",
                   style = "color: white; background-color: #1E88E5;"),
      actionButton("clearSel", "Clear",
                   style = "color: white; background-color: #D32F2F;"),
      
      br(), br(),
      
      # EDGE LABEL MODE
      selectInput(
        "label_mode",
        "Edge Label:",
        choices = c("type", "method", "block", "module_function",
                    "type:method", "type:block"),
        selected = "type:method"
      ),
      
      checkboxInput(
        "show_full_set",
        "Show Full Node Set (not only transitions)?",
        value = TRUE
      ),
      
      sliderInput(
        "opacity",
        "Link Opacity:",
        min = 0.1, max = 1.0, value = 0.5, step = 0.05
      ),
      
      sliderInput(
        "node_thick",
        "Node Thickness:",
        min = 5, max = 40, value = 15
      )
    ),
    
    mainPanel(
      width = 9,
      plotlyOutput("sankeyPlot", height = "900px")
    )
  )
)

server <- function(input, output, session) {
  
  # ---------------------------
  # SELECT ALL button behavior
  # ---------------------------
  observeEvent(input$selectAll, {
    updateSelectInput(
      session,
      "targets",
      selected = sort(unique(lineage_summary_all$target_df))
    )
  })
  
  # ---------------------------
  # CLEAR button behavior
  # ---------------------------
  observeEvent(input$clearSel, {
    updateSelectInput(session, "targets", selected = character(0))
  })
  
  # ---------------------------
  # MAIN SANKEY RENDER
  # ---------------------------
  output$sankeyPlot <- renderPlotly({
    
    req(input$targets)
    
    # Filter only selected targets
    df_sel <- lineage_summary_all %>%
      filter(target_df %in% input$targets)
    
    # Compute labels dynamically
    edge_label <- switch(
      input$label_mode,
      "type" = df_sel$type,
      "method" = df_sel$method,
      "block" = df_sel$block,
      "module_function" = df_sel$module_function,
      "type:method" = paste(df_sel$type, df_sel$method, sep = ": "),
      "type:block" = paste(df_sel$type, df_sel$block, sep = ": ")
    )
    
    # Build unique node list
    nodes <- df_sel %>%
      select(from_id, to_id) %>%
      pivot_longer(cols = everything(), values_to = "df_id") %>%
      distinct(df_id)
    
    nodes <- nodes %>%
      mutate(node_id = row_number() - 1)
    
    if (input$show_full_set) {
      extra_nodes <- lineage_summary_all %>%
        select(from_id, to_id) %>%
        pivot_longer(cols = everything(), values_to = "df_id") %>%
        distinct(df_id)
      
      nodes <- bind_rows(nodes, extra_nodes) %>%
        distinct(df_id) %>%
        mutate(node_id = row_number() - 1)
    }
    
    # Build links (edges)
    links <- df_sel %>%
      transmute(
        source = nodes$node_id[match(from_id, nodes$df_id)],
        target = nodes$node_id[match(to_id, nodes$df_id)],
        value  = 1,
        label  = edge_label
      )
    
    # PLOTLY SANKEY — FIXED EDGE LABELS
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(
        label = nodes$df_id,
        color = "steelblue",
        pad = 15,
        thickness = input$node_thick,
        line = list(color = "black", width = 0.4)
      ),
      link = list(
        source = links$source,
        target = links$target,
        value  = links$value,
        # FIXED: Hovertemplate forces dynamic edge labels to appear
        hovertemplate = paste("Transition:<br>%{label}<extra></extra>"),
        label = links$label,
        color  = paste0("rgba(200, 50, 50, ", input$opacity, ")")
      )
    ) %>%
      layout(
        title = paste("Dynamic Sankey — Target(s):",
                      paste(input$targets, collapse = ", ")),
        font = list(size = 12)
      )
  })
}

shinyApp(ui, server)
#######
#######
#######
