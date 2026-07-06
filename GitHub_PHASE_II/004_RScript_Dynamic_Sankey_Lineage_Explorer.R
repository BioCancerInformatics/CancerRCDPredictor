###############################################################################
# Dynamic Sankey — Lineage Explorer with TAR + CANARY Node Annotations
#
# FIX (requested):
#   If ANY selected metric yields PHASE_III_logic ==
#     "INELIGIBLE__INSUFFICIENT_SURVIVAL_INFORMATION"
#   for a given df_id (in feas_log), that node MUST be colored RED,
#   even if TAR would otherwise mark it admissible.
#
# Truth-preserving design:
#   - Lineage edges are NEVER rewired (no stratum-dependent edges).
#   - TAR/CANARY are node attributes only.
#   - Duplicate transitions induced by stacking per-target traces are aggregated
#     into unique (from_id -> to_id) links with weight = multiplicity count.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(plotly)
  library(shiny)
  library(htmlwidgets)
})

###############################################################################
# 0) Inputs
###############################################################################
TAR_FILE   <- "improved_unchanged_best_fullset.tsv"
FEAS_FILE  <- "CoxNet_phaseII_feasibility_log.tsv"
BLOCK_FILE <- "imputation_block_table.tsv"

stopifnot(file.exists(TAR_FILE), file.exists(FEAS_FILE), file.exists(BLOCK_FILE))

###############################################################################
# 1) Build df006–df377 target list
###############################################################################
df_nums <- 6:377
df_list_rds <- paste0("df", sprintf("%03d", df_nums), ".rds")
df_list <- sub("\\.rds$", "", df_list_rds)

###############################################################################
# 2) Trace lineage (requires your existing function)
###############################################################################
stopifnot(exists("trace_imputation_lineage"))
lineage_all <- trace_imputation_lineage(df_list, BLOCK_FILE)

###############################################################################
# 3) Combined lineage summary (strict checks)
###############################################################################
lineage_summary_all <- purrr::imap_dfr(
  lineage_all,
  function(lineage_tbl, df_name) {
    
    lineage_tbl <- as.data.frame(lineage_tbl)
    
    required_cols <- c("step", "from_id", "to_id", "type", "block", "method", "module_function")
    missing_cols <- setdiff(required_cols, colnames(lineage_tbl))
    if (length(missing_cols) > 0) {
      stop(
        paste(
          "Missing expected columns in lineage table for", df_name, ":",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    lineage_tbl %>%
      mutate(
        target_df    = df_name,
        root_df      = dplyr::first(from_id),
        final_df     = dplyr::last(to_id),
        total_steps  = suppressWarnings(max(step, na.rm = TRUE))
      ) %>%
      select(
        target_df, step, from_id, to_id, type, block, method, module_function,
        root_df, final_df, total_steps
      )
  }
)

###############################################################################
# 4) Load TAR admissibility table (verified columns)
###############################################################################
tar_tbl <- read.delim(TAR_FILE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)

required_tar_cols <- c("df", "cancer_type", "metric", "category")
miss_tar <- setdiff(required_tar_cols, names(tar_tbl))
if (length(miss_tar) > 0) {
  stop("TAR file is missing required columns: ", paste(miss_tar, collapse = ", "), call. = FALSE)
}

tar_tbl <- tar_tbl %>%
  mutate(
    df_id       = sub("\\.rds$", "", df),
    cancer_type = as.character(cancer_type),
    metric      = as.character(metric),
    category    = as.character(category)
  ) %>%
  select(df_id, cancer_type, metric, category, everything())

###############################################################################
# 5) Load CANARY feasibility log (verified columns)
###############################################################################
feas_log <- read.delim(FEAS_FILE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)

required_feas_cols <- c(
  "cancer_type", "metric", "df_file", "algorithm",
  "feasibility_code", "last_reason", "PHASE_III_logic"
)
miss_feas <- setdiff(required_feas_cols, names(feas_log))
if (length(miss_feas) > 0) {
  stop("Feasibility log is missing required columns: ", paste(miss_feas, collapse = ", "), call. = FALSE)
}

feas_log <- feas_log %>%
  mutate(
    df_id            = sub("\\.rds$", "", df_file),
    cancer_type      = as.character(cancer_type),
    metric           = as.character(metric),
    algorithm        = as.character(algorithm),
    feasibility_code = as.character(feasibility_code),
    last_reason      = as.character(last_reason),
    PHASE_III_logic  = as.character(PHASE_III_logic)
  )

###############################################################################
# 6) Helpers: df/metric parsing + node annotation logic
###############################################################################
is_candidate_df <- function(df_id) {
  if (!grepl("^df\\d{3}$", df_id)) return(FALSE)
  n <- suppressWarnings(as.integer(sub("^df", "", df_id)))
  isTRUE(!is.na(n) && n >= 6 && n <= 377)
}

node_role <- function(df_id) {
  if (df_id == "df005") return("df005: fixed pre-imputation reference baseline")
  if (is_candidate_df(df_id)) return("Phase I candidate preprocessing regime (df006–df377)")
  return("Upstream lineage node (non-candidate or reference intermediate)")
}

# Single-metric TAR status (internal)
tar_status_for_stratum <- function(df_id, tar_sub) {
  if (df_id == "df005") return("Reference (df005)")
  if (!is_candidate_df(df_id)) return("Not evaluated (non-candidate)")
  hit <- tar_sub$category[match(df_id, tar_sub$df_id)]
  if (!is.na(hit)) return(hit)        # "Unchanged" or "Improved"
  return("Degrade/Not admissible")
}

# Multi-metric TAR summary (node attribute only)
tar_status_multi <- function(df_id, tar_sub_multi, metrics_sel) {
  if (df_id == "df005") {
    return(list(summary = "Reference (df005)", detail = "Reference (df005)"))
  }
  if (!is_candidate_df(df_id)) {
    return(list(summary = "Not evaluated (non-candidate)", detail = "Not evaluated (non-candidate)"))
  }
  
  per_metric <- vapply(metrics_sel, function(m) {
    tar_m <- tar_sub_multi %>% filter(metric == m)
    tar_status_for_stratum(df_id, tar_m)
  }, character(1))
  
  any_adm <- any(per_metric %in% c("Unchanged", "Improved"))
  summary <- if (any_adm) "Admissible (>=1 metric)" else "Degrade/Not admissible"
  detail  <- paste(paste0(metrics_sel, "=", per_metric), collapse = "; ")
  
  list(summary = summary, detail = detail)
}

# Single-metric CANARY (CoxNet)
canary_for_stratum <- function(df_id, feas_sub) {
  if (df_id == "df005") {
    return(list(code = NA_character_, reason = NA_character_, logic = NA_character_))
  }
  hit <- which(feas_sub$df_id == df_id)
  if (length(hit) == 0) {
    return(list(code = NA_character_, reason = NA_character_, logic = NA_character_))
  }
  
  if (length(hit) > 1) {
    i <- hit[1]
    dup_flag <- paste0("DUPLICATE_LOG_ROWS=", length(hit))
  } else {
    i <- hit[1]
    dup_flag <- NA_character_
  }
  
  reason_txt <- feas_sub$last_reason[i]
  if (!is.na(dup_flag)) reason_txt <- paste0(reason_txt, " [", dup_flag, "]")
  
  list(
    code   = feas_sub$feasibility_code[i],
    reason = reason_txt,
    logic  = feas_sub$PHASE_III_logic[i]
  )
}

# Multi-metric CANARY summary + per-metric detail + ineligibility flags
canary_multi <- function(df_id, feas_sub_multi, metrics_sel) {
  if (df_id == "df005") {
    return(list(
      code = NA_character_, reason = NA_character_, logic = NA_character_,
      detail = NA_character_,
      any_ineligible = FALSE,
      any_ineligible_insufficient = FALSE
    ))
  }
  
  per_metric <- lapply(metrics_sel, function(m) {
    feas_m <- feas_sub_multi %>% filter(metric == m, algorithm == "CoxNet")
    out <- canary_for_stratum(df_id, feas_m)
    list(metric = m, code = out$code, reason = out$reason, logic = out$logic)
  })
  
  codes   <- vapply(per_metric, function(x) x$code, character(1))
  logics  <- vapply(per_metric, function(x) x$logic, character(1))
  reasons <- vapply(per_metric, function(x) x$reason, character(1))
  
  # Per-metric tooltip detail
  detail <- vapply(per_metric, function(x) {
    paste0(
      x$metric, ": code=", ifelse(is.na(x$code), "NA", x$code),
      "; logic=", ifelse(is.na(x$logic), "NA", x$logic),
      "; reason=", ifelse(is.na(x$reason), "NA", x$reason)
    )
  }, character(1))
  detail_txt <- paste(detail, collapse = "<br>")
  
  # Summaries (kept concise)
  logic_sum <- {
    u <- unique(logics[!is.na(logics) & nzchar(logics)])
    if (length(u) == 0) NA_character_ else paste(u, collapse = " | ")
  }
  reason_sum <- {
    r <- reasons[!is.na(reasons) & nzchar(reasons)]
    if (length(r) == 0) NA_character_ else {
      if (length(r) == 1) r[1] else paste0(r[1], " [+", length(r) - 1, " more]")
    }
  }
  
  # Ineligibility flags (THIS drives color precedence)
  logics_clean <- logics[!is.na(logics) & nzchar(logics)]
  any_ineligible <- any(grepl("^INELIGIBLE", logics_clean))
  any_ineligible_insufficient <- any(logics_clean == "INELIGIBLE__INSUFFICIENT_SURVIVAL_INFORMATION")
  
  # Code summary is *not* used for color, because PHASE_III_logic is the authority for eligibility.
  code_sum <- if (length(codes[!is.na(codes) & nzchar(codes)]) == 0) NA_character_ else "See per-metric detail"
  
  list(
    code = code_sum,
    reason = reason_sum,
    logic = logic_sum,
    detail = detail_txt,
    any_ineligible = any_ineligible,
    any_ineligible_insufficient = any_ineligible_insufficient
  )
}

# FINAL node color with CANARY precedence (requested behavior)
node_color_final <- function(df_id, tar_summary, can_any_inel_insuff, can_any_inel) {
  if (df_id == "df005") return("rgba(100,149,237,0.85)")            # blue
  if (!is_candidate_df(df_id)) return("rgba(180,180,180,0.7)")      # grey
  
  # Hard override (requested): insufficient survival information => RED
  if (isTRUE(can_any_inel_insuff)) return("rgba(220,20,60,0.85)")   # red
  
  # Optional stronger safety: any INELIGIBLE => RED (keeps semantics consistent)
  if (isTRUE(can_any_inel)) return("rgba(220,20,60,0.80)")          # red
  
  # Otherwise, TAR decides: any admissible => GREEN; else RED
  if (!is.na(tar_summary) && tar_summary == "Admissible (>=1 metric)") {
    return("rgba(60,179,113,0.85)")                                 # green
  }
  return("rgba(220,20,60,0.75)")                                    # red
}

parse_df_targets <- function(text, valid_choices) {
  if (is.null(text) || !nzchar(trimws(text))) return(character(0))
  x <- tolower(text)
  x <- gsub("[,;\\t\\n\\r]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  hits <- regmatches(x, gregexpr("\\bdf\\s*\\d{1,3}(?:\\.rds)?\\b", x, perl = TRUE))[[1]]
  if (length(hits) == 0) return(character(0))
  
  canon <- vapply(hits, function(tok) {
    tok <- gsub("\\.rds$", "", tok)
    tok <- gsub("\\s+", "", tok)
    num <- suppressWarnings(as.integer(sub("^df", "", tok)))
    if (is.na(num)) return(NA_character_)
    paste0("df", sprintf("%03d", num))
  }, character(1))
  
  canon <- canon[!is.na(canon)]
  canon <- unique(canon)
  intersect(canon, valid_choices)
}

parse_metrics <- function(text, valid_choices) {
  if (is.null(text) || !nzchar(trimws(text))) return(character(0))
  x <- toupper(text)
  x <- gsub("[,;\\t\\n\\r]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  toks <- unlist(strsplit(x, " ", fixed = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  toks <- unique(toks)
  intersect(toks, valid_choices)
}

###############################################################################
# 7) Static Sankey (optional; kept minimal; single default metric)
###############################################################################
default_ct <- tar_tbl$cancer_type[1]
default_m  <- tar_tbl$metric[1]

tar_sub_default <- tar_tbl %>% filter(cancer_type == default_ct, metric == default_m)
feas_sub_default <- feas_log %>%
  filter(cancer_type == default_ct, metric == default_m, algorithm == "CoxNet")

nodes_static <- lineage_summary_all %>%
  select(from_id, to_id) %>%
  pivot_longer(cols = everything(), values_to = "df_id") %>%
  distinct(df_id) %>%
  mutate(node_id = row_number() - 1)

node_role_txt <- vapply(nodes_static$df_id, node_role, character(1))
node_tar_status <- vapply(nodes_static$df_id, tar_status_for_stratum, character(1), tar_sub = tar_sub_default)

canary_list <- lapply(nodes_static$df_id, canary_for_stratum, feas_sub = feas_sub_default)
node_canary_code   <- vapply(canary_list, `[[`, character(1), "code")
node_canary_reason <- vapply(canary_list, `[[`, character(1), "reason")
node_canary_logic  <- vapply(canary_list, `[[`, character(1), "logic")

# Static coloring: conservative override if logic indicates ineligible insufficient
node_colors_static <- vapply(seq_along(nodes_static$df_id), function(i) {
  df_id <- nodes_static$df_id[i]
  logic <- node_canary_logic[i]
  inel_insuff <- isTRUE(!is.na(logic) && logic == "INELIGIBLE__INSUFFICIENT_SURVIVAL_INFORMATION")
  inel_any <- isTRUE(!is.na(logic) && grepl("^INELIGIBLE", logic))
  tar_sum <- if (node_tar_status[i] %in% c("Unchanged", "Improved")) "Admissible (>=1 metric)" else node_tar_status[i]
  node_color_final(df_id, tar_sum, inel_insuff, inel_any)
}, character(1))

links_static <- lineage_summary_all %>%
  mutate(label = paste(type, method, sep = ": ")) %>%
  count(from_id, to_id, label, name = "value") %>%
  transmute(
    source = nodes_static$node_id[match(from_id, nodes_static$df_id)],
    target = nodes_static$node_id[match(to_id, nodes_static$df_id)],
    value  = value,
    label  = label
  )

p_sankey_static <- plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = nodes_static$df_id,
    color = node_colors_static,
    pad = 15,
    thickness = 15,
    line = list(color = "black", width = 0.5),
    customdata = cbind(node_role_txt, node_tar_status, node_canary_code, node_canary_reason, node_canary_logic),
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "Role: %{customdata[0]}<br>",
      "TAR status (", default_ct, "–", default_m, "): %{customdata[1]}<br>",
      "CANARY feasibility_code: %{customdata[2]}<br>",
      "CANARY last_reason: %{customdata[3]}<br>",
      "PHASE_III_logic: %{customdata[4]}<extra></extra>"
    )
  ),
  link = list(
    source = links_static$source,
    target = links_static$target,
    value  = links_static$value,
    label  = links_static$label,
    hovertemplate = "Transition:<br>%{label}<br>Multiplicity: %{value}<extra></extra>",
    color  = "rgba(200, 30, 30, 0.35)"
  )
) %>%
  layout(
    title = paste0("Sankey — Lineage (annotated) | Default stratum: ", default_ct, "–", default_m),
    font = list(size = 12)
  ) %>%
  config(
    toImageButtonOptions = list(
      format = "png",
      filename = "sankey_lineage_annotated_default_hi_res",
      height = 2160,
      width = 3840,
      scale = 4
    )
  )

p_sankey_static
htmlwidgets::saveWidget(as_widget(p_sankey_static), "sankey_lineage_annotated_default.html", selfcontained = TRUE)



###############################################################################
# 8) Shiny app (multi-metric + df text selection)
###############################################################################
all_targets <- sort(unique(lineage_summary_all$target_df))
all_metrics <- sort(unique(tar_tbl$metric))

ui <- fluidPage(
  titlePanel("Dynamic Sankey — Lineage Explorer with TAR + CANARY Node Annotations"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput(
        "ctab",
        "Cancer type (cancer_type):",
        choices = sort(unique(tar_tbl$cancer_type)),
        selected = default_ct
      ),
      
      tags$div(
        tags$label("Quick select metrics (space-separated):"),
        textAreaInput(
          "metrics_text",
          label = NULL,
          placeholder = "Example: OS DSS DFI PFI",
          rows = 2,
          width = "100%"
        ),
        actionButton(
          "applyTextMetrics",
          "Apply metrics text",
          style = "color: white; background-color: #2E7D32;"
        ),
        tags$small(
          paste0("Valid metrics: ", paste(all_metrics, collapse = ", "), ". (Selection capped at 4)"),
          style = "display:block; margin-top:6px; color:#555;"
        )
      ),
      
      br(),
      
      selectInput(
        "metrics",
        "Select metrics (1–4 endpoints):",
        choices = all_metrics,
        selected = default_m,
        multiple = TRUE
      ),
      
      br(),
      
      tags$div(
        tags$label("Quick select targets (space-separated df IDs):"),
        textAreaInput(
          "targets_text",
          label = NULL,
          placeholder = "Example: df008 df017 df161 df377",
          rows = 2,
          width = "100%"
        ),
        actionButton(
          "applyTextTargets",
          "Apply df text selection",
          style = "color: white; background-color: #6A1B9A;"
        ),
        tags$small(
          "Accepted formats: df008 df17 df 377 df377.rds (separators: space/comma/semicolon).",
          style = "display:block; margin-top:6px; color:#555;"
        )
      ),
      
      br(),
      
      selectInput(
        "targets",
        "Select dfXXX Targets (final_df):",
        choices = all_targets,
        selected = NULL,
        multiple = TRUE
      ),
      actionButton("selectAll", "Select ALL",
                   style = "color: white; background-color: #1E88E5;"),
      actionButton("clearSel", "Clear",
                   style = "color: white; background-color: #D32F2F;"),
      
      br(), br(),
      
      selectInput(
        "label_mode",
        "Edge Label:",
        choices = c("type", "method", "block", "module_function",
                    "type:method", "type:block"),
        selected = "type:method"
      ),
      
      checkboxInput(
        "show_full_set",
        "Show full node universe (not only selected transitions)?",
        value = TRUE
      ),
      
      sliderInput(
        "opacity",
        "Link opacity:",
        min = 0.1, max = 1.0, value = 0.5, step = 0.05
      ),
      
      sliderInput(
        "node_thick",
        "Node thickness:",
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
  
  observeEvent(input$selectAll, {
    updateSelectInput(session, "targets", selected = all_targets)
  })
  
  observeEvent(input$clearSel, {
    updateSelectInput(session, "targets", selected = character(0))
    updateTextAreaInput(session, "targets_text", value = "")
    updateTextAreaInput(session, "metrics_text", value = "")
    updateSelectInput(session, "metrics", selected = default_m)
  })
  
  observeEvent(input$applyTextTargets, {
    parsed <- parse_df_targets(input$targets_text, valid_choices = all_targets)
    updateSelectInput(session, "targets", selected = parsed)
  })
  
  observeEvent(input$applyTextMetrics, {
    parsed <- parse_metrics(input$metrics_text, valid_choices = all_metrics)
    if (length(parsed) > 4) parsed <- parsed[1:4]
    updateSelectInput(session, "metrics", selected = parsed)
  })
  
  observeEvent(input$metrics, {
    sel <- input$metrics
    if (length(sel) > 4) {
      sel <- sel[1:4]
      updateSelectInput(session, "metrics", selected = sel)
    }
  }, ignoreInit = TRUE)
  
  output$sankeyPlot <- renderPlotly({
    
    req(input$targets, input$ctab, input$metrics)
    
    metrics_sel <- unique(as.character(input$metrics))
    if (length(metrics_sel) == 0) validate("Select at least one metric.")
    if (length(metrics_sel) > 4) metrics_sel <- metrics_sel[1:4]
    
    tar_sub_multi <- tar_tbl %>%
      filter(cancer_type == input$ctab, metric %in% metrics_sel)
    
    feas_sub_multi <- feas_log %>%
      filter(cancer_type == input$ctab, metric %in% metrics_sel, algorithm == "CoxNet")
    
    df_sel <- lineage_summary_all %>%
      filter(target_df %in% input$targets)
    
    edge_label <- switch(
      input$label_mode,
      "type" = df_sel$type,
      "method" = df_sel$method,
      "block" = df_sel$block,
      "module_function" = df_sel$module_function,
      "type:method" = paste(df_sel$type, df_sel$method, sep = ": "),
      "type:block" = paste(df_sel$type, df_sel$block, sep = ": ")
    )
    
    nodes <- df_sel %>%
      select(from_id, to_id) %>%
      pivot_longer(cols = everything(), values_to = "df_id") %>%
      distinct(df_id)
    
    if (isTRUE(input$show_full_set)) {
      extra_nodes <- lineage_summary_all %>%
        select(from_id, to_id) %>%
        pivot_longer(cols = everything(), values_to = "df_id") %>%
        distinct(df_id)
      nodes <- bind_rows(nodes, extra_nodes) %>% distinct(df_id)
    }
    
    nodes <- nodes %>% mutate(node_id = row_number() - 1)
    
    node_role_txt <- vapply(nodes$df_id, node_role, character(1))
    
    tar_multi_list <- lapply(nodes$df_id, tar_status_multi, tar_sub_multi = tar_sub_multi, metrics_sel = metrics_sel)
    node_tar_summary <- vapply(tar_multi_list, `[[`, character(1), "summary")
    node_tar_detail  <- vapply(tar_multi_list, `[[`, character(1), "detail")
    
    canary_multi_list <- lapply(nodes$df_id, canary_multi, feas_sub_multi = feas_sub_multi, metrics_sel = metrics_sel)
    node_canary_code    <- vapply(canary_multi_list, `[[`, character(1), "code")
    node_canary_reason  <- vapply(canary_multi_list, `[[`, character(1), "reason")
    node_canary_logic   <- vapply(canary_multi_list, `[[`, character(1), "logic")
    node_canary_detail  <- vapply(canary_multi_list, `[[`, character(1), "detail")
    node_any_inel       <- vapply(canary_multi_list, `[[`, logical(1),  "any_ineligible")
    node_any_inel_insuf <- vapply(canary_multi_list, `[[`, logical(1),  "any_ineligible_insufficient")
    
    # FIXED COLORING: CANARY ineligibility overrides TAR admissibility
    node_colors <- vapply(seq_along(nodes$df_id), function(i) {
      node_color_final(
        df_id = nodes$df_id[i],
        tar_summary = node_tar_summary[i],
        can_any_inel_insuff = node_any_inel_insuf[i],
        can_any_inel = node_any_inel[i]
      )
    }, character(1))
    
    links <- df_sel %>%
      mutate(label = edge_label) %>%
      count(from_id, to_id, label, name = "value") %>%
      transmute(
        source = nodes$node_id[match(from_id, nodes$df_id)],
        target = nodes$node_id[match(to_id, nodes$df_id)],
        value  = value,
        label  = label
      )
    
    metrics_txt <- paste(metrics_sel, collapse = ",")
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(
        label = nodes$df_id,
        color = node_colors,
        pad = 15,
        thickness = input$node_thick,
        line = list(color = "black", width = 0.4),
        # 0 role, 1 TAR_summary, 2 TAR_detail,
        # 3 CANARY_code, 4 CANARY_reason, 5 CANARY_logic, 6 CANARY_detail
        customdata = cbind(
          node_role_txt,
          node_tar_summary,
          node_tar_detail,
          node_canary_code,
          node_canary_reason,
          node_canary_logic,
          node_canary_detail
        ),
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          "Role: %{customdata[0]}<br>",
          "TAR summary (", input$ctab, " | metrics=", metrics_txt, "): %{customdata[1]}<br>",
          "TAR detail: %{customdata[2]}<br>",
          "CANARY PHASE_III_logic (unique): %{customdata[5]}<br>",
          "CANARY reason (summary): %{customdata[4]}<br>",
          "<br><b>CANARY per-metric detail</b><br>%{customdata[6]}<extra></extra>"
        )
      ),
      link = list(
        source = links$source,
        target = links$target,
        value  = links$value,
        label  = links$label,
        hovertemplate = "Transition:<br>%{label}<br>Multiplicity: %{value}<extra></extra>",
        color  = paste0("rgba(200, 50, 50, ", input$opacity, ")")
      )
    ) %>%
      layout(
        title = paste0(
          "Dynamic Sankey — Lineage + TAR/CANARY annotations (node-only) | ",
          "Cancer: ", input$ctab, " | Metrics: ", metrics_txt, " | Targets: ",
          paste(input$targets, collapse = ", ")
        ),
        font = list(size = 12)
      )
  })
}

shinyApp(ui, server)

### Test eligibles : df008 df017 df147 df155 df157 df160 df161 df305 df368 df374 df377




###############################################################################
# PIPELINE SCHEMATIC (LOCKED, READABLE) — VERTICAL LAYOUT + FIXED DIMENSIONS
#
# Fixes applied (per your report):
#   1) Layout changed from horizontal (LR) to vertical (TB) to improve readability.
#   2) Graph canvas dimensions controlled (Graphviz size + ratio + spacing).
#   3) HTML output wrapped with explicit width/height so it does not render “huge”.
#   4) SVG/PDF export uses explicit width/height to avoid absurd zoom defaults.
#
# Outputs:
#   - pipeline_overview.html  (fixed-size, readable)
#   - pipeline_overview.svg   (vector)
#   - pipeline_overview.pdf   (vector, page-sized)
###############################################################################
###############################################################################
# FIXES:
# (1) Node labels not fitting: increase available box width + allow wrapping,
#     slightly reduce font, and enforce generous node margins.
# (2) Export TIFF at 600 dpi (journal standard): render SVG -> PNG bitmap at
#     exact pixel dimensions -> write TIFF with 600 dpi metadata.
#
# Outputs (per version stem):
#   *.html  (viewport-limited)
#   *.svg   (vector)
#   *.pdf   (vector page-sized)
#   *.tiff  (600 dpi, page-sized)
###############################################################################

suppressPackageStartupMessages({
  library(DiagrammeR)
  library(htmlwidgets)
  library(htmltools)
  library(DiagrammeRsvg)
  library(rsvg)
  library(magick)
})

###############################################################################
# 1) DOT templates (JOURNAL + TALK) with improved label fit
###############################################################################
dot_pipeline_JOURNAL <- "
digraph pipeline {

  graph [
    rankdir = TB,
    bgcolor = \"white\",
    labelloc = \"t\",
    fontsize = 18,
    fontname = \"Helvetica\",
    label = \"Analytical pipeline overview (Phases I–III)\",
    size = \"8.5,11\",
    ratio = \"compress\",
    nodesep = 0.40,
    ranksep = 0.65
  ];

  node [
    shape = box,
    style = \"rounded,filled\",
    fontname = \"Helvetica\",
    fontsize = 10,
    color = \"#2B2B2B\",
    fillcolor = \"#F7F7F7\",
    margin = \"0.22,0.14\",
    // Give Graphviz more horizontal room for long labels:
    width = 3.2
  ];

  edge [
    color = \"#2B2B2B\",
    penwidth = 1.2,
    arrowsize = 0.8,
    fontname = \"Helvetica\",
    fontsize = 10
  ];

  subgraph cluster_inputs {
    label = \"Inputs\";
    fontsize = 12;
    color = \"#B0B0B0\";
    style = \"rounded\";

    raw_data   [label = \"Raw multi-omic + clinical\\ndata\", fillcolor = \"#FFFFFF\"];
    endpoints  [label = \"Survival endpoints\\n(OS, DSS, DFI, PFI, …)\", fillcolor = \"#FFFFFF\"];
    strata     [label = \"Strata\\n(cancer_type × metric)\", fillcolor = \"#FFFFFF\"];
  }

  subgraph cluster_phase1 {
    label = \"Phase I — Regime generation\";
    fontsize = 12;
    color = \"#B0B0B0\";
    style = \"rounded\";

    p1_ops   [label = \"Deterministic preprocessing\\n(regime enumeration)\", fillcolor = \"#E8F0FE\"];
    df005    [label = \"df005\\nReference baseline\", fillcolor = \"#E8F0FE\"];
    dfXXX    [label = \"df006–df377\\nCandidate regimes\", fillcolor = \"#E8F0FE\"];
    block_tbl [label = \"Imputation block table\\n(lineage definition)\", fillcolor = \"#FFFFFF\"];
  }

  subgraph cluster_phase2 {
    label = \"Phase II — Diagnostic gating\";
    fontsize = 12;
    color = \"#B0B0B0\";
    style = \"rounded\";

    tar_gate     [label = \"TAR gate\\n(eligibility)\", fillcolor = \"#FFF4E5\"];
    canary_gate  [label = \"CANARY gate\\n(feasibility)\", fillcolor = \"#FFF4E5\"];
    routing_tbl  [label = \"Routing table\\n(TAR-admissible regimes)\", fillcolor = \"#FFF4E5\"];
    feas_log     [label = \"Feasibility log\\n(PHASE_III_logic, μ, reasons)\", fillcolor = \"#FFF4E5\"];
  }

  subgraph cluster_phase3 {
    label = \"Phase III — Supervised prediction\";
    fontsize = 12;
    color = \"#B0B0B0\";
    style = \"rounded\";

    p3_models    [label = \"Supervised survival modeling\\n(TAR-admissible only)\", fillcolor = \"#E6F4EA\"];
    ph_models    [label = \"PH-compatible models\\n(Cox PH / CoxNet)\", fillcolor = \"#E6F4EA\"];
    nonph_models [label = \"Non-PH / nonlinear models\\n(RSF, boosting, MTLR, …)\", fillcolor = \"#E6F4EA\"];
    outputs      [label = \"Outputs\\n(predictions, risk scores)\", fillcolor = \"#E6F4EA\"];
  }

  raw_data  -> strata   [label = \"define\"];
  endpoints -> strata   [label = \"scope\"];
  strata   -> p1_ops    [label = \"fixed cohort\"];
  p1_ops   -> df005     [label = \"reference\"];
  p1_ops   -> dfXXX     [label = \"materialize\"];
  block_tbl -> dfXXX    [style = \"dashed\", label = \"lineage\"];

  dfXXX -> tar_gate     [label = \"assess\"];
  dfXXX -> canary_gate  [label = \"diagnose\"];
  tar_gate -> routing_tbl   [label = \"admit\"];
  canary_gate -> feas_log   [label = \"log\"];
  tar_gate -> canary_gate   [style = \"dashed\", label = \"context\"];

  routing_tbl -> p3_models  [label = \"restrict\"];
  feas_log    -> p3_models  [style = \"dashed\", label = \"condition\"];
  df005       -> p3_models  [style = \"dashed\", label = \"baseline\\n(opt.)\"];
  dfXXX       -> p3_models  [style = \"dashed\", label = \"use\\nas-is\"];
  p3_models -> ph_models    [label = \"PH supported\"];
  p3_models -> nonph_models [label = \"PH unsupported\"];
  ph_models -> outputs;
  nonph_models -> outputs;

  subgraph cluster_legend {
    label = \"Legend\";
    fontsize = 12;
    color = \"#B0B0B0\";
    style = \"rounded\";

    leg1 [label = \"Solid: data/artifact flow\", shape = box, fillcolor = \"#FFFFFF\"];
    leg2 [label = \"Dashed: conditioning\\n(no data alteration)\", shape = box, fillcolor = \"#FFFFFF\"];
  }
}
"

dot_pipeline_TALK <- "
digraph pipeline {

  graph [
    rankdir = TB,
    bgcolor = \"white\",
    labelloc = \"t\",
    fontsize = 20,
    fontname = \"Helvetica\",
    label = \"Analytical pipeline overview (Phases I–III)\",
    size = \"11,8.5\",
    ratio = \"compress\",
    nodesep = 0.45,
    ranksep = 0.75
  ];

  node [
    shape = box,
    style = \"rounded,filled\",
    fontname = \"Helvetica\",
    fontsize = 12,
    color = \"#2B2B2B\",
    fillcolor = \"#F7F7F7\",
    margin = \"0.24,0.16\",
    width = 3.6
  ];

  edge [
    color = \"#2B2B2B\",
    penwidth = 1.3,
    arrowsize = 0.85,
    fontname = \"Helvetica\",
    fontsize = 11
  ];

  subgraph cluster_inputs {
    label = \"Inputs\";
    fontsize = 13;
    color = \"#B0B0B0\";
    style = \"rounded\";

    raw_data  [label = \"Raw multi-omic + clinical data\\n(per cancer type)\", fillcolor = \"#FFFFFF\"];
    endpoints [label = \"Survival endpoints\\n(OS, DSS, DFI, PFI, …)\", fillcolor = \"#FFFFFF\"];
    strata    [label = \"Stratification\\n(cancer_type × metric)\\n(endpoint-scoped cohorts)\", fillcolor = \"#FFFFFF\"];
  }

  subgraph cluster_phase1 {
    label = \"Phase I — Preprocessing regime generation\";
    fontsize = 13;
    color = \"#B0B0B0\";
    style = \"rounded\";

    p1_ops    [label = \"Standard transformations\\n(harmonization, encoding, imputation, …)\\nDeterministic regime enumeration\", fillcolor = \"#E8F0FE\"];
    df005     [label = \"df005\\nFixed reference baseline\", fillcolor = \"#E8F0FE\"];
    dfXXX     [label = \"df006–df377\\nCandidate preprocessing regimes\\n(materialized artifacts)\", fillcolor = \"#E8F0FE\"];
    block_tbl [label = \"Imputation block table\\n(method/type/module_function)\\n(lineage definition)\", fillcolor = \"#FFFFFF\"];
  }

  subgraph cluster_phase2 {
    label = \"Phase II — Eligibility + feasibility gating (diagnostic)\";
    fontsize = 13;
    color = \"#B0B0B0\";
    style = \"rounded\";

    tar_gate    [label = \"TAR gate\\nSurvival invariance / non-degradation\\n(eligibility routing)\", fillcolor = \"#FFF4E5\"];
    canary_gate [label = \"CANARY gate\\nCoxNet feasibility diagnostics\\n(PHASE_III_logic)\", fillcolor = \"#FFF4E5\"];
    routing_tbl [label = \"Routing table\\n(TAR-admissible regime set)\", fillcolor = \"#FFF4E5\"];
    feas_log    [label = \"Feasibility log\\n(μ-ladder, retention, reasons)\", fillcolor = \"#FFF4E5\"];
  }

  subgraph cluster_phase3 {
    label = \"Phase III — Supervised survival prediction (inference)\";
    fontsize = 13;
    color = \"#B0B0B0\";
    style = \"rounded\";

    p3_models    [label = \"Supervised survival modeling\\n(only TAR-admissible regimes)\\nModel class conditioned on Phase II diagnostics\", fillcolor = \"#E6F4EA\"];
    ph_models    [label = \"PH-compatible models\\n(Cox PH / CoxNet when supported)\", fillcolor = \"#E6F4EA\"];
    nonph_models [label = \"Non-PH / nonlinear models\\n(RSF, boosting survival, MTLR, …)\\nwhen PH is unsupported\", fillcolor = \"#E6F4EA\"];
    outputs      [label = \"Outputs\\nRisk scores, survival predictions\\n(stratum-scoped)\", fillcolor = \"#E6F4EA\"];
  }

  raw_data  -> strata   [label = \"define strata\"];
  endpoints -> strata   [label = \"endpoint scoping\"];
  strata   -> p1_ops    [label = \"fixed cohort per stratum\"];
  p1_ops   -> df005     [label = \"reference\"];
  p1_ops   -> dfXXX     [label = \"enumerate regimes\"];
  block_tbl -> dfXXX    [style = \"dashed\", label = \"lineage map\"];
  dfXXX -> tar_gate     [label = \"evaluate regime\"];
  dfXXX -> canary_gate  [label = \"structural feasibility\"];
  tar_gate -> routing_tbl   [label = \"admissible\"];
  canary_gate -> feas_log   [label = \"logged\"];
  tar_gate -> canary_gate   [style = \"dashed\", label = \"gating context\"];
  routing_tbl -> p3_models  [label = \"restrict\"];
  feas_log    -> p3_models  [style = \"dashed\", label = \"condition\"];
  df005       -> p3_models  [style = \"dashed\", label = \"baseline (optional)\"];
  dfXXX       -> p3_models  [style = \"dashed\", label = \"used as-is\"];
  p3_models -> ph_models    [label = \"PH supported\"];
  p3_models -> nonph_models [label = \"PH unsupported\"];
  ph_models -> outputs;
  nonph_models -> outputs;

  subgraph cluster_legend {
    label = \"Legend\";
    fontsize = 13;
    color = \"#B0B0B0\";
    style = \"rounded\";

    leg1 [label = \"Solid arrows: data/artifact flow\", shape = box, fillcolor = \"#FFFFFF\"];
    leg2 [label = \"Dashed arrows: constraints/conditioning\\n(no data alteration)\", shape = box, fillcolor = \"#FFFFFF\"];
  }
}
"

###############################################################################
# 2) Export helper: HTML + SVG + PDF + TIFF(600 dpi)
###############################################################################
export_diagram_all <- function(dot, stem,
                               html_w_px = 1000, html_h_px = 1200,
                               pdf_w_in = 8.5, pdf_h_in = 11,
                               tiff_dpi = 600) {
  
  stopifnot(is.character(dot), length(dot) == 1L)
  stopifnot(is.character(stem), length(stem) == 1L)
  
  g <- DiagrammeR::grViz(dot)
  
  # ----------------------------
  # HTML (viewport-limited)
  # ----------------------------
  g_html <- htmlwidgets::prependContent(
    g,
    htmltools::tags$div(
      style = sprintf(
        "width:%dpx; height:%dpx; overflow:auto; border:1px solid #DDD; padding:8px; background:white;",
        html_w_px, html_h_px
      )
    )
  )
  htmlwidgets::saveWidget(g_html, file = paste0(stem, ".html"), selfcontained = TRUE)
  
  # ----------------------------
  # SVG (vector) — write to disk
  # ----------------------------
  svg_txt  <- DiagrammeRsvg::export_svg(g)
  svg_path <- paste0(stem, ".svg")
  writeLines(svg_txt, svg_path)
  
  if (!file.exists(svg_path) || file.info(svg_path)$size < 200) {
    stop("SVG export failed or file is suspiciously small: ", svg_path, call. = FALSE)
  }
  
  # ----------------------------
  # PDF (vector, page-sized) — via rsvg (no Inkscape)
  # ----------------------------
  rsvg::rsvg_pdf(
    charToRaw(svg_txt),
    file   = paste0(stem, ".pdf"),
    width  = pdf_w_in,
    height = pdf_h_in
  )
  
  # ----------------------------
  # TIFF @ 600 dpi — rsvg rasterization + magick write (NO SVG delegates)
  # ----------------------------
  # Deterministic pixel target
  w_px <- as.integer(round(pdf_w_in * tiff_dpi))
  h_px <- as.integer(round(pdf_h_in * tiff_dpi))
  
  # Rasterize SVG -> PNG bytes using rsvg (does NOT require Inkscape)
  png_raw <- rsvg::rsvg(
    charToRaw(svg_txt),
    width  = w_px,
    height = h_px
  )
  
  # Read PNG bytes with magick (this path does NOT call Inkscape)
  img <- magick::image_read(png_raw)
  
  # Force a white background (avoids "blank" in TIFF viewers due to alpha)
  bg  <- magick::image_blank(w_px, h_px, color = "white")
  img <- magick::image_composite(bg, img, operator = "over")
  img <- magick::image_flatten(img)
  
  # Debug PNG for sanity-check
  magick::image_write(img, path = paste0(stem, "_DEBUG.png"), format = "png")
  
  # Write TIFF with LZW + embedded DPI metadata
  magick::image_write(
    img,
    path = paste0(stem, ".tiff"),
    format = "tiff",
    compression = "lzw",
    density = paste0(tiff_dpi, "x", tiff_dpi)
  )
  
  message("Saved: ", stem, ".html / .svg / .pdf / _DEBUG.png / .tiff")
  invisible(TRUE)
}
###############################################################################
# 3) Produce BOTH versions with TIFF 600 dpi
###############################################################################
export_diagram_all(
  dot = dot_pipeline_JOURNAL,
  stem = "pipeline_overview_JOURNAL",
  html_w_px = 950, html_h_px = 1150,
  pdf_w_in = 8.5, pdf_h_in = 11,
  tiff_dpi = 600
)

export_diagram_all(
  dot = dot_pipeline_TALK,
  stem = "pipeline_overview_TALK",
  html_w_px = 1200, html_h_px = 900,
  pdf_w_in = 11, pdf_h_in = 8.5,
  tiff_dpi = 600
)

export_diagram_all(
  dot = dot_pipeline_JOURNAL,
  stem = "pipeline_overview_JOURNAL",
  html_w_px = 950,
  html_h_px = 1150,
  pdf_w_in  = 8.5,
  pdf_h_in  = 11,
  tiff_dpi  = 600
)
message("Generated: JOURNAL and TALK versions as HTML/SVG/PDF/TIFF(600 dpi).")


