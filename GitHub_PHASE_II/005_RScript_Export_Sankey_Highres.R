###############################################################################
# RScript_Export_Sankey_HighRes.R
# Standalone script to export high-resolution (600 DPI) static figures of 
# the Dynamic Sankey Plot using the Python/Kaleido bridge.
# THIS SCRIPT DOES NOT ALTER THE ORIGINAL APP CODE.
###############################################################################

setwd("D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(plotly)
  library(reticulate)
  library(magick)
})

# 1) Bind Python to ensure Kaleido is used
tryCatch({
  reticulate::py_run_string("import kaleido")
  message("Successfully bound Python/Kaleido engine.")
}, error = function(e) {
  warning("Kaleido might not be perfectly bound in Python. Attempting export anyway...")
})

# 2) Load Pre-requisite data and environment
load("workapace_phase_III.RData")
source("../PHASE_III/Rscript_IMPUTATION LINEAGE ENGINE.R")

TAR_FILE   <- "improved_unchanged_best_fullset.tsv"
FEAS_FILE  <- "CoxNet_phaseII_feasibility_log.tsv"
BLOCK_FILE <- "imputation_block_table.tsv"

df_nums <- 6:377
df_list_rds <- paste0("df", sprintf("%03d", df_nums), ".rds")
df_list <- sub("\\.rds$", "", df_list_rds)

lineage_all <- trace_imputation_lineage(df_list, BLOCK_FILE)

lineage_summary_all <- purrr::imap_dfr(lineage_all, function(lineage_tbl, df_name) {
    lineage_tbl <- as.data.frame(lineage_tbl)
    lineage_tbl %>%
      mutate(target_df = df_name, root_df = dplyr::first(from_id), final_df = dplyr::last(to_id), total_steps = suppressWarnings(max(step, na.rm = TRUE))) %>%
      select(target_df, step, from_id, to_id, type, block, method, module_function, root_df, final_df, total_steps)
})

tar_tbl <- read.delim(TAR_FILE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
tar_tbl <- tar_tbl %>% mutate(df_id = sub("\\.rds$", "", df), cancer_type = as.character(cancer_type), metric = as.character(metric), category = as.character(category)) %>% select(df_id, cancer_type, metric, category, everything())

feas_log <- read.delim(FEAS_FILE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
feas_log <- feas_log %>% mutate(df_id = sub("\\.rds$", "", df_file), cancer_type = as.character(cancer_type), metric = as.character(metric), algorithm = as.character(algorithm), feasibility_code = as.character(feasibility_code), last_reason = as.character(last_reason), PHASE_III_logic = as.character(PHASE_III_logic))

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
tar_status_for_stratum <- function(df_id, tar_sub) {
  if (df_id == "df005") return("Reference (df005)")
  if (!is_candidate_df(df_id)) return("Not evaluated (non-candidate)")
  hit <- tar_sub$category[match(df_id, tar_sub$df_id)]
  if (!is.na(hit)) return(hit)
  return("Degrade/Not admissible")
}
canary_for_stratum <- function(df_id, feas_sub) {
  if (df_id == "df005") return(list(code = NA_character_, reason = NA_character_, logic = NA_character_))
  hit <- which(feas_sub$df_id == df_id)
  if (length(hit) == 0) return(list(code = NA_character_, reason = NA_character_, logic = NA_character_))
  i <- hit[1]
  dup_flag <- if (length(hit) > 1) paste0("DUPLICATE_LOG_ROWS=", length(hit)) else NA_character_
  reason_txt <- feas_sub$last_reason[i]
  if (!is.na(dup_flag)) reason_txt <- paste0(reason_txt, " [", dup_flag, "]")
  list(code = feas_sub$feasibility_code[i], reason = reason_txt, logic = feas_sub$PHASE_III_logic[i])
}
node_color_final <- function(df_id, tar_summary, can_any_inel_insuff, can_any_inel) {
  if (df_id == "df005") return("rgba(100,149,237,0.85)")
  if (!is_candidate_df(df_id)) return("rgba(180,180,180,0.7)")
  if (isTRUE(can_any_inel_insuff)) return("rgba(220,20,60,0.85)")
  if (isTRUE(can_any_inel)) return("rgba(220,20,60,0.80)")
  if (!is.na(tar_summary) && tar_summary == "Admissible (>=1 metric)") return("rgba(60,179,113,0.85)")
  return("rgba(220,20,60,0.75)")
}

default_ct <- tar_tbl$cancer_type[1]
default_m  <- tar_tbl$metric[1]
tar_sub_default <- tar_tbl %>% filter(cancer_type == default_ct, metric == default_m)
feas_sub_default <- feas_log %>% filter(cancer_type == default_ct, metric == default_m, algorithm == "CoxNet")

nodes_static <- lineage_summary_all %>% select(from_id, to_id) %>% pivot_longer(cols = everything(), values_to = "df_id") %>% distinct(df_id) %>% mutate(node_id = row_number() - 1)
node_role_txt <- vapply(nodes_static$df_id, node_role, character(1))
node_tar_status <- vapply(nodes_static$df_id, tar_status_for_stratum, character(1), tar_sub = tar_sub_default)
canary_list <- lapply(nodes_static$df_id, canary_for_stratum, feas_sub = feas_sub_default)
node_canary_code   <- vapply(canary_list, `[[`, character(1), "code")
node_canary_reason <- vapply(canary_list, `[[`, character(1), "reason")
node_canary_logic  <- vapply(canary_list, `[[`, character(1), "logic")

node_colors_static <- vapply(seq_along(nodes_static$df_id), function(i) {
  logic <- node_canary_logic[i]
  inel_insuff <- isTRUE(!is.na(logic) && logic == "INELIGIBLE__INSUFFICIENT_SURVIVAL_INFORMATION")
  inel_any <- isTRUE(!is.na(logic) && grepl("^INELIGIBLE", logic))
  tar_sum <- if (node_tar_status[i] %in% c("Unchanged", "Improved")) "Admissible (>=1 metric)" else node_tar_status[i]
  node_color_final(nodes_static$df_id[i], tar_sum, inel_insuff, inel_any)
}, character(1))

links_static <- lineage_summary_all %>% mutate(label = paste(type, method, sep = ": ")) %>% count(from_id, to_id, label, name = "value") %>% transmute(source = nodes_static$node_id[match(from_id, nodes_static$df_id)], target = nodes_static$node_id[match(to_id, nodes_static$df_id)], value = value, label = label)

p_sankey_static <- plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = nodes_static$df_id, color = node_colors_static, pad = 15, thickness = 15, line = list(color = "black", width = 0.5),
    textfont = list(color = "black", size = 28),
    customdata = cbind(node_role_txt, node_tar_status, node_canary_code, node_canary_reason, node_canary_logic),
    hovertemplate = paste0("<b>%{label}</b><br>Role: %{customdata[0]}<br>TAR status (", default_ct, "–", default_m, "): %{customdata[1]}<br>CANARY feasibility_code: %{customdata[2]}<br>CANARY last_reason: %{customdata[3]}<br>PHASE_III_logic: %{customdata[4]}<extra></extra>")
  ),
  link = list(
    source = links_static$source, target = links_static$target, value = links_static$value, label = links_static$label,
    hovertemplate = "Transition:<br>%{label}<br>Multiplicity: %{value}<extra></extra>", color = "rgba(200, 30, 30, 0.35)"
  )
) %>% layout(
  title = list(
    text = paste0("Sankey — Lineage (annotated) | Default stratum: ", default_ct, "–", default_m),
    font = list(size = 48),
    y = 0.95,
    x = 0.5,
    xanchor = "center",
    yanchor = "top"
  ),
  font = list(size = 24, color = "black"),
  margin = list(t = 150, b = 50, l = 50, r = 50)
)

# 3) Export using plotly::save_image (uses Kaleido)
message("Generating Kaleido static renders (Landscape 3200x1800)...")

plotly::save_image(p_sankey_static, "sankey_lineage_annotated_default.pdf", width = 3200, height = 1800)
plotly::save_image(p_sankey_static, "sankey_lineage_annotated_default.png", width = 3200, height = 1800, scale = 2)

img <- magick::image_read("sankey_lineage_annotated_default.png")
bg <- magick::image_blank(magick::image_info(img)[['width']], magick::image_info(img)[['height']], color = "white")
img <- magick::image_composite(bg, img, operator = "over")
img <- magick::image_flatten(img)

magick::image_write(img, path = "sankey_lineage_annotated_default.jpg", format = "jpeg", quality = 100)
magick::image_write(img, path = "sankey_lineage_annotated_default.tiff", format = "tiff", compression = "lzw", density = "600x600")

message("Export complete: PDF, PNG, JPG, and 600 DPI TIFF generated successfully.")
