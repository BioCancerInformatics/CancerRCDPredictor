# ============================================================
#   ✅ 1. ggplot2 HEATMAP CODE (df × cancer × metric)
# ============================================================

library(ggplot2)
library(tidyr)
library(dplyr)

plot_metric_heatmap <- function(results,
                                metric,
                                outfile_prefix = "heatmap",
                                palette_low = "#2166ac",
                                palette_mid = "#f7f7f7",
                                palette_high = "#b2182b") {
  
  df_plot <- results %>%
    filter(metric == !!metric) %>%
    select(df, cancer_type, cindex)
  
  # Order df numerically in correct sequence
  df_plot$df_num <- as.numeric(gsub("df", "", gsub(".rds", "", df_plot$df)))
  
  df_plot <- df_plot %>%
    arrange(df_num) %>%
    mutate(df = factor(df, levels = unique(df)))
  
  p <- ggplot(df_plot, aes(x = cancer_type, y = df, fill = cindex)) +
    geom_tile(color = "grey90", size = 0.2) +
    scale_fill_gradient2(
      low = palette_low,
      mid = palette_mid,
      high = palette_high,
      midpoint = 0.5,
      na.value = "grey85",
      limits = c(0, 1)
    ) +
    labs(
      title = paste0("C-index Heatmap: ", metric),
      x = "Cancer Type",
      y = "dfXXX",
      fill = "C-index"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  # PDF export
  pdf(paste0(outfile_prefix, "_", metric, ".pdf"), width = 12, height = 14)
  print(p)
  dev.off()
  
  # PNG export
  png(paste0(outfile_prefix, "_", metric, ".png"), width = 2400, height = 2800, res = 300)
  print(p)
  dev.off()
  
  p
}

## Run for each metric:
plot_metric_heatmap(all_results, "OS")
plot_metric_heatmap(all_results, "DSS")
plot_metric_heatmap(all_results, "DFI")
plot_metric_heatmap(all_results, "PFI")



# ============================================================
#   ✅ 2. MULTI-PANEL PDF EXPORT (ALL METRICS TOGETHER)
# ============================================================
plot_all_metrics_panel <- function(results,
                                   outfile = "heatmap_all_metrics.pdf") {
  
  metrics <- c("OS", "DSS", "DFI", "PFI")
  
  pdf(outfile, width = 14, height = 18)
  
  for (metric in metrics) {
    print(plot_metric_heatmap(results, metric))
  }
  
  dev.off()
}

plot_all_metrics_panel(all_results)

# 
# ============================================================
#   ✅ 3. SUMMARY TABLES OF NA-REASONS (NA Diagnostics)
# ============================================================
summarize_na_reasons <- function(results,
                                 outfile_prefix = "na_reason_summary") {
  
  na_summary <- results %>%
    group_by(metric, reason) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(metric, desc(n))
  
  write.table(
    na_summary,
    paste0(outfile_prefix, ".tsv"),
    sep = "\t", row.names = FALSE, quote = FALSE
  )
  
  na_summary
}

na_summary <- summarize_na_reasons(all_results)
na_summary


summarize_na_by_cancer <- function(results) {
  results %>%
    group_by(cancer_type, metric, reason) %>%
    summarise(n = n(), .groups = "drop")
}

summarize_na_by_df <- function(results) {
  results %>%
    group_by(df, metric, reason) %>%
    summarise(n = n(), .groups = "drop")
}


# ============================================================
#   ✅ 4. PERFORMANCE DASHBOARD (MODEL-15 STYLE)
# ============================================================
# 

build_performance_dashboard <- function(results,
                                        outfile = "performance_dashboard.tsv") {
  
  # 1. Valid C-index counts per df
  valid_counts <- results %>%
    group_by(df) %>%
    summarise(
      valid_OS  = sum(metric == "OS"  & reason == "OK"),
      valid_DSS = sum(metric == "DSS" & reason == "OK"),
      valid_DFI = sum(metric == "DFI" & reason == "OK"),
      valid_PFI = sum(metric == "PFI" & reason == "OK"),
      total_valid = sum(reason == "OK"),
      .groups = "drop"
    )
  
  # 2. Global NA-reason summary per df
  na_by_df <- results %>%
    filter(reason != "OK") %>%
    group_by(df, reason) %>%
    summarise(n = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = reason, values_from = n, values_fill = 0)
  
  # 3. Merge
  dashboard <- valid_counts %>%
    left_join(na_by_df, by = "df") %>%
    arrange(desc(total_valid))
  
  # 4. Export
  write.table(
    dashboard,
    outfile,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  
  dashboard
}

dashboard <- build_performance_dashboard(all_results)
dashboard

