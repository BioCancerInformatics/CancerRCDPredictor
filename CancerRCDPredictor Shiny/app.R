  # ==============================================================================
  # CANCERRCDPREDICTOR PHASE IV: EDUCATIONAL SANDBOX & TOPOLOGY EXPLORER
  # ==============================================================================
  # ==============================================================================
  # AUTONOMOUS DEPENDENCY MANAGEMENT
  # ==============================================================================
  required_packages <- c("shiny", "bslib", "bsicons", "DT", "magick", "rmarkdown", "pagedown")
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  
  if(length(missing_packages)) {
    message("Installing missing dependencies: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, repos = "http://cran.rstudio.com/")
  }
  
  # Suppress startup messages during library loading
  suppressPackageStartupMessages({
    invisible(lapply(required_packages, require, character.only = TRUE))
  })
  # ==============================================================================
  
  # ==============================================================================
  # NIXOS CACHE PURGE: Prevent read-only overwrite errors on ZIMA Server
  # NixOS copies files as read-only. On subsequent runs in the same R session, 
  # htmltools tries to overwrite them and fails, crashing the app. 
  # Purging the temp cache before launch ensures a clean, successful copy every time.
  # ==============================================================================
  try({
    unlink(list.files(tempdir(), pattern = "selectize", full.names = TRUE), recursive = TRUE, force = TRUE)
    unlink(list.files(tempdir(), pattern = "bootstrap", full.names = TRUE), recursive = TRUE, force = TRUE)
  }, silent = TRUE)
  # ==============================================================================
  
  # ==============================================================================
  # 1. GLOBAL DATA LOADING
  # ==============================================================================
  # Establish Virtual Tunnel to 35GB External Drive (Zero-Copy Architecture)
  # We use a fault-tolerant check to ensure the app doesn't crash if the drive is unplugged.
  ZIMA_ROOT <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
  # Establish Virtual Tunnel to ZIMA Cube
  zima_drive_path <- file.path(ZIMA_ROOT, "PHASE_III_ML_Models")
  if (dir.exists(zima_drive_path)) {
    addResourcePath("zima_models", zima_drive_path)
  } else {
    # Fallback: map to a temporary directory so the app still launches
    addResourcePath("zima_models", tempdir())
    message("WARNING: ZIMA External Drive (E:\\) not detected! PDF modules will display empty frames.")
  }
  
  # Load the 96 verified cohort matrices to power the dynamic selector engines (Locally Copied)
  raw_cohort_matrix <- read.csv(file.path(ZIMA_ROOT, "Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv"), stringsAsFactors = FALSE)
  
  # The raw matrix contains 'Cohort' entries like "ACC_DSS_df377". We must parse these to power the dropdown logic:
  cohort_matrix <- data.frame(Full_Name = unique(raw_cohort_matrix$Cohort), stringsAsFactors = FALSE)
  cohort_matrix$Cancer <- sapply(strsplit(cohort_matrix$Full_Name, "_"), `[`, 1)
  cohort_matrix$Metric <- sapply(strsplit(cohort_matrix$Full_Name, "_"), `[`, 2)
  cohort_matrix$DF_ID <- sapply(strsplit(cohort_matrix$Full_Name, "_"), `[`, 3)
  
  ui <- tagList(
    # Inject the Glassmorphism CSS architecture
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(HTML("
        .clickable-card { cursor: pointer; transition: transform 0.2s, box-shadow 0.2s; }
        .clickable-card:hover { transform: translateY(-5px); box-shadow: 0 10px 20px rgba(0,0,0,0.5); border-color: var(--primary-color); }
        .dropdown-menu { z-index: 99999 !important; }
        .navbar { z-index: 99998 !important; }
        
        @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
        .spinner-box { background: white; border-radius: 14px; padding: 42px 64px; text-align: center; box-shadow: 0 10px 30px rgba(0,0,0,0.3); display: flex; flex-direction: column; align-items: center; }
        .spinner-ring { width: 66px; height: 66px; border: 7px solid #ecf0f1; border-top-color: #3b82f6; border-radius: 50%; animation: spin .85s linear infinite; margin-bottom: 20px; }
        
        /* Responsive Design Media Queries */
        @media (max-width: 1200px) {
          .clickable-card { height: auto !important; min-height: 130px; padding: 15px !important; }
          .glass-panel { margin-bottom: 15px; }
          iframe { max-height: 60vh !important; }
        }
        @media (max-width: 992px) {
          .bslib-layout-columns { display: flex !important; flex-direction: column !important; gap: 15px; }
          .clickable-card { width: 100% !important; margin-top: 10px !important; }
          .glass-panel > div { flex: 1 1 100% !important; text-align: left; }
          img { max-width: 100%; height: auto; }
        }
      "))
    ),
    
    # Custom JS Loading Overlay OUTSIDE of the navbar constraints
    tags$div(id = "loading-overlay", style = "display: none; position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background: rgba(0,0,0,0.58); z-index: 999999; flex-direction: column; justify-content: center; align-items: center; backdrop-filter: blur(2px);",
      tags$div(class = "spinner-box",
        tags$div(class = "spinner-ring"),
        h3(style = "color: #1e293b; font-weight: bold; letter-spacing: 0.5px; margin-top: 10px;", "Preparing Report!"),
        p(style = "color: #64748b; font-size: 1.1rem; margin-top: 10px;", "Synthesizing individual multi-omic geometry.", tags$br(), "Please wait while the file is compiled.")
      )
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('hide_spinner', function(message) {
        document.getElementById('loading-overlay').style.display = 'none';
      });
    ")),
    
    page_navbar(
      title = "CancerRCDPredictor",
      id = "main_nav", # ID for navigation control
      theme = bs_theme(
        version = 5,
        bg = "#0f172a", fg = "#ffffff", primary = "#3b82f6",
        base_font = font_google("Outfit")
      ),
    
    # ==============================================================================
    # TAB 0: Welcome & Capability
    # ==============================================================================
    nav_panel(title = "Welcome", value = "tab_welcome", icon = bs_icon("house-fill"),
              div(style = "width: 100%;",
                div(class = "glass-panel", style = "display: flex; flex-direction: row; gap: 20px; align-items: flex-start; flex-wrap: wrap; margin-bottom: 10px;",
                    div(style = "flex: 1; min-width: 300px;",
                        div(class = "glass-title", bs_icon("info-circle"), "Precision Oncology Diagnostic Engine"),
                        p("Welcome to the CancerRCDPredictor. This tool bridges 96 predictive models across Pan-Cancer cohorts (spanning OS, DSS, PFI, and DFI metrics) using a frozen, pre-computed SuperLearner Multi-View architecture."),
                        p("This application operates as an advanced Clinical Diagnostic Engine. It utilizes pre-rendered survival geometries to prevent server exhaustion while delivering high-fidelity, manuscript-grade interaction topologies."),
                        p(style="color: #cbd5e1; margin-top: 15px; font-weight: 300; line-height: 1.6;", 
                          "At the core of this framework lies an exhaustive machine learning pipeline designed to bypass linear topological failures and predict individualized clinical non-proportional hazard trajectories. By deploying a Quadripartite Ensemble (RSF, XGBoost, Boruta, and MTLR) fused via a Multi-View Meta-Learner (MVL), the algorithm structurally audited over 12,613 multi-omic signatures spanning 7 distinct omic layers. Rather than relying on simple additive prognostic markers, this advanced SuperLearner isolates the true predictive power of non-linear biological geometries. By extracting N-dimensional TreeSHAP topologies, the application dynamically exposes the exact lethal and protective trajectories per patient—singularizing the specific biological markers that act as primary non-proportional hazard drivers or robust protective shields."
                        )
                    ),
                    div(style = "flex-shrink: 0; margin: 0 auto;",
                        tags$img(src = "cancerrcdpredictor_logo_bloodorange.png", 
                                 style = "width: 160px; height: 160px; border-radius: 50%; box-shadow: 0 4px 20px rgba(255, 69, 0, 0.4); object-fit: cover; border: 2px solid rgba(255, 69, 0, 0.6); display: block;")
                    )
                )
              ),
              layout_columns(
                col_widths = c(4, 4, 4),
                # Clickable Navigation Cards
                div(class = "clickable-card", id = "nav_card_atlas", onclick = "Shiny.setInputValue('go_to_tab', 'tab_atlas', {priority: 'event'});", style = "padding: 10px; margin-top: 0px; height: 130px; display: flex; flex-direction: column; justify-content: center; align-items: center; background: rgba(59, 130, 246, 0.1); border: 1px solid #3b82f6; border-radius: 8px; text-align: center;",
                    bs_icon("diagram-3-fill", size = "1.5em", style = "color: #3b82f6; margin-bottom: 5px;"),
                    h6(style = "color: #60a5fa; margin-bottom: 2px; font-size: 0.9rem; font-weight: bold;", "Dependency Topologies"),
                    p(style="font-size: 0.75rem; color: #cbd5e1; margin-top: 5px; margin-bottom: 0;", "Click to explore the Atlas of interactions")
                ),
                div(class = "clickable-card", id = "nav_card_beeswarm", onclick = "Shiny.setInputValue('go_to_tab', 'tab_beeswarm', {priority: 'event'});", style = "padding: 10px; margin-top: 0px; height: 130px; display: flex; flex-direction: column; justify-content: center; align-items: center; background: rgba(239, 68, 68, 0.1); border: 1px solid #ef4444; border-radius: 8px; text-align: center;",
                    bs_icon("globe2", size = "1.5em", style = "color: #ef4444; margin-bottom: 5px;"),
                    h4(style = "color: #f87171; font-weight: bold; margin-bottom: 2px; font-size: 1.1rem;", "96 Cohort Models"),
                    p(style="font-size: 0.75rem; color: #cbd5e1; margin-top: 5px; margin-bottom: 0;", "Click to explore Synergies")
                ),
                div(class = "clickable-card", id = "nav_card_signatures", onclick = "Shiny.setInputValue('go_to_tab', 'tab_interpreter', {priority: 'event'});", style = "padding: 10px; margin-top: 0px; height: 130px; display: flex; flex-direction: column; justify-content: center; align-items: center; background: rgba(16, 185, 129, 0.1); border: 1px solid #10b981; border-radius: 8px; text-align: center;",
                    bs_icon("file-medical", size = "1.5em", style = "color: #10b981; margin-bottom: 5px;"),
                    h4(style = "color: #34d399; font-weight: bold; margin-bottom: 2px; font-size: 1.1rem;", "12,613 Signatures"),
                    p(style="font-size: 0.75rem; color: #cbd5e1; margin-top: 5px; margin-bottom: 0;", "Click to explore the Interpreter")
                )
              )
    ),
    
    # ==============================================================================
    # TAB 0.1: How to Read (Pedagogical Module)
    
    nav_panel(title = "How to Read", icon = bs_icon("book"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("journal-medical"), "Pedagogical Module: Interpreting Geometries"),
                    p("Machine Learning survival geometries (like SHAP and LIME) can be dense. This section serves as a static guide to interpreting penalty vs. protection axes."),
                    hr(style = "border-color: rgba(255,255,255,0.1);"),
                    tags$a(href = "#", onclick = "Shiny.setInputValue('go_to_tab', 'tab_beeswarm', {priority: 'event'}); return false;", style = "text-decoration: none; color: #60a5fa;", 
                           h4("1. The SHAP BeeSwarm", style = "transition: color 0.2s; cursor: pointer;", onmouseover="this.style.color='#93c5fd';", onmouseout="this.style.color='#60a5fa';")),
                    p("A right-sided (positive) SHAP value indicates a lethality driver (increased non-proportional hazard). A left-sided (negative) value indicates a protective shield (decreased non-proportional hazard)."),
                    tags$a(href = "#", onclick = "Shiny.setInputValue('go_to_tab', 'tab_atlas', {priority: 'event'}); return false;", style = "text-decoration: none; color: #60a5fa;", 
                           h4("2. SHAP Topologies (Synergy, Antagonism, & Bifurcation)", style = "transition: color 0.2s; cursor: pointer;", onmouseover="this.style.color='#93c5fd';", onmouseout="this.style.color='#60a5fa';")),
                    p("By plotting two interacting genes against their SHAP values, we decode their biological synergy, antagonism, or bifurcation across the clinical non-proportional hazard domain."),
                    tags$a(href = "#", onclick = "Shiny.setInputValue('go_to_tab', 'tab_precision_oncology', {priority: 'event'}); return false;", style = "text-decoration: none; color: #60a5fa;", 
                           h4("3. Individual Patient Trajectories (Precision Oncology)", style = "transition: color 0.2s; cursor: pointer;", onmouseover="this.style.color='#93c5fd';", onmouseout="this.style.color='#60a5fa';")),
                    p("SHAP Waterfall/Force Plots decompile the predictive logic for a single patient against a population baseline. The visualized bars directly represent specific multi-omic signatures (both continuous expression vectors and discrete genomic states like mutations/CNVs) acting as vectors of non-proportional hazard or protection. The predictive weight of each signature is defined by its breadth (width), orientation along the axis, and its top-to-bottom sequence in the trajectory."), p("Orange bars represent omic signatures imposing aggressive non-proportional hazard penalties (lethality), while purple bars represent signatures forcing deep negative non-proportional hazard pushes (protective shields).")
                )
              )
    ),
    
    # ==============================================================================
    # TAB 0.2: Clinical User Manual
    
    nav_panel(title = "Methodological Integrity", icon = bs_icon("shield-check"),
              layout_columns(
                col_widths = c(12),
                # Phase Pipeline Schematic
                div(class = "glass-panel", style = "margin-bottom: 20px;",
                    div(class = "glass-title", bs_icon("diagram-3-fill"), "Analytical Architecture: Phase I-III Protocol"),
                    div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 15px; text-align: center;",
                        div(style = "flex: 1; padding: 15px; background: rgba(59, 130, 246, 0.1); border: 1px solid #3b82f6; border-radius: 8px;",
                            bs_icon("database-check", size = "2em", style = "color: #3b82f6; margin-bottom: 10px;"),
                            h5(style = "color: #60a5fa;", "Phase I: Harmonization"),
                            p(style = "font-size: 0.85rem; color: #cbd5e1;", "Raw multi-omic inputs strictly harmonized and dimensionally mapped via LiSHMOM logic.")
                        ),
                        div(style = "padding: 0 15px;", bs_icon("arrow-right", size = "2em", style = "color: #94a3b8;")),
                        div(style = "flex: 1; padding: 15px; background: rgba(239, 68, 68, 0.1); border: 1px solid #ef4444; border-radius: 8px;",
                            bs_icon("shield-shaded", size = "2em", style = "color: #ef4444; margin-bottom: 10px;"),
                            h5(style = "color: #f87171;", "Phase II: CANARY Protocol"),
                            p(style = "font-size: 0.85rem; color: #cbd5e1;", "Cohorts violating Proportional Hazards (PH) geometry structurally quarantined via CoxNet.")
                        ),
                        div(style = "padding: 0 15px;", bs_icon("arrow-right", size = "2em", style = "color: #94a3b8;")),
                        div(style = "flex: 1; padding: 15px; background: rgba(16, 185, 129, 0.1); border: 1px solid #10b981; border-radius: 8px;",
                            bs_icon("cpu-fill", size = "2em", style = "color: #10b981; margin-bottom: 10px;"),
                            h5(style = "color: #34d399;", "Phase III: Ensemble Synthesis"),
                            p(style = "font-size: 0.85rem; color: #cbd5e1;", "Topologies decoded using a Quadripartite Ensemble synthesized by a Multi-View ElasticNet SuperLearner.")
                        )
                    )
                ),
                # Quadripartite Ensemble Explorer
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("cpu"), "Phase III: The Quadripartite Ensemble Explorer"),
                    p(style = "color: #94a3b8; font-size: 0.9rem;", "Select a specific machine learning algorithmic framework to instantly project its specific mathematical Importance and Validation payload across the Golden 150 biological signatures."),
                    hr(style = "border-color: rgba(255,255,255,0.1);"),
                    layout_columns(
                      col_widths = c(3, 9),
                      div(
                        selectInput("ensemble_algo", "Select Algorithm Validation:", 
                                    choices = c("RSF (Random Survival Forest)" = "RSF", 
                                                "XGBoost (Extreme Gradient Boosting)" = "XGBoost", 
                                                "Boruta (Wrapper RF)" = "Boruta", 
                                                "MTLR (Multi-Task Logistic Regression)" = "MTLR")),
                        hr(style = "border-color: rgba(255,255,255,0.1);"),
                        uiOutput("ensemble_algo_desc")
                      ),
                      div(
                        DTOutput("ensemble_table")
                      )
                    )
                )
              )
    ),
  
    # ==============================================================================
    
    nav_panel(title = "MVL Performance", value = "tab_mvl", icon = bs_icon("graph-up"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("activity"), "MVL SuperLearner Performance"),
                    p("Explore the Time-dependent ROC (AUROC) horizons for the 96 finalized models."),
                    
                    navset_card_pill(
                      id = "mvl_pill",
                      nav_panel("1. Pedagogical Exemplars (Manuscript)", icon = bs_icon("book-half"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel", style = "background: rgba(15, 23, 42, 0.4);",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("info-circle"), " Figure 9: Dual TimeROC"),
                                      div(style = "margin-top: 20px; padding: 15px; background: rgba(0,0,0,0.2); border-radius: 8px;",
                                          h6(style = "color: #94a3b8; font-size: 0.8rem; text-transform: uppercase;", "HOW TO READ: AUROC PERFORMANCE"),
                                          p(style = "color: #e2e8f0; font-size: 0.85rem;", "The X-axis represents False Positive Rate (1-Specificity), while the Y-axis tracks True Positive Rate (Sensitivity). The colored curves represent the discriminatory capability (AUC) of the MVL framework across three time horizons (1-year, 3-year, 5-year). The dotted diagonal represents random effect (AUC 0.50)."),
                                          p(style = "color: #e2e8f0; font-size: 0.85rem;", tags$strong("(A) Lush Multi-Omic Prognostic Stability (LGG DSS): "), "Testing the algorithm's decentralized quad-core resilience, this panel demonstrates the framework generating and maintaining a high plateau of clinical discrimination across a deeply fragmented multi-omic terrain. By democratically balancing 25.0% trust across all four learning architectures, the SuperLearner successfully flattens prognostic entropy over time (maintaining an AUC of 0.931 at 1-year, 0.900 at 3-year, and 0.802 at 5-year progression nodes). This prevents the chronological predictive degradation typically seen in Lower Grade Gliomas."),
                                          p(style = "color: #e2e8f0; font-size: 0.85rem;", tags$strong("(B) Supreme Algorithmic Convergence (READ OS): "), "Powered by a near-total 95.7% sparsity-aware XGBoost hierarchy, this panel maps the temporal diagnostic trajectory when continuous omic parameters align perfectly into deterministic geometric axes. The framework achieves flawless instantaneous prognostic authority (AUC = 1.000 at 1-year) and successfully anchors a virtually impenetrable predictive barrier (AUC = 0.996) out to the 3-year tracking horizon, before encountering expected entropy drop-offs at extended boundaries (AUC = 0.842 at 5-year).")
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("render_fig9_composite")
                                  )
                                )
                      ),
                     nav_panel("2. The 96-Cohort Explorer (Data)", icon = bs_icon("database-fill-gear"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " Model Selection"),
                                      selectInput("auroc_cancer", "Target Cohort (Cancer):", choices = NULL),
                                      selectInput("auroc_metric", "Clinical Outcome Metric:", choices = NULL),
                                      hr(style = "border-color: rgba(255,255,255,0.1);"),
                                      uiOutput("auroc_df_info"),
                                      div(style = "margin-top: 20px;",
                                          downloadButton("download_auroc", "Download High-Res TIFF", class = "btn btn-primary", style = "width: 100%;")
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("auroc_plot_container")
                                  )
                                )
                      )
                    )
                )
              )
    ),
  
    # ==============================================================================
    
    nav_panel(title = "Global Impact (Beeswarms)", value = "tab_beeswarm", icon = bs_icon("bar-chart-steps"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("globe2"), "Global Multi-Omic Impact"),
                    p("Explore the macro-level non-proportional hazard drivers isolated across the 96 finalized prognostic models."),
                    
                    navset_card_pill(
                      id = "beeswarm_pill",
                      nav_panel("1. Pedagogical Exemplar (Manuscript)", icon = bs_icon("book-half"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("info-circle"), " Pedagogical Exemplars"),
                                      p(style = "color: #cbd5e1; font-size: 0.9rem;", "Select a specific cohort exemplar from the manuscript to explore its macro-level non-proportional hazard drivers."),
                                      selectInput("edu_beeswarm_exemplar", "Select Exemplar:", choices = c("Multi-Omic Dominance (LGG DSS - Figure 4)", "Sparsity Rescue Anchors (READ OS - Figure S8)")),
                                      uiOutput("edu_beeswarm_text")
                                  ),
                                  div(class = "glass-panel", style = "min-height: 400px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("edu_beeswarm_image_container")
                                  )
                                )
                      ),
                      nav_panel("2. The 96-Cohort Explorer (Data)", icon = bs_icon("database-fill-gear"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " Model Selection"),
                                      selectInput("beeswarm_cancer", "Target Cohort (Cancer):", choices = NULL),
                                      selectInput("beeswarm_metric", "Clinical Outcome Metric:", choices = NULL),
                                      hr(style = "border-color: rgba(255,255,255,0.1);"),
                                      uiOutput("beeswarm_df_info"),
                                      div(style = "margin-top: 20px;",
                                          downloadButton("download_beeswarm", "Download High-Res TIFF", class = "btn btn-primary", style = "width: 100%;")
                                      ),
                                      hr(style = "border-color: rgba(255,255,255,0.1); margin-top: 25px;"),
                                      div(style = "background: rgba(16, 185, 129, 0.1); border: 1px solid #10b981; border-radius: 8px; padding: 15px;",
                                          h6(style = "color: #34d399; font-weight: bold; margin-bottom: 10px;", bs_icon("robot"), " Clinical Intelligence Panel"),
                                          uiOutput("beeswarm_ai_summary")
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("beeswarm_plot_container")
                                  )
                                )
                      )
                    )
                )
              )
    ),
  
    # ==============================================================================
    
    nav_panel(title = "Interaction Topologies", value = "tab_atlas", icon = bs_icon("diagram-3"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("map"), "Interaction Topologies"),
                    p("Explore specific multi-omic interactions mapping clinical non-proportional hazard interception at the cohort level."),
                    
                    navset_card_pill(
                      id = "topology_pill",
                      nav_panel("1. Pedagogical Exemplars (Manuscript)", icon = bs_icon("book-half"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel", style = "background: rgba(15, 23, 42, 0.4);",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("info-circle"), " Figure 8: Interaction Topologies"),
                                      div(style = "margin-top: 20px; padding: 15px; background: rgba(0,0,0,0.2); border-radius: 8px;",
                                          h6(style = "color: #94a3b8; font-size: 0.8rem; text-transform: uppercase;", "HOW TO READ: PANELS A, B, C"),
                                          p(style = "color: #e2e8f0; font-size: 0.85rem;", "(A) Synergy (LUAD DSS): The partner mutation acts as a potent catalyst, violently accelerating the patient cloud upward into extreme lethality as the primary driver increases across the x-axis."),
                                          p(style = "color: #e2e8f0; font-size: 0.85rem;", "(B) Antagonism (LUAD DSS): The partner mutation acts as a functional buffer, physically rescuing the patient cloud by forcing the mortality trajectory back down into the protective zone."),
                                          p(style = "color: #e2e8f0; font-size: 0.85rem;", "(C) Bifurcation (SKCM OS): The primary driver dictates the absolute clinical hemisphere (x-axis), while the distinct modulating partner stratifies the internal density of the clouds.")
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("render_fig8_composite")
                                  )
                                )
                      ),
                      nav_panel("2. The 96-Cohort Explorer (Data)", icon = bs_icon("database-fill-gear"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " Model Selection"),
                                      selectInput("topology_cancer", "Target Cohort (Cancer):", choices = NULL),
                                      selectInput("topology_metric", "Clinical Outcome Metric:", choices = NULL),
                                      hr(style = "border-color: rgba(255,255,255,0.1);"),
                                      uiOutput("topology_df_info"),
                                      selectInput("topology_signature", "Select Interaction Pair:", choices = NULL),
                                      div(style = "margin-top: 20px;",
                                          downloadButton("download_topology", "Download High-Res TIFF", class = "btn btn-primary", style = "width: 100%;")
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("topology_plot_container"),
                                      uiOutput("topology_metadata_table")
                                  )
                                )
                      )
                    )
                )
              )
    ),
  
      # ==============================================================================
    
    nav_panel(title = tags$span(style="color: #fbbf24; font-weight: bold;", bs_icon("award-fill"), " The Golden 150"), value = "tab_golden",
              layout_columns(col_widths = c(12),
                             div(class = "glass-panel", style = "border: 1px solid #fbbf24; box-shadow: 0 4px 30px rgba(251, 191, 36, 0.1);",
                                 div(class = "glass-title", style = "color: #fbbf24;", bs_icon("trophy"), "The Pan-Cancer Golden Signatures"),
                                 p("This module isolates the 150 elite biological signatures (Table S9) that were universally retained by all four Machine Learning algorithms (RSF, XGBoost, Boruta, MTLR)."),
                                 hr(style = "border-color: rgba(251,191,36,0.2);"),
                                 plotOutput("golden_layer_plot", height = "300px"),
                                 hr(style = "border-color: rgba(251,191,36,0.2);"),
                                 DTOutput("golden_table")
                             )
              )
    ),
  
    # ==============================================================================
    
    nav_panel(title = "Precision Oncology", value = "tab_precision_oncology", icon = bs_icon("bullseye"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("person-vcard"), "Individualized Patient Trajectories"),
                    p("Explore specific multi-omic interactions mapping clinical non-proportional hazard interception at the individual patient level."),
                    
                    navset_card_pill(
                      id = "precision_pill",
                      nav_panel("1. Pedagogical Exemplar (Manuscript)", icon = bs_icon("book-half"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel", style = "background: rgba(15, 23, 42, 0.4); border: 1px solid rgba(59, 130, 246, 0.3);",
                                      h5(style = "color: #3b82f6; border-bottom: 1px solid rgba(59, 130, 246, 0.2); padding-bottom: 10px;", bs_icon("exclamation-triangle"), " Non-Proportional Hazard Trajectory Selector"),
                                      p(style = "color: #cbd5e1; font-size: 0.9rem;", "Select a patient trajectory to singularize the signatures driving the prediction toward lethality or pulling it back to safety."),
                                      selectInput("edu_trajectory_type", "Select Exemplar Trajectory:", choices = c("Lethal Trajectory (LGG DSS: TCGA-HT-7616-01)", "Protective Trajectory (LGG DSS: TCGA-DU-7008-01)")),
                                      uiOutput("edu_trajectory_text"),
                                      tags$div(style = "margin-top: 30px;",
                                        h6(style = "color: #cbd5e1; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 8px; margin-bottom: 15px;", bs_icon("file-earmark-medical"), " CancerRCDPredictor Diagnostic Report (Precision Oncology)"),
                                        tags$div(style = "display: flex; gap: 10px;",
                                          downloadButton("download_patient_html", "Download HTML Report", class = "btn btn-primary", style = "flex: 1; font-weight: bold;", onclick = "document.getElementById('loading-overlay').style.display = 'flex';"),
                                          downloadButton("download_patient_pdf", "Download PDF Report", class = "btn btn-danger", style = "flex: 1; font-weight: bold;", onclick = "document.getElementById('loading-overlay').style.display = 'flex';")
                                        )
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 400px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("render_trajectory_container")
                                  )
                                )
                      ),
                      nav_panel("2. The 96-Cohort Explorer (Data)", icon = bs_icon("database-fill-gear"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " Model Selection"),
                                      selectInput("precision_cancer", "Target Cohort (Cancer):", choices = NULL),
                                      selectInput("precision_metric", "Clinical Outcome Metric:", choices = NULL),
                                      hr(style = "border-color: rgba(255,255,255,0.1);"),
                                      uiOutput("precision_df_info"),
                                      selectInput("precision_signature", "Select Signature Trajectory:", choices = NULL),
                                      tags$div(style = "margin-top: 30px;",
                                        h6(style = "color: #cbd5e1; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 8px; margin-bottom: 15px;", bs_icon("file-earmark-medical"), " CancerRCDPredictor Diagnostic Report (Precision Oncology)"),
                                        tags$div(style = "display: flex; gap: 10px;",
                                          downloadButton("download_precision_html", "Download HTML Report", class = "btn btn-primary", style = "flex: 1; font-weight: bold;", onclick = "document.getElementById('loading-overlay').style.display = 'flex';"),
                                          downloadButton("download_precision_pdf", "Download PDF Report", class = "btn btn-danger", style = "flex: 1; font-weight: bold;", onclick = "document.getElementById('loading-overlay').style.display = 'flex';")
                                        )
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      uiOutput("precision_trajectory_container")
                                  )
                                )
                      )
                    )
                )
              )
    ),
  # ==============================================================================
    
    nav_panel(title = "Signatures Overview", value = "tab_signatures", icon = bs_icon("table"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("table"), "12,613 Prognostic Signatures"),
                    p("A searchable datatable containing all prognostic signatures, categorized by their biological omic layer origin."),
                    DTOutput("signatures_overview_table")
                )
              )
    ),
  
    # ==============================================================================
    
    nav_panel(title = tags$span(style="color: #60a5fa; font-weight: bold;", bs_icon("translate"), " The Diagnostic Interpreter"), value = "tab_interpreter",
              layout_columns(col_widths = c(12),
                             div(class = "glass-panel", style = "border: 1px solid #60a5fa; box-shadow: 0 4px 30px rgba(96, 165, 250, 0.1);",
                                 div(class = "glass-title", style = "color: #60a5fa;", bs_icon("file-medical"), "Table S8 Diagnostic Engine"),
                                 p("This engine converts the 12,613 high-dimensional prognostic signatures (Table S8) into human-readable clinical diagnostic reports."),
                                 hr(style = "border-color: rgba(96,165,250,0.2);"),
                                 
                                 layout_columns(col_widths = c(4, 8),
                                   # Signature Selector (Left Side)
                                   div(
                                     selectizeInput("interpreter_signature_select", tags$span(style="color: #cbd5e1;", "Search & Select Signature Nomenclature:"), choices = NULL, width = "100%", options = list(dropdownParent = 'body'))
                                   ),
                                   
                                   # Diagnostic Printout Panel (Right Side)
                                   div(style = "background: rgba(15, 23, 42, 0.8); border: 1px solid #334155; border-radius: 8px; padding: 25px;",
                                       uiOutput("diagnostic_report_ui")
                                   )
                                 )
                             )
              )
    ),
    
    # ==============================================================================
    # UPPER MENU BAR: About Section
    # ==============================================================================
    nav_spacer(),
    
    nav_menu(
      title = "About", icon = bs_icon("info-circle"),
      nav_panel(title = "Cite Us", icon = bs_icon("quote"),
                layout_columns(col_widths = c(8),
                               div(class = "glass-panel", style = "margin: 100px auto 20px auto;",
                                   div(class = "glass-title", bs_icon("quote"), "Cite Our Work"),
                                   p("If you use this prediction tool or the underlying ML methodologies in your research, please cite our manuscript:"),
                                   tags$pre(style = "background: rgba(0,0,0,0.3); color: #cbd5e1; padding: 15px; border-radius: 8px; border: 1px solid rgba(255,255,255,0.1);",
  "@article{CancerRCDPredictor2026,
    title={A pan-cancer quadripartite machine learning ensemble for decoding prognostic multi-omic topographies},
    author={Emanuell Rodrigues de Souza and Higor Almeida Cordeiro Nogueira and Victor dos Santos Lopes and Enrique Medina-Acosta},
    journal={Under Review},
    year={2026}
  }"
                                   ),
                                   div(style = "text-align: center; margin-top: 15px;",
                                       tags$a(href = "https://www.frontiersin.org/journals/bioinformatics/articles/10.3389/fbinf.2025.1630518/full", target = "_blank", HTML(paste(bs_icon("box-arrow-up-right"), " Read Manuscript Online")), class = "btn btn-outline-light")
                                   )
                               )
                )
      ),
      
      # --- [NEW] TAB 6: DEVELOPERS ---
      nav_panel(title = "Developers", icon = bs_icon("people"),
                layout_columns(col_widths = c(8),
                               div(class = "glass-panel", style = "margin: 100px auto 20px auto;",
                                   div(class = "glass-title", bs_icon("people"), "Authors & Developers"),
                                   p("Meet the team behind the Phase I-III computational pipelines."),
                                   layout_columns(col_widths = c(6, 6),
                                                  div(style = "text-align: center; margin-top: 20px;",
                                                      tags$img(src = "ESR_Photo.jpg", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Emanuell Rodrigues de Souza", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Co-Author"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Emanuell-Rodrigues-De-Souza", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  ),
                                                  div(style = "text-align: center; margin-top: 20px;",
                                                      tags$img(src = "HACN_Photo.jpg", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Higor Almeida Cordeiro Nogueira", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Co-Author"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Higor-Cordeiro-Nogueira", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  )
                                   ),
                                   layout_columns(col_widths = c(6, 6),
                                                  div(style = "text-align: center; margin-top: 20px;",
                                                      tags$img(src = "VSL_Photo.png", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Victor dos Santos Lopes", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Co-Author"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Victor-Lopes-25", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  ),
                                                  div(style = "text-align: center; margin-top: 20px;",
                                                      tags$img(src = "EMA_Photo.jpg", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Enrique Medina-Acosta", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Corresponding Author / ML Architect"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Enrique-Medina-Acosta", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  )
                                   )
                               )
                )
      ),
  
      "---",
      nav_item(tags$a(href = "https://cancerrcdshiny.shinyapps.io/cancerrcdshiny/", target = "_blank", HTML(paste(bs_icon("box-arrow-up-right"), "Explore CancerRCDShiny (Populational Atlas)")), class = "nav-link")),
      nav_item(tags$a(href = "https://www.frontiersin.org/journals/bioinformatics/articles/10.3389/fbinf.2025.1630518/full", target = "_blank", HTML(paste(bs_icon("journal-text"), "Read Legacy CancerRCDShiny (Populational Atlas) Manuscript")), class = "nav-link"))
    )
  )
)
  
  # ==============================================================================
  # 2. SERVER ARCHITECTURE
  # ==============================================================================
  
  server <- function(input, output, session) {
    
    # Navigation Logic from Welcome Cards
    observeEvent(input$go_to_tab, {
      updateNavbarPage(session, "main_nav", selected = input$go_to_tab)
    })
      # External Link Handlers (Replaced with native tags$a in UI to avoid popup blockers)
    
    # --- 96-COHORT DROPDOWN ENGINE (TAB 2B) ---
    observe({
      updateSelectInput(session, "beeswarm_cancer", choices = unique(cohort_matrix$Cancer))
    })
    
    observeEvent(input$beeswarm_cancer, {
      available_metrics <- cohort_matrix$Metric[cohort_matrix$Cancer == input$beeswarm_cancer]
      updateSelectInput(session, "beeswarm_metric", choices = available_metrics)
    })
    
    # Display the selected dfXX matrix identity
    output$cohort_df_info <- renderUI({
      req(input$beeswarm_cancer, input$beeswarm_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$beeswarm_cancer & cohort_matrix$Metric == input$beeswarm_metric, ]
      req(nrow(selected_model) > 0)
      
      tags$div(
        style = "padding: 10px; background: rgba(59, 130, 246, 0.1); border-left: 4px solid #3b82f6; border-radius: 4px;",
        tags$strong("Active Matrix: "), tags$span(style="color: #60a5fa;", selected_model$DF_ID[1])
      )
    })
    
    # Render the High-Res Beeswarm TIFF via Magick PNG Conversion
    output$beeswarm_plot_container <- renderUI({
      req(input$beeswarm_cancer, input$beeswarm_metric)
      
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$beeswarm_cancer & cohort_matrix$Metric == input$beeswarm_metric, ]
      req(nrow(selected_model) > 0)
      
      folder_name <- selected_model$Full_Name[1]
      file_name <- paste0(folder_name, "_SHAP_Overall_Beeswarm.tiff")
      tiff_path <- file.path(zima_drive_path, folder_name, "XGBoost", file_name)
      
      if (file.exists(tiff_path)) {
        # We use renderImage to create a temp PNG representation for the browser
        output$dynamic_beeswarm_img <- renderImage({
          tmp_png <- sub("\\.tiff$", ".png", tiff_path, ignore.case = TRUE)
          if(!file.exists(tmp_png)) {
            img <- image_read(tiff_path)
            image_write(img, path = tmp_png, format = "png")
          }
          list(src = tmp_png, contentType = 'image/png', width = "100%", style="border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.4);", alt = "SHAP Beeswarm")
        }, deleteFile = FALSE)
        
        imageOutput("dynamic_beeswarm_img", height = "auto")
      } else {
        tags$div(style = "color: #ef4444; padding: 20px; text-align: center;", bs_icon("exclamation-triangle"), " TIFF asset not found in ZIMA directory.")
      }
    })
    
    # Download Handler for the Beeswarm Plot
    output$download_beeswarm <- downloadHandler(
      filename = function() {
        selected_model <- cohort_matrix[cohort_matrix$Cancer == input$beeswarm_cancer & cohort_matrix$Metric == input$beeswarm_metric, ]
        if(nrow(selected_model) > 0) {
          paste0(selected_model$Full_Name[1], "_SHAP_Overall_Beeswarm.tiff")
        } else {
          "beeswarm.tiff"
        }
      },
      content = function(file) {
        selected_model <- cohort_matrix[cohort_matrix$Cancer == input$beeswarm_cancer & cohort_matrix$Metric == input$beeswarm_metric, ]
        if(nrow(selected_model) > 0) {
          folder_name <- selected_model$Full_Name[1]
          file_name <- paste0(folder_name, "_SHAP_Overall_Beeswarm.tiff")
          tiff_path <- file.path(zima_drive_path, folder_name, "XGBoost", file_name)
          if(file.exists(tiff_path)) {
            file.copy(tiff_path, file)
          }
        }
      }
    )
    # --- TAB 2A / 3A EDUCATIONAL STATIC RENDERERS ---
    
    output$edu_beeswarm_text <- renderUI({
      req(input$edu_beeswarm_exemplar)
      if(input$edu_beeswarm_exemplar == "Multi-Omic Dominance (LGG DSS - Figure 4)") {
          div(style = "margin-top: 20px; padding: 15px; background: rgba(0,0,0,0.2); border-radius: 8px;",
              h6(style = "color: #94a3b8; font-size: 0.8rem; text-transform: uppercase;", "How to Read"),
              p(style = "color: #e2e8f0; font-size: 0.85rem;", "Each dot is a patient. The X-axis indicates SHAP (impact on non-proportional hazard). Colors denote high/low continuous expression or the presence/absence of discrete somatic mutations and CNVs. Positive SHAP values (mapped to the right) indicate a lethal projection, whereas negative SHAP values (mapped to the left) indicate a protective projection.")
          )
      } else {
          div(style = "margin-top: 20px; padding: 15px; background: rgba(0,0,0,0.2); border-radius: 8px;",
              h6(style = "color: #94a3b8; font-size: 0.8rem; text-transform: uppercase;", "Sparsity Rescue (Somatic Mutations & CNVs)"),
              p(style = "color: #e2e8f0; font-size: 0.85rem;", "In highly sparse cohorts, traditional clinical markers often fail. Here, the model relies on discrete genomic events (Somatic Mutations and CNVs). Note how the presence (red) vs absence (blue) of specific mutations completely anchors the patient population into distinct lethal (right) or protective (left) hemispheres, acting as absolute decision boundaries.")
          )
      }
    })
  
    # Safely load an exemplary Beeswarm image dynamically so the visual correctly aligns with the pedagogy
    output$edu_beeswarm_image_container <- renderUI({
      req(input$edu_beeswarm_exemplar)
      if(input$edu_beeswarm_exemplar == "Multi-Omic Dominance (LGG DSS - Figure 4)") {
         demo_tiff <- file.path(ZIMA_ROOT, "Figures", "Figure_4_LGG_DSS_df374_SHAP_Overall_Beeswarm.tiff")
      } else {
         demo_tiff <- file.path(ZIMA_ROOT, "Figures", "Figure_S8_Sup_READ_OS_df160_SHAP_Overall_Beeswarm.tiff")
      }
      
      if (file.exists(demo_tiff)) {
        output$edu_static_beeswarm_img <- renderImage({
          tmp_png <- sub("\\.tiff$", ".png", demo_tiff, ignore.case = TRUE)
          if(!file.exists(tmp_png)) {
            img <- image_read(demo_tiff)
            image_write(img, path = tmp_png, format = "png")
          }
          list(src = tmp_png, contentType = 'image/png', width = "100%", style="border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.4);", alt = "Pedagogical Beeswarm Example")
        }, deleteFile = FALSE)
        imageOutput("edu_static_beeswarm_img", height = "auto")
      } else {
        tags$div(style = "color: #ef4444; padding: 20px;", "Pedagogical asset not found.")
      }
    })
    
    output$beeswarm_ai_summary <- renderUI({
      req(input$beeswarm_cancer, input$beeswarm_metric)
      tags$div(
        tags$p(style = "color: #cbd5e1; font-size: 0.85rem; margin-bottom: 5px;", 
               paste0("This ", input$beeswarm_cancer, " (", input$beeswarm_metric, ") cohort exhibits complex non-linear dependency.")),
        tags$p(style = "color: #94a3b8; font-size: 0.8rem; font-style: italic;", 
               "The Multi-View Meta-Learner (MVL) successfully collapsed high-dimensional multi-omic features into a definitive hazard-projection geometry.")
      )
    })
    
    # Safely load an exemplary AUROC image dynamically
    output$edu_auroc_image_container <- renderUI({
      demo_tiff <- file.path(zima_drive_path, "ACC_OS_df377", "MVL_Synthesis", "ACC_OS_df377_MVL_Synthesis_AUC_Curves.tiff")
      if (file.exists(demo_tiff)) {
        output$edu_static_auroc_img <- renderImage({
          tmp_png <- sub("\\.tiff$", ".png", demo_tiff, ignore.case = TRUE)
          if(!file.exists(tmp_png)) {
            img <- image_read(demo_tiff)
            image_write(img, path = tmp_png, format = "png")
          }
          list(src = tmp_png, contentType = 'image/png', width = "100%", style="border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.4);", alt = "Pedagogical AUROC Example")
        }, deleteFile = FALSE)
        imageOutput("edu_static_auroc_img", height = "auto")
      } else {
        tags$div(style = "color: #ef4444;", "AUROC Plot Not Found")
      }
    })
    
    # --- 96-COHORT DROPDOWN ENGINE (TAB 1B: TOPOLOGIES) ---
    observe({
      updateSelectInput(session, "topology_cancer", choices = unique(cohort_matrix$Cancer))
    })
    
    observeEvent(input$topology_cancer, {
      available_metrics <- cohort_matrix$Metric[cohort_matrix$Cancer == input$topology_cancer]
      updateSelectInput(session, "topology_metric", choices = available_metrics)
    })
    
    # Display the selected dfXX matrix identity for Topologies
    output$topology_df_info <- renderUI({
      req(input$topology_cancer, input$topology_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$topology_cancer & cohort_matrix$Metric == input$topology_metric, ]
      req(nrow(selected_model) > 0)
      
      tags$div(
        style = "padding: 10px; background: rgba(59, 130, 246, 0.1); border-left: 4px solid #3b82f6; border-radius: 4px; margin-bottom: 15px;",
        tags$strong("Active Matrix: "), tags$span(style="color: #60a5fa;", selected_model$DF_ID[1])
      )
    })
    
    # Dynamically scan the XGBoost folder for SHAP Dependence PDFs
    observeEvent(c(input$topology_cancer, input$topology_metric), {
      req(input$topology_cancer, input$topology_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$topology_cancer & cohort_matrix$Metric == input$topology_metric, ]
      req(nrow(selected_model) > 0)
      
      folder_name <- selected_model$Full_Name[1]
      xgboost_path <- file.path(zima_drive_path, folder_name, "XGBoost")
      
      if(dir.exists(xgboost_path)) {
        pdf_files <- list.files(xgboost_path, pattern = "_SHAP_Dependence_.*\\.pdf$")
        if(length(pdf_files) > 0) {
          clean_names <- gsub(paste0(folder_name, "_SHAP_Dependence_"), "", pdf_files)
          clean_names <- gsub("\\.pdf$", "", clean_names)
          
          # Restore actual biological nomenclature format (e.g., STAD_174_6... to STAD-174.6...)
          clean_names <- sub("_", "-", clean_names)
          clean_names <- gsub("_", ".", clean_names)
          
          updateSelectInput(session, "topology_signature", choices = setNames(pdf_files, clean_names))
        } else {
          updateSelectInput(session, "topology_signature", choices = c("No interactions extracted for this cohort" = ""))
        }
      } else {
        updateSelectInput(session, "topology_signature", choices = c("ZIMA XGBoost Directory Not Found" = ""))
      }
    })
    
    # Render the High-Res Topology PDF via iframe
    output$topology_plot_container <- renderUI({
      req(input$topology_cancer, input$topology_metric, input$topology_signature)
      if(input$topology_signature == "") return(NULL)
      
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$topology_cancer & cohort_matrix$Metric == input$topology_metric, ]
      req(nrow(selected_model) > 0)
      
      folder_name <- selected_model$Full_Name[1]
      
      # Use the Virtual Path zima_models defined earlier
      pdf_url <- paste0("zima_models/", folder_name, "/XGBoost/", input$topology_signature, "#zoom=100")
      
      tags$iframe(src = pdf_url, width = "100%", height = "700px", style = "border: 1px solid #334155; border-radius: 8px;")
    })
    
    # --- RENDER INTERACTION METADATA TABLE ---
    output$topology_metadata_table <- renderUI({
      req(input$topology_cancer, input$topology_metric, input$topology_signature)
      if(input$topology_signature == "") return(NULL)
      
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$topology_cancer & cohort_matrix$Metric == input$topology_metric, ]
      req(nrow(selected_model) > 0)
      folder_name <- selected_model$Full_Name[1]
      
      # Filter the raw_cohort_matrix to the active cohort
      cohort_data <- raw_cohort_matrix[raw_cohort_matrix$Cohort == folder_name, ]
      req(nrow(cohort_data) > 0)
      
      # Robust string matching to find the exact interaction pair
      # We strip ALL non-alphanumeric characters to avoid unicode minus signs (U+2212) vs regular hyphens
      clean_sig <- gsub("[^[:alnum:]]", "", input$topology_signature)
      
      # Find the row corresponding to the Primary Signature shown in the filename.
      # Note: SHAP Dependence plots automatically color by the top partner, so the filename typically only contains the Primary signature.
      matched_indices <- which(
        sapply(cohort_data$Primary_Signature, function(x) {
          if(is.na(x) || x == "") return(FALSE)
          grepl(gsub("[^[:alnum:]]", "", x), clean_sig, ignore.case=TRUE)
        })
      )
      matched_row <- cohort_data[matched_indices, ]
      
      # If we found a match, display the metadata panel
      if(nrow(matched_row) > 0) {
        match <- matched_row[1, ] # Take the first match if multiple
        
        tags$div(style = "margin-top: 20px; width: 100%; background: rgba(15, 23, 42, 0.8); border: 1px solid #334155; border-radius: 8px; padding: 20px; text-align: left;",
            tags$h6(style = "color: #3b82f6; font-weight: bold; margin-bottom: 15px; border-bottom: 1px solid rgba(59,130,246,0.3); padding-bottom: 8px;", bs_icon("clipboard-data"), " Clinical Intelligence (Table S15 Metadata)"),
            
            tags$div(style = "display: flex; gap: 15px; margin-bottom: 15px; flex-wrap: wrap;",
              tags$div(style = "flex: 1; min-width: 200px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #fbbf24;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Mathematical Classification"),
                tags$strong(style = "color: #fbbf24; font-size: 1.1rem;", match$Mathematical_Classification)
              ),
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #ef4444;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "FDR (High Zone)"),
                tags$strong(style = "color: #f87171;", match$FDR_HighZone)
              ),
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #10b981;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Spearman CrossTalk"),
                tags$strong(style = "color: #34d399;", match$Spearman_HighZone_CrossTalk)
              )
            ),
            
            tags$div(style = "display: flex; gap: 15px; flex-wrap: wrap;",
              tags$div(style = "flex: 1; min-width: 250px; background: rgba(59,130,246,0.1); padding: 15px; border-radius: 6px; border: 1px solid rgba(59,130,246,0.3);",
                tags$div(style="display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px;",
                    tags$h6(style = "color: #60a5fa; margin: 0;", "Primary Axis Driver (X-Axis)"),
                    tags$span(style = "background: rgba(59,130,246,0.2); color: #93c5fd; padding: 2px 8px; border-radius: 12px; font-size: 0.75rem; font-weight: bold;", match$Primary_Omic_Token)
                ),
                tags$p(style = "color: #93c5fd; font-size: 0.8rem; margin-bottom: 2px;", match$Signature_Primary),
                tags$p(style = "color: #e2e8f0; font-family: monospace; font-size: 0.9rem; margin-bottom: 5px;", match$Primary_Signature),
                tags$p(style = "color: #cbd5e1; font-size: 0.85rem; line-height: 1.4; margin: 0; padding-top: 5px; border-top: 1px solid rgba(59,130,246,0.2);", match$Decoded_Genetic_Element_Primary)
              ),
              tags$div(style = "flex: 1; min-width: 250px; background: rgba(139,92,246,0.1); padding: 15px; border-radius: 6px; border: 1px solid rgba(139,92,246,0.3);",
                tags$div(style="display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px;",
                    tags$h6(style = "color: #a78bfa; margin: 0;", "Interaction Partner (Color Axis)"),
                    tags$span(style = "background: rgba(139,92,246,0.2); color: #c4b5fd; padding: 2px 8px; border-radius: 12px; font-size: 0.75rem; font-weight: bold;", match$Partner_Omic_Token)
                ),
                tags$p(style = "color: #c4b5fd; font-size: 0.8rem; margin-bottom: 2px;", match$Signature_Partner),
                tags$p(style = "color: #e2e8f0; font-family: monospace; font-size: 0.9rem; margin-bottom: 5px;", match$color_var_Partner),
                tags$p(style = "color: #cbd5e1; font-size: 0.85rem; line-height: 1.4; margin: 0; padding-top: 5px; border-top: 1px solid rgba(139,92,246,0.2);", match$Decoded_Genetic_Element_Partner)
              )
            )
        )
      } else {
        tags$div(style = "margin-top: 20px; width: 100%; color: #94a3b8; font-style: italic; text-align: center;", "Metadata not found for this specific interaction in Table S15.")
      }
    })
    
    # Download Handler for the Topology Plot (Provides TIFF download!)
    output$download_topology <- downloadHandler(
      filename = function() {
        if(is.null(input$topology_signature) || input$topology_signature == "") {
          "topology.tiff"
        } else {
          gsub("\\.pdf$", ".tiff", input$topology_signature)
        }
      },
      content = function(file) {
        req(input$topology_signature)
        selected_model <- cohort_matrix[cohort_matrix$Cancer == input$topology_cancer & cohort_matrix$Metric == input$topology_metric, ]
        if(nrow(selected_model) > 0) {
          folder_name <- selected_model$Full_Name[1]
          tiff_name <- gsub("\\.pdf$", ".tiff", input$topology_signature)
          tiff_path <- file.path(zima_drive_path, folder_name, "XGBoost", tiff_name)
          if(file.exists(tiff_path)) {
            file.copy(tiff_path, file)
          }
        }
      }
    )
    
    # --- 96-COHORT DROPDOWN ENGINE (TAB 3B: MVL AUROC PERFORMANCE) ---
    observe({
      updateSelectInput(session, "auroc_cancer", choices = unique(cohort_matrix$Cancer))
    })
    
    observeEvent(input$auroc_cancer, {
      available_metrics <- cohort_matrix$Metric[cohort_matrix$Cancer == input$auroc_cancer]
      updateSelectInput(session, "auroc_metric", choices = available_metrics)
    })
    
    output$auroc_df_info <- renderUI({
      req(input$auroc_cancer, input$auroc_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$auroc_cancer & cohort_matrix$Metric == input$auroc_metric, ]
      req(nrow(selected_model) > 0)
      tags$div(
        style = "padding: 10px; background: rgba(59, 130, 246, 0.1); border-left: 4px solid #3b82f6; border-radius: 4px; margin-bottom: 15px;",
        tags$strong("Active Matrix: "), tags$span(style="color: #60a5fa;", selected_model$DF_ID[1])
      )
    })
    
    output$auroc_plot_container <- renderUI({
      req(input$auroc_cancer, input$auroc_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$auroc_cancer & cohort_matrix$Metric == input$auroc_metric, ]
      req(nrow(selected_model) > 0)
      
      folder_name <- selected_model$Full_Name[1]
      file_name <- paste0(folder_name, "_MVL_Synthesis_AUC_Curves.tiff")
      tiff_path <- file.path(zima_drive_path, folder_name, "MVL_Synthesis", file_name)
      
      if (file.exists(tiff_path)) {
        output$dynamic_auroc_img <- renderImage({
          tmp_png <- sub("\\.tiff$", ".png", tiff_path, ignore.case = TRUE)
          if(!file.exists(tmp_png)) {
            img <- image_read(tiff_path)
            image_write(img, path = tmp_png, format = "png")
          }
          list(src = tmp_png, contentType = 'image/png', width = "100%", style="border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.4);", alt = "TimeROC Horizon")
        }, deleteFile = FALSE)
        
        imageOutput("dynamic_auroc_img", height = "auto")
      } else {
        tags$div(style = "color: #ef4444; padding: 20px; text-align: center;", bs_icon("exclamation-triangle"), " AUROC TIFF asset not found in ZIMA directory.")
      }
    })
    
    output$download_auroc <- downloadHandler(
      filename = function() {
        selected_model <- cohort_matrix[cohort_matrix$Cancer == input$auroc_cancer & cohort_matrix$Metric == input$auroc_metric, ]
        if(nrow(selected_model) > 0) {
          paste0(selected_model$Full_Name[1], "_MVL_Synthesis_AUC_Curves.tiff")
        } else {
          "auroc.tiff"
        }
      },
      content = function(file) {
        selected_model <- cohort_matrix[cohort_matrix$Cancer == input$auroc_cancer & cohort_matrix$Metric == input$auroc_metric, ]
        if(nrow(selected_model) > 0) {
          folder_name <- selected_model$Full_Name[1]
          file_name <- paste0(folder_name, "_MVL_Synthesis_AUC_Curves.tiff")
          tiff_path <- file.path(zima_drive_path, folder_name, "MVL_Synthesis", file_name)
          if(file.exists(tiff_path)) {
            file.copy(tiff_path, file)
          }
        }
      }
    )
    # --- TAB 0.2: METHODOLOGICAL INTEGRITY (QUADRIPARTITE ENSEMBLE) ---
    output$ensemble_algo_desc <- renderUI({
      req(input$ensemble_algo)
      desc <- switch(input$ensemble_algo,
                     "RSF" = "Random Survival Forests geometrically isolate survival interactions via dynamic tree structures.",
                     "XGBoost" = "Extreme Gradient Boosting sequentially minimizes loss functions, capturing highly non-linear protective and lethal decision paths.",
                     "Boruta" = "A wrapper methodology mapping authentic importance against mathematically randomized shadow features.",
                     "MTLR" = "Multi-Task Logistic Regression concurrently optimizes across multiple survival time horizons.")
      tags$p(style = "color: #94a3b8; font-size: 0.9rem; font-style: italic;", desc)
    })
    
    output$ensemble_table <- renderDT({
      req(input$ensemble_algo)
      df <- golden_table_data()
      if("Error" %in% names(df)) return(datatable(df))
      
      # Select columns based on the algorithm
      algo <- input$ensemble_algo
      imp_col <- paste(algo, "Importance")
      val_col <- paste(algo, "Validation")
      
      # Ensure columns exist, else fallback to standard Golden 150 columns
      if(imp_col %in% names(df) && val_col %in% names(df)) {
        display_df <- df[, c("Nomenclature", "CTAB", imp_col, val_col)]
      } else {
        display_df <- df[, c("Nomenclature", "CTAB")]
      }
      
      datatable(display_df, 
                options = list(pageLength = 15, scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-left', targets = "_all")),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'rgba(16, 185, 129, 0.1)', 'color': '#34d399', 'white-space': 'nowrap', 'font-family': 'Calibri', 'font-size': '10pt', 'text-align': 'left'});",
                                 "}"
                               )),
                class = 'cell-border stripe hover',
                rownames = FALSE) |>
        formatStyle(columns = names(display_df), backgroundColor = "rgba(0,0,0,0.5)", color = "#e2e8f0", fontFamily = "Calibri", fontSize = "10pt", textAlign = 'left') |>
        formatStyle("Nomenclature", whiteSpace = "nowrap")
    })
    
    # --- TAB 4: GOLDEN 150 DATATABLE ENGINE ---
    golden_table_data <- reactive({
      csv_path <- "Table_S9_Golden_150.csv"
      if(file.exists(csv_path)) {
        df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
        return(df)
      } else {
        return(data.frame(Error = "Table_S9_Golden_150.csv not found locally"))
      }
    })
    
    output$golden_layer_plot <- renderPlot({
      req(golden_table_data())
      df <- golden_table_data()
      if("Error" %in% names(df)) return(NULL)
      
      library(ggplot2)
      library(dplyr)
      
      if(!("Biological Layer" %in% names(df))) return(NULL)
      
      plot_data <- df %>%
        group_by(`Biological Layer`) %>%
        summarise(Count = n(), .groups = "drop")
        
      ggplot(plot_data, aes(x = reorder(`Biological Layer`, Count), y = Count)) +
        geom_bar(stat = "identity", fill = "#fbbf24", color = "white", linewidth=0.2) +
        coord_flip() +
        labs(title = "Distribution by Biological Layer", x = "", y = "Retained Signatures") +
        theme_minimal(base_size = 14) +
        theme(
          plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          text = element_text(color = "#cbd5e1", family = "sans"),
          axis.text.y = element_text(color = "#e2e8f0", size = 12, hjust = 1),
          axis.text.x = element_text(color = "#94a3b8"),
          axis.title = element_text(color = "#cbd5e1", face = "bold"),
          plot.title = element_text(color = "#fbbf24", face = "bold", size = 16, hjust = 0.5),
          panel.grid.major = element_line(color = "#FFFFFF0D"),
          panel.grid.minor = element_blank()
        )
    }, bg = "transparent")
    
    output$golden_rcd_plot <- renderPlot({
      req(golden_table_data())
      df <- golden_table_data()
      if("Error" %in% names(df)) return(NULL)
      
      library(ggplot2)
      library(dplyr)
      
      if(!("RCD form" %in% names(df))) return(NULL)
      
      plot_data <- df %>%
        mutate(`RCD form` = ifelse(is.na(`RCD form`) | `RCD form` == "", "Unknown", `RCD form`)) %>%
        group_by(`RCD form`) %>%
        summarise(Count = n(), .groups = "drop")
        
      ggplot(plot_data, aes(x = reorder(`RCD form`, Count), y = Count)) +
        geom_bar(stat = "identity", fill = "#3b82f6", color = "white", linewidth=0.2) +
        coord_flip() +
        labs(title = "Distribution by Regulated Cell Death (RCD) Form", x = "", y = "Retained Signatures") +
        theme_minimal(base_size = 14) +
        theme(
          plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          text = element_text(color = "#cbd5e1", family = "sans"),
          # Use a slightly smaller font size for the y-axis because there are ~40 complex RCD labels
          axis.text.y = element_text(color = "#e2e8f0", size = 10, hjust = 1),
          axis.text.x = element_text(color = "#94a3b8"),
          axis.title = element_text(color = "#cbd5e1", face = "bold"),
          plot.title = element_text(color = "#3b82f6", face = "bold", size = 16, hjust = 0.5),
          panel.grid.major = element_line(color = "#FFFFFF0D"),
          panel.grid.minor = element_blank()
        )
    }, bg = "transparent")
    
    output$golden_table <- renderDT({
      df <- golden_table_data()
      
      # Restrict to user selected variables for Table S9 in precise sequential order
      selected_vars <- c("CTAB", "Nomenclature", "Signature", "Elements", "Biological Layer", "Cohorts Present", "RCD form", "Phenotype", "Total Validated Algorithms")
      available_vars <- intersect(selected_vars, names(df))
      if(length(available_vars) > 0 && !("Error" %in% names(df))) {
        df <- df[, available_vars, drop=FALSE]
      }
      
      datatable(df, 
                options = list(pageLength = 15, 
                               lengthMenu = list(c(15, 50, 100, 150, -1), c('15', '50', '100', '150', 'All')),
                               scrollX = TRUE, 
                               autoWidth = TRUE,
                               columnDefs = list(list(width = '600px', targets = 2),
                                                 list(className = 'dt-left', targets = "_all")),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'rgba(251, 191, 36, 0.1)', 'color': '#fbbf24', 'white-space': 'nowrap', 'font-family': 'Calibri', 'font-size': '10pt', 'text-align': 'left'});",
                                 "}"
                               )),
                class = 'cell-border stripe hover',
                rownames = FALSE,
                selection = "single") %>%
        formatStyle(columns = names(df), backgroundColor = "rgba(0,0,0,0.5)", color = "#e2e8f0", fontFamily = "Calibri", fontSize = "10pt", textAlign = 'left') %>%
        formatStyle("Nomenclature", whiteSpace = "nowrap")
    })
    
    # Modal intercepter for row selection (Renders PDF from E:\ drive)
    observeEvent(input$golden_table_rows_selected, {
      row_idx <- input$golden_table_rows_selected
      row_data <- golden_table_data()[row_idx, ]
      
      feature <- row_data$Feature
      cohorts_str <- row_data$`Cohorts Present`
      
      # Feature formatted for filename (e.g., ACC-68.5.3.P.1.44.44.4.4.1 -> ACC_68_5_3_P_1_44_44_4_4_1)
      f_name <- gsub("-", "_", feature)
      f_name <- gsub("\\.", "_", f_name)
      
      # Extract first cohort validator
      cohorts <- unlist(strsplit(cohorts_str, ",\\s*"))
      first_cohort <- cohorts[1]
      
      pdf_url <- paste0("zima_models/", first_cohort, "/ZIMA_Exhaustive_SHAP_C_Suite/", first_cohort, "_SHAP_Dependence_", f_name, ".pdf#zoom=100")
      
      showModal(modalDialog(
        title = tags$span(style="color: #60a5fa; font-weight: bold;", bs_icon("graph-up"), paste("SHAP Trajectory:", feature)),
        size = "xl",
        easyClose = TRUE,
        
        tags$p(style="color: #cbd5e1;", paste("Cohort Validator:", first_cohort)),
        
        tags$iframe(src = pdf_url, width = "100%", height = "700px", style = "border: 1px solid #334155; border-radius: 8px;"),
        
        tags$div(style = "margin-top: 20px; width: 100%; background: rgba(15, 23, 42, 0.8); border: 1px solid #fbbf24; border-radius: 8px; padding: 20px; text-align: left; box-shadow: 0 4px 30px rgba(251, 191, 36, 0.1);",
            tags$h6(style = "color: #fbbf24; font-weight: bold; margin-bottom: 15px; border-bottom: 1px solid rgba(251, 191, 36, 0.3); padding-bottom: 8px;", bs_icon("trophy"), " Golden 150 Diagnostic Parameters"),
            
            tags$div(style = "display: flex; gap: 15px; margin-bottom: 15px; flex-wrap: wrap;",
              tags$div(style = "flex: 1; min-width: 200px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #fbbf24;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Total Validated Algorithms"),
                tags$strong(style = "color: #fbbf24; font-size: 1.1rem;", row_data$`Total Validated Algorithms`)
              ),
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #60a5fa;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Cancer Type (CTAB)"),
                tags$strong(style = "color: #93c5fd;", row_data$CTAB)
              ),
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #10b981;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Cohorts Present"),
                tags$strong(style = "color: #34d399; font-size: 0.9rem;", row_data$`Cohorts Present`)
              )
            ),
            
            tags$div(style = "display: flex; gap: 15px; flex-wrap: wrap;",
              tags$div(style = "flex: 1; min-width: 250px; background: rgba(251,191,36,0.1); padding: 15px; border-radius: 6px; border: 1px solid rgba(251,191,36,0.3);",
                tags$div(style="display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px;",
                    tags$h6(style = "color: #fbbf24; margin: 0;", "Golden Signature"),
                    tags$span(style = "background: rgba(251,191,36,0.2); color: #fcd34d; padding: 2px 8px; border-radius: 12px; font-size: 0.75rem; font-weight: bold;", row_data$Phenotype)
                ),
                tags$p(style = "color: #e2e8f0; font-family: monospace; font-size: 0.9rem; margin-bottom: 5px;", row_data$Signature),
                tags$p(style = "color: #cbd5e1; font-size: 0.85rem; line-height: 1.4; margin: 0; padding-top: 5px; border-top: 1px solid rgba(251,191,36,0.2);", row_data$Elements)
              ),
              tags$div(style = "flex: 1; min-width: 250px; background: rgba(16,185,129,0.1); padding: 15px; border-radius: 6px; border: 1px solid rgba(16,185,129,0.3);",
                tags$div(style="display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px;",
                    tags$h6(style = "color: #34d399; margin: 0;", "Biological Pathway"),
                    tags$span(style = "background: rgba(16,185,129,0.2); color: #6ee7b7; padding: 2px 8px; border-radius: 12px; font-size: 0.75rem; font-weight: bold;", "RCD Form")
                ),
                tags$p(style = "color: #6ee7b7; font-size: 0.8rem; margin-bottom: 2px;", row_data$`Biological Layer`),
                tags$p(style = "color: #cbd5e1; font-size: 0.85rem; line-height: 1.4; margin: 0; padding-top: 5px; border-top: 1px solid rgba(16,185,129,0.2);", row_data$`RCD form`)
              )
            )
        ),
        
        footer = modalButton("Close")
      ))
    })
    
    # ==============================================================================
    # TAB 5: THE DIAGNOSTIC INTERPRETER ENGINE
    # ==============================================================================
    interpreter_data <- reactive({
      csv_path <- "Table_S8_Interpreter_12k.csv"
      if(file.exists(csv_path)) {
        read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        NULL
      }
    })
    
    observe({
      req(interpreter_data())
      df <- interpreter_data()
      # Server-side selectize for 12k rows performance
      updateSelectizeInput(session, "interpreter_signature_select", choices = df$Feature, server = TRUE)
    })
    
    output$signatures_overview_table <- renderDT({
      req(interpreter_data())
      df <- interpreter_data()
      
      # Harmonize column names to match Golden 150
      if("Biological_Layer" %in% names(df)) names(df)[names(df) == "Biological_Layer"] <- "Biological Layer"
      if("Cohorts_Present" %in% names(df)) names(df)[names(df) == "Cohorts_Present"] <- "Cohorts Present"
      if("Total_Validated_Algorithms" %in% names(df)) names(df)[names(df) == "Total_Validated_Algorithms"] <- "Total Validated Algorithms"
      
      # Calculate Total Validated Algorithms if missing
      if(!("Total Validated Algorithms" %in% names(df)) && "Cohorts Present" %in% names(df)) {
        df$`Total Validated Algorithms` <- sapply(df$`Cohorts Present`, function(x) length(unlist(strsplit(as.character(x), ","))))
      }
      
      # Enforce exact sequential order
      selected_vars <- c("CTAB", "Nomenclature", "Signature", "Elements", "Biological Layer", "Cohorts Present", "RCD form", "Phenotype", "Total Validated Algorithms")
      available_vars <- intersect(selected_vars, names(df))
      if(length(available_vars) > 0) {
        df <- df[, available_vars, drop=FALSE]
      }
      
      datatable(df, 
                options = list(pageLength = 15, scrollX = TRUE,
                               autoWidth = TRUE,
                               columnDefs = list(list(width = '600px', targets = 2),
                                                 list(className = 'dt-left', targets = "_all")),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'rgba(16, 185, 129, 0.1)', 'color': '#34d399', 'white-space': 'nowrap', 'font-family': 'Calibri', 'font-size': '10pt', 'text-align': 'left'});",
                                 "}"
                               )),
                class = 'cell-border stripe hover',
                rownames = FALSE) |>
        formatStyle(columns = names(df), backgroundColor = "rgba(0,0,0,0.5)", color = "#e2e8f0", fontFamily = "Calibri", fontSize = "10pt", textAlign = 'left') |>
        formatStyle("Nomenclature", whiteSpace = "nowrap")
    })
    
    output$diagnostic_report_ui <- renderUI({
      req(input$interpreter_signature_select)
      df <- interpreter_data()
      req(df)
      
      row <- df[df$Feature == input$interpreter_signature_select, ]
      if(nrow(row) == 0) return(tags$p("Signature not found.", style="color: #ef4444;"))
      
      # Extract values with fallback
      f_bio <- ifelse(is.na(row$`Biological_Layer`) | row$`Biological_Layer`=="", "Unspecified Layer", row$`Biological_Layer`)
      f_cohorts <- ifelse(is.na(row$`Cohorts_Present`) | row$`Cohorts_Present`=="", "Unspecified Cohorts", row$`Cohorts_Present`)
      f_nom <- ifelse(is.na(row$`Nomenclature`) | row$`Nomenclature`=="", "Unspecified Nomenclature", row$`Nomenclature`)
      f_sig <- ifelse(is.na(row$`Signature`) | row$`Signature`=="", "Unspecified Signature", row$`Signature`)
      f_elem <- ifelse(is.na(row$`Elements`) | row$`Elements`=="", "an unspecified number of", row$`Elements`)
      f_ctab <- ifelse(is.na(row$`CTAB`) | row$`CTAB`=="", "Unspecified", row$`CTAB`)
      f_rcd <- ifelse(is.na(row$`RCD form`) | row$`RCD form`=="", "Unspecified", row$`RCD form`)
      f_omic <- ifelse(is.na(row$`Omic feature`) | row$`Omic feature`=="", "Unspecified", row$`Omic feature`)
      f_pheno <- ifelse(is.na(row$`Phenotype`) | row$`Phenotype`=="", "Unspecified", row$`Phenotype`)
      
      # Parse nomenclature for correlation sign (5th element)
      nom_parts <- unlist(strsplit(gsub("-", "\\.", row$Feature), "\\."))
      f_corr <- "an unspecified"
      if(length(nom_parts) >= 5) {
        if(nom_parts[5] == "N") f_corr <- "negative"
        if(nom_parts[5] == "P") f_corr <- "positive"
      }
      
      # Expand Phenotype text
      f_pheno_full <- "phenotype"
      if(f_pheno == "1" || f_pheno == "TMB") f_pheno_full <- "Tumor Mutational Burden (TMB)"
      if(f_pheno == "2" || f_pheno == "MSI") f_pheno_full <- "Microsatellite Instability (MSI)"
      if(f_pheno == "3" || f_pheno == "TSM") f_pheno_full <- "Tumor Stemness Measure (TSM)"
      
      # Count survival metrics
      f_num_metrics <- length(unlist(strsplit(f_cohorts, ",")))
      if(f_num_metrics == 1) {
        metric_text <- "one"
      } else if(f_num_metrics == 2) {
        metric_text <- "two"
      } else if(f_num_metrics == 3) {
        metric_text <- "three"
      } else if(f_num_metrics == 4) {
        metric_text <- "four"
      } else {
        metric_text <- as.character(f_num_metrics)
      }
      
      # Safely extract Total Validated Algorithms (user indicated this column will be added)
      f_total_algos <- "Unspecified"
      if ("Total Validated Algorithms" %in% names(row)) f_total_algos <- as.character(row[["Total Validated Algorithms"]])
      else if ("Total_Validated_Algorithms" %in% names(row)) f_total_algos <- as.character(row[["Total_Validated_Algorithms"]])
      else f_total_algos <- metric_text # Fallback to survival metric count if not found
      
      # Build HTML Report matching user's exact template
      tags$div(
        tags$h4(style="color: #60a5fa; font-weight: bold; border-bottom: 1px solid #334155; padding-bottom: 10px; margin-bottom: 20px;", 
                bs_icon("clipboard2-pulse"), " Clinical Diagnostic Report"),
        tags$p(style="color: #e2e8f0; font-size: 16px; line-height: 1.8;",
          HTML(paste0("The <strong style='color:#fbbf24;'>", f_nom, "</strong> signature embodies a complex interaction network within the <strong style='color:#38bdf8;'>", f_bio, "</strong> layer of the <strong style='color:#fbbf24;'>", f_sig, "</strong> gene(s), localized to <strong style='color:#38bdf8;'>", f_ctab, "</strong>. Biologically, these elements are associated with <strong style='color:#f87171;'>", f_rcd, "</strong> regulatory pathways."))
        ),
        tags$h5(style="color: #cbd5e1; font-weight: bold; margin-top: 25px; margin-bottom: 10px;", "Clinical Prognosis"),
        tags$p(style="color: #e2e8f0; font-size: 16px; line-height: 1.8;",
          HTML(paste0("This prognostic marker drives a <strong style='color:#f87171;'>", f_corr, "</strong> phenotypic correlation with the <strong style='color:#c084fc;'>", f_pheno_full, "</strong>. The Multi-View SuperLearner (MVL) architecture successfully validated the <strong style='color:#fbbf24;'>", row$Feature, "</strong> topology across independent patient populations spanning <strong style='color:#f87171;'>", f_total_algos, "</strong> survival metrics: <em style='color:#94a3b8;'>", f_cohorts, "</em>."))
        )
      )
    })
    
    # --- PER-PATIENT CLINICAL EXPORTERS (MOCKUP) ---
    get_patient_params <- function() {
      req(input$edu_trajectory_type)
      
      zima_drive <- zima_drive_path
      lgg_dss_dir <- file.path(zima_drive, "LGG_DSS_df374")
      
      if (input$edu_trajectory_type == "Lethal Trajectory (LGG DSS: TCGA-HT-7616-01)") {
        patient_id <- "TCGA-HT-7616-01"
        cohort <- "LGG DSS"
        traj_type <- "Lethal Accelerating Hazard"
        traj_tiff <- file.path(lgg_dss_dir, "XGBoost", "LGG_DSS_df374_SHAP_Decision_Lethal_Trajectory_TCGA-HT-7616-01.tiff")
        dependence_tiff <- file.path(lgg_dss_dir, "XGBoost", "LGG_DSS_df374_SHAP_Dependence_LGG_1100_7_3_P_3_35_71_1_2_4.tiff")
      } else {
        patient_id <- "TCGA-DU-7008-01"
        cohort <- "LGG DSS"
        traj_type <- "Protective Reversal Hazard"
        traj_tiff <- file.path(lgg_dss_dir, "XGBoost", "LGG_DSS_df374_SHAP_Decision_Protective_Trajectory_TCGA-DU-7008-01.tiff")
        dependence_tiff <- file.path(lgg_dss_dir, "XGBoost", "LGG_DSS_df374_SHAP_Dependence_LGG_579_5_3_N_3_44_35_2_4_2.tiff")
      }
      
      beeswarm_tiff <- file.path(lgg_dss_dir, "XGBoost", "LGG_DSS_df374_SHAP_Overall_Beeswarm.tiff")
      auroc_tiff <- file.path(lgg_dss_dir, "MVL_Synthesis", "LGG_DSS_df374_MVL_Synthesis_AUC_Curves.tiff")
      
      ensure_png <- function(tiff_path) {
        png_path <- sub("\\.tiff$", ".png", tiff_path, ignore.case = TRUE)
        if(!file.exists(png_path) && file.exists(tiff_path)) {
          img <- magick::image_read(tiff_path)
          magick::image_write(img, path = png_path, format = "png")
        }
        return(gsub("\\\\", "/", normalizePath(png_path, mustWork = FALSE)))
      }
      
      list(
        logo = gsub("\\\\", "/", file.path(getwd(), "www", "cancerrcdpredictor_logo_bloodorange.png")),
        patient_id = patient_id,
        cohort = cohort,
        trajectory_type = traj_type,
        trajectory_path = ensure_png(traj_tiff),
        beeswarm_path = ensure_png(beeswarm_tiff),
        dependence_path = ensure_png(dependence_tiff),
        auroc_path = ensure_png(auroc_tiff)
      )
    }
  
    output$download_patient_html <- downloadHandler(
      filename = function() { 
        cohort_clean <- gsub("[^[:alnum:]]", "_", get_patient_params()$cohort)
        paste0("Clinical_Report_", get_patient_params()$patient_id, "_", cohort_clean, ".html") 
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("hide_spinner", list()))
        report_params <- get_patient_params()
        
        # 1. Check persistent cache
        zima_drive <- zima_drive_path
        cache_dir <- file.path(zima_drive, "Clinical_Reports_Cache")
        dir.create(cache_dir, showWarnings = FALSE)
        
        cohort_clean <- gsub("[^[:alnum:]]", "_", report_params$cohort)
        cached_filename <- paste0("Clinical_Report_", report_params$patient_id, "_", cohort_clean, ".html")
        cached_path <- file.path(cache_dir, cached_filename)
        
        if (file.exists(cached_path)) {
          file.copy(cached_path, file)
          return()
        }
        
        # 2. Compile if not cached
        tempReport <- tempfile(fileext = ".Rmd")
        file.copy("clinical_report.Rmd", tempReport, overwrite = TRUE)
        
        withCallingHandlers({
          rmarkdown::render(tempReport, output_file = file, params = report_params, envir = new.env(parent = globalenv()), quiet = TRUE)
        }, warning = function(w) invokeRestart("muffleWarning"))
        
        # 3. Save to persistent cache for future users
        file.copy(file, cached_path, overwrite = TRUE)
      }
    )
  
    output$download_patient_pdf <- downloadHandler(
      filename = function() { 
        cohort_clean <- gsub("[^[:alnum:]]", "_", get_patient_params()$cohort)
        paste0("Clinical_Report_", get_patient_params()$patient_id, "_", cohort_clean, ".pdf") 
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("hide_spinner", list()))
        report_params <- get_patient_params()
        
        # 1. Check persistent cache
        zima_drive <- zima_drive_path
        cache_dir <- file.path(zima_drive, "Clinical_Reports_Cache")
        dir.create(cache_dir, showWarnings = FALSE)
        
        cohort_clean <- gsub("[^[:alnum:]]", "_", report_params$cohort)
        cached_filename <- paste0("Clinical_Report_", report_params$patient_id, "_", cohort_clean, ".pdf")
        cached_path <- file.path(cache_dir, cached_filename)
        
        if (file.exists(cached_path)) {
          file.copy(cached_path, file)
          return()
        }
        
        # 2. Compile if not cached
        tempReport <- tempfile(fileext = ".Rmd")
        file.copy("clinical_report.Rmd", tempReport, overwrite = TRUE)
        tempHtml <- tempfile(fileext = ".html")
        
        withCallingHandlers({
          rmarkdown::render(tempReport, output_file = tempHtml, params = report_params, envir = new.env(parent = globalenv()), quiet = TRUE)
        }, warning = function(w) invokeRestart("muffleWarning"))
        
        if(requireNamespace("pagedown", quietly = TRUE)) {
          pagedown::chrome_print(tempHtml, output = file)
          # 3. Save PDF to persistent cache
          file.copy(file, cached_path, overwrite = TRUE)
        } else {
          showNotification("Real PDF compilation requires the 'pagedown' package. Downloading HTML fallback.", type = "error", duration = 8)
          file.copy(tempHtml, file)
        }
      }
    )
    
    # --- 96-COHORT EXPLORER CLINICAL EXPORTERS ---
    get_precision_params <- function() {
      req(input$precision_cancer, input$precision_metric, input$precision_signature)
      
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$precision_cancer & cohort_matrix$Metric == input$precision_metric, ]
      req(nrow(selected_model) > 0)
      
      folder_name <- selected_model$Full_Name[1]
      zima_drive <- zima_drive_path
      cohort_dir <- file.path(zima_drive, folder_name)
      
      # Extract Patient ID from filename: ACC_DSS_df377_SHAP_Decision_Lethal_Trajectory_TCGA-OR-A5J1-01.pdf
      filename_no_ext <- sub("\\.pdf$", "", input$precision_signature)
      parts <- unlist(strsplit(filename_no_ext, "_"))
      patient_id <- parts[length(parts)]
      
      # Extract Trajectory Type
      traj_type <- ifelse(grepl("Lethal", input$precision_signature, ignore.case=TRUE), "Lethal Accelerating Hazard", "Protective Reversal Hazard")
      
      traj_tiff <- file.path(cohort_dir, "XGBoost", sub("\\.pdf$", ".tiff", input$precision_signature))
      beeswarm_tiff <- file.path(cohort_dir, "XGBoost", paste0(folder_name, "_SHAP_Overall_Beeswarm.tiff"))
      auroc_tiff <- file.path(cohort_dir, "MVL_Synthesis", paste0(folder_name, "_MVL_Synthesis_AUC_Curves.tiff"))
      
      # Get first Dependence plot as Representative Topology
      dep_files <- list.files(file.path(cohort_dir, "XGBoost"), pattern = "_SHAP_Dependence_.*\\.tiff$")
      if (length(dep_files) > 0) {
        dependence_tiff <- file.path(cohort_dir, "XGBoost", dep_files[1])
      } else {
        dependence_tiff <- beeswarm_tiff # Fallback
      }
      
      ensure_png <- function(tiff_path) {
        if(!file.exists(tiff_path)) return("")
        png_path <- sub("\\.tiff$", ".png", tiff_path, ignore.case = TRUE)
        if(!file.exists(png_path)) {
          img <- magick::image_read(tiff_path)
          magick::image_write(img, path = png_path, format = "png")
        }
        return(gsub("\\\\", "/", normalizePath(png_path, mustWork = FALSE)))
      }
      
      list(
        logo = gsub("\\\\", "/", file.path(getwd(), "www", "cancerrcdpredictor_logo_bloodorange.png")),
        patient_id = patient_id,
        cohort = paste(input$precision_cancer, input$precision_metric),
        trajectory_type = traj_type,
        trajectory_path = ensure_png(traj_tiff),
        beeswarm_path = ensure_png(beeswarm_tiff),
        dependence_path = ensure_png(dependence_tiff),
        auroc_path = ensure_png(auroc_tiff)
      )
    }
  
    output$download_precision_html <- downloadHandler(
      filename = function() { 
        p <- get_precision_params()
        cohort_clean <- gsub("[^[:alnum:]]", "_", p$cohort)
        paste0("Clinical_Report_", p$patient_id, "_", cohort_clean, ".html") 
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("hide_spinner", list()))
        report_params <- get_precision_params()
        
        zima_drive <- zima_drive_path
        cache_dir <- file.path(zima_drive, "Clinical_Reports_Cache")
        dir.create(cache_dir, showWarnings = FALSE)
        
        cohort_clean <- gsub("[^[:alnum:]]", "_", report_params$cohort)
        cached_filename <- paste0("Clinical_Report_", report_params$patient_id, "_", cohort_clean, ".html")
        cached_path <- file.path(cache_dir, cached_filename)
        
        if (file.exists(cached_path)) {
          file.copy(cached_path, file)
          return()
        }
        
        tempReport <- tempfile(fileext = ".Rmd")
        file.copy("clinical_report.Rmd", tempReport, overwrite = TRUE)
        
        withCallingHandlers({
          rmarkdown::render(tempReport, output_file = file, params = report_params, envir = new.env(parent = globalenv()), quiet = TRUE)
        }, warning = function(w) invokeRestart("muffleWarning"))
        
        file.copy(file, cached_path, overwrite = TRUE)
      }
    )
  
    output$download_precision_pdf <- downloadHandler(
      filename = function() { 
        p <- get_precision_params()
        cohort_clean <- gsub("[^[:alnum:]]", "_", p$cohort)
        paste0("Clinical_Report_", p$patient_id, "_", cohort_clean, ".pdf") 
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("hide_spinner", list()))
        report_params <- get_precision_params()
        
        zima_drive <- zima_drive_path
        cache_dir <- file.path(zima_drive, "Clinical_Reports_Cache")
        dir.create(cache_dir, showWarnings = FALSE)
        
        cohort_clean <- gsub("[^[:alnum:]]", "_", report_params$cohort)
        cached_filename <- paste0("Clinical_Report_", report_params$patient_id, "_", cohort_clean, ".pdf")
        cached_path <- file.path(cache_dir, cached_filename)
        
        if (file.exists(cached_path)) {
          file.copy(cached_path, file)
          return()
        }
        
        tempReport <- tempfile(fileext = ".Rmd")
        file.copy("clinical_report.Rmd", tempReport, overwrite = TRUE)
        tempHtml <- tempfile(fileext = ".html")
        
        withCallingHandlers({
          rmarkdown::render(tempReport, output_file = tempHtml, params = report_params, envir = new.env(parent = globalenv()), quiet = TRUE)
        }, warning = function(w) invokeRestart("muffleWarning"))
        
        if(requireNamespace("pagedown", quietly = TRUE)) {
          pagedown::chrome_print(tempHtml, output = file)
          file.copy(file, cached_path, overwrite = TRUE)
        } else {
          file.copy(tempHtml, file)
        }
      }
    )
    
    # --- DEAD SERVER CODE REMOVED ---
    
      
    output$render_fig8_composite <- renderUI({
      img_path <- file.path(ZIMA_ROOT, "Figures", "Figure_8_Master_Composite_600DPI.tiff")
      if (file.exists(img_path)) {
        output$fig8_img <- renderImage({
          temp_file <- sub("\\.tiff$", ".png", img_path, ignore.case = TRUE)
          if(!file.exists(temp_file)) {
            img <- image_read(img_path)
            image_write(img, path = temp_file, format = "png")
          }
          list(src = temp_file, contentType = "image/png", style = "width: 100%; max-width: 800px; height: auto; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.3);")
        }, deleteFile = FALSE)
        imageOutput("fig8_img", height = "auto")
      } else {
        tags$div(style = "color: #ef4444; padding: 20px; text-align: center;", "Figure 8 TIFF not found.")
      }
    })
    
    output$render_fig9_composite <- renderUI({
      img_path <- file.path(ZIMA_ROOT, "Figures", "Figure_9_Native_Dual_TimeROC.tiff")
      if (file.exists(img_path)) {
        output$fig9_img <- renderImage({
          temp_file <- sub("\\.tiff$", ".png", img_path, ignore.case = TRUE)
          if(!file.exists(temp_file)) {
            img <- image_read(img_path)
            image_write(img, path = temp_file, format = "png")
          }
          list(src = temp_file, contentType = "image/png", style = "width: 100%; max-width: 800px; height: auto; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.3);")
        }, deleteFile = FALSE)
        imageOutput("fig9_img", height = "auto")
      } else {
        tags$div(style = "color: #ef4444; padding: 20px; text-align: center;", "Figure 9 TIFF not found.")
      }
    })
  
    output$edu_bifurcation_img <- renderUI({ create_edu_fallback("SKCM (Bifurcation)", "SKCM_OS_dfXXX_SHAP_Dependence_2.tiff") })
    
    # --- PRECISION ONCOLOGY (TAB 5) RENDERERS ---
    output$edu_trajectory_text <- renderUI({
      req(input$edu_trajectory_type)
      if (input$edu_trajectory_type == "Lethal Trajectory (LGG DSS: TCGA-HT-7616-01)") {
        div(style = "margin-top: 20px; padding: 15px; background: rgba(0,0,0,0.2); border-radius: 8px;",
            h6(style = "color: #94a3b8; font-size: 0.8rem; text-transform: uppercase;", "HOW TO READ: Lethal Trajectories"),
            p(style = "color: #ef4444; font-size: 0.85rem; font-family: monospace;", "> LETHAL APEX ISOLATED"),
            p(style = "color: #e2e8f0; font-size: 0.85rem;", "Originating at the population baseline (E[f(x)]), the visualized bars directly represent specific multi-omic signatures (both continuous expression and discrete genomic states) acting as vectors of non-proportional hazard. The predictive weight of each signature is defined by its breadth, axis orientation, and top-to-bottom numeric value. Specifically, orange bars represent omic signatures that impose consecutive, aggressive non-proportional hazard penalties, systematically accelerating the patient's prediction forward toward an elevated, terminal f(x).")
        )
      } else {
        div(style = "margin-top: 20px; padding: 15px; background: rgba(0,0,0,0.2); border-radius: 8px;",
            h6(style = "color: #94a3b8; font-size: 0.8rem; text-transform: uppercase;", "HOW TO READ: Protective Trajectories"),
            p(style = "color: #10b981; font-size: 0.85rem; font-family: monospace;", "> PROTECTIVE APEX ISOLATED"),
            p(style = "color: #e2e8f0; font-size: 0.85rem;", "In stark contrast, purple bars directly represent multi-omic signatures acting as vectors of protection. By autonomously flipping its risk evaluation, these signatures force deep negative non-proportional hazard pushes. The width, orientation, and top-to-bottom sequence of these bars structurally shield the patient, plunging their final prognosis f(x) significantly below the population baseline.")
        )
      }
    })
    
    output$render_trajectory_container <- renderUI({
      req(input$edu_trajectory_type)
      
      if (input$edu_trajectory_type == "Lethal Trajectory (LGG DSS: TCGA-HT-7616-01)") {
        img_path <- file.path(ZIMA_ROOT, "Figures", "Figure_5_LGG_DSS_df374_SHAP_Decision_Lethal_Trajectory_TCGA-HT-7616-01.tiff")
        alt_text <- "Lethal Trajectory"
      } else {
        img_path <- file.path(ZIMA_ROOT, "Figures", "Figure_6_LGG_DSS_df374_SHAP_Decision_Protective_Trajectory_TCGA-DU-7008-01.tiff")
        alt_text <- "Protective Trajectory"
      }
      
      if (file.exists(img_path)) {
        output$dynamic_trajectory_img <- renderImage({
          temp_file <- sub("\\.tiff$", ".png", img_path, ignore.case = TRUE)
          if(!file.exists(temp_file)) {
            img <- image_read(img_path)
            image_write(img, path = temp_file, format = "png")
          }
          list(src = temp_file, contentType = "image/png", style = "width: 100%; max-width: 800px; height: auto; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.3);", alt = alt_text)
        }, deleteFile = FALSE)
        
        imageOutput("dynamic_trajectory_img", height = "auto")
      } else {
        tags$div(style = "color: #ef4444; padding: 20px; text-align: center;", bs_icon("exclamation-triangle"), " TIFF asset not found at: ", img_path)
      }
    })
    
    # --- 96-COHORT DROPDOWN ENGINE (TAB 5: PRECISION ONCOLOGY) ---
    observe({
      updateSelectInput(session, "precision_cancer", choices = unique(cohort_matrix$Cancer))
    })
    
    observeEvent(input$precision_cancer, {
      available_metrics <- cohort_matrix$Metric[cohort_matrix$Cancer == input$precision_cancer]
      updateSelectInput(session, "precision_metric", choices = available_metrics)
    })
    
    # Dynamically scan the XGBoost folder for SHAP Trajectory PDFs (Patient-Level)
    observeEvent(c(input$precision_cancer, input$precision_metric), {
      req(input$precision_cancer, input$precision_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$precision_cancer & cohort_matrix$Metric == input$precision_metric, ]
      req(nrow(selected_model) > 0)
      
      folder_name <- selected_model$Full_Name[1]
      trajectory_path <- file.path(zima_drive_path, folder_name, "XGBoost")
      
      if(dir.exists(trajectory_path)) {
        # Target actual patient decision trajectories, not dependencies
        pdf_files <- list.files(trajectory_path, pattern = "_SHAP_Decision_.*Trajectory.*\\.pdf$")
        if(length(pdf_files) > 0) {
          clean_names <- gsub(paste0(folder_name, "_SHAP_Decision_"), "", pdf_files)
          clean_names <- gsub("\\.pdf$", "", clean_names)
          
          # Format "Lethal_Trajectory_TCGA-OR-A5J1-01" to "Lethal Trajectory: TCGA-OR-A5J1-01"
          clean_names <- gsub("_", " ", clean_names)
          clean_names <- sub("Trajectory ", "Trajectory: ", clean_names)
          
          updateSelectInput(session, "precision_signature", choices = setNames(pdf_files, clean_names))
        } else {
          updateSelectInput(session, "precision_signature", choices = c("No trajectories found for this cohort" = ""))
        }
      } else {
        updateSelectInput(session, "precision_signature", choices = c("ZIMA Trajectory Directory Not Found" = ""))
      }
    })
    
    # Render the High-Res Trajectory PDF via iframe
    output$precision_trajectory_container <- renderUI({
      req(input$precision_cancer, input$precision_metric, input$precision_signature)
      if(input$precision_signature == "") return(NULL)
      
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$precision_cancer & cohort_matrix$Metric == input$precision_metric, ]
      req(nrow(selected_model) > 0)
      
      folder_name <- selected_model$Full_Name[1]
      
      # Render the actual Trajectory from XGBoost
      pdf_url <- paste0("zima_models/", folder_name, "/XGBoost/", input$precision_signature, "#zoom=100")
      tags$iframe(src = pdf_url, width = "100%", height = "700px", style = "border: 1px solid #334155; border-radius: 8px;")
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
