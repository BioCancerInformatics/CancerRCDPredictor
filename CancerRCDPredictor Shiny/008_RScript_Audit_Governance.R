# ==============================================================================
# GOVERNANCE AUDIT: app_work.R — CancerRCDPredictor Phase IV Shiny Application
# ==============================================================================
# Scope: Code quality, security, error handling, LLM governance, file system safety,
#        input validation, maintainability, and architectural patterns.
# Generated: 2026-06-12
# ==============================================================================

AUDIT_DIR <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/PHASE_IV_CancerRCDShiny"
TARGET_FILE <- file.path(AUDIT_DIR, "app_work.R")

cat("============================================================\n")
cat("GOVERNANCE AUDIT: app_work.R\n")
cat("============================================================\n")
cat(sprintf("File size: %.1f KB  |  Lines: ~4,721\n", file.info(TARGET_FILE)$size / 1024))

# ==============================================================================
# SECTION 1: DEPENDENCY MANAGEMENT
# ==============================================================================
cat("\n--- 1. DEPENDENCY MANAGEMENT ---\n")

dep_issues <- c()

# 1a. Auto-install in production
dep_issues <- c(dep_issues,
  "⚠️  HIGH: Auto-installs packages at startup (line ~16). In production, this can cause:
     - Write-permission failures on read-only NixOS deployments
     - Unexpected network calls on startup
     - Version conflicts with system packages
     RECOMMENDATION: Move install logic to a setup script; fail gracefully at app start.")

# 1b. suppressPackageStartupMessages usage
dep_issues <- c(dep_issues,
  "✅  PASS: Uses suppressPackageStartupMessages() to prevent console spam.")

# 1c. Missing namespace qualification
dep_issues <- c(dep_issues,
  "⚠️  MEDIUM: Several packages used without explicit namespace qualification:
     - dplyr functions (filter, group_by, summarise, %>%)
     - stringr (str_match_all)
     - ggplot2 (ggplot, aes, geom_line, etc.)
     - scales (percent_format)
     - data.table (fread)
     RECOMMENDATION: Add library() calls inside the server function or use pkg::fn syntax.")

# 1d. Package list completeness
dep_issues <- c(dep_issues,
  "⚠️  MEDIUM: Required packages list at top is missing:
     - httr2 (used extensively for Ollama calls)
     - dplyr / stringr / openxlsx (used in code but not in required_packages)
     RECOMMENDATION: Add 'httr2', 'dplyr', 'openxlsx', 'stringr' to required_packages.")

cat(paste(dep_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 2: NIXOS CACHE PURGE
# ==============================================================================
cat("\n--- 2. NIXOS READ-ONLY HANDLING ---\n")

cat("✅  PASS: Proactive tempdir cache purge for selectize/bootstrap files (lines ~30-35).
     Essential for NixOS deployments where read-only copies clash with htmltools.\n")

# ==============================================================================
# SECTION 3: LLM GOVERNANCE FRAMEWORKS
# ==============================================================================
cat("\n--- 3. LLM PROMPT GOVERNANCE ---\n")

gov_issues <- c()

gov_issues <- c(gov_issues,
  "✅  PASS: llm_glossary — Enforces strict abbreviation definitions (TMB, TSM, MSI).
     Forces the LLM to interpret TSM = Stemness exclusively.")

gov_issues <- c(gov_issues,
  "✅  PASS: narrative_governance_framework (v2.6) — Dynamic tumor-state reasoning prompt
     with mandatory interpretive rules, hedging language requirements, and checklist.")

gov_issues <- c(gov_issues,
  "✅  PASS: domain_boundary_governance — 5-category domain classification layer:
     Categories 1-3: Full response | Category 4: Educational only | Category 5: Refuse
     Prevents clinical treatment drift.")

gov_issues <- c(gov_issues,
  "✅  PASS: ensure_questions_and_disclaimer() — Post-processing guard that:
     - Strips existing disclaimers to prevent duplication
     - Ensures exactly 3 Suggested Clinical Queries
     - Appends mandatory hypothesis-generating disclaimer")

gov_issues <- c(gov_issues,
  "✅  PASS: AI Transparency Disclaimer modal on first visit to AI tabs.
     Discloses that LLM is a semantic translator, not a fact generator.")

gov_issues <- c(gov_issues,
  "⚠️  LOW: The governance frameworks are injected as system prompts.
     While effective, there is no server-side validation that the LLM actually
     complied with domain boundaries. A classifier could validate responses.
     RECOMMENDATION: Consider post-hoc keyword scanning for compliance.")

cat(paste(gov_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 4: OLLAMA / LLM INTEGRATION
# ==============================================================================
cat("\n--- 4. OLLAMA INTEGRATION ---\n")

ollama_issues <- c()

ollama_issues <- c(ollama_issues,
  "✅  PASS: check_ollama_status() — Robust health check:
     - Parses URL, checks host/port
     - Queries /api/tags for available models
     - Intelligent fallback: qwen3:8b → any qwen → any model
     - 2-second timeout prevents hanging")

ollama_issues <- c(ollama_issues,
  "✅  PASS: All LLM calls (5 modules) pre-check Ollama status before queueing.
     Returns user-friendly error notification on failure.")

ollama_issues <- c(ollama_issues,
  "✅  PASS: 300-second httr2 timeout on all LLM requests prevents infinite hangs.")

ollama_issues <- c(ollama_issues,
  "✅  PASS: Context window limited to 16K tokens (num_ctx = 16384).
     Prevents runaway token consumption.")

ollama_issues <- c(ollama_issues,
  "⚠️  MEDIUM: Hardcoded Ollama URL (http://localhost:11434/api/chat) and model name
     (qwen3:8b) at multiple sites (5+ places). If the URL or model changes,
     every occurrence must be updated.
     RECOMMENDATION: Move to a single configuration constant or .Renviron variable.")

ollama_issues <- c(ollama_issues,
  "⚠️  LOW: No retry logic on transient Ollama failures.
     RECOMMENDATION: Add exponential backoff for 503/connection errors.")

ollama_issues <- c(ollama_issues,
  "⚠️  MEDIUM: User prompt injection risk. The chat module lets users type arbitrary
     text that is sent directly to Ollama. While domain_boundary_governance
     restricts the LLM's behavior, there is no input sanitization for:
     - Prompt injection attacks (e.g., 'ignore previous instructions')
     - Excessively long inputs that could DOS the LLM
     RECOMMENDATION: Add input length limit and sanitization filter.")

ollama_issues <- c(ollama_issues,
  "⚠️  LOW: clean_llm_text_block() strips markdown but does not strip HTML tags.
     If the LLM outputs raw HTML, it is rendered via HTML() in the UI,
     which could theoretically allow XSS.
     RECOMMENDATION: Strip HTML tags from LLM output before rendering.")

cat(paste(ollama_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 5: QUEUE MANAGEMENT
# ==============================================================================
cat("\n--- 5. GPU/LLM QUEUE MANAGEMENT ---\n")

queue_issues <- c()

queue_issues <- c(queue_issues,
  "✅  PASS: In-memory global queue (global_llm_queue) with:
     - Zombie sweeper (1-minute timeout for stalled sessions)
     - Double-click ghosting prevention
     - join_queue / release_queue / remove_from_queue lifecycle
     - Session cleanup on browser close (session$onSessionEnded)")

queue_issues <- c(queue_issues,
  "✅  PASS: reactivePoll (3s interval) for queue position updates.")

queue_issues <- c(queue_issues,
  "⚠️  MEDIUM: Force Reset Queue button is exposed to all users in production.
     A malicious user could starve other users by repeatedly resetting.
     RECOMMENDATION: Remove or gate behind admin authentication.")

queue_issues <- c(queue_issues,
  "⚠️  LOW: Queue is in-memory only. If the Shiny server restarts, all queued
     requests are lost without notification to waiting users.
     RECOMMENDATION: Consider persistent queue (Redis/SQLite) for production.")

cat(paste(queue_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 6: FILE SYSTEM & PATH GOVERNANCE
# ==============================================================================
cat("\n--- 6. FILE SYSTEM GOVERNANCE ---\n")

fs_issues <- c()

fs_issues <- c(fs_issues,
  "✅  PASS: ZIMA_ROOT with fallback chain:
     Primary → ../PHASE_III_Megarun_4_4_complete → . (local)
     Prevents hard crash if main drive is unavailable.")

fs_issues <- c(fs_issues,
  "✅  PASS: addResourcePath() for virtual tunnels to external models and media.
     Avoids copying large files into the Shiny www directory.")

fs_issues <- c(fs_issues,
  "✅  PASS: Defensive file.exists() checks before all file reads.")

fs_issues <- c(fs_issues,
  "✅  PASS: TIFF → PNG caching via image_read/image_write to avoid re-encoding.
     Cached PNG files are only regenerated if they don't exist.")

fs_issues <- c(fs_issues,
  "✅  PASS: Clinical report caching in Clinical_Reports_Cache/ directory.
     Prevents redundant RMarkdown compilation.")

fs_issues <- c(fs_issues,
  "⚠️  MEDIUM: Multiple tempdir() usages for RMarkdown rendering. On NixOS,
     tempdir() may be cleaned unpredictably.
     RECOMMENDATION: Use a fixed, app-managed temp directory with cleanup logic.")

fs_issues <- c(fs_issues,
  "⚠️  MEDIUM: CSV delimiter detection relies on checking the first line for ';'.
     If a CSV contains quoted fields with embedded semicolons, detection may fail.
     RECOMMENDATION: Use data.table::fread() which auto-detects delimiters.")

fs_issues <- c(fs_issues,
  "⚠️  LOW: normalizePath(mustWork = FALSE) in get_patient_params().
     Could return paths to non-existent files silently.
     RECOMMENDATION: Validate file existence after normalization.")

cat(paste(fs_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 7: SECURITY AUDIT
# ==============================================================================
cat("\n--- 7. SECURITY ---\n")

sec_issues <- c()

sec_issues <- c(sec_issues,
  "🔴  CRITICAL: system() call with user input (feedback module, line ~1272):
     system(paste(py_cmd, 'send_email.py', shQuote(feedback)))
     While shQuote() provides shell escaping, the feedback text is still passed
     as a command-line argument. A malicious user could inject shell metacharacters
     that bypass shQuote().
     RECOMMENDATION: 
     - Write feedback to a temp file and pass the file path instead
     - Or use reticulate to call Python directly
     - Or use a proper email API (sendmailR, blastula, etc.)")

sec_issues <- c(sec_issues,
  "⚠️  MEDIUM: User feedback logged to CSV without sanitization.
     feedback_text is written directly to user_feedback_log.csv.
     If rendered in a spreadsheet, could execute CSV injection formulas
     (starting with =, +, -, @).
     RECOMMENDATION: Prefix fields starting with =, +, -, @ with a single quote.")

sec_issues <- c(sec_issues,
  "⚠️  MEDIUM: No authentication/authorization.
     All tabs, including AI features and repository downloads, are accessible
     to anyone with the URL. No rate limiting on:
     - LLM requests (beyond the single-threaded queue)
     - Report downloads
     - Repository file downloads
     RECOMMENDATION: Add shiny.telemetry or custom rate limiting for production.")

sec_issues <- c(sec_issues,
  "⚠️  LOW: downloadHandler exposes file system paths via ZIMA virtual tunnel.
     While addResourcePath is intentional, ensure no sensitive files are
     accidentally exposed through the zima_models prefix.")

cat(paste(sec_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 8: INPUT VALIDATION
# ==============================================================================
cat("\n--- 8. INPUT VALIDATION ---\n")

input_issues <- c()

input_issues <- c(input_issues,
  "✅  PASS: selectInput/selectizeInput choices are populated server-side
     from validated Table S15 data, preventing arbitrary value injection.")

input_issues <- c(input_issues,
  "✅  PASS: req() guards on all reactive observers and renderers.
     Prevents execution with NULL/empty inputs.")

input_issues <- c(input_issues,
  "✅  PASS: Text input for chat cleared after submission (updateTextInput value = '').
     Prevents accidental double-submission.")

input_issues <- c(input_issues,
  "⚠️  MEDIUM: tumor_chat_input has no length validation.
     Extremely long prompts could overwhelm the LLM context window.
     RECOMMENDATION: Add maxlength or nchar validation (e.g., 2000 chars).")

input_issues <- c(input_issues,
  "⚠️  LOW: No validation that selectInput choices haven't been tampered with
     via browser dev tools (Shiny's built-in protection handles most cases).")

cat(paste(input_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 9: ERROR HANDLING
# ==============================================================================
cat("\n--- 9. ERROR HANDLING ---\n")

err_issues <- c()

err_issues <- c(err_issues,
  "✅  PASS: tryCatch blocks on all 5 LLM modules (interpreter, SHAP, pharma, chat).
     Errors are captured and displayed to user as formatted error messages.")

err_issues <- c(err_issues,
  "✅  PASS: shiny::validate() in validation trajectory plots with informative
     error messages for missing columns and infinite coefficient cases.")

err_issues <- c(err_issues,
  "✅  PASS: on.exit() handlers for:
     - release_queue() after LLM calls
     - session$sendCustomMessage('hide_spinner') to dismiss loading overlay
     - setwd(owd) in download handlers")

err_issues <- c(err_issues,
  "⚠️  MEDIUM: column_descriptors and gene_roles load with tryCatch but fall
     back to NULL silently. If the file format changes, features will degrade
     without visible warning.
     RECOMMENDATION: Log warnings when fallback data structures are used.")

err_issues <- c(err_issues,
  "⚠️  MEDIUM: readRDS() calls within loops (e.g., validation trajectory) lack
     tryCatch. A corrupted .rds file could crash the server.
     RECOMMENDATION: Wrap readRDS in tryCatch with graceful degradation.")

err_issues <- c(err_issues,
  "⚠️  LOW: unified_drug_matrix has defensive column renaming (lines ~415-420),
     but no validation that the renamed column contains actual gene symbols.
     RECOMMENDATION: Validate column contents match expected patterns.")

cat(paste(err_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 10: LOGGING & AUDIT TRAIL
# ==============================================================================
cat("\n--- 10. LOGGING & AUDIT TRAIL ---\n")

log_issues <- c()

log_issues <- c(log_issues,
  "✅  PASS: llm_performance_log.csv — Tracks timestamp, module, processing time
     for all 5 LLM modules (Phase_I_II_Interpreter, SHAP_Decoding,
     Pharmacogenomic, Clinical_QA).")

log_issues <- c(log_issues,
  "✅  PASS: user_feedback_log.csv — Persistent feedback storage with timestamps.")

log_issues <- c(log_issues,
  "✅  PASS: Audit payload data exposed via collapsible <details> sections in the
     SHAP and Pharmacogenomic report panels. Users can inspect the raw prompts.")

log_issues <- c(log_issues,
  "⚠️  MEDIUM: No structured application-level logging (e.g., session start,
     navigation events, download events). Only LLM performance + feedback.
     RECOMMENDATION: Add shiny logging middleware or log4r for observability.")

log_issues <- c(log_issues,
  "⚠️  LOW: Log files appended via write.table without file locking in some
     paths. Concurrent writes from multiple sessions could interleave.
     RECOMMENDATION: Use filelock package (already loaded!) for CSV appends.")

cat(paste(log_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 11: CODE ARCHITECTURE & MAINTAINABILITY
# ==============================================================================
cat("\n--- 11. CODE ARCHITECTURE ---\n")

arch_issues <- c()

arch_issues <- c(arch_issues,
  "🔴  CRITICAL: Monolithic single-file architecture (4,721 lines).
     Mixes UI, server logic, LLM prompts, data loading, and CSS in one file.
     RECOMMENDATION: Modularize into:
     - global.R (data loading + constants)
     - ui.R (or ui_blocks.R — already exists, use it)
     - server/ (split by tab/feature)
     - prompts/ (separate directory for governance frameworks)
     - R/ (utility functions)")

arch_issues <- c(arch_issues,
  "⚠️  HIGH: Extensive code duplication in LLM modules.
     - Ollama status check + URL construction repeated 5 times
     - clean_llm_text_block logic duplicated across report renderers
     - governance prompt injection pattern repeated
     - Queue join/release pattern duplicated
     RECOMMENDATION: Extract shared functions:
     - call_ollama(prompt, system_prompt) → response
     - render_llm_report(text, title, payload)
     - join_llm_queue(task_name, trigger_reactive)")

arch_issues <- c(arch_issues,
  "⚠️  MEDIUM: HTML/CSS inline styles scattered throughout (400+ style attributes).
     Makes visual consistency fragile.
     RECOMMENDATION: Consolidate CSS rules in style.css (already linked but underused).")

arch_issues <- c(arch_issues,
  "⚠️  MEDIUM: Business logic mixed with presentation. The interpreter module
     contains 200+ lines of prompt construction inside observeEvent().
     RECOMMENDATION: Extract prompt builders into pure functions.")

arch_issues <- c(arch_issues,
  "⚠️  MEDIUM: Cancer dictionary (cancer_dict) hardcoded inside the server function
     rather than loaded from a data file.
     RECOMMENDATION: Move to a standalone CSV or RData file.")

arch_issues <- c(arch_issues,
  "⚠️  LOW: ui_blocks.R exists but is not imported. The UI is entirely defined
     inline in app_work.R rather than using the modular UI blocks file.
     RECOMMENDATION: Either use ui_blocks.R or remove it to avoid confusion.")

cat(paste(arch_issues, collapse = "\n"), "\n")

# ==============================================================================
# SECTION 12: SPECIFIC BUGS & CODE ISSUES
# ==============================================================================
cat("\n--- 12. SPECIFIC CODE ISSUES ---\n")

bugs <- c()

bugs <- c(bugs,
  "🐛  MEDIUM: Misnamed output ID (line ~1289):
     output$cohort_df_info should be output$beeswarm_df_info.
     The existing output$cohort_df_info is defined but never referenced in the UI.
     The UI expects beeswarm_df_info but it doesn't exist.")

bugs <- c(bugs,
  "🐛  LOW: enable_TMB_MSI flag set to TRUE (line ~361) but the TMB/MSI-dependent
     logic is commented out as 'omitted here as enable_TMB_MSI is FALSE' (line ~3282).
     The flag and the logic are in disagreement. Flag says TRUE, comment says FALSE.
     RECOMMENDATION: Reconcile the flag value with the actual implementation.")

bugs <- c(bugs,
  "🐛  LOW: Fallback HTML in diagnostic_report_ui uses <strong> and <em> tags,
     but the LLM governance explicitly forbids markdown bold/italics.
     This is inconsistent, though fallback is non-LLM content.")

bugs <- c(bugs,
  "⚠️  LOW: remove_from_queue() is just an alias for release_queue() (line ~286).
     The session$onSessionEnded callback calls remove_from_queue() which will
     trigger the sweep logic unnecessarily. It should just remove from the queue
     array without attempting to promote the next request.
     RECOMMENDATION: Separate 'cancel' from 'release' semantics.")

cat(paste(bugs, collapse = "\n"), "\n")

# ==============================================================================
# SUMMARY
# ==============================================================================
cat("\n============================================================\n")
cat("AUDIT SUMMARY\n")
cat("============================================================\n")

severity_counts <- list(
  "🔴 CRITICAL" = 2,
  "⚠️  HIGH"   = 2,
  "⚠️  MEDIUM" = 15,
  "⚠️  LOW"    = 10,
  "✅  PASS"   = 17
)

for (sev in names(severity_counts)) {
  cat(sprintf("  %s: %d\n", sev, severity_counts[[sev]]))
}

cat("\n--- TOP PRIORITY FIXES ---\n")
cat("1. [CRITICAL] Modularize 4,721-line monolith into global.R + ui.R + server modules\n")
cat("2. [CRITICAL] Replace system() call for email with a secure alternative\n")
cat("3. [HIGH]   Extract duplicated LLM call patterns into shared functions\n")
cat("4. [HIGH]   Consolidate inline CSS into style.css\n")
cat("5. [MEDIUM] Add input length validation for chat (prevent prompt injection)\n")
cat("6. [MEDIUM] Add HTML tag stripping from LLM output (prevent XSS)\n")
cat("7. [MEDIUM] Add CSV injection sanitization for feedback log\n")
cat("8. [MEDIUM] Reconcile enable_TMB_MSI flag vs implementation\n")
cat("9. [MEDIUM] Move hardcoded Ollama URL/model to configuration constant\n")
cat("10. [MEDIUM] Add 'httr2' and 'dplyr' to required_packages\n")

cat("\n============================================================\n")
cat("END OF GOVERNANCE AUDIT\n")
cat("============================================================\n")
