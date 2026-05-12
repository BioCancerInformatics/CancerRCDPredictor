lines <- readLines("app.R")
# Extract blocks based on known line numbers:
# 1. Welcome (55-90)
# 2. How to Read (92-113)
# 3. Methodological Integrity (115-130)
# 4. Global Impact (133-181)
# 5. Interaction Topologies (184-234)
# 6. Precision Oncology (237-280)
# 7. MVL Performance (283-332)
# 8. Golden 150 (335-350)
# 9. Diagnostic Interpreter (353-380)

# Replace the title "Clinical User Manual" to "Methodological Integrity"
lines[115] <- sub("Clinical User Manual", "Methodological Integrity", lines[115])

welcome_idx <- 55:90
howtoread_idx <- 92:113
method_idx <- 115:130
global_idx <- 133:181
interact_idx <- 184:234
precision_idx <- 237:280
mvl_idx <- 283:332
golden_idx <- 335:350
diag_idx <- 353:380

# New Order: Welcome, How to Read, Methodological, MVL, Global, Interaction, Golden, Precision, Diag
new_ui_content <- c(
  lines[1:54],
  lines[welcome_idx],
  "  ",
  lines[howtoread_idx],
  "  ",
  lines[method_idx],
  "  ",
  lines[mvl_idx],
  "  ",
  lines[global_idx],
  "  ",
  lines[interact_idx],
  "  ",
  lines[golden_idx],
  "  ",
  lines[precision_idx],
  "  ",
  lines[diag_idx],
  lines[381:length(lines)]
)

writeLines(new_ui_content, "app.R")
