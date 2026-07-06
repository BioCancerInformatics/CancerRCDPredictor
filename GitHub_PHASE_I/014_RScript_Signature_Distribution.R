#### Distribution of signatures per cancer type and per omic layer- Analysis induced in response to Reviewer 4
# 📚 Pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)
library(rio)

Dataset_S2 <- import("Dataset_S2.tsv") # full signature database of the manuscript

# 🔢 Ordenar fator Omic feature conforme hierarquia definida
Dataset_S2$`Omic feature` <- factor(
  Dataset_S2$`Omic feature`,
  levels = c("Protein", "Mutation", "CNV", "miRNA", "Transcript", "mRNA", "Methylation")
)

# 🔤 Ordenar CTAB alfabeticamente de cima para baixo no heatmap (ACC no topo, UVM na base)
Dataset_S2$CTAB <- factor(Dataset_S2$CTAB, levels = rev(sort(unique(Dataset_S2$CTAB))))

# 👉 Group 1: Assinaturas por tipo de câncer
signatures_per_cancer <- Dataset_S2 %>%
  group_by(CTAB) %>%
  summarise(Signature_Count = n_distinct(Nomenclature)) %>%
  arrange(desc(Signature_Count))

# 👉 Group 2: Assinaturas por tipo de câncer e camada ômica
signatures_per_cancer_omic <- Dataset_S2 %>%
  group_by(CTAB, `Omic feature`) %>%
  summarise(Signature_Count = n_distinct(Nomenclature), .groups = "drop")

# 👉 Matriz para visualização opcional
signature_matrix <- signatures_per_cancer_omic %>%
  pivot_wider(names_from = `Omic feature`, values_from = Signature_Count, values_fill = 0)

# 🖼️ Heatmap com ordens ajustadas
heatmap_plot <- ggplot(signatures_per_cancer_omic, aes(x = `Omic feature`, y = CTAB, fill = Signature_Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Distribution of Signatures by Cancer Type and Omic Layer",
       x = "Omic Layer", y = "Cancer Type") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 💾 Salvar como TIFF (600 dpi)
tiff("heatmap_output.tiff", width = 8, height = 6, units = "in", res = 600, compression = "lzw")
print(heatmap_plot)
dev.off()

# 💾 Salvar como PDF
pdf("heatmap_output.pdf", width = 8, height = 6)
print(heatmap_plot)
dev.off()


# 📥 Importar tabela de TCGA patients per cancer type
TCGA_patients_CTAB <- import("TCGA_patients_CTAB.txt", format = "tsv", na.strings = "NA")

library(dplyr)

# Add a CTAB column based on 'Detailed category'
TCGA_patients_CTAB <- TCGA_patients_CTAB %>%
  mutate(CTAB = case_when(
    `Detailed category` == "Adrenocortical Cancer" ~ "ACC",
    `Detailed category` == "Bladder Urothelial Carcinoma" ~ "BLCA",
    `Detailed category` == "Breast Invasive Carcinoma" ~ "BRCA",
    `Detailed category` == "Cervical & Endocervical Cancer" ~ "CESC",
    `Detailed category` == "Cholangiocarcinoma" ~ "CHOL",
    `Detailed category` == "Colon Adenocarcinoma" ~ "COAD",
    `Detailed category` == "Diffuse Large B-Cell Lymphoma" ~ "DLBC",  # Not in Dataset_S2 but mapped for completeness
    `Detailed category` == "Esophageal Carcinoma" ~ "ESCA",
    `Detailed category` == "Brain Lower Grade Glioma" ~ "LGG",
    `Detailed category` == "Glioblastoma Multiforme" ~ "GBM",
    `Detailed category` == "Head & Neck Squamous Cell Carcinoma" ~ "HNSC",
    `Detailed category` == "Kidney Chromophobe" ~ "KICH",
    `Detailed category` == "Kidney Clear Cell Carcinoma" ~ "KIRC",
    `Detailed category` == "Kidney Papillary Cell Carcinoma" ~ "KIRP",
    `Detailed category` == "Liver Hepatocellular Carcinoma" ~ "LIHC",
    `Detailed category` == "Lung Adenocarcinoma" ~ "LUAD",
    `Detailed category` == "Lung Squamous Cell Carcinoma" ~ "LUSC",
    `Detailed category` == "Mesothelioma" ~ "MESO",
    `Detailed category` == "Ovarian Serous Cystadenocarcinoma" ~ "OV",
    `Detailed category` == "Pancreatic Adenocarcinoma" ~ "PAAD",
    `Detailed category` == "Pheochromocytoma & Paraganglioma" ~ "PCPG",
    `Detailed category` == "Prostate Adenocarcinoma" ~ "PRAD",
    `Detailed category` == "Rectum Adenocarcinoma" ~ "READ",
    `Detailed category` == "Sarcoma" ~ "SARC",
    `Detailed category` == "Skin Cutaneous Melanoma" ~ "SKCM",
    `Detailed category` == "Stomach Adenocarcinoma" ~ "STAD",
    `Detailed category` == "Testicular Germ Cell Tumor" ~ "TGCT",
    `Detailed category` == "Thymoma" ~ "THYM",
    `Detailed category` == "Thyroid Carcinoma" ~ "THCA",
    `Detailed category` == "Uterine Carcinosarcoma" ~ "UCS",
    `Detailed category` == "Uterine Corpus Endometrioid Carcinoma" ~ "UCEC",
    `Detailed category` == "Uveal Melanoma" ~ "UVM",
    `Detailed category` == "Acute Myeloid Leukemia" ~ "LAML",  # ✅ Newly added
    TRUE ~ NA_character_  # Catch-all for unmatched values
  ))

# 📊 Count number of patients per cancer type
patient_counts <- TCGA_patients_CTAB %>%
  filter(!is.na(CTAB)) %>%
  group_by(CTAB) %>%
  summarise(Patient_Count = n(), .groups = "drop") %>%
  arrange(CTAB)

# ➕ Sum total number of patients
total_patients <- sum(patient_counts$Patient_Count, na.rm = TRUE)

# 🖨️ Output patient count table and total
print(patient_counts)
cat("\n🧮 Total number of patients across all cancer types:", total_patients, "\n")


# 📌 Task 2: Merge patient count and signature count per CTAB and compute correlation

# 🧮 Signature count per cancer type from Dataset_S2
signature_counts <- Dataset_S2 %>%
  group_by(CTAB) %>%
  summarise(Signature_Count = n_distinct(Nomenclature), .groups = "drop")

# 🔄 Ensure CTAB factor levels are consistent
signature_counts$CTAB <- as.character(signature_counts$CTAB)
patient_counts$CTAB <- as.character(patient_counts$CTAB)

# 🔗 Merge the two tables by CTAB (cancer type)
merged_counts <- inner_join(patient_counts, signature_counts, by = "CTAB")

# 💾 Save to TSV
write.table(merged_counts, file = "merged_patient_signatues_per_type_counts.tsv", sep = "\t", quote = FALSE, row.names = FALSE)


# 📊 Compute signature-to-patient ratio per cancer type
merged_counts <- merged_counts %>%
  mutate(Signature_Per_Patient = Signature_Count / Patient_Count) %>%
  arrange(desc(Signature_Per_Patient))

# 🖨️ Print the updated dataframe to console for inspection
print(merged_counts)

# 💾 Optionally save as .tsv for downstream documentation or figure/table annotation
write.table(merged_counts,
            file = "signature_patient_ratio_per_cancer.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE)

# 📈 Compute Spearman's rank correlation
spearman_result <- cor.test(merged_counts$Patient_Count, merged_counts$Signature_Count, method = "spearman")

# 🖨️ Print result
print(merged_counts)
cat("\n🔗 Spearman’s rank correlation:\n")
print(spearman_result)

# 📦 Extract key elements from Spearman correlation result
spearman_df <- data.frame(
  Method = spearman_result$method,
  Alternative = spearman_result$alternative,
  Statistic_S = spearman_result$statistic,
  P_value = spearman_result$p.value,
  Rho = spearman_result$estimate,
  Sample_Size = nrow(merged_counts)
)

# 💾 Save to TSV
write.table(spearman_df, file = "spearman_correlation_result.tsv", sep = "\t", quote = FALSE, row.names = FALSE)

# Extract unique cancer types from each dataframe
ctab_signatures <- sort(unique(Dataset_S2$CTAB))
ctab_patients <- sort(unique(TCGA_patients_CTAB$CTAB))

# Identify CTABs in signatures (Dataset_S2) that are not present in patients (TCGA_patients_CTAB)
missing_in_patients <- setdiff(ctab_signatures, ctab_patients)

# Print result
cat("CTAB values in Dataset_S2 but missing in TCGA_patients_CTAB:", paste(missing_in_patients, collapse = ", "), "\n")

# 📌 All CTAB values from Dataset_S2 (should be 32)
cancer_types_signatures <- sort(unique(Dataset_S2$CTAB))

# 📌 All CTAB values from TCGA_patients_CTAB (should be 32 as well, if mapped correctly)
cancer_types_patients <- sort(unique(TCGA_patients_CTAB$CTAB))

# 📌 Check if any assigned CTABs in TCGA_patients_CTAB are NOT found in Dataset_S2
mismatched_ctabs <- setdiff(cancer_types_patients, cancer_types_signatures)

# 🔍 Display mismatched CTABs
cat("Mismatched or extra CTAB values in TCGA_patients_CTAB:", paste(mismatched_ctabs, collapse = ", "), "\n")

# 📚 Pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)

# 🔢 Ordenar fator Omic feature conforme hierarquia definida
Dataset_S2$`Omic feature` <- factor(
  Dataset_S2$`Omic feature`,
  levels = c("Protein", "Mutation", "CNV", "miRNA", "Transcript", "mRNA", "Methylation")
)

# 🔤 Ordenar CTAB alfabeticamente de cima para baixo no heatmap
Dataset_S2$CTAB <- factor(Dataset_S2$CTAB, levels = rev(sort(unique(Dataset_S2$CTAB))))

# 👉 Distribuição por tipo de câncer e camada ômica
signatures_per_cancer_omic <- Dataset_S2 %>%
  group_by(CTAB, `Omic feature`) %>%
  summarise(Signature_Count = n_distinct(Nomenclature), .groups = "drop")

# 👉 Matriz para visualização
signature_matrix <- signatures_per_cancer_omic %>%
  pivot_wider(names_from = `Omic feature`, values_from = Signature_Count, values_fill = 0)

# 💾 Salvar a matriz como tabela TSV
write.table(signature_matrix,
            file = "signatures_per_cancer_and_omic.tsv",
            sep = "\t", quote = FALSE, row.names = FALSE)

# 🖼️ Heatmap por camada ômica e tipo de câncer
heatmap_plot <- ggplot(signatures_per_cancer_omic, aes(x = `Omic feature`, y = CTAB, fill = Signature_Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Distribution of Signatures by Cancer Type and Omic Layer",
       x = "Omic Layer", y = "Cancer Type") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 💾 Exportar heatmap
tiff("signatures_per_cancer_omic.tiff", width = 8, height = 6, units = "in", res = 600, compression = "lzw")
print(heatmap_plot)
dev.off()

pdf("signatures_per_cancer_omic.pdf", width = 8, height = 6)
print(heatmap_plot)
dev.off()

# 📊 Distribuição agregada por camada ômica (independente do câncer)
signatures_per_omic <- Dataset_S2 %>%
  group_by(`Omic feature`) %>%
  summarise(Signature_Count = n_distinct(Nomenclature)) %>%
  arrange(desc(Signature_Count))

# 💾 Salvar distribuição agregada
write.table(signatures_per_omic,
            file = "signature_distribution_by_omic_layer.tsv",
            sep = "\t", quote = FALSE, row.names = FALSE)

# 🖨️ Mostrar ao console
print(signatures_per_omic)

# 📊 Scatter plot showing relationship between number of patients and number of signatures
scatter_plot <- ggplot(merged_counts, aes(x = Patient_Count, y = Signature_Count, label = CTAB)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "gray50", linetype = "dashed") +
  geom_text(vjust = -0.8, size = 3) +
  labs(
    title = "Correlation Between Number of Patients and Number of Signatures",
    x = "Number of Patients per Cancer Type",
    y = "Number of Signatures"
  ) +
  theme_minimal(base_size = 12)

# 💾 Save as high-resolution TIFF
tiff("scatterplot_patients_vs_signatures.tiff", width = 7, height = 6, units = "in", res = 600, compression = "lzw")
print(scatter_plot)
dev.off()

# 💾 Save as PDF
pdf("scatterplot_patients_vs_signatures.pdf", width = 7, height = 6)
print(scatter_plot)
dev.off()


##### 
##### Figure 4 for reviewer 4 - Normalized Distribution of Multi-Omic Signatures per Patient
##### Distribution of signatures per omic layer per cancer normalized per patient numebr
##### 
##### ##### 
# 📚 Load required packages
library(rio)
library(dplyr)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# 🎨 Define Okabe-Ito palette (color-blind friendly)
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                       "#0072B2", "#D55E00", "#CC79A7")

# 📥 Import datasets
signatures_df <- import("data_df1011.tsv")  # Contains CTAB and Genotype columns
patient_df <- import("TCGA_patients_CTAB.txt", format = "tsv", na.strings = "NA")

# 🧩 Harmonize CTAB mapping in patient data
patient_df <- patient_df %>%
  mutate(CTAB = case_when(
    `Detailed category` == "Adrenocortical Cancer" ~ "ACC",
    `Detailed category` == "Bladder Urothelial Carcinoma" ~ "BLCA",
    `Detailed category` == "Breast Invasive Carcinoma" ~ "BRCA",
    `Detailed category` == "Cervical & Endocervical Cancer" ~ "CESC",
    `Detailed category` == "Cholangiocarcinoma" ~ "CHOL",
    `Detailed category` == "Colon Adenocarcinoma" ~ "COAD",
    `Detailed category` == "Diffuse Large B-Cell Lymphoma" ~ "DLBC",
    `Detailed category` == "Esophageal Carcinoma" ~ "ESCA",
    `Detailed category` == "Brain Lower Grade Glioma" ~ "LGG",
    `Detailed category` == "Glioblastoma Multiforme" ~ "GBM",
    `Detailed category` == "Head & Neck Squamous Cell Carcinoma" ~ "HNSC",
    `Detailed category` == "Kidney Chromophobe" ~ "KICH",
    `Detailed category` == "Kidney Clear Cell Carcinoma" ~ "KIRC",
    `Detailed category` == "Kidney Papillary Cell Carcinoma" ~ "KIRP",
    `Detailed category` == "Liver Hepatocellular Carcinoma" ~ "LIHC",
    `Detailed category` == "Lung Adenocarcinoma" ~ "LUAD",
    `Detailed category` == "Lung Squamous Cell Carcinoma" ~ "LUSC",
    `Detailed category` == "Mesothelioma" ~ "MESO",
    `Detailed category` == "Ovarian Serous Cystadenocarcinoma" ~ "OV",
    `Detailed category` == "Pancreatic Adenocarcinoma" ~ "PAAD",
    `Detailed category` == "Pheochromocytoma & Paraganglioma" ~ "PCPG",
    `Detailed category` == "Prostate Adenocarcinoma" ~ "PRAD",
    `Detailed category` == "Rectum Adenocarcinoma" ~ "READ",
    `Detailed category` == "Sarcoma" ~ "SARC",
    `Detailed category` == "Skin Cutaneous Melanoma" ~ "SKCM",
    `Detailed category` == "Stomach Adenocarcinoma" ~ "STAD",
    `Detailed category` == "Testicular Germ Cell Tumor" ~ "TGCT",
    `Detailed category` == "Thymoma" ~ "THYM",
    `Detailed category` == "Thyroid Carcinoma" ~ "THCA",
    `Detailed category` == "Uterine Carcinosarcoma" ~ "UCS",
    `Detailed category` == "Uterine Corpus Endometrioid Carcinoma" ~ "UCEC",
    `Detailed category` == "Uveal Melanoma" ~ "UVM",
    `Detailed category` == "Acute Myeloid Leukemia" ~ "LAML",
    TRUE ~ NA_character_
  ))

# 🧮 Compute patient count per cancer type
patient_counts <- patient_df %>%
  filter(!is.na(CTAB)) %>%
  group_by(CTAB) %>%
  summarise(Patient_Count = n(), .groups = "drop")

# 🔢 Compute signature counts per CTAB and Genomic Feature
# df_plot <- signatures_df %>%
#   rename(CTAB = `Cancer Type Abbreviation`, Genomic_Feature = Genotype) %>%
#   group_by(CTAB, Genomic_Feature) %>%
#   summarise(Signature_Count = n(), .groups = "drop")


df_plot <- signatures_df %>%
  group_by(CTAB, Genomic_Feature = Genotype) %>%
  summarise(Signature_Count = n(), .groups = "drop")

# 🔗 Merge with patient counts to calculate normalized values
df_plot_normalized <- df_plot %>%
  left_join(patient_counts, by = "CTAB") %>%
  mutate(Signature_Per_Patient = Signature_Count / Patient_Count)

# 🧹 Remove rows with missing patient counts (i.e., unmapped cancer types)
df_plot_normalized <- df_plot_normalized %>% filter(!is.na(Signature_Per_Patient))

# 📊 Plot normalized stacked bar chart (signatures per patient)
signature_per_patient_plot <- ggplot(df_plot_normalized, aes(x = CTAB, y = Signature_Per_Patient, fill = Genomic_Feature)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = okabe_ito_palette) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  ) +
  labs(
    title = "Normalized Distribution of Multi-Omic Signatures per Patient",
    x = "Cancer Type Abbreviation",
    y = "Signature Count per Patient",
    fill = "Genomic Feature"
  )

# 🖼️ Display plot
print(signature_per_patient_plot)

# 💾 Save figure
ggsave("Figure_4_A_Signature_Per_Patient_Normalized.tif", 
       plot = signature_per_patient_plot,
       width = 29, height = 21, units = "cm", dpi = 600)

