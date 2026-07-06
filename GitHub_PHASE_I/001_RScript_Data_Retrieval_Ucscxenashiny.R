# ENRIQUE MEDINA-ACOSTA - UENF
# UCSCXenaShiny
# Installed (last) version UCSCXenaShiny v2.0.0 based on UCSCXenaTools v1.4.8 
# Manual package source
shell.exec("https://cran.r-project.org/web/packages/UCSCXenaShiny/UCSCXenaShiny.pdf")
# UCSCXenaShiny development tutorials(231119)
shell.exec("https://lishensuo.github.io/posts/program/300ucscxenashiny-dev-tutorials--231119")
#### Code
shell.exec("https://github.com/openbiox/UCSCXenaShiny.git")
# Last update -------------------------------------------------------------
# 25/09/2023

#### Installing the latest UCSCXenaShiny version from GitHub (with following command in R console)#### 
remotes::install_github("openbiox/UCSCXenaShiny", force = TRUE, dependencies = TRUE)

####### Alternatively, download the last version and install it:
shell.exec("https://github.com/openbiox/UCSCXenaShiny/releases")

shell.exec("https://github.com/IOBR/IOBR")
##IOBR is an R package to perform comprehensive analysis of tumor microenvironment and signatures 
##for immuno-oncology.
## shell.exec(*https://iobr.github.io/book/*)
library(UCSCXenaShiny)
library(UCSCXenaTools)

app_run()
# Note, from UCSCXenaShiny developers, starting 04/18/2023 the package "ggradar" turned incompatible with radar plot utilities.

##### NOte change in cnv dataset default only in R not in the UCSC v1 or v2
##### in v2 the default dataset is (gistic2_thresholded), whereas in v2 is  (gistic2) 
#The main reason for the difference is due to the default CNV datasets. 
# In the old version, we used the TCGA pan-cancer gene-level copy number (gistic2_thresholded) dataset 
# for KM survival analysis in the v1 Shiny module. 
# In the new version, we use the TCGA pan-cancer gene-level copy number (gistic2) dataset for KM survival analysis in the v2 Shiny module.

#Although the default datasets cannot be changed in the v2 Shiny TPC modules, users can easily select a dataset of interest for the downstream analysis using R code in RStudio.

# With your example data, the expected codes are below.

opt_pancan = .opt_pancan
opt_pancan$toil_cnv$use_thresholded_data = TRUE

data <- tcga_surv_get(
  item = "(A2M + AGTR1 + CCND3 + CD163 + CYP1B1 + EDN1 + GHSR + GPT + IL15RA + MUC4 + PRL + SDC2 + TIMP2 + TMC8 + YBX3 + ZFP36L2)", # Gene or protein identifier
  TCGA_cohort = "BRCA", # TCGA BRCA cohort (breast cancer)
  profile = "methylation", # Molecular profile (in this case, CNV)
  TCGA_cli_data = dplyr::full_join(load_data("tcga_clinical"), load_data("tcga_surv"), by = "sample"),
  opt_pancan = opt_pancan
)

tcga_surv_plot(
  data = data, # The subset of data returned by tcga_surv_get
  time = "OS.time", # Time column
  status = "OS", # Status column
  cutoff_mode = "Auto", # Custom cutoff mode
  cnv_type = c("Duplicated", "Normal", "Deleted"), # Types of CNV
  profile = "cnv" # Molecular profile (CNV)
)


#PS: The minor difference between Shiny app and R codes is due to the filter operation in shiny app, which will discard some samples without eligible clinical metadata.he main reason for the difference is due to the default CNV datasets. In the old version, we used the TCGA pan-cancer gene-level copy number (gistic2_thresholded) dataset for KM survival analysis in the v1 Shiny module. In the new version, we use the TCGA pan-cancer gene-level copy number (gistic2) dataset for KM survival analysis in the v2 Shiny module.

library(UCSCXenaShiny)
library(UCSCXenaTools)
  
    opt_pancan = .opt_pancan
    opt_pancan$toil_cnv$use_thresholded_data = TRUE
    
        data <- tcga_surv_get(
          item = "USP4", # Gene or protein identifier
          TCGA_cohort = "KICH", # TCGA BRCA cohort (breast cancer)
          profile = "cnv", # Molecular profile (in this case, CNV)
          TCGA_cli_data = dplyr::full_join(load_data("tcga_clinical"), load_data("tcga_surv"), by = "sample"),
          opt_pancan = opt_pancan
        )
        
        tcga_surv_plot(
          data = data, # The subset of data returned by tcga_surv_get
          time = "OS.time", # Time column
          status = "OS", # Status column
          cutoff_mode = "Auto", # Custom cutoff mode
          cnv_type = c("Duplicated", "Normal", "Deleted"), # Types of CNV
          profile = "cnv" # Molecular profile (CNV)
        )
  

library(UCSCXenaShiny)
library(UCSCXenaTools)
library(dplyr)
app_run()


q <- vis_unicox_tree(
  Gene = "MYC",
  measure = "OS",
  data_type = "cnv",
  #use_optimal_cutoff = TRUE,
  values = c("grey", "#E31A1C", "#377DB8"),
  opt_pancan = .opt_pancan
)

plot(q)


p <- vis_unicox_tree(
  Gene = "USP4",
  measure = "OS",
  data_type = "cnv",
  threshold = 0.5,
  values = c("grey", "#E31A1C", "#377DB8"),
  opt_pancan = .opt_pancan
)

plot(p)

###### Function to print out an html UCSCXenaShiby report
getwd()
if (!requireNamespace("IOBR", quietly = TRUE))
  devtools::install_github("IOBR/IOBR")

meudeusito7 <- mol_quick_analysis("ENST00000616053", "transcript", out_dir = "N:/4 UENF/Pré-projeto IC João Pedro Vieira Rangel/Machine learning gender designation in R/PANoptosis project/Pré-artigo 5-optosis model", out_report = TRUE)


meudeusito6 <- mol_quick_analysis(("CAMSAP2 + DVL1 + KLHL22 + NCAPD3 + SCAP + SRMS"), "mRNA", out_dir = "N:/4 UENF/Pré-projeto IC João Pedro Vieira Rangel/Machine learning gender designation in R/PANoptosis project/Pré-artigo 5-optosis model", out_report = TRUE)


# Please remove the ggradar package:
# remove.packages("ggradar")
# install the previous ggradar version:
# test ggradar using the following code.

library(ggradar)
library(tidyverse)
# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(conflicted)
library(dplyr)


    library(UCSCXenaShiny)
    library(UCSCXenaTools)
#app_run()
p = vis_gene_stemness_cor(
  # Gene = "(`MT-ATP8` + `MT-ATP6` + `MT-CO1` + `MT-CO2` + `MT-CO3` + `MT-CYB` + `MT-ND1` + `MT-ND2` + `MT-ND3` + `MT-ND4L` + `MT-ND4` + `MT-ND5` + `MT-ND6`)", 
  # Gene = "RICTOR",
  Gene = ("TP53 + 2 * KRAS - 1.3 * PTEN"),
  cor_method = "spearman",
  data_type = "mRNA",
  Plot = "TRUE"
)
pdata <- p$data %>%
  dplyr::mutate(cor = round(cor, digits = 3), p.value = round(p.value, digits = 3))
df <- pdata %>%
  select(cor, cancer) %>%
  pivot_wider(names_from = cancer, values_from = cor)
df$gene <- "Gene signature"
df<-df[,c(34,1:33)]
df

ggradar::ggradar(
  df[1, ],
  font.radar = "sans",
  values.radar = c("-1", "0", "1"),
  grid.min = -1, grid.mid = 0, grid.max = 1,
  # Background and grid lines
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  # Polygons
  group.line.width = 1,
  group.point.size = 3,
  group.colours = "#00AFBB",
) + theme(plot.title = element_text(hjust = .5)) 

# devtools::session_info()
# 
devtools::install_github("ricardo-bion/ggradar@345535f", force = TRUE)
library(ggradar)

##### Check functionality online in the Chinese server ####
shell.exec("https://shiny.hiplot.cn/ucsc-xena-shiny/")
shell.exec("https://shixiangwang.shinyapps.io/ucscxenashiny/")

# CRAN packages 
if(!require(shiny)) install.packages("shiny")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tibble)) install.packages("tibble")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(rlang)) install.packages("rlang")
if(!require(ggplotify)) install.packages("ggplotify")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(patchwork)) install.packages("patchwork")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(pheatmap)) install.packages("pheatmap")
if(!require(devtools)) install.packages("devtools")
if(!require(remotes)) install.packages("remotes")
if(!require(DT)) install.packages("DT")
if(!require(shinyFiles)) install.packages("shinyFiles")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(tm)) install.packages("tm")
if(!require(rmarkdown)) install.packages("rmarkdown")

# Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if(!require(DESeq2)) BiocManager::install("DESeq2")
if(!require(apeglm)) BiocManager::install("apeglm")
if(!require(maftools)) BiocManager::install("maftools")
if(!require(clusterProfiler)) BiocManager::install("clusterProfiler")
if(!require(GSEAmining)) BiocManager::install("GSEAmining")
if(!require(GSEABase)) BiocManager::install("GSEABase")
if(!require(GSVA)) BiocManager::install("GSVA")
if(!require(SummarizedExperiment)) BiocManager::install("SummarizedExperiment")
if(!require(BSgenome)) BiocManager::install("BSgenome")
if(!require(BSgenome.Hsapiens.UCSC.hg19)) BiocManager::install("BSgenome.Hsapiens.UCSC.hg19")
if(!require(BSgenome.Hsapiens.UCSC.hg38)) BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
if(!require(BSgenome.Mmusculus.UCSC.mm10)) BiocManager::install("BSgenome.Mmusculus.UCSC.mm10")
if(!require(BSgenome.Mmusculus.UCSC.mm39)) BiocManager::install("BSgenome.Mmusculus.UCSC.mm39")
if(!require(DO.db)) BiocManager::install("DO.db")
if(!require(GO.db)) BiocManager::install("GO.db")

# Datasets source TCGA: http://xena.ucsc.edu/
# TCGA dictionary terms at https://www.cancer.gov/publications/dictionaries/cancer-terms
# shell.exec("https://www.cancer.gov/publications/dictionaries/cancer-terms")

# setwd("O:/UCSCXenaShiny Rdocumentation")
# getwd()

#### Short cut if all is ready - call library (run every time)#### 
library(UCSCXenaShiny)
library(UCSCXenaTools)
# dev.off() # to eliminate any plot in the memory
# launch app
app_run()

# It opens app in browser (run every time)
# It will open the browser and run the analysis, for example <Listening on http://127.0.0.1:6729>
# Note: do not run simultaneously the browser and script in R because the plot functions will not work properly


# Ensure TIL_signatures is a character vector
TIL_signatures <- as.character(TIL_signatures)

# Use strsplit on TIL_signatures
# ... your code to use strsplit ...

# Increase timeout for download.file
options(timeout = 300)  # Setting timeout to 300 seconds (5 minutes)

# Now, attempt to download the file again
#download.file(data_url, data_path)

# shell.exec("https://openbiox.github.io/UCSCXenaShiny/index.html")

# Note 1: inactivate temporally your antivirus, specially if it is Avast antivirus.
# 1: All         
# 2: CRAN packages only     
# 3: None         
# 4: ps   (1.7.0 -> 1.7.1) [CRAN]
# 5: processx  (3.6.0 -> 3.6.1) [CRAN]
# 6: car   (3.0-13 -> 3.1-0) [CRAN]
# 7: UCSCXenaT... (1.4.7 -> 1.4.8) [CRAN]

#### UCSCXenaShiny Extra Data Repository#### 
# shell.exec("https://zenodo.org/record/5548587# .YrDUoqjMLDc")

#### Datasource UCSCXenaShiny: https://xenabrowser.net/datapages / or https://xena.hiplot.com.cn/datapages/#### 

# UCSCXenaShiny APP - Interactive Analysis of UCSC Xena Data

#### Shell Open an URL file in your browser#### 

# shell.exec("https://github.com/openbiox/UCSCXenaShiny")

# Report issues: 

shell.exec('https://github.com/openbiox/UCSCXenaShiny/issues')

#### Queries and issues. Bug report#### 
shell.exec("https://github.com/openbiox/UCSCXenaShiny/issues")
shell.exec("https://xenabrowser.net/datapages/?dataset=StemnessScores_RNAexp_20170127.2.tsv&host=https://pancanatlas.xenahubs.net")

### Data surce for stemness subtypes
shell.exec("https://xenabrowser.net/datapages/?dataset=StemnessScores_RNAexp_20170127.2.tsv&host=https://pancanatlas.xenahubs.net")

#### Install R documentation update#### 
# shell.exec("https://cran.r-project.org/bin/windows/base/")
# New! R 4.2.0 for Windows (87 megabytes, 32/64 bit) 
# shell.exec("from https://cran.fiocruz.br/")

#### Download RStudio Desktop #### 
# shell.exec("https://www.rstudio.com/products/rstudio/download/")

#### Install base packages. Note: Done only once in your PC, unless there is persistent failure to execute #### 

# A Shiny application is simply a directory containing a user-interface 
# definition, a server script, and any additional data, scripts, or other
# resources required to support the application.

# Check also:
# shell.exec("https://rdrr.io/cran/UCSCXenaShiny/")

# UCSCXenaShiny documentation built on Nov. 17, 2021, 9:06 a.m

# UCSCXenaShiny: An R/CRAN Package for Interactive Analysis of UCSC Xena Data

#### SOURCE: S. Wang, Y. Xiong, L. Zhao, K. Gu, Y. Li, F. Zhao, et al.#### 
# Bioinformatics 2021 
# Accession Number: 34323947 DOI: 10.1093/bioinformatics/btab561
# https://www.ncbi.nlm.nih.gov/pubmed/34323947# install <date: 05/11/2021 supervision JuanCa) (only run once)

#### Keep in mind the color codes at <https://www.color-hex.com/>#### 

#### Cancer Cell Line Encyclopedia (CCLE) - Broad Institute <https://sites.broadinstitute.org/ccle/>#### 

#### install packages, plot functions and calling libraries. Note: done every time you run the libraries#### 
# NOTE: when "permission denied" message pups-up, then inactivate temporally your antivirus,
# specially if it is Avast antivirus.
# dev.off() # to eliminate any plot in the memory


# install.packages("rlang", version = "1.1.0", dependencies = TRUE)
# install.packages("rlang")
# library(rlang)

# install.packages("devtools")
devtools::install_github("jespermaag/gganatogram", force = TRUE) # force is to bypass administrator privilege.
# 
library(gganatogram)
  # install.packages('tidyverse')
# devtools::install_github("tidyverse/tidyr")
# remove.packages("cli")
# install.packages("cli")
# install.packages("ggstatsplot")
# install.packages("remotes", force = TRUE) # force is to bypass administrator privilege.
# install.packages("Rtools")
# install.packages("gganatogram")
# install.packages("ggpolypath")
# install.packages("ggsci")
# install.packages("ggplot2")
# install.packages("RColorBrewer") # https://cran.r-project.org/web/packages/RColorBrewer/index.html
# install.packages("ggrepel")
# install.packages("covr")
# install.packages("DT")
# install.packages("furrr")
# install.packages("future")
# install.packages("knitr")
# install.packages("pacman")
# install.packages("plyr")
# install.packages("plotly")
# install.packages("rmarkdown")
# install.packages("scales")
# install.packages("survival")
# install.packages("survminer")
# install.packages("testthat")
# install.packages("fmsb")
# install.packages("rms")
# install.packages("glmnet")
# install.packages("pivot_wider") # Note: package ‘pivot_wider’ is not available for this version of R
# install.packages("ggfortify")
# install.packages("ggradar") # Note: package 'ggradar' is not available for this version of R
# install.packages("grid") # package ‘grid’ is a base package, and should not be updated
# install.packages("debug") # Note: package ‘debug’ is not available for this version of R
# install.packages("dplyr")
# install.packages("UpSetR")
# 
# 
# install.packages("rlang", version = "1.1.0", dependencies = TRUE)

library(scales)
library(ggsci)
# install.packages("rlang")
library(rlang)
library(ggstatsplot)
library(ggplot2)
library(cowplot)
library(gganatogram)
library(ggpolypath)
library(ggradar)
library(pacman)
library(fmsb)
library(rms)
library(glmnet)
library(grid)
# library(debug) # Error in library(debug) : there is no package called ‘debug’
library(dplyr)
# mtrace(lapply)
library(vctrs) 
library(UpSetR)
# pacman::p_load(UCSCXenaShiny,UCSCXenaTools,ggsci,ggstatsplot,ggplot2,cowplot,gganatogram,ggpolypath,ggradar,fmsb,rms,glmnet)

#### TCGA study abbreviations - pancer types#### 
# shell.exec("https://gdc.cancer.gov/resources-tcga-users/tcga-code-tables/tcga-study-abbreviations")

### You can cite the ggstatsplot package as:
### Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
### Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.0316

# READ Package 'UCSCXenaShiny' file in PDF in the working directory, courtesy of Leonardo

#### UCSCXenaShiny Functions and Data#### 
# IMPORTAT online - All exported data and functions are organized at here <https://openbiox.github.io/UCSCXenaShiny/reference/index.html>
# Note: there are (more than) n=51 function applications. Most of them are listed
# in the supplementary material of the in the Bioinformatics 2021 article. 
# analyze_gene_drug_response_asso	Analyze Association between Gene (Signature) and Drug...
# analyze_gene_drug_response_diff	Analyze Difference of Drug Response (IC50 Value (uM)) between...
# app_run	Run UCSC Xena Shiny App
# available_hosts	Show Available Hosts
# ccle_absolute	ABSOLUTE Result of CCLE Database
# ccle_info	Phenotype Info of CCLE Database
# ezcor	Run Correlation between Two Variables and Support Group by a...
# ezcor_batch	Run correlation between two variables in a batch mode and...
# ezcor_partial_cor	Run partial correlation
# get_pancan_value	Fetch Identifier Value from Pan-cancer Dataset
# keep_cat_cols	Keep Only Columns Used for Sample Selection
# load_data	Load Dataset Provided by This Package
# pcawg_info	Phenotype Info of PCAWG Database
# pcawg_purity	Purity Data of PCAWG
# pipe	Pipe Operator
# query_molecule_value	Get Molecule or Signature Data Values from Dense (Genomic)...
# query_pancan_value	Query Single Identifier or Signature Value from Pan-cancer...
# query_toil_value_df	Obtain ToilHub Info for Single Molecule
# tcga_clinical	Toil Hub: TCGA Clinical Data
# tcga_genome_instability	TCGA: Genome Instability Data
# tcga_gtex	Toil Hub: Merged TCGA GTEx Selected Phenotype
# TCGA.organ	TCGA: Organ Data
# tcga_purity	TCGA: Purity Data
# tcga_subtypes	TCGA Subtype Data
# tcga_surv	Toil Hub: TCGA Survival Data
# tcga_surv_analysis	TCGA Survival Analysis
# tcga_tmb	TCGA: TMB (Tumor Mutation Burden) Data
# toil_info	Toil Hub: TCGA TARGET GTEX Selected Phenotype

#### To create an print pdf (alternatively, one can save the plot by exporting via Export PDF)#### 
# It made accuse error because it saves a PDF but empty file, then run dev.off() para fechar os pdf abertos. Ai terminar? de salver

pdf(file = 'teste_exemplo.pdf')
vis_gene_immune_cor(
 Gene = "(GRB10 + RUNX1)",
 cor_method = "pearson",
 data_type = "mRNA",
 Immune_sig_type = "Cibersort",
 Plot = "TRUE"
)

### Two component PCAnalysis
p <-  vis_dim_dist(
  ids = c("TP53", "KRAS", "PTEN", "MDM2", "CDKN1A"),
  cancer = "BRCA",
  group = "Gender",
  group_levels = NULL
)
p

# dev.off() para apagar da mem?ria um plot e poder plotar outros, pois as vezes n?o plota
# a fun??o anatomy

dev.off()

#### UCSCXenaShiny	Xena Shiny App functions#### 
# vis_ccle_gene_cor	Visualize CCLE Gene Expression Correlation
# vis_ccle_tpm	Visualize CCLE Gene Expression
# vis_gene_cor	Visualize Gene-Gene Correlation in TCGA
# vis_gene_cor_cancer	Visualize Gene-Gene Correlation in a TCGA Cancer Type
# vis_gene_drug_response_asso	Visualize Gene and Drug-Target Association with CCLE Data
# vis_gene_drug_response_diff	Visualize Gene and Drug Response Difference with CCLE Data
# vis_gene_immune_cor	Heatmap for Correlation between Gene and Immune Signatures
# vis_gene_msi_cor	Visualize Correlation between Gene and MSI (Microsatellite...
# vis_gene_stemness_cor	Visualize Correlation between Gene and Tumor Stemness
# vis_gene_TIL_cor	Heatmap for Correlation between Gene and Tumor Immune...
# vis_gene_tmb_cor	Visualize Correlation between Gene and TMB (Tumor Mutation...
# vis_identifier_cor	Visualize Identifier-Identifier Correlation
# vis_identifier_grp_comparison	Visualize Comparison of an Molecule Identifier between Groups
# vis_identifier_grp_surv	Visualize Identifier Group Survival Difference
# vis_identifier_multi_cor	Visualize Correlation for Multiple Identifiers
# vis_pancan_anatomy	Visualize Single Gene Expression in Anatomy Location
# vis_pcawg_dist	Visualize molecular profile in PCAWG
# vis_pcawg_gene_cor	Visualize Gene-Gene Correlation in TCGA
# vis_pcawg_unicox_tree	Visualize Single Gene Univariable Cox Result in PCAWG
# vis_toil_TvsN	Visualize Pan-cancer TPM (tumor (TCGA) vs Normal (TCGA &...
# vis_toil_TvsN_cancer	Visualize Gene TPM in Single Cancer Type (Tumor (TCGA) vs...
# vis_unicox_tree	Visualize Single Gene Univariable Cox Result from Toil Data... 

## search in "help" panel for details of each function and to copy the script for that aprticualr function
## type in R the first letters and select the function you want from the rolling dowb dysplay
## for example, type in > vis_ para selecionar uma fun??o de visualiza??o

# This function closes the specified plot (by default the current device) and if it is an imguR device, uploads the plots for web hostingto shut a plot device 
# dev.off 
## dev. off shuts down the specified (by default the current) device. If the current device is shut down and any other devices
### are open, the next open device is made current.


#### Example tests##### 
# example signature with complex gene name# 
# vis_gene_TIL_cor(
# Gene = ("`MT-ND1` + `MT-CO1` + `MT-CO2`"), # may exclude the () in the formula
# cor_method = "spearman",
# data_type = "mRNA",
# sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
# "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
# Plot = "TRUE"
# )

#### Example tests#### 
# Test vis_unicox_tree() to visualize Single Gene Univariable Cox Result from Toil Data Hub
vis_unicox_tree(
 Gene = "GLIS3",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("# 1ad2e3", "# E31A1C", "# 377DB8")
)

## Test gene/loci alvo (UMODL1-AS1, UMODL1, LINC00319, LINC01423) 
vis_unicox_tree(
Gene = "STX18-AS1",
measure = "OS",
data_type = "mRNA",
threshold = 0.5,
values = c("grey", "# E31A1C", "# 377DB8")
)

dev.off()

## Test
vis_unicox_tree(
 Gene = "LINC01423",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_unicox_tree(
 Gene = "LINC00319",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)


# Test
vis_unicox_tree(
 Gene = "UMODL1",
 measure = "OS",
 data_type = "methylation",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)


# Test
vis_unicox_tree(
 Gene = "UMODL1-AS1",
 measure = "OS",
 data_type = "methylation",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_unicox_tree(
 Gene = "LINC00319",
 measure = "OS",
 data_type = "methylation",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
# vis_pcawg_unicox_tree()
## Visualize Single Gene Univariable Cox Result in PCAWG

vis_pcawg_unicox_tree(
 Gene = "TP53",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_pcawg_unicox_tree(
 Gene = "UMODL1",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_pcawg_unicox_tree(
 Gene = "LINC00319",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_pcawg_unicox_tree(
 Gene = "LINC01423",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_pcawg_unicox_tree(
 Gene = "UMODL1-As1",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# vis_identifier_multi_cor()

### vis_gene_cor_cancer() 
# Note: Visualize Correlation for Multiple Identifiers

## Not run
vis_identifier_multi_cor(
 dataset,
 ids,
 samples = NULL,
 matrix.type = c("full", "upper", "lower"),
 type = c("parametric", "nonparametric", "robust", "bayes"),
 partial = FALSE,
 sig.level = 0.05,
 p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
      "none"),
 color_low = "# E69F00",
 color_high = "# 009E73"
)

# Example 1
dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
ids <- c("TP53", "KRAS", "PTEN")
vis_identifier_multi_cor(dataset, ids)

# Example 2
dataset <- "gtex_RSEM_Hugo_norm_count"
ids <- c("TP53", "KRAS", "PTEN")
vis_identifier_multi_cor(dataset, ids)


## Test
dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
ids <- c("UMODL1-AS1", "UMODL1", "LINC00319", "LINC01423")
vis_identifier_multi_cor(dataset, ids)

## Test
dataset <- "TcgaTargetGtex_rsem_isoform_tpm"
ids <- c("UMODL1-AS1", "UMODL1", "LINC00319", "LINC01423")
vis_identifier_multi_cor(
 dataset,
 ids,
 samples = NULL,
 matrix.type = c("full"),
 type = c("bayes"),
 partial = FALSE,
 sig.level = 0.05,
 p.adjust.method = c("fdr"),
 color_low = "# E69F00",
 color_high = "# 009E73"
)

## Test
dataset <- "gtex_RSEM_Hugo_norm_count"
ids <- c("UMODL1-AS1", "UMODL1", "LINC00319", "LINC01423")
vis_identifier_multi_cor(dataset, ids)

## Test
dataset <- "gtex_RSEM_Hugo_norm_count"
ids <- c("UMODL1-AS1", "UMODL1", "LINC00319", "LINC01423")
vis_identifier_multi_cor(
 dataset,
 ids,
 samples = NULL,
 matrix.type = c("full"),
 type = c("bayes"),
 partial = FALSE,
 sig.level = 0.05,
 p.adjust.method = c("fdr"),
 color_low = "# E69F00",
 color_high = "# 009E73"
)

## Note: read information and examples in help panel
# Visualize Gene-Gene Correlation in a TCGA Cancer Type, example: GBM

# Test
vis_gene_cor_cancer(
 Gene1 = "NFKB2",
 Gene2 = "NFKBIB",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 purity_adj = TRUE,
 cancer_choose = "LUSC",
 use_regline = TRUE,
 cor_method = "spearman",
 use_all = FALSE,
 alpha = 0.5,
 color = "# 10b516"
)

# vis_gene_cor() Note: Visualize Gene-Gene Correlation in TCG in TCGA PANCAN dataset
### data_typeschoose gene profile type for the first gene, including
## "miRNA","protein" generate error message related to purity adjustment - to investigate

# Test
vis_gene_cor(Gene1 = "CSF1R", Gene2 = "JAK3",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 use_regline = TRUE,
 purity_adj = TRUE,
 alpha = 0.5,
 color = "# 10b516",
 filter_tumor = TRUE
)

### vis_gene_immune_cor() 
# Note 1: Heatmap for Correlation between Gene and Immune Signatures
## Note2: pode mudar nome de gene, tipo de data, assinatura immune e m?todo estat?stico

# Test
vis_gene_immune_cor(
 Gene = "GRB10",
 cor_method = "pearson",
 data_type = "mRNA",
 Immune_sig_type = "Cibersort",
 Plot = "TRUE"
)

#### Example tests#### 
# vis_pancan_anatomy() 
# Note1: Visualize Single Gene Expression in Anatomy Location
# Nota 2: para plotar os n?veis de express?o nos tecidos normal (GTEx) versus cancer (TCGA)

# vis_pancan_anatomy()
# Compare Gene Expression Level in Different Anatomic Regions

# This function needs gganatogram package, which is not on CRAN. Please install it before using this function.
if (require("gganatogram")) {
 vis_pancan_anatomy(Gene = "TP53", Gender = c("Female", "Male"), option = "D")
}

## or
vis_pancan_anatomy(
 Gene = "UMODL1-AS1",
 Gender = c("Female", "Male"),
 data_type = "mRNA",
 option = "D"
)

# Test
## vis_pancan_anatomy()
vis_pancan_anatomy(
 Gene = "UMODL1",
 Gender = c("Female", "Male"),
 data_type = "mRNA",
 option = "D"
)

# Test
## vis_pancan_anatomy()
vis_pancan_anatomy(
 Gene = "LINC01423",
 Gender = c("Female", "Male"),
 data_type = "mRNA",
 option = "D"
)

# Test
# vis_gene_cor()
vis_gene_cor(
 Gene1 = "UMODL1-AS1",
 Gene2 = "UMODL1",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 use_regline = TRUE,
 purity_adj = TRUE,
 alpha = 0.5,
 color = "# 350080",
 filter_tumor = TRUE
)

# Test
# vis_gene_immune_cor()
vis_gene_immune_cor(
 Gene = "TP53",
 cor_method = "spearman",
 data_type = "mRNA",
 Immune_sig_type = "Cibersort",
 Plot = "TRUE"
)

# Test
# vis_pancan_anatomy()
vis_pancan_anatomy(
 Gene = "UMODL1-AS1",
 Gender = c("Female", "Male"),
 data_type = "mRNA",
 option = "E"
)

# Test
# dev.off() para limpar os dados de navega??o de plots
dev.off()


# Test
### vis_gene_cor_cancer()
vis_gene_cor_cancer(
 Gene1 = "DIRAS3",
 Gene2 = "GLIS3",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 purity_adj = TRUE,
 cancer_choose = "GBM",
 use_regline = TRUE,
 cor_method = "pearson",
 use_all = FALSE,
 alpha = 0.5,
 color = "# 000000"
)

# Test
# vis_gene_TIL_cor()
vis_gene_TIL_cor(
 Gene = "UMODL1",
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

# Test
vis_gene_TIL_cor(
 Gene = "LINC00319",
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

# Test
vis_gene_TIL_cor(
 Gene = "LINC01423",
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

# Test
vis_gene_TIL_cor(
 Gene = "RNU5D-1",
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

## Not run: 
p <- vis_gene_TIL_cor(Gene = "LInC01423")

## End(Not run)

# Test
## vis_gene_tmb_cor() Note> Visualize Correlation between Gene and TMB (Tumor Mutation Burden)
vis_gene_tmb_cor(
 Gene = "LINC01423",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)

# Test
vis_gene_tmb_cor(
 Gene = "LINC00319",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)

# Test
vis_gene_tmb_cor(
 Gene = "UMODL1",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)

# Test
vis_gene_tmb_cor(
 Gene = "UMODL1-AS1",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)

# Test
vis_gene_tmb_cor(
 Gene = "TP53",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)


# Test
## vis_toil_Tvs() Note:Visualize Pan-cancer TPM (tumor (TCGA) vs Normal (TCGA & GTEx))
vis_toil_TvsN(
 Gene = "UMODL1-AS1",
 Mode = c("Boxplot"),
 data_type = "mRNA",
 Show.P.value = TRUE,
 Show.P.label = TRUE,
 Method = c("wilcox.test"),
 values = c("# DF2020", "# DDDF21"),
 TCGA.only = FALSE,
 draw_quantiles = c(0.25, 0.5, 0.75),
 trim = TRUE
)

# Test
# vis_toil_TvsN_cancer() Note: Visualize Gene TPM in Single Cancer Type (Tumor (TCGA) vs Normal (TCGA & GTEx))
vis_toil_TvsN_cancer(
 Gene = "UMODL1-AS1",
 Mode = c("Violinplot"),
 data_type = "mRNA",
 Show.P.value = FALSE,
 Show.P.label = FALSE,
 Method = "wilcox.test",
 values = c("# DF2020", "# DDDF21"),
 TCGA.only = FALSE,
 Cancer = "LAML"
)

# Test
vis_toil_TvsN_cancer(
 Gene = "UMODL1-AS1",
 Mode = c("Dotplot"),
 data_type = "mRNA",
 Show.P.value = FALSE,
 Show.P.label = FALSE,
 Method = "wilcox.test",
 values = c("# DF2020", "# DDDF21"),
 TCGA.only = FALSE,
 Cancer = "LAML"
)

# Test
### vis_unicox_tree() 
# Note 1: Visualize Single Gene Univariable Cox Result from Toil Data Hub
# Note 2: Visualize Relationship between Gene Expression and Prognosis in the PANCAN Dataset
vis_unicox_tree(
 Gene = "TP53",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_unicox_tree(
 Gene = "UMODL1",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_unicox_tree(
 Gene = "LINC01423",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_unicox_tree(
Gene = "LINC00319",
measure = "OS",
data_type = "mRNA",
threshold = 0.5,
values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_unicox_tree(
 Gene = "UMODL1-AS1",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)


## surv_adjustedcurves()

tcga_surv_get("DIRAS3")

data <- tcga_surv_get("DIRAS3")

# Test
## vis_pancan_anatomy() Note: para plotar os niveis de express?o nos tecidos
vis_pancan_anatomy(
 Gene = "GRB10",
 Gender = c("Female", "Male"),
 data_type = "mRNA",
 option = "D"
)

# Test
## vis_toil_TvsN() t Visualize Pan-cancer TPM (tumor (TCGA) vs Normal (TCGA & GTEx))
vis_toil_TvsN(
 Gene = "WT1",
 Mode = c("Boxplot"),
 data_type = "mRNA",
 Show.P.value = TRUE,
 Show.P.label = TRUE,
 Method = c("t.test"),
 values = c("# 20df33", "# DDDF21"),
 TCGA.only = FALSE,
 draw_quantiles = c(0.25, 0.5, 0.75),
 trim = TRUE
)

# Test
## vis_gene_immune_cor() to Heatmap for Correlation between Gene and Immune Signatures
vis_gene_immune_cor(
 Gene = "UMODL1-AS1",
 cor_method = "pearson",
 data_type = "mRNA",
 Immune_sig_type = "Cibersort",
 Plot = "TRUE"
)

# Test
# vis_pcawg_unicox_tree
# Note: Visualize Single Gene Univariable Cox Result in PCAWG
vis_pcawg_unicox_tree(
 Gene = "LINC00319",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# vis_gene_TIL_cor()

# Test
# Note: Heatmap for Correlation between Gene and Tumor Immune Infiltration (TIL)
vis_gene_TIL_cor(
 Gene = ("TP53 + 2 * KRAS - 1.3 * PTEN"),
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

# Test
vis_gene_TIL_cor(
 Gene = ("TP53 + 2 * KRAS - 1.3 * PTEN"),
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

# Test
vis_gene_TIL_cor(
 Gene = ("`MT-ND1` + `MT-CO1` + `MT-CO2`"),
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

#### query_molecule_value(TCGA.GBM.sampleMap/HiSeqV2,"GRB10", host = NULL)#### 

## table(UCSCXenaTools::XenaData$Type)

## dataset <- "ccle/CCLE_copynumber_byGene_2013-12-03"
## x <- query_molecule_value(dataset, "TP53")
# head(x)

# Test
## tcga_surv_get()
tcga_surv_get(
 "DIRAS3",
 TCGA_cohort = "GBM",
 profile = c("mRNA"),
 TCGA_cli_data = dplyr::full_join(load_data("tcga_clinical"), load_data("tcga_surv"),
         by = "sample")
)

tcga_surv_get("DIRAS3")

data <- tcga_surv_get("DIRAS3")

# Test
tcga_surv_plot(
 data,
 time = "OS.time",
 status = "OS",
 cutoff_mode = c("Auto"),
 cutpoint = c(50, 50),
 profile = c("mRNA"),
 palette = "aaas"
)

## Kaplan-Meier plot
tcga_surv_plot(data, time = "OS.time", status = "OS")

tcga_surv_plot(data, time = "DSS.time", status = "DSS")

tcga_surv_plot(data, time = "DFI.time", status = "DFI")


### surv_adjustedcurves()
tcga_surv_get("DIRAS3")

data <- tcga_surv_get("DIRAS3")

library()

fit <- coxph( Surv(stop, event) ~ size, data = bladder )

# single curve
ggadjustedcurves(fit, data = bladder)
curve <- surv_adjustedcurves(fit, data = bladder)

# conditional balancing in groups
ggadjustedcurves(fit, data = bladder, method = "marginal", variable = "rx")
curve <- surv_adjustedcurves(fit2, data = bladder, method = "marginal", variable = "rx")

# conditional balancing in groups
ggadjustedcurves(fit, data = bladder, method = "conditional", variable = "rx")
curve <- surv_adjustedcurves(fit, data = bladder, method = "conditional", variable = "rx")

ggadjustedcurves(
 fit,
 variable = NULL,
 data = NULL,
 reference = NULL,
 method = "conditional",
 fun = NULL,
 palette = "hue",
 ylab = " rate",
 size = 1,
 ggtheme = theme_survminer()
)


fdata <- flchain[flchain$futime >=7,]
fdata$age2 <- cut(fdata$age, c(0,54, 59,64, 69,74,79, 89, 110),
     labels = c(paste(c(50,55,60,65,70,75,80),
         c(54,59,64,69,74,79,89), sep='-'), "90+"))
fdata$group <- factor(1+ 1*(fdata$flc.grp >7) + 1*(fdata$flc.grp >9),
      levels=1:3,
      labels=c("FLC < 3.38", "3.38 - 4.71", "FLC > 4.71"))

# single curve
fit <- coxph( Surv(futime, death) ~ age*sex, data = fdata)
ggadjustedcurves(fit, data = fdata, method = "single")

surv_adjustedcurves(
 fit,
 variable = NULL,
 data = NULL,
 reference = NULL,
 method = "conditional",
 size = 1
)

### More info about dataset please run following commands:
library(UCSCXenaTools)
XenaGenerate(subset = XenaDatasets == "TcgaTargetGtex_rsem_gene_tpm") %>% XenaBrowse()

### To see the indormatiopn behing a plot
vis_gene_TIL_cor(Gene = "UMODL1-AS1")
p <- vis_gene_TIL_cor(Gene = "UMODL1-AS1")

## Not run: 
# this will take some time
fdata <- flchain[flchain$futime >=7,]
fdata$age2 <- cut(fdata$age, c(0,54, 59,64, 69,74,79, 89, 110),
     labels = c(paste(c(50,55,60,65,70,75,80),
         c(54,59,64,69,74,79,89), sep='-'), "90+"))
fdata$group <- factor(1+ 1*(fdata$flc.grp >7) + 1*(fdata$flc.grp >9),
      levels=1:3,
      labels=c("FLC < 3.38", "3.38 - 4.71", "FLC > 4.71"))
# single curve
fit <- coxph( Surv(futime, death) ~ age*sex, data = fdata)
ggadjustedcurves(fit, data = fdata, method = "single")

# average in groups
fit <- coxph( Surv(futime, death) ~ age*sex + strata(group), data = fdata)
ggadjustedcurves(fit, data = fdata, method = "average")

# conditional balancing in groups
ggadjustedcurves(fit, data = fdata, method = "conditional")

# marginal balancing in groups
ggadjustedcurves(fit, data = fdata, method = "marginal", reference = fdata)

dev.off()

## End(Not run)

# Test
vis_gene_cor(
 Gene1 = "DIRAS3",
 Gene2 = "H19",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 use_regline = TRUE,
 purity_adj = TRUE,
 alpha = 0.5,
 color = "# 4083c2",
 filter_tumor = TRUE
)

# Test
vis_gene_cor(
 Gene1 = "CSF1R",
 Gene2 = "JAK3",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 use_regline = TRUE,
 purity_adj = TRUE,
 alpha = 0.5,
 color = "# b9c240",
 filter_tumor = TRUE
)

# Test
vis_gene_cor_cancer(
 Gene1 = "CSF1R",
 Gene2 = "TP53",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 purity_adj = TRUE,
 cancer_choose = "LGG",
 use_regline = TRUE,
 cor_method = "spearman",
 use_all = FALSE,
 alpha = 0.5,
 color = "# 000000"
)

# Test
vis_pancan_anatomy(
 Gene = "USP9Y",
 Gender = c("Male"),
 data_type = "mRNA",
 option = "D"
)

dev.off() 

# Test
data <- tcga_surv_get(
 "DIRAS3",
 TCGA_cohort = "GBM",
 profile = c("mRNA"),
 TCGA_cli_data = dplyr::full_join(load_data("tcga_clinical"), load_data("tcga_surv"),
         by = "sample")
)

# Test
tcga_surv_plot(
 data,
 time = "OS.time",
 status = "OS",
 cutoff_mode = c("Auto"),
 cutpoint = c(50, 50),
 cnv_type = c("Duplicated"),
 profile = c("mRNA"),
 palette = "aaas",
)

### run n<rlang::last_error()> para ver o ?ltimo error

# Test
vis_unicox_tree(
 Gene = "UMODL1-AS1",
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

# Test
vis_gene_cor(
 Gene1 = "ATP6V1A",
 Gene2 = "CYC1",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 use_regline = TRUE,
 purity_adj = TRUE,
 alpha = 0.5,
 color = "# c90076",
 filter_tumor = TRUE
)

# Test
vis_gene_cor_cancer(
 Gene1 = "TP53",
 Gene2 = "ATP4A",
 data_type1 = "miRNA",
 data_type2 = "miRNA",
 purity_adj = TRUE,
 cancer_choose = "GBM",
 use_regline = TRUE,
 cor_method = "spearman",
 use_all = FALSE,
 alpha = 0.5,
 color = "# c90076"
)

# Test
vis_gene_cor_cancer(
 Gene1 = "CSF1R",
 Gene2 = "JAK3",
 data_type1 = "mRNA",
 data_type2 = "mRNA",
 purity_adj = TRUE,
 cancer_choose = "GBM",
 use_regline = TRUE,
 cor_method = "spearman",
 use_all = FALSE,
 alpha = 0.5,
 color = "# 000000"
)

dev.off()

knitr::opts_chunk$set(
 collapse = TRUE,
 comment = "# >",
 fig.align = "center"
)


## args(name) - to display the argument names and corresponding default values of a function or primitive.

args(query_pancan_value)

### Fetch Gene Expression : For TCGA gene expression data, we use Xena dataset with ID TcgaTargetGtex_rsem_gene_tpm which includes 19131 samples with tumor tissue samples and normal tissue samples. The expression value unit is log2(tpm+0.001).

gene_expr_UMODL1_AS1 <- query_pancan_value("UMODL1-AS1")
str(gene_expr_UMODL1_AS1)

### Fetch Transcript Expression
transcript_expr <- query_pancan_value("ENST00000000233", data_type = "transcript")

### Fetch Gene CNV
gene_cnv <- query_pancan_value("UMODL1-AS1", data_type = "cnv")

## Fetch Gene Mutation
gene_mut <- query_pancan_value("UMODL1-AS1", data_type = "mutation")

dev.off() 

# Test
## Compare Gene Expression Level in Single Cancer Type
GRB_UMODL1_AS1 <-vis_toil_TvsN_cancer(
 Gene = "UMODL1-AS1",
 Mode = "Violinplot",
 Show.P.value = TRUE,
 Show.P.label = TRUE,
 Method = "wilcox.test",
 values = c("# DF2020", "# DDDF21"),
 TCGA.only = FALSE,
 Cancer = "GBM"
)

dev.off()

print(GRB_UMODL1_AS1)

## Compare Gene Expression Level in Different Anatomic Regions
## This function needs gganatogram package, which is not on CRAN. Please install it before using this function.

install.packages("gganatogram")

if (require("gganatogram")) {
 vis_pancan_anatomy(Gene = "UMODL1-AS1", Gender = c("Female"), option = "D")
}

### Visualize Relationship between Gene Expression and Prognosis in the PANCAN Dataset
### methods: [OS, DSS, DFI, PFI]
## Overall (OS)
## Disease-Specific (DSS)
## Disease-Free (DFS)
## Progression free interva (PFI) 

# Test
vis_unicox_tree(
 Gene = "UMODL1-AS1",
 measure = "PFI",
 threshold = 0.5,
 values = c("grey", "# E31A1C", "# 377DB8")
)

### pdf(file = 'J:/UCSCXenaShiny Rdocumentation//myplot') if it does not work for saving it
# in the directory, run dev.off()

dev.off()

help(UCSCXenaShiny)

# grDevices::savePlot


### J:/UCSCXenaShiny Rdocumentation/testes/myplot
## ------------------------- ## 
## An Example of ## 
## Automating Plot Output ## 
## ------------------------- ## 

names = LETTERS[1:26] ## Gives a sequence of the letters of the alphabet

beta1 = rnorm(26, 5, 2) ## A vector of slopes (one for each letter)
beta0 = 10 ## A common intercept

for(i in 1:26){
 x = rnorm(500, 105, 10)
 y = beta0 + beta1[i]*x + 15*rnorm(500)
 
 mypath <- file.path("J:","UCSCXenaShiny Rdocumentation","SAVEHERE",paste("myplot_", names[i], ".jpg", sep = ""))
 
 jpeg(file=mypath)
 mytitle = paste("slope of", names[i])
 plot(x,y, main = mytitle)
 dev.off()
}

###### ploting expression of formula profiles

# Test
vis_unicox_tree(
 Gene = ("TP53 + 2 * KRAS - 1.3 * PTEN"),
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("# 15F4EE", "# E31A1C", "# 377DB8")
)

# My crazy test
vis_unicox_tree(
 Gene = ("PLEKHG4B - 4 * DIRAS3 / H19"),
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("# 15F4EE", "# E31A1C", "# 377DB8")
)


# My crazy test 2
vis_unicox_tree(
 Gene = ("PLEKHG4B"),
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("# 15F4EE", "# E31A1C", "# 377DB8")
)

# My crazy test 3
vis_unicox_tree(
 Gene = ("LINC00598"),
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("# 15F4EE", "# E31A1C", "# 377DB8")
)

# My crazy test 4 - normalization by a gene
vis_unicox_tree(
 Gene = ("PLEKHG4B / LINC00598"),
 measure = "OS",
 data_type = "mRNA",
 threshold = 0.5,
 values = c("# 15F4EE", "# E31A1C", "# 377DB8")
)

app_run()

## Crazy staff not related

vis_gene_stemness_cor()

# example cowplot theme
# source: https://wilkelab.org/cowplot/articles/themes.html

p <- ggplot(mtcars, aes(disp, mpg)) + geom_point()

p + theme_half_open() # identical to theme_cowplot()

p +
 theme_half_open() +
 background_grid() # always place this after the theme

### https://cran.r-project.org/web/packages/fmsb/fmsb.pdf
### Package 'fmsb'

install.packages("fmsb")

## radarchart()

library(fmsb)
dev.off()
#### stemness
teste1 <- vis_gene_stemness_cor(
 Gene = "(SPANXA1 + SPANXA2 + SPANXB1 + `RP1-171K16.5` + SPANXD + SPANXC + `SPANXA2-OT1` + SPANXN1)",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
 )

teste1


## save table supporting fugure

write.table(teste1$data, file = "SPANX_signature", sep = "\t")

X <- read.table(file = "SPANX_signature", sep = "\t", header = T)

X 

teste2 <- as.data.frame(teste1)

radarchart(teste1)

tcga_stemness()

dev.off()

app_run()


#### Check parsing of gene names #### 
# Note: In the UCSCXenaShiny app, when using formulas with names separated by "-", the the gene names must be `MT-NDi`, example:

# correct
all.vars(parse(text = "`MT-ND1` + `MT-CO1`"))
# [1] "MT-ND1" "MT-CO1"

# incorrect
all.vars(parse(text = "MT-ND1 + MT-CO1"))
# [1] "MT" "ND1" "CO1"

all.vars(parse(text = "SNRPN + H19 + MEG3 + SNHG14 + PLAGL1 + SNURF + PEG3 + ZDBF2 + IGF2 + L3MBTL1 + NAP1L5 + PEG10 + ZNF331 + FAM50B + KCNQ1 + MEST + DLK1 + MEG9 + PPIEL + NDN + ZNF597 + MEG8 + PWRN1 + LPAR6 + INPP5F + UTS2 + DIRAS3 + CPA4 + MAGI2 + SYCE1 + `RP11-7F17.7` + THEGL + PRSS50 + `UGT2B4` + SGK2 + MAGEL2 + CST1 + UBE3A + KIF25 + NTM + GRB10 + `IGF2-AS`"))

#### Baran 42 gene signature - formatted a validated formula#### 
# (SNRPN + H19 + MEG3 + SNHG14 + PLAGL1 + SNURF + PEG3 + ZDBF2 +
# IGF2 + L3MBTL1 + NAP1L5 + PEG10 + ZNF331 + FAM50B + KCNQ1 + MEST +
# DLK1 + MEG9 + PPIEL + NDN + ZNF597 + MEG8 + PWRN1 + LPAR6 + INPP5F +
# UTS2 + DIRAS3 + CPA4 + MAGI2 + SYCE1 + `RP11-7F17.7` + THEGL + PRSS50 +
# `UGT2B4` + SGK2 + MAGEL2 + CST1 + UBE3A + KIF25 + NTM + GRB10 + `IGF2-AS`)


#### Access an URL and read data in R. Example: data for Human Gene Damage Index (GDI)#### 

GDI_full.txt <- read.table(url("http://lab.rockefeller.edu/casanova/assets/file/GDI_full_10282015.txt"),
       sep = "\t", fill = TRUE, header = TRUE)

write.table(GDI_full.txt,"GDI_full.txt", sep = "\t", row.names = FALSE, quote = FALSE)

GDI_full.txt

# add_GDI.py <- read.csv(url("http://lab.rockefeller.edu/casanova/assets/file/add_GDI.py.txt"))


##### example Bio infiltrates

dev.off()

# Test
vis_gene_TIL_cor(
 Gene = "ADARB1 + RUNX1 + ETS2 + PKNOX1 + BTG3 + SIK1 + MIRLET7C + MIR125B2 + MIR155 + MIR99A",
 cor_method = "spearman", # choose wisely between spearman, pearson and kendall
 data_type = "mRNA",
 sig = c("NK cell resting_CIBERSORT", "Macrophage M0_CIBERSORT", "T cell CD4+ memory activated_CIBERSORT-ABS",
 "Neutrophil_TIMER", "Myeloid dendritic cell_XCELL", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE"
)

# Example stemness
p <- vis_gene_stemness_cor(
 Gene = "TP53",
 cor_method = "spearman",
 data_type = "cnv_gistic2",
 Plot = "TRUE"
)
p
# Transcription factors associated with TLS in several cancer types: FOXM1 + MYBL2 + TAL1 + ERG
# https://www.frontiersin.org/articles/10.3389/fimmu.2021.644350/full

# Test
vis_gene_TIL_cor(
 Gene = "FOXM1 + MYBL2 + TAL1 + ERG",
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("T cell CD8+_CIBERSORT", "B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER",
   "Macrophage_TIMER", "Myeloid dendritic cell_TIMER"),
 Plot = "TRUE" # "TRUE" only plot; "FALSE" plots and provides dataframe in the console
)

# Test
vis_gene_immune_cor(
 Gene = "FOXM1 + MYBL2 + TAL1 + ERG",
 cor_method = "spearman",
 data_type = "mRNA",
 Immune_sig_type = "Cibersort",
 Plot = "TRUE"
)

#### Radar plot in R with UCSCXenaShiny#### 
# Currently thee shiny package does not executes the radar plot as a function. 
# But the developers provided the following code about how to plot a radar plot. 
# This is the same way as the shiny app does. Hope this could help you!
# Below is the code: example with Baran 42 gene list

# Test
# p = vis_gene_stemness_cor(
# Gene = "`MT-ATP8` + `MT-ATP6` + `MT-CO1` + `MT-CO2` + `MT-CO3` + `MT-CYB` + `MT-ND1` + `MT-ND2` + `MT-ND3` + `MT-ND4L` + `MT-ND4` + `MT-ND5` + `MT-ND6`",
# cor_method = "spearman",
# data_type = "mRNA",
# Plot = "TRUE"
# )

dev.off()
p <- vis_gene_stemness_cor(
 Gene = "`MT-ATP8` + `MT-ATP6` + `MT-CO1`", 
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)

pdata <- p$data %>%
 dplyr::mutate(cor = round(cor, digits = 3), p.value = round(p.value, digits = 3))

df <- pdata %>%
 select(cor, cancer) %>%
 pivot_wider(names_from = cancer, values_from = cor)
df$gene <- "Gene signature"
df<-df[,c(34,1:33)]

ggradar::ggradar(
 df[1, ],
 font.radar = "sans",
 values.radar = c("-1", "0", "1"),
 grid.min = -1, grid.mid = 0, grid.max = 1,
 # Background and grid lines
 background.circle.colour = "white",
 gridline.mid.colour = "grey",
 # Polygons
 group.line.width = 1,
 group.point.size = 3,
 group.colours = "# 00AFBB",
) + theme(plot.title = element_text(hjust = .5))

# dev.off()

### Error pivot_wider() could not find function "pivot_wider" ### re-installed everything

# Test
p = vis_gene_stemness_cor(
 Gene = "GRB10",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)

pdata <- p$data %>%
 dplyr::mutate(cor = round(cor, digits = 3), p.value = round(p.value, digits = 3))

df <- pdata %>%
 select(cor, cancer) %>%
 pivot_wider(names_from = cancer, values_from = cor)

ggradar::ggradar(
 df[1, ],
 font.radar = "sans",
 values.radar = c("-1", "0", "1"),
 grid.min = -1, grid.mid = 0, grid.max = 1,
 # Background and grid lines
 background.circle.colour = "white",
 gridline.mid.colour = "grey",
 # Polygons
 group.line.width = 1,
 group.point.size = 3,
 group.colours = "# 00AFBB",
) + theme(plot.title = element_text(hjust = .5))

d# ev.off()

###### Fetching URL tables#### 
# PAXdb - Protein Abundace Database

# https://pax-db.org/downloads/4.2/datasets/9606/9606-Human_PeptideAtlas_2011-08_Ens62.new.txt

PAXdb_full.txt <- read.table(url("https://pax-db.org/downloads/4.2/datasets/9606/9606-Human_PeptideAtlas_2011-08_Ens62.new.txt"),
       sep = "\t", fill = TRUE, header = FALSE)

write.table(PAXdb_full.txt,"PAXdb_full.txt", sep = "\t", row.names = FALSE, quote = FALSE)

PAXdb_full.txt

dim(PAXdb_full.txt)

# To print the last and first 10 rows

tail(PAXdb_full.txt, 10) # to print the last 10 rows

head(PAXdb_full.txt, 10) # to print the first 10 rows

## Example 2 - fetching URL tables

# https://pax-db.org/downloads/4.2/datasets/9606/9606-WHOLE_ORGANISM-integrated.txt

PAXdb_full_2.txt <- read.table(url("https://pax-db.org/downloads/4.2/datasets/9606/9606-WHOLE_ORGANISM-integrated.txt"),
        sep = "\t", fill = TRUE, header = FALSE)

write.table(PAXdb_full_2.txt,"PAXdb_full_2.txt", sep = "\t", row.names = FALSE, quote = FALSE)

PAXdb_full_2.txt

dim(PAXdb_full_2.txt)

# To print the last and first 10 rows

tail(PAXdb_full_2.txt, 10) # to print the last 10 rows

head(PAXdb_full_2.txt, 10) # to print the first 10 rows 



# NOTE >>> URL fetching does not work for temporary tables such as URL outputs form UCSC Genome at http://genome.ucsc.edu/cgi-bin/hgTables

#### Example Kléber#### 
# Test
vis_gene_immune_cor(
 Gene = "FOXM1 + MYBL2 + TAL1 + ERG",
 cor_method = "spearman",
 data_type = "mRNA",
 Immune_sig_type = "Cibersort",
 Plot = "TRUE"
)

# app_run()

#### Radar plotting in Xena Shiny #### -------------------------------------------
# Currently thee shiny package does not executes the radar plot as a function. 
# But the developers provided the following code about how to plot a radar plot. 
# This is the same way as the shiny app does. Hope this could help you!
# Below is the code: example with Baran 42 gene list

library(UCSCXenaShiny)
library(ggradar)

dev.off()

# Test
p = vis_gene_stemness_cor(
 Gene = "TP53",
 cor_method = "spearman",
 data_type = "mRNA",
 Plot = "TRUE"
)

pdata <- p$data %>%
 dplyr::mutate(cor = round(cor, digits = 3), p.value = round(p.value, digits = 3))

df <- pdata %>%
 select(cor, cancer) %>%
 pivot_wider(names_from = cancer, values_from = cor)

# write.table(df)

# dim(df)

ggradar::ggradar(
 df[1, ],
 font.radar = "sans",
 values.radar = c("-1", "0", "1"),
 grid.min = -1, grid.mid = 0, grid.max = 1,
 # Background and grid lines
 background.circle.colour = "white",
 gridline.mid.colour = "grey",
 # Polygons
 group.line.width = 1,
 group.point.size = 3,
 group.colours = "# 00AFBB",
) + theme(plot.title = element_text(hjust = .5))


#### Nomogram function ## added in 04/05/2022 to make nomograms#### 
# based regression model strategies. Example: on Front. Genet. 13:816460.
# doi: 10.3389/fgene.2022.816460
# https://pubmed.ncbi.nlm.nih.gov/35360864
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8961878/

#### Lasso and Elastic-Net Regularized Generalized Linear Models#### 
# Based on example: https://bmccancer.biomedcentral.com/articles/10.1186/s12885-021-07916-3# :~:text=screening%20%5B28%5D.-,LASSO%20Cox%20regression%20analysis%20constructs%20a%20penalty%20function%20to%20obtain,some%20regression%20coefficients%20to%200.
install.packages("rms")
install.packages("glmnet")
library(rms)
library(glmnet)


#### Multicorrelation example#### 
# Visualize Correlation for Multiple Identifiers
multicorrelation<- vis_identifier_multi_cor(
 dataset <- "TcgaTargetGtex_rsem_isoform_tpm",
 ids <- c("AXL", "NDN", "CACNA1C", "ITGA8"),
 samples = NULL,
 matrix.type = c("full"),
 type = c("parametric"),
 partial = FALSE,
 sig.level = 0.00001,
 p.adjust.method = c("fdr"),
 color_low = "# E69F00",
 color_high = "# 009E73",
)

multicorrelation



#### UCSCXenaShiny Support for PCA analysis#### 
setwd("O:/UCSCXenaShiny Rdocumentation/RScripts")
getwd()
library(UCSCXenaShiny)
library(dplyr)
library(purrr)
library(tidyr)
# remove.packages("purrr")
# install.packages("purrr")
# geneList = c("TP53", "KRAS", "PTEN", "ERBB2", "MYC")

geneList = c("UMODL1", "UMODL1-As1", "LINC00319", "LINC01423") 

alldata = purrr::map(geneList, query_pancan_value)

expr = purrr::reduce(
 purrr::map2(alldata, geneList,
    function(.x, .y) {
    dt = data.frame(sample = names(.x$expression),
        expr = as.numeric(.x$expression))
    colnames(dt)[2] = .y
    dt
    }),
 merge, by = "sample", sort = FALSE)
head(expr)

tail(expr)

# Generate data ----------------------------------------------------------------

tcga_gtex <- tcga_gtex %>% dplyr::group_by(.data$tissue) %>%
 dplyr::distinct(.data$sample, .keep_all = TRUE)

ov_cli = dplyr::filter(tcga_gtex, tissue == "OV") |>
 dplyr::ungroup() |>
 dplyr::select(sample, group = type2) |> unique()

ov_cli
head(ov_cli)
tail(ov_cli)

ov_expr = dplyr::filter(expr, sample %in% ov_cli$sample)
ov_cli = dplyr::filter(ov_cli, sample %in% ov_expr$sample)

head(ov_cli)
tail(ov_cli)

ov_expr2 = ov_expr |>
 tidyr::pivot_longer(cols = -"sample",
      names_to = "gene",
      values_to = "tpm") |>
 tidyr::pivot_wider(id_cols = "gene", names_from = "sample", values_from = "tpm")

readr::write_csv(ov_cli, file = "ov_group.csv")
readr::write_csv(ov_expr2, file = "ov_expr.csv")

# Plot --------------------------------------------------------------------

plot_pca = function(df) {
 pca <- prcomp(df |> dplyr::select(-sample, -group), scale. = TRUE)
 autoplot(pca, data = df, colour = 'group') +
 scale_color_brewer(palette = "Set1", direction = -1) # Set1, Set2, Set3 etc
}
 

library(ggfortify)
df_ov = dplyr::left_join(ov_expr, ov_cli, by = "sample")

p1 = plot_pca(df_ov)
p1

# You can remove outlier with
p1 = p1 + xlim(-0.1, 0.15) + ylim(-0.15, 0.1)
p1


ggsave("PCA_with_outliera.pdf", p1, width = 5, height = 4)


### get_data() 

dev.off()

## Tumor Immune Infiltrates TIL for ALL (n=119) immune cell categories
p <- vis_gene_TIL_cor(
 Gene = "UMODL1",
 cor_method = "spearman",
 data_type = "mRNA",
 sig = c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER", "Macrophage_TIMER", "Myeloid dendritic cell_TIMER", "B cell naive_CIBERSORT", "B cell memory_CIBERSORT", "B cell plasma_CIBERSORT", "T cell CD8+_CIBERSORT", "T cell CD4+ naive_CIBERSORT", "T cell CD4+ memory resting_CIBERSORT", "T cell CD4+ memory activated_CIBERSORT", "T cell follicular helper_CIBERSORT", "T cell regulatory (Tregs)_CIBERSORT", "T cell gamma delta_CIBERSORT", "NK cell resting_CIBERSORT", "NK cell activated_CIBERSORT", "Monocyte_CIBERSORT", "Macrophage M0_CIBERSORT", "Macrophage M1_CIBERSORT", "Macrophage M2_CIBERSORT", "Myeloid dendritic cell resting_CIBERSORT", "Myeloid dendritic cell activated_CIBERSORT", "Mast cell activated_CIBERSORT", "Mast cell resting_CIBERSORT", "Eosinophil_CIBERSORT", "Neutrophil_CIBERSORT", "B cell naive_CIBERSORT-ABS", "B cell memory_CIBERSORT-ABS", "B cell plasma_CIBERSORT-ABS", "T cell CD8+_CIBERSORT-ABS", "T cell CD4+ naive_CIBERSORT-ABS", "T cell CD4+ memory resting_CIBERSORT-ABS", "T cell CD4+ memory activated_CIBERSORT-ABS", "T cell follicular helper_CIBERSORT-ABS", "T cell regulatory (Tregs)_CIBERSORT-ABS", "T cell gamma delta_CIBERSORT-ABS", "NK cell resting_CIBERSORT-ABS", "NK cell activated_CIBERSORT-ABS", "Monocyte_CIBERSORT-ABS", "Macrophage M0_CIBERSORT-ABS", "Macrophage M1_CIBERSORT-ABS", "Macrophage M2_CIBERSORT-ABS", "Myeloid dendritic cell resting_CIBERSORT-ABS", "Myeloid dendritic cell activated_CIBERSORT-ABS", "Mast cell activated_CIBERSORT-ABS", "Mast cell resting_CIBERSORT-ABS", "Eosinophil_CIBERSORT-ABS", "Neutrophil_CIBERSORT-ABS", "B cell_QUANTISEQ", "Macrophage M1_QUANTISEQ", "Macrophage M2_QUANTISEQ", "Monocyte_QUANTISEQ", "Neutrophil_QUANTISEQ", "NK cell_QUANTISEQ", "T cell CD4+ (non-regulatory)_QUANTISEQ", "T cell CD8+_QUANTISEQ", "T cell regulatory (Tregs)_QUANTISEQ", "Myeloid dendritic cell_QUANTISEQ", "uncharacterized cell_QUANTISEQ", "T cell_MCPCOUNTER", "T cell CD8+_MCPCOUNTER", "cytotoxicity score_MCPCOUNTER", "NK cell_MCPCOUNTER", "B cell_MCPCOUNTER", "Monocyte_MCPCOUNTER", "Macrophage/Monocyte_MCPCOUNTER", "Myeloid dendritic cell_MCPCOUNTER", "Neutrophil_MCPCOUNTER", "Endothelial cell_MCPCOUNTER", "Cancer associated fibroblast_MCPCOUNTER", "Myeloid dendritic cell activated_XCELL", "B cell_XCELL", "T cell CD4+ memory_XCELL", "T cell CD4+ naive_XCELL", "T cell CD4+ (non-regulatory)_XCELL", "T cell CD4+ central memory_XCELL", "T cell CD4+ effector memory_XCELL", "T cell CD8+ naive_XCELL", "T cell CD8+_XCELL", "T cell CD8+ central memory_XCELL", "T cell CD8+ effector memory_XCELL", "Class-switched memory B cell_XCELL", "Common lymphoid progenitor_XCELL", "Common myeloid progenitor_XCELL", "Myeloid dendritic cell_XCELL", "Endothelial cell_XCELL", "Eosinophil_XCELL", "Cancer associated fibroblast_XCELL", "Granulocyte-monocyte progenitor_XCELL", "Hematopoietic stem cell_XCELL", "Macrophage_XCELL", "Macrophage M1_XCELL", "Macrophage M2_XCELL", "Mast cell_XCELL", "B cell memory_XCELL", "Monocyte_XCELL", "B cell naive_XCELL", "Neutrophil_XCELL", "NK cell_XCELL", "T cell NK_XCELL", "Plasmacytoid dendritic cell_XCELL", "B cell plasma_XCELL", "T cell gamma delta_XCELL", "T cell CD4+ Th1_XCELL", "T cell CD4+ Th2_XCELL", "T cell regulatory (Tregs)_XCELL", "immune score_XCELL", "stroma score_XCELL", "microenvironment score_XCELL", "B cell_EPIC", "Cancer associated fibroblast_EPIC", "T cell CD4+_EPIC", "T cell CD8+_EPIC", "Endothelial cell_EPIC", "Macrophage_EPIC", "NK cell_EPIC", "uncharacterized cell_EPIC"),
 Plot = "TRUE"
)

print(p)

dev.off()


## Tumor Immune Infiltrates TIL for ALL (n=119) immune cell categories
TIL_list <- c("B cell_TIMER", "T cell CD4+_TIMER", "T cell CD8+_TIMER", "Neutrophil_TIMER", "Macrophage_TIMER", "Myeloid dendritic cell_TIMER", "B cell naive_CIBERSORT", "B cell memory_CIBERSORT", "B cell plasma_CIBERSORT", "T cell CD8+_CIBERSORT", "T cell CD4+ naive_CIBERSORT", "T cell CD4+ memory resting_CIBERSORT", "T cell CD4+ memory activated_CIBERSORT", "T cell follicular helper_CIBERSORT", "T cell regulatory (Tregs)_CIBERSORT", "T cell gamma delta_CIBERSORT", "NK cell resting_CIBERSORT", "NK cell activated_CIBERSORT", "Monocyte_CIBERSORT", "Macrophage M0_CIBERSORT", "Macrophage M1_CIBERSORT", "Macrophage M2_CIBERSORT", "Myeloid dendritic cell resting_CIBERSORT", "Myeloid dendritic cell activated_CIBERSORT", "Mast cell activated_CIBERSORT", "Mast cell resting_CIBERSORT", "Eosinophil_CIBERSORT", "Neutrophil_CIBERSORT", "B cell naive_CIBERSORT-ABS", "B cell memory_CIBERSORT-ABS", "B cell plasma_CIBERSORT-ABS", "T cell CD8+_CIBERSORT-ABS", "T cell CD4+ naive_CIBERSORT-ABS", "T cell CD4+ memory resting_CIBERSORT-ABS", "T cell CD4+ memory activated_CIBERSORT-ABS", "T cell follicular helper_CIBERSORT-ABS", "T cell regulatory (Tregs)_CIBERSORT-ABS", "T cell gamma delta_CIBERSORT-ABS", "NK cell resting_CIBERSORT-ABS", "NK cell activated_CIBERSORT-ABS", "Monocyte_CIBERSORT-ABS", "Macrophage M0_CIBERSORT-ABS", "Macrophage M1_CIBERSORT-ABS", "Macrophage M2_CIBERSORT-ABS", "Myeloid dendritic cell resting_CIBERSORT-ABS", "Myeloid dendritic cell activated_CIBERSORT-ABS", "Mast cell activated_CIBERSORT-ABS", "Mast cell resting_CIBERSORT-ABS", "Eosinophil_CIBERSORT-ABS", "Neutrophil_CIBERSORT-ABS", "B cell_QUANTISEQ", "Macrophage M1_QUANTISEQ", "Macrophage M2_QUANTISEQ", "Monocyte_QUANTISEQ", "Neutrophil_QUANTISEQ", "NK cell_QUANTISEQ", "T cell CD4+ (non-regulatory)_QUANTISEQ", "T cell CD8+_QUANTISEQ", "T cell regulatory (Tregs)_QUANTISEQ", "Myeloid dendritic cell_QUANTISEQ", "uncharacterized cell_QUANTISEQ", "T cell_MCPCOUNTER", "T cell CD8+_MCPCOUNTER", "cytotoxicity score_MCPCOUNTER", "NK cell_MCPCOUNTER", "B cell_MCPCOUNTER", "Monocyte_MCPCOUNTER", "Macrophage/Monocyte_MCPCOUNTER", "Myeloid dendritic cell_MCPCOUNTER", "Neutrophil_MCPCOUNTER", "Endothelial cell_MCPCOUNTER", "Cancer associated fibroblast_MCPCOUNTER", "Myeloid dendritic cell activated_XCELL", "B cell_XCELL", "T cell CD4+ memory_XCELL", "T cell CD4+ naive_XCELL", "T cell CD4+ (non-regulatory)_XCELL", "T cell CD4+ central memory_XCELL", "T cell CD4+ effector memory_XCELL", "T cell CD8+ naive_XCELL", "T cell CD8+_XCELL", "T cell CD8+ central memory_XCELL", "T cell CD8+ effector memory_XCELL", "Class-switched memory B cell_XCELL", "Common lymphoid progenitor_XCELL", "Common myeloid progenitor_XCELL", "Myeloid dendritic cell_XCELL", "Endothelial cell_XCELL", "Eosinophil_XCELL", "Cancer associated fibroblast_XCELL", "Granulocyte-monocyte progenitor_XCELL", "Hematopoietic stem cell_XCELL", "Macrophage_XCELL", "Macrophage M1_XCELL", "Macrophage M2_XCELL", "Mast cell_XCELL", "B cell memory_XCELL", "Monocyte_XCELL", "B cell naive_XCELL", "Neutrophil_XCELL", "NK cell_XCELL", "T cell NK_XCELL", "Plasmacytoid dendritic cell_XCELL", "B cell plasma_XCELL", "T cell gamma delta_XCELL", "T cell CD4+ Th1_XCELL", "T cell CD4+ Th2_XCELL", "T cell regulatory (Tregs)_XCELL", "immune score_XCELL", "stroma score_XCELL", "microenvironment score_XCELL", "B cell_EPIC", "Cancer associated fibroblast_EPIC", "T cell CD4+_EPIC", "T cell CD8+_EPIC", "Endothelial cell_EPIC", "Macrophage_EPIC", "NK cell_EPIC", "uncharacterized cell_EPIC")
p <- vis_gene_TIL_cor(
 Gene = "UMODL1",
 cor_method = "spearman",
 data_type = "mRNA",
 sig = TIL_list,
 Plot = "TRUE"
)

print(p)

dev.off()

# Ensure TIL_signatures is a character vector
TIL_signatures <- as.character(TIL_signatures)

# Use strsplit on TIL_signatures
# ... your code to use strsplit ...

# Increase timeout for download.file
options(timeout = 300)  # Setting timeout to 300 seconds (5 minutes)

# Now, attempt to download the file again
download.file(data_url, data_path)

