  # ==============================================================================
  # CANCERRCDPREDICTOR PHASE IV: EDUCATIONAL SANDBOX & TOPOLOGY EXPLORER
  # ==============================================================================
  # ==============================================================================
  # AUTONOMOUS DEPENDENCY MANAGEMENT
  # ==============================================================================

  options(sass.cache = FALSE)

  required_packages <- c("shiny", "bslib", "bsicons", "DT", "magick", "rmarkdown", "pagedown", "zip", "ggplot2", "data.table", "scales", "jsonlite", "filelock", "dplyr", "tidyr", "stringr", "httr2", "blastula", "promises", "future")
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

  if(length(missing_packages)) {
    message("Installing missing dependencies: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, repos = "http://cran.rstudio.com/")
  }

  # Suppress startup messages during library loading
  suppressPackageStartupMessages({
    invisible(lapply(required_packages, require, character.only = TRUE))
  })

  # Configure future for non-blocking concurrent LLM API calls
  # Each DeepSeek API call runs in a separate background R process,
  # freeing the Shiny event loop to serve other user sessions immediately.
  future::plan(future::multisession, workers = 4)

  if (file.exists(".Renviron")) {
      readRenviron(".Renviron")
  }
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
  # LLM GLOBAL GLOSSARY
  # ==============================================================================
  llm_glossary <- "\n\nSTRICT ABBREVIATION DEFINITIONS:\nWhen interpreting acronyms or generating insights, you MUST adhere strictly to the following definitions. Do not hallucinate or modify them:\n- TMB (Tumor Mutational Burden): The total number of somatic mutations per megabase of tumor DNA, commonly used as a measure of tumor genomic mutational load.\n- TSM (Tumor Stemness Metrics): Quantitative measures of stem cell-like characteristics in tumors, reflecting the degree to which a tumor exhibits stemness-associated molecular features. CRITICAL: TSM exclusively represents stemness-related measurements and stemness-associated phenotypes. TSM DOES NOT refer to tumor stroma, tumor stromal interactions, tumor microenvironment, stromal remodeling, stromal signaling, or any related concept. You MUST internally interpret TSM = Stemness whenever generating biological, clinical, pharmacogenomic, prognostic, or therapeutic narratives.\n- MSI (Microsatellite Instability): A genomic instability phenotype caused by defects in the DNA mismatch repair system, resulting in alterations in microsatellite sequences.\n\nFORMATTING & LOGICAL STRICT RULES:\n1. FORMATTING: Do NOT use markdown asterisks (* or **) around gene symbols or any other words. Output gene symbols as plain text without asterisks. You may use colored text if the environment supports it, but NO asterisks.\n2. BIOLOGICAL LOGIC: When discussing SHAP trajectories within a patient, first identify if each signature has a positive (lethal) or negative (protective) SHAP impact. Discuss that the final trajectory is defined by the cumulative net effect of signatures that may have either positive or negative SHAP indexes. Do NOT describe opposing SHAP signs (protective vs lethal) within a patient as 'discordant', 'incongruent', 'paradoxical', or 'inconsistent'. Survival trajectories are mathematically determined by the cumulative net effect of signatures that are important for the specific patient. A variable of importance may have a SHAP sign pulling toward the lethal or protective side simultaneously. Focus on the cumulative balance rather than calling them contradictory to the overall MVL prediction.\n3. LANGUAGE CONSTRAINT: You MUST generate your entire response exclusively in English. Do not output any Chinese characters, translations, or foreign text.

--- OMIC LAYER TOKEN DECODING GOVERNANCE ---

The CancerRCDPredictor signature nomenclature may contain encoded omic-layer tokens (e.g., .5, .6, .7) embedded within alphanumeric identifier strings. These tokens are CATEGORICAL IDENTIFIER CODES that map to specific biological omic data types. They carry NO quantitative, ordinal, hierarchical, or interpretive meaning whatsoever.

OMIC LAYER DECODING DICTIONARY:
.1 = Protein
.2 = Mutation
.3 = Copy Number Variation (CNV)
.4 = miRNA
.5 = Transcript Isoform
.6 = mRNA Expression
.7 = CpG Methylation

CRITICAL PROHIBITIONS - THE FOLLOWING INTERPRETATIONS ARE STRICTLY FORBIDDEN:
- .6 does NOT mean \"six omic layers\", \"six biological layers\", \"six molecular dimensions\", \"spanning 6 layers\", or any similar quantitative interpretation.
- .5 does NOT mean \"five biological dimensions\", \"five datasets\", \"five modalities\", \"across 5 layers\", or any similar quantitative interpretation.
- .7 does NOT mean \"seven datasets\", \"seven modalities\", \"across 7 omic dimensions\", or greater evidence than .6.
- .3 does NOT mean \"three CNV events\", \"three copies\", or any copy-number magnitude.
- No ordinal, hierarchical, cumulative, quantitative, or mechanistic interpretation may be derived from the numeric token itself.
- The tokens are NOT counts, quantities, rankings, levels, dimensions, layers, strengths, priorities, scores, magnitudes, or biological attributes. They are IDENTIFIER CODES ONLY.

MANDATORY DECODING PROTOCOL:
1. Before generating any narrative, decode each omic-layer token to its corresponding categorical label using the dictionary above.
2. Use ONLY the decoded categorical label (e.g., \"mRNA Expression\", \"CpG Methylation\", \"Protein\") for all downstream reasoning and narrative generation.
3. DISCARD the original numerical identifier from the reasoning process. Never reference .1, .2, .3, .4, .5, .6, .7 or \"Token N\" in your narrative output.
4. The numerical value of an encoded token conveys ZERO biological, clinical, statistical, topological, mechanistic, or prognostic information - only its decoded label matters.
5. This governance applies to ALL Lineage Intelligence modules: The Digital Molecular Tumor Board, Population & Topological Intelligence, and Trans-Signature Interaction Intelligence.

END OF OMIC LAYER TOKEN DECODING GOVERNANCE"

  narrative_governance_framework <- "\n\n--- VERSION 2.6 DYNAMIC TUMOR-STATE REASONING PROMPT ---\n\nPRIMARY OBJECTIVE\n\nPreserve the full structure, readability, governance discipline, and molecular tumor board style of Version 2.5. Do not add new major sections. Do not expand the report unnecessarily. Instead, tune the biological reasoning layer to interpret the tumor profile as a dynamic adaptive system.\n\nThe report should retain the concise, integrated, expert-style communication of Version 2.5 while incorporating tumor-state dynamics when biologically justified.\n\n----------------------------------------\n\nCORE DYNAMIC REASONING PRINCIPLE\n\nDo not interpret molecular signatures as isolated static features.\nInterpret them as components of an adaptive tumor-state landscape.\n\nWhen appropriate, discuss:\n• stemness ↔ differentiation balance\n• plasticity\n• compensatory pathway activation\n• adaptive resistance\n• pathway redundancy\n• metabolic rewiring\n• stress-response states\n• vulnerability-resistance trade-offs\n• temporal evolution of tumor states\n\n----------------------------------------\n\nMANDATORY INTERPRETIVE RULES\n\nPreserve v2.5 architecture.\nDo not create additional sections such as \"Dynamic Tumor-State Model\" or \"Evolutionary Model\".\nInstead, embed dynamic reasoning naturally within existing sections.\n\nPreserve evidence discipline.\nDynamic interpretations must remain hypothesis-generating unless directly supported by payload data.\nUse cautious language: \"may reflect\", \"could indicate\", \"is consistent with\", \"may represent\".\nAvoid: \"proves\", \"confirms\", \"demonstrates\", \"drives\", \"causes\".\n\nPreserve molecular tumor board realism.\nThe narrative should read like an expert multidisciplinary interpretation, not like a systems biology lecture.\n\nAvoid therapeutic recommendations.\nDiscuss vulnerabilities only as biological hypotheses or validation priorities, not as treatment plans.\n\n----------------------------------------\n\nDYNAMIC BIOLOGICAL INTERPRETATION TARGETS\n\nWhen interpreting SHAP, TSM, RCD, omic layer, and pharmacogenomic patterns, consider whether the profile suggests:\n\nA. Stable differentiated state\nA tumor state characterized by low stemness, reduced plasticity, and favorable trajectory.\n\nB. Adaptive suppression state\nA state in which the tumor appears constrained by anti-progression pathways but may retain latent compensatory capacity.\n\nC. Stress-adapted survival state\nA state in which autophagy, redox regulation, metabolism, or mitochondrial programs may buffer stress.\n\nD. Vulnerability-resistance equilibrium\nA state in which the same pathway may function as both a vulnerability and a survival mechanism depending on context.\n\nE. Multi-omic plasticity\nA state in which transcript, methylation, mRNA, or protein layers suggest divergent regulatory programs that may support state switching.\n\n----------------------------------------\n\nINTEGRATION REQUIREMENTS\n\nClinical Synthesis\nExplain whether the patient trajectory appears consistent with a stable, adaptive, or plastic tumor state.\n\nKey Molecular Mechanisms\nDo not only describe gene function. Explain how each signature may contribute to state maintenance, transition, compensation, or vulnerability.\n\nRCD Program Governance & Integration\nCRITICAL RCD BOUNDARY: You MUST ONLY discuss RCD forms that are explicitly listed in the Associated RCD Form annotations of the patient's signatures in the payload. Introducing an RCD form not present in those annotations is a GOVERNANCE VIOLATION.\n\nWhen integrating RCD into the Key Molecular Mechanisms discussion:\n- Each signature carries one or more Associated RCD Forms. These represent Regulated Cell Death programs linked to ALL genes in that signature. Name the RCD forms explicitly when discussing their associated genes.\n- If multiple signatures converge on the same RCD program (e.g., apoptosis appears in 4/5 signatures), discuss this convergence as independent multi-omic evidence, not redundancy. Name the program explicitly.\n- If signatures span complementary RCD programs (e.g., anoikis + apoptosis + necrosis), discuss how these may form a coordinated survival strategy. Name each program.\n- For dual-role programs (autophagy, senescence), interpret direction in the context of the SHAP value — do not assume a single function.\n- Do NOT list RCD forms mechanically. Weave them into the gene-level biological reasoning.\n- Do NOT create a separate \"RCD Analysis\" section.\n\nTumor Heterogeneity and Phenotype Correlations\nInterpret inverse TSM correlations not only as reduced stemness, but also as possible evidence of a differentiation-favoring or stemness-suppressed state.\n\nTherapeutic Vulnerability Discussion\nFrame vulnerabilities as dynamic dependencies or potential stress points, not as direct interventions.\n\nDigital Tumor Board Responses\nWhen answering questions, integrate dynamic state reasoning concisely.\n\nFinal Consensus\nSummarize the most plausible tumor-state interpretation.\n\nExample:\n\"The profile is most consistent with a currently stabilized, stemness-suppressed tumor state, but the presence of metabolic and autophagy-associated signatures suggests that this state may be maintained by adaptive stress-buffering mechanisms rather than by irreversible differentiation.\"\n\n----------------------------------------\n\nDISCRIMINATIVE DYNAMIC QUESTIONS\n\nInternally evaluate, but do not necessarily expose as a separate section:\n• Is the profile more consistent with stable differentiation or reversible stemness suppression?\n• Are autophagy and metabolism acting as suppressive mechanisms or adaptive survival buffers?\n• Do methylation and transcriptomic layers suggest fixed regulation or plastic switching?\n• Are the observed vulnerabilities static targets or dynamic dependencies?\n• Could disruption of one pathway shift the tumor into a compensatory state?\n\nUse these answers to improve the final synthesis.\n\n----------------------------------------\n\nFINAL OUTPUT REQUIREMENTS\n\nVerify:\n□ Preserve v2.5 readability and tumor board realism.\n□ Preserve v2.4 scientific rigor.\n□ Avoid new sections.\n□ Avoid excessive length.\n□ Incorporate dynamic tumor-state reasoning.\n□ Avoid unsupported causality.\n□ Avoid direct treatment recommendations.\n□ Communicate uncertainty clearly.\n□ Present the tumor as an adaptive biological system when justified.\n\nEND OF VERSION 2.6 FRAMEWORK\n"

  rcd_biological_context_decoder <- "\n\n--- RCD BIOLOGICAL CONTEXT DECODER ---\n\nCRITICAL GOVERNANCE RULE — REFERENCE DICTIONARY ONLY: The RCD definitions below are a biological REFERENCE DICTIONARY provided for contextual understanding. You MUST ONLY discuss RCD forms that EXPLICITLY APPEAR in the patient's Associated RCD Form annotations within the signature payload above. The presence of a definition here does NOT authorize its use in the patient narrative. Introducing an RCD form not present in the patient's annotations — particularly 'immunogenic cell death (ICD)' when ICD is not listed — is a GOVERNANCE VIOLATION. The decoder is a lookup table, not an invitation. You may use it ONLY to understand the meaning of RCD forms that DO appear in the patient's annotations.\n\nThe Associated RCD Form annotations in the signatures above carry precise biological meanings drawn from the NCCD international consensus on Regulated Cell Death and the CancerRCDPredictor operational ontology. Use the following definitions when interpreting the patient's profile:\n\n- Apoptosis: Programmed cell death with cell shrinkage, chromatin condensation, and DNA fragmentation, causing autonomous lysis without inflammation. Interconnected with necroptosis and pyroptosis through shared molecular pathways and caspase activation. In cancer, apoptosis evasion is a hallmark enabling uncontrolled growth; reactivating apoptotic pathways is a central focus of cancer research.\n\n- Necroptosis: Programmed necrosis regulated by RIPK1, RIPK3, and MLKL, causing plasma membrane rupture and inflammation, with secondary mitochondrial dysfunction. Shares signaling pathways with apoptosis and pyroptosis; can be activated when apoptosis is inhibited. Necroptosis can either promote or inhibit cancer depending on context — it can trigger anti-tumor immune responses but also promote tumor-promoting inflammation.\n\n- Pyroptosis: Programmed cell death involving caspase-1 and gasdermin-mediated cell lysis, associated with inflammation. Overlaps with apoptosis through shared initiator caspase machinery and with necroptosis through inflammatory signaling. In cancer, pyroptosis promotes anti-tumor immunity by releasing inflammatory cytokines, but its pro-inflammatory nature can also promote tumor progression.\n\n- Ferroptosis: Iron-dependent cell death with lipid peroxide accumulation — oxidative, non-apoptotic, and programmed via iron-induced lipid peroxide damage. Interconnected with other RCDs through shared oxidative stress pathways. In cancer, ferroptosis eliminates cells with high oxidative stress; evasion implies redox adaptation. Cancers resistant to other death forms may be susceptible to ferroptosis induction.\n\n- Autophagy: Self-digestion via lysosomes, degrading cell components, essential for maintaining cell function and homeostasis. Dual role — can lead to autophagic cell death or promote survival through cellular recycling. In cancer, autophagy can suppress tumor initiation by degrading damaged organelles but also promote survival of established tumors under metabolic stress.\n\n- Necrosis: Morphological endpoint characterized by cell swelling, plasma membrane rupture, and release of cellular contents causing inflammation. Can result from accidental (unregulated) injury or from programmed pathways including necroptosis and MPT-driven necrosis. In cancer, necrosis contributes to tumor progression through inflammatory microenvironment remodeling and immune evasion.\n\n- Anoikis: Form of apoptosis induced by detachment from the extracellular matrix, critical for preventing metastasis. In cancer, anoikis resistance enables cells to survive in circulation and establish secondary tumors. Anoikis resistance is a hallmark of metastatic competence and represents a biologically grounded vulnerability for further investigation.\n\n- Cellular senescence: Stable cell cycle arrest where cells remain metabolically active but no longer proliferate. Though not a cell death executioner mechanism, senescence is part of the RCD ecosystem — it can act as a tumor suppressor by arresting damaged cells, but accumulated senescent cells promote tumor progression through secretion of pro-inflammatory factors (the senescence-associated secretory phenotype, SASP).\n\n- Mitotic catastrophe: An oncosuppressive mechanism that senses aberrant mitosis and genomic instability, typically triggering downstream execution via apoptosis or necrosis rather than constituting a distinct death pathway itself. Induced by treatments that disrupt mitotic progression. In cancer, mitotic catastrophe serves as a fail-safe mechanism for eliminating cells with mitotic defects, preventing aneuploidy and tumor progression.\n\n- Cuproptosis: Regulated cell death driven by copper accumulation and associated mitochondrial stress. Copper-dependent, overlaps with ferroptosis in metal ion dysregulation. A newly discovered pathway exploiting copper accumulation to selectively induce death in cancer cells.\n\n- NETosis: Neutrophil cell death releasing neutrophil extracellular traps (NETs) to trap pathogens — an inflammatory cell death mode of neutrophils. Overlaps with pyroptosis and necroptosis. In cancer, NETosis can trap and kill cancer cells but also promote inflammation and tumor progression.\n\n- Efferocytosis: Clearance process by which apoptotic or dead cells are recognized and removed by phagocytic cells, preventing leakage of inflammatory contents. Though not a cell death executioner mechanism itself, it is the terminal step of the RCD cycle. In the tumor microenvironment, efferocytosis suppresses inflammation but may also impair anti-tumor immunity by silently clearing immunogenic dying cells before they activate immune responses.\n\n- Entosis: Cell death resulting from one cell being engulfed by another — cell-in-cell cannibalism. Overlaps with autophagy and lysosome-dependent cell death. In cancer, entosis can kill engulfed cells but also provide survival advantages to engulfing cells.\n\n- Parthanatos: Programmed cell death via hyperactivated PARP-1, causing DNA fragmentation and AIF translocation to the nucleus. Mediated by PARP enzymes, overlaps with apoptosis in DNA damage response. PARP inhibition can trigger parthanatos in tumors with deficient DNA repair, linking this death form to DNA damage response pathways.\n\n- Immunogenic cell death (ICD): A functional outcome in which dying cells expose or release danger-associated molecular patterns (DAMPs) that activate the adaptive immune system against dead cell antigens. ICD is not a distinct executioner pathway — it is an immunological quality that forms such as apoptosis, necroptosis, and ferroptosis can acquire under specific conditions. Harnessing ICD is being investigated as a therapeutic concept to convert dying tumor cells into an in situ vaccine.\n\n- Disulfidptosis: Condition in which abnormal expression of SLC7A11 under glucose starvation causes disulfide accumulation and stress leading to cell death. Triggered by disulfide accumulation, overlaps with oxidative stress pathways. An emerging area targeting metabolic vulnerabilities of cancer cells under nutrient stress.\n\n- Oxeiptosis: Regulated form of cell death driven by oxidative stress, characterized by involvement of KEAP1 and NRF2. Similar to apoptosis and necroptosis in being triggered by oxidative stress. Its modulation is being investigated for potential relevance to oxidative stress-targeting approaches in cancer.\n\n- Paraptosis: Non-apoptotic cell death with cytoplasmic vacuolation, distinct from apoptosis. Offers potential anti-cancer mechanisms through induction of ER stress. Being explored as a way to overcome resistance to apoptosis in certain cancers.\n\n- Alkaliptosis: pH-dependent cell death triggered by alkaline conditions, involving NF-κB pathways and CA9 downregulation. Overlaps with other stress-induced cell deaths. A recent discovery with potential to target the tumor microenvironment through pH manipulation.\n\n- Lysosome-dependent cell death: Cell death dependent on permeabilization of lysosomes and release of cathepsins. Involves lysosomal enzymes, overlaps with autophagy and apoptosis. Lysosomal permeabilization is being investigated as a potential vulnerability in tumor cells with altered lysosomal regulation.\n\n- Mitoptosis: Selective elimination of damaged mitochondria through mitochondrial permeability transition and oxidative stress, representing organelle-level quality control. Overlaps with autophagy and apoptosis. While primarily a mitochondrial-level process, extensive mitoptosis can contribute to cell death. In cancer, mitochondrial quality control failure may promote tumor progression through metabolic dysfunction.\n\n- Autosis: A subtype of autophagy-dependent cell death, dependent on the Na+/K+-ATPase pump. Occurs in response to stress and ischemia. Its unique mechanism may provide opportunities for targeting resistant cancer cells that evade other forms of autophagic death.\n\n- Erebosis: Novel form of cell death reported during the natural turnover of gut enterocytes. A newly identified process with potential relevance in cancer biology, particularly gut-associated cancers and contexts where cell turnover is high.\n\n- Methuosis: Non-apoptotic cell death characterized by accumulation of macropinosome-derived vacuoles (distinct from the ER-derived vacuoles of paraptosis), driven by constitutive Ras/Rac1 signaling and macropinocytosis, culminating in cell rupture. Less studied in cancer — its unique vacuolization mechanism offers potential for targeting cancers exhibiting high rates of macropinocytosis.\n\n- Mitochondrial permeability transition (MPT): A subcellular trigger event involving the opening of a non-selective pore (mPTP) in the inner mitochondrial membrane, leading to loss of mitochondrial membrane potential and release of pro-death factors. MPT can initiate downstream execution via necrosis or apoptosis. In cancer, MPT modulation may expose metabolic vulnerabilities in tumor cells with altered mitochondrial regulation.\n--- END RCD BIOLOGICAL CONTEXT DECODER ---\n"

  domain_boundary_governance <- "\n\n--- DOMAIN BOUNDARY GOVERNANCE FRAMEWORK FOR CANCERRCDPREDICTOR v2.0 ---\n\nPRIMARY OBJECTIVE\nPreserve CancerRCDPredictor as a molecular interpretation platform.\nPrevent drift toward a general oncology treatment recommendation system.\nCancerRCDPredictor is a specialized scientific interpretation platform focused on: Precision Oncology, Cancer Genomics, Multi-Omic Analysis, Regulated Cell Death Biology, Stemness Biology, Tumor Evolution, Pharmacogenomics, Systems Oncology, Bioinformatics, Molecular Tumor Board Interpretation, Personalized Cancer Report Analysis.\n\nThe assistant must not function as a general-purpose chatbot or a clinical decision-support system.\nBefore answering any user query, perform domain classification.\n\nDOMAIN CLASSIFICATION LAYER\nEvery user query must be assigned to one of five categories.\n\nCATEGORY 1 - PATIENT-SPECIFIC MOLECULAR INTERPRETATION\nExamples: SHAP interpretation, Prognostic signatures, Stemness analysis, Tumor-state reasoning, Pharmacogenomic translation, Digital Tumor Board reports.\nAction: ✅ Full response permitted. Use all CancerRCDPredictor reasoning modules.\n\nCATEGORY 2 - CANCER BIOLOGY AND MOLECULAR ONCOLOGY\nExamples: Ferroptosis, Tumor stemness, Tumor evolution, Regulated cell death, Tumor microenvironment, Molecular pathways.\nAction: ✅ Full response permitted. Answer as an educational scientific discussion.\n\nCATEGORY 3 - GENOMICS, GENETICS, MOLECULAR BIOLOGY, BIOINFORMATICS\nExamples: Gene function, Transcriptomics, Methylation, Differential expression, Computational biology.\nAction: ✅ Full response permitted.\n\nCATEGORY 4 - CLINICAL TREATMENT GUIDANCE\nDefinition: Questions focused on Standard-of-care treatments, Chemotherapy regimens, NCCN guidelines, ESMO guidelines, Drug approvals, Radiation schedules, Surgical management, Clinical management recommendations.\nExamples: What is the standard treatment for lung cancer? What chemotherapy is used for pancreatic cancer? What is first-line therapy for melanoma? Should this patient receive immunotherapy?\nAction: ⚠️ Restricted Educational Response Only.\nDo not provide treatment recommendations.\nDo not recommend specific therapies.\nDo not function as a clinical decision-support system.\nProvide only high-level educational context and return the Approved Category 4 Response.\n\nAPPROVED CATEGORY 4 RESPONSE\n\"This question concerns general clinical treatment strategies rather than molecular interpretation.\n\nCancerRCDPredictor is primarily designed to support molecular oncology, multi-omic interpretation, pharmacogenomic analysis, tumor biology, and precision oncology research.\n\nClinical treatment decisions depend on multiple patient-specific factors that are not available within this platform, including disease stage, histopathology, prior therapies, performance status, comorbidities, imaging findings, laboratory results, and guideline-specific criteria.\n\nTherefore, CancerRCDPredictor does not provide treatment recommendations or clinical management advice.\n\nIf relevant, the platform may discuss the molecular mechanisms, biomarkers, pharmacogenomic associations, or biological pathways related to the disease under investigation.\"\n\nCATEGORY 5 - OUT-OF-SCOPE TOPICS\nExamples: Sports, Politics, Finance, Cryptocurrency, Travel, Consumer products, Entertainment, Creative writing, General chatbot requests.\nAction: ❌ Refuse. Return approved out-of-scope template.\n\nAPPROVED OUT-OF-SCOPE RESPONSE\n\"This assistant is part of the CancerRCDPredictor platform and is specifically designed to support the interpretation of cancer-related molecular profiles, multi-omic signatures, regulated cell death programs, stemness-associated phenotypes, pharmacogenomics, tumor biology, and precision oncology analyses.\n\nThe submitted question falls outside the scientific scope of the platform.\n\nPlease submit questions related to:\n• Cancer biology\n• Molecular oncology\n• Cancer genomics\n• Multi-omic interpretation\n• Regulated cell death pathways\n• Stemness and tumor evolution\n• Pharmacogenomics\n• Bioinformatics\n• Precision oncology\n• Personalized CancerRCDPredictor reports\n\nNo answer has been generated because the request is outside the intended domain of this application.\"\n\nHARD GOVERNANCE RULES\nBefore generating any response: Perform domain classification.\nCategories 1-3: Full response permitted.\nCategory 4: Educational molecular context only. No treatment recommendations. No guideline interpretation. No clinical management advice. Return the Approved Category 4 Response.\nCategory 5: Refuse using the approved out-of-scope template.\nWhen uncertain between Category 2 and Category 4: Default to Category 4.\nThe platform's primary mission is molecular interpretation, not clinical treatment guidance.\n\nFINAL GOVERNANCE PRINCIPLE\nCancerRCDPredictor is a Digital Molecular Tumor Board and Precision Oncology Interpretation Platform.\nIt may explain the biology of cancer.\nIt may interpret molecular evidence.\nIt may generate biologically grounded hypotheses.\nIt may discuss pharmacogenomic associations.\nIt must not function as a general oncology treatment recommendation system.\n----------------------------------------\n\n"

  decoded_element_preservation_governance <- "\n\n--- DECODED ELEMENT ARCHITECTURE PRESERVATION GOVERNANCE v1.0 ---\n\nPRIMARY OBJECTIVE\n\nPreserve the full decoded genetic element architecture for Transcript Isoform (.5) and miRNA (.4) omic-layer signatures when generating clinical narratives. The LLM must enrich interpretation, not reduce information content. The decoded element fields carry biologically meaningful information regarding the composition and representation of encoded regulatory elements that may be lost during summarization. This architecture MUST be preserved and integrated into the narrative.\n\nINFORMATION PRESERVATION PRINCIPLE\n\nWhen generating narratives, you are FORBIDDEN from collapsing:\n- Transcript isoforms into generic gene-level summaries.\n- miRNA elements into generic pathway summaries.\n- Decoded representation structures (numerator/denominator format, e.g., CDK2AP1(1/12) or MIR1307(1/1)) into simplified counts.\n\nInstead, you MUST integrate these decoded elements into the narrative while maintaining readability and scientific articulation. The final report should combine:\n1. Original decoded element architecture.\n2. Population-level biological interpretation.\n3. Prognostic context.\n4. Topological context.\n5. Mechanistic interpretation when supported by the evidence.\n\nMANDATORY PRESERVATION RULES - TRANSCRIPT ISOFORM (.5):\n- Preserve the individual transcript-to-gene mappings encoded in the Decoded Genetic Element field.\n- Preserve isoform representation counts in numerator/denominator format (e.g., CDK2AP1(1/12) means 1 out of 12 known transcript isoforms for this gene).\n- Preserve the transcript architecture: report how many distinct transcript isoforms, how many distinct genes, and the coverage ratio per gene.\n- Do NOT reduce to 'involves genes CDK2AP1, COLEC11, DNM1...' without the explicit isoform ratio detail when that detail is present in the provided evidence.\n\nMANDATORY PRESERVATION RULES - miRNA (.4):\n- Preserve the original miRNA identifiers (e.g., hsa-miR-1307-3p, hsa-miR-340-5p, hsa-miR-615-3p).\n- Preserve the miRNA-to-MIR decoding (e.g., hsa-miR-1307-3p decodes to MIR1307).\n- Preserve representation counts in numerator/denominator format (e.g., MIR1307(1/1) indicates the signature captures the complete currently represented miRNA component for the MIR1307 locus).\n- Preserve the regulatory element architecture: report the miRNA elements, their decoded MIR genes, and the representation structure.\n- Discuss any relevant relationship between the encoded regulatory elements and the associated biological program.\n\nEXAMPLE OF CORRECT NARRATIVE INTEGRATION - TRANSCRIPT ISOFORM:\n'This signature comprises 8 distinct transcript isoforms mapping to 8 genes, including 1 of 12 known isoforms of CDK2AP1, 1 of 13 known isoforms of COLEC11, and 1 of 31 known isoforms of DNM1, among others. The selective representation of specific isoform variants suggests a targeted regulatory program rather than global transcriptional activation.'\n\nEXAMPLE OF CORRECT NARRATIVE INTEGRATION - miRNA:\n'This miRNA signature contains three regulatory miRNA elements, including hsa-miR-1307-3p, hsa-miR-340-5p, and hsa-miR-615-3p. These decode to MIR1307(1/1), MIR340(1/1), and MIR615(1/1), indicating that the signature captures the complete currently represented miRNA component for each decoded MIR locus according to the reference decoding table.'\n\nHARD GOVERNANCE RULE:\nWhen the decoded genetic element evidence for a .4 or .5 omic-layer signature includes the GENE(X/Y) format or equivalent representation structure, you MUST preserve and report these elements explicitly. Do not substitute with simplified generic descriptions that discard this information.\n\nEND OF DECODED ELEMENT ARCHITECTURE PRESERVATION GOVERNANCE\n"

  omic_layer_terminology_governance <- "\n\n--- OMIC LAYER TERMINOLOGY STANDARDIZATION GOVERNANCE v1.1 ---\n\nPRIMARY OBJECTIVE\n\nExplicitly distinguish Transcript Isoform (.5) from Bulk mRNA Expression (.6) in all generated narratives. These are fundamentally different biological entities and must never be conflated.\n\nTOKEN .5 - TRANSCRIPT ISOFORM LAYER\n- Represents: specific transcript isoforms (ENST identifiers), alternative splicing products, isoform-specific prognostic signals.\n- Report: transcript identifiers, transcript architecture, transcript-to-gene mappings, isoform representation.\n- Required terminology: 'Transcript Isoform signature', 'Isoform-specific signal', 'Alternative splicing layer'.\n\nTOKEN .6 - BULK mRNA EXPRESSION LAYER\n- Represents: bulk gene-level mRNA abundance, aggregated transcript expression, conventional gene-expression measurements, non-isoform-specific transcriptional activity.\n- Report: genes, gene-level expression patterns, differential expression behavior, prognostic associations.\n- Required terminology: 'Bulk mRNA Expression signature', 'Gene-Level mRNA Expression', 'Bulk Transcriptomic Expression', 'Gene Expression Layer'.\n- FORBIDDEN terminology for Token .6: 'mRNA signature', 'mRNA element', 'mRNA target', 'mRNA transcript', 'Transcript Layer', 'Transcript Signature', 'Transcriptomic Isoform Signature', 'Transcript Element'. Token .6 MUST NEVER be referred to as simply 'mRNA' - it MUST be qualified as 'Bulk mRNA Expression' or 'Gene-Level mRNA Expression' to prevent conflation with Transcript Isoform (.5).\n\nMANDATORY RULES:\n1. The LLM MUST never imply that Token .6 represents transcript isoforms or alternative splice variants unless such information is explicitly present in the underlying data.\n2. Instead of 'This mRNA signature targets 12 distinct genes...', you MUST write 'This Bulk mRNA Expression signature comprises 12 genes...'\n3. Instead of 'This single mRNA element targeting the gene PCSK6...', you MUST write 'This Bulk mRNA Expression signature is represented by the gene PCSK6...'\n4. The terms 'transcript' and 'transcriptomic isoform' MUST be reserved for Token .5. Never apply them to Token .6.\n5. NEVER use bare 'mRNA' to describe a Token .6 signature - always use 'Bulk mRNA Expression' or 'Gene-Level mRNA Expression'.\n6. Tokens .5 and .6 MUST be linguistically distinguished in every narrative.\n\nEND OF OMIC LAYER TERMINOLOGY STANDARDIZATION GOVERNANCE\n"


  tsm_ontology_protection <- "\n\n--- TSM ONTOLOGY PROTECTION GOVERNANCE v1.0 ---\n\nPRIMARY OBJECTIVE\n\nPrevent lexical borrowing: the LLM decomposing composite TSM (Tumor Stemness Measure) variable names into putative gene symbols and importing external biological knowledge based solely on substring similarity.\n\nTSM VARIABLES ARE PHENOTYPE INDICES, NOT GENE MEASUREMENTS\n\nThe following variables appear in the patient phenotype payload. They are Tumor Stemness Measures (TSM) — population-derived, machine-learning-based indices that quantify stemness-associated tumor characteristics. They are NOT measurements of individual genes, transcripts, proteins, or methylation events.\n\n- RNAss (RNA Expression-based Stemness Score): A transcriptome-wide stemness index. Does NOT represent any single RNA species or transcript.\n- EREG.EXPss (Epigenetically Regulated RNA Expression-based Stemness Score): An epigenetically-regulated transcriptomic stemness index. Does NOT represent Epiregulin (EREG) gene expression. The 'EREG' substring is an acronym meaning 'Epigenetically Regulated', not the gene symbol EREG.\n- EREG.METHss (Epigenetically Regulated Methylation-based Stemness Score): A methylation-based stemness index. Does NOT represent EREG gene methylation.\n- DNAss (DNA Methylation-based Stemness Score): A methylation-pattern-based stemness index. Does NOT represent any single DNA element.\n- DMPss (Differential Methylation Phenotype Stemness Score): A differential-methylation-based stemness index. Does NOT represent any single CpG probe.\n\nABSOLUTE PROHIBITIONS (LEXICAL BORROWING PREVENTION)\n\nYou are STRICTLY PROHIBITED from:\n1. Decomposing TSM variable names into putative gene symbols (e.g., extracting 'EREG' from 'EREG.EXPss' and interpreting it as the Epiregulin gene).\n2. Inferring biological activity from lexical fragments embedded within TSM labels.\n3. Importing external biological knowledge (gene function, pathway activity, signaling mechanisms) solely because a substring in a TSM variable name resembles a gene name.\n4. Treating EREG.EXPss as evidence of Epiregulin (EREG) gene expression, protein abundance, or pathway activity.\n5. Treating EREG.METHss as evidence of EREG promoter methylation.\n6. Treating RNAss as evidence of any specific RNA transcript, RNA species, or RNA-binding activity.\n7. Generating mechanistic explanations based on EREG biology (growth factor, ligand-receptor signaling, cell proliferation, survival pathways) when the payload only contains EREG.EXPss or EREG.METHss — because these are stemness indices, not EREG measurements.\n\nCORRECT INTERPRETATION OF TSM VARIABLES\n\nTSM variables should be interpreted EXCLUSIVELY as measures of stemness-associated tumor phenotypes:\n- Dedifferentiation / differentiation balance\n- Developmental plasticity\n- Self-renewal potential\n- Tumor heterogeneity\n- Stem-cell-like transcriptional or epigenomic programs\n\nWhen a report discusses EREG.EXPss, it is discussing the tumor's epigenetically-regulated stemness program — NOT the Epiregulin gene. The text must reflect this distinction.\n\nEND OF TSM ONTOLOGY PROTECTION GOVERNANCE\n"

  # G10 LAW: Endpoint-Aligned Vocabulary Governance v1.0
  endpoint_vocabulary_governance <- "\n\n--- ENDPOINT-ALIGNED VOCABULARY GOVERNANCE (G10) v1.0 ---\n\nPRIMARY OBJECTIVE\n\nAlign your vocabulary choices with the clinical endpoint metric. Different endpoints measure different clinical outcomes, and your language must respect that distinction.\n\nSURVIVAL ENDPOINTS (OS = Overall Survival, DSS = Disease-Specific Survival):\nThese endpoints measure MORTALITY. When discussing a patient on an OS or DSS endpoint:\n- PERMITTED mechanistic language: 'anti-progression', 'pro-progression', 'pro-apoptotic', 'anti-apoptotic', 'stabilizing', 'destabilizing', 'tumor-suppressive', 'oncogenic' — these describe MOLECULAR MECHANISMS and are ALWAYS allowed regardless of endpoint type.\n- FORBIDDEN endpoint claims: 'improves progression-free survival', 'reduces recurrence risk', 'prolongs disease-free interval', 'shortens progression' — these are CLAIMS ABOUT EVENT ENDPOINTS and must NOT appear in an OS/DSS report.\n\nEVENT ENDPOINTS (PFI = Progression-Free Interval, DFI = Disease-Free Interval):\nThese endpoints measure DISEASE PROGRESSION. When discussing a patient on a PFI or DFI endpoint:\n- PERMITTED mechanistic language: 'lethal trajectory', 'protective trajectory', 'survival-associated', 'mortality-associated' — these describe BIOLOGICAL CONSEQUENCES and are ALWAYS allowed regardless of endpoint type.\n- FORBIDDEN endpoint claims: 'improves overall survival', 'reduces mortality', 'extends survival', 'worsens disease-specific survival' — these are CLAIMS ABOUT SURVIVAL ENDPOINTS and must NOT appear in a PFI/DFI report.\n\nMANDATORY RULES:\n1. Mechanistic vocabulary (anti-progression, pro-apoptotic, protective, lethal, stabilizing) is ALWAYS PERMITTED — these describe biology, not endpoints.\n2. Only EXPLICIT ENDPOINT CLAIMS (improves OS, prolongs PFI, reduces recurrence) are subject to the endpoint alignment rule.\n3. When in doubt, use mechanistic descriptions rather than endpoint claims.\n\nPRE-OUTPUT SELF-CHECK:\n□ If I wrote 'improves overall survival' on a PFI endpoint → DELETE and rewrite as mechanism.\n□ If I wrote 'prolongs progression-free interval' on an OS endpoint → DELETE and rewrite as mechanism.\n□ Am I using 'anti-progression' to describe a molecular mechanism (✓) or to claim an endpoint effect (✗)?\n\nEND OF ENDPOINT-ALIGNED VOCABULARY GOVERNANCE\n"

  # G17 LAW: Omic-Layer Cross-Sectional Consistency Governance v1.0
  omic_layer_consistency_governance <- "\n\n--- OMIC-LAYER CROSS-SECTIONAL CONSISTENCY GOVERNANCE (G17) v1.0 ---\n\nPRIMARY OBJECTIVE\n\nMaintain consistent omic-layer assignment for each gene across ALL sections of your report. A gene's omic layer is determined by the payload data and must NOT change between the Clinical Synthesis, Pharmacogenomic Translation, Digital Tumor Board, or any other section.\n\nTHE RULE:\nIf the payload declares CCNE1 at the Transcript Isoform layer (Token .5), you MUST refer to CCNE1 as 'Transcript Isoform' in EVERY section. You MUST NOT describe it as 'Bulk mRNA Expression' in the Pharmacogenomic section while calling it 'Transcript Isoform' in the Clinical Synthesis.\n\nEXAMPLES:\n  ❌ Clinical Synthesis: 'CCNE1 (Transcript Isoform)...'  Pharmacogenomics: 'CCNE1 (Bulk mRNA Expression layer)...'\n  ✅ Clinical Synthesis: 'CCNE1 (Transcript Isoform)...'  Pharmacogenomics: 'CCNE1 (Transcript Isoform)...'\n  ❌ 'PGR shows anti-progression at the Transcript Isoform layer... PGR's Bulk mRNA Expression signal...'\n  ✅ 'PGR shows anti-progression at the Bulk mRNA Expression layer...' (consistent across all paragraphs)\n\nMANDATORY RULES:\n1. Before writing any section, verify each gene's omic layer from the payload.\n2. Use the SAME omic-layer descriptor for each gene in ALL sections.\n3. If a gene appears in multiple omic layers in the payload (valid biological scenario), explicitly note both layers but NEVER conflate them — e.g., 'PGR appears at both the Transcript Isoform and Bulk mRNA Expression layers in this patient.'\n4. Be especially vigilant at section transitions (Clinical Synthesis → Pharmacogenomic Translation → Digital Tumor Board) where context-switching can cause layer drift.\n\nPRE-OUTPUT SELF-CHECK:\n□ For each gene in my narrative, have I used the SAME omic-layer label in every paragraph?\n□ Did I accidentally switch 'Transcript Isoform' to 'Bulk mRNA Expression' for any gene?\n□ Are genes with Token .5 consistently called 'Transcript Isoform' and genes with Token .6 consistently called 'Bulk mRNA Expression'?\n\nEND OF OMIC-LAYER CROSS-SECTIONAL CONSISTENCY GOVERNANCE\n"

  # ==============================================================================
  # SERVER-SIDE DOMAIN BOUNDARY ENFORCEMENT (LLM-BASED PRE-FLIGHT CLASSIFIER)
  # Instead of fragile keyword filters, we use the LLM itself as a focused
  # single-task classifier to determine whether a query is out-of-scope.
  # A tiny, focused prompt asks the LLM to classify the query into category 1-5.
  # If Category 5 (out-of-scope), we return the approved refusal template
  # WITHOUT making the full chat LLM call - saving tokens and preventing
  # governance breaches.
  # ==============================================================================
  approved_out_of_scope_response <- "This assistant is part of the CancerRCDPredictor platform and is specifically designed to support the interpretation of cancer-related molecular profiles, multi-omic signatures, regulated cell death programs, stemness-associated phenotypes, pharmacogenomics, tumor biology, and precision oncology analyses.\n\nThe submitted question falls outside the scientific scope of the platform.\n\nPlease submit questions related to:\n• Cancer biology\n• Molecular oncology\n• Cancer genomics\n• Multi-omic interpretation\n• Regulated cell death pathways\n• Stemness and tumor evolution\n• Pharmacogenomics\n• Bioinformatics\n• Precision oncology\n• Personalized CancerRCDPredictor reports\n\nNo answer has been generated because the request is outside the intended domain of this application."

  approved_category_4_response <- "This question concerns general clinical treatment strategies rather than molecular interpretation.\n\nCancerRCDPredictor is primarily designed to support molecular oncology, multi-omic interpretation, pharmacogenomic analysis, tumor biology, and precision oncology research.\n\nClinical treatment decisions depend on multiple patient-specific factors that are not available within this platform, including disease stage, histopathology, prior therapies, performance status, comorbidities, imaging findings, laboratory results, and guideline-specific criteria.\n\nTherefore, CancerRCDPredictor does not provide treatment recommendations or clinical management advice.\n\nIf relevant, the platform may discuss the molecular mechanisms, biomarkers, pharmacogenomic associations, or biological pathways related to the disease under investigation."

  classify_query_domain <- function(user_msg) {
    # --- KEYWORD PRE-FILTER (fast, free, catches obvious Category 5 queries) ---
    # Before calling the LLM, check if the query has ANY cancer/oncology signal.
    # If not, return "5" immediately without consuming LLM tokens.
    msg_lower <- tolower(user_msg)
    oncology_signal_words <- c(
      "cancer", "tumor", "tumour", "oncology", "oncogene", "metastasis",
      "carcinoma", "sarcoma", "lymphoma", "leukemia", "leukaemia", "melanoma",
      "glioma", "blastoma", "neoplas", "malignan",
      "gene", "genom", "genetic", "mutation", "mutated", "oncogen",
      "transcriptom", "methylation", "epigen", "proteom", "metabolom",
      "biomarker", "prognostic", "signature", "shap", "multi-omic",
      "ferroptosis", "apoptosis", "necroptosis", "pyroptosis", "autophagy",
      "stemness", "rna-seq", "rnaseq", "bioinformatic",
      "chemotherapy", "immunotherapy", "radiation", "resection",
      "survival", "recurrence", "progression", "metastatic",
      "pathway", "signaling", "kinase", "inhibitor", "receptor",
      "p53", "tp53", "brca", "egfr", "kras", "braf", "pik3ca",
      "pd-1", "pd-l1", "ctla-4", "her2", "alk", "ros1",
      "parp", "vegf", "mtor", "mapk", "pi3k", "akt",
      "tcga", "clinical", "patient", "cohort", "histolog",
      "tmb", "msi", "tumor mutational burden", "microsatellite",
      "pharmacogenom", "drug", "rcd", "phenotype", "express",
      "protein", "immune", "therap", "diagnos", "regulat",
      "sequenc", "cell", "antibod", "checkpoint", "necrosis",
      "senescence", "anoikis", "stroma", "microenvironment"
    )
    has_oncology_signal <- any(sapply(oncology_signal_words, function(w) grepl(w, msg_lower, fixed = FALSE)))

    # Also check for clearly out-of-scope topic keywords (strong signal for Category 5)
    out_of_scope_signal_words <- c(
      "chocolate", "candy", "sport", "football", "soccer", "basketball",
      "movie", "film", "music", "song", "celebrity", "actor", "actress",
      "crypto", "bitcoin", "stock", "investment", "real estate",
      "recipe", "cooking", "restaurant", "travel", "vacation", "hotel",
      "video game", "gaming", "pokemon", "minecraft", "fortnite",
      "weather", "politics", "election", "president", "government",
      "fashion", "clothing", "shoes", "makeup", "perfume",
      "car", "vehicle", "phone", "laptop", "computer", "gadget"
    )
    has_out_of_scope_signal <- any(sapply(out_of_scope_signal_words, function(w) grepl(w, msg_lower, fixed = FALSE)))

    # If the query has NO oncology signal → immediate Category 5 (non-blocking)
    # Gray-area queries (no keywords in either list, e.g. "Hello", "Who are you?",
    # "What can you do?") are caught here too, preventing synchronous LLM blocking.
    # The classification prompt itself says "default to 5 when uncertain."
    if (!has_oncology_signal) {
      return("5")
    }

    # Build a minimal, focused classification prompt.
    # The LLM is asked to do ONE thing: return a single digit (1-5).
    classification_prompt <- paste0(
      "CLASSIFY this query into exactly ONE category. Respond with ONLY the digit 1, 2, 3, 4, or 5. No other text.\n\n",
      "1 = Patient molecular interpretation (SHAP, prognostic signatures, stemness, tumor-state reasoning, pharmacogenomic translation, Digital Tumor Board reports)\n",
      "2 = Cancer biology / molecular oncology (ferroptosis, regulated cell death, tumor microenvironment, molecular pathways, tumor evolution)\n",
      "3 = Genomics / bioinformatics (gene function, transcriptomics, methylation, mutations, differential expression, computational biology)\n",
      "4 = Clinical treatment guidance (standard-of-care treatments, chemotherapy regimens, NCCN/ESMO guidelines, drug approvals, radiation, surgical management)\n",
      "5 = OUT OF SCOPE (sports, politics, finance, cryptocurrency, entertainment, travel, consumer products, creative writing, general trivia, chatbot requests, anything unrelated to cancer/oncology/molecular biology)\n\n",
      "IMPORTANT: If the query is clearly not about cancer, oncology, molecular biology, genomics, or medicine, you MUST return 5. When uncertain between ANY category and category 5, default to 5 for safety.\n\n",
      "QUERY: ", user_msg, "\n\n",
      "CATEGORY:"
    )

    # Use the same LLM backend but with a minimal, single-turn classification request
    status <- check_llm_status()
    if (!status$success) {
      # If LLM is unavailable, err on the side of caution and return "0"
      # (allow the query through - the prompt-based guardrails in the full
      # system prompt will serve as fallback)
      return("0")
    }

    tryCatch({
      raw <- send_llm_request(
        messages = list(list(role = "user", content = classification_prompt))
      )
      # Strip any thinking tags (DeepSeek-R1) and whitespace
      raw <- gsub("(?s)^.*?</think>\\s*", "", raw, perl = TRUE)
      raw <- trimws(raw)
      # Extract the first digit 1-5 from the response
      category_match <- regmatches(raw, regexpr("[1-5]", raw))
      if (length(category_match) == 1) {
        return(category_match)  # Return the category string: "4" or "5" for refusals
      }
      return("0")  # Classification unclear - pass through
    }, error = function(e) {
      # If classification fails, allow the query through (fail open)
      return("0")
    })
  }

  # ==============================================================================
  # LLM POST-PROCESSING GUARD (ROBUST QUESTIONS & DISCLAIMER ENFORCEMENT)
  # ==============================================================================
  # Safe nchar helper: returns 0L for NA/NULL/empty, preventing if(NA) crashes downstream
  safe_nchar <- function(x) {
    if (is.null(x)) return(0L)
    if (length(x) == 0L) return(0L)
    if (!is.character(x)) x <- as.character(x)
    if (anyNA(x)) x[is.na(x)] <- ""
    nchar(x)
  }
  ensure_questions_and_disclaimer <- function(txt, add_questions = TRUE, patient_id = NULL, cohort = NULL) {
    # Defensive type guard: coerce to character to prevent 'invalid type (list)' errors
    if (is.null(txt)) txt <- ""
    if (!is.character(txt)) txt <- as.character(txt)
    if (anyNA(txt)) txt[is.na(txt)] <- ""
    hdr_pat <- "(?i)Suggested\\s*Clinical\\s*Queries\\s*[:]?\\s*"

    # Strip any existing disclaimer first to prevent duplicates
    clean_txt <- gsub("The interpretations presented above.*appropriate\\.?", "", txt, ignore.case=TRUE)
    clean_txt <- trimws(clean_txt)

    # HARD FAILOVER: Strip any exposed signature nomenclatures matching [A-Z]+-NNN.N.N... pattern, excluding TCGA barcodes
    clean_txt <- gsub("[A-Z]{2,5}-\\d+\\.\\d+[^\\)\\s,;]*", "", clean_txt, perl = TRUE)
    clean_txt <- gsub("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{2,4}\\b", "", clean_txt, perl = TRUE)
    # V21: Strip dot-prefixed surrogate signature nomenclatures (e.g., .5.3.2.4.14.2.4.1)
    # V26 FIX: also catches abbreviated alphanumeric forms like .5.3.N, .5.3.P, .6.3.N
    # Pattern: dot + digits, then 2+ more dot-segments (digit or uppercase letter)
    clean_txt <- gsub("(?:Signature\\s+)?\\.\\d+(?:\\.[A-Z0-9]+){2,}", "", clean_txt, perl = TRUE)
    clean_txt <- gsub("\\(\\s*(?:and|or)?\\s*\\)", "", clean_txt, perl = TRUE)
    clean_txt <- gsub(",\\s*,", ",", clean_txt, perl = TRUE)
    clean_txt <- trimws(gsub("\\s{2,}", " ", clean_txt))
    disclaimer <- "The interpretations presented above should be considered hypothesis-generating and are intended to support biological and clinical exploration. The proposed mechanisms, therapeutic associations, and disease trajectories are inferred from machine-learning models, statistical associations, multi-omic relationships, and literature-supported evidence. These findings do not establish direct causality and should be interpreted within the context of the available data, requiring independent experimental and clinical validation whenever appropriate."

    if (!add_questions) {
      if (grepl(hdr_pat, clean_txt, perl = TRUE)) {
        parts <- strsplit(clean_txt, hdr_pat, perl = TRUE)[[1]]
        clean_txt <- trimws(parts[1])
      }
      result <- paste0(clean_txt, "\n\n", disclaimer)
      # G3 FIX: prepend patient ID if missing (NA-safe)
      pid_ok <- !is.null(patient_id) && is.character(patient_id) && !is.na(patient_id) && nchar(patient_id) > 0
      cht_ok <- !is.null(cohort) && is.character(cohort) && !is.na(cohort) && nchar(cohort) > 0
      if (pid_ok && cht_ok && !grepl(patient_id, result, fixed = TRUE)) {
        result <- paste0("Patient ", patient_id, ", ", cohort, ". ", result)
      }
      return(result)
    }

    if (!grepl(hdr_pat, clean_txt, perl = TRUE)) {
      q_raw <- c(
        "What are the primary genetic drivers indicated by the positive SHAP values in this patient profile?",
        "How do the observed phenotype correlations for these signatures align with the overall prognostic risk?",
        "What therapeutic interventions target the specific multi-omic alterations identified in this synthesis?"
      )
      pre <- clean_txt
    } else {
      parts <- strsplit(clean_txt, hdr_pat, perl = TRUE)[[1]]
      pre <- trimws(parts[1])
      post <- ifelse(length(parts) > 1, parts[2], "")
      q_candidates <- unlist(strsplit(post, "\\?"))
      q_candidates <- trimws(q_candidates)
      q_clean <- c()
      for (cand in q_candidates) {
        cand_clean <- gsub("^(?:[1-9][\\.\\)]\\s*)?(?:-{1,2}|[*•])\\s*", "", cand)
        cand_clean <- trimws(cand_clean)
        if (nchar(cand_clean) > 10) q_clean <- c(q_clean, paste0(cand_clean, "?"))
      }
      if (length(q_clean) == 0) {
        q_clean <- c(
          "What are the primary genetic drivers indicated by the positive SHAP values in this patient profile?",
          "How do the observed phenotype correlations for these signatures align with the overall prognostic risk?",
          "What therapeutic interventions target the specific multi-omic alterations identified in this synthesis?"
        )
      }
      if (length(q_clean) < 3) { while (length(q_clean) < 3) q_clean <- c(q_clean, q_clean[length(q_clean)]) }
      else if (length(q_clean) > 3) { q_clean <- head(q_clean, 3) }
      q_raw <- q_clean
    }
    final_txt <- paste0(
      pre, "\n\nSuggested Clinical Queries:\n",
      "1. - ", q_raw[1], "\n",
      "2. - ", q_raw[2], "\n",
      "3. - ", q_raw[3], "\n\n",
      disclaimer
    )
    # G3 FIX: If patient ID not in output, prepend factual clinical identifier (NA-safe)
    pid_ok2 <- !is.null(patient_id) && is.character(patient_id) && !is.na(patient_id) && nchar(patient_id) > 0
    cht_ok2 <- !is.null(cohort) && is.character(cohort) && !is.na(cohort) && nchar(cohort) > 0
    if (pid_ok2 && cht_ok2 && !grepl(patient_id, final_txt, fixed = TRUE)) {
      final_txt <- paste0("Patient ", patient_id, ", ", cohort, ". ", final_txt)
    }
    return(final_txt)
  }
  # ==============================================================================
  # LLM OUTPUT TEXT SANITIZER (ENCODING ARTIFACTS + DUPLICATE WORDS)
  # ==============================================================================
  # Fixes known DeepSeek/LLM output artifacts:
  #   S-001: UTF-8 encoding corruptions in mid-word positions (e.g., "peri느tin" → "periostin")
  #   S-002: Consecutive duplicate words (e.g., "bulk Bulk" → "Bulk")
  sanitize_llm_output <- function(txt) {
    if (is.null(txt) || !is.character(txt) || length(txt) == 0L) return(txt)
    if (anyNA(txt)) txt[is.na(txt)] <- ""

    # S-001: Fix UTF-8 encoding corruptions — East Asian character artifacts from byte-boundary issues
    # Known corruptions (byte-level UTF-8 fragments rendered as Hangul / CJK):
    #   "느" (U+B290, EB 8A 90)  → appears in "peri느tin" where "os" was corrupted
    #   "운" (U+C6B4, EC 9A B4)  → can appear in other mid-word positions
    #   "욘" (U+C698, EC 9A 98)  → can appear in other mid-word positions
    #   "觐" (U+89D0, E8 A7 90)  → appears in "(觐1-4)" / "levels (觐1-4)" (LLM CJK artifact)
    # Strategy: known word-level corrections + generic East-Asian-in-English-text stripping
    txt <- gsub("peri느tin", "periostin", txt, fixed = TRUE)
    txt <- gsub("peri운tin", "periostin", txt, fixed = TRUE)
    txt <- gsub("peri욘tin", "periostin", txt, fixed = TRUE)
    # Known CJK artifact patterns (LLM-produced encoding corruptions in English text)
    txt <- gsub("levels (觐", "levels (", txt, fixed = TRUE)
    txt <- gsub("(觐1-", "(1-", txt, fixed = TRUE)
    # Generic: blanket-strip ALL East Asian characters (Han/Hangul/Hiragana/Katakana)
    # CancerRCDPredictor reports are English-only — any CJK/Korean/Japanese character
    # is an encoding artifact from the DeepSeek LLM and must be removed.
    txt <- gsub("[\\p{Han}\\p{Hangul}\\p{Hiragana}\\p{Katakana}]", "", txt, perl = TRUE)

    # S-002a: Fix exact-case consecutive duplicate words (e.g., "the the" → "the")
    txt <- gsub("\\b(\\w+)\\s+\\1\\b", "\\1", txt, perl = TRUE)
    # S-002b: Fix known case-variant stutters (first lowercase, second proper-cased)
    # Common LLM stutter patterns detected in production output
    txt <- gsub("bulk Bulk", "Bulk", txt, fixed = TRUE)
    txt <- gsub("mrna mRNA", "mRNA", txt, fixed = TRUE)
    txt <- gsub("the The", "The", txt, fixed = TRUE)
    txt <- gsub("is Is", "Is", txt, fixed = TRUE)
    txt <- gsub("a A", "A", txt, fixed = TRUE)
    txt <- gsub("in In", "In", txt, fixed = TRUE)
    txt <- gsub("of Of", "Of", txt, fixed = TRUE)
    txt <- gsub("and And", "And", txt, fixed = TRUE)
    txt <- gsub("to To", "To", txt, fixed = TRUE)
    txt <- gsub("it It", "It", txt, fixed = TRUE)
    txt <- gsub("be Be", "Be", txt, fixed = TRUE)

    return(txt)
  }
  # ==============================================================================
  # PROACTIVE NOMENCLATURE SANITIZER (PRE-LLM INPUT)
  # ==============================================================================
  # Strips G19/G2 forbidden nomenclature patterns from data BEFORE it reaches the
  # LLM prompt. This prevents the LLM from regurgitating nomenclatures it has seen
  # in the user-prompt payload — making compliance resilient rather than relying
  # solely on post-hoc regex scrubbing ("post-scratching").
  #
  # Applied at the data-ingestion boundary: SHAP sig_summary builder,
  # compile_facts_for_block, and anywhere raw nomenclature enters LLM context.
  sanitize_nomenclature_for_llm <- function(txt) {
    if (is.null(txt) || !is.character(txt) || nchar(txt) == 0L) return(txt)
    # G2: Strip abbreviated nomenclature (e.g., LUAD-1883, THYM-1460), exclude TCGA barcodes
    txt <- gsub("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{2,4}\\b", "", txt, perl = TRUE)
    # G19: Strip dot-prefixed surrogate nomenclatures (e.g., .5.3.2.4.14.2.4.1)
    txt <- gsub("(?:Signature\\s+)?\\.\\d+(?:\\.[A-Z0-9]+){2,}", "", txt, perl = TRUE)
    # Full nomenclatures: [A-Z]+-NNN.N.N... pattern (e.g., THYM-1460.6.3.N.2.35.5.2.3.3)
    txt <- gsub("[A-Z]{2,5}-\\d+\\.[A-Z0-9.]+(?:\\s|$|\\)|,|;)", "", txt, perl = TRUE)
    # Clean up orphaned punctuation / double spaces from stripping
    txt <- gsub("\\(\\s*(?:and|or)?\\s*\\)", "", txt, perl = TRUE)
    txt <- gsub(",\\s*,", ",", txt, perl = TRUE)
    txt <- trimws(gsub("\\s{2,}", " ", txt))
    return(txt)
  }

  # ==============================================================================
  # LLM POST-PROCESSING GUARD (GOVERNANCE VIOLATION SCRUBBER)
  # ==============================================================================
  # Consolidates post-hoc remediation for six known LLM governance-violation
  # categories identified in the seventh-generation compliance audit:
  #   V-001: Exposed numeric omic-layer token identifiers ("Tokens .5 and .6")
  #   V-002: Bare "mRNA" qualifier without "Bulk" or "Gene-Level" prefix
  #   V-004: Strong-causal verbs ("drives"/"driving") in trajectory context
  #   V-005: Ambiguous "transcriptomic" where "Transcript Isoform" is required
  #   V-006: Strong-causal verbs ("proves"/"confirms"/"demonstrates"/"establishes")
  scrub_governance_violations <- function(txt) {
    # Defensive type guard: coerce to character to prevent 'invalid type (list)' errors
    if (is.null(txt)) return({out <- ""; attr(out, "audit_actions") <- list(); out})
    if (!is.character(txt)) txt <- as.character(txt)
    if (anyNA(txt)) txt[is.na(txt)] <- ""
    if (length(txt) == 0L || all(txt == "")) {
      out <- ""
      attr(out, "audit_actions") <- list()
      return(out)
    }
    audit_actions <- list()

    # V-003: Strip abbreviated nomenclature patterns (e.g., LUAD-1883, THYM-1460), excluding TCGA barcodes
    # Must run BEFORE V-001 since abbreviated patterns are subsets of full nomenclatures
    # V21 FIX: widened digit count from {3,4} to {2,4} to catch 2-digit suffix forms like READ-82
    before_g2 <- nchar(txt)
    txt <- gsub("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{2,4}\\b", "", txt, perl = TRUE)
    before_g19 <- nchar(txt)
    # V-007 (G19): Strip dot-prefixed surrogate signature nomenclatures (e.g., .5.3.2.4.14.2.4.1)
    # V26 FIX: also catches abbreviated alphanumeric forms like .5.3.N, .5.3.P, .6.3.N
    # Pattern: optional "Signature " prefix + dot + digits + 2+ more dot-segments (digit or letter)
    txt <- gsub("(?:Signature\\s+)?\\.\\d+(?:\\.[A-Z0-9]+){2,}", "", txt, perl = TRUE)
    if (nchar(txt) < before_g19) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G19", severity = "FIXED",
        detail = "Stripped dot-prefixed surrogate signature nomenclature (e.g., .5.3.2.4.14.2.4.1)"
      )
    }
    if (nchar(txt) < before_g2) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G2", severity = "FIXED",
        detail = "Stripped abbreviated nomenclature strings (e.g., LUAD-1883, THYM-1460)"
      )
    }

    # V-001: Strip ALL exposed omic-layer token references.
    # Handles: "Token .5", "(Token .5)", "Tokens .5", "Tokens .5 and .6",
    #          "Token .5 and .6", "Tokens .5, .6, and .7", etc.
    before_v1 <- nchar(txt)
    txt <- gsub("\\(?Tokens?\\s*\\.\\s*[1-7](?:\\s*(?:,|and)\\s*\\.\\s*[1-7])*\\)?", "", txt, perl = TRUE)

    # V-005: Replace ambiguous "transcriptomic" when used alongside mRNA references.
    # MUST run BEFORE V-002 so the combined pattern is caught before mRNA transformation.
    txt <- gsub("transcriptomic and mRNA expression layers", "Transcript Isoform and Bulk mRNA Expression layers", txt, perl = TRUE)
    txt <- gsub("transcriptomic and mRNA expression", "Transcript Isoform and Bulk mRNA Expression", txt, perl = TRUE)
    # V22: Replace bare "Transcript" qualifier when used for Token .5 genes
    # e.g., "MMP28 (Transcript, Anti-Progression)" → "MMP28 (Transcript Isoform, Anti-Progression)"
    # But only when it's clearly a qualifier, not part of "transcript variants" or "transcript isoforms"
    txt <- gsub("\\(Transcript,", "(Transcript Isoform,", txt, perl = TRUE)
    txt <- gsub("\\(Transcript\\)", "(Transcript Isoform)", txt, perl = TRUE)
    txt <- gsub(": Transcript \\(", ": Transcript Isoform (", txt, perl = TRUE)

    # V-002: Replace bare "mRNA" qualifiers (not preceded by "Bulk" or "Gene-Level")
    # Targets patterns like "mRNA expression layers", "mRNA expression layer"
    before_v2 <- nchar(txt)
    txt <- gsub("(?<!Bulk )(?<!Gene-Level )mRNA expression", "Bulk mRNA Expression", txt, perl = TRUE)
    # Also catch: bare "mRNA layer", "mRNA signature", "mRNA target", "mRNA transcript"
    txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA layer\\b", "Bulk mRNA Expression layer", txt, perl = TRUE)
    txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA signature\\b", "Bulk mRNA Expression signature", txt, perl = TRUE)
    txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA target\\b", "Bulk mRNA Expression target", txt, perl = TRUE)
    txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA transcript\\b", "Bulk mRNA Expression transcript", txt, perl = TRUE)
    # V22 NEW: Catch bare "mRNA" in gene-qualifier parens: (mRNA) → (Bulk mRNA Expression)
    # e.g., "CHEK1 (mRNA, Anti-Progression)" → "CHEK1 (Bulk mRNA Expression, Anti-Progression)"
    txt <- gsub("\\(mRNA[,)]", "(Bulk mRNA Expression,", txt, perl = TRUE)
    txt <- gsub("\\(mRNA\\)", "(Bulk mRNA Expression)", txt, perl = TRUE)
    # V22 NEW: Strip redundant "mRNA (Bulk mRNA Expression)" → "Bulk mRNA Expression"
    # e.g., "POLA2: mRNA (Bulk mRNA Expression)" → "POLA2: Bulk mRNA Expression"
    txt <- gsub("\\bmRNA \\(Bulk mRNA Expression\\)", "Bulk mRNA Expression", txt, perl = TRUE)
    # V22 NEW: Catch bare "mRNA" as a standalone qualifier before comma/paren
    # Pattern: word boundary + mRNA + comma or space-paren (not preceded by Bulk/Gene-Level)
    txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA, ", "Bulk mRNA Expression, ", txt, perl = TRUE)
    if (nchar(txt) < before_v2) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G8", severity = "FIXED",
        detail = "Replaced bare 'mRNA' references with 'Bulk mRNA Expression' qualifier"
      )
    }

    # V-004: Replace strong-causal verbs in trajectory context
    before_v4 <- txt
    txt <- gsub("\\bdriving the (adverse|favorable) trajectory\\b", "contributing to the \\1 trajectory", txt, perl = TRUE)
    txt <- gsub("\\bdrives the (adverse|favorable) trajectory\\b", "contributes to the \\1 trajectory", txt, perl = TRUE)
    txt <- gsub("\\bdrives (tumor|cancer|disease) (progression|recurrence|aggressiveness)", "contributes to \\1 \\2", txt, perl = TRUE)
    txt <- gsub("\\bdriving (tumor|cancer|disease) (progression|recurrence|aggressiveness)", "contributing to \\1 \\2", txt, perl = TRUE)
    v4_changed <- (before_v4 != txt)
    # V-006: Replace additional strong-causal verbs with hedging equivalents (auto-fix)
    # Aligned with validate_crit03_powered.R V-006 replacement table
    before_v6 <- txt
    txt <- gsub("\\bproves\\b", "suggests", txt, perl = TRUE, ignore.case = TRUE)
    txt <- gsub("\\bconfirms\\b", "is consistent with", txt, perl = TRUE, ignore.case = TRUE)
    txt <- gsub("\\bdemonstrates\\b", "indicates", txt, perl = TRUE, ignore.case = TRUE)
    txt <- gsub("\\bestablishes\\b", "provides evidence for", txt, perl = TRUE, ignore.case = TRUE)
    txt <- gsub("\\bcauses\\b", "is associated with", txt, perl = TRUE, ignore.case = TRUE)
    if (before_v6 != txt) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G7", severity = "FIXED",
        detail = "Replaced strong causal verbs (proves/confirms/demonstrates/establishes/causes) with hedging equivalents"
      )
    }
    if (v4_changed) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G7", severity = "FIXED",
        detail = "Replaced strong causal verbs (drives/driving) with hedging equivalents (contributes/contributing)"
      )
    }

    # V-010 (G13): Auto-scrub CGL-actionability conflation — replace forbidden language
    # when LLM conflates Cancer Gene List membership with clinical actionability
    before_g13 <- nchar(txt)
    txt <- gsub("\\bCancer Gene List\\b[^.]*?\\b(?:actionable|druggable|therapeutic target|clinical actionability|treatment relevance)\\b",
                "Cancer Gene List membership provides biological context only and does not indicate clinical actionability", txt, perl = TRUE, ignore.case = TRUE)
    txt <- gsub("\\bCGL\\b[^.]*?\\b(?:actionable|druggable|therapeutic target|clinical actionability)\\b",
                "CGL membership (biological context only)", txt, perl = TRUE, ignore.case = TRUE)
    if (nchar(txt) < before_g13) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G13", severity = "FIXED",
        detail = "Auto-scrubbed CGL-actionability conflation language"
      )
    }

    # V-011 (G12): Auto-scrub Tier 0 treatment-recommendation conflation
    before_g12 <- nchar(txt)
    txt <- gsub("\\bTier 0\\b[^.]*?\\b(?:should receive|is indicated for|treatment recommendation|prescribe|recommend)\\b",
                "Tier 0 provides regulatory recognition only and does not constitute a treatment recommendation", txt, perl = TRUE, ignore.case = TRUE)
    if (nchar(txt) < before_g12) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G12", severity = "FIXED",
        detail = "Auto-scrubbed Tier 0 treatment-recommendation conflation"
      )
    }

    # V-008: Strip markdown asterisks around gene symbols (*GENE* → GENE, **GENE** → GENE)
    # The LLM sometimes wraps gene symbols in asterisks for emphasis, which is distracting
    # and violates the formatting governance (llm_glossary FORMATTING rule #1)
    before_v8 <- nchar(txt)
    txt <- gsub("\\*\\*([A-Z][A-Z0-9]{1,}[0-9]*)\\*\\*", "\\1", txt, perl = TRUE)
    txt <- gsub("\\*([A-Z][A-Z0-9]{1,}[0-9]*)\\*", "\\1", txt, perl = TRUE)
    # Also catch bold/italic wrap around gene+qualifier pairs like *GENE (Bulk mRNA Expression)*
    txt <- gsub("\\*\\*?([A-Z][A-Z0-9]{1,}[0-9]*\\s*\\([^)]+\\))\\*\\*?", "\\1", txt, perl = TRUE)
    if (nchar(txt) < before_v8) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G20", severity = "FIXED",
        detail = "Stripped markdown asterisks from gene symbols (e.g., *CHEK1* → CHEK1, **TP53** → TP53)"
      )
    }

    # V-009: Endpoint vocabulary enforcement — PFI prohibits "survival state"
    # The system prompt says use "stress-adapted persistence state" not "survival state" for PFI.
    # This hard-scrubs any leaking "survival state" when PFI metric is active.
    before_v9 <- nchar(txt)
    txt <- gsub("stress-adapted survival state", "stress-adapted persistence state", txt, perl = TRUE)
    txt <- gsub("adaptive survival state", "adaptive persistence state", txt, perl = TRUE)
    if (nchar(txt) < before_v9) {
      audit_actions[[length(audit_actions) + 1]] <- list(
        rule = "G21", severity = "FIXED",
        detail = "Replaced 'survival state' with 'persistence state' (PFI/DFI endpoint vocabulary governance)"
      )
    }

    # Collapse residual artifacts from token stripping and whitespace cleanup
    txt <- gsub("\\s{2,}", " ", txt, perl = TRUE)
    txt <- gsub("\\(\\s*(?:and|or)?\\s*\\)", "", txt, perl = TRUE)
    txt <- gsub(",\\s*,", ",", txt, perl = TRUE)
    txt <- gsub("\\s,(?=[a-zA-Z])", ", ", txt, perl = TRUE)
    txt <- trimws(txt)

    attr(txt, "audit_actions") <- audit_actions
    return(txt)
  }

  # ============================================================================
  # GOVERNANCE AUDIT REPORTER - Aggregates all post-processing actions
  # ============================================================================
  # Collects audit_actions from scrub_governance_violations(),
  # ensure_questions_and_disclaimer(), check_ecological_fallacy(),
  # and validate_llm_factuality() into a unified HTML audit badge.
  build_governance_audit_badge <- function(audit_actions_list, eco_count = 0, fact_issues = 0) {
    all_actions <- do.call(c, audit_actions_list)
    n_actions <- length(all_actions)

    if (n_actions == 0 && eco_count == 0 && fact_issues == 0) {
      return(HTML('<div style="background:#065f46; color:#6ee7b7; padding:6px 12px; border-radius:4px; font-size:0.85rem; margin-top:12px;">
        ✅ Governance Audit: No violations detected. LLM output passed all automated checks.
      </div>'))
    }

    n_fixed <- sum(sapply(all_actions, function(a) a$severity == "FIXED"))
    n_warn  <- sum(sapply(all_actions, function(a) a$severity == "WARNING"))
    n_flag  <- sum(sapply(all_actions, function(a) a$severity == "FLAGGED"))

    html <- paste0('<div style="background:#1e293b; border:1px solid #fbbf24; border-radius:6px; padding:10px 14px; margin-top:12px; font-size:0.85rem;">',
      '<details><summary style="color:#fbbf24; cursor:pointer; font-weight:bold;">',
      '⚠️ Governance Audit: ', n_actions, ' automated correction(s) applied',
      if (eco_count > 0) paste0(' + ', eco_count, ' ecological-fallacy flag(s)') else '',
      if (fact_issues > 0) paste0(' + ', fact_issues, ' factuality issue(s)') else '',
      ' (click to expand)</summary>',
      '<div style="margin-top:8px; color:#cbd5e1; line-height:1.6;">',
      '<p style="color:#94a3b8; margin-bottom:8px;">The following governance rules were violated by the LLM and automatically corrected before display. These corrections ensure compliance with the CancerRCDPredictor Governance Framework.</p>')

    if (n_fixed > 0) {
      html <- paste0(html, '<p style="color:#6ee7b7; margin-bottom:4px;"><strong>✅ Automatically Fixed (', n_fixed, '):</strong></p><ul style="margin-top:0;">')
      for (a in all_actions) {
        if (a$severity == "FIXED") {
          html <- paste0(html, '<li><span style="color:#fbbf24;">[', a$rule, ']</span> ', a$detail, '</li>')
        }
      }
      html <- paste0(html, '</ul>')
    }

    if (n_warn > 0) {
      html <- paste0(html, '<p style="color:#fbbf24; margin-bottom:4px;"><strong>⚠️ Warnings — Review Recommended (', n_warn, '):</strong></p><ul style="margin-top:0;">')
      for (a in all_actions) {
        if (a$severity == "WARNING") {
          html <- paste0(html, '<li><span style="color:#fbbf24;">[', a$rule, ']</span> ', a$detail, '</li>')
        }
      }
      html <- paste0(html, '</ul>')
    }

    if (n_flag > 0) {
      html <- paste0(html, '<p style="color:#f87171; margin-bottom:4px;"><strong>🚩 Flagged — Needs Attention (', n_flag, '):</strong></p><ul style="margin-top:0;">')
      for (a in all_actions) {
        if (a$severity == "FLAGGED") {
          html <- paste0(html, '<li><span style="color:#fbbf24;">[', a$rule, ']</span> ', a$detail, '</li>')
        }
      }
      html <- paste0(html, '</ul>')
    }

    if (eco_count > 0) {
      html <- paste0(html, '<p style="color:#f87171; margin-bottom:4px;"><strong>🚩 Ecological Fallacy Risk (', eco_count, ' paragraph(s)):</strong></p>',
        '<p style="color:#94a3b8;">Population-level correlation signs were detected in patient-specific paragraphs. The flagged paragraphs have been annotated with ⚠️ markers in the report above. Review these sections to verify that population-level signs were not used to infer patient-level phenotype status.</p>')
    }

    if (fact_issues > 0) {
      html <- paste0(html, '<p style="color:#f87171; margin-bottom:4px;"><strong>🚩 Factuality Check (', fact_issues, ' issue(s)):</strong></p>',
        '<p style="color:#94a3b8;">Potential factual inconsistencies were detected between the LLM output and the input payload data. These have been logged to factuality_audit_log.csv for review.</p>')
    }

    html <- paste0(html, '</div></details></div>')
    return(HTML(html))
  }
  # ==============================================================================
  # LLM POST-PROCESSING GUARD (ECOLOGICAL FALLACY DETECTOR - CRIT-05)
  # ==============================================================================
  # Scans LLM output for population-level correlation sign imports (P-positive,
  # N-negative, population-level, cohort-level) appearing in paragraphs that also
  # contain patient-specific markers. Logs violations to ecological_fallacy_audit_log.csv.
  # V4: Population guardrail removed - ecological fallacy check disabled per governance review.
  # Function retained as no-op for backward compatibility with all call sites.
  check_ecological_fallacy <- function(txt, patient_id, module) {
    list(annotated_text = txt, violation_count = 0)
  }
  # ==============================================================================
  # LLM POST-PROCESSING GUARD (FACTUALITY VALIDATOR - CRIT-02)
  # ==============================================================================
  # Cross-checks LLM output against ground-truth input data:
  #   (1) Gene symbols in output vs genes in patient profile
  #   (2) RCD pathway attributions vs Table S11 annotations
  #   (3) TSM/TMB/MSI classifications vs stemness_df values
  # Logs mismatches to factuality_audit_log.csv.
  validate_llm_factuality <- function(txt, patient_id, module, profile_text = NULL, sig_noms = NULL) {
    issues <- list()

    # --- Build known gene symbol list from NCBI gene_info (loaded as gene_roles) ---
    known_genes <- if (!is.null(gene_roles) && "Gene.symbol" %in% names(gene_roles)) {
      unique(gene_roles$Gene.symbol)
    } else character(0)

    # --- Extract gene symbols from LLM output ---
    # Matches standard human gene symbols: uppercase alphanumeric, 2+ chars
    gene_regex <- "\\b([A-Z][A-Z0-9]{1,}[0-9]*)\\b"
    llm_genes_raw <- unique(unlist(regmatches(txt, gregexpr(gene_regex, txt, perl = TRUE))))
    # Filter to known genes only (removes acronyms, non-gene ALLCAPS)
    llm_genes <- intersect(llm_genes_raw, known_genes)

    # --- Extract ground-truth genes and gene→RCD mapping from profile_text ---
    ground_truth_genes <- character(0)
    gene_rcd_map <- list()
    if (!is.null(profile_text) && safe_nchar(profile_text) > 0L) {
      # Extract gene→RCD pairs from profile_text. Handles pipe-delimited multi-gene entries
      # (e.g., "Encoded Gene Mechanics: GENE1 (info) | GENE2 (info) | GENE3 (info)")
      # and both orderings: "RCD Form: X > Encoded Gene Mechanics: GENE" or vice versa.
      gt_blocks <- unlist(regmatches(profile_text,
        gregexpr("RCD Form: [A-Za-z/]+[\\s\\S]*?Encoded Gene Mechanics: [^\n]+", profile_text, perl = TRUE)))
      for (block in gt_blocks) {
        rcd <- NA_character_
        rm <- regmatches(block, regexpr("RCD Form: ([A-Za-z/]+)", block, perl = TRUE))
        if (length(rm) > 0) rcd <- tolower(sub("RCD Form: ", "", rm))
        
        gm_line <- regmatches(block, regexpr("Gene Mechanics: [^\n]+", block, perl = TRUE))
        if (length(gm_line) > 0) {
          gm_text <- sub("Gene Mechanics: ", "", gm_line)
          # Split on pipe delimiter to get individual gene entries
          gene_entries <- trimws(unlist(strsplit(gm_text, "\\|")))
          for (entry in gene_entries) {
            # Extract the gene symbol (first alphanumeric word before any paren/space)
            gene <- regmatches(entry, regexpr("[A-Za-z][A-Za-z0-9-]+", entry))
            if (length(gene) > 0 && gene != "Unknown") {
              ground_truth_genes <- c(ground_truth_genes, gene)
              if (!is.na(rcd)) gene_rcd_map[[gene]] <- rcd
            }
          }
        }
      }
    }

    # --- SUPPLEMENT: Extract ALL genes from full signature decoded payloads via Table_S11 ---
    # Each SHAP signature decodes to 125-235 gene elements; the profile text shows only
    # a subset. This ensures ground truth covers the complete signature membership.
    if (!is.null(table_s11_global) && nrow(table_s11_global) > 0 && 
        !is.null(profile_text) && safe_nchar(profile_text) > 0L) {
      # Extract nomenclature codes: use explicitly provided sig_noms if available, else parse from profile_text
      if (is.null(sig_noms) || length(sig_noms) == 0) {
        sig_lines <- regmatches(profile_text, gregexpr("Signature: [^\n]+", profile_text))[[1]]
        sig_noms <- unique(trimws(gsub("^Signature: ", "", sig_lines)))
      }
      if (length(sig_noms) > 0) {
        # Build regex to match catalog nomenclatures that END with the stripped pattern
        for (sn in sig_noms) {
          # The profile strips the cancer prefix; match any Table_S11 nomenclature ending with sn
          matches <- table_s11_global[grepl(paste0(gsub("\\.", "\\\\.", sn), "$"), 
                                            table_s11_global$Nomenclature), ]
          if (nrow(matches) > 0) {
            decoded <- as.character(matches$Decoded.Genetic.Element[1])
            if (!is.na(decoded) && nchar(decoded) > 0) {
              # Use same extraction logic as benchmark: strip isoform fractions, split on +
              clean <- gsub("\\(\\d+/\\d+\\)", "", decoded)
              clean <- gsub("`", "", clean)
              parts <- trimws(unlist(strsplit(clean, "\\+")))
              parts <- gsub("^\\(|\\)$", "", parts)
              sig_genes <- parts[grepl("^[A-Z][A-Za-z0-9-]+$", parts) & parts != "Unknown"]
              ground_truth_genes <- c(ground_truth_genes, sig_genes)
            }
          }
        }
      }
    }
    ground_truth_genes <- unique(ground_truth_genes)

    # --- (1) Gene reference check (V17.1: NCBI-gated — only flag truly unknown genes) ---
    # Genes in NCBI reference (known_genes) are legitimate biological context, not fabrications.
    # Only genes absent from BOTH the signature payload AND NCBI are flagged.
    if (length(ground_truth_genes) > 0) {
      external_genes <- setdiff(llm_genes, ground_truth_genes)
      # Filter: only flag genes not in NCBI reference (true unknowns)
      truly_external <- setdiff(external_genes, known_genes)
      if (length(truly_external) > 0) {
        for (g in truly_external) {
          issues[[length(issues) + 1]] <- list(
            type = "GENE_OUTSIDE_PROFILE",
            detail = paste0("Gene ", g, " mentioned but not found in patient signature gene set or NCBI reference"),
            gene = g
          )
        }
      }
    }

    # --- (2) RCD pathway mismatch check ---
    rcd_terms <- c("ferroptosis", "apoptosis", "autophagy", "necrosis", "anoikis", "pyroptosis")
    llm_genes_in_profile <- intersect(llm_genes, ground_truth_genes)
    for (g in llm_genes_in_profile) {
      expected <- gene_rcd_map[[g]]
      if (is.null(expected) || expected == "") next
      # Find sentences mentioning this gene
      sentences <- unlist(strsplit(txt, "[.!?]+"))
      gene_sents <- sentences[grepl(g, sentences, ignore.case = TRUE)]
      for (rcd in rcd_terms) {
        if (any(grepl(rcd, gene_sents, ignore.case = TRUE))) {
          # Check if gene is also correctly associated with its expected RCD
          has_correct <- any(grepl(expected, gene_sents, ignore.case = TRUE))
          # Check if the mentioned RCD contradicts expected (e.g., gene annotated
          # as Apoptosis but LLM says it's associated with Ferroptosis)
          if (!grepl(expected, rcd) && !has_correct) {
            issues[[length(issues) + 1]] <- list(
              type = "RCD_MISMATCH",
              detail = paste0("Gene ", g, " associated with ", rcd,
                             " in output, but profile annotates it as ", expected),
              gene = g
            )
          }
        }
      }
    }

    # --- (3) TSM/TMB/MSI classification check (V22 ENHANCED) ---
    if (!is.null(stemness_df) && nrow(stemness_df) > 0) {
      pheno <- stemness_df[stemness_df$sample_id == patient_id, ]
      if (nrow(pheno) > 0) {
        # Build ground-truth classification map for ALL available phenotype variables
        gt_pheno <- list()
        for (col in c("RNAss_class", "EREG_class",
                      "TMB_class", "MSI_class")) {
          if (col %in% names(pheno)) {
            val <- tolower(as.character(pheno[[col]][1]))
            if (!is.na(val) && val != "" && val != "unknown") {
              gt_pheno[[col]] <- val
            }
          }
        }

        # --- (3a) Phantom TSM variable detection ---
        # Detect when LLM fabricates TSM variables NOT in the payload
        # e.g., EREG.METHss, DNAss, DMPss when only RNAss and EREG.EXPss are present
        all_tsm_vars <- c("RNAss", "EREG.EXPss")
        gt_tsm_vars <- unique(gsub("_class$", "", names(gt_pheno)[grepl("_(class|EXPss|METHss)$", names(gt_pheno))]))
        # V26 FIX: EREG_class column strips to "EREG" but the actual TSM variable is "EREG.EXPss"
        gt_tsm_vars[gt_tsm_vars == "EREG"] <- "EREG.EXPss"
        # Also check profile_text for explicitly stated TSM variables
        if (!is.null(profile_text) && safe_nchar(profile_text) > 0L) {
          stated_vars <- regmatches(profile_text, gregexpr(
            "(RNAss|EREG\\.EXPss)", profile_text, perl = TRUE))[[1]]
          gt_tsm_vars <- unique(c(gt_tsm_vars, stated_vars))
        }
        for (tsm_var in all_tsm_vars) {
          if (!tsm_var %in% gt_tsm_vars) {
            # This TSM variable is NOT in the payload — check if LLM mentions it
            if (grepl(tsm_var, txt, perl = TRUE, ignore.case = TRUE)) {
              issues[[length(issues) + 1]] <- list(
                type = "PHANTOM_TSM_VARIABLE",
                detail = paste0("Output mentions ", tsm_var, " which is NOT in patient payload (payload only has: ",
                               paste(gt_tsm_vars, collapse = ", "), ")"),
                gene = ""
              )
            }
          }
        }

        # --- (3b) TSM/TMB/MSI classification contradiction check (ENHANCED) ---
        # Scan ALL TSM classification mentions in output, not just those near "the patient"
        # Pattern: captures "RNAss = Low", "EREG.EXPss = Intermediate", "RNAss: High", etc.
        abbrev_map <- c(
          "RNAss_class" = "RNAss", "EREG_class" = "EREG\\.EXPss",
          "TMB_class" = "TMB", "MSI_class" = "MSI"
        )
        class_levels <- c("high", "low", "intermediate")
        for (gcol in names(gt_pheno)) {
          expected_class <- gt_pheno[[gcol]]
          abbrev <- abbrev_map[[gcol]]
          if (is.null(abbrev)) next
          # For each OTHER classification level, check if LLM contradicts
          for (other_level in setdiff(class_levels, expected_class)) {
            # Patterns: "RNAss = Low", "RNAss: Low", "RNAss (Low)", "RNAss is Low"
            # Also catch inverted: "Low RNAss"
            pat1 <- paste0(abbrev, "\\s*[=:]\\s*", other_level)
            pat2 <- paste0(abbrev, "\\s*\\(\\s*", other_level, "\\s*\\)")
            pat3 <- paste0(abbrev, "\\s+is\\s+", other_level)
            pat4 <- paste0(abbrev, "\\s*:\\s*", other_level)
            pat5 <- paste0(other_level, "\\s+", abbrev)
            if (grepl(pat1, tolower(txt), perl = TRUE) ||
                grepl(pat2, tolower(txt), perl = TRUE) ||
                grepl(pat3, tolower(txt), perl = TRUE) ||
                grepl(pat4, tolower(txt), perl = TRUE) ||
                grepl(pat5, tolower(txt), perl = TRUE)) {
              # Verify it's not just describing a different context (e.g., population reference)
              # Only flag if it's clearly a patient-level attribution
              # Also avoid flagging when expected==other_level (shouldn't happen but defensive)
              clean_name <- gsub("\\\\.", ".", abbrev)
              issues[[length(issues) + 1]] <- list(
                type = "PHENOTYPE_MISMATCH",
                detail = paste0("Output claims ", clean_name, " is '", other_level,
                               "' but patient ground truth is '", expected_class, "'"),
                gene = ""
              )
              break  # One violation per phenotype variable is enough
            }
          }
        }
      }
    }

    # --- Defensive: exit early if profile_text is not usable (prevents if(NA) crash) ---
    if (!is.character(profile_text) || is.na(profile_text) || safe_nchar(profile_text) == 0L) {
      return(list(issues = issues, factuality_score = 1))
    }

    # --- (4) SHAP Value Attribution Check (V22 NEW) ---
    # Extract ground-truth gene→SHAP mapping from profile_text and verify
    # that the LLM output correctly attributes SHAP values to their genes.
    # This catches errors like "TNFRSF10B has negative SHAP (-0.7368)" when
    # -0.7368 actually belongs to POLA2 and TNFRSF10B has +0.6135.
    if (safe_nchar(profile_text) > 0L) {
      gt_gene_shap <- list()
      # Extract per-signature blocks: SHAP Impact line + Encoded Gene Mechanics line
      shap_blocks <- unlist(regmatches(profile_text,
        gregexpr("Signature:[\\s\\S]*?Encoded Gene Mechanics: [^\n]+", profile_text, perl = TRUE)))
      for (block in shap_blocks) {
        # Extract SHAP value and sign
        shap_val_match <- regmatches(block, regexpr("Value:\\s*([-]?[0-9]+\\.?[0-9]*)", block, perl = TRUE))
        shap_sign_match <- regmatches(block, regexpr("SHAP Impact:\\s*(Positive|Negative)", block, perl = TRUE))
        if (length(shap_val_match) == 0) next
        shap_val <- as.numeric(sub("Value:\\s*", "", shap_val_match))
        shap_sign <- if (length(shap_sign_match) > 0) {
          ifelse(grepl("Positive", shap_sign_match), "positive", "negative")
        } else {
          ifelse(shap_val > 0, "positive", "negative")
        }
        # Extract gene symbol(s) from Encoded Gene Mechanics line
        gm_line <- regmatches(block, regexpr("Gene Mechanics: [^\n]+", block, perl = TRUE))
        if (length(gm_line) > 0) {
          gm_text <- sub("Gene Mechanics: ", "", gm_line)
          gene_entries <- trimws(unlist(strsplit(gm_text, "\\|")))
          for (entry in gene_entries) {
            gene <- regmatches(entry, regexpr("[A-Za-z][A-Za-z0-9-]+", entry))
            if (length(gene) > 0 && gene != "Unknown") {
              gt_gene_shap[[gene]] <- list(value = shap_val, sign = shap_sign)
            }
          }
        }
      }

      # Now scan LLM output for gene→SHAP attributions and verify
      if (length(gt_gene_shap) > 0) {
        txt_lower <- tolower(txt)
        # For each gene in ground truth, find SHAP value mentions near it
        for (g in names(gt_gene_shap)) {
          gt_info <- gt_gene_shap[[g]]
          # Find sentences mentioning this gene
          sentences <- unlist(strsplit(txt, "[.!?]+"))
          gene_sents <- sentences[grepl(paste0("\\b", g, "\\b"), sentences, perl = TRUE)]
          if (length(gene_sents) == 0) next

          for (sent in gene_sents) {
            sent_lower <- tolower(sent)
            # Extract SHAP values mentioned in this sentence
            sent_shap_vals <- regmatches(sent_lower, gregexpr(
              "shap\\s*(?:value|impact)?\\s*(?:of|is|:)?\\s*([-]?[0-9]+\\.?[0-9]*)", sent_lower, perl = TRUE))[[1]]
            # Also match parenthetical patterns like "(-0.7648)"
            sent_shap_vals2 <- regmatches(sent_lower, gregexpr(
              "\\(\\s*([-]?[0-9]+\\.?[0-9]*)\\s*\\)", sent_lower, perl = TRUE))[[1]]
            all_vals <- c(
              as.numeric(gsub("[^0-9.-]", "", sent_shap_vals)),
              as.numeric(gsub("[^0-9.-]", "", sent_shap_vals2))
            )
            all_vals <- all_vals[!is.na(all_vals)]

            # Check if any SHAP value in this sentence matches a DIFFERENT gene's value
            for (other_g in names(gt_gene_shap)) {
              if (other_g == g) next
              other_val <- gt_gene_shap[[other_g]]$value
              for (sv in all_vals) {
                if (abs(sv - other_val) < 0.001) {
                  # The LLM attributed another gene's SHAP value to this gene!
                  issues[[length(issues) + 1]] <- list(
                    type = "SHAP_VALUE_MISATTRIBUTION",
                    detail = paste0("SHAP value ", other_val, " (belongs to ", other_g,
                                   ") incorrectly attributed to ", g),
                    gene = g
                  )
                  break
                }
              }
            }

            # Also check SHAP sign if explicitly stated
            if (grepl("positive shap", sent_lower) && gt_info$sign == "negative") {
              issues[[length(issues) + 1]] <- list(
                type = "SHAP_SIGN_ERROR",
                detail = paste0("Gene ", g, " described as positive SHAP but ground truth is negative (",
                               gt_info$value, ")"),
                gene = g
              )
            }
            if (grepl("negative shap", sent_lower) && gt_info$sign == "positive") {
              issues[[length(issues) + 1]] <- list(
                type = "SHAP_SIGN_ERROR",
                detail = paste0("Gene ", g, " described as negative SHAP but ground truth is positive (",
                               gt_info$value, ")"),
                gene = g
              )
            }
          }
        }
      }
    }

    # --- (5) G18 RCD Boundary Check (V22 NEW) ---
    # Enforce the CRITICAL RCD BOUNDARY rule: LLM must NOT discuss RCD forms
    # NOT present in the patient's Associated RCD Form annotations.
    # Matches CRIT-03 G18 governance check but operates in the live app.
    if (!is.null(gene_rcd_map) && length(gene_rcd_map) > 0) {
      # Normalize ground-truth RCD forms (split compound annotations like "Apoptosis/Autophagy")
      gt_rcd_raw <- unique(tolower(trimws(unlist(strsplit(unlist(gene_rcd_map), "/")))))
      gt_rcd <- gt_rcd_raw[gt_rcd_raw != "unknown" & gt_rcd_raw != ""]

      # All 25 RCD forms from the decoder — universe of forms the LLM may name
      all_rcd_forms <- c(
        "apoptosis", "necroptosis", "pyroptosis", "ferroptosis",
        "autophagy", "necrosis", "anoikis", "cellular senescence",
        "mitotic catastrophe", "cuproptosis", "netosis", "efferocytosis",
        "entosis", "parthanatos", "immunogenic cell death", "disulfidptosis",
        "oxeiptosis", "paraptosis", "alkaliptosis",
        "lysosome-dependent cell death", "mitoptosis", "autosis",
        "erebosis", "methuosis", "mitochondrial permeability transition"
      )

      resp_lower <- tolower(txt)
      mentioned_rcd <- all_rcd_forms[sapply(all_rcd_forms,
        function(r) grepl(r, resp_lower, fixed = TRUE))]

      extraneous_rcd <- setdiff(mentioned_rcd, gt_rcd)
      if (length(extraneous_rcd) > 0) {
        for (er in extraneous_rcd) {
          # Only flag if it appears in a context suggesting the LLM is discussing
          # it as part of the patient's biology (not just in the RCD decoder itself
          # or in a general educational context).
          # Check if the RCD term appears near gene symbols or patient references.
          issues[[length(issues) + 1]] <- list(
            type = "RCD_BOUNDARY_VIOLATION",
            detail = paste0("Discussed '", er, "' which is NOT in patient signature RCD annotations [patient RCD: ",
                           paste(gt_rcd, collapse = ", "), "]"),
            gene = ""
          )
        }
      }
    }

    # --- (6) G9b Forbidden Canonical Gene Check (V22 NEW) ---
    # The LLM is explicitly instructed NOT to import canonical cancer genes
    # (EGFR, TP53, TNF, BRAF, BRCA1, GPX4, CDK2, SMAD2, FGFR3, ATM, APC)
    # unless they appear in the driver gene list or are biologically connected.
    # This check flags any forbidden gene that appears WITHOUT a driver connection.
    forbidden_canonical <- c("TP53", "EGFR", "TNF", "BRAF", "BRCA1", "GPX4", "CDK2",
                             "SMAD2", "FGFR3", "ATM", "APC", "TRAF2", "E2F1", "RHOA", "HSPD1")
    if (length(ground_truth_genes) > 0) {
      forbidden_found <- intersect(llm_genes, forbidden_canonical)
      if (length(forbidden_found) > 0) {
        for (fg in forbidden_found) {
          # Check if the gene is directly biologically connected to a driver gene
          # by looking for co-mention in the same sentence
          sentences <- unlist(strsplit(txt, "[.!?]+"))
          gene_sents <- sentences[grepl(paste0("\\b", fg, "\\b"), sentences, perl = TRUE)]
          has_driver_connection <- FALSE
          for (sent in gene_sents) {
            for (dg in ground_truth_genes) {
              if (grepl(paste0("\\b", dg, "\\b"), sent, perl = TRUE)) {
                # Check for connection language: "via", "through", "downstream of",
                # "upstream of", "interacts with", "binds", "phosphorylates", etc.
                if (grepl("(via|through|downstream|upstream|interact|bind|phosphorylat|regulat|signal|pathway|complex|substrate|target)",
                         sent, perl = TRUE, ignore.case = TRUE)) {
                  has_driver_connection <- TRUE
                  break
                }
              }
            }
            if (has_driver_connection) break
          }
          if (!has_driver_connection) {
            issues[[length(issues) + 1]] <- list(
              type = "FORBIDDEN_GENE_IMPORT",
              detail = paste0("Canonical gene ", fg, " imported without documented connection to any driver gene (",
                             paste(intersect(names(gt_gene_shap), ground_truth_genes), collapse = ", "), ")"),
              gene = fg
            )
          }
        }
      }
    }

    # --- Log issues ---
    if (length(issues) > 0) {
      log_file <- "factuality_audit_log.csv"
      for (iss in issues) {
        new_row <- data.frame(
          Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          Patient_ID = patient_id,
          Module = module,
          Issue_Type = iss$type,
          Detail = iss$detail,
          stringsAsFactors = FALSE
        )
        if (!file.exists(log_file)) write.csv(new_row, log_file, row.names = FALSE)
        else write.table(new_row, log_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
      }
    }

    factuality_score <- if (length(llm_genes) > 0) {
      max(0, 1 - (length(issues) / max(1, length(llm_genes))))
    } else 1

    return(list(issues = issues, factuality_score = factuality_score))
  }
  # ==============================================================================
  # ==============================================================================
  # 1. GLOBAL DATA LOADING
  # ==============================================================================
  # ----- Portable ZIMA_ROOT detection -----
  resolve_zima_root <- function() {
    sig_file <- "Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv"
    nixos_path <- "/mnt/sharefiles/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
    check_readable <- function(p) file.exists(file.path(p, sig_file)) && file.access(file.path(p, sig_file), 4) == 0
    if (dir.exists(nixos_path) && check_readable(nixos_path)) return(nixos_path)
    if (check_readable("..")) return(normalizePath(".."))
    if (file.exists(sig_file) && file.access(sig_file, 4) == 0) return(normalizePath("."))
    parent_candidate <- dirname(getwd())
    if (check_readable(parent_candidate)) return(parent_candidate)
    if (check_readable("../..")) return(normalizePath("../.."))
    return(normalizePath("."))
  }
  ZIMA_ROOT <- resolve_zima_root()
  message("[ZIMA] Root resolved to: ", ZIMA_ROOT)
  zima_drive_path <- file.path(ZIMA_ROOT, "PHASE_III_ML_Models")
  LLM_LOCK_PATH    <- file.path(ZIMA_ROOT, ".llm_queue.lock")

  # Virtual tunnels to static assets (required for NixOS service)
  addResourcePath("www", file.path(getwd(), "www"))
  if (dir.exists("Multimedia")) {
    addResourcePath("media", "Multimedia")
  } else if (dir.exists(file.path("..", "Multimedia"))) {
    addResourcePath("media", file.path("..", "Multimedia"))
  }
  addResourcePath("zima_models", zima_drive_path)

  # ==============================================================================
  # LLM BACKEND CONFIGURATION (Ollama local <-> DeepSeek API)
  # ==============================================================================
  if (file.exists(".Renviron")) {
    tryCatch(readRenviron(".Renviron"), error = function(e) NULL)
  }

  llm_config <- shiny::reactiveValues(
    backend          = Sys.getenv("LLM_BACKEND", unset = "deepseek"),
    ollama_url       = Sys.getenv("OLLAMA_URL", unset = "http://localhost:11434"),
    ollama_model     = Sys.getenv("OLLAMA_MODEL", unset = "qwen3:8b"),
    deepseek_api_key = Sys.getenv("DEEPSEEK_API_KEY", unset = ""),
    deepseek_model   = Sys.getenv("DEEPSEEK_MODEL", unset = "deepseek-chat")
  )

  send_llm_request <- function(messages, backend = NULL, api_key = NULL, model = NULL, ollama_url = NULL) {
    if (is.null(backend)) backend <- shiny::isolate(llm_config$backend)

    if (backend == "deepseek") {
      key <- if (!is.null(api_key)) api_key else shiny::isolate(llm_config$deepseek_api_key)
      if (is.null(key) || identical(key, "") || identical(key, NA_character_)) stop("DeepSeek API key not set.")
      mdl <- if (!is.null(model)) model else shiny::isolate(llm_config$deepseek_model)
      req <- httr2::request("https://api.deepseek.com/v1/chat/completions") |>
        httr2::req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", key)) |>
        httr2::req_body_json(list(model = mdl, stream = FALSE, messages = messages, max_tokens = 4096, temperature = 0)) |>
        httr2::req_timeout(120)
    } else {
      # ---- OLLAMA QUEUE (filelock - hardware-level safety net) ----
      message("[LLM-QUEUE] Tentando adquirir lock...")
      t_lock <- Sys.time()
      lck <- filelock::lock(LLM_LOCK_PATH, timeout = 600000)
      message("[LLM-QUEUE] Lock adquirido (esperou ", round(difftime(Sys.time(), t_lock, units="s"), 1), "s)")
      on.exit({
        filelock::unlock(lck)
        message("[LLM-QUEUE] Lock liberado")
      }, add = TRUE)
      # ----
      base_url <- if (!is.null(ollama_url)) ollama_url else shiny::isolate(llm_config$ollama_url)
      mdl <- if (!is.null(model)) model else shiny::isolate(llm_config$ollama_model)
      req <- httr2::request(paste0(base_url, "/api/chat")) |>
        httr2::req_headers("Content-Type" = "application/json") |>
        httr2::req_body_json(list(model = mdl, stream = FALSE, messages = messages, options = list(num_ctx = 16384, temperature = 0, num_predict = 4096), keep_alive = "30m")) |>
        httr2::req_timeout(900)
    }
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        # Capture HTTP response body from httr2 errors for diagnostics
        err_body <- ""
        if (inherits(e, "httr2_http_400")) {
          err_body <- tryCatch(httr2::resp_body_string(e), error = function(ignore) "<unreadable>")
        } else if (inherits(e, "httr2_http")) {
          err_body <- tryCatch(httr2::resp_body_string(e), error = function(ignore) "<unreadable>")
        }
        # Estimate total prompt tokens for diagnostics
        total_chars <- sum(nchar(unlist(lapply(messages, function(m) m$content))))
        est_tokens <- round(total_chars / 4)
        msg <- paste0(e$message,
          if (safe_nchar(err_body) > 0L) paste0(" [API Response: ", substr(err_body, 1, 300), "]") else "",
          " [Prompt ~", est_tokens, " tokens | ", total_chars, " chars]")
        stop(msg)
      }
    )
    parsed <- httr2::resp_body_json(resp)
    raw <- if (backend == "deepseek") parsed$choices[[1]]$message$content else parsed$message$content
    # Defensive: ensure we always return a plain character string, never NULL or list
    if (is.null(raw)) stop("LLM returned empty content (NULL). API response structure may have changed.")
    if (!is.character(raw)) raw <- as.character(raw)
    raw <- sanitize_llm_output(raw)
    return(raw)
  }

  check_llm_status <- function() {
    backend <- shiny::isolate(llm_config$backend)
    if (backend == "deepseek") {
      key <- shiny::isolate(llm_config$deepseek_api_key)
      if (is.null(key) || identical(key, "") || identical(key, NA_character_)) return(list(success = FALSE, message = "DeepSeek API key not configured."))
      return(list(success = TRUE, message = "DeepSeek API ready."))
    }
    base_url <- shiny::isolate(llm_config$ollama_url)
    mdl_name <- shiny::isolate(llm_config$ollama_model)
    available <- c()
    tryCatch({
      req <- httr2::request(paste0(base_url, "/api/tags")) |> httr2::req_timeout(3)
      resp <- httr2::req_perform(req)
      j <- httr2::resp_body_json(resp)
      if (!is.null(j$models)) available <- sapply(j$models, function(m) m$name)
    }, error = function(e) {
      return(list(success = FALSE, message = paste0("Ollama at ", base_url, " unreachable."), selected_model = mdl_name))
    })
    if (mdl_name %in% available) return(list(success = TRUE, message = "Ollama ready.", selected_model = mdl_name))
    base <- sub(":.*$", "", mdl_name)
    hits <- available[sub(":.*$", "", available) == base]
    if (length(hits) > 0) return(list(success = TRUE, message = paste0("Using ", hits[1]), selected_model = hits[1]))
    if (length(available) > 0) return(list(success = TRUE, message = paste0("Using ", available[1]), selected_model = available[1]))
    return(list(success = FALSE, message = paste0("No models. Run: ollama pull ", mdl_name), selected_model = mdl_name))
  }

  # ==============================================================================
  # ASYNC LLM HELPERS — capture config as plain values for use in future() workers
  # shiny::reactiveValues are NOT accessible inside future::future() background
  # processes, so we snapshot config before dispatching.
  # ==============================================================================

  capture_llm_config <- function() {
    b <- shiny::isolate(llm_config$backend)
    if (b == "deepseek") {
      list(
        backend  = "deepseek",
        api_key  = shiny::isolate(llm_config$deepseek_api_key),
        model    = shiny::isolate(llm_config$deepseek_model),
        ollama_url = NULL
      )
    } else {
      list(
        backend  = "ollama",
        api_key  = NULL,
        model    = shiny::isolate(llm_config$ollama_model),
        ollama_url = shiny::isolate(llm_config$ollama_url)
      )
    }
  }

  # Run a single send_llm_request in a background future() and return a promise.
  # This is the core non-blocking primitive.  All LLM observers build their
  # prompts synchronously (fast, sub-second) then dispatch the HTTP I/O here.
  llm_future_promise <- function(messages, cfg) {
    future::future({
      send_llm_request(
        messages   = messages,
        backend    = cfg$backend,
        api_key    = cfg$api_key,
        model      = cfg$model,
        ollama_url = cfg$ollama_url
      )
    }, seed = TRUE)
  }


  # Load the 96 verified cohort matrices to power the dynamic selector engines (Locally Copied)
  raw_cohort_matrix_path <- file.path(ZIMA_ROOT, "Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv")
  if(!file.exists(raw_cohort_matrix_path) && file.exists("Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv")) {
    raw_cohort_matrix_path <- "Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv"
  }
  raw_cohort_matrix <- read.csv(raw_cohort_matrix_path, stringsAsFactors = FALSE)

  # Ensure CTAB and Metric columns exist for high-speed indexing
  if(!"CTAB" %in% names(raw_cohort_matrix)) {
      raw_cohort_matrix$CTAB <- sapply(strsplit(raw_cohort_matrix$Cohort, "_"), `[`, 1)
  }
  if(!"Metric" %in% names(raw_cohort_matrix)) {
      raw_cohort_matrix$Metric <- sapply(strsplit(raw_cohort_matrix$Cohort, "_"), `[`, 2)
  }

  # Load the Pharmacogenomic Matrix
  drug_matrix_path <- "Unified_Drug_Matrix.rds"
  if (file.exists(drug_matrix_path)) {
      unified_drug_matrix <- tryCatch(
        as.data.frame(readRDS(drug_matrix_path), stringsAsFactors = FALSE),
        error = function(e) {
          message("[WARN] Unified_Drug_Matrix.rds corrupt or unreadable; pharmacogenomic features disabled.")
          data.frame(Gene_Symbol=character(), Drug_Name=character(), Interaction_Type=character(), Clinical_Status=character(), Source_Database=character(), stringsAsFactors=FALSE)
        }
      )
  } else {
      unified_drug_matrix <- data.frame(Gene_Symbol=character(), Drug_Name=character(), Interaction_Type=character(), Clinical_Status=character(), Source_Database=character(), stringsAsFactors=FALSE)
  }

  # Load OncoKB Gene Annotations
  oncokb_gene_path <- "OncoKB_Gene_Annotations.rds"
  if (file.exists(oncokb_gene_path)) {
      oncokb_gene_annotations <- tryCatch(
        as.data.frame(readRDS(oncokb_gene_path), stringsAsFactors = FALSE),
        error = function(e) {
          message("[WARN] OncoKB_Gene_Annotations.rds unreadable.")
          data.frame(Gene_Symbol=character(), Oncogenic_Class=character(), Gene_Summary=character(), stringsAsFactors=FALSE)
        }
      )
  } else {
      oncokb_gene_annotations <- data.frame(Gene_Symbol=character(), Oncogenic_Class=character(), Gene_Summary=character(), stringsAsFactors=FALSE)
  }

# Load OncoKB Cancer Gene List (v99, May-2026) as orthogonal cancer-gene evidence layer
# Provides: Hugo Symbol, OncoKB Annotated, MSK-IMPACT, MSK-HEME, FoundationOne,
#          FoundationOne Heme, Vogelstein, COSMIC CGC (v99), Gene Type, Gene Aliases
oncokb_cgl_path <- "oncoKB_cancerGeneList.tsv"
if (file.exists(oncokb_cgl_path)) {
    oncokb_cancer_gene_list <- tryCatch(
        read.delim(oncokb_cgl_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) {
            message("[WARN] oncoKB_cancerGeneList.tsv unreadable: ", e$message)
            data.frame()
        }
    )
    message(sprintf("[INFO] Loaded OncoKB Cancer Gene List: %d genes", nrow(oncokb_cancer_gene_list)))
} else {
    message("[WARN] oncoKB_cancerGeneList.tsv not found.")
    oncokb_cancer_gene_list <- data.frame()
}

# Load OncoKB FDA-Recognized Biomarker-Drug Associations (Level 2)
# Fda2 = Cancer Mutations with Evidence of Clinical Significance
# Fda3 = Cancer Mutations with Potential of Clinical Significance (loaded by oncokb_clinical_actionability_layer)
# Columns: Level, Setting, Gene, Alterations, Cancer Types
oncokb_fda_path <- "FDA_level_2_oncokb_biomarker_drug_associations.tsv"
if (file.exists(oncokb_fda_path)) {
    oncokb_fda_levels <- tryCatch(
        read.delim(oncokb_fda_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) {
            message("[WARN] FDA level file unreadable: ", e$message)
            data.frame()
        }
    )
    n_fda2 <- sum(oncokb_fda_levels$Level == "Fda2", na.rm = TRUE)
    message(sprintf("[INFO] Loaded OncoKB FDA Level 2: %d associations across %d unique genes",
        nrow(oncokb_fda_levels), length(unique(oncokb_fda_levels$Gene))))
} else {
    message("[WARN] FDA level file not found.")
    oncokb_fda_levels <- data.frame()
}

# ==============================================================================
# Initialize OncoKB Clinical Actionability Layer (v1.0)
# Multi-layer evidence framework: Cancer Gene → Biomarker → Regulatory → Pharmacogenomic
# Preserves strict governance separation between distinct evidence dimensions
# ==============================================================================
source("oncokb_clinical_actionability_layer.R")
oncokb_actionability_layer <- init_oncokb_clinical_actionability_layer(".")
message("[APP] OncoKB Clinical Actionability Layer initialized.")

# Flag to control TMB/MSI dependent functionality in Pharmacogenomic Translation
enable_TMB_MSI <- TRUE  # Patient-level TMB/MSI/TSM data is available from stemness_df; LLM uses actual per-patient classifications, not population-level signs

# -----------------------------------------------------------------------------
# Load stemness data and compute per-cancer cohort limits (Low/Intermediate/High)
stemness_df <- tryCatch(
  read.delim("Merged_Cancer_Stemness.tsv", sep = "\t", stringsAsFactors = FALSE),
  error = function(e) {
    message("[WARN] Merged_Cancer_Stemness.tsv not found; stemness-dependent features disabled.")
    data.frame()
  }
)
# Global variable to hold the TSM context string for the current patient (will be overwritten per request)
tsm_context <- ""  # global placeholder; actual TSM context is built locally inside the pharmacogenomic observeEvent

# DEAD CODE: ts_limits computed but never referenced; per-patient TSM now uses pre-computed classes from merged TSV.
# ts_limits <- stemness_df %>%
#  dplyr::group_by(cancer_type_abbreviation) %>%
#  dplyr::summarise(
#    RNAss_q33 = quantile(RNAss, probs = 0.33, na.rm = TRUE),
#    RNAss_q66 = quantile(RNAss, probs = 0.66, na.rm = TRUE),
#    EREG_q33 = quantile(EREG.EXPss, probs = 0.33, na.rm = TRUE),
#    EREG_q66 = quantile(EREG.EXPss, probs = 0.66, na.rm = TRUE)
#  )

# Helper to classify a TSM score based on its cancer-type cohort
# NOTE: The per-cancer TSM classification is now pre-computed in the merged TSV (columns RNAss_class and EREG_class).
# The helper function is no longer needed and has been removed.

  # Load the Biological Roles Dictionary
  gene_roles_path <- "Gene_Biological_Roles.csv"
  if (file.exists(gene_roles_path)) {
      gene_roles_df <- tryCatch(
        read.csv(gene_roles_path, stringsAsFactors = FALSE),
        error = function(e) {
          message("[WARN] Gene_Biological_Roles.csv corrupt; gene role enrichment disabled.")
          data.frame(Gene=character(), Biological_Role=character(), stringsAsFactors=FALSE)
        }
      )
  } else {
      gene_roles_df <- data.frame(Gene=character(), Biological_Role=character(), stringsAsFactors=FALSE)
  }

  # The raw matrix contains 'Cohort' entries like "ACC_DSS_df377". We must parse these to power the dropdown logic:
  cohort_matrix <- data.frame(Full_Name = unique(raw_cohort_matrix$Cohort), stringsAsFactors = FALSE)
  cohort_matrix$Cancer <- sapply(strsplit(cohort_matrix$Full_Name, "_"), `[`, 1)
  cohort_matrix$Metric <- sapply(strsplit(cohort_matrix$Full_Name, "_"), `[`, 2)
  cohort_matrix$DF_ID <- sapply(strsplit(cohort_matrix$Full_Name, "_"), `[`, 3)

  # ==============================================================================
  # 2. GLOBAL GPU VRAM
  # ==============================================================================
  # LLM TASK QUEUE (Global definitions)
  # ==============================================================================
  # ==============================================================================
  # ==============================================================================
  # GPU/LLM QUEUE MANAGEMENT SYSTEM (IN-MEMORY)
  # ==============================================================================
  # Define queue environment globally for the Shiny Server process
  global_llm_queue <- new.env(parent = emptyenv())
  global_llm_queue$status <- "idle"
  global_llm_queue$queue <- list()
  global_llm_queue$current_session <- NULL
  global_llm_queue$timestamp <- NULL

  # Helper to access/modify queue safely
  sweep_zombies <- function() {
      if (global_llm_queue$status == "busy" && !is.null(global_llm_queue$timestamp)) {
          last_time <- as.POSIXct(global_llm_queue$timestamp)
          if (as.numeric(difftime(Sys.time(), last_time, units="mins")) > 1) {
              if (length(global_llm_queue$queue) > 0) {
                  global_llm_queue$current_session <- global_llm_queue$queue[[1]]
                  global_llm_queue$queue <- if (length(global_llm_queue$queue) > 1) global_llm_queue$queue[-1] else list()
                  global_llm_queue$timestamp <- as.character(Sys.time())
                  global_llm_queue$status <- "busy"
              } else {
                  global_llm_queue$status <- "idle"
                  global_llm_queue$current_session <- NULL
              }
          }
      }
  }

  join_queue <- function(session_id) {
      sweep_zombies()

      # Prevent double-click ghosting
      if (global_llm_queue$status == "busy" && identical(global_llm_queue$current_session, session_id)) {
          return(0L)
      }

      if (global_llm_queue$status == "idle" && (is.null(global_llm_queue$current_session) || identical(global_llm_queue$current_session, ""))) {
          global_llm_queue$status <- "busy"
          global_llm_queue$current_session <- session_id
          global_llm_queue$timestamp <- as.character(Sys.time())
          return(0L)  # Go immediately
      }

      if (!session_id %in% unlist(global_llm_queue$queue)) {
          global_llm_queue$queue <- c(global_llm_queue$queue, list(session_id))
          global_llm_queue$timestamp <- as.character(Sys.time())
      }
      pos <- which(unlist(global_llm_queue$queue) == session_id)
      return(as.integer(pos))
  }

  release_queue <- function(session_id) {
      sweep_zombies()
      if (identical(global_llm_queue$current_session, session_id)) {
          if (length(global_llm_queue$queue) > 0) {
              global_llm_queue$current_session <- global_llm_queue$queue[[1]]
              global_llm_queue$queue <- if (length(global_llm_queue$queue) > 1) global_llm_queue$queue[-1] else list()
              global_llm_queue$status <- "busy"
          } else {
              global_llm_queue$current_session <- NULL
              global_llm_queue$status <- "idle"
          }
          global_llm_queue$timestamp <- as.character(Sys.time())
      } else {
          # If not current session, just remove from queue array
          if (session_id %in% unlist(global_llm_queue$queue)) {
              global_llm_queue$queue <- global_llm_queue$queue[unlist(global_llm_queue$queue) != session_id]
          }
      }
  }

  remove_from_queue <- function(session_id) {
      release_queue(session_id)
  }

  is_my_turn <- function(session_id) {
      sweep_zombies()
      return(identical(global_llm_queue$current_session, session_id))
  }

  get_queue_position <- function(session_id) {
      sweep_zombies()
      pos <- which(unlist(global_llm_queue$queue) == session_id)
      if (length(pos) == 0) return(NA_integer_)
      return(as.integer(pos))
  }

  # Load LLM Descriptors for Variable Dictionary and Gene Biological Roles
  if (file.exists("Table_S11_S12_Column_Descriptors.csv")) {
    tryCatch({
      first_line <- readLines("Table_S11_S12_Column_Descriptors.csv", n = 1)
      if(grepl(";", first_line)) {
        column_descriptors <- read.csv2("Table_S11_S12_Column_Descriptors.csv", stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        column_descriptors <- read.csv("Table_S11_S12_Column_Descriptors.csv", stringsAsFactors = FALSE, check.names = FALSE)
      }
    }, error = function(e) {
      column_descriptors <- NULL
    })
  } else {
    column_descriptors <- NULL
  }

  # gene_roles (NCBI_gene_info.csv) used by LLM insight generation in lineage module
  if (file.exists("NCBI_gene_info.csv")) {
    gene_roles <- read.csv("NCBI_gene_info.csv", sep=";", stringsAsFactors = FALSE)
  } else {
    gene_roles <- NULL
  }

  if (file.exists("Table_S11_Interpreter_12k.csv")) {
    tryCatch({
      first_line <- readLines("Table_S11_Interpreter_12k.csv", n = 1)
      if(grepl(";", first_line)) {
        table_s11_global <- read.csv2("Table_S11_Interpreter_12k.csv", stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        table_s11_global <- read.csv("Table_S11_Interpreter_12k.csv", stringsAsFactors = FALSE, check.names = FALSE)
      }
    }, error = function(e) { table_s11_global <<- NULL })
  } else {
    table_s11_global <- NULL
  }

  ui <- tagList(
    # Inject the Glassmorphism CSS architecture
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
      tags$style(HTML("
        body, html { overflow-y: auto !important; overflow-x: hidden !important; height: auto !important; min-height: 100vh; }
        .clickable-card { cursor: pointer; transition: transform 0.2s, box-shadow 0.2s; }
        .clickable-card:hover { transform: translateY(-5px); box-shadow: 0 10px 20px rgba(0,0,0,0.5); border-color: var(--primary-color); }
        .dropdown-menu { z-index: 99999 !important; }
        .navbar { z-index: 99998 !important; }
        .selectize-dropdown, .selectize-dropdown-content, .selectize-dropdown .option {
          background-color: #1e293b !important;
          background: #1e293b !important;
          color: #ffffff !important;
          opacity: 1 !important;
          z-index: 999999 !important;
        }
        .selectize-dropdown .active, .selectize-dropdown .option:hover {
          background: #3b82f6 !important;
          color: #ffffff !important;
        }

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
        h3(id = "spinner-title", style = "color: #1e293b; font-weight: bold; letter-spacing: 0.5px; margin-top: 10px;", "Preparing Report!"),
        p(id = "spinner-text", style = "color: #64748b; font-size: 1.1rem; margin-top: 10px;", HTML("Synthesizing individual multi-omic geometry.<br>Please wait while the file is compiled."))
      )
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('hide_spinner', function(message) {
        document.getElementById('loading-overlay').style.display = 'none';
        setTimeout(function() {
          var t = document.getElementById('spinner-title');
          var d = document.getElementById('spinner-text');
          if(t) t.innerText = 'Preparing Report!';
          if(d) d.innerHTML = 'Synthesizing individual multi-omic geometry.<br>Please wait while the file is compiled.';
        }, 500);
      });
      Shiny.addCustomMessageHandler('show_spinner', function(message) {
        showRepoSpinner(message.title, message.text);
      });
      Shiny.addCustomMessageHandler('llm_toggle_panels', function(message) {
        // Reserved for future queue panel visibility toggles
      });
      function showRepoSpinner(title, text) {
        var t = document.getElementById('spinner-title');
        var d = document.getElementById('spinner-text');
        if(t && title) t.innerText = title;
        if(d && text) d.innerHTML = text;
        document.getElementById('loading-overlay').style.display = 'flex';
      }
    ")),

    page_navbar(
      title = tags$span(style = "color: #00e5ff; text-shadow: 0 0 5px #00e5ff; font-weight: bold;", "CancerRCDPredictor"),
      id = "main_nav", # ID for navigation control
      fillable = FALSE,
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
                        div(class = "glass-title", bs_icon("info-circle"), "Precision Oncology Diagnostic Engine & Digital Molecular Tumor Board"),
                        p("Welcome to CancerRCDPredictor. This platform integrates 96 validated predictive models across 33 TCGA Pan-Cancer cohorts, spanning the OS, DSS, PFI, and DFI survival endpoints, through a deterministically mapped, audit-compliant SuperLearner architecture coupled with advanced AI-driven biological synthesis."),
                        p("Operating not only as a predictive diagnostic engine but also as an interactive Digital Molecular Tumor Board, the application translates high-dimensional multi-omic geometries into precision oncology insights. It shifts the analytical paradigm from population-level risk stratification toward individualized biological risk mapping, by interrogating precomputed survival topologies to generate high-fidelity, manuscript-grade interpretations."),
                        p(style="color: #cbd5e1; margin-top: 15px; font-weight: 300; line-height: 1.6;",
                          "At the core of the framework is an extensive machine-learning pipeline specifically designed to overcome the limitations of conventional linear proportional-hazards models. Through a non-linear Quadripartite ML Ensemble composed of Random Survival Forests (RSF), XGBoost, insulated Survival-Boruta feature selection, and Multi-Task Logistic Regression (MTLR), combined through an Elastic Net Multi-View Meta-Learner (MVL), the system evaluates more than 12,613 multi-omic prognostic signatures encompassing 25 forms of Regulated Cell Death (RCD) across seven distinct omic layers. Rather than relying on simple additive prognostic markers, this architecture isolates the predictive contribution of complex non-linear biological geometries. By extracting exact N-dimensional TreeSHAP interactions, the platform exposes the specific lethal and protective prognostic trajectories contributing to each patient's predicted outcome. These topological representations are subsequently interpreted by the integrated LLM engine through dynamic tumor-state reasoning. The system translates mathematical vulnerabilities, tumor stemness metrics (TSM), tumor mutational burden (TMB), microsatellite instability (MSI), pharmacogenomic associations, and multi-omic biological signals into a comprehensive translational molecular report designed to support precision oncology interpretation."
                        ),
                        p(style="color: #cbd5e1; margin-top: 10px; font-weight: 300; line-height: 1.6;",
                          "Importantly, all biological interpretations generated by the LLM are constrained by a multi-layer governance framework incorporating evidence-tiering, audit validation, uncertainty communication, ecological-fallacy prevention, hypothesis framing, and domain-boundary controls to ensure that patient-level conclusions remain strictly grounded in the available computational evidence."
                        )
                    ),
                    div(style = "flex-shrink: 0; margin: 0 auto;",
                        tags$img(src = "www/cancerrcdpredictor_logo_bloodorange.png",
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
                    p("SHAP Waterfall/Force Plots decompile the predictive logic for a single patient against a population baseline. The visualized bars directly represent specific multi-omic signatures (both continuous expression vectors and discrete genomic states like mutations/CNVs) acting as vectors of non-proportional hazard or protection. The predictive weight of each signature is defined by its breadth (width), orientation along the axis, and its top-to-bottom sequence in the trajectory."), p("Orange bars represent omic signatures imposing aggressive non-proportional hazard penalties (lethality), while purple bars represent signatures forcing deep negative non-proportional hazard pushes (protective shields)."),
                    tags$a(href = "#", onclick = "Shiny.setInputValue('go_to_tab', 'tab_tumor_shap', {priority: 'event'}); return false;", style = "text-decoration: none; color: #60a5fa;",
                           h4("4. The Digital Molecular Tumor Board", style = "transition: color 0.2s; cursor: pointer;", onmouseover="this.style.color='#93c5fd';", onmouseout="this.style.color='#60a5fa';")),
                    p("The Digital Molecular Tumor Board acts as a multi-agent AI system designed for deep, context-aware precision oncology. It integrates a patient's geometric trajectory (SHAP) with pharmacogenomic vulnerability profiling, allowing users to dynamically interrogate the model. The AI synthesizes multi-omic signatures, correlates them with tumor stemness (TSM), and proposes targeted therapeutic interventions while providing an interactive dialogue for hypothesis testing.")
                )
              )
    ),

    # ==============================================================================
    # TAB 0.2: Clinical User Manual

    nav_panel(title = "Methodological Integrity", value = "tab_methodology", icon = bs_icon("shield-check"),
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
                                      selectInput("edu_beeswarm_exemplar", "Select Exemplar:", choices = c("Multi-Omic Dominance (LGG DSS - Figure 4)", "Supreme Exemplar (READ OS - Figure S8)")),
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
                                 p("This module isolates the 150 elite biological signatures (Table S12) that were universally retained by all four Machine Learning algorithms (RSF, XGBoost, Boruta, MTLR)."),
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
                    p(tags$strong("Clinical Probabilities:"), " Analyze dynamic patient-specific survival and event probabilities across sequential 1, 3, and 5-year clinical landmarks using the Phase III algorithmic ensemble."),

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
                      ),
                      nav_panel("3. Individualized Clinical Probabilities", icon = bs_icon("activity"),
                        layout_columns(
                          col_widths = c(4, 8),
                          div(class = "glass-panel",
                              h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " Trajectory Selector"),
                              selectInput("traj_phase3_cancer", "Target Cohort (Cancer):", choices = NULL),
                              selectInput("traj_phase3_metric", "Clinical Outcome Metric:", choices = NULL),
                              hr(style = "border-color: rgba(255,255,255,0.1);"),
                              selectInput("traj_phase3_patient", "Select Patient ID:", choices = NULL),
                              tags$div(style = "margin-top: 30px;",
                                tags$div(style = "display: flex; gap: 10px;",
                                  downloadButton("download_traj_phase3_pdf", "Download PDF", class = "btn btn-danger", style = "flex: 1; font-weight: bold;")
                                )
                              )
                          ),
                          div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                              plotOutput("render_traj_phase3_plot", height = "500px")
                          )
                        )
                      ) # Closes nav_panel
                    ) # Closes navset_card_pill
                ) # Closes div
              ) # Closes layout_columns
    ),
  # ==============================================================================

    nav_panel(title = "Clinical Blind Validation", value = "tab_blind_validation", icon = bs_icon("shield-lock-fill"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("shield-check"), "Phase IV: Sequestered Validation Execution Paths"),
                    p("Navigate the completely sequestered internal validation cohort. View the Dual-Track continuous algorithmic trajectories mapping 1, 3, and 5-year clinical probabilities."),
                    navset_card_pill(
                      id = "validation_pill",
                      nav_panel("Survival Probability (OS/DSS)", icon = bs_icon("activity"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " S(t) Trajectory Selector"),
                                      selectInput("val_surv_cancer", "Target Cohort (Cancer):", choices = NULL),
                                      selectInput("val_surv_metric", "Survival Metric:", choices = c("OS", "DSS")),
                                      hr(style = "border-color: rgba(255,255,255,0.1);"),
                                      selectInput("val_surv_patient", "Select Patient ID:", choices = NULL),
                                      uiOutput("val_surv_path_info"),
                                      tags$div(style = "margin-top: 30px;",
                                        tags$div(style = "display: flex; gap: 10px;",
                                          downloadButton("download_val_surv_pdf", "Download PDF", class = "btn btn-danger", style = "flex: 1; font-weight: bold;")
                                        )
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      plotOutput("render_val_surv_plot", height = "450px")
                                  )
                                )
                      ),
                      nav_panel("Event Probability (DFI/PFI)", icon = bs_icon("exclamation-circle"),
                                layout_columns(
                                  col_widths = c(4, 8),
                                  div(class = "glass-panel",
                                      h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " 1 - S(t) Trajectory Selector"),
                                      selectInput("val_event_cancer", "Target Cohort (Cancer):", choices = NULL),
                                      selectInput("val_event_metric", "Event Metric:", choices = c("DFI", "PFI")),
                                      hr(style = "border-color: rgba(255,255,255,0.1);"),
                                      selectInput("val_event_patient", "Select Patient ID:", choices = NULL),
                                      uiOutput("val_event_path_info"),
                                      tags$div(style = "margin-top: 30px;",
                                        tags$div(style = "display: flex; gap: 10px;",
                                          downloadButton("download_val_event_pdf", "Download PDF", class = "btn btn-danger", style = "flex: 1; font-weight: bold;")
                                        )
                                      )
                                  ),
                                  div(class = "glass-panel", style = "min-height: 500px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
                                      plotOutput("render_val_event_plot", height = "450px")
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

    # ==============================================================================
    # TAB: THE DIGITAL MOLECULAR TUMOR BOARD
    # ==============================================================================
    nav_menu(
      title = tags$span(style="color: #10b981; font-weight: bold;", bs_icon("cpu"), " The Digital Molecular Tumor Board"),
      icon = bs_icon("diagram-3-fill"),

      nav_panel(title = "1. Population & Topological Intelligence", value = "tab_interpreter",
              layout_columns(col_widths = c(12),
                             div(class = "glass-panel", style = "border: 1px solid #60a5fa; box-shadow: 0 4px 30px rgba(96, 165, 250, 0.1);",
                                 div(class = "glass-title", style = "color: #60a5fa;", bs_icon("file-medical"), "Population & Topological Intelligence"),
                                 p("This engine converts the 12,613 high-dimensional prognostic signatures (Table S11) into human-readable clinical diagnostic reports."),
                                 hr(style = "border-color: rgba(96,165,250,0.2);"),

                                 layout_columns(col_widths = c(4, 8),
                                   # Signature Selector (Left Side)
                                   div(
                                     selectizeInput("interpreter_cancer_select", tags$span(style="color: #cbd5e1;", "1. Filter by Cancer Type (CTAB):"), choices = c("All Cancers" = ""), width = "100%", options = list(dropdownParent = 'body')),
                                     selectInput("interpreter_metric_select", tags$span(style="color: #cbd5e1;", "2. Filter by Survival Metric:"), choices = c("All Metrics" = ""), width = "100%"),
                                     selectizeInput("interpreter_signature_select", tags$span(style="color: #cbd5e1;", "3. Search & Select Signature Nomenclature:"), choices = NULL, width = "100%", options = list(dropdownParent = 'body'))
                                   ),

                                   # Diagnostic Printout Panel (Right Side)
                                   div(
                                     actionButton("btn_run_llm", "Generate AI Insight", class = "btn btn-primary", style="margin-bottom: 15px; width: 100%;", onclick = "showRepoSpinner('AI Engine', 'Synthesizing Narrative...<br>Please wait while the LLM generates the insight.');"),
                                     uiOutput("llm_queue_ui"),
                                     uiOutput("llm_queue_button_ui"),
                                     div(style = "background: rgba(15, 23, 42, 0.8); border: 1px solid #334155; border-radius: 8px; padding: 25px;",
                                         uiOutput("diagnostic_report_ui")
                                     )
                                   )
                                 )
                             )
              )
      ), # End Sub-Module 1

      nav_panel(title = "2. Personalized SHAP Decoding", value = "tab_tumor_shap",
                layout_columns(col_widths = c(12),
                               div(class = "glass-panel", style = "border: 1px solid #10b981; box-shadow: 0 4px 30px rgba(16, 185, 129, 0.1);",
                                   div(class = "glass-title", style = "color: #10b981;", bs_icon("person-bounding-box"), "Personalized SHAP Decoding"),
                                   p("Translate the mathematical trajectory (SHAP) of a specific patient into clinical vulnerability analysis."),
                                   hr(style = "border-color: rgba(16,185,129,0.2);"),
                                   layout_columns(col_widths = c(4, 8),
                                     div(
                                       selectInput("tumor_shap_cancer", tags$span(style="color: #cbd5e1;", "1. Select Cancer Cohort:"), choices = c("Loading..." = ""), width = "100%"),
                                       selectInput("tumor_shap_metric", tags$span(style="color: #cbd5e1;", "2. Select Survival Metric:"), choices = c("Loading..." = ""), width = "100%"),
                                       selectInput("tumor_shap_patient", tags$span(style="color: #cbd5e1;", "3. Select Patient ID:"), choices = c("Loading..." = ""), width = "100%"),
                                       actionButton("btn_run_shap_llm", "Decode SHAP Geometry", class = "btn btn-primary", style="margin-top: 15px; width: 100%;", onclick = "showRepoSpinner('AI Engine', 'Synthesizing Vulnerability...<br>Please wait while the LLM decodes the SHAP geometries.');"),
                                       uiOutput("llm_queue_ui_shap"),
                                       uiOutput("llm_queue_button_ui_shap"),
                                       hr(style = "border-color: rgba(255,255,255,0.1); margin-top: 20px; margin-bottom: 20px;"),
                                       actionButton("btn_run_pharma_llm", "Execute Pharmacogenomic Translation", class = "btn btn-warning", style="width: 100%; background: linear-gradient(135deg, #FFDF00, #D4AF37); color: black; border: none; font-weight: bold; box-shadow: 0 4px 15px rgba(212, 175, 55, 0.3);", onclick = "showRepoSpinner('Pharmacogenomic AI', 'Translating vulnerabilities...<br>Please wait while the LLM processes the matrix.');"),
                                       uiOutput("llm_queue_ui_pharma"),
                                       uiOutput("llm_queue_button_ui_pharma"),
                                       hr(style = "border-color: rgba(255,255,255,0.1); margin-top: 20px; margin-bottom: 20px;"),
                                       downloadButton("download_ai_report_html", "Download AI Synthesis (HTML)", class = "btn btn-outline-info", style="width: 100%; font-weight: bold; margin-bottom: 10px;", icon = icon("file-code"))
                                     ),
                                     div(style = "background: rgba(15, 23, 42, 0.8); border: 1px solid #334155; border-radius: 8px; padding: 25px;",
                                         uiOutput("tumor_shap_report_ui"),
                                         uiOutput("pharmacogenomic_report_ui")
                                     )
                                   )
                               )
                )
      ), # End Sub-Module 2

      nav_panel(title = "3. Clinical Q&A AI", value = "tab_tumor_chat",
                layout_columns(col_widths = c(12),
                               div(class = "glass-panel", style = "border: 1px solid #8b5cf6; box-shadow: 0 4px 30px rgba(139, 92, 246, 0.1);",
                                   div(class = "glass-title", style = "color: #8b5cf6;", bs_icon("chat-dots-fill"), "Clinical Q&A AI"),
                                   p("Interrogate the Tumor Board AI on specific topological interactions or patient trajectory vulnerabilities."),
                                   hr(style = "border-color: rgba(139,92,246,0.2);"),
                                   div(style = "height: 400px; overflow-y: auto; background: rgba(15, 23, 42, 0.8); border: 1px solid #334155; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
                                       uiOutput("tumor_chat_report_ui")
                                   ),
                                   uiOutput("llm_queue_ui_chat"),
                                   uiOutput("llm_queue_button_ui_chat"),
                                   selectInput("chat_prompt_examples", "Example Clinical Queries:",
                                               choices = c("Custom (Type your own query below)" = "",
                                                           "How does a BRCA1 mutation affect PARP inhibitor sensitivity in this cohort?" = "How does a BRCA1 mutation affect PARP inhibitor sensitivity in this cohort?",
                                                           "Explain the prognostic significance of high TP53 expression combined with elevated PIK3CA." = "Explain the prognostic significance of high TP53 expression combined with elevated PIK3CA.",
                                                           "What is the expected overall survival trajectory for a patient with high immune infiltration?" = "What is the expected overall survival trajectory for a patient with high immune infiltration?",
                                                           "Describe the multi-omic interactions driving early relapse in aggressive gliomas." = "Describe the multi-omic interactions driving early relapse in aggressive gliomas."),
                                               width = "100%"),
                                   layout_columns(col_widths = c(8, 2, 2),
                                     textInput("tumor_chat_input", label = NULL, placeholder = "Ask the AI a clinical question...", width = "100%"),
                                     actionButton("btn_run_chat_llm", "Send", class = "btn btn-primary", style="width: 100%;", onclick = "showRepoSpinner('AI Engine', 'Synthesizing response...<br>Please wait.');"),
                                     actionButton("btn_clear_chat", "Clear", class = "btn btn-secondary", style="width: 100%;")
                                   )
                               )
                )
      ) # End Sub-Module 3
    ), # End nav_menu The Digital Molecular Tumor Board

    # ==============================================================================
    # TAB: MULTIMEDIA
    # ==============================================================================
    nav_panel(title = "Multimedia", value = "tab_multimedia", icon = bs_icon("play-btn"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("collection-play-fill"), "Research Presentations"),
                    p("Explore the video and audio presentations covering the core concepts and findings of the study."),

                    layout_columns(
                      col_widths = c(6, 6),
                      # Audio Section
                      div(style = "background: rgba(15, 23, 42, 0.4); border: 1px solid #3b82f6; border-radius: 8px; padding: 20px;",
                          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                            h4(style = "color: #60a5fa; margin: 0;", bs_icon("headphones"), " Podcast"),
                            actionButton("btn_audio_desc", "Description", icon = icon("info-circle"), class = "btn btn-sm btn-info")
                          ),
                          h5("Why AI Ignores DNA in Cancer Survival: A Multi-Omic Paradox", style = "color: #cbd5e1; margin-bottom: 20px; font-weight: bold;"),
                          tags$audio(controls = NA, style = "width: 100%; outline: none;",
                            tags$source(src = "media/Why_AI_ignores_DNA_in_cancer_survival.m4a", type = "audio/mp4")
                          )
                      ),
                      # Video Section
                      div(style = "background: rgba(15, 23, 42, 0.4); border: 1px solid #ef4444; border-radius: 8px; padding: 20px;",
                          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                            h4(style = "color: #f87171; margin: 0;", bs_icon("camera-video-fill"), " Video Presentation"),
                            actionButton("btn_video_desc", "Description", icon = icon("info-circle"), class = "btn-sm btn-danger")
                          ),
                          h5("Insight Engine: Bypassing the Cox-Proportional Collapse in Personalized Survival Topologies", style = "color: #cbd5e1; margin-bottom: 20px; font-weight: bold;"),
                          tags$video(controls = NA, style = "width: 100%; border-radius: 4px;",
                            tags$source(src = "media/Insight_Engine__Bypassing_the_Cox-Proportional_Collapse.mp4", type = "video/mp4")
                          )
                      )
                    )
                )
              )
    ),

    # ==============================================================================
    # TAB: REPOSITORY
    # ==============================================================================
    nav_panel(title = "Repository", value = "tab_repository", icon = bs_icon("folder-symlink"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel",
                    div(class = "glass-title", bs_icon("hdd-network"), "Data Repository Downloads"),
                    p("Navigate and download the supporting raw datasets, trained model bundles, and analytical matrices directly from the ZIMA server architecture."),
                    hr(style = "border-color: rgba(255,255,255,0.1);"),
                    layout_columns(
                      col_widths = c(4, 8),
                      div(
                        h5(style = "color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 10px;", bs_icon("funnel"), " Repository Navigation"),
                        selectInput("repo_cancer", "Target Cohort (Cancer):", choices = NULL),
                        selectInput("repo_metric", "Clinical Outcome Metric:", choices = NULL),
                        hr(style = "border-color: rgba(255,255,255,0.1);"),
                        uiOutput("repo_df_info"),
                        selectInput("repo_subfolder", "Select Subdirectory / Bundle:", choices = NULL),
                        hr(style = "border-color: rgba(255,255,255,0.1); margin-top: 25px;"),
                        tags$div(style = "display: flex; gap: 10px; flex-direction: column;",
                                 downloadButton("download_repo_selected", "Download Selected", class = "btn btn-primary", style = "width: 100%;", onclick = "showRepoSpinner('Preparing Download!', 'Bundling the requested repository files.<br>Please wait while the ZIP is generated.');"),
                                 downloadButton("download_repo_all", "Download All in Directory (ZIP)", class = "btn btn-secondary", style = "width: 100%;", onclick = "showRepoSpinner('Preparing Download!', 'Bundling the entire directory.<br>Please wait while the ZIP is generated.');")
                        )
                      ),
                      div(style = "background: rgba(15, 23, 42, 0.8); border: 1px solid #334155; border-radius: 8px; padding: 25px; min-height: 500px;",
                          h5(style = "color: #cbd5e1; margin-bottom: 20px;", bs_icon("folder2-open"), " Directory Contents"),
                          DTOutput("repo_files_table")
                      )
                    )
                )
              )
    ),

    # ==============================================================================
    # TAB: RCDome
    # ==============================================================================
    nav_panel(title = span("RCDome", style = "text-transform:none;font-variant:normal;letter-spacing:normal"), value = "tab_rcdome", icon = bs_icon("grid-3x3-gap-fill"),
              layout_columns(
                col_widths = c(12),
                div(class = "glass-panel", style = "padding: 30px; text-align: center;",
                    div(class = "glass-title", bs_icon("box-arrow-up-right"), "RCDome — Regulated Cell Death Compendium"),
                    br(),
                    p("The comprehensive reference table of the 25 recognized forms of regulated cell death."),
                    tags$a(
                      href = "www/RCD_25_Forms_Reference_Table_FINAL.html", target = "_blank",
                      class = "btn btn-primary btn-lg",
                      style = "margin-top: 15px;",
                      bs_icon("box-arrow-up-right"), " Open RCDome in New Tab"
                    ),
                    br(), br(),
                    p(style = "font-size: 12px; color: #888;", "Click the button above to open the full reference table in a new browser tab.")
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
  "@article {Rodrigues de Souza2026.05.29.728842,
	author = {Rodrigues de Souza, Emanuell and Almeida Cordeiro Nogueira, Higor and dos Santos Lopes, Victor and Medina-Acosta, Enrique},
	title = {A Pan-Cancer Multi-Omic SuperLearner for Regulated Cell Death Survival Topologies},
	elocation-id = {2026.05.29.728842},
	year = {2026},
	doi = {10.64898/2026.05.29.728842},
	publisher = {Cold Spring Harbor Laboratory},
		URL = {https://www.biorxiv.org/content/early/2026/06/02/2026.05.29.728842},
	eprint = {https://www.biorxiv.org/content/early/2026/06/02/2026.05.29.728842.full.pdf},
	journal = {bioRxiv}
}"
                                   ),
                                   div(style = "text-align: center; margin-top: 15px;",
                                       tags$a(href = "https://www.biorxiv.org/content/10.64898/2026.05.29.728842v1", target = "_blank", HTML(paste(bs_icon("box-arrow-up-right"), " Read Manuscript Online")), class = "btn", style = "background-color: #ffffff; color: #1a365d; font-weight: bold; border: 2px solid #ffffff;")
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
                                                      tags$img(src = "www/ESR_Photo.jpg", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Emanuell Rodrigues de Souza", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Co-Author"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Emanuell-Rodrigues-De-Souza", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  ),
                                                  div(style = "text-align: center; margin-top: 20px;",
                                                      tags$img(src = "www/HACN_Photo.jpg", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Higor Almeida Cordeiro Nogueira", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Co-Author"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Higor-Cordeiro-Nogueira", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  )
                                   ),
                                   layout_columns(col_widths = c(6, 6),
                                                  div(style = "text-align: center; margin-top: 20px;",
                                                      tags$img(src = "www/VSL_Photo.png", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Victor dos Santos Lopes", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Co-Author"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Victor-Lopes-25", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  ),
                                                  div(style = "text-align: center; margin-top: 20px;",
                                                      tags$img(src = "www/EMA_Photo.jpg", style="width: 120px; height: 120px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 2px solid #3b82f6; box-shadow: 0 4px 10px rgba(0,0,0,0.5);"),
                                                      h5("Enrique Medina-Acosta", style = "margin-top: 15px;"),
                                                      p(style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 5px;", "Corresponding Author / ML Architect"),
                                                      tags$a(href = "https://www.researchgate.net/profile/Enrique-Medina-Acosta", target = "_blank", "ResearchGate", style = "color: #3b82f6; text-decoration: underline; font-weight: bold;")
                                                  )
                                   )
                               )
                )
      ),

      "---",
      nav_item(tags$a(href = "https://github.com/BioCancerInformatics/CancerRCDPredictor", target = "_blank", HTML(paste(bs_icon("github"), " GitHub Repository")), class = "nav-link")),
      nav_item(tags$a(href = "https://cancerrcdshiny.shinyapps.io/cancerrcdshiny/", target = "_blank", HTML(paste(bs_icon("box-arrow-up-right"), "Explore Legacy CancerRCDShiny (Populational Atlas)")), class = "nav-link")),
      nav_item(tags$a(href = "https://www.frontiersin.org/journals/bioinformatics/articles/10.3389/fbinf.2025.1630518/full", target = "_blank", HTML(paste(bs_icon("journal-text"), "Read Legacy CancerRCDShiny (Populational Atlas) Manuscript")), class = "nav-link"))
    ),

    nav_item(actionButton("open_feedback_btn", "💡 Send Feedback", style = "background: #0f172a; color: #38bdf8; border: 1px solid #0369a1; border-radius: 8px; font-weight: bold; margin-left: 15px;"))
  )
)

  # ==============================================================================
  # 2. SERVER ARCHITECTURE
  # ==============================================================================

  server <- function(input, output, session) {

    # AI Transparency Disclaimer Modal
    rv_ai_disclaimer_shown <- reactiveVal(FALSE)

    observeEvent(input$main_nav, {
      req(input$main_nav)
      ai_tabs <- c("tab_interpreter", "tab_tumor_shap", "tab_tumor_chat")

      if (input$main_nav %in% ai_tabs && !rv_ai_disclaimer_shown()) {
        showModal(modalDialog(
          title = HTML("<div style='color: #fbbf24; font-weight: bold; text-align: center;'>⚠️ Generative AI Transparency</div>"),
          HTML("
            <div style='text-align: justify; padding: 10px 20px;'>
              <p style='color: #ffffff; font-size: 1.05rem; line-height: 1.6;'>The predictive survival trajectories, mathematical SHAP coordinates, and pharmacogenomic drug interactions you are about to see are derived exclusively from our strictly regulated SuperLearner pipeline and evidence databases (e.g., DGIdb, RefSeq). <strong style='color: #fbbf24;'>They are not AI-generated.</strong></p>
              <p style='color: #ffffff; font-size: 1.05rem; line-height: 1.6;'>We utilize a Large Language Model (LLM) strictly as a <strong>clinical semantic translator</strong>. Its sole function is to bridge the gap between hard mathematics (such as a gene's correlation with tumor stemness) and established biology, weaving them into a fluid clinical hypothesis.</p>
              <p style='color: #ffffff; font-size: 1.05rem; line-height: 1.6;'><strong style='color: #fbbf24;'>The AI does not hallucinate facts, invent drug names, or borrow inferences outside the strict boundaries of the injected payload.</strong> Because this synthesis serves to bridge multi-omic mathematics with biological reasoning, it is intended to be hypothesis-generating. Under no circumstances should these interpretations be applied directly to patient care without independent evaluation by an attending oncology specialist.</p>
            </div>
          "),
          footer = tagList(
            actionButton("btn_accept_ai_disclaimer", "I Understand and Accept", style = "background: #10b981; color: white; border: none; font-weight: bold; padding: 10px 20px; border-radius: 6px;")
          ),
          size = "l",
          easyClose = FALSE,
          fade = TRUE
        ))
      }
    })

    observeEvent(input$btn_accept_ai_disclaimer, {
      rv_ai_disclaimer_shown(TRUE)
      removeModal()
    }, ignoreInit = TRUE)

    # Beta Feedback Module
    observeEvent(input$open_feedback_btn, {
      showModal(modalDialog(
        title = HTML("<div style='color: #38bdf8; font-weight: bold;'>💡 Suggestions and Improvements (Beta)</div>"),
        textAreaInput("feedback_text", "Describe your suggestion, issue, or any discrepancies found:", width = "100%", height = "150px"),
        textInput("feedback_email", "Email Address (Optional):", width = "100%", placeholder = "Enter your email if you'd like a response from the team"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_feedback_btn", "Send Feedback", style = "background: #0284c7; color: white; border: none; font-weight: bold;", onclick = "Shiny.setInputValue('feedback_email', document.getElementById('feedback_email').value, {priority: 'event'});")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$submit_feedback_btn, {
      feedback <- input$feedback_text
      email <- input$feedback_email
      if (trimws(feedback) != "") {
        # Save to persistent CSV
        feedback_file <- "user_feedback_log.csv"
        new_row <- data.frame(
          Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          Feedback = feedback,
          Email = email,
          stringsAsFactors = FALSE
        )
        if (!file.exists(feedback_file)) {
          write.csv(new_row, feedback_file, row.names = FALSE)
        } else {
          write.table(new_row, feedback_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
        }

        # Email transmission via blastula (R-native SMTP - no shell, no Python)
        # Uses SMTP_USER / SMTP_PASS from .Renviron. Silent fallback to CSV on failure.
        tryCatch({
          email_body <- feedback
          if (trimws(email) != "") {
            email_body <- paste0(email_body, "\n\n---\n📧 User Email: ", email)
          }
          blastula::smtp_send(
            blastula::compose_email(body = blastula::md(email_body)),
            to = "quique@uenf.br",
            from = Sys.getenv("SMTP_USER"),
            subject = "CancerRCD Prediction - Novo Feedback",
            credentials = blastula::creds_envvar(
              user = Sys.getenv("SMTP_USER"),
              pass_envvar = "SMTP_PASS",
              provider = "gmail"
            )
          )
        }, error = function(e) {
          # Silent fallback - feedback already persisted to CSV
        })

        removeModal()
        showNotification("✅ Feedback successfully saved and sent to the team! Thank you.", type = "message", duration = 8)
      }
    })

    # UNIQUE SESSION ID for Queue System
    my_session_id <- session$token
    rv_in_queue <- reactiveVal(FALSE)
    rv_queue_pos <- reactiveVal(NA_integer_)

    # CLEANUP: Remove user from JSON queue if they close their browser tab
    session$onSessionEnded(function() {
      remove_from_queue(my_session_id)
    })

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
    output$beeswarm_df_info <- renderUI({
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
              h6(style = "color: #94a3b8; font-size: 0.8rem; text-transform: uppercase; margin-bottom: 12px;", "Supreme Exemplar: Global Hierarchy of Survival Determinants"),
              p(style = "color: #e2e8f0; font-size: 0.85rem; margin-bottom: 8px;", "This beeswarm plot illustrates how the model organizes survival prediction across an entire patient population. Rather than focusing on a single patient, it reveals the global hierarchy of biological features that most strongly influence outcome prediction."),
              p(style = "color: #e2e8f0; font-size: 0.85rem; margin-bottom: 8px;", "Features are ranked from top to bottom according to their overall importance. The highest-ranking determinants define the primary decision architecture of the model, while lower-ranking variables provide progressively smaller refinements to the final prediction."),
              p(style = "color: #e2e8f0; font-size: 0.85rem; margin-bottom: 8px;", "Each dot represents one patient. Its horizontal position indicates whether that specific feature pushes the prediction toward higher risk (right) or lower risk (left). The color of the dot reflects the measured abundance of the biological variable, ranging from low values (blue) to high values (orange/red)."),
              p(style = "color: #e2e8f0; font-size: 0.85rem; margin-bottom: 8px;", "A key concept demonstrated by this figure is that survival prediction emerges from multiple biological layers acting simultaneously. Here, mRNA-, transcript isoform-, and microRNA-derived signatures occupy the top levels of the hierarchy, showing how distinct molecular systems collectively shape patient risk trajectories. The concentration of high-abundance values on the positive SHAP side indicates features whose elevated activity consistently drives mortality risk, whereas low-abundance states often contribute to protective survival trajectories."),
              p(style = "color: #e2e8f0; font-size: 0.85rem; margin-bottom: 0px;", "In practical terms, this figure teaches users how to interpret a SHAP beeswarm: feature rank reflects global importance, horizontal displacement reflects directional impact on risk, and color reveals the biological state responsible for that effect.")
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

    # DEAD CODE: edu_auroc_image_container not rendered in UI; safe to remove.
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

    # Resolve the effective signature: use the user's pick, or auto-discover the first available.
    resolve_topology_signature <- reactive({
      req(input$topology_cancer, input$topology_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$topology_cancer & cohort_matrix$Metric == input$topology_metric, ]
      req(nrow(selected_model) > 0)
      folder_name <- selected_model$Full_Name[1]
      xgboost_path <- file.path(zima_drive_path, folder_name, "XGBoost")

      # If the user already picked a signature that exists, use it
      if (!is.null(input$topology_signature) && input$topology_signature != "") {
        candidate <- file.path(xgboost_path, input$topology_signature)
        if (file.exists(candidate)) return(list(sig = input$topology_signature, folder = folder_name, path = xgboost_path))
      }

      # Otherwise auto-discover the first SHAP Dependence PDF
      if (dir.exists(xgboost_path)) {
        pdf_files <- list.files(xgboost_path, pattern = "_SHAP_Dependence_.*\\.pdf$")
        if (length(pdf_files) > 0) {
          return(list(sig = pdf_files[1], folder = folder_name, path = xgboost_path))
        }
      }
      return(NULL)
    })

    # Render the High-Res Topology PDF via iframe
    output$topology_plot_container <- renderUI({
      topo <- resolve_topology_signature()
      req(topo)

      pdf_url <- paste0("zima_models/", topo$folder, "/XGBoost/", topo$sig, "#zoom=100")

      tags$iframe(src = pdf_url, width = "100%", height = "700px", style = "border: 1px solid #334155; border-radius: 8px;")
    })

    # --- RENDER INTERACTION METADATA TABLE ---
    output$topology_metadata_table <- renderUI({
      topo <- resolve_topology_signature()
      req(topo)
      folder_name <- topo$folder

      # Filter the raw_cohort_matrix to the active cohort
      cohort_data <- raw_cohort_matrix[raw_cohort_matrix$Cohort == folder_name, ]
      req(nrow(cohort_data) > 0)

      # Robust string matching to find the exact interaction pair
      # We strip ALL non-alphanumeric characters to avoid unicode minus signs (U+2212) vs regular hyphens
      clean_sig <- gsub("[^[:alnum:]]", "", topo$sig)

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

        is_bifurcation <- grepl("BIFURCATION", match$Mathematical_Classification, ignore.case = TRUE)

        zone_ui <- tagList(
              tags$div(style = "flex: 1; min-width: 200px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #fbbf24;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Mathematical Classification"),
                tags$strong(style = "color: #fbbf24; font-size: 1.1rem;", match$Mathematical_Classification)
              ),
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #ef4444;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "FDR (High Zone)"),
                tags$strong(style = "color: #f87171;", match$FDR_HighZone)
              ),
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #10b981;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Spearman (High Zone)"),
                tags$strong(style = "color: #34d399;", match$Spearman_HighZone_CrossTalk)
              )
        )

        if (is_bifurcation) {
            zone_ui <- tagList(
              zone_ui,
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #ef4444;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "FDR (Mid Zone)"),
                tags$strong(style = "color: #f87171;", match$FDR_MidZone)
              ),
              tags$div(style = "flex: 1; min-width: 150px; background: rgba(0,0,0,0.3); padding: 10px; border-radius: 6px; border-left: 3px solid #10b981;",
                tags$small(style = "color: #94a3b8; display: block; margin-bottom: 4px;", "Spearman (Mid Zone)"),
                tags$strong(style = "color: #34d399;", match$Spearman_MidZone_CrossTalk)
              )
            )
        }

        tags$div(style = "margin-top: 20px; width: 100%; background: rgba(15, 23, 42, 0.8); border: 1px solid #334155; border-radius: 8px; padding: 20px; text-align: left;",
            tags$h6(style = "color: #3b82f6; font-weight: bold; margin-bottom: 15px; border-bottom: 1px solid rgba(59,130,246,0.3); padding-bottom: 8px;", bs_icon("clipboard-data"), " Clinical Intelligence (Table S15 Metadata)"),

            tags$div(style = "display: flex; gap: 15px; margin-bottom: 15px; flex-wrap: wrap;",
              zone_ui
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
        topo <- resolve_topology_signature()
        if (is.null(topo)) return("topology.tiff")
        gsub("\\.pdf$", ".tiff", topo$sig)
      },
      content = function(file) {
        topo <- resolve_topology_signature()
        req(topo)
        tiff_name <- gsub("\\.pdf$", ".tiff", topo$sig)
        tiff_path <- file.path(topo$path, tiff_name)
        if(file.exists(tiff_path)) {
          file.copy(tiff_path, file)
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
      csv_path <- "Table_S12_Golden_150.csv"
      if(file.exists(csv_path)) {
        tryCatch({
          first_line <- readLines(csv_path, n = 1)
          if(grepl(";", first_line)) {
            df <- read.csv2(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
          } else {
            df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
          }
          if(ncol(df) < 5) stop("Incomplete parsing (columns < 5)")
          return(df)
        }, error = function(e) {
          return(data.frame(Error = paste("Data schema mismatch:", e$message)))
        })
      } else {
        return(data.frame(Error = "Table_S12_Golden_150.csv not found locally"))
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

    # DEAD CODE: golden_rcd_plot not rendered in UI; safe to remove.
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

      # Restrict to user selected variables for Table S12 in precise sequential order
      selected_vars <- c("CTAB", "Nomenclature", "Signature", "Elements", "Decoded Genetic Element", "Biological Layer", "Cohorts Present", "RCD form", "Phenotype", "Total Validated Algorithms")
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

      pdf_url <- paste0("zima_models/", first_cohort, "/XGBoost/", first_cohort, "_SHAP_Dependence_", f_name, ".pdf#zoom=100")

      showModal(modalDialog(
        title = tags$span(style="color: #60a5fa; font-weight: bold;", bs_icon("graph-up"), paste("SHAP Dependency:", feature)),
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
      csv_path <- "Table_S11_Interpreter_12k.csv"
      if(file.exists(csv_path)) {
        tryCatch({
          first_line <- readLines(csv_path, n = 1)
          if(grepl(";", first_line)) {
            df <- read.csv2(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
          } else {
            df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
          }
          return(df)
        }, error = function(e) { return(NULL) })
      } else {
        NULL
      }
    })

    # STRICT UI LOCKOUT: All Navigation strictly bound to the 10k validated Table_S15 registry
    observeEvent(interpreter_data(), {
      if(exists("raw_cohort_matrix") && "CTAB" %in% names(raw_cohort_matrix)) {
        cancer_choices <- sort(unique(raw_cohort_matrix$CTAB))
        cancer_choices <- cancer_choices[cancer_choices != ""]
        updateSelectizeInput(session, "interpreter_cancer_select", choices = c("All Cancers" = "", cancer_choices), server = TRUE)
      }
    })

    observeEvent(input$interpreter_cancer_select, {
      cancer_filter <- input$interpreter_cancer_select
      metrics <- c()

      if (exists("raw_cohort_matrix") && !is.null(cancer_filter) && cancer_filter != "") {
        s15_sub <- raw_cohort_matrix[raw_cohort_matrix$CTAB == cancer_filter, ]
        if("Metric" %in% names(s15_sub)) {
           metrics <- sort(unique(s15_sub$Metric))
           metrics <- metrics[!is.na(metrics) & metrics != ""]
        }
      }

      updateSelectInput(session, "interpreter_metric_select", choices = c("All Metrics" = "", metrics), selected = "")
    })

    observe({
      cancer_filter <- input$interpreter_cancer_select
      metric_filter <- input$interpreter_metric_select

      valid_choices <- c()

      if (!is.null(cancer_filter) && cancer_filter != "" && exists("raw_cohort_matrix")) {
          s15_sub <- raw_cohort_matrix[raw_cohort_matrix$CTAB == cancer_filter, ]

          if(!is.null(metric_filter) && metric_filter != "") {
              s15_sub <- s15_sub[s15_sub$Metric == metric_filter, ]
          }

          # STRICT ENFORCEMENT: Dropdown is populated exclusively from Table S15 Primary Signatures
          s15_features <- unique(trimws(s15_sub$Primary_Signature))
          valid_choices <- s15_features[s15_features != ""]
      }

      updateSelectizeInput(session, "interpreter_signature_select", choices = valid_choices, server = TRUE)
    })

    output$signatures_overview_table <- renderDT({
      req(interpreter_data())
      df <- interpreter_data()
      cancer_filter <- input$interpreter_cancer_select
      metric_filter <- input$interpreter_metric_select

      if (!is.null(cancer_filter) && cancer_filter != "") {
        df <- df[df$CTAB == cancer_filter, ]
      }
      if (!is.null(metric_filter) && metric_filter != "") {
        col_cohorts <- if("Cohorts Present" %in% names(df)) "Cohorts Present" else "Cohorts_Present"
        if(col_cohorts %in% names(df)) {
          df <- df[grepl(paste0("_", metric_filter, "_"), df[[col_cohorts]], ignore.case = TRUE), ]
        }
      }

      # Harmonize column names to match Golden 150
      if("Biological_Layer" %in% names(df)) names(df)[names(df) == "Biological_Layer"] <- "Biological Layer"
      if("Cohorts_Present" %in% names(df)) names(df)[names(df) == "Cohorts_Present"] <- "Cohorts Present"
      if("Total_Validated_Algorithms" %in% names(df)) names(df)[names(df) == "Total_Validated_Algorithms"] <- "Total Validated Algorithms"

      # Calculate Total Validated Algorithms if missing
      if(!("Total Validated Algorithms" %in% names(df)) && "Cohorts Present" %in% names(df)) {
        df$`Total Validated Algorithms` <- sapply(df$`Cohorts Present`, function(x) length(unlist(strsplit(as.character(x), ","))))
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

    clean_llm_text_block <- function(txt) {
      if (is.null(txt)) return(character(0))
      # Strip reasoning/thinking blocks enclosed in <think>...</think>
      txt <- gsub("(?s)^.*?</think>\\s*", "", txt, perl = TRUE)
      txt <- gsub("\\*\\*", "", txt)
      txt <- gsub("\\*", "", txt)
      txt <- gsub("###", "", txt)
      txt <- gsub("##", "", txt)
      txt <- gsub("#", "", txt)
      # Remove leading bullet signs/dashes/colons
      txt <- gsub("\n[[:space:]*•-]+\\s*", " ", txt, perl = TRUE)
      txt <- gsub("^[[:space:]*•-]+\\s*", "", txt, perl = TRUE)
      txt <- gsub("^[Cc]olon[sS]?:?", "", txt)
      txt <- gsub("^[[:space:]\\p{P}]+", "", txt, perl = TRUE)
      txt <- trimws(txt)

      # Split into paragraphs by double newline
      paragraphs <- unlist(strsplit(txt, "\n\n+"))
      paragraphs <- sapply(paragraphs, function(p) {
        p <- gsub("\n", " ", p) # join single newlines
        p <- gsub("\\s+", " ", p) # normalize spaces
        p
      })
      paragraphs <- paragraphs[paragraphs != ""]
      return(paragraphs)
    }

    rv_llm_text <- reactiveVal(NULL)
    rv_llm_time <- reactiveVal(NULL)

    observeEvent(input$interpreter_signature_select, {
       rv_llm_text(NULL)
    })

    rv_trigger_llm <- reactiveVal(0)
    rv_trigger_shap_llm <- reactiveVal(0)
    rv_trigger_pharma_llm <- reactiveVal(0)
    rv_trigger_chat_llm <- reactiveVal(0)
    rv_pending_task <- reactiveVal("")

    observeEvent(input$btn_run_llm, {
      req(input$interpreter_signature_select)

      # Quick check if LLM is available
      status <- check_llm_status()
      if (!status$success) {
         session$sendCustomMessage("hide_spinner", list())
         showNotification(paste0("LLM Service Error: ", status$message), type = "error")
         return()
      }

      rv_pending_task("interpreter")

      # DeepSeek: no queue needed (cloud API handles parallel requests)
      if (shiny::isolate(llm_config$backend) == "deepseek") {
        rv_in_queue(FALSE)
        session$sendCustomMessage("llm_toggle_panels", list(show = FALSE))
        rv_trigger_llm(rv_trigger_llm() + 1)
        return()
      }

      # Ollama: use queue (single-process LLM)
      pos <- join_queue(my_session_id)

      if (pos == 0L) {
        rv_in_queue(FALSE)
        session$sendCustomMessage("llm_toggle_panels", list(show = FALSE))
        rv_trigger_llm(rv_trigger_llm() + 1)
      } else {
        rv_in_queue(TRUE)
        rv_queue_pos(pos)
        session$sendCustomMessage("hide_spinner", list())
      }
    })

    observeEvent(input$btn_force_reset_queue, {
        global_llm_queue$status <- "idle"
        global_llm_queue$queue <- list()
        global_llm_queue$current_session <- NULL
        global_llm_queue$timestamp <- NULL
        rv_in_queue(FALSE)
        rv_queue_pos(NA_integer_)
        session$sendCustomMessage("hide_spinner", list())
    })

    queue_ui_content <- reactive({
      if (rv_in_queue()) {
        pos <- rv_queue_pos()
        pos_text <- if (!is.na(pos)) paste0(pos, " person(s) ahead of you (Estimated wait: ~", pos * 30, "s)") else "calculating..."
        return(
          div(style = "margin-bottom: 15px; padding: 14px; background: rgba(251,191,36,0.1); border: 1px solid #fbbf24; border-radius: 8px;",
              tags$p(style = "color: #fbbf24; font-weight: bold; margin-bottom: 6px;",
                     bs_icon("clock-history"), " Waiting in Queue..."),
              tags$p(style = "color: #e2e8f0; font-size: 0.88rem; margin-bottom: 10px;",
                     paste0("The LLM is currently occupied processing another request. You have ", pos_text, ".")),
              tags$p(style = "color: #94a3b8; font-size: 0.78rem; margin-bottom: 10px;",
                     "Your interpretation will automatically start when it is your turn."),
              actionButton("btn_force_reset_queue", "Force Reset Queue (Debug)", class = "btn btn-danger btn-sm", style="margin-top: 10px; width: 100%;")
          )
        )
      }
      return(NULL)
    })

    queue_button_content <- reactive({
      if (rv_in_queue()) {
         return(actionButton("btn_cancel_queue", "Cancel Request", class = "btn btn-sm btn-outline-warning", style = "width: 100%; margin-bottom: 15px;"))
      }
      return(NULL)
    })

    output$llm_queue_ui <- renderUI({ queue_ui_content() })
    output$llm_queue_ui_shap <- renderUI({ queue_ui_content() })
    output$llm_queue_ui_chat <- renderUI({ queue_ui_content() })

    output$llm_queue_button_ui <- renderUI({ queue_button_content() })
    output$llm_queue_button_ui_shap <- renderUI({ queue_button_content() })
    output$llm_queue_button_ui_chat <- renderUI({ queue_button_content() })

    output$llm_queue_ui_pharma <- renderUI({ queue_ui_content() })
    output$llm_queue_button_ui_pharma <- renderUI({ queue_button_content() })

    observeEvent(input$btn_cancel_queue, {
      remove_from_queue(my_session_id)
      rv_in_queue(FALSE)
      rv_queue_pos(NA_integer_)
    })

    # QUEUE POLLING MECHANISM
    queue_poll <- reactivePoll(3000, session,
      checkFunc = function() {
        if (!rv_in_queue()) return(Sys.time())
        # Force a continuous poll every 3 seconds to guarantee the Zombie Sweeper executes!
        return(Sys.time())
      },
      valueFunc = function() {
        if (!rv_in_queue()) return(NULL)
        return(list(my_turn = is_my_turn(my_session_id), pos = get_queue_position(my_session_id)))
      }
    )

    observe({
      val <- queue_poll()
      if (is.null(val) || !rv_in_queue()) return()

      if (!is.na(val$pos)) rv_queue_pos(val$pos)

      if (isTRUE(val$my_turn)) {
        rv_in_queue(FALSE)
        rv_queue_pos(NA_integer_)
        session$sendCustomMessage("llm_toggle_panels", list(show = FALSE))

        task <- rv_pending_task()
        if (task == "interpreter") {
            session$sendCustomMessage("show_spinner", list(title="AI Engine", text="Synthesizing Narrative...<br>Please wait while the LLM generates the insight."))
            rv_trigger_llm(rv_trigger_llm() + 1)
        } else if (task == "shap") {
            session$sendCustomMessage("show_spinner", list(title="AI Engine", text="Synthesizing Vulnerability...<br>Please wait while the LLM decodes the SHAP geometries."))
            rv_trigger_shap_llm(rv_trigger_shap_llm() + 1)
        } else if (task == "pharma") {
            session$sendCustomMessage("show_spinner", list(title="AI Engine", text="Translating Pharmacogenomic Tokens...<br>Please wait while the LLM matches therapeutic interventions."))
            rv_trigger_pharma_llm(rv_trigger_pharma_llm() + 1)
        } else if (task == "chat") {
            session$sendCustomMessage("show_spinner", list(title="AI Engine", text="Generating Response...<br>Please wait while the LLM replies to your query."))
            rv_trigger_chat_llm(rv_trigger_chat_llm() + 1)
        } else {
            session$sendCustomMessage("show_spinner", list(title="AI Engine", text="Processing..."))
            rv_trigger_llm(rv_trigger_llm() + 1)
        }
      }
    })

    observeEvent(rv_trigger_llm(), {
      req(rv_trigger_llm() > 0)
      on.exit({
        release_queue(my_session_id)
      })

      req(input$interpreter_signature_select)
      df <- interpreter_data()
      req(df)
      row <- df[df$Feature == input$interpreter_signature_select, ]
      if(nrow(row) == 0) return(NULL)

      extract_col <- function(col_names, default = "Unspecified") {
        for (col in col_names) {
          if (col %in% names(row) && !is.na(row[[col]]) && row[[col]] != "") {
            return(as.character(row[[col]]))
          }
        }
        return(default)
      }

      f_bio <- extract_col(c("Biological Layer", "Biological_Layer"), "Unspecified Layer")
      f_cohorts <- extract_col(c("Cohorts Present", "Cohorts_Present"), "Unspecified Cohorts")
      f_nom <- extract_col("Nomenclature", "Unspecified Nomenclature")
      f_sig <- extract_col("Signature", "Unspecified Signature")
      f_ctab <- extract_col("CTAB", "Unspecified")
      f_rcd <- extract_col(c("RCD form", "RCD_form"), "Unspecified")
      f_pheno <- extract_col("Phenotype", "Unspecified")

      selected_metric <- input$interpreter_metric_select
      if (is.null(selected_metric) || selected_metric == "" || selected_metric == "All Metrics") {
         cohorts_list <- unlist(strsplit(f_cohorts, ",\\s*"))
         if (length(cohorts_list) > 0) {
            first_cohort <- cohorts_list[1]
            parts <- unlist(strsplit(first_cohort, "_"))
            if (length(parts) >= 2) {
               selected_metric <- parts[2]
            }
         }
      }
      if (is.null(selected_metric) || is.na(selected_metric)) {
         selected_metric <- ""
      }

      nom_parts <- unlist(strsplit(gsub("-", "\\.", row$Feature), "\\."))

      f_corr <- "an unspecified"
      f_gfc <- "Unknown"; f_tnc <- "Unknown"; f_rcd_count <- "Unknown"

      if(length(nom_parts) >= 11) {
        if(nom_parts[5] == "N") f_corr <- "negative"
        if(nom_parts[5] == "P") f_corr <- "positive"

        gfc_val <- nom_parts[3]
        if(gfc_val == "1") f_gfc <- "Protein expression"
        if(gfc_val == "2") f_gfc <- "Mutations"
        if(gfc_val == "3") f_gfc <- "CNV"
        if(gfc_val == "4") f_gfc <- "miRNA expression"
        if(gfc_val == "5") f_gfc <- "Transcript expression"
        if(gfc_val == "6") f_gfc <- "mRNA expression"
        if(gfc_val == "7") f_gfc <- "CpG Methylation"

        tnc_val <- nom_parts[6]
        if(tnc_val == "0") f_tnc <- "No Data"
        if(tnc_val == "1") f_tnc <- "Unchanged expression"
        if(tnc_val == "2") f_tnc <- "Underexpressed"
        if(tnc_val == "3") f_tnc <- "Overexpressed"

        f_rcd_count <- nom_parts[11]
      }

      f_pheno_full <- "phenotype"
      if(f_pheno == "1" || f_pheno == "TMB") f_pheno_full <- "Tumor Mutational Burden (TMB)"
      if(f_pheno == "2" || f_pheno == "MSI") f_pheno_full <- "Microsatellite Instability (MSI)"
      if(f_pheno == "3" || f_pheno == "TSM") f_pheno_full <- "Tumor Stemness Measure (TSM)"

      cancer_dict <- c(
        "ACC" = "Adrenocortical Carcinoma", "BLCA" = "Bladder Urothelial Carcinoma",
        "BRCA" = "Breast Invasive Carcinoma", "CESC" = "Cervical Squamous Cell Carcinoma",
        "CHOL" = "Cholangiocarcinoma", "COAD" = "Colon Adenocarcinoma",
        "DLBC" = "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma", "ESCA" = "Esophageal Squamous Cell Carcinoma",
        "GBM" = "Glioblastoma Multiforme", "HNSC" = "Head and Neck Squamous Cell Carcinoma",
        "KICH" = "Kidney Chromophobe", "KIRC" = "Kidney Renal Clear Cell Carcinoma",
        "KIRP" = "Kidney Renal Papillary Cell Carcinoma", "LAML" = "Acute Myeloid Leukemia",
        "LGG" = "Brain Lower Grade Glioma", "LIHC" = "Liver Hepatocellular Carcinoma",
        "LUAD" = "Lung Adenocarcinoma", "LUSC" = "Lung Squamous Cell Carcinoma",
        "MESO" = "Mesothelioma", "OV" = "Ovarian Serous Cystadenocarcinoma",
        "PAAD" = "Pancreatic Adenocarcinoma", "PCPG" = "Pheochromocytoma and Paraganglioma",
        "PRAD" = "Prostate Adenocarcinoma", "READ" = "Rectum Adenocarcinoma",
        "SARC" = "Sarcoma", "SKCM" = "Skin Cutaneous Melanoma",
        "STAD" = "Stomach Adenocarcinoma", "TGCT" = "Testicular Germ Cell Tumors",
        "THCA" = "Thyroid Carcinoma", "THYM" = "Thymoma", "UCEC" = "Uterine Corpus Endometrial Carcinoma",
        "UCS" = "Uterine Carcinosarcoma", "UVM" = "Uveal Melanoma"
      )
      f_cancer_full <- if(f_ctab %in% names(cancer_dict)) cancer_dict[[f_ctab]] else f_ctab

      f_num_metrics <- length(unlist(strsplit(f_cohorts, ",")))
      metric_text <- as.character(f_num_metrics)
      f_total_algos <- "Unspecified"
      if ("Total Validated Algorithms" %in% names(row)) f_total_algos <- as.character(row[["Total Validated Algorithms"]])
      else if ("Total_Validated_Algorithms" %in% names(row)) f_total_algos <- as.character(row[["Total_Validated_Algorithms"]])
      else f_total_algos <- metric_text

      f_decoded <- "Unspecified"
      if ("Decoded Genetic Element" %in% names(row)) f_decoded <- as.character(row[["Decoded Genetic Element"]])
      else if ("Decoded_Genetic_Element" %in% names(row)) f_decoded <- as.character(row[["Decoded_Genetic_Element"]])
      else if ("Decoded Genetic Element Primary" %in% names(row)) f_decoded <- as.character(row[["Decoded Genetic Element Primary"]])
      else if ("Decoded_Genetic_Element_Primary" %in% names(row)) f_decoded <- as.character(row[["Decoded_Genetic_Element_Primary"]])

      f_combined_outcome <- "Unspecified"
      if ("Combined Outcome" %in% names(row)) f_combined_outcome <- as.character(row[["Combined Outcome"]])
      else if ("Combined_Outcome" %in% names(row)) f_combined_outcome <- as.character(row[["Combined_Outcome"]])

      # Clean up Ensembl IDs in Signature
      raw_sig_elements <- unlist(strsplit(gsub("[()\\s`]", "", as.character(f_sig)), "\\+"))
      if (length(raw_sig_elements) > 5) {
        clean_sig_ids <- paste0(paste(raw_sig_elements[1:5], collapse = ", "), ", ... (total of ", length(raw_sig_elements), " elements)")
      } else {
        clean_sig_ids <- as.character(f_sig)
        clean_sig_ids <- gsub("[()\\s`]", "", clean_sig_ids)
        clean_sig_ids <- gsub("\\+", ", ", clean_sig_ids)
        clean_sig_ids <- sub(",\\s*([^,\\s]+)$", " and \\1", clean_sig_ids)
      }

      # Extract number of elements directly from the Elements column in the dataset
      f_elements_count <- "Unspecified"
      if ("Elements" %in% names(row)) f_elements_count <- as.character(row[["Elements"]])
      else if ("elements" %in% names(row)) f_elements_count <- as.character(row[["elements"]])
      else f_elements_count <- as.character(length(raw_sig_elements))

      target_desc <- ""
      # Strip backticks from f_decoded before matching to prevent regex failures on backticked gene names
      f_decoded_clean <- gsub("`", "", f_decoded)
      matches <- gregexpr("([A-Za-z0-9_\\-]+)\\(([0-9]+)/([0-9]+)\\)", f_decoded_clean)

      if (matches[[1]][1] != -1) {
        match_list <- regmatches(f_decoded_clean, matches)[[1]]
        parts_list <- lapply(match_list, function(m) {
          p <- strsplit(m, "\\(")[[1]]
          gene <- p[1]
          frac <- gsub("\\)$", "", p[2])
          frac_p <- strsplit(frac, "/")[[1]]
          list(gene = gene, num = frac_p[1], total = frac_p[2])
        })

        layer_term <- if (f_bio %in% c("miRNA expression", "microRNA")) "mature microRNAs" else "transcript isoforms"

        if (length(parts_list) > 5) {
          # High-dimensional: format concisely to avoid prompt bloat and keep facts clear
          shown_parts <- parts_list[1:5]
          isoform_details <- sapply(shown_parts, function(x) {
            paste0(x$gene, " (", x$num, "/", x$total, ")")
          })
          target_desc <- paste0("the targeted biological elements in this signature are ", f_elements_count, " distinct ", layer_term, " across ",
                                length(parts_list), " distinct genes (including: ",
                                paste(isoform_details, collapse = ", "),
                                ", among others). These elements are mapped to the specific sequence identifier(s) ", clean_sig_ids)
        } else {
          # Low-dimensional: build detailed narrative parts
          narrative_parts <- sapply(parts_list, function(x) {
            paste0(layer_term, " of the gene ", x$gene, " (specifically, ", x$num, " out of ", x$total, " known isoforms)")
          })
          target_desc <- paste0("the targeted biological elements in this signature are ", f_elements_count, " distinct ", layer_term,
                                " across ", length(parts_list), " distinct genes, consisting of ",
                                paste(narrative_parts, collapse = ", and "),
                                ", mapped to the specific sequence identifier(s) ", clean_sig_ids)
        }
      } else {
        # Fallback if no isoform parenthesis structure exists
        genes_list <- unique(sapply(strsplit(gsub("[()\\s`]", "", f_decoded), "\\+")[[1]], trimws))

        # Check for CpG Methylation layer to avoid claiming a single CpG site is involved
        if (!is.null(f_bio) && !is.na(f_bio) && f_bio %in% c("CpG Methylation", "Methylation")) {
          if (length(genes_list) > 5) {
            shown_genes <- paste(genes_list[1:5], collapse = ", ")
            target_desc <- paste0("the targeted biological elements in this signature represent the methylation states of a set of CpG sites across ",
                                  length(genes_list), " distinct genes (including: ", shown_genes, ", among others). These elements are mapped to the sequence identifier(s) ", clean_sig_ids)
          } else {
            clean_decoded <- gsub("[()\\s`]", "", f_decoded)
            clean_decoded <- gsub("\\+", ", ", clean_decoded)
            clean_decoded <- sub(",\\s*([^,\\s]+)$", " and \\1", clean_decoded)
            target_desc <- paste0("the targeted biological elements in this signature represent the methylation states of a set of CpG sites associated with the gene(s) ",
                                  clean_decoded, ". These elements are mapped to the specific sequence identifier(s) ", clean_sig_ids)
          }
        } else {
          layer_term <- "gene/transcript"
          if (!is.null(f_bio) && !is.na(f_bio)) {
            if (f_bio %in% c("Protein", "Protein expression")) layer_term <- "protein product of the gene"
            else if (f_bio %in% c("mRNA", "mRNA expression")) layer_term <- "mRNA transcript"
            else if (f_bio %in% c("Somatic Mutation", "Mutation")) layer_term <- "somatic mutation locus"
          }

          if (length(genes_list) > 5) {
            # High-dimensional: show count and first 5 genes
            shown_genes <- paste(genes_list[1:5], collapse = ", ")
            target_desc <- paste0("the targeted biological elements in this signature are ", f_elements_count, " distinct ",
                                  layer_term, "s across ", length(genes_list), " distinct genes (including: ",
                                  shown_genes, ", among others). These elements are mapped to the sequence identifier(s) ", clean_sig_ids)
          } else {
            # Low-dimensional: list all genes
            clean_decoded <- gsub("[()\\s`]", "", f_decoded)
            clean_decoded <- gsub("\\+", ", ", clean_decoded)
            clean_decoded <- sub(",\\s*([^,\\s]+)$", " and \\1", clean_decoded)
            target_desc <- paste0("the targeted biological elements in this signature are ", f_elements_count, " distinct ",
                                  layer_term, "s of the gene(s) ", clean_decoded,
                                  ". These elements are mapped to the specific sequence identifier(s) ", clean_sig_ids)
          }
        }
      }

      # Add biological role details if available in gene_roles database
      role_sentences <- ""
      genes_to_lookup <- unique(sapply(strsplit(gsub("[()\\s`]", "", f_decoded), "\\+")[[1]], function(g) {
        gsub("\\(.*$", "", g) # remove anything after parenthesis
      }))

      role_details <- c()
      if (!is.null(gene_roles)) {
        # Limit NCBI lookups to the first 5 genes to prevent prompt bloat and context window distraction
        lookup_subset <- if(length(genes_to_lookup) > 5) genes_to_lookup[1:5] else genes_to_lookup
        for (g in lookup_subset) {
          if (g %in% gene_roles$Gene.symbol) {
            idx <- which(gene_roles$Gene.symbol == g)[1]
            desc <- as.character(gene_roles$Description[idx])
            summ <- as.character(gene_roles$Summary[idx])

            if(is.na(desc) || desc == "nan" || desc == "") desc <- ""
            if(is.na(summ) || summ == "nan" || summ == "" || grepl("no summary available", summ, ignore.case=TRUE)) summ <- ""

            full_role <- desc
            if(summ != "") {
              if(full_role != "") full_role <- paste0(full_role, ": ")
              full_role <- paste0(full_role, summ)
            }

            if (full_role != "") {
              # Clean up citations
              full_role <- gsub("\\[provided by RefSeq[^\\]]*\\]", "", full_role)
              full_role <- trimws(full_role)
              role_details <- c(role_details, paste0("The biological role of ", g, " is characterized by its function in ", full_role))
            }
          }
        }
        if(length(genes_to_lookup) > 5) {
          role_details <- c(role_details, paste0("Note: This signature targets a total of ", length(genes_to_lookup), " genes; only the primary biological roles of the first 5 genes are summarized here to prevent details overload."))
        }
      }

      if (length(role_details) > 0) {
        role_sentences <- paste0(" ", paste(role_details, collapse = " Additionally, "), ".")
      }

      f_gene_narrative <- paste0(target_desc, ".", role_sentences)

      # Calculate Interaction Topology
      f_total_interactions <- 0
      f_synergy_count <- 0
      f_antagonism_count <- 0
      f_bifurcation_count <- 0
      f_interaction_role <- "No significant interactions found."
      f_modulated_layers <- "None"

      if(exists("raw_cohort_matrix")) {
          if (!is.null(selected_metric) && selected_metric != "") {
              cohort_subset <- raw_cohort_matrix[raw_cohort_matrix$CTAB == f_ctab & raw_cohort_matrix$Metric == selected_metric, ]
          } else {
              cohort_regex <- paste0("^", f_ctab)
              cohort_subset <- raw_cohort_matrix[grepl(cohort_regex, raw_cohort_matrix$Cohort, ignore.case=TRUE), ]
          }

          # EXCLUDE ALL "NOT SIGNIFICANT" EVALUATIONS BEFORE SEARCHING
          cohort_subset <- cohort_subset[!grepl("NOT SIGNIFICANT", cohort_subset$Mathematical_Classification, ignore.case=TRUE), ]

          if(nrow(cohort_subset) > 0) {
              target_sig <- trimws(input$interpreter_signature_select)

              # STRICT UNIDIRECTIONAL HIERARCHY: Selected signature is ALWAYS the Primary Target
              all_idx <- which(trimws(cohort_subset$Primary_Signature) == target_sig)
              f_total_interactions <- length(all_idx)

              if(f_total_interactions > 0) {
                 interactions_df <- cohort_subset[all_idx, ]
                 f_synergy_count <- sum(grepl("SYNERGY", interactions_df$Mathematical_Classification, ignore.case=TRUE))
                 f_antagonism_count <- sum(grepl("ANTAGONISM", interactions_df$Mathematical_Classification, ignore.case=TRUE))
                 f_bifurcation_count <- sum(grepl("BIFURCATION", interactions_df$Mathematical_Classification, ignore.case=TRUE))

                 partners <- unique(trimws(interactions_df$color_var_Partner))
                 partners <- partners[partners != ""]

                 if(length(partners) > 0) {
                     decode_partner_layer <- function(p) {
                         parts <- unlist(strsplit(as.character(p), "\\."))
                         if (length(parts) >= 2) {
                             layer <- switch(parts[2],
                                 "1" = "Protein Expression",
                                 "2" = "Mutations",
                                 "3" = "CNV",
                                 "4" = "miRNA Expression",
                                 "5" = "Transcript Isoform",
                                 "6" = "Bulk mRNA Expression",
                                 "7" = "CpG Methylation",
                                 "Unknown Layer"
                             )
                             return(paste0(p, " (Omic Layer: ", layer, ")"))
                         }
                         return(p)
                     }
                     decoded_partners <- sapply(head(partners, 5), decode_partner_layer)
                     f_interaction_role <- paste0("Primary Prognostic Driver modulated by ", length(partners), " distinct trans-signature partner(s). Explicit Modulating Partners include: ", paste(decoded_partners, collapse=", "))
                 } else {
                     f_interaction_role <- "Primary Prognostic Driver with trans-signature modulation."
                 }

                 layers <- unique(c(interactions_df$Primary_Omic_Token, interactions_df$Partner_Omic_Token))
                 layers <- layers[!is.na(layers) & layers != ""]
                 if(length(layers) > 0) f_modulated_layers <- paste(layers, collapse=", ")
              }
          }
      }

      # Dynamic facts compiler based on Table_S11_S12_Column_Descriptors.csv
      compile_facts_for_block <- function(block_name) {
        facts <- c()
        if (!is.null(column_descriptors)) {
          # Find descriptors targeting this block (supporting dual block mappings)
          matching_desc <- column_descriptors[grepl(block_name, column_descriptors$Target_Block, fixed = TRUE), ]
          if (nrow(matching_desc) > 0) {
            for (i in seq_len(nrow(matching_desc))) {
              col_name <- matching_desc$Column_Name[i]
              col_desc <- matching_desc$Descriptor[i]

              if (col_name %in% names(row)) {
                if (col_name %in% c("Feature", "Nomenclature")) {
                  next
                }
                val <- as.character(row[[col_name]])
                if (!is.na(val) && val != "") {
                  decoded_val <- val
                  if (col_name %in% c("Decoded Genetic Element", "Decoded_Genetic_Element")) {
                     decoded_val <- f_gene_narrative
                  } else if (col_name == "CTAB") {
                     decoded_val <- paste0(val, " (", f_cancer_full, ")")
                  } else if (col_name == "Phenotype") {
                     decoded_val <- f_pheno_full
                  } else if (col_name == "SCS") {
                     decoded_val <- paste0(val, " (", f_corr, " correlation with ", f_pheno_full, ")")
                  } else if (col_name %in% c("RCD form", "RCD_form")) {
                     decoded_val <- val
                  } else if (col_name == "Expression") {
                     decoded_val <- f_tnc
                  } else if (col_name %in% c("Biological Layer", "Biological_Layer", "Omic feature", "Omic_feature")) {
                     decoded_val <- f_bio
                  } else if (col_name %in% c("Signature")) {
                     decoded_val <- clean_sig_ids
                  } else if (col_name == "Elements") {
                     decoded_val <- f_elements_count
                  }
                  facts <- c(facts, paste0("- ", col_desc, ": ", decoded_val))
                }
              }
            }
          }
        }

        # Fallback list if column_descriptors is missing or empty
        if (length(facts) == 0) {
          if (block_name == "Population Information Block") {
            facts <- c(
              paste0("- The precise gene symbols decoded from the signature, representing the true biological target: ", f_gene_narrative),
              paste0("- The primary gene symbol or combined genetic elements targeted by the biomarker: ", clean_sig_ids),
              paste0("- The TCGA cancer type abbreviation representing the localized tissue context: ", f_ctab, " (", f_cancer_full, ")"),
              paste0("- Regulated Cell Death (RCD) pathways associated with the target: ", f_rcd),
              paste0("- Spearman Correlation Sign indicating a positive or negative correlation with the phenotype: ", f_corr, " correlation with ", f_pheno_full),
              paste0("- The clinical phenotype correlated with the signature: ", f_pheno_full),
              paste0("- The categorical expression profile of the target in tumor versus normal tissues: ", f_tnc),
              paste0("- The exact number of biological sequence components (e.g. transcript isoforms or mutation sites) in the signature: ", f_elements_count),
              paste0("- The summarized prognostic value combining statistical hazard models: ", f_combined_outcome)
            )
          } else if (block_name == "Interaction Intelligence Block") {
            facts <- c(
              paste0("- Primary / Partner Role: ", f_interaction_role),
              paste0("- Selected survival metric for interaction assessment: ", selected_metric),
              paste0("- Total significant trans-signature interactions in this cohort: ", f_total_interactions)
            )
            if (f_synergy_count > 0) facts <- c(facts, paste0("- Synergy (Hazard Amplification) interactions: ", f_synergy_count))
            if (f_antagonism_count > 0) facts <- c(facts, paste0("- Antagonism (Rescue/Protective) interactions: ", f_antagonism_count))
            if (f_bifurcation_count > 0) facts <- c(facts, paste0("- Context-Dependent Bifurcation interactions: ", f_bifurcation_count))
            facts <- c(facts, paste0("- Modulated molecular layers: ", f_modulated_layers))
          } else {
            facts <- c(
              paste0("- Target biomarker context: The ", sanitize_nomenclature_for_llm(f_nom), " multi-omic signature evaluated in ", f_cancer_full, "."),
              paste0("- Selected clinical outcome metric: ", selected_metric),
              paste0("- The total number of machine learning algorithms deployed for validation: ", f_total_algos),
              paste0("- The specific clinical patient cohorts where the signature was successfully validated: ", f_cohorts),
              paste0("- Validation machine learning algorithms: RSF, XGBoost, Boruta, and MTLR")
            )
          }
        }
        return(paste(facts, collapse = "\n"))
      }

      # Prepare grammar context based on element count
      element_plurality <- if(!is.na(suppressWarnings(as.numeric(f_elements_count))) && as.numeric(f_elements_count) == 1) {
        "SINGULAR (e.g., 'this element is', 'the target exhibits')"
      } else {
        "PLURAL (e.g., 'these elements are', 'the targets exhibit')"
      }

      # Override for CpG Methylation to prevent "single CpG site" hallucinations
      if (grepl("CpG|Methylation", f_bio, ignore.case = TRUE)) {
          element_plurality <- paste0(element_plurality, ", BUT CRITICALLY: because this is a Methylation signature, you MUST refer to the biology as 'methylation states at CpG sites' (plural) encoding the underlying gene. NEVER use the phrase 'a single CpG site'")
      }

      # 1. Compile facts for Population Information Block
      pop_dados_text <- compile_facts_for_block("Population Information Block")

      strict_nomenclature_rule_interp <- "STRICT NOMENCLATURE RULE: You MUST NOT use, mention, or display ANY part of the signature nomenclatures (e.g., THYM-1460.6.3.N.2.35.5.2.3.3), their abbreviated forms (e.g., THYM-1460, LUAD-1883, LUAD-636), or their dot-prefixed surrogate forms (e.g., .5.3.2.4.14.2.4.1) anywhere in your clinical narrative. These technical provenance identifiers belong exclusively in the audit section. Whenever a signature contributes to the interpretation, you MUST automatically decode it and discuss ONLY the constituent biological gene (e.g., EMP1, ACTA2) and its mechanisms, clinical relevance, and therapeutic associations. NEVER use the phrase 'Signature LUAD-...' or any similar identifier. NEVER translate or alter the provided Gene Symbols. Do not use ellipses to truncate any information. CRITICAL OMIC TOKEN PROHIBITION: The numeric tokens embedded in nomenclature (e.g., .5, .6, .7) are categorical omic-layer identifier codes (Protein, Mutation, CNV, miRNA, Transcript Isoform, mRNA Expression, CpG Methylation) - they are NEVER quantities, counts, dimensions, or layers. You MUST NOT claim a signature spans '6 omic layers' or involves 'seven molecular dimensions' based on these identifier codes."

      nomenclature_quarantine_checklist_interp <- "\n\n--- ⚠️ PRE-OUTPUT NOMENCLATURE QUARANTINE - EXECUTE THIS SELF-AUDIT BEFORE RELEASING YOUR RESPONSE ⚠️ ---\n\nYou MUST scan your COMPLETE output for the following FORBIDDEN patterns and REMOVE them before finalizing:\n\nFAILURE CHECK #1 - FULL SIGNATURE NOMENCLATURES: Scan for ANY string matching the pattern [A-Z]+-\\d+\\.\\d+ (e.g., READ-311.6.3.P.3.2.2.2.4.3, THYM-1460.6.3.N.2.35.5.2.3.3). If found, DELETE the entire nomenclature string. Replace with ONLY the decoded gene symbol (e.g., CHEK1, HJURP, AMIGO2).\nFAILURE CHECK #2 - ABBREVIATED NOMENCLATURES: Scan for strings like 'READ-311', 'LUAD-1883', or any [A-Z]+-\\d+ prefix. If found, DELETE.\nFAILURE CHECK #3 - CANCER-PREFIXED GENE REFERENCES: The pattern 'READ CHEK1' or 'The READ-311... signature' is FORBIDDEN. Write simply 'CHEK1' or 'the CHEK1-associated signature'.\nFAILURE CHECK #4 - OMIC TOKEN COUNTING: You MUST NOT state or imply that a signature spans 'X omic layers' or 'Y molecular dimensions' based on numeric tokens (.1-.7) in the nomenclature. These are categorical codes, not quantities.\nFAILURE CHECK #5 - DOT-PREFIXED SURROGATE NOMENCLATURES: Scan for ANY string matching the pattern .X.X.X.X... (e.g., .5.3.2.4.14.2.4.1, .6.3.3.30.30.2.4.2, .5.2.2.4.4.4.4.1). These are surrogate naming conventions — numeric dot-separated signature identifiers without a cancer-cohort prefix. If found, DELETE the entire dotted string. NEVER write sentences like 'Signature .5.3.2.4.14.2.4.1 (Transcript, Necrosis)...' — instead write 'the C1QTNF7-associated Transcript Isoform signature (associated with Necrosis)...'.\n\nIf your output contains ANY of the above violations, you MUST rewrite the offending sentences BEFORE releasing your response. The audit payload section handles provenance; your narrative must contain ONLY gene symbols and biological mechanisms.\n\n--- END NOMENCLATURE QUARANTINE CHECKLIST ---\n"

      system_prompt_pop <- paste0(
        "You are an expert clinical molecular oncologist. Your role is to interpret multi-omic regulated cell death (RCD) signatures and write a concise, precise, human-like diagnostic paragraph for the population baseline profile.\n",
        "STRICT CONSTRAINTS:\n",
        "1. NO CANCER DEFINITIONS: Do NOT define, explain, or describe the cancer type itself. Simply mention the specific cancer type name and abbreviation provided in the evidence.\n",
        "2. STRICT EXPRESSION VS. CORRELATION LOGIC: The Tumor Expression status (e.g., 'Unchanged', 'Overexpressed') refers to tumor vs. normal medians. The correlation with the phenotype (e.g., TMB, TSM) relies on continuous expression variance within the cohort. Do NOT use words like 'presence', 'absence', 'mutation', or 'genetic alteration' to explain this correlation. You must state that 'expression levels' correlate positively/negatively with the phenotype.\n",
        "3. NO RCD REDUNDANCY OR SPECULATION: State the associated pathways directly (e.g., 'linked to Necrosis'). Do not redundantly state 'associated with an RCD form'. Do NOT speculate or extrapolate that high or low signature expression alters the rate of the pathway.\n",
        "4. NO SURVIVAL/RISK SPECULATION FOR HRC/SMC: Do NOT speculate, conjecture, or predict whether the signature correlates with increased/decreased survival or clinical risk based on the HRC/SMC reference contexts. You MUST, however, report the official Combined Outcome prognostic value (e.g. Risky, Protective) as provided in the evidence.\n",
        "5. NO CIRCUMLOCUTUARY CODING REFERENCES: Do NOT output phrases like 'nomenclature code', 'index code', or refer to raw integers/codes (like code 1, code 7, 71, etc.) under any circumstances. CRITICALLY: omic-layer tokens (.1, .2, .3, .4, .5, .6, .7) embedded in signature IDs are categorical identifier codes (Protein, Mutation, CNV, miRNA, Transcript Isoform, mRNA Expression, CpG Methylation respectively) - they are NOT quantities, counts, dimensions, or layers. Never state or imply that a signature spans '6 omic layers' or '7 molecular dimensions' based on a numeric token. The numbers inside the signature string are not to be extracted or interpreted as magnitudes.\n",
        "6. NO SINGLE CpG SITE CLAIMS: For signatures in the CpG Methylation layer, you MUST describe the targets as a set of CpG methylation sites or methylation states rather than referring to them as a single CpG site or one single site.\n",
        "7. EXACT MATH AVOIDANCE: When mentioning the genes in the signature, if you choose to list a subset of them, DO NOT attempt to calculate or state the number of 'additional genes'. Simply list the subset and append 'among others' or just state the exact total number of genes provided in the data. Never write 'and X additional genes' as it frequently leads to mathematical hallucinations.\n",
        "8. TRANSCRIPT AND miRNA FRACTION DECODING - MANDATORY: When the evidence includes parenthetical fraction notation (e.g., ADAMTS7(1/5), MIR1307(1/1)), you MUST rewrite it in explanatory prose, NOT as bare 'GENE(X/Y)' identifiers. For transcript signatures: narrate each fraction as 'X of Y annotated [GENE] isoforms' (e.g., '1 of 5 annotated ADAMTS7 transcript isoforms') and immediately follow with a sentence explaining this means the prognostic signal is driven by specific transcript variants, not global gene-level expression. For miRNA signatures: narrate each fraction as 'X of Y mature miRNA species decoded from [MIR] locus' and explain the signature captures specific mature regulatory miRNA molecules. You MUST NEVER output a raw parenthetical fraction like 'ADAMTS7 (1/5)' without translating it into its biological meaning in the same sentence. This is a HARD constraint - failure to do so invalidates the narrative.\n",
        "9. NOMENCLATURE QUARANTINE — IDENTIFIER STRIPPING: You MUST NEVER output any complete, partial, abbreviated, or dot-prefixed surrogate signature identifier anywhere in your narrative. Signature nomenclatures (e.g., COAD-49.6.3.N.3.35.35.3.2.1), their abbreviated forms (e.g., COAD-49, LUAD-1883), and dot-prefixed surrogates (e.g., .5.3.2.4.14.2.4.1) are FORBIDDEN. Refer to signatures ONLY by their decoded gene content — e.g., 'the CHEK1-associated Bulk mRNA Expression signature' — never by any form of their technical identifier.\n",
        "STYLE RULES:\n",
        "- Write ONLY a single continuous, flowing paragraph. Do NOT use bullet points, lists, or dashes.\n",
        "- Do NOT use bold (**text**) or italics (*text*).\n",
        "- Do NOT include headers/titles or use variable labels as prefixes. Integrate the facts naturally into fluid clinical prose.",
        llm_glossary,
        decoded_element_preservation_governance,
        omic_layer_terminology_governance,
        "\n\n", strict_nomenclature_rule_interp, "\n\n",
        nomenclature_quarantine_checklist_interp
      )

      user_prompt_pop <- paste0(
        "Generate a clinical diagnostic narrative paragraph based strictly on this population evidence:\n\n",
        pop_dados_text,
        "\n\nDIRECTIONS:\n",
        "Adopt a fluid, consultative clinical voice. Synthesize the data logically rather than mechanically listing facts. You MUST observe the following grammar rule: Because the signature contains ", f_elements_count, " element(s), you must use strictly ", element_plurality, " grammar when referring to the targeted biology.\n",
        "1. Weave the specific cancer type together with the targeted biology (including the total number of elements and distinct genes) and their associated Regulated Cell Death pathways into a cohesive introductory context. Do not use redundant phrasing like 'associated with a regulated cell death form, specifically apoptosis'; simply state 'associated with Apoptosis'.\n",
        "2. In a fluid transition, connect the tumor vs. normal expression status with its resulting phenotypic correlation direction (positive or negative). Explain this correlation strictly as a relationship between continuous expression levels and the phenotypic score (e.g., 'while expression is unchanged relative to normal tissues, higher expression levels within the tumor correlate with increased TMB scores'). Do NOT use the words 'presence', 'absence', or 'alteration'.\n",
        "3. Conclude by smoothly explaining how this signature establishes a baseline population reference profile for hazard and survival modeling, culminating in the official combined prognostic outcome value (e.g., Risky, Protective).\n",
        "4. Note: For CpG Methylation signatures, ensure targets are described naturally as a set of methylation states, not a single site. Do NOT output raw index numbers or nomenclature codes.\n",
        "5. CRITICAL - FRACTION DECODING: If the evidence contains transcript or miRNA fractions (e.g., ADAMTS7(1/5), MIR1307(1/1)), you MUST translate them in-line: write 'X of Y annotated [GENE] isoforms' rather than 'GENE (X/Y)'. Then you MUST add one sentence explaining that the prognostic signal arises from specific transcript variants (not global gene expression) or specific mature miRNA species (for miRNA). Never output a raw '(X/Y)' without immediately providing its biological meaning. For example, instead of 'ADAMTS7 (1/5), C1QTNF5 (1/4)' write '1 of 5 annotated ADAMTS7 transcript isoforms, 1 of 4 annotated C1QTNF5 isoforms... indicating that the prognostic signal is driven by specific transcript variants rather than global gene-level expression.'\n",
        "6. CRITICAL - TOKEN .6 MUST BE READ AS BULK mRNA: When the omic layer is mRNA Expression (.6), you MUST describe the signature as 'Bulk mRNA Expression signature', 'Gene-Level mRNA Expression signature', or 'Bulk Transcriptomic Expression signature'. You MUST NEVER use bare 'mRNA signature', 'mRNA element', 'mRNA target', or 'mRNA transcript'. The word 'transcript' is reserved for Token .5 (Transcript Isoform) and MUST NEVER be applied to Token .6. The word 'mRNA' alone without 'Bulk' or 'Gene-Level' qualifier is FORBIDDEN for Token .6 signatures. This distinguishes .6 (Bulk mRNA) from .5 (Transcript Isoform)."
      )

      # 2. Compile facts for Interaction Intelligence Block
      interaction_dados_text <- compile_facts_for_block("Interaction Intelligence Block")

      system_prompt_interaction <- paste0(
        "You are an expert clinical systems biologist and molecular pathologist. Your role is to explain the multi-omic network topology and context-dependency of a prognostic signature.\n",
        "STRICT CONSTRAINTS:\n",
        "1. DO NOT PARSE NOMENCLATURE CODES: The partner signatures are provided as long alphanumeric ID strings (e.g., LGG-1538.7.3...). Do NOT attempt to interpret the numbers inside these strings. The embedded numeric tokens (e.g., .1, .2, .3, .4, .5, .6, .7) are strictly CATEGORICAL IDENTIFIER CODES. Decode them ONLY via this dictionary: .1=Protein, .2=Mutation, .3=CNV, .4=miRNA, .5=Transcript Isoform, .6=mRNA Expression, .7=CpG Methylation. The number itself carries NO quantitative meaning - .6 does NOT mean 'six layers', .7 does NOT mean 'seven dimensions', .5 does NOT mean 'five modalities'. You must NEVER extract numbers from these strings to hallucinate quantities, dimensionalities, or biological attributes. Do not recite the raw alphanumeric ID strings in your output; instead, refer to them functionally as 'partner signatures' or by their biological targets if provided.\n",
        "2. NO SINGLE CpG SITE CLAIMS: For any CpG Methylation layer targets, describe them collectively as 'methylation states at CpG sites' encoding the gene, NEVER 'a single CpG site'.\n",
        "3. NOMENCLATURE QUARANTINE — IDENTIFIER STRIPPING: You MUST NEVER output any complete, partial, abbreviated, or dot-prefixed surrogate signature identifier anywhere in your narrative. Signature nomenclatures (e.g., COAD-49.6.3.N.3.35.35.3.2.1), their abbreviated forms (e.g., COAD-49, LUAD-1883), and dot-prefixed surrogates (e.g., .5.3.2.4.14.2.4.1) are FORBIDDEN. Refer to signatures ONLY by their decoded gene content — e.g., 'the CHEK1-associated Bulk mRNA Expression signature' — never by any form of their technical identifier.\n",
        "STYLE RULES:\n",
        "- Write ONLY a single continuous, flowing paragraph. Do NOT use bullet points, numbered lists, or dashes.\n",
        "- Do NOT use bold (**text**) or italics (*text*).\n",
        "- Do NOT include any headers or titles.\n",
        "- Integrate the details naturally into clinical prose.",
        llm_glossary,
        "\n\n", strict_nomenclature_rule_interp, "\n\n",
        nomenclature_quarantine_checklist_interp
      )

      user_prompt_interaction <- paste0(
        "Generate an interaction intelligence narrative paragraph based strictly on this topological evidence:\n\n",
        interaction_dados_text,
        "\n\nDIRECTIONS:\n",
        "Adopt the authoritative voice of an expert systems biologist presenting to a tumor board.\n"
      )

      if(f_total_interactions > 0) {
        user_prompt_interaction <- paste0(user_prompt_interaction,
          "CRITICAL SYSTEM INSTRUCTION: You are being provided with explicitly validated interaction topologies. YOU MUST NEVER describe this signature as an 'independent univariate driver'.\n",
          "1. Smoothly transition by stating that this signature does NOT operate in isolation, but exhibits significant context-dependency.\n",
          "2. Explain its plasticity by describing how it is modulated by its explicitly named interacting partner signatures. Emphasize that these partner signatures exert the modulatory (synergistic, antagonistic, or bifurcating) effects on this primary driver.\n",
          "3. Highlight how its prognostic value is dynamically modulated by emphasizing ONLY the interaction types (Synergy, Antagonism, or Context-Dependent Bifurcation) that are explicitly listed in the evidence. Do NOT mention or assume interaction types that are not listed.\n",
          "4. Briefly mention the broader molecular layers it interacts with to emphasize its cross-omic reach."
        )
      } else {
        user_prompt_interaction <- paste0(user_prompt_interaction,
          "Write exactly one clear, concise sentence stating that this signature operates primarily as an independent univariate driver without any significant downstream trans-signature molecular dependencies in this specific cohort."
        )
      }

      # 3. Compile facts for Personalized Prediction Block
      pred_dados_text <- compile_facts_for_block("Personalized Prediction Block")

      system_prompt_pred <- paste0(
        "You are an expert clinical molecular oncologist. Your role is to describe the machine learning validation of a multi-omic prognostic signature and translate this validation into patient survival trajectory modeling.\n",
        "STRICT CONSTRAINTS:\n",
        "1. EXACT MATH AVOIDANCE: When mentioning the genes in the signature, if you choose to list a subset of them, DO NOT attempt to calculate or state the number of 'additional genes'. Simply list the subset and append 'among others' or just state the exact total number of genes provided in the data. Never write 'and X additional genes' as it frequently leads to mathematical hallucinations.\n",
        "2. NOMENCLATURE QUARANTINE — IDENTIFIER STRIPPING: You MUST NEVER output any complete, partial, abbreviated, or dot-prefixed surrogate signature identifier anywhere in your narrative. Signature nomenclatures (e.g., COAD-49.6.3.N.3.35.35.3.2.1), their abbreviated forms (e.g., COAD-49, LUAD-1883), and dot-prefixed surrogates (e.g., .5.3.2.4.14.2.4.1) are FORBIDDEN. Refer to signatures ONLY by their decoded gene content — e.g., 'the CHEK1-associated Bulk mRNA Expression signature' — never by any form of their technical identifier.\n",
        "STYLE RULES:\n",
        "- Write ONLY a single continuous, flowing paragraph. Do NOT use bullet points, numbered lists, or dashes.\n",
        "- Do NOT use bold (**text**) or italics (*text*).\n",
        "- Do NOT include any headers or titles.\n",
        "- Do NOT use variable labels as paragraph prefixes. Integrate the details naturally into clinical prose.",
        llm_glossary,
        "\n\n", strict_nomenclature_rule_interp, "\n\n",
        nomenclature_quarantine_checklist_interp
      )

      user_prompt_pred <- paste0(
        "Generate a patient survival trajectory narrative paragraph based strictly on this machine learning validation evidence:\n\n",
        pred_dados_text,
        "\n\nDIRECTIONS:\n",
        "Adopt the authoritative, synthesizing voice of a senior attending oncologist delivering a summary to a tumor board.\n",
        "1. Write this as a clinical concluding thought. Smoothly reference the biomarker in its cancer context (e.g., 'To translate this molecular profile into clinical practice...', or 'This specific signature in ", f_cancer_full, "...').\n",
        "2. Emphasize how the rigorous multi-algorithm validation (RSF, XGBoost, Boruta, and MTLR) across the specified cohorts provides robust predictive power.\n",
        "3. Explain that this validated framework is utilized to define actionable individual patient survival trajectories in precision oncology, allowing clinicians to dynamically differentiate between lethal accelerating hazard and protective reversal hazard outcomes on a per-patient basis."
      )

      # --- ASYNC LLM DISPATCH: Capture config and messages, run non-blocking ---
      status <- check_llm_status()
      if (!status$success) {
        session$sendCustomMessage("hide_spinner", list())
        rv_llm_text(paste0("ERROR: ", status$message))
        return()
      }
      cfg <- capture_llm_config()
      start_t <- Sys.time()

      msgs_pop  <- list(list(role = "system", content = system_prompt_pop), list(role = "user", content = user_prompt_pop))
      msgs_int  <- list(list(role = "system", content = system_prompt_interaction), list(role = "user", content = user_prompt_interaction))
      msgs_pred <- list(list(role = "system", content = system_prompt_pred), list(role = "user", content = user_prompt_pred))
      sid <- my_session_id

      # Run all 3 DeepSeek API calls in a single background future (non-blocking)
      future::future({
        pop_response  <- send_llm_request(msgs_pop,  cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url)
        int_response  <- send_llm_request(msgs_int,  cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url)
        pred_response <- send_llm_request(msgs_pred, cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url)
        list(pop = pop_response, int = int_response, pred = pred_response,
             elapsed = round(as.numeric(difftime(Sys.time(), start_t, units = "secs")), 1))
      }, seed = TRUE) %...>% (function(res) {
        # --- Post-processing & reactive updates (back in main Shiny session) ---
        release_queue(sid)
        session$sendCustomMessage("hide_spinner", list())

        pop_response  <- res$pop
        int_response  <- res$int
        pred_response <- res$pred
        elapsed_s     <- res$elapsed

        log_file <- "llm_performance_log.csv"
        new_row <- data.frame(Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Module = "Phase_I_II_Interpreter", Time_secs = elapsed_s, stringsAsFactors = FALSE)
        if (!file.exists(log_file)) write.csv(new_row, log_file, row.names = FALSE) else write.table(new_row, log_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)

        time_str <- paste0("Automatically generated on ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), " (Processing Time: ", elapsed_s, "s)")
        rv_llm_time(time_str)

        if (is.null(pop_response) || nchar(pop_response) < 10 || is.null(pred_response) || nchar(pred_response) < 10 || is.null(int_response) || nchar(int_response) < 10) {
           rv_llm_text("ERROR: AI generation failed to produce a valid response.")
        } else {
           pop_response  <- gsub("(?s)^.*?</think>\\s*", "", pop_response, perl = TRUE)
           int_response  <- gsub("(?s)^.*?</think>\\s*", "", int_response, perl = TRUE)
           pred_response <- gsub("(?s)^.*?</think>\\s*", "", pred_response, perl = TRUE)
           pop_response  <- scrub_governance_violations(pop_response)
           int_response  <- scrub_governance_violations(int_response)
           pred_response <- scrub_governance_violations(pred_response)
           rv_llm_text(list(population = pop_response, interaction = int_response, prediction = pred_response))
        }
      }) %...!% (function(e) {
        release_queue(sid)
        session$sendCustomMessage("hide_spinner", list())
        err_msg <- paste0("ERROR: LLM request failed - ", conditionMessage(e))
        rv_llm_text(err_msg)
      })
      return(NULL)
    })

    output$diagnostic_report_ui <- renderUI({
      req(input$interpreter_signature_select)
      df <- interpreter_data()
      req(df)

      row <- df[df$Feature == input$interpreter_signature_select, ]
      if(nrow(row) == 0) return(tags$p("Signature not found.", style="color: #ef4444;"))

      llm_content <- rv_llm_text()
      llm_ui <- NULL
      if(!is.null(llm_content)) {
        if(is.character(llm_content) && startsWith(llm_content, "ERROR:")) {
          err_msg <- sub("^ERROR: ", "", llm_content)
          llm_ui <- tags$div(
            style="background: rgba(239, 68, 68, 0.1); border: 1px solid rgba(239, 68, 68, 0.3); border-radius: 8px; padding: 15px; margin-top: 20px;",
            tags$p(style="color: #f87171; font-size: 14px; margin: 0; font-weight: 500;",
                   bs_icon("exclamation-triangle-fill"), " ", err_msg)
          )
        } else if (is.list(llm_content)) {
          # Clean headers and formatting from individual blocks
          pop_paragraphs <- clean_llm_text_block(llm_content$population)
          pred_paragraphs <- clean_llm_text_block(llm_content$prediction)

          pop_ui_paragraphs <- lapply(pop_paragraphs, function(p) {
            tags$p(style="color: #e2e8f0; font-size: 15px; line-height: 1.7; margin-bottom: 12px;", p)
          })
          pop_panel <- tags$div(
            style="background: rgba(30, 41, 59, 0.4); border: 1px solid #334155; border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);",
            tags$div(
              style="display: flex; align-items: center; margin-bottom: 15px; border-bottom: 1px solid #334155; padding-bottom: 8px;",
              tags$span(style="color: #60a5fa; font-size: 1.2rem; margin-right: 10px;", bs_icon("people")),
              tags$h5(style="color: #60a5fa; font-weight: bold; margin: 0; font-size: 1.1rem; letter-spacing: 0.5px;", "Population Information Block")
            ),
            pop_ui_paragraphs
          )

          pred_ui_paragraphs <- lapply(pred_paragraphs, function(p) {
            tags$p(style="color: #e2e8f0; font-size: 15px; line-height: 1.7; margin-bottom: 12px;", p)
          })
          pred_panel <- tags$div(
            style="background: rgba(30, 41, 59, 0.4); border: 1px solid #334155; border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);",
            tags$div(
              style="display: flex; align-items: center; margin-bottom: 15px; border-bottom: 1px solid #334155; padding-bottom: 8px;",
              tags$span(style="color: #c084fc; font-size: 1.2rem; margin-right: 10px;", bs_icon("person-check")),
              tags$h5(style="color: #c084fc; font-weight: bold; margin: 0; font-size: 1.1rem; letter-spacing: 0.5px;", "Personalized Prediction Block")
            ),
            if(length(pred_ui_paragraphs) > 0) pred_ui_paragraphs else tags$p(style="color: #94a3b8; font-style: italic;", "No prediction narrative generated.")
          )

          int_paragraphs <- clean_llm_text_block(llm_content$interaction)
          int_ui_paragraphs <- lapply(int_paragraphs, function(p) {
            tags$p(style="color: #e2e8f0; font-size: 15px; line-height: 1.7; margin-bottom: 12px;", p)
          })
          int_panel <- tags$div(
            style="background: rgba(30, 41, 59, 0.4); border: 1px solid #334155; border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);",
            tags$div(
              style="display: flex; align-items: center; margin-bottom: 15px; border-bottom: 1px solid #334155; padding-bottom: 8px;",
              tags$span(style="color: #34d399; font-size: 1.2rem; margin-right: 10px;", bs_icon("diagram-3")),
              tags$h5(style="color: #34d399; font-weight: bold; margin: 0; font-size: 1.1rem; letter-spacing: 0.5px;", "Trans-Signature Interaction Intelligence")
            ),
            if(length(int_ui_paragraphs) > 0) int_ui_paragraphs else tags$p(style="color: #94a3b8; font-style: italic;", "No interaction narrative generated.")
          )

          time_ui <- if(!is.null(rv_llm_time())) {
            tags$div(style="font-size:10px; color:#94a3b8; text-align:right; margin-top:5px; margin-bottom:15px;", rv_llm_time())
          } else { NULL }

          llm_ui <- tags$div(
            style="margin-top: 25px;",
            tags$h5(style="color: #34d399; font-weight: bold; margin-bottom: 15px; font-size: 1rem;", "✦ AI Synthesized Narrative"),
            pop_panel,
            int_panel,
            pred_panel,
            time_ui
          )
        }
      }

      tags$div(
        tags$h4(style="color: #60a5fa; font-weight: bold; border-bottom: 1px solid #334155; padding-bottom: 10px; margin-bottom: 20px;",
                bs_icon("clipboard2-pulse"), " Clinical Diagnostic Report"),
        llm_ui
      )
    })

    # ==============================================================================
    # TUMOR BOARD - PERSONALIZED SHAP DECODING LLM ENGINE
    # ==============================================================================
    rv_shap_llm_text <- reactiveVal(NULL)
    rv_shap_llm_payload <- reactiveVal(NULL)
    rv_shap_llm_time <- reactiveVal(NULL)
    rv_shap_audit <- reactiveVal(NULL)
    rv_active_patient_profile <- reactiveVal(NULL)
    rv_active_patient_profile_audit <- reactiveVal(NULL)
    rv_active_patient_nomenclatures <- reactiveVal(NULL)

    rv_pharma_llm_text <- reactiveVal(NULL)
    rv_pharma_llm_payload <- reactiveVal(NULL)
    rv_pharma_llm_time <- reactiveVal(NULL)
    rv_pharma_audit <- reactiveVal(NULL)
    rv_patient_phenotype <- reactiveVal(NULL)

    observeEvent(input$tumor_shap_patient, {
       rv_shap_llm_text(NULL)
       rv_shap_llm_payload(NULL)
       rv_shap_audit(NULL)
       rv_active_patient_profile(NULL)
       rv_active_patient_profile_audit(NULL)
       rv_pharma_llm_text(NULL)
       rv_pharma_llm_payload(NULL)
       rv_pharma_audit(NULL)
    })

    observeEvent(rv_trigger_shap_llm(), {
      req(rv_trigger_shap_llm() > 0)
      on.exit({
        release_queue(my_session_id)
      })

      tryCatch({
        df <- tumor_shap_data()
        if (is.null(df)) stop("Phase III SHAP matrix not loaded for this cohort.")
        pat_df <- df[df$Sample_ID == input$tumor_shap_patient, ]
        if (nrow(pat_df) == 0) stop("Selected patient not found in the matrix.")

        # Dynamically map the probability columns in case they are prefixed (e.g. XGBoost_Prob_1Yr)
        cols <- names(pat_df)

        get_best_prob_col <- function(cols, suffix) {
            matches <- cols[grepl(suffix, cols)]
            if (length(matches) == 0) return(NULL)
            mvl <- matches[grepl("MVL|SuperLearner", matches, ignore.case = TRUE)]
            if (length(mvl) > 0) return(mvl[1])
            xgb <- matches[grepl("XGBoost", matches, ignore.case = TRUE)]
            if (length(xgb) > 0) return(xgb[1])
            return(matches[1])
        }

        c1 <- get_best_prob_col(cols, "Prob_1Yr")
        c3 <- get_best_prob_col(cols, "Prob_3Yr")
        c5 <- get_best_prob_col(cols, "Prob_5Yr")
        r_cols <- cols[grepl("Risk", cols)]

        pat_1yr <- if (length(c1) > 0) as.numeric(pat_df[[c1[1]]][1]) else NA
        pat_3yr <- if (length(c3) > 0) as.numeric(pat_df[[c3[1]]][1]) else NA
        pat_5yr <- if (length(c5) > 0) as.numeric(pat_df[[c5[1]]][1]) else NA

        # The data array is natively exported from Phase III.
        # Survival metrics (OS/DSS) are natively S(t) and Event metrics (DFI/PFI) are natively Cumulative Incidence (1 - S(t)).
        # No further probability inversions are required.

        pat_1yr <- if (!is.na(pat_1yr)) pat_1yr else "N/A"
        pat_3yr <- if (!is.na(pat_3yr)) pat_3yr else "N/A"
        pat_5yr <- if (!is.na(pat_5yr)) pat_5yr else "N/A"

        exclude_cols <- c("Sample_ID", "Status", "Time", "Model", "Metric", "Cohort", "Cancer", "Endpoint", c1, c3, c5, r_cols)

        # 1. DYNAMIC ZIMA COHORT/DF RESOLUTION
        cohort_metric <- paste0(input$tumor_shap_cancer, "_", input$tumor_shap_metric)
        ml_models_dir <- file.path(ZIMA_ROOT, "PHASE_III_ML_Models")
        cohort_dirs <- list.dirs(ml_models_dir, recursive = FALSE, full.names = TRUE)
        target_dir <- cohort_dirs[grepl(paste0("^", cohort_metric, "_df[0-9]{3}$"), basename(cohort_dirs))]

        trajectory_csv_path <- ""
        if (length(target_dir) > 0) {
           xgboost_dir <- file.path(target_dir[1], "XGBoost")
           if (dir.exists(xgboost_dir)) {
               possible_files <- list.files(xgboost_dir, pattern = paste0("_Trajectory_", input$tumor_shap_patient, "_Trajectory_Data\\.csv$"), full.names = TRUE)
               if (length(possible_files) > 0) {
                   trajectory_csv_path <- possible_files[1]
               }
           }
        }

        patient_profile <- ""
        patient_nomenclatures <- c()
        patient_profile_audit <- ""

        # 2. EXTRACT EXACT PATIENT SHAP COORDINATES & DECODE
        if (file.exists(trajectory_csv_path)) {
            traj_df <- tryCatch(
              read.csv(trajectory_csv_path, stringsAsFactors = FALSE),
              error = function(e) {
                message("[WARN] Trajectory CSV for patient ", input$tumor_shap_patient, " is corrupt; skipping SHAP coordinate extraction.")
                data.frame()
              }
            )
            if (nrow(traj_df) == 0) { patient_profile <- "" } else {
            # Order by absolute SHAP value
            traj_df <- traj_df[order(abs(as.numeric(traj_df[[2]])), decreasing = TRUE), ]

            top_signatures <- head(traj_df[[1]], 5)
            signature_summaries <- c()
            signature_summaries_audit <- c()
            patient_nomenclatures <- c()
            sub_matrix <- raw_cohort_matrix[raw_cohort_matrix$CTAB == input$tumor_shap_cancer & raw_cohort_matrix$Metric == input$tumor_shap_metric, ]

            for (sig in top_signatures) {
                sig_genes <- c()
                sig_rcd <- c()
                sig_omic <- c()

                # Extract SHAP value and direction
                shap_val <- as.numeric(traj_df[traj_df[[1]] == sig, 2][1])
                if (input$tumor_shap_metric %in% c("OS", "DSS")) {
                    shap_dir <- ifelse(shap_val > 0, "Positive SHAP (Increases Mortality Risk/Lethal)", "Negative SHAP (Decreases Mortality Risk/Protective)")
                } else {
                    shap_dir <- ifelse(shap_val > 0, "Positive SHAP (Pro-Progression/Recurrence)", "Negative SHAP (Anti-Progression/Stabilizing)")
                }

                # Extract RCD form and Phenotype Correlation
                if (!is.null(table_s11_global)) {
                    match_row <- table_s11_global[table_s11_global$Feature == sig, ]
                    if (nrow(match_row) > 0) {
                        match_rcd <- match_row$`RCD form`[1]
                        if (!is.na(match_rcd) && match_rcd != "") {
                            sig_rcd <- match_rcd
                        }

                        match_layer <- match_row$`Biological Layer`[1]
                        if (!is.na(match_layer) && match_layer != "") {
                            sig_omic <- match_layer
                        }

                        # NOTE: SCS/Phenotype correlation is POPULATION-LEVEL data and is intentionally excluded
                        # from patient SHAP profiles to prevent ecological inference bias (see phenotype_correlation_rule).
                    }
                }

                # Extract Genes
                match_primary <- sub_matrix[sub_matrix$Primary_Signature == sig, "Decoded_Genetic_Element_Primary"]
                match_partner <- sub_matrix[sub_matrix$Signature_Partner == sig, "Decoded_Genetic_Element_Partner"]
                sig_genes_raw <- unique(c(match_primary, match_partner))
                sig_genes_raw <- sig_genes_raw[!is.na(sig_genes_raw) & sig_genes_raw != ""]

                # Split multi-gene strings by "+" to process each GENE(X/Y) individually
                # e.g., "(EGFL7(1/7)) + (PLOD3(1/16)) + (SH3BP2(1/31))" becomes 3 separate elements
                sig_genes <- unique(unlist(strsplit(sig_genes_raw, "\\s*\\+\\s*")))

                # Enrich Genes — each gene gets its own isoform fraction from its own (X/Y) notation
                enriched_genes <- sapply(sig_genes, function(g) {
                    role <- "Biological role under investigation."
                    clean_g <- regmatches(g, regexpr("[A-Za-z0-9\\\\-]+", g))
                    if (length(clean_g) > 0) clean_g <- clean_g[[1]] else clean_g <- g

                    iso_info <- ""
                    if (grepl("\\\\([0-9]+/[0-9]+\\\\)", g)) {
                        iso_info <- gsub(".*?\\\\(([0-9]+)/([0-9]+)\\\\).*", "[NOTE: This specific SHAP coordinate maps to Transcript Isoform \\\\1 out of \\\\2 known transcripts for this gene.] ", g, perl = TRUE)
                    }

                    if (nrow(gene_roles_df) > 0) {
                        match_idx <- which(gene_roles_df$Gene == clean_g)
                        if (length(match_idx) > 0) {
                            role <- gene_roles_df$Biological_Role[match_idx[1]]
                        }
                    }
                    paste0(clean_g, " (", iso_info, role, ")")
                })

                # Compile specific signature summary (nomenclature stripped before LLM ingestion to prevent recitation)
                sig_sanitized <- sanitize_nomenclature_for_llm(sig)
                sig_summary <- paste0(
                    "- Signature: ", sig_sanitized, "\n",
                    "  > SHAP Impact: ", shap_dir, " (Value: ", round(shap_val, 4), ")\n",
                    "  > Omic Layer: ", ifelse(length(sig_omic) > 0, sig_omic, "Unknown"), "\n",
                    "  > Associated RCD Form: ", ifelse(length(sig_rcd) > 0, sig_rcd, "Unknown"), "\n",

                    "  > Encoded Gene Mechanics: ", ifelse(length(enriched_genes) > 0, paste(enriched_genes, collapse=" | "), "Unknown")
                )
                # AUDIT VERSION: Preserve full signature nomenclature for provenance traceability in audit display
                sig_summary_audit <- paste0(
                    "- Signature: ", sig, "\n",
                    "  > SHAP Impact: ", shap_dir, " (Value: ", round(shap_val, 4), ")\n",
                    "  > Omic Layer: ", ifelse(length(sig_omic) > 0, sig_omic, "Unknown"), "\n",
                    "  > Associated RCD Form: ", ifelse(length(sig_rcd) > 0, sig_rcd, "Unknown"), "\n",

                    "  > Encoded Gene Mechanics: ", ifelse(length(enriched_genes) > 0, paste(enriched_genes, collapse=" | "), "Unknown")
                )
                signature_summaries <- c(signature_summaries, sig_summary)
                signature_summaries_audit <- c(signature_summaries_audit, sig_summary_audit)
                patient_nomenclatures <- c(patient_nomenclatures, sig)
            }

            if (length(signature_summaries) > 0) {
                patient_profile <- paste0(
                    "The patient's clinical trajectory is mathematically driven by the following Top ", length(top_signatures), " unique molecular signatures extracted from their personalized XGBoost SHAP geometry:\n\n",
                    paste(signature_summaries, collapse = "\n\n"),
                    "\n\nBIOLOGICAL CONTEXT: Each signature is a unique multi-omic coordinate. Note that different signatures may contain different omic layers (e.g., RNA vs Methylation) of the same gene, allowing the tumor to utilize the same genetic element in opposing directions simultaneously. You MUST NOT use the signature nomenclature to infer the patient's individual biology; rely strictly on the explicit High/Intermediate/Low TSM, TMB, and MSI classifications provided."
                )
                # AUDIT VERSION: Preserve full signature nomenclatures for provenance traceability
                patient_profile_audit <- paste0(
                    "The patient's clinical trajectory is mathematically driven by the following Top ", length(top_signatures), " unique molecular signatures extracted from their personalized XGBoost SHAP geometry:\n\n",
                    paste(signature_summaries_audit, collapse = "\n\n"),
                    "\n\nBIOLOGICAL CONTEXT: Each signature is a unique multi-omic coordinate. Note that different signatures may contain different omic layers (e.g., RNA vs Methylation) of the same gene, allowing the tumor to utilize the same genetic element in opposing directions simultaneously. You MUST NOT use the signature nomenclature to infer the patient's individual biology; rely strictly on the explicit High/Intermediate/Low TSM, TMB, and MSI classifications provided."
                )
            }
        }  # closes else (nrow(traj_df) == 0)
        }  # closes if(file.exists(trajectory_csv_path))

        # HALLUCINATION SHIELD FALLBACK
        if (patient_profile == "") {
             cohort_row <- raw_cohort_matrix[raw_cohort_matrix$CTAB == input$tumor_shap_cancer & raw_cohort_matrix$Metric == input$tumor_shap_metric, ]
             signature <- if(nrow(cohort_row) > 0) cohort_row$Primary_Signature[1] else "Unknown Signature"
             sig_fallback <- sanitize_nomenclature_for_llm(signature)
             patient_profile <- paste0("Specific personalized SHAP coordinates could not be located in ZIMA. The patient's clinical outcome is mathematically aligned with the following biological signature: ", sig_fallback, ".")
             patient_profile_audit <- paste0("Specific personalized SHAP coordinates could not be located in ZIMA. The patient's clinical outcome is mathematically aligned with the following biological signature: ", signature, ".")
        }

        # Synchronize exactly what the SHAP engine decoded for the Chat module
        rv_active_patient_profile(patient_profile)
        rv_active_patient_profile_audit(patient_profile_audit)
        rv_active_patient_nomenclatures(patient_nomenclatures)

        # --- PATIENT PHENOTYPE CONTEXT (TSM/TMB/MSI) FOR CLINICAL SYNTHESIS ---
        patient_pheno_info <- stemness_df %>% dplyr::filter(sample_id == input$tumor_shap_patient)
        if (nrow(patient_pheno_info) > 0) {
          rnass_val   <- patient_pheno_info$RNAss[1]
          ereg_val    <- patient_pheno_info$EREG.EXPss[1]
          rnass_cls   <- patient_pheno_info$RNAss_class[1]
          ereg_cls    <- patient_pheno_info$EREG_class[1]
          tmb_val     <- patient_pheno_info$Non_silent_per_Mb[1]
          tmb_cls     <- patient_pheno_info$TMB_class[1]
          msi_val     <- patient_pheno_info$Total_nb_MSI_events[1]
          msi_cls     <- patient_pheno_info$MSI_class[1]

          tmb_str <- if (is.na(tmb_val)) "TMB: No data available for this patient." else paste0("TMB (Non-silent/Mb): ", round(tmb_val, 3), " (", tmb_cls, ").")
          msi_str <- if (is.na(msi_val)) "MSI: No data available for this patient." else paste0("MSI (Total Events): ", round(msi_val, 3), " (", msi_cls, ").")

          patient_phenotype_context <- paste0(
            "\n\nPATIENT-SPECIFIC PHENOTYPE CLASSIFICATIONS (USE THESE EXACT VALUES FOR ALL INTERPRETATION):\n",
            "TSM: RNAss = ", round(rnass_val, 3), " (", rnass_cls, "), ",
            "EREG.EXPss = ", round(ereg_val, 3), " (", ereg_cls, ").\n",
            tmb_str, "\n",
            msi_str, "\n",
            "CRITICAL TSM ONTOLOGY NOTE: RNAss and EREG.EXPss are Tumor Stemness Measures (TSM) — phenotype-level indices derived from population-level transcriptomic programs. They are NOT measurements of individual genes. EREG.EXPss does NOT represent Epiregulin (EREG) gene expression. The 'EREG' substring is part of an acronym (Epigenetically Regulated), not a gene symbol. RNAss does NOT represent any specific RNA species. Interpret these variables EXCLUSIVELY as stemness-associated phenotype metrics: dedifferentiation, developmental plasticity, self-renewal potential, tumor heterogeneity.\n",
            "CRITICAL: You MUST incorporate these exact patient-level TSM, TMB, and MSI classifications into your clinical synthesis."
          )
        } else {
          patient_phenotype_context <- "\n\nPATIENT-SPECIFIC PHENOTYPE CLASSIFICATIONS: No phenotype data available for this patient."
        }
        # Cache for downstream modules (Chat)
        rv_patient_phenotype(patient_phenotype_context)

        # METRIC INTELLIGENCE CORE
        if(input$tumor_shap_metric %in% c("OS", "DSS")) {
            metric_context <- "CRITICAL CONTEXT: The chosen metric is a SURVIVAL metric. SURVIVAL MODEL DIRECTION: These are survival probabilities S(t) — HIGH values (>80%) mean FAVORABLE prognosis, LOW values (<50%) mean UNFAVORABLE. State whether the patient is on a LETHAL or PROTECTIVE clinical trajectory based ONLY on the provided probabilities. Do not use an arbitrary threshold to define lethality. For example, a survival probability remaining above 80% at 5 years indicates a PROTECTIVE and highly favorable trajectory. A trajectory should only be interpreted as LETHAL if the survival probabilities demonstrate a severe and progressive collapse (e.g., falling below 50-60%). Analyze the provided SHAP signatures in alignment with the true severity of the survival decline. Standard survival terminology (lethal, protective, mortality, survival) is appropriate for this endpoint."
            metric_label <- "Survival Probabilities"
        } else if(input$tumor_shap_metric == "PFI") {
            metric_context <- "CRITICAL CONTEXT: The chosen metric is Progression-Free Interval (PFI), an EVENT metric measuring disease progression. CUMULATIVE INCIDENCE MODEL: These are cumulative incidence probabilities (1−S(t)) — LOW values (<5%) mean FAVORABLE prognosis (negligible progression risk), HIGH values (>20% with escalation) mean UNFAVORABLE. State whether the patient is on an ADVERSE PROGRESSION, STABLE, or LOW-RISK PROGRESSION clinical trajectory based ONLY on the provided progression probabilities. Do not use an arbitrary threshold. For example, progression probabilities remaining below 5% at all time horizons (1yr, 3yr, 5yr) indicate a LOW-RISK PROGRESSION trajectory with negligible progression risk and a highly favorable prognosis. Progression probabilities in an intermediate band (e.g., 5-15%) suggest a STABLE but monitored trajectory. A trajectory should only be interpreted as ADVERSE PROGRESSION if the progression probabilities rise into a clinically concerning range (e.g., above 15-20%) and show meaningful escalation over the time horizon. Analyze the SHAP signatures in alignment with the true severity of the progression probability trajectory. Prefer progression-specific terminology: pro-progression (increasing hazard) / anti-progression or stabilizing (decreasing hazard). Avoid survival-framing terms (lethal, protective, mortality) and recurrence terminology since this is a progression endpoint. For NGF v2.6 dynamic tumor-state reasoning, use 'stress-adapted persistence state' instead of 'stress-adapted survival state'. Explain any complex interplay between local mathematical hazard and systemic MVL probability as tumor heterogeneity without calling them discordant."
            metric_label <- "Progression Probabilities"
        } else {
            metric_context <- "CRITICAL CONTEXT: The chosen metric is Disease-Free Interval (DFI), an EVENT metric measuring disease recurrence (new tumor events). CUMULATIVE INCIDENCE MODEL: These are cumulative incidence probabilities (1−S(t)) — LOW values (<5%) mean FAVORABLE prognosis (negligible recurrence risk), HIGH values (>20% with escalation) mean UNFAVORABLE. State whether the patient is on an ADVERSE RECURRENCE, STABLE, or LOW-RISK RECURRENCE clinical trajectory based ONLY on the provided recurrence probabilities. Do not use an arbitrary threshold. For example, recurrence probabilities remaining below 5% at all time horizons (1yr, 3yr, 5yr) indicate a LOW-RISK RECURRENCE trajectory with negligible recurrence risk and a highly favorable prognosis. Recurrence probabilities in an intermediate band (e.g., 5-15%) suggest a STABLE but monitored trajectory. A trajectory should only be interpreted as ADVERSE RECURRENCE if the recurrence probabilities rise into a clinically concerning range (e.g., above 15-20%) and show meaningful escalation over the time horizon. Analyze the SHAP signatures in alignment with the true severity of the recurrence probability trajectory. Prefer recurrence-specific terminology: pro-recurrence (increasing hazard) / anti-recurrence or stabilizing (decreasing hazard). Avoid survival-framing terms (lethal, protective, mortality) and progression terminology since this is a recurrence endpoint. For NGF v2.6 dynamic tumor-state reasoning, use 'stress-adapted persistence state' instead of 'stress-adapted survival state'. Explain any complex interplay between local mathematical hazard and systemic MVL probability as tumor heterogeneity without calling them discordant."
            metric_label <- "Recurrence Probabilities"
        }

        global_associativity <- "GLOBAL ASSOCIATIVITY RULE: The multi-omic signatures, SHAP values, and stemness correlations provided represent associative mathematical relationships, not proven causative biological pathways. You MUST frame all pharmacological interventions as targeting associative vulnerabilities, not definitively causative mechanisms."
        # CRIT-05 MITIGATION: ecological fallacy prevention rule injected into system prompt
        phenotype_correlation_rule <- "\n\n--- ECOLOGICAL FALLACY PREVENTION ---\n\nThe patient's TSM, TMB, and MSI phenotypes (High/Intermediate/Low) are INDIVIDUAL MEASUREMENTS from the stemness database. They are NOT population-level correlations. You MUST reason about the patient using ONLY their own measured values.\n\nFORBIDDEN PATTERNS (DELETE AND REWRITE IMMEDIATELY):\n  - 'The patient's Intermediate TSM may reflect the P-positive correlation...'\n  - 'Consistent with the N-negative population-level sign...'\n  - 'This aligns with the cohort-level association of TSM with survival...'\n  - 'Given the population-wide TMB correlation pattern...'\n  - 'The P-positive population sign suggests that this patient...'\n  - '...consistent with the negative population-level correlation...'\n\nPERMITTED PATTERNS:\n  - 'The patient's individual TSM measurement is Intermediate (RNAss = X), which individually...'\n  - 'The patient exhibits Low TMB (X Mut/Mb), which in this individual context may suggest...'\n\nPOPULATION-LEVEL TERMS YOU MUST NEVER USE in patient-specific reasoning:\n  'P-positive', 'N-negative', 'population-level correlation', 'cohort-level correlation',\n  'population sign', 'population-wide', 'cohort-wide', 'population-level TSM/TMB/MSI'\n\nPRE-OUTPUT SELF-CHECK: Scan every paragraph containing patient-specific markers. If ANY also contains population-level terms, REWRITE using only the patient's own measurements.\n\n--- END ECOLOGICAL FALLACY PREVENTION ---\n"

        strict_nomenclature_rule <- "STRICT NOMENCLATURE RULE: You MUST NOT use, mention, or display ANY part of the signature nomenclatures (e.g., THYM-1460.6.3.N.2.35.5.2.3.3), their abbreviated forms (e.g., THYM-1460, LUAD-1883, LUAD-636), or their dot-prefixed surrogate forms (e.g., .5.3.2.4.14.2.4.1) anywhere in your clinical narrative. These technical provenance identifiers belong exclusively in the audit section. Whenever a signature contributes to the interpretation, you MUST automatically decode it and discuss ONLY the constituent biological gene (e.g., EMP1, ACTA2) and its mechanisms, clinical relevance, and therapeutic associations. NEVER use the phrase 'Signature LUAD-...' or any similar identifier. NEVER translate or alter the provided Gene Symbols. Do not use ellipses to truncate any information. CRITICAL OMIC TOKEN PROHIBITION: The numeric tokens embedded in nomenclature (e.g., .5, .6, .7) are categorical omic-layer identifier codes (Protein, Mutation, CNV, miRNA, Transcript Isoform, mRNA Expression, CpG Methylation) - they are NEVER quantities, counts, dimensions, or layers. You MUST NOT claim a signature spans '6 omic layers' or involves 'seven molecular dimensions' based on these identifier codes."

        nomenclature_quarantine_checklist <- "\n\n--- ⚠️ PRE-OUTPUT NOMENCLATURE QUARANTINE - EXECUTE THIS SELF-AUDIT BEFORE RELEASING YOUR RESPONSE ⚠️ ---\n\nYou MUST scan your COMPLETE output for the following FORBIDDEN patterns and REMOVE them before finalizing:\n\nFAILURE CHECK #1 - FULL SIGNATURE NOMENCLATURES: Scan for ANY string matching the pattern [A-Z]+-\\d+\\.\\d+ (e.g., READ-311.6.3.P.3.2.2.2.4.3, THYM-1460.6.3.N.2.35.5.2.3.3). If found, DELETE the entire nomenclature string. Replace with ONLY the decoded gene symbol (e.g., CHEK1, HJURP, AMIGO2).\nFAILURE CHECK #2 - ABBREVIATED NOMENCLATURES: Scan for strings like 'READ-311', 'LUAD-1883', or any [A-Z]+-\\d+ prefix. If found, DELETE.\nFAILURE CHECK #3 - CANCER-PREFIXED GENE REFERENCES: The pattern 'READ CHEK1' or 'The READ-311... signature' is FORBIDDEN. Write simply 'CHEK1' or 'the CHEK1-associated signature'.\nFAILURE CHECK #4 - OMIC TOKEN COUNTING: You MUST NOT state or imply that a signature spans 'X omic layers' or 'Y molecular dimensions' based on numeric tokens (.1-.7) in the nomenclature. These are categorical codes, not quantities.\nFAILURE CHECK #5 - DOT-PREFIXED SURROGATE NOMENCLATURES: Scan for ANY string matching the pattern .X.X.X.X... (e.g., .5.3.2.4.14.2.4.1, .6.3.3.30.30.2.4.2, .5.2.2.4.4.4.4.1). These are surrogate naming conventions — numeric dot-separated signature identifiers without a cancer-cohort prefix. If found, DELETE the entire dotted string. NEVER write sentences like 'Signature .5.3.2.4.14.2.4.1 (Transcript, Necrosis)...' — instead write 'the C1QTNF7-associated Transcript Isoform signature (associated with Necrosis)...'.\n\nIf your output contains ANY of the above violations, you MUST rewrite the offending sentences BEFORE releasing your response. The audit payload section handles provenance; your narrative must contain ONLY gene symbols and biological mechanisms.\n\n--- END NOMENCLATURE QUARANTINE CHECKLIST ---\n"

        mrna_terminology_quarantine_checklist <- "\n\n--- ⚠️ PRE-OUTPUT mRNA TERMINOLOGY QUARANTINE - EXECUTE THIS SELF-AUDIT BEFORE RELEASING YOUR RESPONSE ⚠️ ---\n\nYou MUST scan your COMPLETE output for FORBIDDEN bare mRNA references and FIX them before finalizing:\n\nFAILURE CHECK #1 - BARE 'mRNA layer': Replace 'HJURP (mRNA layer)' with 'HJURP (Bulk mRNA Expression layer)'. Replace 'mRNA layer' with 'Bulk mRNA Expression layer'.\nFAILURE CHECK #2 - BARE 'mRNA signature': Replace with 'Bulk mRNA Expression signature' or 'Gene-Level mRNA Expression signature'.\nFAILURE CHECK #3 - BARE 'mRNA element', 'mRNA target', or 'mRNA transcript': These are FORBIDDEN. Use 'Bulk mRNA Expression element/target' or delete 'mRNA transcript' entirely (it belongs to Token .5 Transcript Isoform).\nFAILURE CHECK #4 - Token .6 references: Any gene from the mRNA Expression omic layer (Token .6) MUST be described as 'Bulk mRNA Expression' or 'Gene-Level mRNA Expression'. The bare qualifier 'mRNA' standing alone next to a gene symbol - e.g., 'CHEK1 (mRNA)' - is FORBIDDEN. Write 'CHEK1 (Bulk mRNA Expression)'.\n\nIf your output contains ANY bare 'mRNA' qualifier not preceded by 'Bulk' or 'Gene-Level', you MUST fix it BEFORE releasing your response.\n\n--- END mRNA TERMINOLOGY QUARANTINE CHECKLIST ---\n"

        system_prompt_shap <- paste0(
        "=== GENE GOVERNANCE (READ FIRST) ===\n",
        "The patient payload below identifies the TOP 5 SIGNATURE-DRIVING GENES — the primary molecular drivers ",
        "from this patient's SHAP trajectory. Center your clinical synthesis on these driver genes. ",
        "You MAY discuss additional genes that are biologically connected to the driver genes ",
        "(e.g., pathway partners, downstream targets, interacting regulators). ",
        "You MUST NOT import canonical cancer genes (EGFR, TP53, TNF, BRAF, BRCA1, GPX4, CDK2, ",
        "SMAD2, FGFR3, ATM, APC, TRAF2, E2F1, RHOA, HSPD1, or any other gene) unless they appear ",
        "in the driver gene list or are directly biologically connected to a driver gene. ",
        "Do NOT borrow unrelated genes from your training knowledge. ",
        "Each unconnected gene mention will be audited as a GOVERNANCE VIOLATION.\n\n",
        "=== PATIENT IDENTIFIER RULE (READ SECOND) ===\n",
        "You MUST start your narrative by explicitly stating the Patient ID and the Cohort. ",
        "The first sentence MUST contain both identifiers. Omitting either is a GOVERNANCE FAILURE.\n\n",
        "CRITICAL TERMINOLOGY PRECONDITION - READ BEFORE WRITING ANY OUTPUT:

When you encounter a signature whose Omic Layer indicates mRNA (Token .6), you MUST write 'Bulk mRNA Expression' or 'Gene-Level mRNA Expression'. The bare word 'mRNA' standing alone - such as 'EFEMP2 (mRNA Expression)' or 'PGR (mRNA)' - is STRICTLY FORBIDDEN. You MUST write 'EFEMP2 (Bulk mRNA Expression)' or 'PGR (Gene-Level mRNA Expression)'. The qualifier 'mRNA' alone without 'Bulk' or 'Gene-Level' is prohibited in every sentence, every paragraph, and every section of your response. Likewise, any signature from the Transcript layer (Token .5) MUST be described as 'Transcript Isoform', never bare 'Transcript'. Execute this check mentally before outputting each sentence. There are NO exceptions.

", phenotype_correlation_rule, "

You are an expert clinical molecular oncologist analyzing a patient's multi-omic profile. You MUST start your narrative by explicitly stating the Patient ID (", input$tumor_shap_patient, ") and the Cohort (", input$tumor_shap_cancer, ") so the user knows exactly who you are talking about. Output a highly professional, fluid clinical synthesis in continuous paragraph form. ", metric_context, " \n\nCRITICAL ENSEMBLE ARCHITECTURE INSTRUCTION: The clinical probabilities you see are generated by a highly calibrated Elastic Net Multi-View Meta-Learner (MVL) SuperLearner to prevent overfitting. However, the specific molecular drivers (genetic biomarkers) are extracted exclusively from the XGBoost sub-model's SHAP geometry. The genetic drivers listed are the top features pushing the XGBoost model's decision. Do NOT arbitrarily assign outcome labels to these genes simply to create a narrative contrast. If a gene's biological role appears contradictory to the final MVL clinical trajectory, professionally explain this as tumor heterogeneity: where localized molecular mechanisms (XGBoost) are ultimately overridden by systemic disease factors (MVL). If the biology aligns with the trajectory, simply explain the synergistic mechanisms. \n\nCRITICAL HEDGING & HYPOTHESIS INSTRUCTION: You MUST use hedging language. Replace strong claims (e.g., 'demonstrate', 'prove', 'establish') with cautious terms (e.g., 'suggest', 'are consistent with', 'are compatible with', 'raise the possibility that', 'may indicate that'). Explicitly frame your synthesis as a hypothesis or conceptual model rather than a definitive conclusion. \n\n", strict_nomenclature_rule, "\n\nYou MUST explicitly cite the specific biomarkers provided in the patient profile to justify your synthesis and identify therapeutic vulnerabilities. Do not hallucinate genes or phenotype statuses (such as TMB or MSI) that are not explicitly provided in the patient's data. \n\n", global_associativity, "\n\nDO NOT use robotic formatting, markdown bullet points, numbered lists, or bolding (like **Answer:** or ###) for the main synthesis. Write naturally as a physician would in a clinical chart. MANDATORY REQUIREMENT: At the very end of your response, you MUST append a new section titled 'Suggested Clinical Queries:'. In this section, you MUST provide EXACTLY 3 highly specific, logical follow-up questions tailored to this patient's exact genetic profile that the user can ask the Interactive Clinical Dialogue module to investigate further. Do NOT generate 4 questions. Do NOT generate 5 questions. Generate EXACTLY 3. Format these exactly as a numbered list with dashes, like this: \n1. - [Question 1]\n2. - [Question 2]\n3. - [Question 3]\n\nFinally, append this exact sentence as a separate paragraph at the very end of your entire output: 'The interpretations presented above should be considered hypothesis-generating and are intended to support biological and clinical exploration. The proposed mechanisms, therapeutic associations, and disease trajectories are inferred from machine-learning models, statistical associations, multi-omic relationships, and literature-supported evidence. These findings do not establish direct causality and should be interpreted within the context of the available data, requiring independent experimental and clinical validation whenever appropriate.'", nomenclature_quarantine_checklist, mrna_terminology_quarantine_checklist, narrative_governance_framework, llm_glossary, omic_layer_terminology_governance, tsm_ontology_protection,
        build_nine_strata_governance_block(),
        "\n\n--- CANCER GENE LIST GOVERNANCE v2.0 ---\n",
        "The CANCER GENE LIST EVIDENCE section provides cancer-gene annotations from OncoKB Cancer Gene Census. ",
        "This is a BIOLOGICAL CONTEXT LAYER (Stratum 5), NOT a therapeutic actionability layer. ",
        "FORBIDDEN when discussing CGL: 'actionable', 'druggable', 'therapeutic target', 'clinical actionability'. ",
        "SAFE VOCABULARY: 'recognized cancer gene', 'cancer-relevant', 'biological context'. ",
        "END CANCER GENE LIST GOVERNANCE\n\n",
        "\n\n--- TIER 0 REGULATORY RECOGNITION GOVERNANCE v1.0 ---\n",
        "Tier 0 is REGULATORY/CURATORIAL RECOGNITION (Stratum 6), NOT a treatment recommendation. ",
        "T0A = Exact biomarker-drug match. T0B = Gene-level match. T0C = OncoKB curated without FDA label. ",
        "FORBIDDEN: 'treatment recommendation', 'should receive', 'is indicated for', 'prescribe'. ",
        "SAFE: 'FDA-recognized biomarker-drug association', 'regulatory recognition', 'curated biomarker-drug relationship'. ",
        "END TIER 0 REGULATORY RECOGNITION GOVERNANCE\n\n",
        "\n\n--- ONCOKB CONCORDANCE LOCK v1.0 ---\n",
        "OncoKB therapeutic Levels 1-4 are LICENSE-GATED. Do NOT fabricate OncoKB Level classifications. ",
        "Do NOT conflate Fda2/Fda3 (Tier 0, Stratum 6) with therapeutic Levels 1-4 (Stratum 7). ",
        "END ONCOKB CONCORDANCE LOCK\n\n",
        endpoint_vocabulary_governance, omic_layer_consistency_governance)

        user_prompt_shap <- paste0(
          "Patient ID: ", input$tumor_shap_patient, "\n",
          "Cancer Cohort: ", input$tumor_shap_cancer, "\n",
          "Clinical Metric: ", input$tumor_shap_metric, "\n\n",
          "Predicted ", metric_label, ":\n",
          "- 1 Year: ", pat_1yr, "\n",
          "- 3 Years: ", pat_3yr, "\n",
          "- 5 Years: ", pat_5yr, "\n\n",
          "Patient's Specific Multi-Omic Expression Profile:\n",
          patient_profile, "\n",
          rcd_biological_context_decoder, "\n",
          patient_phenotype_context, "\n\n",
          "Analyze this patient's profile and provide the therapeutic vulnerability synthesis."
        )
        # AUDIT VERSION: preserves full signature nomenclatures for provenance traceability
        user_prompt_shap_audit <- paste0(
          "Patient ID: ", input$tumor_shap_patient, "\n",
          "Cancer Cohort: ", input$tumor_shap_cancer, "\n",
          "Clinical Metric: ", input$tumor_shap_metric, "\n\n",
          "Predicted ", metric_label, ":\n",
          "- 1 Year: ", pat_1yr, "\n",
          "- 3 Years: ", pat_3yr, "\n",
          "- 5 Years: ", pat_5yr, "\n\n",
          "Patient's Specific Multi-Omic Expression Profile:\n",
          patient_profile_audit, "\n",
          rcd_biological_context_decoder, "\n",
          patient_phenotype_context, "\n\n",
          "Analyze this patient's profile and provide the therapeutic vulnerability synthesis."
        )
        # --- ASYNC LLM DISPATCH: Capture config and inputs, run non-blocking ---
        status <- check_llm_status()
        if (!status$success) {
          session$sendCustomMessage("hide_spinner", list())
          rv_shap_llm_text(paste0("ERROR: ", status$message))
          return()
        }
        cfg <- capture_llm_config()
        start_t <- Sys.time()
        sid <- my_session_id
        pat_id <- input$tumor_shap_patient
        pat_cohort <- input$tumor_shap_cancer

        msgs_shap <- list(list(role = "system", content = system_prompt_shap), list(role = "user", content = user_prompt_shap))
        sys_prompt_shap <- system_prompt_shap
        usr_prompt_shap <- user_prompt_shap
        pat_profile <- patient_profile
        pat_pheno_ctx <- patient_phenotype_context
        pat_nom <- patient_nomenclatures
        usr_prompt_audit <- user_prompt_shap_audit

        # Run LLM call(s) + post-processing in background (non-blocking)
        future::future({
          shap_response <- send_llm_request(msgs_shap, cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url)

          shap_response <- gsub("(?s)^.*?</think>\\s*", "", shap_response, perl = TRUE)
          shap_response <- gsub("*", "", shap_response, fixed = TRUE)
          if (is.na(shap_response) || !is.character(shap_response) || length(shap_response) == 0L) {
            stop("Post-processing failure: shap_response became NA/empty after gsub.")
          }
          audit_collector <- list()
          shap_response <- scrub_governance_violations(shap_response)
          audit_collector[[length(audit_collector) + 1]] <- attr(shap_response, "audit_actions")
          eco_result <- check_ecological_fallacy(shap_response, pat_id, "SHAP_Decoding")
          shap_response <- eco_result$annotated_text
          eco_count <- eco_result$violation_count

          fact_issues_count <- 0
          MAX_RETRIES <- 2
          for (retry_i in seq_len(MAX_RETRIES + 1)) {
            fact_result <- tryCatch(
              validate_llm_factuality(shap_response, pat_id, "SHAP_Decoding", paste(pat_profile, pat_pheno_ctx, sep = "\n"), pat_nom),
              error = function(e2) {
                message("[WARN] validate_llm_factuality failed: ", e2$message)
                list(issues = list(), factuality_score = 0)
              }
            )
            fact_issues_count <- length(fact_result$issues)
            if (fact_issues_count == 0 || retry_i > MAX_RETRIES) break
            correction_instruction <- paste0(
              "\n\nCRITICAL FACTUALITY CORRECTION (RETRY ", retry_i, "/", MAX_RETRIES, "): Your previous response contained ", fact_issues_count,
              " factual inconsistency/ies. CORRECT these and regenerate.\n"
            )
            shap_response <- send_llm_request(
              list(list(role = "system", content = paste0(sys_prompt_shap, correction_instruction)),
                   list(role = "user", content = paste0(usr_prompt_shap, "\n\n[FACTUALITY REGENERATION]"))),
              cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url
            )
            shap_response <- gsub("(?s)^.*?</think>\\s*", "", shap_response, perl = TRUE)
            shap_response <- gsub("*", "", shap_response, fixed = TRUE)
            shap_response <- scrub_governance_violations(shap_response)
          }
          shap_response <- ensure_questions_and_disclaimer(shap_response, patient_id = pat_id, cohort = pat_cohort)
          audit_collector[[length(audit_collector) + 1]] <- attr(shap_response, "audit_actions")

          # Parse suggested queries for the chat dropdown
          chat_choices <- NULL
          parts <- strsplit(shap_response, "(?i)Suggested Clinical Queries:", perl = TRUE)[[1]]
          if (length(parts) > 1) {
            queries_part <- parts[length(parts)]
            splits <- strsplit(queries_part, "\\?(?:\\s+|$)", perl = TRUE)[[1]]
            parsed_queries <- trimws(splits[splits != "" & !grepl("^\\s*$", splits)])
            if (length(parsed_queries) > 0) {
              qs <- c()
              for (q in parsed_queries) {
                q <- gsub("The interpretations presented above.*appropriate\\.?", "", q, ignore.case = TRUE)
                q <- gsub("^(?:[1-9][\\.\\)]\\s*)?(?:-{1,2}|[*•])\\s*", "", trimws(q))
                q <- gsub("\\n", " ", q)
                q <- gsub("\\s+", " ", q)
                q <- trimws(q)
                if (nchar(q) > 5) qs <- c(qs, paste0(q, "?"))
              }
              if (length(qs) > 0) chat_choices <- qs
            }
          }

          elapsed_s <- round(as.numeric(difftime(Sys.time(), start_t, units = "secs")), 1)
          list(text = shap_response, audit_badge = build_governance_audit_badge(audit_collector, eco_count, fact_issues_count),
               elapsed = elapsed_s, chat_choices = chat_choices, payload_audit = usr_prompt_audit)
        }, seed = TRUE) %...>% (function(res) {
          # --- Reactive updates (back in main Shiny session) ---
          release_queue(sid)
          session$sendCustomMessage("hide_spinner", list())

          rv_shap_llm_text(res$text)
          rv_shap_audit(res$audit_badge)
          rv_shap_llm_payload(res$payload_audit)

          elapsed_s <- res$elapsed
          log_file <- "llm_performance_log.csv"
          new_row <- data.frame(Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Module = "SHAP_Decoding", Time_secs = elapsed_s, stringsAsFactors = FALSE)
          if (!file.exists(log_file)) write.csv(new_row, log_file, row.names = FALSE) else write.table(new_row, log_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
          time_str <- paste0("Automatically generated on ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), " (Processing Time: ", elapsed_s, "s)")
          rv_shap_llm_time(time_str)

          if (!is.null(res$chat_choices)) {
            new_choices <- c("Custom (Type your own query below)" = "")
            for (qc in res$chat_choices) new_choices[qc] <- qc
            updateSelectInput(session, "chat_prompt_examples", choices = new_choices)
          }
        }) %...!% (function(e) {
          release_queue(sid)
          session$sendCustomMessage("hide_spinner", list())
          rv_shap_llm_text(paste0("ERROR: The AI Engine failed to decode the geometric trajectory. ", conditionMessage(e)))
          rv_shap_llm_payload(NULL)
        })
        return(NULL)
      }, error = function(e) {
        session$sendCustomMessage("hide_spinner", list())
        rv_shap_llm_text(paste0("ERROR: ", conditionMessage(e)))
        rv_shap_llm_payload(NULL)
      })
    })

    output$tumor_shap_report_ui <- renderUI({
      txt <- rv_shap_llm_text()
      if (is.null(txt)) return(tags$p(style="color: #94a3b8; font-style: italic;", "Decoding trajectory..."))

      if(startsWith(txt, "ERROR:")) {
         return(tags$div(
           style="background: rgba(239, 68, 68, 0.1); border: 1px solid rgba(239, 68, 68, 0.3); border-radius: 8px; padding: 15px;",
           tags$p(style="color: #f87171; font-weight: bold;", txt)
         ))
      }

      txt <- gsub("\\\\*", "", txt) # Strip all asterisks
      txt <- gsub("###", "", txt) # Strip headers

      paragraphs <- unlist(strsplit(txt, "\n\n+"))
      ui_paragraphs <- lapply(paragraphs, function(p) {
        tags$p(style="color: #e2e8f0; font-size: 15px; line-height: 1.7;", p)
      })

      payload_ui <- if (!is.null(rv_shap_llm_payload())) {
        tags$details(
          style="margin-top: 15px; padding: 10px; background: rgba(15, 23, 42, 0.4); border: 1px solid #334155; border-radius: 6px;",
          tags$summary(style="color: #94a3b8; font-size: 0.85rem; cursor: pointer;", bs_icon("code-square"), " Audit Clinical Payload Data (Raw Injection)"),
          tags$pre(style="margin-top: 10px; font-size: 0.75rem; color: #cbd5e1; white-space: pre-wrap; word-wrap: break-word;", rv_shap_llm_payload())
        )
      } else { NULL }

      time_ui <- if(!is.null(rv_shap_llm_time())) {
        tags$div(style="font-size:10px; color:#94a3b8; text-align:right; margin-top:15px;", rv_shap_llm_time())
      } else { NULL }

      tags$div(
        style="background: rgba(16, 185, 129, 0.1); border: 1px solid rgba(16, 185, 129, 0.3); border-radius: 8px; padding: 20px;",
        tags$h4(style="color: #34d399; font-weight: bold; margin-top: 0;", bs_icon("clipboard2-pulse"), " Clinical Synthesis & Therapeutic Vulnerabilities"),
        ui_paragraphs,
        if (!is.null(rv_shap_audit())) rv_shap_audit() else NULL,
        payload_ui,
        time_ui
      )
    })

    # ==============================================================================
    # TUMOR BOARD - PERSONALIZED SHAP DECODING UI TRIGGERS
    # ==============================================================================

    observeEvent(input$btn_run_shap_llm, {
      if (is.null(input$tumor_shap_patient) || input$tumor_shap_patient == "") {
         session$sendCustomMessage("hide_spinner", list())
         showNotification("System Error: No Patient ID detected. Please ensure the Phase III Clinical Probabilities datasets are loaded correctly in the working directory.", type = "error")
         return()
      }

      # Quick check if LLM is available
      status <- check_llm_status()
      if (!status$success) {
         session$sendCustomMessage("hide_spinner", list())
         showNotification(paste0("LLM Service Error: ", status$message), type = "error")
         return()
      }

      rv_pending_task("shap")

      # DeepSeek: no queue needed (cloud API handles parallel requests)
      if (shiny::isolate(llm_config$backend) == "deepseek") {
        rv_in_queue(FALSE)
        rv_trigger_shap_llm(rv_trigger_shap_llm() + 1)
        return()
      }

      # Ollama: use queue (single-process LLM)
      pos <- join_queue(my_session_id)

      if (pos == 0L) {
        rv_in_queue(FALSE)
        rv_trigger_shap_llm(rv_trigger_shap_llm() + 1)
      } else {
        rv_in_queue(TRUE)
        rv_queue_pos(pos)
        session$sendCustomMessage("hide_spinner", list())
      }
    })

    # ==============================================================================
    # PHARMACOGENOMIC TRANSLATION MODULE
    # ==============================================================================
    observeEvent(input$btn_run_pharma_llm, {

  # The enable_TMB_MSI flag (set to TRUE above) controls TMB/MSI-dependent processing.
  # Patient-level TSM/TMB/MSI data is always sourced from stemness_df.
      if (is.null(input$tumor_shap_patient) || input$tumor_shap_patient == "") {
         session$sendCustomMessage("hide_spinner", list())
         showNotification("System Error: No Patient ID detected.", type = "error")
         return()
      }

      # Quick check if LLM is available
      status <- check_llm_status()
      if (!status$success) {
         session$sendCustomMessage("hide_spinner", list())
         showNotification(paste0("LLM Service Error: ", status$message), type = "error")
         return()
      }

      rv_pending_task("pharma")

      # DeepSeek: no queue needed (cloud API handles parallel requests)
      if (shiny::isolate(llm_config$backend) == "deepseek") {
        rv_in_queue(FALSE)
        rv_trigger_pharma_llm(rv_trigger_pharma_llm() + 1)
        return()
      }

      # Ollama: use queue (single-process LLM)
      pos <- join_queue(my_session_id)
      if (pos == 0L) {
        rv_in_queue(FALSE)
        rv_trigger_pharma_llm(rv_trigger_pharma_llm() + 1)
      } else {
        rv_in_queue(TRUE)
        rv_queue_pos(pos)
        session$sendCustomMessage("hide_spinner", list())
      }
    })

    observeEvent(rv_trigger_pharma_llm(), {
      req(rv_trigger_pharma_llm() > 0)
      on.exit({
        release_queue(my_session_id)
      })

      # 1. Extract Targets from Patient Profile
      profile_text <- rv_active_patient_profile()
      profile_text_audit <- rv_active_patient_profile_audit()
      if (is.null(profile_text) || profile_text == "") {
         rv_pharma_llm_text("ERROR: No patient geometric profile found. Please generate the SHAP geometry first.")
         return()
      }

      # Retrieve patient-level stemness data and classify it
      patient_info <- stemness_df %>% dplyr::filter(sample_id == input$tumor_shap_patient)
      if (nrow(patient_info) == 0) {
        rv_pharma_llm_text("ERROR: No stemness data found for this patient.")
        return()
      }
      cancer_type <- patient_info$cancer_type_abbreviation[1]
      rnass_score <- patient_info$RNAss[1]
      ereg_score  <- patient_info$EREG.EXPss[1]

      rnass_class <- patient_info$RNAss_class[1]
        ereg_class  <- patient_info$EREG_class[1]

        # TMB Data
        tmb_val <- patient_info$Non_silent_per_Mb[1]
        tmb_class <- patient_info$TMB_class[1]
        if (is.na(tmb_val)) {
           tmb_str <- "TMB: No data available for this patient."
        } else {
           tmb_str <- paste0("TMB (Non-silent/Mb): ", round(tmb_val, 3), " (", tmb_class, ").")
        }

        # MSI Data
        msi_val <- patient_info$Total_nb_MSI_events[1]
        msi_class <- patient_info$MSI_class[1]
        if (is.na(msi_val)) {
           msi_str <- "MSI: No data available for this patient."
        } else {
           msi_str <- paste0("MSI (Total Events): ", round(msi_val, 3), " (", msi_class, ").")
        }

        # Build a short context string for the LLM later
        tsm_context <- paste0(
          "Cancer type: ", cancer_type, ".\n",
          "Stemness - RNAss = ", round(rnass_score, 3), " (", rnass_class, "), ",
          "EREG.EXPss = ", round(ereg_score, 3), " (", ereg_class, ").\n",
          tmb_str, "\n",
          msi_str
        )

      # Extract ALL genes from "Encoded Gene Mechanics:" blocks, including pipe-delimited multi-gene entries
      # Step 1: pull the full content after "Encoded Gene Mechanics: " through end of each line
      mech_lines <- regmatches(profile_text, gregexpr("Encoded Gene Mechanics: [^\n]+", profile_text))[[1]]
      mech_lines <- gsub("^Encoded Gene Mechanics: ", "", mech_lines)
      # Step 2: split on " | " to get individual gene entries, then extract the gene symbol (first word before space)
      patient_genes <- unique(unlist(lapply(strsplit(mech_lines, " \\| "), function(gene_entries) {
        gsub("^([A-Za-z0-9-]+) .*", "\\1", gene_entries)
      })))
      patient_genes <- patient_genes[!is.na(patient_genes) & patient_genes != "" & patient_genes != "Unknown"]

      if(length(patient_genes) == 0) {
         rv_pharma_llm_text("ERROR: No valid gene targets could be extracted from the patient's signature.")
         return()
      }

      # 2. Matrix Cross-Reference
      # Defensive column name mapping to prevent fatal R process crashes
      if (!"Gene_Symbol" %in% colnames(unified_drug_matrix)) {
          col_idx <- grep("(?i)gene.*symbol|gene", colnames(unified_drug_matrix))[1]
          if (is.na(col_idx)) col_idx <- 1
          colnames(unified_drug_matrix)[col_idx] <- "Gene_Symbol"
      }
      # Ensure Evidence_Tier and Cancer_Type_Context columns exist (backward compat with older matrix)
      if (!"Evidence_Tier" %in% colnames(unified_drug_matrix)) {
          unified_drug_matrix$Evidence_Tier <- 5L
      }
      if (!"Cancer_Type_Context" %in% colnames(unified_drug_matrix)) {
          unified_drug_matrix$Cancer_Type_Context <- "Not annotated"
      }

      # Safely subset the matrix using base R to prevent dplyr mapping errors
      matched_drugs <- tryCatch({
          unified_drug_matrix[unified_drug_matrix$Gene_Symbol %in% patient_genes, , drop = FALSE]
      }, error = function(e) { data.frame() })

      # OncoKB Gene Annotation cross-reference
      matched_oncokb_genes <- tryCatch({
        oncokb_gene_annotations[oncokb_gene_annotations$Gene_Symbol %in% patient_genes, , drop = FALSE]
      }, error = function(e) { data.frame() })

      if(nrow(matched_drugs) == 0 && nrow(matched_oncokb_genes) == 0) {
         rv_pharma_llm_text("CLINICAL SYNTHESIS: No pharmacogenomic vulnerabilities were found in the Unified Drug Matrix (DGIdb, CIViC, OncoKB) for this patient's specific gene targets. This may reflect limited database coverage for the genes in this signature rather than an absence of biological targetability.")
         return()
      }

      # Build OncoKB Gene Annotation summary for the LLM
      build_oncokb_gene_summary <- function(oc_genes, all_patient_genes) {
        lines <- c()
        lines <- c(lines, "ONCOKB GENE ANNOTATIONS (MSKCC Precision Oncology Knowledge Base)")
        lines <- c(lines, "============================================================")
        lines <- c(lines, sprintf("Patient genes with OncoKB annotations: %d / %d", nrow(oc_genes), length(all_patient_genes)))
        lines <- c(lines, "")
        lines <- c(lines, "Oncogenic Classification Legend:")
        lines <- c(lines, "  ONCOGENE = Known oncogene (gain-of-function alterations promote cancer)")
        lines <- c(lines, "  TSG = Tumor Suppressor Gene (loss-of-function alterations promote cancer)")
        lines <- c(lines, "  ONCOGENE_AND_TSG = Context-dependent (may act as either)")
        lines <- c(lines, "  INSUFFICIENT_EVIDENCE = Not yet classified by OncoKB")
        lines <- c(lines, "")

        # Group by oncogenic class
        for (oc in c("ONCOGENE", "TSG", "ONCOGENE_AND_TSG", "INSUFFICIENT_EVIDENCE")) {
          sub <- oc_genes[oc_genes$Oncogenic_Class == oc, , drop = FALSE]
          if (nrow(sub) == 0) next
          lines <- c(lines, paste0("--- ", oc, " (", nrow(sub), " genes) ---"))
          for (i in seq_len(nrow(sub))) {
            row <- sub[i, ]
            level_info <- ""
            if (!is.null(row$Highest_Sensitive_Level) && !is.na(row$Highest_Sensitive_Level) && row$Highest_Sensitive_Level != "") {
              level_info <- paste0(" [Highest OncoKB Level: ", row$Highest_Sensitive_Level, "]")
            }
            lines <- c(lines, sprintf("  %s | %s | %s%s",
              row$Gene_Symbol, row$Oncogenic_Class,
              ifelse(nchar(row$Gene_Summary) > 200,
                     paste0(substr(row$Gene_Summary, 1, 197), "..."),
                     row$Gene_Summary),
              level_info))
          }
          lines <- c(lines, "")
        }

        # Also list patient genes NOT in OncoKB
        missing_genes <- setdiff(all_patient_genes, oc_genes$Gene_Symbol)
        if (length(missing_genes) > 0) {
          lines <- c(lines, paste0("Genes without OncoKB annotation: ", paste(missing_genes, collapse = ", ")))
        }
        lines
      }

      oncokb_gene_text <- if (nrow(matched_oncokb_genes) > 0) {
        build_oncokb_gene_summary(matched_oncokb_genes, patient_genes)
      } else {
        c("OncoKB Gene Annotations: None of the patient's signature genes are currently curated in the OncoKB precision oncology knowledge base.")
      }

      # Build Cancer Gene List summary (orthogonal cancer-gene evidence layer from oncoKB_cancerGeneList.tsv)
      build_cancer_gene_list_summary <- function(patient_genes, cgl_df) {
        if (nrow(cgl_df) == 0) return("Cancer Gene List: Data not available.")
        # Map Hugo Symbol column name variants
        hugo_col <- grep("Hugo.*Symbol|Gene.*Symbol", colnames(cgl_df), value = TRUE, ignore.case = TRUE)[1]
        if (is.na(hugo_col)) hugo_col <- colnames(cgl_df)[1]
        matched <- cgl_df[cgl_df[[hugo_col]] %in% patient_genes, , drop = FALSE]
        if (nrow(matched) == 0) {
          return(sprintf("CANCER GENE LIST (OncoKB Cancer Gene Census v99): None of the patient's %d signature genes (%s) are currently recognized in the OncoKB Cancer Gene List (1,240 curated cancer genes from MSK-IMPACT, FoundationOne, Vogelstein, COSMIC CGC, and OncoKB annotation resources).",
            length(patient_genes), paste(patient_genes, collapse = ", ")))
        }
        lines <- c("CANCER GENE LIST EVIDENCE (OncoKB Cancer Gene Census v99, May-2026)",
                   "========================================================================")
        lines <- c(lines, sprintf("Patient genes recognized as cancer genes: %d / %d", nrow(matched), length(patient_genes)))
        lines <- c(lines, sprintf("Total curated cancer genes in reference list: %d", nrow(cgl_df)))
        lines <- c(lines, "")
        lines <- c(lines, "Evidence Resource Legend:")
        lines <- c(lines, "  OncoKB Annotated = Curated in OncoKB precision oncology knowledge base")
        lines <- c(lines, "  MSK-IMPACT     = Included in MSK-IMPACT clinical sequencing panel (468 genes)")
        lines <- c(lines, "  MSK-HEME       = Included in MSK-HEME heme-onc sequencing panel")
        lines <- c(lines, "  FoundationOne   = Included in FoundationOne CDx comprehensive genomic profiling")
        lines <- c(lines, "  FoundationOne Heme = Included in FoundationOne Heme panel")
        lines <- c(lines, "  Vogelstein     = Classified in Vogelstein cancer gene census (oncogene/TSG)")
        lines <- c(lines, "  COSMIC CGC (v99) = Included in COSMIC Cancer Gene Census v99")
        lines <- c(lines, "")
        for (i in seq_len(nrow(matched))) {
          row <- matched[i, ]
          gene_sym <- row[[hugo_col]]
          # Collect membership flags
          memberships <- c()
          if ("OncoKB Annotated" %in% colnames(row) && !is.na(row[["OncoKB Annotated"]]) &&
              tolower(as.character(row[["OncoKB Annotated"]])) == "yes")
            memberships <- c(memberships, "OncoKB")
          if ("MSK-IMPACT" %in% colnames(row) && !is.na(row[["MSK-IMPACT"]]) &&
              tolower(as.character(row[["MSK-IMPACT"]])) == "yes")
            memberships <- c(memberships, "MSK-IMPACT")
          if ("MSK-HEME" %in% colnames(row) && !is.na(row[["MSK-HEME"]]) &&
              tolower(as.character(row[["MSK-HEME"]])) == "yes")
            memberships <- c(memberships, "MSK-HEME")
          if ("FOUNDATION ONE" %in% colnames(row) && !is.na(row[["FOUNDATION ONE"]]) &&
              tolower(as.character(row[["FOUNDATION ONE"]])) == "yes")
            memberships <- c(memberships, "FoundationOne")
          if ("FOUNDATION ONE HEME" %in% colnames(row) && !is.na(row[["FOUNDATION ONE HEME"]]) &&
              tolower(as.character(row[["FOUNDATION ONE HEME"]])) == "yes")
            memberships <- c(memberships, "FoundationOneHeme")
          if ("Vogelstein" %in% colnames(row) && !is.na(row[["Vogelstein"]]) &&
              tolower(as.character(row[["Vogelstein"]])) == "yes")
            memberships <- c(memberships, "Vogelstein")
          if ("COSMIC CGC (v99)" %in% colnames(row) && !is.na(row[["COSMIC CGC (v99)"]]) &&
              tolower(as.character(row[["COSMIC CGC (v99)"]])) == "yes")
            memberships <- c(memberships, "COSMIC_CGC")
          gene_type <- ""
          if ("Gene Type" %in% colnames(row) && !is.na(row[["Gene Type"]]))
            gene_type <- paste0(" [", as.character(row[["Gene Type"]]), "]")
          occ <- ""
          occ_col <- grep("occurrence.*resource|# of occurrence", colnames(row), value = TRUE, ignore.case = TRUE)
          if (length(occ_col) > 0 && !is.na(row[[occ_col[1]]]))
            occ <- paste0(" (resource score: ", row[[occ_col[1]]], ")")
          lines <- c(lines, sprintf("  %s%s%s | Memberships: %s",
            gene_sym, gene_type, occ,
            if (length(memberships) > 0) paste(memberships, collapse = ", ") else "None"))
        }
        # List patient genes NOT in cancer gene list
        missing <- setdiff(patient_genes, matched[[hugo_col]])
        if (length(missing) > 0) {
          lines <- c(lines, "")
          lines <- c(lines, sprintf("Genes NOT in Cancer Gene List: %s", paste(missing, collapse = ", ")))
        }
        lines <- c(lines, "", "CRITICAL: Membership in cancer gene resources does NOT imply therapeutic actionability. This layer provides biological context regarding the degree to which a gene is recognized as relevant to cancer biology across major curated resources.")
        paste(lines, collapse = "\n")
      }

      cancer_gene_list_text <- build_cancer_gene_list_summary(patient_genes, oncokb_cancer_gene_list)

      if(nrow(matched_drugs) == 0) {
         # Has OncoKB gene data but no drug matches
         rv_pharma_llm_text(paste0(
           "CLINICAL SYNTHESIS: No pharmacogenomic drug interactions were found in DGIdb/CIViC for this patient's gene targets. ",
           "However, ", nrow(matched_oncokb_genes), " genes have OncoKB precision oncology annotations (see below). ",
           "This may reflect limited drug database coverage rather than an absence of biological targetability.\n\n",
           paste(oncokb_gene_text, collapse = "\n")
         ))
         return()
      }

      # Build a tiered, evidence-aware drug summary for the LLM
      # Group by evidence tier (1=highest, 5=lowest) so the LLM can prioritize
      tier_labels <- c(
        "1" = "TIER 1 - FDA-recognized / Standard-of-care",
        "2" = "TIER 2 - Standard care / CIViC Level A",
        "3" = "TIER 3 - Compelling clinical evidence",
        "4" = "TIER 4 - Clinical / Preclinical evidence",
        "5" = "TIER 5 - Single-source / Preclinical / Inferential"
      )

      build_tiered_summary <- function(df) {
        if (!"Evidence_Tier" %in% names(df)) {
          # Backward compatibility: old matrix without tiering
          return(paste(capture.output(print(df[, c("Gene_Symbol","Drug_Name","Interaction_Type","Source_Database","Clinical_Status")])), collapse = "\n"))
        }
        tiers <- sort(unique(df$Evidence_Tier))
        lines <- c()
        lines <- c(lines, "PHARMACOGENOMIC EVIDENCE-TIERED MATRIX")
        lines <- c(lines, "==========================================")
        lines <- c(lines, sprintf("Total gene-drug associations found: %d", nrow(df)))
        lines <- c(lines, sprintf("Genes with matches: %s", paste(unique(df$Gene_Symbol), collapse = ", ")))
        lines <- c(lines, "")
        # --- OncoKB Concordance Lock: only reference OncoKB therapeutic levels
        #     if OncoKB API data ACTUALLY contributed to the matrix.
        #     Fda2/Fda3 from local TSV files are Tier 0 regulatory evidence (Stratum 6),
        #     NOT therapeutic Levels 1-4 (Stratum 7). Never conflate them.
        has_oncokb_api <- any(grepl("OncoKB", unique(df$Source_Database), ignore.case = TRUE), na.rm = TRUE)
        lines <- c(lines, "EVIDENCE TIER LEGEND:")
        if (has_oncokb_api) {
          lines <- c(lines, "  Tier 1 = FDA-recognized (OncoKB Level 1)")
          lines <- c(lines, "  Tier 2 = Standard care (OncoKB Level 2 / CIViC Level A)")
          lines <- c(lines, "  Tier 3 = Compelling clinical evidence (OncoKB Level 3A/3B / CIViC Level B / DGIdb MultiSource+PMID)")
          lines <- c(lines, "  Tier 4 = Clinical/Preclinical (OncoKB Level 4 / CIViC Level C / DGIdb MultiSource)")
        } else {
          lines <- c(lines, "  Tier 1 = FDA-recognized / Standard-of-care (CIViC Level A)")
          lines <- c(lines, "  Tier 2 = Standard care / Strong clinical evidence (CIViC Level B)")
          lines <- c(lines, "  Tier 3 = Compelling clinical evidence (CIViC Level B / DGIdb MultiSource+PMID)")
          lines <- c(lines, "  Tier 4 = Clinical/Preclinical (CIViC Level C / DGIdb MultiSource)")
        }
        lines <- c(lines, "  Tier 5 = Single-source / Preclinical / Inferential (CIViC Level D-E / DGIdb SingleSource)")
        if (!has_oncokb_api) {
          lines <- c(lines, "", "⚠ ONCOKB CONCORDANCE LOCK: OncoKB therapeutic Levels 1-4 are NOT present in this matrix.")
          lines <- c(lines, "  These require API access under a license agreement. The matrix includes OncoKB gene-level")
          lines <- c(lines, "  annotations (oncogene/TSG, Cancer Gene List) and FDA-recognized biomarker-drug associations")
          lines <- c(lines, "  (Fda2/Fda3 / Tier 0 / Stratum 6) downloaded from the OncoKB public repository.")
          lines <- c(lines, "  OncoKB therapeutic Level 1-4 drug-variant associations are not available.")
          lines <- c(lines, "  YOU MUST NOT claim any association has 'OncoKB Level 1/2/3A/3B/4' support.")
          lines <- c(lines, "  YOU MUST NOT fabricate OncoKB therapeutic level classifications.")
        }
        lines <- c(lines, "")
        for (t in tiers) {
          t_label <- if (as.character(t) %in% names(tier_labels)) tier_labels[as.character(t)] else paste("Tier", t)
          t_df <- df[df$Evidence_Tier == t, , drop = FALSE]
          lines <- c(lines, paste0("--- ", t_label, " (", nrow(t_df), " associations) ---"))
          for (i in seq_len(nrow(t_df))) {
            row <- t_df[i, ]
            ctx <- if ("Cancer_Type_Context" %in% names(row) && !is.na(row$Cancer_Type_Context) && row$Cancer_Type_Context != "Unknown") {
              paste0(" [", row$Cancer_Type_Context, "]")
            } else ""
            lines <- c(lines, sprintf("  %s | %s | %s | %s | %s%s",
              row$Gene_Symbol, row$Drug_Name, row$Interaction_Type,
              row$Source_Database, row$Clinical_Status, ctx))
          }
          lines <- c(lines, "")
        }
        lines <- c(lines, "CRITICAL: You MUST qualify each drug-gene association by its evidence tier when discussing it.")
        lines <- c(lines, "Tier 1-2 associations should be highlighted as having stronger clinical evidence support.")
        lines <- c(lines, "Tier 4-5 associations MUST be presented as requiring further validation.")
        if (!has_oncokb_api) {
          lines <- c(lines, "ONCOKB CONCORDANCE LOCK (repeated): Do NOT attribute OncoKB therapeutic levels (1/2/3A/3B/4)")
          lines <- c(lines, "  to any association. The only OncoKB evidence available is Tier 0 (Stratum 6)")
          lines <- c(lines, "  regulatory recognition (Fda2/Fda3) and Cancer Gene List membership (Stratum 5).")
        }
        return(paste(lines, collapse = "\n"))
      }

      drug_summary <- build_tiered_summary(matched_drugs)
      # TMB/MSI data is available from stemness_df; patient classifications (High/Int/Low) are passed to the LLM.
      #
      # 3. Omic-Compatibility Logic
      omic_warnings <- "MULTI-COMPONENT SIGNATURE RULE: Signatures MAY be multi-component. The number of constituent elements for each signature is listed in Table S11, where values are enumerated. If a specific gene within a signature is targetable, explicitly discuss that while acknowledging that other elements of the mathematical signature may remain unaffected.\n\nOMIC LAYER TOKEN DECODING DICTIONARY: The omic-layer tokens embedded in signature nomenclature are strictly CATEGORICAL IDENTIFIER CODES. Decode them ONLY via this dictionary: .1=Protein, .2=Mutation, .3=Copy Number Variation (CNV), .4=miRNA, .5=Transcript Isoform, .6=mRNA Expression, .7=CpG Methylation. The numeric token itself carries NO quantitative meaning. .6 does NOT mean 'six omic layers', .5 does NOT mean 'five biological dimensions', .7 does NOT mean 'seven modalities'. Use ONLY the decoded category label for reasoning.\n\nOMIC TARGETABILITY RULE: You MUST explicitly evaluate drug amenability based on the specific Omic Layer presented in the signature. Direct pharmacological targeting is typically viable for protein, mRNA isoform and miRNA. Conversely, if the omic layer is Mutation, CNV (Copy Number Variation), or CpG Methylation, you MUST explicitly state that standard direct inhibitors are generally NOT directly amenable, even if the gene itself appears targetable in the drug matrix, due to the structural or epigenetic nature of the feature.\n\n"

      # We check the tokens present in the profile
      if(grepl("Token 1|Token 4|Token 5|Token 6|Transcript|Protein|miRNA", profile_text, ignore.case=TRUE)) {
         omic_warnings <- paste0(omic_warnings, "DIRECT TARGETING RULE: The patient's signature includes Transcripts/Proteins/miRNA (Tokens 1, 4, 5, 6). Explicitly name the biological layer and discuss RNA interference (for miRNA) vs small molecule inhibition (for Proteins).\n")
      }
      if(grepl("Token 2|Token 3|Mutation|CNV", profile_text, ignore.case=TRUE)) {
         omic_warnings <- paste0(omic_warnings, "STRUCTURAL RESISTANCE WARNING: The patient's signature includes Mutations or CNV (Tokens 2, 3). Standard competitive inhibitors may be structurally blocked or overwhelmed. Discuss allosteric inhibitors or synthetic lethality.\n")
      }
      if(grepl("Token 7|Methylation|CpG", profile_text, ignore.case=TRUE)) {
         omic_warnings <- paste0(omic_warnings, "EPIGENETIC ASSOCIATIVE WARNING: The patient's signature includes CpG Methylation (Token 7). The CpG methylation data maps to promoter and 5' UTR regions. Aberrant methylation in tumor cells CANNOT be taken as a direct proxy for gene expression. You must interpret this exclusively as an associative correlation. Discuss pharmacological interventions cautiously in the context of this mathematical association.\n")
      }

      # PFI metric-aware context for Pharmacogenomic module (G1 fix)
      if(input$tumor_shap_metric %in% c("OS", "DSS")) {
        metric_context_pharma <- "CRITICAL CONTEXT: The chosen metric is a SURVIVAL metric. SURVIVAL MODEL DIRECTION: S(t) — HIGH = favorable, LOW = unfavorable. Use standard survival terminology: refer to signatures as 'lethal' or 'protective', and use 'stress-adapted survival state' for dynamic tumor-state reasoning."
      } else if(input$tumor_shap_metric == "PFI") {
        metric_context_pharma <- "CRITICAL CONTEXT: The chosen metric is Progression-Free Interval (PFI), an EVENT metric measuring disease progression. CUMULATIVE INCIDENCE MODEL: 1−S(t) — LOW = favorable, HIGH = unfavorable. Prefer progression-specific terminology: pro-progression (increasing hazard) / anti-progression or stabilizing (decreasing hazard). Avoid survival-framing terms (lethal, protective, mortality). For dynamic tumor-state reasoning, use 'stress-adapted persistence state'. Avoid recurrence terminology."
      } else {
        metric_context_pharma <- "CRITICAL CONTEXT: The chosen metric is Disease-Free Interval (DFI), an EVENT metric measuring disease recurrence. CUMULATIVE INCIDENCE MODEL: 1−S(t) — LOW = favorable, HIGH = unfavorable. Prefer recurrence-specific terminology: pro-recurrence (increasing hazard) / anti-recurrence or stabilizing (decreasing hazard). Avoid survival-framing terms (lethal, protective, mortality). For dynamic tumor-state reasoning, use 'stress-adapted persistence state'. Avoid progression terminology."
      }

      global_associativity <- "GLOBAL ASSOCIATIVITY RULE: The multi-omic signatures, SHAP values, and stemness correlations provided represent associative mathematical relationships, not proven causative biological pathways. You MUST frame all pharmacological interventions as targeting associative vulnerabilities, not definitively causative mechanisms."
      # CRIT-05 MITIGATION: ecological fallacy prevention rule injected into system prompt
      phenotype_correlation_rule_pharma <- "\n\n--- ECOLOGICAL FALLACY PREVENTION ---\n\nThe patient's TSM, TMB, and MSI phenotypes (High/Intermediate/Low) are INDIVIDUAL MEASUREMENTS from the stemness database. They are NOT population-level correlations. You MUST reason about the patient using ONLY their own measured values.\n\nFORBIDDEN PATTERNS: 'The patient's TSM may reflect the P-positive correlation...', 'Consistent with the N-negative population-level sign...', 'This aligns with the cohort-level association...', 'Given the population-wide TMB pattern...', 'The P-positive population sign suggests that this patient...'\n\nPERMITTED: 'The patient's individual TSM measurement is Intermediate (RNAss = X), which individually...', 'The patient exhibits Low TMB (X Mut/Mb), which in this individual context may suggest...'\n\nPOPULATION-LEVEL TERMS YOU MUST NEVER USE: 'P-positive', 'N-negative', 'population-level correlation', 'cohort-level correlation', 'population sign', 'population-wide', 'cohort-wide', 'population-level TSM/TMB/MSI'\n\nPRE-OUTPUT SELF-CHECK: Scan every paragraph containing patient-specific markers. If ANY also contains population-level terms, REWRITE using only the patient's own measurements.\n\n--- END ECOLOGICAL FALLACY PREVENTION ---\n"

      strict_nomenclature_rule <- "STRICT NOMENCLATURE RULE: You MUST NOT use, mention, or display ANY part of the signature nomenclatures (e.g., THYM-1460.6.3.N.2.35.5.2.3.3), their abbreviated forms (e.g., THYM-1460, LUAD-1883, LUAD-636), or their dot-prefixed surrogate forms (e.g., .5.3.2.4.14.2.4.1) anywhere in your clinical narrative. These technical provenance identifiers belong exclusively in the audit section. Whenever a signature contributes to the interpretation, you MUST automatically decode it and discuss ONLY the constituent biological gene (e.g., EMP1, ACTA2) and its mechanisms, clinical relevance, and therapeutic associations. NEVER use the phrase 'Signature LUAD-...' or any similar identifier. NEVER translate or alter the provided Gene Symbols. Do not use ellipses to truncate any information. CRITICAL OMIC TOKEN PROHIBITION: The numeric tokens embedded in nomenclature (e.g., .5, .6, .7) are categorical omic-layer identifier codes (Protein, Mutation, CNV, miRNA, Transcript Isoform, mRNA Expression, CpG Methylation) - they are NEVER quantities, counts, dimensions, or layers. You MUST NOT claim a signature spans '6 omic layers' or involves 'seven molecular dimensions' based on these identifier codes."
      tsm_clarification_rule <- "TSM CONTEXTUALIZATION RULE: The patient's personalized TSM classification (provided below) represents their individual tumor state. You MUST explicitly state the patient's individual TSM classification in your synthesis, and discuss how the proposed drugs interact with their individual stemness state."

      nomenclature_quarantine_checklist <- "\n\n--- ⚠️ PRE-OUTPUT NOMENCLATURE QUARANTINE - EXECUTE THIS SELF-AUDIT BEFORE RELEASING YOUR RESPONSE ⚠️ ---\n\nYou MUST scan your COMPLETE output for the following FORBIDDEN patterns and REMOVE them before finalizing:\n\nFAILURE CHECK #1 - FULL SIGNATURE NOMENCLATURES: Scan for ANY string matching the pattern [A-Z]+-\\d+\\.\\d+ (e.g., READ-311.6.3.P.3.2.2.2.4.3, THYM-1460.6.3.N.2.35.5.2.3.3). If found, DELETE the entire nomenclature string. Replace with ONLY the decoded gene symbol (e.g., CHEK1, HJURP, AMIGO2). You MUST NEVER write sentences like 'The READ-311.6.3.P.3.2.2.2.4.3 signature, representing Bulk mRNA Expression of CHEK1...' - instead write 'CHEK1, at the Bulk mRNA Expression layer, aligns with...'\nFAILURE CHECK #2 - ABBREVIATED NOMENCLATURES: Scan for strings like 'READ-311', 'LUAD-1883', or any [A-Z]+-\\d+ prefix. If found, DELETE.\nFAILURE CHECK #3 - CANCER-PREFIXED GENE REFERENCES: The pattern 'the READ CHEK1 signature' or 'the READ-associated signature for CHEK1' containing the cancer abbreviation as a nominal prefix is FORBIDDEN. Write simply 'CHEK1' or 'the CHEK1-associated Bulk mRNA Expression signature'. The cancer cohort context is stated once in your opening paragraph and MUST NOT be recapitulated as a prefix before every gene.\nFAILURE CHECK #4 - DOT-PREFIXED SURROGATE NOMENCLATURES: Scan for ANY string matching the pattern .X.X.X.X... (e.g., .5.3.2.4.14.2.4.1, .6.3.3.30.30.2.4.2, .5.2.2.4.4.4.4.1). These are surrogate naming conventions — numeric dot-separated signature identifiers without a cancer-cohort prefix. If found, DELETE the entire dotted string. NEVER write sentences like 'Signature .5.3.2.4.14.2.4.1 (Transcript, Necrosis)...' — instead write 'the C1QTNF7-associated Transcript Isoform signature (associated with Necrosis)...'.\n\nIf your output contains ANY of the above violations, you MUST rewrite the offending sentences BEFORE releasing your response. The raw nomenclatures belong exclusively in the audit payload section; your pharmacogenomic narrative must contain ONLY gene symbols, drug names, and biological mechanisms.\n\n--- END NOMENCLATURE QUARANTINE CHECKLIST ---\n"

      mrna_terminology_quarantine_checklist <- "\n\n--- ⚠️ PRE-OUTPUT mRNA TERMINOLOGY QUARANTINE - EXECUTE THIS SELF-AUDIT BEFORE RELEASING YOUR RESPONSE ⚠️ ---\n\nYou MUST scan your COMPLETE output for FORBIDDEN bare mRNA references and FIX them before finalizing:\n\nFAILURE CHECK #1 - BARE 'mRNA layer': Replace 'HJURP (mRNA layer)' with 'HJURP (Bulk mRNA Expression layer)'. Replace 'mRNA layer' with 'Bulk mRNA Expression layer'.\nFAILURE CHECK #2 - BARE 'mRNA signature': Replace with 'Bulk mRNA Expression signature' or 'Gene-Level mRNA Expression signature'.\nFAILURE CHECK #3 - BARE 'mRNA element', 'mRNA target', or 'mRNA transcript': These are FORBIDDEN. Use 'Bulk mRNA Expression element/target' or delete 'mRNA transcript' entirely (it belongs to Token .5 Transcript Isoform).\nFAILURE CHECK #4 - Token .6 references: Any gene from the mRNA Expression omic layer (Token .6) MUST be described as 'Bulk mRNA Expression' or 'Gene-Level mRNA Expression'. The bare qualifier 'mRNA' standing alone next to a gene symbol - e.g., 'CHEK1 (mRNA)' - is FORBIDDEN. Write 'CHEK1 (Bulk mRNA Expression)'.\n\nIf your output contains ANY bare 'mRNA' qualifier not preceded by 'Bulk' or 'Gene-Level', you MUST fix it BEFORE releasing your response.\n\n--- END mRNA TERMINOLOGY QUARANTINE CHECKLIST ---\n"

      system_prompt_pharma <- paste0(
  phenotype_correlation_rule_pharma, "\n\n",
  "=== ONCOKB GENE ANNOTATIONS — MANDATORY DISCUSSION REQUIREMENT ===\n",
  "You MUST discuss the OncoKB Gene Annotations for EVERY patient gene that has them.\n",
  "This includes genes classified as ONCOGENE, TSG, ONCOGENE_AND_TSG, or INSUFFICIENT_EVIDENCE.\n",
  "For each annotated gene, state its OncoKB classification and summarize its biological role.\n",
  "You MUST also list which genes lack OncoKB annotation. Do NOT skip this section.\n",
  "This is a non-negotiable requirement — the OncoKB section MUST appear in your output.\n\n",
  "You are an expert clinical pharmacogenomic AI analyzing a patient's top genetic vulnerabilities cross-referenced against the databases listed in the pharmacogenomic matrix. Output a professional clinical synthesis analyzing the reported pharmacogenomic interactions. Do NOT synthesize a treatment plan, formulate therapeutic strategies, or make clinical recommendations. Do NOT name specific experimental or therapeutic intervention techniques (e.g., RNA interference, CRISPR, small molecule inhibition, monoclonal antibodies, gene silencing, antisense oligonucleotides) as strategies to target vulnerabilities. Frame all vulnerabilities purely as biological hypotheses and validation priorities without specifying how they would be experimentally or therapeutically addressed. ", metric_context_pharma, "\n\n",
  "--- PHARMACOGENOMIC EVIDENCE GOVERNANCE v1.1 ---\n\n",
  "The pharmacogenomic matrix you receive is EVIDENCE-TIERED (Tier 1 through Tier 5). You MUST respect these tiers in your synthesis:\n\n",
  "TIER 1 (FDA-recognized / Standard-of-care): Drug-gene associations with the highest level of clinical validation. You may present these as 'supported by regulatory-level evidence' or 'FDA-recognized association'.\n",
  "TIER 2 (Standard care / Strong clinical evidence): Robust clinical evidence. Present as 'supported by clinical evidence' or 'standard-care association'.\n",
  "TIER 3 (Compelling clinical evidence): Promising clinical data. Present as 'supported by clinical evidence, requiring further validation in this disease context'.\n",
  "TIER 4 (Clinical/Preclinical): Early evidence. Present as 'preclinical or early clinical evidence suggests' and explicitly note that validation is needed.\n",
  "TIER 5 (Single-source / Preclinical / Inferential): Weakest evidence. Present as 'limited or single-source evidence' and MUST note that these associations are preliminary and require independent validation.\n\n",
  "--- ONCOKB CONCORDANCE LOCK v1.0 ---\n\n",
  "CRITICAL: OncoKB therapeutic Levels 1-4 (LEVEL_1, LEVEL_2, LEVEL_3A, LEVEL_3B, LEVEL_4) require API access under a license agreement and are not available as downloadable files. The matrix includes gene-level annotations from OncoKB (oncogene/TSG classification, Cancer Gene List membership) and FDA-recognized biomarker-drug associations (Fda2/Fda3) downloaded from the OncoKB public repository, queried via the Tier 0 Clinical Actionability Layer (Stratum 6). OncoKB therapeutic Level 1-4 drug-variant associations are NOT available in this matrix.\n\n",
  "YOU MUST NOT:\n",
  "  • Claim any drug-gene association has 'OncoKB Level 1/2/3A/3B/4' support unless the Clinical_Status column explicitly states it.\n",
  "  • Fabricate or infer OncoKB therapeutic level classifications for associations from DGIdb or CIViC.\n",
  "  • Conflate Fda2/Fda3 (Tier 0 regulatory recognition, Stratum 6) with therapeutic Levels 1-4 (Stratum 7).\n",
  "  • Present Tier 0 evidence (T0A/T0B/T0C) as equivalent to OncoKB therapeutic level evidence.\n\n",
  "YOU MUST:\n",
  "  • Reference only the databases that actually appear in the Source_Database column.\n",
  "  • Report the exact Clinical_Status string as provided — do not reinterpret or elevate it.\n",
  "  • When OncoKB therapeutic levels are absent, acknowledge the license limitation if discussing clinical evidence expectations.\n\n",
  "END ONCOKB CONCORDANCE LOCK\n\n",
  "MANDATORY EVIDENCE-QUALIFICATION RULES:\n",
  "1. Every drug-gene association you discuss MUST be accompanied by its evidence tier qualification.\n",
  "2. You MUST NOT present a Tier 4 or Tier 5 association with the same confidence as a Tier 1 or Tier 2 association. The language must reflect the evidence gradient.\n",
  "3. If multiple drugs target the same gene at different evidence tiers, present them in descending tier order, highlighting the tier differences.\n",
  "4. The Cancer_Type_Context column indicates whether the drug-gene association was annotated in the patient's cancer type or a different disease. An association annotated for a different cancer type MUST be qualified as such (e.g., 'this association was reported in [cancer type], not in [patient's cancer type]').\n",
  "5. If a drug-gene association is from DGIdb without cancer-type annotation, you MUST note that the disease context is unspecified.\n",
  "6. NEVER claim an association is 'clinically validated' or 'FDA-approved' unless the Evidence_Tier is 1 or the Clinical_Status explicitly states FDA approval.\n",
  "7. SOURCE DATABASE INTEGRITY: You MUST ONLY name databases that actually appear in the Source_Database column of the matched pharmacogenomic matrix. If the matrix header lists specific databases, those are the ONLY databases you may reference as evidence sources. Do NOT invent or add database names (e.g., do not claim CIViC contributed if CIViC does not appear in any Source_Database entry). Report the provenance exactly as it appears in the matrix.\n\n",
  "END PHARMACOGENOMIC EVIDENCE GOVERNANCE\n\n",
  "--- CANCER GENE LIST GOVERNANCE v2.0 (MANDATORY VETO) ---\n\n",
  "The CANCER GENE LIST EVIDENCE section provides orthogonal cancer-gene annotations from the OncoKB Cancer Gene Census (v99, May-2026, ~1,240 genes). This is a BIOLOGICAL CONTEXT LAYER (Stratum 5), NOT a therapeutic actionability layer.\n\n",
  "MANDATORY CANCER GENE LIST RULES:\n",
  "1. Report cancer gene list membership as biological context only: state whether patient genes are recognized by major cancer gene resources (OncoKB, COSMIC CGC, Vogelstein, MSK-IMPACT, FoundationOne, FoundationOne Heme).\n",
  "2. Use this information to contextualize findings. Example: 'EGFL7 is recognized as an OncoKB-annotated oncogene and is a member of MSK-IMPACT, FoundationOne, and COSMIC Cancer Gene Census, supporting its relevance to cancer biology.'\n",
  "3. For genes NOT in the cancer gene list, note this appropriately.\n",
  "4. The Gene Type annotation (ONCOGENE, TSG, ONCOGENE_AND_TSG) provides orthogonal classification.\n",
  "5. The resource occurrence score reflects how many of 7 evidence resources recognize the gene.\n",
  "6. CRITICAL: Cancer gene list membership does NOT indicate therapeutic actionability, druggability, or clinical significance for this patient. It is a biological context annotation only.\n",
  "\n",
  "FORBIDDEN WORDS when discussing CGL (Stratum 5): 'actionable', 'druggable', 'therapeutic target', 'clinical actionability', 'treatment relevance'. These belong to Stratum 6 (Tier 0), NOT Stratum 5 (CGL).\n",
  "SAFE VOCABULARY — use ONLY these when discussing CGL: 'recognized cancer gene', 'cancer-relevant', 'oncogenic classification', 'cancer gene census member', 'biological context'.\n",
  "\n",
  "EXAMPLES — WRONG vs RIGHT:\n",
  "  ❌ PROHIBITED: 'CCNE1 is on the Cancer Gene List, indicating it is a clinically actionable target.'\n",
  "  ✅ CORRECT:   'CCNE1 is recognized by the OncoKB Cancer Gene List as a cancer-relevant oncogene.'\n",
  "  ❌ PROHIBITED: 'PGR Cancer Gene List membership confirms its therapeutic relevance.'\n",
  "  ✅ CORRECT:   'PGR is classified as an oncogene in the Cancer Gene List with broad panel recognition.'\n",
  "  ❌ PROHIBITED: 'These cancer gene list members represent druggable vulnerabilities.'\n",
  "  ✅ CORRECT:   'These cancer-relevant genes are recognized across multiple evidence resources.'\n",
  "\n",
  "MANDATORY SELF-VERIFICATION: Before finalizing your response, verify that NO sentence links CGL membership to 'actionable', 'druggable', 'therapeutic target', 'clinical actionability', or 'treatment relevance'. If any such sentence exists, REWRITE it using only the SAFE VOCABULARY above.\n",
  "END CANCER GENE LIST GOVERNANCE\n\n",
  "--- TIER 0 REGULATORY RECOGNITION GOVERNANCE v1.0 (MANDATORY VETO) ---\n\n",
  "The TIER 0 EVIDENCE section (Stratum 6) provides FDA-recognized biomarker-drug associations ",
  "from local TSV files (Fda2/Fda3) and OncoKB-curated gene-cancer annotations (T0C). ",
  "Tier 0 is REGULATORY/CURATORIAL RECOGNITION — NOT a treatment recommendation layer. ",
  "It answers: 'Is this gene-biomarker-drug relationship recognized by regulatory/curatorial bodies?' ",
  "It does NOT answer: 'What treatment should this patient receive?'\n\n",
  "TIER 0 CLASSIFICATION:\n",
  "  T0A = Exact match: gene + alteration + cancer type + drug → FDA/OncoKB recognized\n",
  "  T0B = Gene-level match: gene + cancer type recognized, but alteration/drug context may differ\n",
  "  T0C = OncoKB curated: gene curated by OncoKB without FDA drug labeling\n\n",
  "MANDATORY RULES:\n",
  "1. Present Tier 0 findings as regulatory/curatorial recognition context ONLY.\n",
  "2. ALWAYS qualify T0B matches: 'This biomarker-drug association is recognized at the gene level ",
  "but the specific alteration and drug pairing may differ from the original regulatory context.'\n",
  "3. ALWAYS qualify T0C associations: 'This gene is recognized by OncoKB as cancer-relevant ",
  "but lacks FDA drug labeling for the associated biomarker-drug pairing.'\n",
  "4. NEVER present any Tier 0 classification as a treatment recommendation, prescription guidance, ",
  "or clinical management instruction.\n\n",
  "FORBIDDEN WORDS when discussing Tier 0 (Stratum 6):\n",
  "  'treatment recommendation', 'should receive', 'should be treated with', ",
  "  'is indicated for', 'prescribe', 'therapeutic regimen', 'clinical management', ",
  "  'standard of care for this patient', 'recommended therapy', 'first-line treatment'.\n\n",
  "SAFE VOCABULARY — use ONLY these when discussing Tier 0:\n",
  "  'FDA-recognized biomarker-drug association', 'regulatory recognition', ",
  "  'curated biomarker-drug relationship', 'OncoKB-recognized gene-cancer association', ",
  "  'biomarker evidence recognized by regulatory resources'.\n\n",
  "EXAMPLES — WRONG vs RIGHT:\n",
  "  ❌ PROHIBITED: 'T0A evidence indicates the patient should receive EGFR-targeted therapy.'\n",
  "  ✅ CORRECT:   'EGFR has a T0A FDA-recognized biomarker-drug association in this cancer type, ",
  "representing regulatory-level recognition of its biomarker relevance.'\n",
  "  ❌ PROHIBITED: 'Based on Tier 0 recognition, treatment with this drug is indicated.'\n",
  "  ✅ CORRECT:   'Tier 0 regulatory recognition identifies this gene-drug relationship as ",
  "curated by FDA/OncoKB resources, warranting further biological investigation.'\n",
  "  ❌ PROHIBITED: 'T0B recognition supports the use of this targeted agent.'\n",
  "  ✅ CORRECT:   'This gene has T0B gene-level regulatory recognition, though the specific ",
  "alteration and drug context should be interpreted with cross-context qualification.'\n\n",
  "MANDATORY SELF-VERIFICATION (VETO CHECK): Before finalizing your response, scan every sentence ",
  "that mentions T0A/T0B/T0C or Tier 0. If ANY sentence contains a FORBIDDEN WORD or ",
  "reads as a treatment instruction, DELETE that sentence and rewrite it using ONLY the ",
  "SAFE VOCABULARY. Tier 0 is regulatory recognition — never treatment guidance. ",
  "This is a NON-NEGOTIABLE requirement.\n",
  "END TIER 0 REGULATORY RECOGNITION GOVERNANCE\n\n",
  build_nine_strata_governance_block(), "\n\n",
  "You MUST adhere to the following Omic-Compatibility strict rules:\n\n",
  omic_warnings, "\n",
  global_associativity, "\n\n",
  strict_nomenclature_rule, "\n\n",
  tsm_clarification_rule, "\n\n",
  "**Patient TSM classification:** ", tsm_context, "\n\n",
    "CRITICAL REPORTING INSTRUCTION: In your opening synthesis paragraph, you MUST explicitly state the EXACT numerical values and classifications for TSM, TMB, and MSI provided above. Do not just say 'High' or 'Low'; you must include the specific numbers. If the data explicitly says 'No data available for this patient', you MUST declare that the patient lacks that specific phenotype data.\n\n",

  "DO NOT use robotic formatting, markdown bullet points, numbered lists, or bolding (like **Answer:** or ###) for the main synthesis. Write naturally as a physician would in a clinical chart in continuous paragraphs. \n\nCRITICAL HEDGING & HYPOTHESIS INSTRUCTION: Use hedging language (e.g., 'suggest', 'may indicate that').\n\nFinally, append this exact sentence as a separate paragraph at the very end of your entire output: 'The interpretations presented above should be considered hypothesis-generating and are intended to support biological and clinical exploration. The proposed mechanisms, therapeutic associations, and disease trajectories are inferred from machine-learning models, statistical associations, multi-omic relationships, and literature-supported evidence. These findings do not establish direct causality and should be interpreted within the context of the available data, requiring independent experimental and clinical validation whenever appropriate.'", nomenclature_quarantine_checklist, mrna_terminology_quarantine_checklist,
  rcd_biological_context_decoder,
  llm_glossary, omic_layer_terminology_governance, tsm_ontology_protection,
  endpoint_vocabulary_governance, omic_layer_consistency_governance
)

      oncokb_gene_section <- paste(oncokb_gene_text, collapse = "\n")

      # V-003 mitigation: compute actual database sources present in matched data
      # to prevent LLM from hallucinating databases that did not contribute
      actual_db_sources <- unique(matched_drugs$Source_Database)
      actual_db_sources <- actual_db_sources[!is.na(actual_db_sources) & actual_db_sources != ""]
      pharma_db_label <- if (length(actual_db_sources) > 0) {
        paste(actual_db_sources, collapse = " + ")
      } else {
        "DGIdb + CIViC"
      }
      pharma_db_header <- paste0("Matched Pharmacogenomic Matrix (", pharma_db_label, "):\n")

      # ---- TIER 0: ONCOKB CLINICAL ACTIONABILITY QUERY ----
      # Stratum 6 of the nine-strata architecture: Regulatory/Curatorial evidence
      # Queries the FDA biomarker-drug bridge + OncoKB TSV to produce T0A/T0B/T0C matches
      tier0_result <- tryCatch({
        matrix_drugs <- if (nrow(matched_drugs) > 0 && "Drug_Name" %in% colnames(matched_drugs)) {
          unique(matched_drugs$Drug_Name)
        } else NULL

        pat <- query_patient_actionability(
          genes       = patient_genes,
          layer       = oncokb_actionability_layer,
          cancer_type = input$tumor_shap_cancer,
          drug_names  = matrix_drugs
        )
        message(sprintf("[Tier0] Actionability: T0A=%d T0B=%d T0C=%d for %d genes in %s",
                        pat$tier0$n_t0a, pat$tier0$n_t0b, pat$tier0$n_t0c,
                        length(patient_genes), input$tumor_shap_cancer))
        pat
      }, error = function(e) {
        message("[Tier0] Query failed: ", e$message)
        NULL
      })
      # -----------------------------------------------------------------------

      # — Prompt size guard: estimate total tokens and truncate if needed —
      # DeepSeek-chat context limit ~64K tokens (~256K chars). Budget: system ~25K chars,
      # user payload must fit within ~100K chars (25K tokens) to leave headroom.
      MAX_USER_CHARS <- 100000
      sys_chars <- nchar(system_prompt_pharma)
      budget <- MAX_USER_CHARS

      # Truncation helpers
      truncate_preserving_lines <- function(txt, max_chars) {
        if (length(txt) > 1) txt <- paste(txt, collapse = "\n")
        if (nchar(txt) <= max_chars) return(txt)
        lines_vec <- strsplit(txt, "\n")[[1]]
        out <- character(0)
        used <- 0L
        for (ln in lines_vec) {
          if (used + nchar(ln) + 1 > max_chars) break
          out <- c(out, ln)
          used <- used + nchar(ln) + 1
        }
        msg <- sprintf("\n[... TRUNCATED: %d chars -> %d chars due to LLM context limit ...]\n", nchar(txt), used)
        paste(paste(out, collapse = "\n"), msg, sep = "")
      }

      # Build user prompt components, allocating budget from most-critical to least
      header <- paste0("Patient ID: ", input$tumor_shap_patient, "\n\n")
      gene_whitelist_block <- paste0(
        "\n\nTOP 5 SIGNATURE-DRIVING GENES — These are the primary molecular drivers ",
        "of this patient's SHAP trajectory: ",
        paste(unique(patient_genes), collapse = ", "),
        ". Center your clinical discussion on these driver genes. ",
        "You MAY discuss additional biologically connected genes (pathway partners, downstream targets, ",
        "interacting regulators) when biologically justified. ",
        "You MUST NOT import canonical cancer genes (EGFR, TP53, TNF, BRAF, BRCA1, GPX4, ",
        "CDK2, SMAD2, FGFR3, ATM, APC) unless they appear in the driver gene list or are ",
        "directly biologically connected to a driver gene. ",
        "Unconnected gene imports from training knowledge remain GOVERNANCE VIOLATIONS.\n\n",
        "SELF-VERIFICATION: Before writing, confirm each gene you mention is either: ",
        "(a) in the driver gene list above, or (b) biologically connected to a driver gene. ",
        "Do NOT 'knowledge-complete' by importing unrelated canonical cancer genes."
      )
      analysis_instr <- paste0(
        "ANALYSIS INSTRUCTION: Analyze across the nine-strata evidence architecture. ",
        "The Tier 0 section (Stratum 6) provides regulatory/curatorial biomarker-drug recognition ",
        "(T0A=exact match, T0B=gene-level, T0C=OncoKB curated). ",
        "The Pharmacogenomic Matrix (Stratum 7) provides database-reported gene-drug interactions (Tiers 1-5). ",
        "The Cancer Gene List (Stratum 5) provides biological cancer relevance context. ",
        "These are DISTINCT evidence dimensions. ",
        "Present Tier 0 regulatory recognition separately from pharmacogenomic interactions. ",
        "Qualify T0B matches by their cross-context nature and T0C associations by their lack of FDA drug labeling. ",
        "Do NOT present Tier 0 evidence as a treatment recommendation. ",
        "Do NOT conflate cancer gene membership with clinical actionability.",
        gene_whitelist_block
      )

      # Component sizes
      profile_hdr <- "\nPatient's Specific Multi-Omic Expression Profile:\n"
      oncokb_hdr <- "\nOncoKB Gene Annotations (MSKCC Precision Oncology Knowledge Base):\n"
      cgl_hdr <- "\n"
      tier0_hdr <- "\n"
      pharma_hdr <- "\n"

      # Fixed overhead
      fixed_overhead <- nchar(header) + nchar(analysis_instr) +
        nchar(profile_hdr) + nchar(oncokb_hdr) + nchar(cgl_hdr) +
        nchar(tier0_hdr) + nchar(pharma_hdr)

      rem <- budget - fixed_overhead
      # Allocate: profile 40%, drug_summary 25%, oncokb 15%, cgl 10%, tier0 10%
      budget_profile  <- max(1000, floor(rem * 0.40))
      budget_drugs    <- max(1000, floor(rem * 0.25))
      budget_oncokb   <- max(500,  floor(rem * 0.15))
      budget_cgl      <- max(500,  floor(rem * 0.10))
      budget_tier0    <- max(500,  floor(rem * 0.10))

      profile_trimmed   <- truncate_preserving_lines(profile_text, budget_profile)
      profile_trimmed_audit <- if (!is.null(profile_text_audit) && profile_text_audit != "") {
        truncate_preserving_lines(profile_text_audit, budget_profile)
      } else {
        profile_trimmed
      }
      drugs_trimmed     <- truncate_preserving_lines(drug_summary, budget_drugs)
      oncokb_trimmed    <- truncate_preserving_lines(oncokb_gene_section, budget_oncokb)
      cgl_trimmed       <- truncate_preserving_lines(cancer_gene_list_text, budget_cgl)

      tier0_block <- if (!is.null(tier0_result) && !is.null(tier0_result$tier0_narrative)) {
        truncate_preserving_lines(paste0(tier0_result$tier0_narrative), budget_tier0)
      } else ""

      user_prompt_pharma <- paste0(
        header,
        profile_hdr, profile_trimmed, "\n",
        rcd_biological_context_decoder, "\n",
        oncokb_hdr, oncokb_trimmed, "\n",
        cgl_hdr, cgl_trimmed, "\n",
        tier0_hdr, tier0_block, "\n",
        pharma_hdr, pharma_db_header, drugs_trimmed, "\n\n",
        analysis_instr
      )
      # AUDIT VERSION: preserves full signature nomenclatures for provenance traceability
      user_prompt_pharma_audit <- paste0(
        header,
        profile_hdr, profile_trimmed_audit, "\n",
        rcd_biological_context_decoder, "\n",
        oncokb_hdr, oncokb_trimmed, "\n",
        cgl_hdr, cgl_trimmed, "\n",
        tier0_hdr, tier0_block, "\n",
        pharma_hdr, pharma_db_header, drugs_trimmed, "\n\n",
        analysis_instr
      )

      message(sprintf("[Pharma-Prompt] system=%d chars | user=%d chars | total=%d chars",
        sys_chars, nchar(user_prompt_pharma), sys_chars + nchar(user_prompt_pharma)))

      # --- ASYNC LLM DISPATCH: Capture config and inputs, run non-blocking ---
      status <- check_llm_status()
      if (!status$success) {
        session$sendCustomMessage("hide_spinner", list())
        rv_pharma_llm_text(paste0("ERROR: ", status$message))
        return()
      }
      cfg <- capture_llm_config()
      start_t <- Sys.time()
      sid <- my_session_id
      pat_id <- input$tumor_shap_patient
      pat_cohort <- input$tumor_shap_cancer

      msgs_pharma <- list(list(role = "system", content = system_prompt_pharma), list(role = "user", content = user_prompt_pharma))
      sys_prompt_ph <- system_prompt_pharma
      usr_prompt_ph <- user_prompt_pharma
      prof_txt <- profile_text
      patient_pheno_val <- rv_patient_phenotype()
      patient_nom_val <- rv_active_patient_nomenclatures()
      usr_prompt_ph_audit <- user_prompt_pharma_audit

      future::future({
        pharma_response <- send_llm_request(msgs_pharma, cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url)

        pharma_response <- gsub("(?s)^.*?</think>\\s*", "", pharma_response, perl = TRUE)
        pharma_response <- gsub("*", "", pharma_response, fixed = TRUE)
        if (is.na(pharma_response) || !is.character(pharma_response) || length(pharma_response) == 0L) {
          stop("Post-processing failure: pharma_response became NA/empty after gsub.")
        }
        audit_collector_ph <- list()
        pharma_response <- scrub_governance_violations(pharma_response)
        audit_collector_ph[[length(audit_collector_ph) + 1]] <- attr(pharma_response, "audit_actions")
        eco_result <- check_ecological_fallacy(pharma_response, pat_id, "Pharmacogenomic")
        pharma_response <- eco_result$annotated_text
        eco_count_ph <- eco_result$violation_count

        fact_issues_count_ph <- 0
        for (retry_i in seq_len(3)) {
          fact_result_ph <- tryCatch(
            validate_llm_factuality(pharma_response, pat_id, "Pharmacogenomic", paste(prof_txt, patient_pheno_val, sep = "\n"), patient_nom_val),
            error = function(e2) {
              message("[WARN] validate_llm_factuality (pharma) failed: ", e2$message)
              list(issues = list(), factuality_score = 0)
            }
          )
          fact_issues_count_ph <- length(fact_result_ph$issues)
          if (fact_issues_count_ph == 0 || retry_i > 2) break
          correction_instruction <- paste0(
            "\n\nCRITICAL FACTUALITY CORRECTION (RETRY ", retry_i, "/2): Your previous response contained ", fact_issues_count_ph,
            " factual inconsistency/ies. CORRECT these and regenerate.\n"
          )
          pharma_response <- send_llm_request(
            list(list(role = "system", content = paste0(sys_prompt_ph, correction_instruction)),
                 list(role = "user", content = paste0(usr_prompt_ph, "\n\n[FACTUALITY REGENERATION]"))),
            cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url
          )
          pharma_response <- gsub("(?s)^.*?</think>\\s*", "", pharma_response, perl = TRUE)
          pharma_response <- gsub("*", "", pharma_response, fixed = TRUE)
          pharma_response <- scrub_governance_violations(pharma_response)
        }
        pharma_response <- ensure_questions_and_disclaimer(pharma_response, add_questions = FALSE, patient_id = pat_id, cohort = pat_cohort)
        audit_collector_ph[[length(audit_collector_ph) + 1]] <- attr(pharma_response, "audit_actions")

        # Parse suggested clinical queries
        chat_choices <- NULL
        parts <- strsplit(pharma_response, "(?i)Suggested clinical queries:", perl = TRUE)[[1]]
        if (length(parts) > 1) {
          queries_part <- parts[length(parts)]
          splits <- strsplit(queries_part, "\\?(?:\\s+|$)", perl = TRUE)[[1]]
          parsed_queries <- trimws(splits[splits != "" & !grepl("^\\s*$", splits)])
          if (length(parsed_queries) > 0) {
            qs <- c()
            for (q in parsed_queries) {
              q <- gsub("The interpretations presented above.*appropriate\\.?", "", q, ignore.case = TRUE)
              q <- gsub("^(?:[1-9][\\.\\)]\\s*)?(?:-{1,2}|[*•])\\s*", "", trimws(q))
              q <- gsub("\\n", " ", q)
              q <- gsub("\\s+", " ", q)
              q <- trimws(q)
              if (nchar(q) > 5) qs <- c(qs, paste0(q, "?"))
            }
            if (length(qs) > 0) chat_choices <- qs
          }
        }

        elapsed_s <- round(as.numeric(difftime(Sys.time(), start_t, units = "secs")), 1)
        list(text = pharma_response, audit_badge = build_governance_audit_badge(audit_collector_ph, eco_count_ph, fact_issues_count_ph),
             elapsed = elapsed_s, chat_choices = chat_choices, payload_audit = usr_prompt_ph_audit)
      }, seed = TRUE) %...>% (function(res) {
        release_queue(sid)
        session$sendCustomMessage("hide_spinner", list())

        rv_pharma_llm_text(res$text)
        rv_pharma_audit(res$audit_badge)
        rv_pharma_llm_payload(res$payload_audit)

        elapsed_s <- res$elapsed
        log_file <- "llm_performance_log.csv"
        new_row <- data.frame(Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Module = "Pharmacogenomic", Time_secs = elapsed_s, stringsAsFactors = FALSE)
        if (!file.exists(log_file)) write.csv(new_row, log_file, row.names = FALSE) else write.table(new_row, log_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
        time_str <- paste0("Automatically generated on ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), " (Processing Time: ", elapsed_s, "s)")
        rv_pharma_llm_time(time_str)

        if (!is.null(res$chat_choices)) {
          new_choices <- c("Custom (Type your own query below)" = "")
          for (qc in res$chat_choices) new_choices[qc] <- qc
          updateSelectInput(session, "chat_prompt_examples", choices = new_choices)
        }
      }) %...!% (function(e) {
        release_queue(sid)
        session$sendCustomMessage("hide_spinner", list())
        rv_pharma_llm_text(paste0("ERROR: The AI Engine failed to execute pharmacogenomic translation. ", conditionMessage(e)))
        rv_pharma_llm_payload(NULL)
      })
      return(NULL)
    })

    output$pharmacogenomic_report_ui <- renderUI({
      txt <- rv_pharma_llm_text()
      if (is.null(txt)) return(NULL)

      if(startsWith(txt, "ERROR:")) {
         return(tags$div(
           style="background: rgba(239, 68, 68, 0.1); border: 1px solid rgba(239, 68, 68, 0.3); border-radius: 8px; padding: 15px; margin-top: 20px;",
           tags$p(style="color: #f87171; font-weight: bold;", txt)
         ))
      }

      txt <- gsub("\\\\*", "", txt) # Strip all asterisks
      txt <- gsub("###", "", txt) # Strip headers

      paragraphs <- unlist(strsplit(txt, "\n\n+"))
      ui_paragraphs <- lapply(paragraphs, function(p) {
        tags$p(style="color: #e2e8f0; font-size: 15px; line-height: 1.7;", p)
      })

      payload_ui <- if (!is.null(rv_pharma_llm_payload())) {
        oncokb_drug_present <- any(grepl("OncoKB", unified_drug_matrix$Source_Database, ignore.case = TRUE))
        oncokb_note <- if (!oncokb_drug_present) {
          tags$div(
            style="margin-top: 8px; padding: 8px 10px; background: rgba(239, 68, 68, 0.08); border-left: 3px solid #ef4444; border-radius: 4px;",
            tags$p(style="font-size: 0.7rem; color: #fca5a5; margin: 0;",
              tags$strong(bs_icon("exclamation-triangle"), " OncoKB therapeutic-level drug-variant associations (Levels 1–4) are not present in this matrix. "),
              "These require API access under a license agreement and are not available as downloadable files. The matrix includes CIViC + DGIdb drug-gene associations (Tiers 2–5) and OncoKB gene-level annotations (oncogene/TSG classification, Cancer Gene List). Tier 0 FDA-recognized biomarker-drug associations (Fda2/Fda3) were downloaded from the OncoKB public repository and are queried per patient via the Tier 0 Clinical Actionability Layer (Stratum 6)."
            )
          )
        } else { NULL }
        tags$details(
          style="margin-top: 15px; padding: 10px; background: rgba(15, 23, 42, 0.4); border: 1px solid #334155; border-radius: 6px;",
          tags$summary(style="color: #94a3b8; font-size: 0.85rem; cursor: pointer;", bs_icon("code-square"), " Audit Pharmacogenomic Payload (Raw Injection)"),
          oncokb_note,
          tags$pre(style="margin-top: 10px; font-size: 0.75rem; color: #cbd5e1; white-space: pre-wrap; word-wrap: break-word;", rv_pharma_llm_payload())
        )
      } else { NULL }

      time_ui <- if(!is.null(rv_pharma_llm_time())) {
        tags$div(style="font-size:10px; color:#94a3b8; text-align:right; margin-top:15px;", rv_pharma_llm_time())
      } else { NULL }

      tags$div(
        style="background: rgba(212, 175, 55, 0.1); border: 1px solid rgba(212, 175, 55, 0.3); border-radius: 8px; padding: 20px; margin-top: 20px;",
        tags$h4(style="color: #FFDF00; font-weight: bold; margin-top: 0;", bs_icon("capsule-pill"), " Pharmacogenomic Translation Matrix"),
        ui_paragraphs,
        tags$div(style="margin-top: 15px; padding: 10px; border-left: 3px solid #f59e0b; background: rgba(245, 158, 11, 0.1);",
          tags$p(style="font-size: 12px; color: #fbbf24; margin-bottom: 0;", tags$strong("CAUTION: "), "This pharmacogenomic assessment is AI-generated for academic research purposes and is NOT intended for clinical decision-making or patient care.")
        ),
        if (!is.null(rv_pharma_audit())) rv_pharma_audit() else NULL,
        payload_ui,
        time_ui
      )
    })

    rv_chat_history <- reactiveVal(list())

    observeEvent(input$chat_prompt_examples, {
       if(input$chat_prompt_examples != "") {
          updateTextInput(session, "tumor_chat_input", value = input$chat_prompt_examples)
       }
    })

    observeEvent(input$btn_clear_chat, {
      rv_chat_history(list())
    })

    observeEvent(input$btn_run_chat_llm, {
      req(input$tumor_chat_input)

      # Quick check if LLM is available
      status <- check_llm_status()
      if (!status$success) {
         session$sendCustomMessage("hide_spinner", list())
         showNotification(paste0("LLM Service Error: ", status$message), type = "error")
         return()
      }

      rv_pending_task("chat")

      # DeepSeek: no queue needed (cloud API handles parallel requests)
      if (shiny::isolate(llm_config$backend) == "deepseek") {
        rv_in_queue(FALSE)
        rv_trigger_chat_llm(rv_trigger_chat_llm() + 1)
        return()
      }

      # Ollama: use queue (single-process LLM)
      pos <- join_queue(my_session_id)

      if (pos == 0L) {
        rv_in_queue(FALSE)
        rv_trigger_chat_llm(rv_trigger_chat_llm() + 1)
      } else {
        rv_in_queue(TRUE)
        rv_queue_pos(pos)
        session$sendCustomMessage("hide_spinner", list())
      }
    })

    observeEvent(rv_trigger_chat_llm(), {
      req(rv_trigger_chat_llm() > 0)
      on.exit({
        release_queue(my_session_id)
      })

      user_msg <- input$tumor_chat_input
      if (user_msg == "") {
         return()
      }

      # Append user message to history
      current_history <- rv_chat_history()
      current_history <- append(current_history, list(list(role = "user", content = user_msg)))
      rv_chat_history(current_history)

      updateTextInput(session, "tumor_chat_input", value = "")

      # --- OUTER SAFETY TRYCATCH: catches ALL errors in the chat pipeline ---
      chat_error_handler <- function(e) {
        current_history <- rv_chat_history()
        current_history <- append(current_history, list(list(role = "assistant", content = paste0("ERROR: The AI Engine failed. ", e$message))))
        rv_chat_history(current_history)
      }

      tryCatch({

      # --- SERVER-SIDE DOMAIN BOUNDARY ENFORCEMENT (LLM-BASED CLASSIFIER) ---
      domain_cat <- classify_query_domain(user_msg)
      if (isTRUE(domain_cat == "5")) {
        current_history <- rv_chat_history()
        current_history <- append(current_history, list(list(role = "assistant", content = approved_out_of_scope_response)))
        rv_chat_history(current_history)
        return()
      }
      if (isTRUE(domain_cat == "4")) {
        current_history <- rv_chat_history()
        current_history <- append(current_history, list(list(role = "assistant", content = approved_category_4_response)))
        rv_chat_history(current_history)
        return()
      }

      # Inject active cohort data context into the Chat LLM prompt
      shared_profile <- rv_active_patient_profile()
      if (is.null(shared_profile) || identical(shared_profile, "") || identical(shared_profile, NA_character_)) {
          cohort_row <- raw_cohort_matrix[raw_cohort_matrix$CTAB == input$tumor_shap_cancer & raw_cohort_matrix$Metric == input$tumor_shap_metric, ]
          signature <- if(nrow(cohort_row) > 0) cohort_row$Primary_Signature[1] else "Unknown Signature"
          signature_sanitized <- sanitize_nomenclature_for_llm(signature)
          cohort_context <- paste0("\n\nCRITICAL CONTEXT: You are currently active within the Digital Molecular Tumor Board, analyzing the ", input$tumor_shap_cancer, " cohort. ",
                                   "The specific multi-omic signature predicting survival in this patient population is mathematically driven by the following biological signature: ", signature_sanitized, ". ",
                                   "If the user asks questions about the cohort, base your answers strictly on this specific genetic signature and do not hallucinate other genes.")
      } else {
          cohort_context <- paste0("\n\nCRITICAL CONTEXT: You are currently active within the Digital Molecular Tumor Board, analyzing patient ", input$tumor_shap_patient, " from the ", input$tumor_shap_cancer, " cohort. ",
                                   shared_profile, " ",
                                   "If the user asks questions, base your answers strictly on this exact genetic profile and do not hallucinate other genes.")
      }

      # PFI metric-aware context for Chat module (G1 fix)
      if(input$tumor_shap_metric %in% c("OS", "DSS")) {
        metric_context_chat <- "CRITICAL CONTEXT: The chosen metric is a SURVIVAL metric. SURVIVAL MODEL DIRECTION: S(t) — HIGH = favorable, LOW = unfavorable. Use standard survival terminology: refer to signatures as 'lethal' or 'protective', and use 'stress-adapted survival state' for dynamic tumor-state reasoning."
      } else if(input$tumor_shap_metric == "PFI") {
        metric_context_chat <- "CRITICAL CONTEXT: The chosen metric is Progression-Free Interval (PFI), an EVENT metric measuring disease progression. CUMULATIVE INCIDENCE MODEL: 1−S(t) — LOW = favorable, HIGH = unfavorable. Prefer progression-specific terminology: pro-progression (increasing hazard) / anti-progression or stabilizing (decreasing hazard). Avoid survival-framing terms (lethal, protective, mortality). For dynamic tumor-state reasoning, use 'stress-adapted persistence state'. Avoid recurrence terminology."
      } else {
        metric_context_chat <- "CRITICAL CONTEXT: The chosen metric is Disease-Free Interval (DFI), an EVENT metric measuring disease recurrence. CUMULATIVE INCIDENCE MODEL: 1−S(t) — LOW = favorable, HIGH = unfavorable. Prefer recurrence-specific terminology: pro-recurrence (increasing hazard) / anti-recurrence or stabilizing (decreasing hazard). Avoid survival-framing terms (lethal, protective, mortality). For dynamic tumor-state reasoning, use 'stress-adapted persistence state'. Avoid progression terminology."
      }

      # Inject patient phenotype data into Chat context (G3 fix)
      patient_pheno_chat <- rv_patient_phenotype()
      if (is.null(patient_pheno_chat) || identical(patient_pheno_chat, "") || identical(patient_pheno_chat, NA_character_)) {
        patient_pheno_chat <- "\n\nPATIENT-SPECIFIC PHENOTYPE CLASSIFICATIONS: Not yet available. Generate the SHAP Clinical Synthesis first to populate TSM, TMB, and MSI data."
      }

      # Build messages for Ollama (System prompt + History)
      global_associativity <- "GLOBAL ASSOCIATIVITY RULE: The multi-omic signatures, SHAP values, and stemness correlations provided represent associative mathematical relationships, not proven causative biological pathways. You MUST frame all pharmacological interventions as targeting associative vulnerabilities, not definitively causative mechanisms."
      # CRIT-05 MITIGATION: ecological fallacy prevention rule injected into system prompt
      phenotype_correlation_rule_chat <- "\n\n--- ECOLOGICAL FALLACY PREVENTION ---\n\nThe patient's TSM, TMB, and MSI phenotypes (High/Intermediate/Low) are INDIVIDUAL MEASUREMENTS. They are NOT population-level correlations. You MUST reason about the patient using ONLY their own measured values.\n\nFORBIDDEN: 'The patient's TSM may reflect the P-positive correlation...', 'Consistent with the N-negative population-level sign...', 'This aligns with the cohort-level association...', 'Given the population-wide TMB pattern...'\n\nNEVER USE: 'P-positive', 'N-negative', 'population-level correlation', 'cohort-level correlation', 'population sign', 'population-wide', 'cohort-wide', 'population-level TSM/TMB/MSI'\n\nPRE-OUTPUT SELF-CHECK: Scan every paragraph. If ANY contains population-level terms, REWRITE using only patient's own measurements.\n\n--- END ECOLOGICAL FALLACY PREVENTION ---\n"
      strict_nomenclature_rule <- "STRICT NOMENCLATURE RULE: You MUST NOT use, mention, or display ANY part of the signature nomenclatures (e.g., THYM-1460.6.3.N.2.35.5.2.3.3), their abbreviated forms (e.g., THYM-1460, LUAD-1883, LUAD-636), or their dot-prefixed surrogate forms (e.g., .5.3.2.4.14.2.4.1) anywhere in your clinical narrative. These technical provenance identifiers belong exclusively in the audit section. Whenever a signature contributes to the interpretation, you MUST automatically decode it and discuss ONLY the constituent biological gene (e.g., EMP1, ACTA2) and its mechanisms, clinical relevance, and therapeutic associations. NEVER use the phrase 'Signature LUAD-...' or any similar identifier. NEVER translate or alter the provided Gene Symbols. Do not use ellipses to truncate any information. CRITICAL OMIC TOKEN PROHIBITION: The numeric tokens embedded in nomenclature (e.g., .5, .6, .7) are categorical omic-layer identifier codes (Protein, Mutation, CNV, miRNA, Transcript Isoform, mRNA Expression, CpG Methylation) - they are NEVER quantities, counts, dimensions, or layers. You MUST NOT claim a signature spans '6 omic layers' or involves 'seven molecular dimensions' based on these identifier codes."

      mrna_terminology_quarantine_checklist <- "\n\n--- PRE-OUTPUT mRNA TERMINOLOGY QUARANTINE --- EXECUTE THIS SELF-AUDIT BEFORE RELEASING YOUR RESPONSE ---\n\nYou MUST scan your COMPLETE output for FORBIDDEN bare mRNA references and FIX them before finalizing:\n\nFAILURE CHECK #1 -- BARE mRNA layer: Replace HJURP (mRNA layer) with HJURP (Bulk mRNA Expression layer).\nFAILURE CHECK #2 -- BARE mRNA signature: Replace with Bulk mRNA Expression signature or Gene-Level mRNA Expression signature.\nFAILURE CHECK #3 -- BARE mRNA element, mRNA target, or mRNA transcript: These are FORBIDDEN.\nFAILURE CHECK #4 -- Token .6 references: MUST be described as Bulk mRNA Expression or Gene-Level mRNA Expression.\n\nIf your output contains ANY bare mRNA qualifier, you MUST fix it BEFORE releasing your response.\n\n--- END mRNA TERMINOLOGY QUARANTINE CHECKLIST ---\n"
      system_prompt_chat <- paste0(
        "--- ⚠️ YOUR MOST IMPORTANT INSTRUCTION - READ THIS FIRST ⚠️ ---\n\n",
        domain_boundary_governance,
        "\n\n--- END OF DOMAIN BOUNDARY CHECK ---\n\n",
        "Now, with that boundary strictly enforced, here is your role and context:\n\nCRITICAL TERMINOLOGY PRECONDITION \u2014 READ BEFORE WRITING ANY OUTPUT:\n\nWhen you encounter a signature from the mRNA Expression omic layer (Token .6), you MUST write 'Bulk mRNA Expression' or 'Gene-Level mRNA Expression'. The bare word 'mRNA' is FORBIDDEN. Any gene from the Transcript layer (Token .5) MUST be described as 'Transcript Isoform'. There are NO exceptions.\n\n",
        "You are the Digital Molecular Tumor Board AI, an expert clinical molecular oncologist integrated directly into the CancerRCDPredictor platform. You have direct access to the user's Phase III clinical probability datasets. When the user asks if you have access to their data or what you are based on, you MUST enthusiastically confirm that you are reading their local ZIMA data arrays and analyzing the currently loaded cohort. Answer the user's clinical questions concisely, fluidly, and professionally. DO NOT use robotic formatting, markdown headers, bullet points, or excessive bolding (like **Answer:** or ###). Speak naturally in continuous paragraphs.\n\nCRITICAL HEDGING & HYPOTHESIS INSTRUCTION: Because your analysis is partially based on multi-omics data predictions, you MUST use hedging language. Replace strong claims ('demonstrate', 'prove', 'establish', 'likely') with cautious terms ('suggest', 'are consistent with', 'are compatible with', 'raise the possibility that', 'may indicate that', 'could reflect'). Explicitly frame your clinical answers as a hypothesis or model rather than a definitive conclusion.\n\n",
        metric_context_chat, "\n\n",
        patient_pheno_chat, "\n\n",
        phenotype_correlation_rule_chat, "\n\n",
        strict_nomenclature_rule, "\n\n",
        global_associativity, "\n\n",
        "--- ⚠️ PRE-OUTPUT NOMENCLATURE QUARANTINE - EXECUTE THIS SELF-AUDIT BEFORE RELEASING YOUR RESPONSE ⚠️ ---\n\nYou MUST scan your COMPLETE output for the following FORBIDDEN patterns and REMOVE them before finalizing:\n\nFAILURE CHECK #1 - FULL SIGNATURE NOMENCLATURES: Scan for ANY string matching the pattern [A-Z]+-\\d+\\.\\d+ (e.g., READ-311.6.3.P.3.2.2.2.4.3, THYM-1460.6.3.N.2.35.5.2.3.3). If found, DELETE the entire nomenclature string. Replace with ONLY the decoded gene symbol (e.g., CHEK1, HJURP, AMIGO2).\nFAILURE CHECK #2 - ABBREVIATED NOMENCLATURES: Scan for strings like 'READ-311', 'LUAD-1883', or any [A-Z]+-\\d+ prefix. If found, DELETE.\nFAILURE CHECK #3 - CANCER-PREFIXED GENE REFERENCES: The pattern 'the READ CHEK1 signature' or any cancer-abbreviation-prefixed gene reference is FORBIDDEN. Write simply 'CHEK1' or 'the CHEK1-associated Bulk mRNA Expression signature'.\nFAILURE CHECK #4 - DOT-PREFIXED SURROGATE NOMENCLATURES: Scan for ANY string matching the pattern .X.X.X.X... (e.g., .5.3.2.4.14.2.4.1, .6.3.3.30.30.2.4.2). These are surrogate naming conventions — numeric dot-separated signature identifiers without a cancer-cohort prefix. If found, DELETE the entire dotted string. NEVER write sentences like 'Signature .5.3.2.4.14.2.4.1...' — instead write 'the C1QTNF7-associated Transcript Isoform signature'.\n\nIf your output contains ANY of the above violations, you MUST rewrite the offending sentences BEFORE releasing your response. The audit payload section handles provenance; your narrative must contain ONLY gene symbols and biological mechanisms.\n\n--- END NOMENCLATURE QUARANTINE CHECKLIST ---\n\n",
        "Finally, at the very end of every single response you generate (UNLESS you are issuing the domain restriction refusal), you MUST append this exact sentence as a separate concluding thought: 'The interpretations presented above should be considered hypothesis-generating and are intended to support biological and clinical exploration. The proposed mechanisms, therapeutic associations, and disease trajectories are inferred from machine-learning models, statistical associations, multi-omic relationships, and literature-supported evidence. These findings do not establish direct causality and should be interpreted within the context of the available data, requiring independent experimental and clinical validation whenever appropriate.' Do NOT append this sentence if you refused the question.",
        cohort_context,
        mrna_terminology_quarantine_checklist,
        rcd_biological_context_decoder,
        narrative_governance_framework,
        llm_glossary,
        omic_layer_terminology_governance,
        tsm_ontology_protection,
        endpoint_vocabulary_governance,
        omic_layer_consistency_governance
      )

      messages_payload <- list(list(role = "system", content = system_prompt_chat))
      messages_payload <- c(messages_payload, current_history)

        # --- ASYNC LLM DISPATCH: Capture config and inputs, run non-blocking ---
        status <- check_llm_status()
        if (!status$success) {
          msg <- status$message
          current_history <- rv_chat_history()
          current_history <- append(current_history, list(list(role = "assistant", content = paste0("ERROR: ", msg))))
          rv_chat_history(current_history)
          session$sendCustomMessage("hide_spinner", list())
          return()
        }
        cfg <- capture_llm_config()
        start_t <- Sys.time()
        sid <- my_session_id
        pat_id <- input$tumor_shap_patient
        chat_profile_val <- rv_shap_llm_payload()
        msgs <- messages_payload
        cur_hist <- current_history

        future::future({
          chat_response <- send_llm_request(msgs, cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url)
          chat_response <- gsub("(?s)^.*?</think>\\s*", "", chat_response, perl = TRUE)
          audit_collector_ch <- list()
          chat_response <- scrub_governance_violations(chat_response)
          eco_result <- check_ecological_fallacy(chat_response, pat_id, "Clinical_QA")
          chat_response <- eco_result$annotated_text

          if (!is.null(chat_profile_val) && safe_nchar(chat_profile_val) > 0L) {
            chat_fact_result <- tryCatch(
              validate_llm_factuality(chat_response, pat_id, "Clinical_QA", chat_profile_val),
              error = function(fact_err) {
                message("[WARN] validate_llm_factuality (Clinical_QA) failed: ", fact_err$message)
                list(issues = list())
              }
            )
            if (length(chat_fact_result$issues) > 0) {
              correction_note <- paste0(
                "\n\nFACTUALITY CORRECTION REQUIRED: Your previous response contained ",
                length(chat_fact_result$issues), " factual error(s). Correct them and regenerate:\n",
                paste(sapply(chat_fact_result$issues, function(iss) paste0("- ", iss$detail)), collapse = "\n"),
                "\n\nVERIFY every gene symbol, SHAP value, SHAP sign, and phenotype classification against the input payload."
              )
              retry_messages <- c(msgs,
                list(list(role = "assistant", content = chat_response)),
                list(list(role = "user", content = correction_note)))
              chat_response <- tryCatch({
                r <- send_llm_request(retry_messages, cfg$backend, cfg$api_key, cfg$model, cfg$ollama_url)
                r <- gsub("(?s)^.*?</think>\\s*", "", r, perl = TRUE)
                scrub_governance_violations(r)
              }, error = function(e2) {
                paste0(chat_response,
                  "\n\n[⚠️ Factuality flag: ", length(chat_fact_result$issues),
                  " potential error(s) detected but auto-correction failed — ",
                  paste(sapply(chat_fact_result$issues, `[[`, "detail"), collapse = "; "), "]")
              })
            }
          }

          if (isTRUE(grepl("This question concerns general clinical treatment strategies|This assistant is part of the CancerRCDPredictor platform", chat_response, perl = TRUE))) {
            chat_response <- gsub("The interpretations presented above.*appropriate\\.?", "", chat_response, ignore.case = TRUE)
            chat_response <- trimws(chat_response)
          }

          elapsed_s <- round(as.numeric(difftime(Sys.time(), start_t, units = "secs")), 1)
          list(text = chat_response, elapsed = elapsed_s)
        }, seed = TRUE) %...>% (function(res) {
          release_queue(sid)
          session$sendCustomMessage("hide_spinner", list())

          chat_response <- res$text
          elapsed_s <- res$elapsed

          log_file <- "llm_performance_log.csv"
          new_row <- data.frame(Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Module = "Clinical_QA", Time_secs = elapsed_s, stringsAsFactors = FALSE)
          if (!file.exists(log_file)) write.csv(new_row, log_file, row.names = FALSE) else write.table(new_row, log_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)

          time_str <- paste0("Automatically generated on ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), " (Processing Time: ", elapsed_s, "s)")
          chat_response <- paste0(chat_response, "\n\n<span style='font-size:10px; color:#94a3b8; display:block; text-align:right; margin-top:10px;'>", time_str, "</span>")

          cur_hist <- append(cur_hist, list(list(role = "assistant", content = chat_response)))
          rv_chat_history(cur_hist)
        }) %...!% (function(e) {
          release_queue(sid)
          session$sendCustomMessage("hide_spinner", list())
          cur_hist <- append(cur_hist, list(list(role = "assistant", content = paste0("ERROR: The AI Engine failed. ", conditionMessage(e)))))
          rv_chat_history(cur_hist)
        })
        return(NULL)
      }, error = chat_error_handler)
    })

    output$tumor_chat_report_ui <- renderUI({
      history <- rv_chat_history()
      if (length(history) == 0) return(tags$p(style="color: #94a3b8; font-style: italic; margin-top: 15px;", "Welcome to the Digital Molecular Tumor Board. How can I assist you with clinical interpretations today?"))

      chat_bubbles <- lapply(history, function(msg) {
        is_user <- msg$role == "user"
        bg_color <- if(is_user) "rgba(96, 165, 250, 0.2)" else "rgba(139, 92, 246, 0.2)"
        border_color <- if(is_user) "rgba(96, 165, 250, 0.4)" else "rgba(139, 92, 246, 0.4)"
        icon <- if(is_user) bs_icon("person-fill") else bs_icon("cpu-fill")
        align <- if(is_user) "right" else "left"
        margin <- if(is_user) "margin-left: auto;" else "margin-right: auto;"
        text_color <- if(is_user) "#93c5fd" else "#c4b5fd"

        clean_txt <- gsub("\\*\\*", "", msg$content)
        clean_txt <- gsub("###", "", clean_txt)
        clean_txt <- gsub("---", "", clean_txt)
        clean_txt <- gsub("- ", "", clean_txt)

        tags$div(
          style = paste0("background: ", bg_color, "; border: 1px solid ", border_color, "; border-radius: 8px; padding: 12px; margin-bottom: 12px; max-width: 85%; text-align: left; ", margin),
          tags$div(style = paste0("color: ", text_color, "; font-weight: bold; margin-bottom: 5px;"), icon, if(is_user) " You" else " AI Tumor Board"),
          tags$div(style = "color: #e2e8f0; font-size: 14px; line-height: 1.5;", HTML(gsub("\n", "<br>", clean_txt)))
        )
      })

      do.call(tagList, chat_bubbles)
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

    output$download_ai_report_html <- downloadHandler(
      filename = function() {
        patient_id <- input$tumor_shap_patient
        if(is.null(patient_id) || patient_id == "") patient_id <- "Unknown"
        paste0("AI_Synthesis_Report_", patient_id, ".html")
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("hide_spinner", list()))

        # --- Robust helper: check if a reactive text value is missing/empty/error ---
        is_missing_or_error <- function(txt) {
          if (is.null(txt)) return(TRUE)
          if (length(txt) != 1L) return(TRUE)
          if (is.na(txt)) return(TRUE)
          if (nchar(txt) == 0L) return(TRUE)
          if (grepl("^\\s*ERROR:", txt, perl = TRUE)) return(TRUE)
          return(FALSE)
        }

        # Verify we have at least SHAP text generated
        shap_txt <- rv_shap_llm_text()
        if (is_missing_or_error(shap_txt)) {
            showNotification("Please generate a valid SHAP Clinical Synthesis before downloading the report.", type = "error")
            # Write a fallback HTML so the download produces a file instead of failing silently
            writeLines(paste0("<html><body><h2>No AI Synthesis Available</h2><p>Please run the SHAP Clinical Synthesis before downloading.</p></body></html>"), file)
            return()
        }

        pharma_txt <- rv_pharma_llm_text()
        if (is_missing_or_error(pharma_txt)) pharma_txt <- "No pharmacogenomic translation generated for this session."

        shap_payload <- rv_shap_llm_payload()
        if (is.null(shap_payload) || length(shap_payload) != 1L || is.na(shap_payload)) shap_payload <- "No payload available."

        pharma_payload <- rv_pharma_llm_payload()
        if (is.null(pharma_payload) || length(pharma_payload) != 1L || is.na(pharma_payload)) pharma_payload <- "No payload available."

        # Sanitize text for safe R Markdown rendering:
        # 1. Remove literal backslash-asterisk patterns (fixes P\1a\1t... bug)
        shap_txt   <- gsub("\\*", "", shap_txt, fixed = TRUE)
        pharma_txt <- gsub("\\*", "", pharma_txt, fixed = TRUE)
        # 2. Escape backticks to prevent knitr inline-R parsing breakage
        shap_txt   <- gsub("`", "&#96;", shap_txt, fixed = TRUE)
        pharma_txt <- gsub("`", "&#96;", pharma_txt, fixed = TRUE)
        # 3. Sanitize payloads too (they may contain backticks from JSON/code snippets)
        shap_payload   <- gsub("`", "&#96;", shap_payload, fixed = TRUE)
        pharma_payload <- gsub("`", "&#96;", pharma_payload, fixed = TRUE)

        report_params <- list(
          logo = gsub("\\\\", "/", file.path(getwd(), "www", "cancerrcdpredictor_logo_bloodorange.png")),
          patient_id = input$tumor_shap_patient,
          cohort = input$tumor_shap_cancer,
          shap_text = shap_txt,
          pharma_text = pharma_txt,
          shap_payload = shap_payload,
          pharma_payload = pharma_payload,
          chat_history = rv_chat_history()
        )

        tempReport <- tempfile(fileext = ".Rmd")
        copy_ok <- file.copy("ai_clinical_report.Rmd", tempReport, overwrite = TRUE)
        if (!copy_ok) {
          showNotification("Error: Could not locate the report template file 'ai_clinical_report.Rmd'.", type = "error", duration = 8)
          writeLines(paste0("<html><body><h2>Report Template Missing</h2><p>Could not find ai_clinical_report.Rmd in the application directory.</p></body></html>"), file)
          return()
        }

        showNotification("Generating AI Synthesis HTML Report...", type = "message", duration = 3)

        # Render to a temporary output file first, then copy to Shiny's download
        # file path. This avoids edge cases where rmarkdown::render may not write
        # directly to the Shiny-managed temp path (e.g., output_dir resolution).
        tmp_output <- tempfile(fileext = ".html")
        render_error <- NULL
        tryCatch({
          withCallingHandlers({
            rmarkdown::render(tempReport, output_file = tmp_output, params = report_params, envir = new.env(parent = globalenv()), quiet = TRUE)
          }, warning = function(w) invokeRestart("muffleWarning"))
        }, error = function(e) {
          render_error <<- e$message
        })

        if (!is.null(render_error)) {
          showNotification(paste0("Report rendering failed: ", render_error), type = "error", duration = 10)
          # Write a diagnostic HTML so the download still produces a file
          fallback_html <- paste0(
            "<html><head><style>body{font-family:Arial,sans-serif;padding:20px;}h2{color:#991b1b;}pre{background:#f8fafc;border:1px solid #e2e8f0;padding:15px;border-radius:6px;white-space:pre-wrap;font-size:14px;}</style></head>",
            "<body><h2>AI Synthesis Report — Rendering Error</h2>",
            "<p>The HTML report could not be compiled due to a rendering error. This may be caused by special characters in the AI-generated text that conflict with R Markdown formatting.</p>",
            "<p><strong>Error details:</strong> <code>", render_error, "</code></p>",
            "<p><strong>Patient ID:</strong> ", input$tumor_shap_patient, " | <strong>Cohort:</strong> ", input$tumor_shap_cancer, "</p>",
            "<hr><h3>Clinical Synthesis (plain text)</h3><pre>", shap_txt, "</pre>",
            "<hr><h3>Pharmacogenomic Translation (plain text)</h3><pre>", pharma_txt, "</pre>",
            "</body></html>"
          )
          writeLines(fallback_html, file)
        } else {
          # Success: copy the rendered output to Shiny's download file path
          file.copy(tmp_output, file, overwrite = TRUE)
        }
      }
    )

    # --- 96-COHORT EXPLORER CLINICAL EXPORTERS ---
    get_precision_params <- function() {
      req(input$precision_cancer, input$precision_metric)

      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$precision_cancer & cohort_matrix$Metric == input$precision_metric, ]
      req(nrow(selected_model) > 0)

      folder_name <- selected_model$Full_Name[1]
      zima_drive <- zima_drive_path
      cohort_dir <- file.path(zima_drive, folder_name)

      # Use the resolved trajectory (auto-discovered if user hasn't picked one)
      traj <- resolve_precision_trajectory()
      req(traj)
      sig_file <- traj$sig

      # Extract Patient ID from filename: ACC_DSS_df377_SHAP_Decision_Lethal_Trajectory_TCGA-OR-A5J1-01.pdf
      filename_no_ext <- sub("\\.pdf$", "", sig_file)
      parts <- unlist(strsplit(filename_no_ext, "_"))
      patient_id <- parts[length(parts)]

      # Extract Trajectory Type
      traj_type <- ifelse(grepl("Lethal", sig_file, ignore.case=TRUE), "Lethal Accelerating Hazard", "Protective Reversal Hazard")

      traj_tiff <- file.path(cohort_dir, "XGBoost", sub("\\.pdf$", ".tiff", sig_file))
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

    # NOTE: "DEAD SERVER CODE REMOVED" - placeholder for future cleanup

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

    # DEAD CODE: create_edu_fallback() is undefined. Safe to remove after audit.
    # output$edu_bifurcation_img <- renderUI({ create_edu_fallback("SKCM (Bifurcation)", "SKCM_OS_dfXXX_SHAP_Dependence_2.tiff") })

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

    output$precision_df_info <- renderUI({
      req(input$precision_cancer, input$precision_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$precision_cancer & cohort_matrix$Metric == input$precision_metric, ]
      req(nrow(selected_model) > 0)
      tags$div(
        style = "padding: 10px; background: rgba(59, 130, 246, 0.1); border-left: 4px solid #3b82f6; border-radius: 4px; margin-bottom: 15px;",
        tags$strong("Active Matrix: "), tags$span(style="color: #60a5fa;", selected_model$DF_ID[1])
      )
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

          # Scrub "Lethal" and "Protective" labels from the UI to avoid misleading users, standardizing to "Patient"
          clean_names <- gsub("Lethal_Trajectory", "Patient_Trajectory", clean_names, ignore.case = TRUE)
          clean_names <- gsub("Protective_Trajectory", "Patient_Trajectory", clean_names, ignore.case = TRUE)

          # Format to "Patient Trajectory: TCGA-OR-A5J1-01"
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

    # Resolve the effective trajectory: use the user's pick, or auto-discover the first available.
    resolve_precision_trajectory <- reactive({
      req(input$precision_cancer, input$precision_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$precision_cancer & cohort_matrix$Metric == input$precision_metric, ]
      req(nrow(selected_model) > 0)
      folder_name <- selected_model$Full_Name[1]
      trajectory_path <- file.path(zima_drive_path, folder_name, "XGBoost")

      # If the user already picked a trajectory that exists, use it
      if (!is.null(input$precision_signature) && input$precision_signature != "") {
        candidate <- file.path(trajectory_path, input$precision_signature)
        if (file.exists(candidate)) return(list(sig = input$precision_signature, folder = folder_name, path = trajectory_path))
      }

      # Otherwise auto-discover the first SHAP Decision Trajectory PDF
      if (dir.exists(trajectory_path)) {
        pdf_files <- list.files(trajectory_path, pattern = "_SHAP_Decision_.*Trajectory.*\\.pdf$")
        if (length(pdf_files) > 0) {
          return(list(sig = pdf_files[1], folder = folder_name, path = trajectory_path))
        }
      }
      return(NULL)
    })

    # Render the High-Res Trajectory PDF via iframe
    output$precision_trajectory_container <- renderUI({
      traj <- resolve_precision_trajectory()
      req(traj)

      pdf_url <- paste0("zima_models/", traj$folder, "/XGBoost/", traj$sig, "#zoom=100")
      tags$iframe(src = pdf_url, width = "100%", height = "700px", style = "border: 1px solid #334155; border-radius: 8px;")
    })

    # ==============================================================================
    # TAB: REPOSITORY
    # ==============================================================================
    observe({
      updateSelectInput(session, "repo_cancer", choices = unique(cohort_matrix$Cancer))
    })

    observeEvent(input$repo_cancer, {
      available_metrics <- cohort_matrix$Metric[cohort_matrix$Cancer == input$repo_cancer]
      updateSelectInput(session, "repo_metric", choices = available_metrics)
    })

    # Display the selected dfXX matrix identity
    output$repo_df_info <- renderUI({
      req(input$repo_cancer, input$repo_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$repo_cancer & cohort_matrix$Metric == input$repo_metric, ]
      req(nrow(selected_model) > 0)

      tags$div(
        style = "padding: 10px; background: rgba(59, 130, 246, 0.1); border-left: 4px solid #3b82f6; border-radius: 4px; margin-bottom: 15px;",
        tags$strong("Active Matrix: "), tags$span(style="color: #60a5fa;", selected_model$DF_ID[1])
      )
    })

    # Scan ZIMA directory to find subdirectories + the root bundle RDS file
    observeEvent(c(input$repo_cancer, input$repo_metric), {
      req(input$repo_cancer, input$repo_metric)
      selected_model <- cohort_matrix[cohort_matrix$Cancer == input$repo_cancer & cohort_matrix$Metric == input$repo_metric, ]
      req(nrow(selected_model) > 0)

      folder_name <- selected_model$Full_Name[1]
      cohort_dir <- file.path(zima_drive_path, folder_name)

      if(dir.exists(cohort_dir)) {
        # Find all actual directories
        dirs <- list.dirs(cohort_dir, full.names = FALSE, recursive = FALSE)

        # We also want to expose the RDS bundle at the root as a "directory" choice
        rds_file <- paste0("model_bundle_", folder_name, ".rds")

        options_list <- c()
        if(file.exists(file.path(cohort_dir, rds_file))) {
           options_list <- c(options_list, "Model Bundle (RDS)" = "ROOT_BUNDLE")
        }

        if(length(dirs) > 0) {
           options_list <- c(options_list, setNames(dirs, dirs))
        }

        # ADD THE TWO NEW STATIC OPTIONS
        options_list <- c(options_list,
                          "Phase III Clinical Probabilities" = "CancerRCDShiny_Phase_III_Clinical_Probabilities",
                          "Blind Clinical Probabilities" = "CancerRCDShiny_Blind_Clinical_Probabilities")

        if(length(options_list) == 0) {
           options_list <- c("No subdirectories or bundles found" = "")
        }

        updateSelectInput(session, "repo_subfolder", choices = options_list)
      } else {
        updateSelectInput(session, "repo_subfolder", choices = c("ZIMA Cohort Directory Not Found" = ""))
      }
    })

    # Get available files in the selected subfolder
    repo_files_reactive <- reactive({
      req(input$repo_subfolder)
      if(input$repo_subfolder == "") return(data.frame(File = character(), Size = character()))

      # Handle static bundles
      if(input$repo_subfolder == "CancerRCDShiny_Phase_III_Clinical_Probabilities") {
        target_dir <- file.path(ZIMA_ROOT, "CancerRCDShiny_Phase_III_Clinical_Probabilities")
        files <- if(dir.exists(target_dir)) list.files(target_dir, full.names = FALSE) else character()
      } else if(input$repo_subfolder == "CancerRCDShiny_Blind_Clinical_Probabilities") {
        target_dir <- file.path(ZIMA_ROOT, "ML_Internal_validation_dataset", "CancerRCDShiny_Blind_Clinical_Probabilities")
        files <- if(dir.exists(target_dir)) list.files(target_dir, full.names = FALSE) else character()
      } else {
        # Dynamic bundles based on cancer and metric
        req(input$repo_cancer, input$repo_metric)
        selected_model <- cohort_matrix[cohort_matrix$Cancer == input$repo_cancer & cohort_matrix$Metric == input$repo_metric, ]
        folder_name <- selected_model$Full_Name[1]
        cohort_dir <- file.path(zima_drive_path, folder_name)

        if(input$repo_subfolder == "ROOT_BUNDLE") {
          target_dir <- cohort_dir
          files <- paste0("model_bundle_", folder_name, ".rds")
          files <- files[file.exists(file.path(cohort_dir, files))]
        } else {
          target_dir <- file.path(cohort_dir, input$repo_subfolder)
          files <- if(dir.exists(target_dir)) list.files(target_dir, full.names = FALSE) else character()
        }
      }

      if(length(files) > 0) {
        sizes <- sapply(files, function(f) {
           info <- file.info(file.path(target_dir, f))
           if(is.na(info$size)) return("Unknown")
           size_mb <- info$size / (1024^2)
           if(size_mb < 1) {
             size_kb <- info$size / 1024
             return(sprintf("%.1f KB", size_kb))
           }
           return(sprintf("%.2f MB", size_mb))
        })
        return(data.frame(File = files, Size = sizes, stringsAsFactors = FALSE))
      } else {
        return(data.frame(File = character(), Size = character()))
      }
    })

    output$repo_files_table <- renderDT({
      df_files <- repo_files_reactive()
      datatable(df_files,
                options = list(pageLength = 15, dom = 'ftp',
                               language = list(search = "Filter files:")),
                selection = "multiple", rownames = FALSE,
                class = "display compact cell-border hover") %>%
        formatStyle(columns = names(df_files), color = "#cbd5e1", backgroundColor = "rgba(15, 23, 42, 0.4)") %>%
        formatStyle("File", fontWeight = "bold", color = "#60a5fa")
    })

    # Download Handlers
    output$download_repo_selected <- downloadHandler(
      filename = function() {
        selected_rows <- input$repo_files_table_rows_selected
        if(length(selected_rows) == 1) {
          df_files <- repo_files_reactive()
          df_files$File[selected_rows]
        } else {
          paste0(input$repo_subfolder, "_selected.zip")
        }
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("hide_spinner", list()), add = TRUE)
        selected_rows <- input$repo_files_table_rows_selected
        req(input$repo_subfolder, length(selected_rows) > 0)

        df_files <- repo_files_reactive()
        selected_files <- df_files$File[selected_rows]

        if(input$repo_subfolder == "CancerRCDShiny_Phase_III_Clinical_Probabilities") {
          target_dir <- file.path(ZIMA_ROOT, "CancerRCDShiny_Phase_III_Clinical_Probabilities")
        } else if(input$repo_subfolder == "CancerRCDShiny_Blind_Clinical_Probabilities") {
          target_dir <- file.path(ZIMA_ROOT, "ML_Internal_validation_dataset", "CancerRCDShiny_Blind_Clinical_Probabilities")
        } else {
          req(input$repo_cancer, input$repo_metric)
          selected_model <- cohort_matrix[cohort_matrix$Cancer == input$repo_cancer & cohort_matrix$Metric == input$repo_metric, ]
          folder_name <- selected_model$Full_Name[1]
          if(input$repo_subfolder == "ROOT_BUNDLE") {
            target_dir <- file.path(zima_drive_path, folder_name)
          } else {
            target_dir <- file.path(zima_drive_path, folder_name, input$repo_subfolder)
          }
        }

        if(length(selected_files) == 1) {
           file.copy(file.path(target_dir, selected_files), file)
        } else {
           owd <- setwd(target_dir)
           on.exit(setwd(owd), add = TRUE)
           zip::zip(zipfile = file, files = selected_files)
        }
      }
    )

    output$download_repo_all <- downloadHandler(
      filename = function() {
        paste0(input$repo_subfolder, "_all.zip")
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("hide_spinner", list()), add = TRUE)
        req(input$repo_subfolder)

        if(input$repo_subfolder == "CancerRCDShiny_Phase_III_Clinical_Probabilities") {
          target_dir <- file.path(ZIMA_ROOT, "CancerRCDShiny_Phase_III_Clinical_Probabilities")
        } else if(input$repo_subfolder == "CancerRCDShiny_Blind_Clinical_Probabilities") {
          target_dir <- file.path(ZIMA_ROOT, "ML_Internal_validation_dataset", "CancerRCDShiny_Blind_Clinical_Probabilities")
        } else {
          req(input$repo_cancer, input$repo_metric)
          selected_model <- cohort_matrix[cohort_matrix$Cancer == input$repo_cancer & cohort_matrix$Metric == input$repo_metric, ]
          folder_name <- selected_model$Full_Name[1]
          if(input$repo_subfolder == "ROOT_BUNDLE") {
            target_dir <- file.path(zima_drive_path, folder_name)
          } else {
            target_dir <- file.path(zima_drive_path, folder_name, input$repo_subfolder)
          }
        }

        df_files <- repo_files_reactive()
        all_files <- df_files$File

        req(length(all_files) > 0)

        owd <- setwd(target_dir)
        on.exit(setwd(owd), add = TRUE)
        zip::zip(zipfile = file, files = all_files)
      }
    )



  # ==============================================================================
  # SERVER LOGIC: CONTINUOUS PROBABILITY TRAJECTORIES (PHASE III & VALIDATION)
  # ==============================================================================

  # Phase III Directory
  phase3_prob_dir <- file.path(ZIMA_ROOT, "CancerRCDShiny_Phase_III_Clinical_Probabilities")
  if (!dir.exists(phase3_prob_dir) && dir.exists(file.path(ZIMA_ROOT, "..", "CancerRCDShiny_Phase_III_Clinical_Probabilities"))) {
      phase3_prob_dir <- file.path(ZIMA_ROOT, "..", "CancerRCDShiny_Phase_III_Clinical_Probabilities")
  }
  val_prob_dir <- file.path(ZIMA_ROOT, "ML_Internal_validation_dataset", "CancerRCDShiny_Blind_Clinical_Probabilities")

  # ---------- PHASE III TRAJECTORIES ----------
  observe({
    req(dir.exists(phase3_prob_dir))
    files <- list.files(phase3_prob_dir, pattern = "_Probabilities\\.tsv$")
    if (length(files) > 0) {
      cancers <- unique(sapply(strsplit(files, "_"), '[', 1))
      updateSelectInput(session, "traj_phase3_cancer", choices = sort(cancers))
    } else {
      updateSelectInput(session, "traj_phase3_cancer", choices = c("ZIMA Directory Not Found" = ""))
    }
  })

  observeEvent(input$traj_phase3_cancer, {
    req(input$traj_phase3_cancer, dir.exists(phase3_prob_dir))
    files <- list.files(phase3_prob_dir, pattern = paste0("^", input$traj_phase3_cancer, "_.*_Probabilities\\.tsv$"))
    if (length(files) > 0) {
      metrics <- unique(sapply(strsplit(files, "_"), '[', 2))
      updateSelectInput(session, "traj_phase3_metric", choices = sort(metrics))
    }
  })

  phase3_data <- reactiveVal(NULL)

  observeEvent(c(input$traj_phase3_cancer, input$traj_phase3_metric), {
    req(input$traj_phase3_cancer, input$traj_phase3_metric)
    files <- list.files(phase3_prob_dir, pattern = paste0("^", input$traj_phase3_cancer, "_", input$traj_phase3_metric, "_.*_Probabilities\\.tsv$"), full.names = TRUE)
    if (length(files) > 0) {
      df <- data.table::fread(files[1])
      phase3_data(df)
      updateSelectInput(session, "traj_phase3_patient", choices = sort(unique(df$Sample_ID)))
    }
  })

  # ==============================================================================
  # TUMOR BOARD - PERSONALIZED SHAP DATA HOOKS
  # ==============================================================================
  observe({
    req(dir.exists(phase3_prob_dir))
    files <- list.files(phase3_prob_dir, pattern = "_Probabilities\\.tsv$")
    if (length(files) > 0) {
      cancers <- unique(sapply(strsplit(files, "_"), '[', 1))
      updateSelectInput(session, "tumor_shap_cancer", choices = sort(cancers))
    } else {
      updateSelectInput(session, "tumor_shap_cancer", choices = c("ZIMA Directory Not Found" = ""))
    }
  })

  observeEvent(input$tumor_shap_cancer, {
    req(input$tumor_shap_cancer, dir.exists(phase3_prob_dir))
    files <- list.files(phase3_prob_dir, pattern = paste0("^", input$tumor_shap_cancer, "_.*_Probabilities\\.tsv$"))
    if (length(files) > 0) {
      metrics <- unique(sapply(strsplit(files, "_"), '[', 2))
      updateSelectInput(session, "tumor_shap_metric", choices = sort(metrics))
    }
  })

  tumor_shap_data <- reactiveVal(NULL)

  observeEvent(c(input$tumor_shap_cancer, input$tumor_shap_metric), {
    req(input$tumor_shap_cancer, input$tumor_shap_metric)
    files <- list.files(phase3_prob_dir, pattern = paste0("^", input$tumor_shap_cancer, "_", input$tumor_shap_metric, "_.*_Probabilities\\.tsv$"), full.names = TRUE)
    if (length(files) > 0) {
      df <- data.table::fread(files[1])
      tumor_shap_data(df)
      updateSelectInput(session, "tumor_shap_patient", choices = sort(unique(df$Sample_ID)))
    }
  })
  traj_phase3_plot_obj <- reactive({
    req(phase3_data(), input$traj_phase3_patient)
    df <- phase3_data()
    pat_df <- df[df$Sample_ID == input$traj_phase3_patient, ]
    req(nrow(pat_df) > 0)

    # Reshape for plotting
    models <- c("RSF", "XGBoost", "MTLR", "Boruta", "MVL")
    long_list <- lapply(models, function(m) {
      if(paste0(m, "_Prob_1Yr") %in% colnames(pat_df)) {
        data.frame(
          Model = m,
          Time = c(1, 3, 5),
          Probability = c(pat_df[[paste0(m, "_Prob_1Yr")]], pat_df[[paste0(m, "_Prob_3Yr")]], pat_df[[paste0(m, "_Prob_5Yr")]])
        )
      } else { NULL }
    })
    plot_df <- do.call(rbind, long_list)
    req(!is.null(plot_df))

    # Invert if Event Metric
    metric <- pat_df$Endpoint[1]
    is_event <- metric %in% c("DFI", "PFI")
    if(is_event) {
      # The imported Phase III data is natively 1 - S(t) for event metrics. No further inversion required.
      y_label <- "Probability of Event 1 - S(t)"
      line_color <- "firebrick"
    } else {
      y_label <- "Probability of Survival S(t)"
      line_color <- "dodgerblue"
    }

    # Colors for Models
    model_colors <- c("RSF" = "forestgreen", "XGBoost" = "darkorange", "MTLR" = "darkorchid", "Boruta" = "dodgerblue", "MVL" = "red")
    model_alpha <- c("RSF" = 0.5, "XGBoost" = 0.5, "MTLR" = 0.5, "Boruta" = 0.5, "MVL" = 1.0)
    model_size <- c("RSF" = 1, "XGBoost" = 1, "MTLR" = 1, "Boruta" = 1, "MVL" = 2)

    ggplot(plot_df, aes(x = Time, y = Probability, color = Model, linewidth = Model, alpha = Model)) +
      geom_line() +
      geom_point(aes(size = Model), show.legend = FALSE) +
      scale_x_continuous(breaks = c(1, 3, 5), labels = c("1 Year", "3 Years", "5 Years"), limits = c(1, 5)) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy=1)) +
      scale_color_manual(name = "Algorithmic Engine:", values = model_colors) +
      scale_alpha_manual(name = "Algorithmic Engine:", values = model_alpha) +
      scale_linewidth_manual(name = "Algorithmic Engine:", values = model_size) +
      scale_size_manual(name = "Algorithmic Engine:", values = model_size * 2) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(size = 12, hjust=0.5, color="grey30")
      ) +
      labs(
        title = paste("Phase III Trajectory:", input$traj_phase3_patient),
        subtitle = paste("Cohort:", input$traj_phase3_cancer, "| Metric:", metric),
        x = "Clinical Landmark",
        y = y_label
      )
  })

  output$render_traj_phase3_plot <- renderPlot({
    traj_phase3_plot_obj()
  })

  output$download_traj_phase3_pdf <- downloadHandler(
    filename = function() {
      paste0("Phase_III_Trajectory_", input$traj_phase3_patient, ".pdf")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = traj_phase3_plot_obj(), width = 10, height = 7, device = "pdf")
    }
  )

  # ---------- BLIND VALIDATION TRAJECTORIES ----------
  observe({
    req(dir.exists(val_prob_dir))
    files <- list.files(val_prob_dir, pattern = "_Probabilities\\.tsv$")
    if (length(files) > 0) {
      cancers <- sapply(strsplit(files, "_"), '[', 1)
      metrics <- sapply(strsplit(files, "_"), '[', 2)

      surv_files <- files[metrics %in% c("OS", "DSS")]
      event_files <- files[metrics %in% c("DFI", "PFI")]

      surv_cancers <- sort(unique(sapply(strsplit(surv_files, "_"), '[', 1)))
      event_cancers <- sort(unique(sapply(strsplit(event_files, "_"), '[', 1)))

      updateSelectInput(session, "val_surv_cancer", choices = surv_cancers)
      updateSelectInput(session, "val_event_cancer", choices = event_cancers)
    } else {
      updateSelectInput(session, "val_surv_cancer", choices = c("ZIMA Directory Not Found" = ""))
      updateSelectInput(session, "val_event_cancer", choices = c("ZIMA Directory Not Found" = ""))
    }
  })

  # Survival
  val_surv_data <- reactiveVal(NULL)

  observeEvent(input$val_surv_cancer, {
    req(input$val_surv_cancer, dir.exists(val_prob_dir))
    files <- list.files(val_prob_dir, pattern = paste0("^", input$val_surv_cancer, "_.*_Probabilities\\.tsv$"))
    metrics <- unique(sapply(strsplit(files, "_"), '[', 2))
    surv_metrics <- intersect(metrics, c("OS", "DSS"))

    if(length(surv_metrics) > 0) {
      updateSelectInput(session, "val_surv_metric", choices = sort(surv_metrics))
    } else {
      updateSelectInput(session, "val_surv_metric", choices = character(0))
    }
  })

  observeEvent(c(input$val_surv_cancer, input$val_surv_metric), {
    req(input$val_surv_cancer, input$val_surv_metric)
    files <- list.files(val_prob_dir, pattern = paste0("^", input$val_surv_cancer, "_", input$val_surv_metric, "_.*_Probabilities\\.tsv$"), full.names = TRUE)
    if (length(files) > 0) {
      df <- data.table::fread(files[1])
      val_surv_data(df)
      updateSelectInput(session, "val_surv_patient", choices = sort(unique(df$Sample_ID)))
    } else {
      val_surv_data(NULL)
      updateSelectInput(session, "val_surv_patient", choices = character(0))
    }
  })

  val_surv_plot_obj <- reactive({
    req(val_surv_data(), input$val_surv_patient)
    df <- val_surv_data()
    pat_df <- df[df$Sample_ID == input$val_surv_patient, ]
    req(nrow(pat_df) > 0)

    shiny::validate(
      need("Prob_1Yr" %in% colnames(pat_df), "ERROR: 'Prob_1Yr' column not found in dataset! Did you run the ZIMA_Validation_Probability_Extractor.R script?"),
      need("Inference_Path" %in% colnames(pat_df), "ERROR: 'Inference_Path' column not found in dataset!"),
      need(!all(is.na(c(pat_df$Prob_1Yr[1], pat_df$Prob_3Yr[1], pat_df$Prob_5Yr[1]))), "CLINICAL EXCEPTION: The baseline hazard mapping safely aborted probability computation. This strict algorithmic safeguard is triggered when the patient's multi-omic signature achieves mathematically perfect separation against the Phase III biological anchor, causing the partial likelihood coefficient to geometrically diverge (Infinite Coefficient). This patient's specific multi-omic topography precludes valid survival extrapolation.")
    )

    plot_df <- data.frame(
      Time = c(1, 3, 5),
      Probability = c(pat_df$Prob_1Yr[1], pat_df$Prob_3Yr[1], pat_df$Prob_5Yr[1]),
      Path = pat_df$Inference_Path[1]
    )

    if (grepl("Path A", pat_df$Inference_Path[1], ignore.case = TRUE)) {
      line_color <- "red"
    } else {
      line_color <- "darkorange"
    }

    ggplot(plot_df, aes(x = Time, y = Probability)) +
      geom_line(color = line_color, linewidth = 2) +
      geom_point(color = line_color, size = 4) +
      scale_x_continuous(breaks = c(1, 3, 5), labels = c("1 Year", "3 Years", "5 Years"), limits = c(1, 5)) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy=1)) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(size = 12, hjust=0.5, color="grey30")
      ) +
      labs(
        title = paste("Clinical Blind Validation:", input$val_surv_patient),
        subtitle = paste(plot_df$Path[1], "|", input$val_surv_cancer, input$val_surv_metric),
        x = "Clinical Landmark",
        y = "Probability of Survival S(t)"
      )
  })

  output$render_val_surv_plot <- renderPlot({
    val_surv_plot_obj()
  })

  output$val_surv_path_info <- renderUI({
    req(input$val_surv_patient)
    tags$div(
      style = "padding: 10px; background: rgba(59, 130, 246, 0.1); border-left: 4px solid #3b82f6; border-radius: 4px; margin-bottom: 15px;",
      tags$strong("Selected Patient: "), tags$span(style="color: #60a5fa;", input$val_surv_patient)
    )
  })

  output$val_event_path_info <- renderUI({
    req(input$val_event_patient)
    tags$div(
      style = "padding: 10px; background: rgba(59, 130, 246, 0.1); border-left: 4px solid #3b82f6; border-radius: 4px; margin-bottom: 15px;",
      tags$strong("Selected Patient: "), tags$span(style="color: #60a5fa;", input$val_event_patient)
    )
  })

  output$download_val_surv_pdf <- downloadHandler(
    filename = function() {
      paste0("Validation_Survival_", input$val_surv_patient, ".pdf")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = val_surv_plot_obj(), width = 10, height = 7, device = "pdf")
    }
  )

  # Event
  val_event_data <- reactiveVal(NULL)

  observeEvent(input$val_event_cancer, {
    req(input$val_event_cancer, dir.exists(val_prob_dir))
    files <- list.files(val_prob_dir, pattern = paste0("^", input$val_event_cancer, "_.*_Probabilities\\.tsv$"))
    metrics <- unique(sapply(strsplit(files, "_"), '[', 2))
    event_metrics <- intersect(metrics, c("DFI", "PFI"))

    if(length(event_metrics) > 0) {
      updateSelectInput(session, "val_event_metric", choices = sort(event_metrics))
    } else {
      updateSelectInput(session, "val_event_metric", choices = character(0))
    }
  })

  observeEvent(c(input$val_event_cancer, input$val_event_metric), {
    req(input$val_event_cancer, input$val_event_metric)
    files <- list.files(val_prob_dir, pattern = paste0("^", input$val_event_cancer, "_", input$val_event_metric, "_.*_Probabilities\\.tsv$"), full.names = TRUE)
    if (length(files) > 0) {
      df <- data.table::fread(files[1])
      val_event_data(df)
      updateSelectInput(session, "val_event_patient", choices = sort(unique(df$Sample_ID)))
    } else {
      val_event_data(NULL)
      updateSelectInput(session, "val_event_patient", choices = character(0))
    }
  })

  val_event_plot_obj <- reactive({
    req(val_event_data(), input$val_event_patient)
    df <- val_event_data()
    pat_df <- df[df$Sample_ID == input$val_event_patient, ]
    req(nrow(pat_df) > 0)

    shiny::validate(
      need("Prob_1Yr" %in% colnames(pat_df), "ERROR: 'Prob_1Yr' column not found in dataset! Did you run the ZIMA_Validation_Probability_Extractor.R script?"),
      need("Inference_Path" %in% colnames(pat_df), "ERROR: 'Inference_Path' column not found in dataset!"),
      need(!all(is.na(c(pat_df$Prob_1Yr[1], pat_df$Prob_3Yr[1], pat_df$Prob_5Yr[1]))), "CLINICAL EXCEPTION: The baseline hazard mapping safely aborted probability computation. This strict algorithmic safeguard is triggered when the patient's multi-omic signature achieves mathematically perfect separation against the Phase III biological anchor, causing the partial likelihood coefficient to geometrically diverge (Infinite Coefficient). This patient's specific multi-omic topography precludes valid survival extrapolation.")
    )

    plot_df <- data.frame(
      Time = c(1, 3, 5),
      Probability = c(pat_df$Prob_1Yr[1], pat_df$Prob_3Yr[1], pat_df$Prob_5Yr[1]),
      Path = pat_df$Inference_Path[1]
    )

    if (grepl("Path A", pat_df$Inference_Path[1], ignore.case = TRUE)) {
      line_color <- "red"
    } else {
      line_color <- "darkorange"
    }

    ggplot(plot_df, aes(x = Time, y = Probability)) +
      geom_line(color = line_color, linewidth = 2) +
      geom_point(color = line_color, size = 4) +
      scale_x_continuous(breaks = c(1, 3, 5), labels = c("1 Year", "3 Years", "5 Years"), limits = c(1, 5)) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy=1)) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(size = 12, hjust=0.5, color="grey30")
      ) +
      labs(
        title = paste("Clinical Blind Validation:", input$val_event_patient),
        subtitle = paste(plot_df$Path[1], "|", input$val_event_cancer, input$val_event_metric),
        x = "Clinical Landmark",
        y = "Probability of Event 1 - S(t)"
      )
  })

  output$render_val_event_plot <- renderPlot({
    val_event_plot_obj()
  })

  output$download_val_event_pdf <- downloadHandler(
    filename = function() {
      paste0("Validation_Event_", input$val_event_patient, ".pdf")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = val_event_plot_obj(), width = 10, height = 7, device = "pdf")
    }
  )


  # ==============================================================================
  # MULTIMEDIA MODALS
  # ==============================================================================
  observeEvent(input$btn_audio_desc, {
    showModal(modalDialog(
      title = "Podcast Description",
      p("This audio explores a critical paradox in multidimensional survival prediction: why advanced machine learning computationally suppresses static DNA mutations when forecasting patient outcomes. Analyzing over 80GB of harmonized data across 10,000 patients, it explains how continuous multi-omic topologies dynamically outcompete static genomic alterations during algorithmic competition. The discussion breaks down the failure of linear Cox models and the power of multi-view meta-learning, revealing how geometric survival mappings redefine biomarker dominance and personalized clinical interception."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$btn_video_desc, {
    showModal(modalDialog(
      title = "Video Description",
      p("This video presents the analytical architecture of CancerRCDPredictor, a non-linear framework that redefines pan-cancer survival prediction through multi-omic topologies. Using atlas-scale regulated cell death (RCD) signatures across 33 cancer types, it demonstrates why traditional proportional-hazards models structurally fail to capture individualized risk. By fusing seven distinct omic layers into a Quadripartite SuperLearner ensemble, the platform maps personalized geometric trajectories-translating complex synergistic lethal peaks and protective valleys into actionable precision oncology via a dynamic digital molecular tumor board."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

}

  # Run the application
shinyApp(
  ui = ui,
  server = server,
  options = list(host = "0.0.0.0", port = 8888)
)
