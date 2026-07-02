# Evolutionary Distinctiveness and Extinction Risk Pipeline 
This repository contains the R code for calculating and visualizing Evolutionary Distinctiveness (ED/ED2), Evolutionary Distinct and Globally Endangered (EDGE/EDGE2) scores, and Expected Phylogenetic Diversity loss (ePD) at the scale of all life.

The pipeline supports analyzing multi-tree datasets incorporating topological and temporal uncertainties (e.g., fully-dated trees containing millions of species), and reproduces the distributions, randomisation tests, and diagnostic plots presented in the accompanying manuscript.

The code can be divided into two primary scripts to separate heavy phylogenetic computation from data aggregation and visualization:

# Mathematical Formulas Implemented in this code:
For any sub-clade rooted at an interior node k, its total unique PD represents the sum of all underlying branch lengths shared or uniquely held by its descendant tips, formulated as:


<img width="211" height="55" alt="image" src="https://github.com/user-attachments/assets/6d2f5cd6-7cba-4751-a01f-65714588e806" />


Where Mk represents the complete set of branches (both interior and terminal) descended from node k, and Lm is the length of branch m.

The expected PD loss of a clade rooted at node k can be calculated as:

<img width="90" height="62" alt="image" src="https://github.com/user-attachments/assets/bb6ae2b8-f637-484d-9cee-8845a2e9aac1" />

where  Dm represents the set of all extant species that descend from branch m. 

The product 

<img width="112" height="64" alt="image" src="https://github.com/user-attachments/assets/62d39b9a-8cfb-4ff9-ae35-e6fd5382225c" />


 quantifies the probability of complete lineage collapse for branch m. 

The ED of species i can be represented as

<img width="255" height="55" alt="image" src="https://github.com/user-attachments/assets/9dc00dbf-8caa-46fa-9b10-8279edcc7ddb" />


Where ni gives the total number of ancestral branches to species i,  𝐿𝑖,1 refers to the terminal branch length of species 𝑖, 𝐿𝑖,𝑗 for 2 ≤ 𝑗 ≤ 𝑛𝑖 gives the total length of interior branch j ancestral to species i and N𝑖,𝑗 gives the number of descendants for each of these same branches.

The ED2 for species i can be expressed as:

<img width="221" height="28" alt="image" src="https://github.com/user-attachments/assets/db0fefbd-c1e2-4ad6-bb1e-fcf03f810d82" />

where ED2i is composed of TBLi (Terminal branch length of species i) plus another component. TBL measures the unique PD contributed by species i, which is also included by the same way while calculating ED. The set Ci,j represents all species descended from the corresponding branch with length Li,j (as defined earlier), and Pk is the probability of extinction of species k


By combining the calculated ED scores with the red list, we calculated EDGE scores using following formula set out in previous work: 


<img width="161" height="31" alt="image" src="https://github.com/user-attachments/assets/65281e94-d6bf-408f-ba28-973021965a38" />

Where GEi is the extinction risk of species i, based on the IUCN red list  (Least Concern=0, Near Threatened and Conservation Dependent = 1, Vulnerable=2, Endangered=3, Critically Endangered=4). 

EDGE2 is another index that measures the avertable loss of PD through the conservation of individual species. The EDGE2 of species i can be expressed as EDGE2i = ED2i x GE2i where GE2i is the probability of extinction for species i at some given future time.


# edge2_pipeline_Ver.submit.R (Core Calculation Pipeline) 

1. Required Input Files for Pipeline (edge2_pipeline_Ver.submit.R)
To run the core calculation pipeline, you need to prepare the following three inputs:
Phylogenetic Trees (.tre files): * A set of fully-dated phylogenetic trees in Newick format containing target extant species. (To account for topological and temporal uncertainty, the full study utilizes 1,000 trees; single-tree testing is also supported).
IUCN Red List Data (.csv file): * A table obtained via the IUCN Red List API containing species conservation statuses.
Required columns: Must include name (Species scientific name) and status_code (e.g., CR, EN, VU, NT, LC, DD, LR/nt, LR/cd, LR/lc, RE).
Ordered Leaves/Species Table (.csv file):
A reference table aligning tree leaves with global taxonomic metadata.
Required columns: Must include species names corresponding to the phylogenetic tree tip labels.


2. Output Data & Objects
The core function EDGE_PD_combined or the execution script processes the inputs and returns three primary components (saved as structured data frames or R objects): Species-Level Score Table (data.frame):A comprehensive table mapping metrics to each species.Output columns: name (Species name), TBL (Terminal Branch Length), GE2 (Continuous Extinction Probability), ED2 (Evolutionary Distinctiveness 2), and EDGE2 scores.Expected PD Loss Tree (phylo object):A mutated tree structure where branch lengths are scaled or weighted by the probability of complete lineage collapse for all descendants of that branch). Clade-Level PD Summary: Calculated metrics indicating the Total Phylogenetic Diversity (PD) and Expected Phylogenetic Diversity Loss (ePD) measured in Millions of Years (Myr) for the evaluated clade.

Project Structure & Usage:
Open edge2_pipeline_Ver.submit.R. Update the paths pointing to your tree directories and metadata tables:
tree_files <- list.files("/path/to/your/trees", pattern = "\\.tre$", full.names = TRUE)
iucn <- read.csv("/path/to/latest_iucn_2025.csv")
leaves_table <- read.csv("/path/to/updated_ordered_leaves_3.0.csv")

GE.2.calc(pext): Performs OLS regression modeling and back-calibration to construct the continuous reference probability pools (1,000,000 simulated species). 
ED_calculation(tree): Computes individual species' Evolutionary Distinctiveness (ED) based on branch lengths weighted by descendent counts via postorder tree reordering. 
EDGE_PD_combined(tree, sp.pext): Combines topology and calculated continuous extinction risk to output terminal branch length (TBL), expected PD loss tree, and comprehensive ED2/EDGE2 scores.

# Make_Plots.R (Data Exploration & Visualization)

Median Tree Selection: Identifies the single representative tree (Tree_377 in our estimation) whose total PD is closest to the global median across the 1,000 trees.

Clade Isolation & Mapping: Matches taxonomic Open Tree of Life (OTT) identifiers to extract descendants for pre-defined clades across the tree of life (e.g., Metazoa, Chloroplastida, Fungi, Vertebrata, Lepidoptera, etc.).

Statistical Diagnostics: Performs Anderson-Darling and Shapiro-Wilk normality tests across isolated clades.

Plotting: * Generates Log10(ED/ED2) Density Plot Grids (Facet) for separated clade groups, automatically mapping them to a unified color scale based on Clade Phylogenetic Diversity (logPD). Includes threshold vertical lines at the 95th (solid) and 99th (dashed) global percentiles.


# Prerequisites & Installation 
Ensure you have R (version $\ge$ 4.4.2) installed. You will need the following CRAN and Bioconductor packages:
install.packages(c("ape", "ggplot2", "dplyr", "tidyr", "ggpubr", "ggpmisc", "RColorBrewer", "ggnewscale", "tidyverse", "nortest"))

# 1. Calculation Pipeline
Open edge2_pipeline_Ver.submit.R. Update the paths pointing to your tree directories and metadata tables:
tree_files <- list.files("/path/to/your/equal_splits_both", pattern = "\\.tre$", full.names = TRUE)
iucn <- read.csv("/path/to/latest_iucn_2025.csv")
leaves_table <- read.csv("/path/to/updated_ordered_leaves_3.0.csv")

    GE.2.calc(pext): Performs OLS regression modeling and back-calibration to construct the continuous reference probability pools.

    ED_calculation(tree): Computes individual species' Evolutionary Distinctiveness (ED) based on branch lengths weighted by descendent counts      via postorder tree reordering.

    EDGE_PD_combined(tree, sp.pext): Combines topology and calculated continuous extinction risk to calculate terminal branch length (TBL),         expected PD loss, and comprehensive ED2/EDGE2 scores.

# 2. Visualization & Results Exploration
Open Make_Plots.R to process data from the selected median tree and generate figures:
plots <- plot_clade_density_by_group(species_in_clade_final2) # Generates density facets
plot_pd_colorbar(species_in_clade_final2)                    # Generates custom color guides

Outputs: Normality test results (normality_test_ED.csv, normality_test_ED2.csv), Processed plotting data frames (ED_Dis_figure1.csv, ED2_Dis_figure1.csv), ggplot charts representing clade-specific distinctiveness distributions.
