Assessing conservation paradoxes and challenges in invasive alien
species of economic concern
================
Github repository & scripts created by Marine Robuchon, with
contributions of Céline Bellard, Camille Bernery, Cheikh Dia, Gustavo
Heringer, Boris Leroy, Sandrine Pavoine & Vanessa Rezende

- <a href="#1-description--organisation-of-the-repository"
  id="toc-1-description--organisation-of-the-repository">1. Description
  &amp; organisation of the repository</a>
  - <a href="#11-data" id="toc-11-data">1.1. data</a>
  - <a href="#12-scripts" id="toc-12-scripts">1.2. scripts</a>
  - <a href="#13-outputs" id="toc-13-outputs">1.3. outputs</a>
- <a href="#2-requirements" id="toc-2-requirements">2. Requirements</a>
  - <a href="#21-clone-the-repository" id="toc-21-clone-the-repository">2.1.
    clone the repository</a>
  - <a href="#22-install-packages" id="toc-22-install-packages">2.2. install
    packages</a>
- <a href="#3-analyses" id="toc-3-analyses">3. Analyses</a>
  - <a href="#31-step-1-calculate-economic-costs-by-species"
    id="toc-31-step-1-calculate-economic-costs-by-species">3.1. step 1:
    calculate economic costs by species</a>
  - <a
    href="#32-step-2-calculate-distinctiveness--originality-scores-of-species"
    id="toc-32-step-2-calculate-distinctiveness--originality-scores-of-species">3.2.
    step 2: calculate distinctiveness (= originality) scores of species</a>
  - <a href="#33-step-3-homogenize-taxonomy-over-the-different-datasets"
    id="toc-33-step-3-homogenize-taxonomy-over-the-different-datasets">3.3.
    step 3: homogenize taxonomy over the different datasets</a>
  - <a href="#34-step-4-build-and-describe-the-final-database"
    id="toc-34-step-4-build-and-describe-the-final-database">3.4. step 4:
    build and describe the final database</a>
  - <a href="#35-step-5-generate-figures-and-tables"
    id="toc-35-step-5-generate-figures-and-tables">3.5. step 5: generate
    figures and tables</a>

# 1. Description & organisation of the repository

The purpose of this github repository is to document the data used, the
analyses carried out and the figures and tables produced with RStudio
for the paper “Assessing conservation paradoxes and challenges in
invasive alien species of economic concern”. The repository contains 3
folders (data, scripts, outputs), which content is described below.

## 1.1. data

This folder includes the following files:

- InvaCost_database_v4.1.xlsx (version 4.1 of the InvaCost database)
- taxonomy_mamPhy_5911species.csv (taxonomy used in the phylogenetic
  trees of mammals)
- MamFuncDat.txt (traits for mammals)
- AllBirdsEricson1.tre (phylogenetic trees of birds)
- BirdFuncDat.txt (traits for birds)
- ALLMB.tre (phylogenetic tree for plants)
- traits_to_run.csv (traits for plants)

This folder further includes the following folders:

- Completed_5911sp_topoCons_FBDasZhouEtAl (folder containing
  phylogenetic trees for mammals)
- RL_2022-1 (folder containing IUCN global assessments for mammals,
  birds and plants downloaded on 22/10/2022)

## 1.2. scripts

This folder includes the following scripts:

- 01_costs_by_species.R (script to calculate economic cost by species)
- 02_distinctiveness_scores.R (script to calculate distinctiveness
  scores by species)
- 03_homogenise_taxo.R (script to homogenize taxonomy across the
  different datasets)
- 04_build_describe_database.R (script to build and describe the final
  database)
- 05_analyses_figures_tables.R (script to generate the figures and
  tables of the paper)

## 1.3. outputs

This folder includes the following outputs (that you can re-generate
yourself by following the requirements and running the analyses):

- damagecost_by_species.csv (damage costs by species)
- managementcost_by_species.csv (management costs by species)
- mammals_vertlife_tree-based_phylori.csv (tree-based phylogenetic
  distinctiveness scores for mammals)
- mammals_vertlife_distance-based_phylori.csv (distance-based
  phylogenetic distinctiveness scores for mammals - no further used in
  the paper)
- mammals_vertlife_tree-based_dietori.csv (tree-based diet
  distinctiveness scores for mammals)
- mammals_vertlife_distance-based_dietori.csv (distance-based
  distinctiveness scores for mammals - no further used in the paper)
- mammals_vertlife_tree-based_activityori.csv (tree-based activity
  distinctiveness scores for mammals)
- mammals_vertlife_distance-based_activityori.csv (distance-based
  activity distinctiveness scores for mammals - no further used in the
  paper)
- mammals_vertlife_tree-based_massori.csv (tree-based mass
  distinctiveness scores for mammals)
- mammals_vertlife_distance-based_massori.csv (distance-based activity
  distinctiveness scores for mammals - no further used in the paper)
- mammals_vertlife_tree-based_funcori.csv (tree-based functional
  distinctiveness scores for mammals)
- mammals_vertlife_distance-based_funcori.csv (distance-based activity
  distinctiveness scores for mammals - no further used in the paper)
- birds_vertlife_tree-based_phylori.csv (tree-based phylogenetic
  distinctiveness scores for birds)
- birds_vertlife_distance-based_phylori.csv (distance-based phylogenetic
  distinctiveness scores for mammals - no further used in the paper)
- birds_vertlife_tree-based_dietori.csv (tree-based diet distinctiveness
  scores for birds)
- birds_vertlife_distance-based_dietori.csv (distance-based diet
  distinctiveness scores for birds - no further used in the paper)
- birds_vertlife_tree-based_activityori.csv (tree-based activity
  distinctiveness scores for birds)
- birds_vertlife_distance-based_activityori.csv (distance-based diet
  distinctiveness scores for birds - no further used in the paper)
- birds_vertlife_tree-based_massori.csv (tree-based mass distinctiveness
  scores for birds)
- birds_vertlife_distance-based_massori.csv (distance-based mass
  distinctiveness scores for birds - no further used in the paper)
- birds_vertlife_tree-based_funcori.csv (tree-based functional
  distinctiveness scores for birds)
- birds_vertlife_distance-based_funcori.csv (distance-based functional
  distinctiveness scores for birds - no further used in the paper)
- plants_tree-based_phylori.csv (tree-based phylogenetic distinctiveness
  scores for plants)
- oriplantsmiss.csv (tree-based functional distinctiveness scores for
  plants)
- taxa_rawandparsednames.csv (table of both raw and parsed binomial to
  be used for matching when building the final database)
- mammals_taxmatch_iucnitismdd.csv (taxonomic reference table for
  mammals)
- birds_taxmatch_iucnitis.csv (taxonomic reference table for birds)
- plants_taxmatch_lcvp.csv 5taxonomic reference table for plants)
- final_db.csv (final database containing costs, threats and
  distinctiveness scores for mammals, birds and plants)
- FIGURES1.png (figure showing costs and threat status of the TOP 5
  costliest species)
- FIGURE3.png (figure showing phylogenetic distinctiveness scores of the
  TOP 5 costliest species)
- FIGURE4.png (figure showing functional distinctiveness scores of the
  TOP 5 costliest species)
- table_invacost_threatened.csv (table listing costs and threat status
  of the species threatened in InvaCost)
- table_invacost_TOP25PO.csv (table listing costs and phylogenetic
  distinctiveness scores of the TOP25% most phylogenetically distinctive
  species by taxonomic group)
- table_invacost_TOP25FO.csv (table listing costs and functional
  distinctiveness scores of the TOP25% most functionally distinctive
  species by taxonomic group)
- table_invacost_TOP5PO.csv (table listing costs and phylogenetic
  distinctiveness scores of the TOP5% most phylogenetically distinctive
  species by taxonomic group)
- table_invacost_TOP5FO.csv (table listing costs and functional
  distinctiveness scores of the TOP5% most functionally distinctive
  species by taxonomic group)
- quanti_consparadox.csv (table with quantitative info about all species
  in InvaCost being either threatened and/or in TOP25PO and/or in
  TOP25FO and/or in TOP5PO and/or in TOP5FO)
- FIGURE2_TOP25.png (Euler diagram taking into account the TOP25PO and
  TOP25FO)
- FIGURE2_TOP5.png (Euler diagram taking into account the TOP5PO and
  TOP5FO)
- FIGURE1.jpeg (figure showing threat status and distinctiveness of
  species in InvaCost in in comparison of all species of their taxonomic
  group)

# 2. Requirements

Before starting the analyses, make sure to follow the two requirements
described below.

## 2.1. clone the repository

Clone this repository in your R working directory on your computer. This
will create a folder “invacost_consparadox” in your R working directory
with 3 folders (data, scripts, outputs).

## 2.2. install packages

Install the following R packages:

``` r
install.packages(c("invacost", "readxl", "adiv", "StatMatch", "ape", "phylobase", "cluster", "dplyr", "Rarity", "kader", "ade4",
                   "rgnparser", "taxize", "mammals", "rebird", "lcvplants", "LCVP", "stringr", "dplyr", "paralell", "plyr"
                   "ggplot2", "DataCombine", "ggpubr", "cowplots", "forcats", "eulerr"))
```

# 3. Analyses

## 3.1. step 1: calculate economic costs by species

Run the script “01_costs_by_species.R”. This will allow you to:

- load the InvaCost database version 4.1
- remove from this database (i) potential costs, (ii) costs from
  unreliable sources, (iii) costs associated to subspecies, (iv) entries
  without cost in “Cost estimate per year 2017 exchange rate” and (v)
  entries with no information on starting and ending years
- identify taxonomic duplicates in the database and correct them
- check the entries associated to species names that have both domestic
  and wild representatives to keep only entries associated with wild
  representatives (the actual removal is done in step 4)
- calculate an average annual damage cost by species based on the
  aggregation of damage cost entries over all sites and all dates after
  1960
- calculate an average annual management cost by species based on the
  aggregation of management cost entries over all sites and all dates
  after 1960.

## 3.2. step 2: calculate distinctiveness (= originality) scores of species

Run the script “02_originality_scores.R”. This will allow you to:

- calculate tree-based phylogenetic distinctiveness scores for mammals
- calculate distance-based phylogenetic distinctiveness scores for
  mammals (no further used in the paper)
- calculate tree-based diet distinctiveness scores for mammals
- calculate distance-based diet distinctiveness scores for mammals (no
  further used in the paper)
- calculate tree-based activity distinctiveness scores for mammals
- calculate distance-based activity distinctiveness scores for mammals
  (no further used in the paper)
- calculate tree-based mass distinctiveness scores for mammals
- calculate distance-based mass distinctiveness scores for mammals (no
  further used in the paper)
- calculate tree-based functional distinctiveness scores for mammals
- calculate distance-based functional distinctiveness scores for mammals
  (no further used in the paper)
- calculate tree-based phylogenetic distinctiveness scores for birds
- calculate distance-based phylogenetic distinctiveness scores for birds
  (no further used in the paper)
- calculate tree-based diet distinctiveness scores for birds
- calculate distance-based diet distinctiveness scores for birds (no
  further used in the paper)
- calculate tree-based activity distinctiveness scores for birds
- calculate distance-based activity distinctiveness scores for birds (no
  further used in the paper)
- calculate tree-based mass distinctiveness scores for birds
- calculate distance-based mass distinctiveness scores for birds (no
  further used in the paper)
- calculate tree-based functional distinctiveness scores for birds
- calculate distance-based functional distinctiveness scores for birds
  (no further used in the paper)
- calculate tree-based phylogenetic distinctiveness scores for plants
- calculate tree-based functional distinctiveness scores for plants

## 3.3. step 3: homogenize taxonomy over the different datasets

Run the script “03_homogenize_taxo.R”. This will allow you to harmonise
species names across the datasets describing species economic costs,
phylogenetic and functional distinctiveness scores and threat status.

## 3.4. step 4: build and describe the final database

Run the script “04_build_describe_database.R”. This will allow you to:

- combine information on costs, phylogenetic and functional
  distinctiveness and threat status in a single dataset by the
  harmonised species names
- remove entries related to domestic, extinct and extinct in the wild
  species from this single dataset to obtain the final dataset used for
  the analyses in the paper
- describe the final dataset

## 3.5. step 5: generate figures and tables

Run the script “05_analyses_figures_tables.R”. This will allow you to
generate all the figures and tables of the paper. Note that, although
this script is the backbone to generate the figures and tables of the
paper, the final figures and tables appearing in the paper may slightly
differ from those generated with this script because of customization
out of R.
