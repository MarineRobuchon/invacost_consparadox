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
- taxonomy_mamPhy_5911species.csv
- MamFuncDat.txt
- AllBirdsEricson1.tre
- BirdFuncDat.txt
- ALLMB.tre
- traits_to_run.csv

This folder further includes the following folders:

- Completed_5911sp_topoCons_FBDasZhouEtAl

## 1.2. scripts

This folder includes the following scripts:

- 01_costs_by_species.R (script to calculate economic cost by species)
- 02_originality_scores.R (script to calculate originality scores by
  species)
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
- mammals_vertlife_distance-based_phylori.csv
- mammals_vertlife_tree-based_dietori.csv
- mammals_vertlife_distance-based_dietori.csv
- mammals_vertlife_tree-based_activityori.csv
- mammals_vertlife_distance-based_activityori.csv
- mammals_vertlife_tree-based_massori.csv
- mammals_vertlife_distance-based_massori.csv
- mammals_vertlife_tree-based_funcori.csv
- mammals_vertlife_distance-based_funcori.csv
- birds_vertlife_tree-based_phylori.csv
- birds_vertlife_distance-based_phylori.csv
- birds_vertlife_tree-based_dietori.csv
- birds_vertlife_distance-based_dietori.csv
- birds_vertlife_tree-based_activityori.csv
- birds_vertlife_distance-based_activityori.csv
- birds_vertlife_tree-based_massori.csv
- birds_vertlife_distance-based_massori.csv
- birds_vertlife_tree-based_funcori.csv
- birds_vertlife_distance-based_funcori.csv
- plants_tree-based_phylori.csv
- oriplantsmiss.csv

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
install.packages(c("invacost", "readxl", "adiv", "StatMatch", "ape", "phylobase", "cluster", "dplyr", "Rarity", "kader", "ade4"))
```

# 3. Analyses

## 3.1. step 1: calculate economic costs by species

## 3.2. step 2: calculate distinctiveness (= originality) scores of species

## 3.3. step 3: homogenize taxonomy over the different datasets

## 3.4. step 4: build and describe the final database

## 3.5. step 5: generate figures and tables

Run the script “analyses_figures_tables.R”. This will allow you to
generate all the figures and tables of the paper. Note that, although
this script is the backbone to generate the figures and tables of the
paper, the final figures and tables appearing in the paper may slightly
differ from those generated with this script because of customizations
out of R.
