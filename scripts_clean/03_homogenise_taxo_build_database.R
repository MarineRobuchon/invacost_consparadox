####################################################################################################
# Script to homogenise taxonomy across the different datasets to compare
# before building the final database by merging IUCN status, originality scores and INVACOST info
# original script by Marine Robuchon
# based on Workflow 1 described in Grenié et al. 2022 (https://doi.org/10.1111/2041-210X.13802)
####################################################################################################

### install and load packages ----
# install.packages("remotes")
# remotes::install_github("idiv-biodiversity/lcplants")
# install.packages("devtools")
# devtools::install_github('idiv-biodiversity/LCVP')
# devtools::install_github('https://github.com/mawiramawira/mammals')
library(rredlist)
library(rgnparser)
library(taxize)
library(mammals)
library (rebird)
library(lcvplants)

### load data ----
## IUCN
iucn <- read.csv("./data/RL_2022-1/simple_summary.csv", header = TRUE, sep = ",") # iucn global assessments for mammals, birds and plants (dowloaded on 22/10/2022)

## originality (= distinctiveness) scores
mammalPtree <- read.csv2("./outputs/mammals_vertlife_tree-based_phylori.csv", header = TRUE)
birdPtree <- read.csv2("./outputs/birds_vertlife_tree-based_phylori.csv", header = TRUE)
plantPtree <- read.csv2("./outputs/plants_tree-based_phylori.csv", header = TRUE)

mammalFtree <- read.csv2("./outputs/mammals_vertlife_tree-based_funcori.csv", header=TRUE)
birdFtree <- read.csv2("./outputs/birds_vertlife_tree-based_funcori.csv", header=TRUE)
plantFtree <- read.csv2("./outputs/oriplantsmiss.csv", header=TRUE)

## cost information
costs_d <- read.csv2("./outputs/damagecost_by_species.csv", header = TRUE)
costs_m <- read.csv2("./outputs/managementcost_by_species.csv", header = TRUE)

### step 1 of taxonomic homogenisation: preprocess names ----
## get a list of raw names across all datasets used



### step 2 of taxonomic homogenisation: match taxonomic databases ----



