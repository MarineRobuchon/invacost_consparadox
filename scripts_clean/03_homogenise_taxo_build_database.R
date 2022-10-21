####################################################################################################
# Script to homogenise taxonomy across the different datasets to compare
# before building the final database by merging IUCN status, originality scores and INVACOST info
# original script by Marine Robuchon
# based on Workflow 1 described in Grenié et al. 2022 (https://doi.org/10.1111/2041-210X.13802)
####################################################################################################

### load packages ----
library(rredlist)
rltoken <- "d2db90179206dcdc872af96c3ec4ebba3e80d0d2e9a37bbf8831a8641a0456b1" # this is my personal token to use the IUCN Red List API, please generate your own (here: https://apiv3.iucnredlist.org/api/v3/token) and replace it



### load data ----
## IUCN
iucn <- read.delim("./data/IUCNdata.txt", header = TRUE) 
colnames(iucn)

# this only includes data for mammals and birds, and it's 2020 version - would be nice to change that
# I made a request on IUCN red list with robuchon@mnhn.fr, this is still processing



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



