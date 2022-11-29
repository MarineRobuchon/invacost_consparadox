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
# install.packages("rgnparser")
# rgnparser::install_gnparser()

library(rgnparser)
library(taxize)
library(mammals)
library (rebird)
library(lcvplants)
library(stringr)
library(dplyr)

### load data ----
## IUCN
iucn <- read.csv("./data/RL_2022-1/simple_summary.csv", header = TRUE, sep = ",") # iucn global assessments for mammals, birds and plants (downloaded on 22/10/2022)

## originality (= distinctiveness) scores
mammalPtree <- read.csv2("./outputs/mammals_vertlife_tree-based_phylori.csv", header = TRUE)
birdPtree <- read.csv2("./outputs/birds_vertlife_tree-based_phylori.csv", header = TRUE)
plantPtree <- read.csv2("./outputs/plants_tree-based_phylori.csv", header = TRUE)

mammalFtree <- read.csv2("./outputs/mammals_vertlife_tree-based_funcori.csv", header=TRUE)
birdFtree <- read.csv2("./outputs/birds_vertlife_tree-based_funcori.csv", header=TRUE)
plantFtree <- read.csv2("./outputs/oriplantsmiss.csv", header = TRUE, sep = " ")

## cost information
costs_d <- read.csv2("./outputs/damagecost_by_species.csv", header = TRUE)
costs_m <- read.csv2("./outputs/managementcost_by_species.csv", header = TRUE)

### step 1 of taxonomic homogenisation: preprocess names ----
## get a list of raw names across all datasets used
iucn_raw <- data.frame(source = "iucn", taxon = iucn$className, raw_name = iucn$scientificName)
iucn_raw$taxon[-which(iucn_raw$taxon %in% c("MAMMALIA", "AVES"))] <- "PLANTS"

phylo_mammal_raw <- data.frame (source = "phylo", taxon = "MAMMALIA", raw_name = mammalPtree$X)
phylo_bird_raw <- data.frame (source = "phylo", taxon = "AVES", raw_name = birdPtree$X)
phylo_plant_raw <- data.frame (source = "phylo", taxon = "PLANTS", raw_name = plantPtree$X)
phylo_raw <- rbind(phylo_mammal_raw, phylo_bird_raw, phylo_plant_raw)
phylo_raw$raw_name <- gsub (pattern = "_", replacement = " ", x = phylo_raw$raw_name)

functio_mammal_raw <- data.frame (source = "functio", taxon = "MAMMALIA", raw_name = mammalFtree$X)
functio_bird_raw <- data.frame (source = "functio", taxon = "AVES", raw_name = birdFtree$X)
functio_plant_raw <- data.frame (source = "functio", taxon = "PLANTS", 
                                 raw_name = gsub("_", " ", row.names(plantFtree)))
functio_raw <- rbind(functio_mammal_raw, functio_bird_raw, functio_plant_raw)

costs_d_raw <- data.frame (source = "costs", taxon = costs_d$Class, raw_name = costs_d$Species)
costs_m_raw <- data.frame (source = "costs", taxon = costs_m$Class, raw_name = costs_m$Species)
costs_raw <- rbind(costs_d_raw, costs_m_raw)

all_raw <- unique(rbind(iucn_raw, phylo_raw, functio_raw, costs_raw))
list_rawnames <- unique(all_raw$raw_name) # 430286 raw names across all our databases

## standardize the writing style of taxon names
parsed_all_raw <- gn_parse_tidy (all_raw$raw_name) # parse name
all_raw$parsed_binomial <- word(parsed_all_raw$canonicalsimple, 1, 2) # keep the first two words of the parsed raw names
all_raw <- all_raw[-which(is.na(all_raw$parsed_binomial)),] # remove names that did not pass parsing
length(unique (all_raw$parsed_binomial)) # 410045 names after parsing
all_raw$length_parsed_binomial <- lengths(strsplit(all_raw$parsed_binomial, " ")) # count the number of words in the parsed binomial
unique(all_raw$length_parsed_binomial) # they all have 2 words

# names in column parsed_binomial form the unified species names

### step 1.5 of taxonomic homogenisation: split the unified species names by taxon ----
unique(all_raw$taxon)
# mammals (= "MAMMALIA" + "Mammalia")
mammals_unified <- all_raw[which(all_raw$taxon %in% c("MAMMALIA", "Mammalia")),]
mammals_unified$taxon <- "MAMMALS"
length(unique(mammals_unified$parsed_binomial)) # 6738 unique parsed binomial names

# birds (= "AVES" + "Aves")

# plants (= "PLANTS" + "Magnoliopsida" + "Liliopsida" + "Pinopsida")

### step 2 of taxonomic homogenisation: match taxonomic databases ----
## mammals
data(mammals) # the taxonomic reference for mammals
head(mammals) 
mammals_unified$mammals_match <- mammals_unified$parsed_binomial %in% mammals$canonical_sciname
length(unique(mammals_unified$parsed_binomial[which(mammals_unified$mammals_match == TRUE)])) # among which 5933 match the taxonomic reference for mammals
length(unique(mammals_unified$parsed_binomial[which(mammals_unified$mammals_match == FALSE)])) # and 805 do not match the taxonomic reference for mammals
mammals_unified$mammals_name <- mammals_unified$parsed_binomial
mammals_unified <- merge(mammals_unified, mammals[c("canonical_sciname", "notes")], by.x = "mammals_name", by.y = "canonical_sciname", all.x = TRUE)
mammals_unified$mammals_name[which(mammals_unified$mammals_match == FALSE)] <- NA

colnames(mammals_unified)
mammals_unified <- mammals_unified %>% select(source, taxon, raw_name, parsed_binomial, length_parsed_binomial, mammals_match, mammals_name, notes)

# we now need to (i) for names matching the reference database, check synonmys to identify the name to be used
# and (ii) for names not matching the reference database, retrieve the the name to be used with gnr_resolve from taxize



### step 2 of taxonomic homogenisation: match taxonomic databases ----



