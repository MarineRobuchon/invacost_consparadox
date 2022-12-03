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
library(LCVP)
library(stringr)
library(dplyr)
library(parallel)

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
length(unique(mammals_unified$parsed_binomial)) # 6 738 unique parsed binomial names

# birds (= "AVES" + "Aves")
birds_unified <- all_raw[which(all_raw$taxon %in% c("AVES", "Aves")),]
birds_unified$taxon <- "BIRDS"
length(unique(birds_unified$parsed_binomial)) # 13 148 unique parsed binomial names

# plants (= "PLANTS" + "Magnoliopsida" + "Liliopsida" + "Pinopsida")
plants_unified <- all_raw[which(all_raw$taxon %in% c("PLANTS", "Magnoliopsida", "Liliopsida", "Pinopsida")),]
plants_unified$taxon <- "PLANTS"
length(unique(plants_unified$parsed_binomial)) # 389 918 unique parsed binomial names

### step 2 of taxonomic homogenisation: match taxonomic databases ----
## mammals
# we will use gnr_resolve with the mammal species diversity database as the reference, and itis for the synonyms1&
out <- gnr_datasources()
mamout <- out[grep("ammal", out$title),] 
mamout[2,c("description", "title", "id")]
# Ref database: Mammal Diversity Database. 2021. www.mammaldiversity.org. American Society of Mammalogists. Accessed 2021-01-28. ASM Mammal Diversity Database

mammals <- unique(mammals_unified %>% 
                   pull(parsed_binomial)) # the 6738 names to match

mdd <- sapply(mammals, 
              function(x) gnr_resolve(x, data_source_ids = 184, canonical = TRUE, best_match_only = TRUE)$matched_name2) # match the accepted names of Mammal Diversity Database

length(mdd[which(mdd=="NULL")]) # 805 unmatched names that we will try to retrieve from ITIS

# tsn
# 
# testitis <- itis_acceptname(get_tsn(c("Mesocapromys nanus", "Physeter catodon", "Spilogale aquaticus")))
# 
# testnotfound <- get_tsn("Spilogale aquaticus")
# 
# testnotfound

itis <- data.frame(mammals = names(mdd[which(mdd=="NULL")]), tsn = NA)
itis$tsn <- get_tsn(itis$mammals) # get tsn
itis <- itis[-which(is.na(itis$tsn)),] # remove names without tsn
itis$acceptedname <- itis_acceptname(itis$tsn)$acceptedname # find accepted names
itis$acceptedname <- word(gn_parse_tidy(itis$acceptedname)$canonicalsimple, 1, 2)
colnames(itis)[1] <- "parsed"
colnames(itis)[3] <- "mdd_itis"


# itis <- itis_acceptname(get_tsn(names(mdd[which(mdd=="NULL")])))
# itis$mammals <- names(mdd[which(mdd=="NULL")])

mdd[which(mdd=="NULL")] <- NA
mdd <- unlist(mdd)

mammals <- tibble(parsed = mammals, mdd_itis = mdd)
mammals <- mammals[-which(is.na(mammals$mdd_itis)),]
mammals <- rbind(mammals, itis[, c(1, 3)])

write.csv2(mammals, "outputs/mammals_taxmatch_mdditis.csv")
                       
## birds
birds <- unique(birds_unified %>%
 pull(parsed_binomial)) # the 13 148 unique parsed names for birds

new_tax <- ebirdtaxonomy()

ebird <- sapply(birds, function(x) tryCatch(rebird::species_code(x),
                               error = function(e) NA))

birds <- tibble(parsed = birds,
                code = ebird) %>%
  left_join(rebird:::tax %>%
              filter(speciesCode %in% ebird) %>%
              transmute(code = speciesCode, ebird = sciName)) %>%
  select(-code)

write.csv2(birds, "outputs/birds_taxmatch_ebird.csv")

## plants
plants <- unique(plants_unified %>% 
  pull(parsed_binomial)) # the 389 918 unique parsed names for plants

data(tab_lcvp)
str(tab_lcvp)

# parallelisation for plants as they are a lot of names to match
cl <- makeCluster(4)
lcvp <- parSapply(
  cl,
  plants, 
  function(x) unlist(lcvplants::lcvp_search(x)[9])) # returns id of accepted taxon

lcvp[which(lcvp=="NULL")] <- NA # assign NA for unmatched names
lcvp <- unlist(lcvp) # unlist for future matching

plants <- tibble(parsed = plants,
                     accepted_id = lcvp) %>%
  left_join(tab_lcvp %>%
              filter(globalId.of.Output.Taxon %in% lcvp) %>%
              transmute(accepted_id = globalId.of.Output.Taxon, lcvp = gn_parse_tidy(Output.Taxon)$canonicalsimple)) %>%  
  distinct_all()  %>%
  select(-accepted_id)

write.csv2(plants, "outputs/plants_taxmatch_lcvp.csv")

stopCluster(cl)






