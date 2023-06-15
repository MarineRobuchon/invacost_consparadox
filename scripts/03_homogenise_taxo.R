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

# save the table of both raw and parsed binomial to be used for matching when building the final database
taxa_unified <- rbind(mammals_unified, birds_unified, plants_unified)
taxa_unified <- unique(taxa_unified[, c("taxon", "raw_name", "parsed_binomial")])
colnames(taxa_unified)[3] <- "parsed"
length(unique(taxa_unified$raw_name))
length(unique(taxa_unified$parsed))
write.csv2(taxa_unified, "outputs/taxa_rawandparsednames.csv")


### step 2 of taxonomic homogenisation: match taxonomic databases ----
## mammals - option IUCN RL 2022-1 as the taxonomic reference and itis + mdd for unmatched names
mammals <- tibble(parsed = unique(mammals_unified %>% 
                       pull(parsed_binomial)))  %>%
            left_join(iucn[, 2:3], by = c("parsed" = "scientificName"), keep = TRUE) 

nrow(mammals[which(is.na(mammals$internalTaxonId)),]) # the 769 unmatched names that we will try to retrieve from iucn RL 2022-1 synomyms and itis

rlsyn <- read.csv("./data/RL_2022-1/synonyms.csv", header = TRUE, sep = ",")

rlsyn$synonym <- paste(rlsyn$genusName, rlsyn$speciesName, sep = " ")

mammals_rlsyn <- tibble(parsed = unique(mammals$parsed[which(is.na(mammals$internalTaxonId))]))  %>%
                  left_join(rlsyn[, c(1, 2, 9)], by = c("parsed" = "synonym")) 

mammals_itis <- tibble(parsed = unique(mammals_rlsyn$parsed[which(is.na(mammals_rlsyn$scientificName))]),
                       tsn = NA)
mammals_itis$tsn <- get_tsn(mammals_itis$parsed) # get tsn

mammals_mdd <- tibble(parsed = unique(mammals_itis$parsed[which(is.na(mammals_itis$tsn))]))

mammals_itis <- mammals_itis[-which(is.na(mammals_itis$tsn)),] # remove names without tsn
mammals_itis$scientificName <- itis_acceptname(mammals_itis$tsn)$acceptedname # find accepted names
mammals_itis$scientificName <- word(gn_parse_tidy(mammals_itis$scientificName)$canonicalsimple, 1, 2)

mammals_mdd$scientificName <- sapply(mammals_mdd$parsed, 
                                     function(x) gnr_resolve(x, data_source_ids = 184, canonical = TRUE, best_match_only = TRUE)$matched_name2)

mammals <- mammals[-which(is.na(mammals$scientificName)),]
mammals_rlsyn <- mammals_rlsyn[-which(is.na(mammals_rlsyn$scientificName)),]
mammals_itis <- mammals_itis[-which(is.na(mammals_itis$scientificName)),]
mammals_mdd <- mammals_mdd[-which(mammals_mdd$scientificName=="NULL"),]

mammals <- unique(rbind(mammals [, c(1, 3)], mammals_rlsyn[, c(1, 3)], mammals_itis[, c(1, 3)], mammals_mdd))
colnames(mammals)[2] <- "iucn_itis_mdd"

# check that there are 2 words into iucn_itis_mdd, NA if not
mammals$length_iucn_itis_mdd <- lengths(strsplit(as.character(mammals$iucn_itis_mdd), " "))
unique(mammals$length_iucn_itis_mdd) # some do not have 2 words 
mammals$iucn_itis_mdd[which(mammals$length_iucn_itis_mdd==1)] <- NA
mammals$iucn_itis_mdd <- as.character(mammals$iucn_itis_mdd)

# save the taxonomic reference table for mammals
write.csv2(mammals[, 1:2], "outputs/mammals_taxmatch_iucnitismdd.csv")

# ## mammals - option mdd itis
# # we will use gnr_resolve with the mammal species diversity database as the reference, and itis for the synonyms
# out <- gnr_datasources()
# mamout <- out[grep("ammal", out$title),] 
# mamout[2,c("description", "title", "id")]
# # Ref database: Mammal Diversity Database. 2021. www.mammaldiversity.org. American Society of Mammalogists. Accessed 2021-01-28. ASM Mammal Diversity Database
# 
# mammals <- unique(mammals_unified %>% 
#                    pull(parsed_binomial)) # the 6738 names to match
# 
# mdd <- sapply(mammals, 
#               function(x) gnr_resolve(x, data_source_ids = 184, canonical = TRUE, best_match_only = TRUE)$matched_name2) # match the accepted names of Mammal Diversity Database
# 
# length(mdd[which(mdd=="NULL")]) # 805 unmatched names that we will try to retrieve from ITIS
# 
# itis <- data.frame(mammals = names(mdd[which(mdd=="NULL")]), tsn = NA)
# itis$tsn <- get_tsn(itis$mammals) # get tsn
# itis <- itis[-which(is.na(itis$tsn)),] # remove names without tsn
# itis$acceptedname <- itis_acceptname(itis$tsn)$acceptedname # find accepted names
# itis$acceptedname <- word(gn_parse_tidy(itis$acceptedname)$canonicalsimple, 1, 2)
# colnames(itis)[1] <- "parsed"
# colnames(itis)[3] <- "mdd_itis"
# 
# 
# mdd[which(mdd=="NULL")] <- NA
# mdd <- unlist(mdd)
# 
# mammals <- tibble(parsed = mammals, mdd_itis = mdd)
# mammals <- mammals[-which(is.na(mammals$mdd_itis)),]
# mammals <- rbind(mammals, itis[, c(1, 3)])
# 
# # check that there are 2 words into mdd_itis, NA if not
# mammals$length_mdd_itis <- lengths(strsplit(mammals$mdd_itis, " "))
# unique(mammals$length_mdd_itis) # some do not have 2 words 
# mammals$mdd_itis[which(mammals$length_mdd_itis==1)] <- NA
# 
# # save the taxonomic reference table for mammals
# write.csv2(mammals[, 1:2], "outputs/mammals_taxmatch_mdditis.csv")

## birds - option IUCN RL 2022-1 as the taxonomic reference and itis + ebird 2022 for unmatched names
birds <- tibble(parsed = unique(birds_unified %>% 
                                    pull(parsed_binomial)))  %>%
  left_join(iucn[, 2:3], by = c("parsed" = "scientificName"), keep = TRUE) 

nrow(birds[which(is.na(birds$internalTaxonId)),]) # the 1986 unmatched names that we will try to retrieve from iucn RL 2022-1 synomyms and itis and ebird

birds_rlsyn <- tibble(parsed = unique(birds$parsed[which(is.na(birds$internalTaxonId))]))  %>%
  left_join(rlsyn[, c(1, 2, 9)], by = c("parsed" = "synonym")) 

birds_itis <- tibble(parsed = unique(birds_rlsyn$parsed[which(is.na(birds_rlsyn$scientificName))]),
                       tsn = NA)
birds_itis$tsn <- get_tsn(birds_itis$parsed) # get tsn

birds_ebird <- tibble(parsed = unique(birds_itis$parsed[which(is.na(birds_itis$tsn))]))

birds_itis <- birds_itis[-which(is.na(birds_itis$tsn)),] # remove names without tsn
birds_itis$scientificName <- itis_acceptname(birds_itis$tsn)$acceptedname # find accepted names
birds_itis$scientificName <- word(gn_parse_tidy(birds_itis$scientificName)$canonicalsimple, 1, 2)

new_tax <- ebirdtaxonomy() # taxonomy ebird V2022

birds_ebird <- birds_ebird %>%
                left_join(new_tax[, c(1, 3)], by = c("parsed" = "sciName"), keep = TRUE) # well, no further match in ebird

birds <- birds[-which(is.na(birds$scientificName)),]
birds_rlsyn <- birds_rlsyn[-which(is.na(birds_rlsyn$scientificName)),]
birds_itis <- birds_itis[-which(is.na(birds_itis$scientificName)),]

birds <- unique(rbind(birds [, c(1, 3)], birds_rlsyn[, c(1, 3)], birds_itis[, c(1, 3)]))
colnames(birds)[2] <- "iucn_itis"

# check that there are 2 words into iucn_itis_ebird, NA if not
birds$length_iucn_itis <- lengths(strsplit(as.character(birds$iucn_itis), " "))
unique(birds$length_iucn_itis) # all have 2 words 

# save the taxonomic reference table for birds
write.csv2(birds[, 1:2], "outputs/birds_taxmatch_iucnitis.csv")

                       
# ## birds - option ebird and itis
# birds <- unique(birds_unified %>%
#  pull(parsed_binomial)) # the 13 148 unique parsed names for birds
# 
# new_tax <- ebirdtaxonomy() # taxonomy ebird V2022
# 
# ebird <- sapply(birds, function(x) tryCatch(rebird::species_code(x),
#                                error = function(e) NA)) # match the code name of the species using package rebird version 1.3.0
# 
# birds <- tibble(parsed = birds,
#                 code = ebird) %>%
#   left_join(rebird:::tax %>%
#               filter(speciesCode %in% ebird) %>%
#               transmute(code = speciesCode, ebird = sciName)) %>%
#   select(-code)
# 
# nrow(birds[which(is.na(birds$ebird)),]) # the 2953 names for which we do not have any match that we will try to retrieve from ITIS
# 
# 
# itis <- data.frame(birds = birds$parsed[which(is.na(birds$ebird))], tsn = NA)
# itis$tsn <- get_tsn(itis$birds) # get tsn
# 
# 
# itis <- itis[-which(is.na(itis$tsn)),] # remove names without tsn
# itis$acceptedname <- itis_acceptname(itis$tsn)$acceptedname # find accepted names
# itis$acceptedname <- word(gn_parse_tidy(itis$acceptedname)$canonicalsimple, 1, 2)
# colnames(itis)[1] <- "parsed"
# colnames(itis)[3] <- "ebird_itis"
# 
# colnames(birds)[2] <- "ebird_itis"
# birds <- birds[-which(is.na(birds$ebird_itis)),]
# birds <- rbind(birds, itis[, c(1, 3)])
# 
# # check that there are 2 words into ebird_itis, NA if not
# birds$length_ebird_itis <- lengths(strsplit(birds$ebird_itis, " "))
# unique(birds$length_ebird_itis) # some do not have 2 words
# birds$ebird_itis[which(birds$length_ebird_itis==1)] #  but they're all NA
# 
# # save the taxonomic reference table for birds
# write.csv2(birds[, 1:2], "outputs/birds_taxmatch_ebirditis.csv")

## plants
plants <- unique(plants_unified %>% 
  pull(parsed_binomial)) # the 389 918 unique parsed names for plants

data(tab_lcvp) # LCVP 3.0.1
str(tab_lcvp)

# parallelisation for plants as they are a lot of names to match
cl <- makeCluster(4)
lcvp <- parSapply(
  cl,
  plants, 
  function(x) unlist(lcvplants::lcvp_search(x)[9])) # returns id of accepted taxon/ using package lcvplants 2.1.0

lcvp[which(lcvp=="NULL")] <- NA # assign NA for unmatched names
lcvp <- unlist(lcvp) # unlist for future matching

plants <- tibble(parsed = plants,
                     accepted_id = lcvp) %>%
  left_join(tab_lcvp %>%
              filter(globalId.of.Output.Taxon %in% lcvp) %>%
              transmute(accepted_id = globalId.of.Output.Taxon, lcvp = gn_parse_tidy(Output.Taxon)$canonicalsimple)) %>%  
  distinct_all()  %>%
  select(-accepted_id)

stopCluster(cl)


# check that there are 2 words into lcvp, NA if not
plants$lcvp <- word(plants$lcvp, 1, 2)
plants$length_lcvp <- lengths(strsplit(plants$lcvp, " "))
unique(plants$length_lcvp) # some do not have 2 words
plants$lcvp[which(plants$length_lcvp==1)] <- NA

# save the taxonomic reference table for plants
write.csv2(plants[, 1:2], "outputs/plants_taxmatch_lcvp.csv")



