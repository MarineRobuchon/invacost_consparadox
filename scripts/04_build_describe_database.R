#############################################################################################
# Script to combine the databases of scores, costs and status for the three taxa
# and to describe the resulting final database
# original script by Marine Robuchon
#############################################################################################

### load packages ----
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

## taxonomic references
# mammals_taxref <- read.csv2("outputs/mammals_taxmatch_mdditis.csv")[, -1]
# birds_taxref <- read.csv2("outputs/birds_taxmatch_ebirditis.csv")[, -1]
mammals_taxref <- read.csv2("outputs/mammals_taxmatch_iucnitismdd.csv")[, -1]
birds_taxref <- read.csv2("outputs/birds_taxmatch_iucnitis.csv")[, -1]
plants_taxref <- read.csv2("outputs/plants_taxmatch_lcvp.csv")[, -1]
taxa_unified <- read.csv2("outputs/taxa_rawandparsednames.csv")[, -1]

### format data to make them comparable----
## the taxonomic references
colnames(mammals_taxref)[2] <- "name2use"
colnames(birds_taxref)[2] <- "name2use"
colnames(plants_taxref)[2] <- "name2use"

taxref <- rbind(mammals_taxref, birds_taxref, plants_taxref)


taxref_unified <- taxa_unified  %>%
                       left_join(taxref) 

length(taxref_unified$raw_name) # the 419 545 raw names with a parsed name for mammals, birds and plants
length(taxref_unified$raw_name[-which(is.na(taxref_unified$name2use))]) # among which 407 011 have a name to use - so we matched 97 % of names

## the original data
colnames(iucn)[3] <- "raw_name"

colnames(mammalPtree)[1] <- "raw_name"
colnames(mammalPtree)[2] <- "oriPtree"
mammalPtree$raw_name <- gsub (pattern = "_", replacement = " ", x = mammalPtree$raw_name)
colnames(birdPtree)[1] <- "raw_name"
colnames(birdPtree)[2] <- "oriPtree"
birdPtree$raw_name <- gsub (pattern = "_", replacement = " ", x = birdPtree$raw_name)
colnames(plantPtree)[1] <- "raw_name"
colnames(plantPtree)[2] <- "oriPtree"
plantPtree$raw_name <- gsub (pattern = "_", replacement = " ", x = plantPtree$raw_name)
oriPtree <- rbind(mammalPtree[, 1:2], birdPtree[, 1:2], plantPtree)

colnames(mammalFtree)[1] <- "raw_name"
colnames(mammalFtree)[2:5] <- c("dietoriFtree", "activityoriFtree", "massoriFtree", "oriFtree")
colnames(birdFtree)[1] <- "raw_name"
colnames(birdFtree)[2:5] <- c("dietoriFtree", "activityoriFtree", "massoriFtree", "oriFtree")
colnames(plantFtree)[1] <- "oriFtree"
plantFtree$raw_name <- gsub (pattern = "_", replacement = " ", x = rownames(plantFtree))
plantFtree$dietoriFtree <- NA
plantFtree$activityoriFtree <- NA
plantFtree$massoriFtree <- NA
plantFtree <- plantFtree[, c("raw_name", "dietoriFtree", "activityoriFtree", "massoriFtree", "oriFtree")]
oriFtree <- rbind(mammalFtree, birdFtree, plantFtree)

colnames(costs_d)[7:8] <- c("raw_name", "damage_cost")
colnames(costs_m)[7:8] <-  c("raw_name", "management_cost")

### match all the data to the taxonomic reference to build the final database ----
## match iucn to the taxonomic reference
iucn_final <- taxref_unified %>%
              filter(!is.na(name2use)) %>%
              left_join(iucn[, c("raw_name", "redlistCategory")]) %>%
                        filter(!is.na(redlistCategory)) %>%
              distinct_all()

# check duplicates
nrow(iucn_final)
length(unique(iucn_final$name2use))

iucn_duplicates <- unique(iucn_final$name2use[duplicated(iucn_final$name2use)]) # 865 names that appear twice or more
db_iucn_dup <- iucn_final[which(iucn_final$name2use %in% iucn_duplicates),] # so we have to keep rows for which parsed = name2use and discard others
iucn_final <- iucn_final[-which(iucn_final$name2use %in% iucn_duplicates & iucn_final$name2use != iucn_final$parsed),]
unique(iucn_final$name2use[duplicated(iucn_final$name2use)]) # no more duplicates! 
  
iucn_final <- unique(iucn_final[, c("taxon", "name2use", "redlistCategory")])


## match phylogenetic originality scores to the taxonomic reference
oriPtree_final <- taxref_unified %>%
  filter(!is.na(name2use)) %>%
  left_join(oriPtree[, c("raw_name", "oriPtree")]) %>%
  filter(!is.na(oriPtree)) %>%
  distinct_all()

# check duplicates
nrow(oriPtree_final)
length(unique(oriPtree_final$name2use))

oriPtree_duplicates <- unique(oriPtree_final$name2use[duplicated(oriPtree_final$name2use)]) # 29955 names that appear twice or more
db_oriPtree_dup <- oriPtree_final[which(oriPtree_final$name2use %in% oriPtree_duplicates),] # so we have to keep rows for which parsed = name2use and discard others
oriPtree_final <- oriPtree_final[-which(oriPtree_final$name2use %in% oriPtree_duplicates & oriPtree_final$name2use != oriPtree_final$parsed),]
oriPtree_duplicates <- unique(oriPtree_final$name2use[duplicated(oriPtree_final$name2use)]) # we still have 4380 duplicates
db_oriPtree_dup <- oriPtree_final[which(oriPtree_final$name2use %in% oriPtree_duplicates),] 
length(db_oriPtree_dup$raw_name[which(db_oriPtree_dup$raw_name!=db_oriPtree_dup$parsed)]) # these are 7120 rows to discard, because it means that they are not species (but subpsecies, varieties...)
oriPtree_final <- oriPtree_final[-which(oriPtree_final$name2use %in% oriPtree_duplicates & oriPtree_final$raw_name != oriPtree_final$parsed),]
unique(oriPtree_final$name2use[duplicated(oriPtree_final$name2use)]) # no more duplicates!


## match functional originality scores to the taxonomic reference
oriFtree_final <- taxref_unified %>%
  filter(!is.na(name2use)) %>%
  left_join(oriFtree) %>%
  filter(!is.na(oriFtree)) %>%
  distinct_all()

# check duplicates
nrow(oriFtree_final)
length(unique(oriFtree_final$name2use))

oriFtree_duplicates <- unique(oriFtree_final$name2use[duplicated(oriFtree_final$name2use)]) # 9873 names that appear twice or more
db_oriFtree_dup <- oriFtree_final[which(oriFtree_final$name2use %in% oriFtree_duplicates),] # so we have to keep rows for which parsed = name2use and discard others
oriFtree_final <- oriFtree_final[-which(oriFtree_final$name2use %in% oriFtree_duplicates & oriFtree_final$name2use != oriFtree_final$parsed),]
oriFtree_duplicates <- unique(oriFtree_final$name2use[duplicated(oriFtree_final$name2use)]) # we still have 4 duplicates
db_oriFtree_dup <- oriFtree_final[which(oriFtree_final$name2use %in% oriFtree_duplicates),] 
length(db_oriFtree_dup$raw_name[which(db_oriFtree_dup$raw_name!=db_oriFtree_dup$parsed)]) # these are the 4 rows to discard, because it means that they are not species (but subpsecies, varieties...)
oriFtree_final <- oriFtree_final[-which(oriFtree_final$name2use %in% oriFtree_duplicates & oriFtree_final$raw_name != oriFtree_final$parsed),]
unique(oriFtree_final$name2use[duplicated(oriFtree_final$name2use)]) # no more duplicates!


## match damage costs to the taxonomic reference
costs_d_final <- taxref_unified %>%
  filter(!is.na(name2use)) %>%
  left_join(costs_d[, c("raw_name", "damage_cost")]) %>%
  filter(!is.na(damage_cost)) %>%
  distinct_all()

# check duplicates
nrow(costs_d_final)
length(unique(costs_d_final$name2use)) # no duplicate!

## match management costs to the taxonomic reference
costs_m_final <- taxref_unified %>%
  filter(!is.na(name2use)) %>%
  left_join(costs_m[, c("raw_name", "management_cost")]) %>%
  filter(!is.na(management_cost)) %>%
  distinct_all()

# check duplicates
nrow(costs_m_final)
length(unique(costs_m_final$name2use)) 

costs_m_duplicates <- unique(costs_m_final$name2use[duplicated(costs_m_final$name2use)]) # 3 names that appear twice or more
db_costs_m_dup <- costs_m_final[which(costs_m_final$name2use %in% costs_m_duplicates),] # so we have to keep rows for which parsed = name2use and discard others
costs_m_final <- costs_m_final[-which(costs_m_final$name2use %in% costs_m_duplicates & costs_m_final$name2use != costs_m_final$parsed),]
unique(costs_m_final$name2use[duplicated(costs_m_final$name2use)]) # one duplicate for Canis lupus but we will remove this entry later anyway


## build the final database
final_db <- taxref_unified %>% 
            filter(!is.na(name2use)) %>% 
            left_join(iucn_final[, c("name2use", "redlistCategory")]) %>%
            left_join(oriPtree_final[, c("name2use", "oriPtree")]) %>%
            left_join(oriFtree_final[, c("name2use", "dietoriFtree",  "activityoriFtree", "massoriFtree", "oriFtree")]) %>%
            left_join(costs_d_final[, c("name2use", "damage_cost")]) %>%
            left_join(costs_m_final[, c("name2use", "management_cost")]) %>%
            select(-c(parsed, raw_name)) %>%
            distinct_all() # 334994 rows

colnames(final_db)[2] <- "species"

# remove domestic species (Canis lupus, Felis catus)
final_db <- final_db[-which(final_db$species %in% c("Canis lupus", "Felis catus")),] # 334992 rows

# remove species extinct and extinct in the wild
final_db <- final_db[-which(final_db$redlistCategory %in% c("Extinct", "Extinct in the Wild")),] #334588 rows

# check duplicates
duplicates <- unique(final_db$species[duplicated(final_db$species)]) # no duplicate!

# save the final database
write.csv2(final_db, "outputs/final_db.csv")

### describe the final database ----
final_db <- read.csv2("outputs/final_db.csv")[, -1]

# damage costs by taxon
damage <- final_db[-which(is.na(final_db$damage_cost)),]
nrow(damage[which(damage$taxon=="MAMMALS"),]) # 19
nrow(damage[which(damage$taxon=="BIRDS"),]) # 10
nrow(damage[which(damage$taxon=="PLANTS"),]) # 53
nrow(damage) # 82
length(unique(damage$species)) # 82, so no duplicate

# management costs by taxon
management <- final_db[-which(is.na(final_db$management_cost)),]
nrow(management[which(management$taxon=="MAMMALS"),]) # 33
nrow(management[which(management$taxon=="BIRDS"),]) # 14
nrow(management[which(management$taxon=="PLANTS"),]) # 287
nrow(management) # 334
length(unique(management$species)) # 334, so no duplicates

# number of cost entries by taxon (damage + management) : 36 + 17 + 302 = 355
length(unique(c(damage$species[which(damage$taxon=="MAMMALS")], management$species[which(management$taxon=="MAMMALS")]))) # 36
length(unique(c(damage$species[which(damage$taxon=="BIRDS")], management$species[which(management$taxon=="BIRDS")]))) # 17
length(unique(c(damage$species[which(damage$taxon=="PLANTS")], management$species[which(management$taxon=="PLANTS")]))) # 302

# threat status by taxon
threat <- final_db[-which(is.na(final_db$redlistCategory)),]
nrow(threat[which(threat$taxon=="MAMMALS"),]) # 5881
nrow(threat[which(threat$taxon=="BIRDS"),]) # 10998
nrow(threat[which(threat$taxon=="PLANTS"),]) # 59401
nrow(threat) # 76280

# phylogenetic originality by taxon
PO <- final_db[-which(is.na(final_db$oriPtree)),]
nrow(PO[which(PO$taxon=="MAMMALS"),]) # 5626
nrow(PO[which(PO$taxon=="BIRDS"),]) # 9758
nrow(PO[which(PO$taxon=="PLANTS"),]) # 287346
nrow(PO) # 302730

# functional originality by taxon
FO <- final_db[-which(is.na(final_db$oriFtree)),]
nrow(FO[which(FO$taxon=="MAMMALS"),]) # 5107
nrow(FO[which(FO$taxon=="BIRDS"),]) # 9758
nrow(FO[which(FO$taxon=="PLANTS"),]) # 245405
nrow(FO) # 260270


