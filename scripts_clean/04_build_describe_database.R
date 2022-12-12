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
mammals_taxref <- read.csv2("outputs/mammals_taxmatch_mdditis.csv")[, -1]
birds_taxref <- read.csv2("outputs/birds_taxmatch_ebirditis.csv")[, -1]
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

length(taxref_unified$raw_name) # the 419 484 raw names with a parsed name for mammals, birds and plants
length(taxref_unified$raw_name[-which(is.na(taxref_unified$name2use))]) # among which 405 076 have a name to use - so we matched 96.6 % of names

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

final_db <- taxref_unified %>%
            left_join(iucn[, c("raw_name", "redlistCategory")]) %>%
            left_join(oriPtree) %>%
            left_join(oriFtree) %>%
            left_join(costs_d[, c("raw_name", "damage_cost")]) %>%
            left_join(costs_m[, c("raw_name", "management_cost")]) %>%
            select(-parsed) %>%
            distinct_all() %>%
            rename(species = name2use) # 419484 rows

# remove species with no name to use and Canis lupus
final_db <- final_db[-which(is.na(final_db$species)),]
final_db <- final_db[-which(final_db$species=="Canis lupus"),]

# check duplicates
duplicates <- unique(final_db$species[duplicated(final_db$species)]) # 48653 names that appear twice or more
dup_db <- final_db[which(final_db$species %in% duplicates),] # so we have to keep rows for which raw_name = species and discard others
final_db <- final_db[-which(final_db$species %in% duplicates & final_db$species != final_db$raw_name),]
duplicates <- unique(final_db$species[duplicated(final_db$species)]) # no more duplicates!

# save the final database
write.csv2(final_db, "outputs/final_db.csv")

### describe the final database ----
final_db <- read.csv2("outputs/final_db.csv")[, -1]

# damage costs by taxon
damage <- final_db[-which(is.na(final_db$damage_cost)),]
nrow(damage[which(damage$taxon=="MAMMALS"),]) # 20
nrow(damage[which(damage$taxon=="BIRDS"),]) # 10
nrow(damage[which(damage$taxon=="PLANTS"),]) # 49
nrow(damage) # 79
length(unique(damage$species)) # 79, so no duplicate

# management costs by taxon
management <- final_db[-which(is.na(final_db$management_cost)),]
nrow(management[which(management$taxon=="MAMMALS"),]) # 39
nrow(management[which(management$taxon=="BIRDS"),]) # 14
nrow(management[which(management$taxon=="PLANTS"),]) # 273
nrow(management) # 326
length(unique(management$species)) # 326, so no duplicates

# number of cost entries by taxon (damage + management) : 41 + 17 + 288 = 346
length(unique(c(damage$species[which(damage$taxon=="MAMMALS")], management$species[which(management$taxon=="MAMMALS")]))) # 41
length(unique(c(damage$species[which(damage$taxon=="BIRDS")], management$species[which(management$taxon=="BIRDS")]))) # 17
length(unique(c(damage$species[which(damage$taxon=="PLANTS")], management$species[which(management$taxon=="PLANTS")]))) # 288

# threat status by taxon
threat <- final_db[-which(is.na(final_db$redlistCategory)),]
nrow(threat[which(threat$taxon=="MAMMALS"),]) # 5651
nrow(threat[which(threat$taxon=="BIRDS"),]) # 10034
nrow(threat[which(threat$taxon=="PLANTS"),]) # 57072
nrow(threat) # 72757

# phylogenetic originality by taxon
PO <- final_db[-which(is.na(final_db$oriPtree)),]
nrow(PO[which(PO$taxon=="MAMMALS"),]) # 5371
nrow(PO[which(PO$taxon=="BIRDS"),]) # 8202
nrow(PO[which(PO$taxon=="PLANTS"),]) # 279604
nrow(PO) # 293177

# functional originality by taxon
FO <- final_db[-which(is.na(final_db$oriFtree)),]
nrow(FO[which(FO$taxon=="MAMMALS"),]) # 4740
nrow(FO[which(FO$taxon=="BIRDS"),]) # 8202
nrow(FO[which(FO$taxon=="PLANTS"),]) # 237205
nrow(FO) # 250147


