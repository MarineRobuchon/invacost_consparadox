#############################################################################################
# Script to combine the databases of scores, costs and status for the three taxa
# and to describe the resulting final database
# original script by Marine Robuchon
#############################################################################################

### load packages ----


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
birds_taxref <- read.csv2("outputs/birds_taxmatch_ebird.csv")[, -1]
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
length(taxref_unified$raw_name[-which(is.na(taxref_unified$name2use))]) # among which 409 703 have a name to use - so we matched 97.5 % of names

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
            select(-c("raw_name", "parsed")) %>%
            distinct_all() %>%
            rename(species = name2use)

write.csv2(final_db, "outputs/final_db.csv")

### describe the final database ----





