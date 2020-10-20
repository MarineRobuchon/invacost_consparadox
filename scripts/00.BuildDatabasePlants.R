#############################################################################################
# script to build the database by merging IUCN status, originality scores and INVACOST info
# for plants
#############################################################################################

## Load packages
library(dplyr)

## Load and prepare data

# phylogenetic originality, threat status, exotic status and presence in InvACOST
scores_plants_phylo <- read.csv2(paste0(getwd(), "/data/plants_tree-based_phylo_IUCN_and_GLONAF.csv")) 
levels(factor(scores_plants_phylo$threatened)) # so the info on NT and LC is not here? need to fix this
scores_plants_phylo <- scores_plants_phylo[, c("name_with_space", "ED", "INVACOST", "threatened", "alien")]
colnames(scores_plants_phylo) <- c("Species", "oriPtree", "invacostY", "redlistCategory_version_2020.2", "Exotic")
scores_plants_phylo$invacostY <- as.factor(scores_plants_phylo$invacostY)
scores_plants_phylo$redlistCategory_version_2020.2 <- as.factor(scores_plants_phylo$redlistCategory_version_2020.2)
scores_plants_phylo$Exotic <- as.factor(scores_plants_phylo$Exotic)
levels(scores_plants_phylo$invacostY) <- "Y"
levels(scores_plants_phylo$redlistCategory_version_2020.2) <- c(NA, "Critically Endangered", "Endangered", 
                                                                "Extinct in the Wild", "Extinct", "Vulnerable")
levels(scores_plants_phylo$Exotic) <- c("FALSE", "TRUE")

# here we will need to remove extinct and extinct in the wild species

# functional originality scores
scores_plants_functio <-  read.csv2(paste0(getwd(), "/outputs/oriplantsmiss.csv"), sep = " ", dec = ".", row.names = NULL)
colnames(scores_plants_functio) <- c("Species", "oriFtree")
scores_plants_functio$Species <- gsub("_", " ", scores_plants_functio$Species)

# costs by species
costs_d <- read.csv2("./outputs/damagecost_by_species.csv", header = TRUE)
costs_m <- read.csv2("./outputs/managementcost_by_species.csv", header = TRUE)
costs_d <- costs_d[, c("Species", "Average.annual.cost")] # only keep species and cost values
colnames(costs_d)[2] <- "Average.annual.cost_damage"
costs_m <- costs_m[, c("Species", "Average.annual.cost")]
colnames(costs_m)[2] <- "Average.annual.cost_management"

## Merge data by Species
scores_plants <- left_join(scores_plants_phylo, scores_plants_functio, by = "Species")

scores_plants <- scores_plants %>% left_join(costs_d, by = "Species")
scores_plants <- scores_plants %>% left_join(costs_m, by = "Species")

length(unique(scores_plants$Species))
length(scores_plants$Species)

dup_plants <- scores_plants$Species[which(duplicated(scores_plants$Species))] ## Need to check these duplicates in the script "costs_by_species"
dup_score_plants <- scores_plants[which(scores_plants$Species%in%dup_plants),]
  

## Also need to calculate occurrence by species and  number of publication by species
## (lines 97-117 of 00.BuildDatabase.R), and then merge them with the previous dataset
## But I cannot do it because invacost does not exist yet in R version 4.0.2...

## save the database for plants (careful , this is not the final version)
write.csv2(scores_plants, paste0(getwd(), "/outputs/data_plants.csv"))
