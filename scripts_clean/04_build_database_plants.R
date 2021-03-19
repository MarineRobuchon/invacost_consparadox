#############################################################################################
# script to build the database by merging IUCN status, originality scores and INVACOST info
# for plants
# original script by Marine Robuchon
#############################################################################################

## Load packages
library(dplyr)
library(invacost)

## Load and prepare data

# invacost
data(invacost)

# phylogenetic originality, threat status, exotic status and presence in InvACOST
scores_plants_phylo <- read.csv2(paste0(getwd(), "/data/plants_tree-based_phylo_IUCN_and_GLONAF.csv")) 
levels(factor(scores_plants_phylo$threatened)) # so the info on NT and LC is not here? need to fix this
scores_plants_phylo <- scores_plants_phylo[, c("name_with_space", "ED", "INVACOST", "threatened", "alien")]
colnames(scores_plants_phylo) <- c("Species", "oriPtree", "invacostY", "redlistCategory_version_2020.2", "Exotic")
scores_plants_phylo$invacostY <- as.factor(scores_plants_phylo$invacostY)
scores_plants_phylo$redlistCategory_version_2020.2 <- as.factor(scores_plants_phylo$redlistCategory_version_2020.2)
scores_plants_phylo$Exotic <- as.factor(scores_plants_phylo$Exotic)
levels(scores_plants_phylo$invacostY) <- "Y"
levels(scores_plants_phylo$redlistCategory_version_2020.2) <- c(NA, "Critically Endangered", "Data Deficient", "Endangered", 
                                                                "Extinct in the Wild", "Extinct", "Least Concern",
                                                                "LR", "Near Threatened", "Vulnerable")
levels(scores_plants_phylo$Exotic) <- c("FALSE", "TRUE")

scores_plants_phylo[which(scores_plants_phylo$redlistCategory_version_2020.2=="LR") ,]

# remove extinct and extinct in the wild species
scores_plants_phylo <- scores_plants_phylo[-which(scores_plants_phylo$redlistCategory_version_2020.2 %in% c("Extinct in the Wild", "Extinct")) ,]

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

## Calculate occurrence by species and  number of publication by species
# occurences in invacost 
invacost$frequence <- 1

freq_cost<-invacost %>%
  group_by(Species) %>%
  summarise(freq_cost = sum(frequence))

head(as.data.frame(freq_cost))

# number of publications by species in invacost
species_pub <- unique(invacost[, c("Species", "Reference_ID")])
head(species_pub)
species_pub$frequence <- 1

freq_publi<-species_pub %>%
  group_by(Species) %>%
  summarise(freq_publi = sum(frequence))

head(as.data.frame(freq_publi))

# add frequency values in invacost database
invacostF <- inner_join(invacost,freq_cost)
invacostF <- inner_join(invacostF, freq_publi)
colnames(invacostF)
invacostF <- unique(invacostF[, c("Species", "freq_cost", "freq_publi")])

# add these frequency values to scores_plants
scores_plants <- left_join(scores_plants, invacostF, by = "Species")
colnames(scores_plants)

## save the database for plants 
write.csv2(scores_plants, paste0(getwd(), "/outputs/data_plants.csv"))
