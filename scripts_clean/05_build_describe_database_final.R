#############################################################################################
# Script to combine the database of mammals and birds to the one of plants
# and to describe the resulting final database
# original script by Marine Robuchon
#############################################################################################

## Load and prepare data
data_mammals_birds <- read.csv2(paste0(getwd(), "/outputs/data_mammals_birds.csv"))[, -1] # database for birds and mammals
head(data_mammals_birds)
colnames(data_mammals_birds)

data_plants <- read.csv2(paste0(getwd(), "/outputs/data_plants.csv"))[, -1] # database for plants
head(data_plants)
colnames(data_plants)
str(data_plants$invacostY)
data_plants$invacostY[which(is.na(data_plants$invacostY))] <- "N"
data_plants$invacostY <- factor(data_plants$invacostY, levels = c("Y", "N"))
levels(data_plants$invacostY)
colnames(data_plants)[6] <- "meanoriFtree" # change the name of this variable so it has the same name than in the db for birds and mammals
data_plants$className <- "PLANTS"

commoncol <- intersect(colnames(data_mammals_birds), colnames(data_plants))
commoncol

mammalsbirds <- data_mammals_birds[, commoncol]
colnames(mammalsbirds)
plants <- data_plants[, commoncol]
colnames(plants)

# bind the data for the columns in common
data_all <- rbind(mammalsbirds, plants)

# add the columns of the different functional originalities for birds & mammals
data_all <- merge(data_all, data_mammals_birds[, c("Species", "dietoriFtree", "activityoriFtree", "massoriFtree")], by = "Species", all.x = TRUE)
head(data_all)
str(data_all)

## Describe the database
# damage costs by taxon
damage <- data_all[-which(is.na(data_all$Average.annual.cost_damage)),]
nrow(damage[which(damage$className=="MAMMALIA"),]) # 21
nrow(damage[which(damage$className=="AVES"),]) # 13
nrow(damage[which(damage$className=="PLANTS"),]) # 112
nrow(damage) # 146
length(unique(damage$Species)) # 146, so no duplicate

# management costs by taxon
management <- data_all[-which(is.na(data_all$Average.annual.cost_management)),]
nrow(management[which(management$className=="MAMMALIA"),]) # 36
nrow(management[which(management$className=="AVES"),]) # 15
nrow(management[which(management$className=="PLANTS"),]) # 287
nrow(management) # 338
length(unique(management$Species)) # 338, so no duplicate

# number of cost entries by taxon (damage + management) : 39 + 20 + 323 = 382
length(unique(c(damage$Species[which(damage$className=="MAMMALIA")], management$Species[which(management$className=="MAMMALIA")]))) # 39 
length(unique(c(damage$Species[which(damage$className=="AVES")], management$Species[which(management$className=="AVES")]))) # 20
length(unique(c(damage$Species[which(damage$className=="PLANTS")], management$Species[which(management$className=="PLANTS")]))) # 323

# number of species in InvaCost
nrow(data_all[which(data_all$invacostY=="Y"),]) # 397
nrow(data_all[which(data_all$invacostY=="Y" & data_all$className=="MAMMALIA"),]) # 44
nrow(data_all[which(data_all$invacostY=="Y" & data_all$className=="AVES"),]) # 20
nrow(data_all[which(data_all$invacostY=="Y" & data_all$className=="PLANTS"),]) # 333

# threat status by taxon
threat <- data_all[-which(is.na(data_all$redlistCategory_version_2020.2)),]
nrow(threat[which(threat$className=="MAMMALIA"),]) # 5813
nrow(threat[which(threat$className=="AVES"),]) # 10983
nrow(threat[which(threat$className=="PLANTS"),]) # 16019
nrow(threat) # 32815

# phylogenetic originality by taxon
PO <- data_all[-which(is.na(data_all$oriPtree)),]
nrow(PO[which(PO$className=="MAMMALIA"),]) # 5425
nrow(PO[which(PO$className=="AVES"),]) # 8095
nrow(PO[which(PO$className=="PLANTS"),]) # 356184
nrow(PO) # 369704

# functional originality by taxon
FO <- data_all[-which(is.na(data_all$meanoriFtree)),]
nrow(FO[which(FO$className=="MAMMALIA"),]) # 4798
nrow(FO[which(FO$className=="AVES"),]) # 8095
nrow(FO[which(FO$className=="PLANTS"),]) # 212530
nrow(FO) # 225423

## Save the database
write.csv2(data_all, paste0(getwd(), "/outputs/data_all.csv"))


