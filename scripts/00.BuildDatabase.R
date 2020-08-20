#############################################################################################
# script to build the database by merging IUCN status, originality scores and INVACOST info
# original script by Céline Bellard
# modifications by: Marine Robuchon, XX, 
#############################################################################################

rm(list=ls())
library(dplyr)
library(stringr)
library(invacost)

#setwd("D:/these/Invacost/Marine/") # to personalise if needed
 setwd("D:/Collaboration/Invacost workshop/Phylogenie Marine/") # to personalise if needed

######################################
############# Load data ##############
######################################

# NB plant data are missing for now

iucn<-read.delim("./data/IUCNdata.txt", header=TRUE)
data(invacost)

birdFDist<-read.csv2("./outputs/birds_vertlife_distance-based_funcori.csv", header=TRUE) # NB: I changed the path to outputs, because scores have been saved in outputs
birdFtree<-read.csv2("./outputs/birds_vertlife_tree-based_funcori.csv", header=TRUE)
mammalFDist<-read.csv2("./outputs/mammals_vertlife_distance-based_funcori.csv", header=TRUE)
mammalFtree<-read.csv2("./outputs/mammals_vertlife_tree-based_funcori.csv", header=TRUE)


birdPDist<-read.csv2("./outputs/birds_vertlife_distance-based_phylori.csv", header=TRUE)
birdPtree<-read.csv2("./outputs/birds_vertlife_tree-based_phylori.csv", header=TRUE)
mammalPDist<-read.csv2("./outputs/mammals_vertlife_distance-based_phylori.csv", header=TRUE)
mammalPtree<-read.csv2("./outputs/mammals_vertlife_tree-based_phylori.csv", header=TRUE)

costs_d <- read.csv2("./outputs/damagecost_by_species.csv", header = TRUE)
costs_m <- read.csv2("./outputs/managementcost_by_species.csv", header = TRUE)

iucnsp<-read.csv("./outputs/IUCN_exotic_mammal_bird.csv") 
gavia<-read.csv("./outputs/GAVIA_exotic_bird.csv") 
gisd<-read.csv2("./outputs/gisd_exotic_mammal_bird.csv") 

######################################
############# Format data ############
######################################

colnames(iucn)[3] <- "Species"

colnames(birdFDist) <- c("Species", "dietoriFdist", "activityoriFdist", "massoriFdist", "meanoriFdist")
colnames(birdFtree) <- c("Species","dietoriFtree", "activityoriFtree", "massoriFtree", "meanoriFtree")
colnames(mammalFDist) <- c("Species","dietoriFdist", "activityoriFdist", "massoriFdist", "meanoriFdist")
colnames(mammalFtree) <- c("Species","dietoriFtree", "activityoriFtree", "massoriFtree", "meanoriFtree")

colnames(birdPDist)[1]<- "Species"
birdPDist$Species <-str_replace_all(birdPDist$Species, "_", " ")

colnames(birdPtree)[1]<- "Species"
birdPtree$Species <-str_replace_all(birdPtree$Species, "_", " ")

colnames(mammalPDist)[1]<- "Species"
mammalPDist$Species <-str_replace_all(mammalPDist$Species, "_", " ")

colnames(mammalPtree)[1]<- "Species"
mammalPtree$Species <-str_replace_all(mammalPtree$Species, "_", " ")

######################################
############# Build all database #####
######################################


table(iucn$className)
iucnM<-filter(iucn, className == "MAMMALIA")
iucnB<-filter(iucn, className == "AVES")

### Merge originality value and IUCN data 

iucnBOri<-left_join(iucnB,birdFDist,by="Species")
iucnBOri<-left_join(iucnBOri,birdFtree,by="Species")
iucnBOri<-left_join(iucnBOri,birdPDist,by="Species")
iucnBOri<-left_join(iucnBOri,birdPtree,by="Species")

head(mammalPDist)

iucnMOri<-left_join(iucnM,mammalFDist,by="Species")
iucnMOri<-left_join(iucnMOri,mammalFtree,by="Species")
iucnMOri<-left_join(iucnMOri,mammalPDist,by="Species")
iucnMOri<-left_join(iucnMOri,mammalPtree,by="Species")

iucnMBOri <- rbind(iucnMOri,iucnBOri)


## Occurences in invacost (simpliest way, we can think about other ways)

invacost$frequence <- 1

freq_cost<-invacost %>%
  group_by(Species) %>%
  summarise(freq_cost = sum(frequence))

head(as.data.frame(freq_cost))

## Number of publications by species in invacost

species_pub <- unique(invacost[, c("Species", "Reference_ID")])
head(species_pub)
species_pub$frequence <- 1

freq_publi<-species_pub %>%
  group_by(Species) %>%
  summarise(freq_publi = sum(frequence))

head(as.data.frame(freq_publi))

## Add frequency values in invacost database

invacostF<-inner_join(invacost,freq_cost)
invacostF <- inner_join(invacostF, freq_publi)

## Add Pres/Abs in invacost in IUCN database

invacostF$invacostY <- "Y"

dataAll<-iucnMBOri %>% left_join(invacostF,by="Species")
colnames(dataAll)

invacostIUCN<-(dataAll[which(dataAll$invacostY == "Y"),]) #849 >> 963?
invacostIUCNsp<-as.data.frame(unique(invacostIUCN$Species)) # 62 species 

colnames(dataAll)

dataAllF<-dataAll[,c(3:10,15,20:27,29,32,96:98)] # Select only variables that we need to conduct the analyses
colnames(dataAllF)
colnames(dataAllF)[c(18, 19)] <-  c("oriPdist", "oriPtree") # rename columns corresponding to phylogenetic originality
dataAllF <- unique(dataAllF) # to remove duplicates due to the fact that some species can have several costs in invacost

## Add cost values in the final database

costs_d <- costs_d[, c("Species", "Average.annual.cost")] # only keep species and cost values
colnames(costs_d)[2] <- "Average.annual.cost_damage"
costs_m <- costs_m[, c("Species", "Average.annual.cost")]
colnames(costs_m)[2] <- "Average.annual.cost_management"

dataAllF <- dataAllF %>% left_join(costs_d, by = "Species")
dataAllF <- dataAllF %>% left_join(costs_m, by = "Species")

nrow(dataAllF[which(!is.na(dataAllF$Average.annual.cost_damage)) ,]) # 33 species for which we have damage costs
nrow(dataAllF[which(!is.na(dataAllF$Average.annual.cost_management)) ,]) # 41 species for which we have damage costs

##Add exotic status in the final database 

# Filter GAVIA - exotic bird database #527 species
#Remove "unsuccessfull" and "unknow" categories
newgavia <- gavia[which(gavia$StatusCat!='Unsuccessful' & gavia$StatusCat!='Unknown'),] #527 species
##only keep one repetition per species
head(newgavia)
gaviasp<-unique(newgavia$Binomial)

###Reunite all databasis
Allbasis<-c(levels(iucnsp$scientificName),levels(gisd$Species), levels(factor(gaviasp))) #all exotic species found in all basis
Exotic<-dataAllF$Species%in% Allbasis #which species are in the global database
dataAllF<-add_column(dataAllF, Exotic, .after = "oriPtree") #add a column with TRUE = exotic species and FALSE = not an exotic species
#Verify the number of exotic species in the global database
trueexo<-subset(dataAllF, Exotic==TRUE) # 435 exotiques 
Allexoticsp<-unique(trueexo[c("Species", "className")])  # 434 exotiques 
summary(Allexoticsp$className)



## save the database
write.table(dataAllF,"./outputs/dataAllF.txt")
write.csv2(dataAllF,"./outputs/dataAllF.csv")
