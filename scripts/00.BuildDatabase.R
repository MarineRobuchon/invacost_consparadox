#############################################################################################
# script to build the database by merging IUCN status, originality scores and INVACOST info
# original script by Céline Bellard
# modifications by: Marine Robuchon, XX, 
#############################################################################################

rm(list=ls())
library(dplyr)
library(stringr)

# setwd("D:/Collaboration/Invacost workshop/Phylogenie Marine/") # to personalise if needed

######################################
############# Load data ##############
######################################

iucn<-read.delim("./data/IUCNdata.txt", header=TRUE)
invacost<-read.delim("./data/Invacostdata27072020.txt", header=TRUE)

birdFDist<-read.csv2("./outputs/birds_vertlife_distance-based_funcori.csv", header=TRUE) # NB: I changed the path to outputs, because scores have been saved in outputs
birdFtree<-read.csv2("./outputs/birds_vertlife_tree-based_funcori.csv", header=TRUE)
mammalFDist<-read.csv2("./outputs/mammals_vertlife_distance-based_funcori.csv", header=TRUE)
mammalFtree<-read.csv2("./outputs/mammals_vertlife_tree-based_funcori.csv", header=TRUE)


birdPDist<-read.csv2("./outputs/birds_vertlife_distance-based_phylori.csv", header=TRUE)
birdPtree<-read.csv2("./outputs/birds_vertlife_tree-based_phylori.csv", header=TRUE)
mammalPDist<-read.csv2("./outputs/mammals_vertlife_distance-based_phylori.csv", header=TRUE)
mammalPtree<-read.csv2("./outputs/mammals_vertlife_tree-based_phylori.csv", header=TRUE)

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

iucnBOri<-full_join(iucnB,birdFDist,by="Species")
iucnBOri<-full_join(iucnBOri,birdFtree,by="Species")
iucnBOri<-full_join(iucnBOri,birdPDist,by="Species")
iucnBOri<-full_join(iucnBOri,birdPtree,by="Species")

head(mammalPDist)

iucnMOri<-full_join(iucnM,mammalFDist,by="Species")
iucnMOri<-full_join(iucnMOri,mammalFtree,by="Species")
iucnMOri<-full_join(iucnMOri,mammalPDist,by="Species")
iucnMOri<-full_join(iucnMOri,mammalPtree,by="Species")

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

dataAllF<-dataAll[,c(3:10,15,20:27,29,32,94:96)] # Select only variables that we need to conduct the analyses
colnames(dataAllF)
colnames(dataAllF)[c(18, 19)] <-  c("oriPdist", "oriPtree") # rename columns corresponding to phylogenetic originality
dataAllF <- unique(dataAllF) # to remove duplicates due to the fact that some species can have several costs in invacost

## save the database
write.table(dataAllF,"./outputs/dataAllF.txt")


###  test IUCN category among IAS invacost
table(invacostIUCN[!duplicated(invacostIUCN$Species,invacostIUCN$redlistCategory_version_2020.2),]$redlistCategory_version_2020.2)

# Endangered   Least Concern Near Threatened      Vulnerable 
# 1              49               1               2 
