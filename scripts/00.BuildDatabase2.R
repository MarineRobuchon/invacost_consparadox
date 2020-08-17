#############################################################################################
# script to build the database by merging IUCN status, originality scores and INVACOST info
# original script by Céline Bellard
# modifications by: Marine Robuchon, XX, 
#############################################################################################

rm(list=ls())
library(dplyr)
library(stringr)
library(invacost)
data(invacost)

install.packages("D:/Collaboration/Invacost workshop/Phylogenie Marine/invacost_0.3-4.tar.gz", repos = NULL, type = "source")
setwd("D:/Collaboration/Invacost workshop/Phylogenie Marine/") # to personalise if needed

######################################
############# Load data ##############
######################################

iucn<-read.delim("./data/IUCNdata.txt", header=TRUE)
#invacost<-read.delim("./data/Invacostdata27072020.txt", header=TRUE)

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
invacostIUCNsp<-as.data.frame(unique(invacostIUCN$Species)) # 64 species 

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
# 1              50               1               3


colnames(dataAllF)
head(dataAllF)
dataAllF$freq_cost

dataAllFInvacost<-filter(dataAllF, frequence ==1) #105 lignes

# Relation between originality and threat status of exotic species
# in INVACOST and their frequency of estimation

ggplot(dataAllFInvacost, aes(x=meanoriFdist, y=freq_cost)) +
  geom_point()+  facet_grid(className ~ .) + geom_smooth(method="loess")

dataAllFInvacost[which(is.na(dataAllFInvacost$className)),]






# First we create new columns in character format to avoid factor errors in R

invacost$sp.list <- as.character(invacost$Species)
invacost$genus.list <- as.character(invacost$Genus)

# Second, we merge Rattus and Mus together in a single group in these columns
# Species column
invacost$sp.list[which(invacost$Genus == "Rattus" | invacost$Genus == "Mus")] <- "Rattus spp./Mus spp."
invacost$sp.list[which(invacost$Species %in% c("Rattus sp./Mus sp.", 
                                               "Mus musculus/Rattus rattus",
                                               "Mus musculus/Rattus norvegicus",
                                               "Mus sp./Rattus sp."))] <- "Rattus spp./Mus spp."
# Genus column
invacost$genus.list[which(invacost$sp.list == "Rattus spp./Mus spp.")] <- "Rattus/Mus"

invacost$sp.list[which(invacost$Genus == "Aedes")] <- "Aedes spp."
invacost$sp.list[which(invacost$Genus == "Felis/Rattus")] <- "Felis catus/Rattus spp."
invacost$sp.list[which(invacost$Genus == "Oryctolagus/Rattus")] <- "Oryctolagus spp./Rattus spp." 
invacost$sp.list[which(invacost$Genus == "Canis")] <- "Canis lupus spp."


# Unique identifier
invacost$unique.sp.id <- do.call("paste", invacost[, c("Kingdom", "Phylum", "Class", "Family", "genus.list", "sp.list")])
# First we expand the database
db.over.time <- expandYearlyCosts(invacost,
                                  startcolumn = "Probable_starting_year_low_margin",
                                  endcolumn = "Probable_ending_year_low_margin")


# Then we prepare a data.frame in which we will store our results
species.summary <- data.frame()
# We will cycle the loop through all unique identifiers
for(sp in unique(db.over.time$unique.sp.id))
{
  # We subset the database for our current species
  cur.db <- db.over.time[which(db.over.time$unique.sp.id %in% sp), ]
  
  # We apply the raw cost function
  cur.raw <- calculateRawAvgCosts(cur.db, minimum.year = 1970)
  
  
  # And from the cur.raw object we extract the specific information we are looking for
  species.summary <- rbind.data.frame(species.summary,
                                      data.frame(
                                        Kingdom = cur.db$Kingdom[1],
                                        Phylum = cur.db$Phylum[1],
                                        Class = cur.db$Class[1],
                                        Family = cur.db$Family[1],
                                        Genus = cur.db$Genus[1],
                                        Species = cur.db$sp.list[1],
                                        Average.annual.cost = cur.raw$average.total.cost$annual_cost,
                                        Cumulated.cost = cur.raw$average.total.cost$total_cost,
                                        Number.estimates = cur.raw$average.total.cost$number_estimates,
                                        Number.year.values = cur.raw$average.total.cost$number_year_values
                                      ))
}

# To make the summary dataframe nicer, we can sort by cost to have the highest groups first
species.summary <- species.summary[order(species.summary$Cumulated.cost, decreasing = TRUE), ]


# Have a look at the first groups
species.summary[1:10, ]

unique(species.summary$Class)

species.summaryMammals<-filter(species.summary, Class== "Mammalia" ) ### 10 dernier NA
species.summaryMammals[c(90:100),]
species.summaryBirds<-filter(species.summary, Class== "Aves" ) # <32


colnames(dataAllF)
colnames(species.summary)

species.summary<-species.summary[,c(6:10)]
dataAllFInvacost<-inner_join(dataAllF,species.summary)

head(dataAllF)
colnames(dataAllFInvacost)
str(dataAllFInvacost)

hist(dataAllFInvacost$Average.annual.cost)
hist(dataAllFInvacost$Cumulated.cost)
summary(dataAllFInvacost$Average.annual.cost)
summary(dataAllFInvacost$Cumulated.cost)


ggplot(dataAllFInvacost, aes(x=Number.estimates, y=Average.annual.cost)) +
  geom_point()+  facet_grid(className ~ .) + geom_smooth() + ylim(0,2)
  
hist(dataAllFInvacost$Average.annual.cost)
head(dataAllFInvacost$Average.annual.cost)

ggplot(dataAllFInvacost, aes(x=meanoriFtree, y=Cumulated.cost)) +
  geom_point()+  facet_grid(className ~ .) + geom_smooth()+ ylim(0,60)

