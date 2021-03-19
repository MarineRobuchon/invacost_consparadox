#############################################################################################
# script to calculate originality scores for mammals and birds
#############################################################################################

### load packages ----
library(adiv)
library(StatMatch)
library(ape)
library(phylobase)
library(cluster)

### mammals with VertLife  ----

## load phylogenetic trees and taxonomic data
mam_trees <- list.files(paste0(getwd(), "/data/Completed_5911sp_topoCons_FBDasZhouEtAl"), full.names = TRUE)
mam_sub100trees <- mam_trees[seq(1, length(mam_trees), 100)] # select 100 trees
mam_tree_test <- read.tree(mam_sub100trees[1]) # the first tree out of the 100
mamtax <- read.csv(paste0(getwd(), "/data/taxonomy_mamPhy_5911species.csv")) # taxonomic info on mammal species in phylogeentic trees
mam2prune <- mam_tree_test$tip.label[!mam_tree_test$tip.label %in% levels(mamtax$Species_Name)] # list of species names that need to be removed from phylogenetic trees (outroups)

## tree-based phylogenetic originality: calculation for 100 trees to take into account phylogenetic uncertainty
## and then computation of the median score over the 100 trees
mam_tb_phylori <- list()
for (i in 1:length(mam_sub100trees))
{
  mam_tree <- drop.tip(read.tree(mam_sub100trees[i]), mam2prune)
  mam_tb_phylori[[i]] <- distinctTree(mam_tree)
} # start 26/05 16h30 end 27/05 14h

mam_tb_phylori.allscores <- do.call(cbind, mam_tb_phylori) 
mam.EDmed <- apply(X = mam_tb_phylori.allscores[colnames(mam_tb_phylori.allscores)=="ED"], MARGIN = 1, FUN = median)  
mam.ESmed <- apply(X = mam_tb_phylori.allscores[colnames(mam_tb_phylori.allscores)=="ES"], MARGIN = 1, FUN = median)
mam_tb_phylori.med <- data.frame(cbind(ED_median = mam.EDmed, ES_median = mam.ESmed))
write.csv2(mam_tb_phylori.med, paste0(getwd(), "/outputs/mammals_vertlife_tree-based_phylori.csv")) # save results

## distance-based phylogenetic originalityfor 100 trees to take into account phylogenetic uncertainty
## and then computation of the median score over the 100 trees
mam_db_phylori <- list()
for (i in 1:length(mam_sub100trees))
{
  mam_tree <- drop.tip(read.tree(mam_sub100trees[i]), mam2prune)
  d_mam_phylo <- dsimTree(mam_tree, type = "dissimilarity")
  mam_db_phylori[[i]] <- distinctDis(d_mam_phylo)
} # start 27/05 17h05 end 28/05 10h

mam_db_phylori.allscores <- do.call(cbind, mam_db_phylori) 
mam.Rbmed <- apply(X = mam_db_phylori.allscores[colnames(mam_db_phylori.allscores)=="Rb"], MARGIN = 1, FUN = median)  
mam.AVmed <- apply(X = mam_db_phylori.allscores[colnames(mam_db_phylori.allscores)=="AV"], MARGIN = 1, FUN = median)
mam.FVmed <- apply(X = mam_db_phylori.allscores[colnames(mam_db_phylori.allscores)=="FV"], MARGIN = 1, FUN = median)
mam.NNmed <- apply(X = mam_db_phylori.allscores[colnames(mam_db_phylori.allscores)=="NN"], MARGIN = 1, FUN = median)
mam_db_phylori.med <- data.frame(cbind(Rb_median = mam.Rbmed, AV_median = mam.AVmed, FV_median = mam.FVmed, NNmed = mam.NNmed))
write.csv2(mam_db_phylori.med, paste0(getwd(), "/outputs/mammals_vertlife_distance-based_phylori.csv")) # save results

## load trait data
setwd("D:/Documents/Projets/ImpactTPF/")
mamtraits<-read.delim("./data/mammals/Elton/MamFuncDat.txt",header=TRUE)
habitats<-read.csv2("./data/mammals/habitats.csv", sep=",")

dim(mamtraits)
#mamtraits <- read.csv(paste0(getwd(),"/data/MamFuncDat.txt"), sep = "\t")
# lets add a column to mamtraits with equivalents names in mamtax
# and suppress the species names with no equivalents in mamtax from mamtraits:
mamtraits$mamtraits_genus_species <- gsub(" ", "_", mamtraits$Scientific)
mamtraits <- merge(mamtraits, mamtax[c("Species_Name", "MSW3_sciName_matched")], 
                   by.x = "mamtraits_genus_species", by.y = "MSW3_sciName_matched")

mamtraits <- na.omit(mamtraits) # remove empty rows

rownames(mamtraits) <- mamtraits$Species_Name

colnames(mamtraits)

## Recode period of activity

activity <- mamtraits[,c(2,19,20,21)]
summary(activity)


activity2 <- NA
for (i in 1:nrow(activity)){
 print(i)
  if (activity$Activity.Crepuscular[i]==1 & activity$Activity.Diurnal[i]==0 & activity$Activity.Nocturnal[i]==0){
    activity2[i] <-  'Crepus'}
  if (activity$Activity.Crepuscular[i]==0 & activity$Activity.Diurnal[i]==1 & activity$Activity.Nocturnal[i]==0){
    activity2[i] <-  'Diur'}
  if (activity$Activity.Crepuscular[i]==0 & activity$Activity.Diurnal[i]==0 & activity$Activity.Nocturnal[i]==1){
    activity2[i] <-  'Noct'}
  if (activity$Activity.Crepuscular[i]==1 & activity$Activity.Diurnal[i]==1 & activity$Activity.Nocturnal[i]==0){
    activity2[i] <-  'CrepusDiur'}
  if (activity$Activity.Crepuscular[i]==1 & activity$Activity.Diurnal[i]==0 & activity$Activity.Nocturnal[i]==1){
    activity2[i] <-  'CrepusNoct'}
  if (activity$Activity.Crepuscular[i]==1 & activity$Activity.Diurnal[i]==1 & activity$Activity.Nocturnal[i]==1){
    activity2[i] <-  'CrepusDiurNoct'}
}
length(activity2)
mamtraits$PeriodActivity <- activity2

## Recode mass value

mamtraits$BodyMass.ValueLog <- log(mamtraits$BodyMass.Value)


#Recode dominant diet [ here you can choose another way to categorize if you'd prefer]

colnames(mamtraits)
diet <- mamtraits[,c(2,4:13)]

invert <-  as.data.frame(diet$Diet.Inv)
vert <- diet$Diet.Vend +diet$Diet.Vect+diet$Diet.Vfish+diet$Diet.Vunk+diet$Diet.Scav
plant <- diet$Diet.Fruit+diet$Diet.Nect+ diet$Diet.PlantO+diet$Diet.Seed

omniani<- invert+vert
colnames(diet)

diet[which(omniani == 50 & plant == 50),"diet2"] <- "omnigeneral"

diet[which(plant == 100),"diet2"] <- "vegetal_only"
diet[which(vert == 100),"diet2"] <- "vert_only"
diet[which(invert == 100),"diet2"] <- "invert_only"


list<- as.data.frame(which(omniani > 50)) # Invert + Vert
list2<- as.data.frame(which(is.na(diet$diet2))) # 2562 

colnames(list)<- "row"
colnames(list2)<- "row"

listomnian<-inner_join(list2,list)
diet[listomnian[,1],"diet2"] <- "main_animal"

list<- as.data.frame(which(plant > 50)) # Invert + Vert
list2<- as.data.frame(which(is.na(diet$diet2))) # 1380 

colnames(list)<- "row"
colnames(list2)<- "row"

listomnian<-inner_join(list2,list)

diet[listomnian[,1],"diet2"] <- "main_vegetal"

table(diet$diet2) # Tu peux aussi faire d'autres categories si tu le souhaites
mamtraits$diet2<- diet$diet2 


# Nb of habitats
#Habitats from IUCN
habitats$codenum <- as.numeric(as.character(habitats$code))

sum(is.na(habitats$codenum))
habitats$codenum <- trunc(habitats$codenum)
hab_to_comp <- habitats[,c("scientificName","codenum")]
hab_to_comp <- unique(hab_to_comp) #keep all different habitats per sp
hab_to_comp$count <- c(1)
nb_hab_per_sp <- aggregate(count~scientificName, data=hab_to_comp, FUN = sum)

#habitats[which(habitats$scientificName == "Abrocoma bennettii"),]# Verification
colnames(nb_hab_per_sp)<- c("Scientific","nbhabitats")
nb_hab_per_sp<- unique(nb_hab_per_sp[,c(1,2)])
head(nb_hab_per_sp)
head(mamtraits)

mamtraits<-inner_join(mamtraits,nb_hab_per_sp,by="Scientific") # 4860 vs. 5400 (lack data for some species)
colnames(mamtraits)
mamtraits2<- mamtraits[,c(28:31)]

head(mamtraits2)
str(mamtraits2)
mamtraits2$PeriodActivity<- as.factor(mamtraits2$PeriodActivity)
mamtraits2$diet2<- as.factor(mamtraits2$diet2)

## tree-based functional originality
# first calculate fucntional dissimilarity among species using the Gower's distance
dfun_mam_gow <- daisy(x = mamtraits2 , metric = "gower")
dfun_mam_euc <- daisy(x = mamtraits2 , metric = "euclidean")
dfun_mam_man <- daisy(x = mamtraits2 , metric = "manhattan")

# now make the functional tree
mamfuntree <- hclust(as.dist(dfun_mam), method = "average")
mamfuntree <- as.phylo(mamfuntree)
# finally caculate tree-based functional originality
# NB: maybe we should use several trees (eg calculated with method Ward and/or based on another distance metric) 
# to take into account functional uncertainty
mam_tb_funcori <- distinctTree(mamfuntree)

write.csv2(mam_tb_funcori, paste0(getwd(), "/outputs/mammals_vertlife_tree-based_funcori.csv")) # save results

## distance-based functional originality
# NB: maybe we should use several distances to take into account functional uncertainty
mam_db_funcori <- distinctDis(as.dist(dfun_mam))
write.csv2(mam_db_funcori, paste0(getwd(), "/outputs/mammals_vertlife_distance-based_funcori.csv")) # save results
  
  
 
### birds with VertLife  ----

## load phylogenetic trees
bird_trees <- read.tree(paste0(getwd(), "/data/AllBirdsEricson1.tre")) # the 1000 trees
bird_sub100trees <- bird_trees[seq(1, length(bird_trees), 10)] # select 100 trees

## tree-based and distance-based phylogenetic originality (calculated in the same loop to save calculation time)
## calculated for 100 trees to take into account phylogenetic uncertainty
## and then computation of the median score over the 100 trees
bird_tb_phylori <- list()
bird_db_phylori <- list()

for (i in 1:length(bird_sub100trees))
{
  bird_tree <- bird_sub100trees[[i]]
  bird_tb_phylori[[i]] <- distinctTree(bird_tree)
  d_bird_phylo <- dsimTree(bird_tree, type = "dissimilarity")
  bird_db_phylori[[i]] <- distinctDis(d_bird_phylo)
  
} # start 28/05 10h21 end 01/06/2020

bird_tb_phylori.allscores <- do.call(cbind, bird_tb_phylori) 
bird.EDmed <- apply(X = bird_tb_phylori.allscores[colnames(bird_tb_phylori.allscores)=="ED"], MARGIN = 1, FUN = median)  
bird.ESmed <- apply(X = bird_tb_phylori.allscores[colnames(bird_tb_phylori.allscores)=="ES"], MARGIN = 1, FUN = median)
bird_tb_phylori.med <- data.frame(cbind(ED_median = bird.EDmed, ES_median = bird.ESmed))
write.csv2(bird_tb_phylori.med, paste0(getwd(), "/outputs/birds_vertlife_tree-based_phylori.csv")) # save results

bird_db_phylori.allscores <- do.call(cbind, bird_db_phylori) 
bird.Rbmed <- apply(X = bird_db_phylori.allscores[colnames(bird_db_phylori.allscores)=="Rb"], MARGIN = 1, FUN = median)  
bird.AVmed <- apply(X = bird_db_phylori.allscores[colnames(bird_db_phylori.allscores)=="AV"], MARGIN = 1, FUN = median)
bird.FVmed <- apply(X = bird_db_phylori.allscores[colnames(bird_db_phylori.allscores)=="FV"], MARGIN = 1, FUN = median)
bird.NNmed <- apply(X = bird_db_phylori.allscores[colnames(bird_db_phylori.allscores)=="NN"], MARGIN = 1, FUN = median)
bird_db_phylori.med <- data.frame(cbind(Rb_median = bird.Rbmed, AV_median = bird.AVmed, FV_median = bird.FVmed, NNmed = bird.NNmed))
write.csv2(bird_db_phylori.med, paste0(getwd(), "/outputs/birds_vertlife_distance-based_phylori.csv")) # save results

## load trait data
birdtraits <- read.csv(paste0(getwd(), "/data/BirdFuncDat.txt"), sep = "\t")
birdtraits <- na.omit(birdtraits) # remove empty lines
rownames(birdtraits) <- birdtraits$Scientific
birdtraits <- birdtraits[, c("Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", 
                             "Diet.Vunk", "Diet.Scav", "Diet.Fruit", "Diet.Nect", 
                             "Diet.Seed", "Diet.PlantO", "Diet.5Cat",
                             "ForStrat.watbelowsurf", "ForStrat.wataroundsurf", "ForStrat.ground", 
                             "ForStrat.understory", "ForStrat.midhigh", "ForStrat.canopy", 
                             "ForStrat.aerial", "PelagicSpecialist", "Nocturnal", "BodyMass.Value")] # only keep trait values 

## tree-based functional originality
# first calculate functional dissimilarity among species using the Gower's distance
dfun_bird <- daisy(x = birdtraits , metric = "gower")
# now make the functional tree
birdfuntree <- hclust(as.dist(dfun_bird), method = "average")
birdfuntree <- as.phylo(birdfuntree)
# finally caculate tree-based functional originality
# NB: maybe we should use several trees (eg calculated with method Ward and/or based on another distance metric) 
# to take into account functional uncertainty
bird_tb_funcori <- distinctTree(birdfuntree)
write.csv2(bird_tb_funcori, paste0(getwd(), "/outputs/birds_vertlife_tree-based_funcori.csv")) # save results

## distance-based functional originality
bird_db_funcori <- distinctDis(as.dist(dfun_bird))
write.csv2(bird_db_funcori, paste0(getwd(), "/outputs/birds_vertlife_distance-based_funcori.csv")) # save results








