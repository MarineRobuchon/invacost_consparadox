#############################################################################################
# script to calculate originality scores for birds and birds
#############################################################################################

### load packages ----
library(adiv)
library(StatMatch)
library(ape)
library(phylobase)
library(cluster)
library(dplyr)
library(Rarity)
library(kader)
library(ade4)

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
mamtraits <- read.csv(paste0(getwd(),"/data/MamFuncDat.txt"), sep = "\t")
mamtraits <- na.omit(mamtraits) # remove empty rows
rownames(mamtraits) <- mamtraits$Scientific
mamtraits <- mamtraits[, c("Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", 
                           "Diet.Vunk", "Diet.Scav", "Diet.Fruit", "Diet.Nect", 
                           "Diet.Seed", "Diet.PlantO",  "ForStrat.Value", "Activity.Nocturnal", 
                           "Activity.Crepuscular", "Activity.Diurnal", "BodyMass.Value")] # only keep trait values 

## examination of diet traits (10) and calculation of diet originality
# trait selection
mamdiet <- mamtraits[, c("Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", 
                         "Diet.Vunk", "Diet.Scav", "Diet.Fruit", "Diet.Nect", 
                         "Diet.Seed", "Diet.PlantO")]
mamdiet[rowSums(mamdiet) != 100 ,] # check if all species have diet info
mamdiet["Myzopoda aurita", "Diet.Inv"] <- 100 # add missing data for the 3 species from Phylacine trait data
mamdiet["Mystacina robusta", "Diet.PlantO"] <- 80
mamdiet["Mystacina robusta", "Diet.Inv"] <- 20
mamdiet["Mystacina tuberculata", "Diet.PlantO"] <- 11
mamdiet["Mystacina tuberculata", "Diet.Inv"] <- 89

# trait transformation : none as it is percentage (but may be recoded in categorical variables)
# trait correlation
corPlot(mamdiet, method = "pearson") # no pair of traits show correlation > 0.7 so we keep them all
# tree-based diet originality
ddiet_mam <- dist.ktab(ktab.list.df(list(prep.fuzzy(mamdiet, 10))), type = "F") # calculation with Gower's general coefficient of distance
# ddiet_mam <- dsimFun(df = mamdiet, vartype = "P", method = 2, type = "dissimilarity") # calculation with the Jaccard coefficient
mamdiettree <- hclust(as.dist(ddiet_mam), method = "average")
mamdiettree <- as.phylo(mamdiettree)
mam_tb_dietori <- distinctTree(mamdiettree)
write.csv2(mam_tb_dietori, paste0(getwd(), "/outputs/mammals_vertlife_tree-based_dietori.csv"))
# distance-based diet originality
mam_db_dietori <- distinctDis(as.dist(ddiet_mam))
write.csv2(mam_db_dietori, paste0(getwd(), "/outputs/mammals_vertlife_distance-based_dietori.csv")) # save results

## examination of activity and foraging traits representing spatio-temporal activity (4)
mamactivity <- mamtraits[, c("Activity.Nocturnal", "Activity.Crepuscular", "Activity.Diurnal", "ForStrat.Value")]
# trait transformation : none
# trait correlation : not investigated as traits are binary or categorical
# tree-based activity originality
mamactivity_temporal <- prep.binary(mamactivity[, 1:3], 3)
mamactivity_spatial <- data.frame(mamtraits[, c("ForStrat.Value")])
rownames(mamactivity_spatial) <- rownames(mamtraits)
dactivity_mam <- dist.ktab(ktab.list.df(list(mamactivity_temporal, mamactivity_spatial)), type = c("B", "N")) # calculation with Gower's general coefficient of distance
mamactivitytree <- hclust(as.dist(dactivity_mam), method = "average")
mamactivitytree <- as.phylo(mamactivitytree)
mam_tb_activityori <- distinctTree(mamactivitytree)
write.csv2(mam_tb_activityori, paste0(getwd(), "/outputs/mammals_vertlife_tree-based_activityori.csv"))
# distance-based activity originality
mam_db_activityori <- distinctDis(as.dist(dactivity_mam))
write.csv2(mam_db_activityori, paste0(getwd(), "/outputs/mammals_vertlife_distance-based_activityori.csv")) # save results

## examination of body mass trait (1)
mambodymass <- data.frame(mamtraits[, c("BodyMass.Value")])
rownames(mambodymass) <- rownames(mamtraits)
# trait transformation: cube root
mambodymass <- kader:::cuberoot(mambodymass)
# trait correlation : none (one trait only)
# tree-based body mass originality
dmass_mam <- dist.ktab(ktab.list.df(list(mambodymass)), type = "Q")
mammasstree <- hclust(as.dist(dmass_mam), method = "average")
mammasstree <- as.phylo(mammasstree)
mam_tb_massori <- distinctTree(mammasstree)
write.csv2(mam_tb_massori, paste0(getwd(), "/outputs/mammals_vertlife_tree-based_massori.csv"))
# distance-based body mass originality
mam_db_massori <- distinctDis(as.dist(dmass_mam))
write.csv2(mam_db_massori, paste0(getwd(), "/outputs/mammals_vertlife_distance-based_massori.csv")) # save results

## calculation of a global score of functional originality
## by taking the mean over diet originality, activity originality and mass originality
# tree-based functional originality with ED
# mam_tb_dietori <- read.csv2(paste0(getwd(), "/outputs/mammals_vertlife_tree-based_dietori.csv"))
# mam_tb_activityori <- read.csv2(paste0(getwd(), "/outputs/mammals_vertlife_tree-based_activityori.csv"))
# mam_tb_massori <- read.csv2(paste0(getwd(), "/outputs/mammals_vertlife_tree-based_massori.csv"))
mam_tb_funcori <- data.frame(dietori = mam_tb_dietori$ED, activityori = mam_tb_activityori$ED, massori = mam_tb_massori$ED)
rownames(mam_tb_funcori) <- rownames(mamtraits)
mam_tb_funcori$meanori <- rowMeans(mam_tb_funcori)
write.csv2(mam_tb_funcori, paste0(getwd(), "/outputs/mammals_vertlife_tree-based_funcori.csv")) # save results
# distance-based functional originality with AV
# mam_db_dietori <- read.csv2(paste0(getwd(), "/outputs/mammals_vertlife_distance-based_dietori.csv"))
# mam_db_activityori <- read.csv2(paste0(getwd(), "/outputs/mammals_vertlife_distance-based_activityori.csv"))
# mam_db_massori <- read.csv2(paste0(getwd(), "/outputs/mammals_vertlife_distance-based_massori.csv"))
mam_db_funcori <- data.frame(dietori = mam_db_dietori$AV, activityori = mam_db_activityori$AV, massori = mam_db_massori$AV)
rownames(mam_db_funcori) <- rownames(mamtraits)
mam_db_funcori$meanori <- rowMeans(mam_db_funcori)
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



## examination of diet traits (10, we remove "Diet.5Cat" which is a summary of the other diet traits) and calculation of diet originality
# trait selection
birddiet <- birdtraits[, c("Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", 
                      "Diet.Vunk", "Diet.Scav", "Diet.Fruit", "Diet.Nect", 
                      "Diet.Seed", "Diet.PlantO")]
birddiet[rowSums(birddiet) != 100 ,] # check if all species have diet info >> good

# trait transformation : none as it is percentage (but may be recoded in categorical variables)
# trait correlation
corPlot(birddiet, method = "pearson") # no pair of traits show correlation > 0.7 so we keep them all
# tree-based diet originality
ddiet_bird <- dist.ktab(ktab.list.df(list(prep.fuzzy(birddiet, 10))), type = "F") # calculation with Gower's general coefficient of distance
# ddiet_bird <- dsimFun(df = birddiet, vartype = "P", method = 2, type = "dissimilarity") # calculation with the Jaccard coefficient
birddiettree <- hclust(as.dist(ddiet_bird), method = "average")
birddiettree <- as.phylo(birddiettree)
bird_tb_dietori <- distinctTree(birddiettree)
write.csv2(bird_tb_dietori, paste0(getwd(), "/outputs/birds_vertlife_tree-based_dietori.csv"))
# distance-based diet originality
bird_db_dietori <- distinctDis(as.dist(ddiet_bird))
write.csv2(bird_db_dietori, paste0(getwd(), "/outputs/birds_vertlife_distance-based_dietori.csv")) # save results


## examination of activity and foraging traits representing spatio-temporal activity (9)
birdactivity <- birdtraits[, c("ForStrat.watbelowsurf", "ForStrat.wataroundsurf", "ForStrat.ground", 
                               "ForStrat.understory", "ForStrat.midhigh", "ForStrat.canopy", 
                               "ForStrat.aerial", "PelagicSpecialist", "Nocturnal")]
# trait transformation : none
# trait correlation : not investigated as traits are binary or percentage
# tree-based activity originality
# birdactivity_binary <- prep.binary(birdactivity[, c("PelagicSpecialist", "Nocturnal")], 2) 
# not sure this is right, because it is not the one or the other...plus,it generates NA
# the alternative is to consider these traits as categorical (0 is one category and 1 the other category)
birdactivity_binary <- birdactivity[, c("PelagicSpecialist", "Nocturnal")]
birdactivity_binary$PelagicSpecialist <- as.factor(birdactivity_binary$PelagicSpecialist)
birdactivity_binary$Nocturnal <- as.factor(birdactivity_binary$Nocturnal)
birdactivity_fuzzy <- prep.fuzzy(data.frame(birdtraits[, c("ForStrat.watbelowsurf", "ForStrat.wataroundsurf", "ForStrat.ground", 
                                                  "ForStrat.understory", "ForStrat.midhigh", "ForStrat.canopy", 
                                                  "ForStrat.aerial")]), 7)
dactivity_bird <- dist.ktab(ktab.list.df(list(birdactivity_binary, birdactivity_fuzzy)), type = c("N", "F")) # calculation with Gower's general coefficient of distance
birdactivitytree <- hclust(as.dist(dactivity_bird), method = "average")
birdactivitytree <- as.phylo(birdactivitytree)
bird_tb_activityori <- distinctTree(birdactivitytree)
write.csv2(bird_tb_activityori, paste0(getwd(), "/outputs/birds_vertlife_tree-based_activityori.csv"))

# distance-based activity originality
bird_db_activityori <- distinctDis(as.dist(dactivity_bird))
write.csv2(bird_db_activityori, paste0(getwd(), "/outputs/birds_vertlife_distance-based_activityori.csv")) # save results

## examination of body mass trait (1)
birdbodymass <- data.frame(birdtraits[, c("BodyMass.Value")])
rownames(birdbodymass) <- rownames(birdtraits)
# trait transformation: cube root
birdbodymass <- kader:::cuberoot(birdbodymass)
# trait correlation : none (one trait only)
# tree-based body mass originality
dmass_bird <- dist.ktab(ktab.list.df(list(birdbodymass)), type = "Q")
birdmasstree <- hclust(as.dist(dmass_bird), method = "average")
birdmasstree <- as.phylo(birdmasstree)
bird_tb_massori <- distinctTree(birdmasstree)
write.csv2(bird_tb_massori, paste0(getwd(), "/outputs/birds_vertlife_tree-based_massori.csv"))
# distance-based body mass originality
bird_db_massori <- distinctDis(as.dist(dmass_bird))
write.csv2(bird_db_massori, paste0(getwd(), "/outputs/birds_vertlife_distance-based_massori.csv")) # save results

## calculation of a global score of functional originality
## by taking the mean over diet originality, activity originality and mass originality
# tree-based functional originality with ED
# bird_tb_dietori <- read.csv2(paste0(getwd(), "/outputs/birds_vertlife_tree-based_dietori.csv"))
# bird_tb_activityori <- read.csv2(paste0(getwd(), "/outputs/birds_vertlife_tree-based_activityori.csv"))
# bird_tb_massori <- read.csv2(paste0(getwd(), "/outputs/birds_vertlife_tree-based_massori.csv"))
bird_tb_funcori <- data.frame(dietori = bird_tb_dietori$ED, activityori = bird_tb_activityori$ED, massori = bird_tb_massori$ED)
rownames(bird_tb_funcori) <- rownames(birdtraits)
bird_tb_funcori$meanori <- rowMeans(bird_tb_funcori)
write.csv2(bird_tb_funcori, paste0(getwd(), "/outputs/birds_vertlife_tree-based_funcori.csv")) # save results
# distance-based functional originality with AV
# bird_db_dietori <- read.csv2(paste0(getwd(), "/outputs/birds_vertlife_distance-based_dietori.csv"))
# bird_db_activityori <- read.csv2(paste0(getwd(), "/outputs/birds_vertlife_distance-based_activityori.csv"))
# bird_db_massori <- read.csv2(paste0(getwd(), "/outputs/birds_vertlife_distance-based_massori.csv"))
bird_db_funcori <- data.frame(dietori = bird_db_dietori$AV, activityori = bird_db_activityori$AV, massori = bird_db_massori$AV)
rownames(bird_db_funcori) <- rownames(birdtraits)
bird_db_funcori$meanori <- rowMeans(bird_db_funcori)
write.csv2(bird_db_funcori, paste0(getwd(), "/outputs/birds_vertlife_distance-based_funcori.csv")) # save results




