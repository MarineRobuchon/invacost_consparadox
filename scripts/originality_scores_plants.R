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

## load phylogenetic trees and taxonomic data
plant_trees <- read.tree ("ALLMB.tre") #ALLMB from Smith and Brown (2018)
planttax <- read.csv("spp_nova.csv",sep=";", header=T)

plant2prune <- plant_trees$tip.label[!plant_trees$tip.label %in% levels(planttax$species)] # list of species names that need to be removed from phylogenetic trees (outroups)

## tree-based phylogenetic originality
plant_tb_phylori <- list()
for (i in 1:length(plant_trees))
{
  plant_tree <- drop.tip(plant_trees, plant2prune)
  plant_tb_phylori[[i]] <- distinctTree(plant_tree)
} 

plant_tb_phylori.allscores <- do.call(cbind, plant_tb_phylori) 
plant.EDmed <- apply(X = plant_tb_phylori.allscores[colnames(plant_tb_phylori.allscores)=="ED"], MARGIN = 1, FUN = median)  
plant.ESmed <- apply(X = plant_tb_phylori.allscores[colnames(plant_tb_phylori.allscores)=="ES"], MARGIN = 1, FUN = median)
plant_tb_phylori.med <- data.frame(cbind(ED_median = plant.EDmed, ES_median = plant.ESmed))
write.csv2(plant_tb_phylori.med, "plants_tree-based_phylori.csv") # save results


## 
plant_db_phylori <- list()
for (i in 1:length(plant_trees))
{
  plant_tree <- drop.tip(plant_trees, plant2prune)
  d_plant_phylo <- dsimTree(plant_tree, type = "dissimilarity")
  plant_db_phylori[[i]] <- distinctDis(d_plant_phylo)
} 

plant_db_phylori.allscores <- do.call(cbind, plant_db_phylori) 
plant.Rbmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="Rb"], MARGIN = 1, FUN = median)  
plant.AVmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="AV"], MARGIN = 1, FUN = median)
plant.FVmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="FV"], MARGIN = 1, FUN = median)
plant.NNmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="NN"], MARGIN = 1, FUN = median)
plant_db_phylori.med <- data.frame(cbind(Rb_median = plant.Rbmed, AV_median = plant.AVmed, FV_median = plant.FVmed, NNmed = plant.NNmed))
write.csv2(plant_db_phylori.med, "plants_distance-based_phylori.csv") # save results
