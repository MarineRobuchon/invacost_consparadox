#############################################################################################
# script to calculate originality scores for plants 
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
plant_tree <- read.tree ("./data/ALLMB.tre") # ALLMB from Smith and Brown (2018), only one tree
str(plant_tree)

## tree-based phylogenetic originality
plant_tb_phylori <- distinctTree(plant_tree, method = "ED")
write.csv2(plant_tb_phylori, "./outputs/plants_tree-based_phylori.csv") # save results

## distance-based phylogenetic originality
plant_db_phylori <- distinctDis(plant_tree, method = "AV")
write.csv2(plant_db_phylori.med, "./outputs/plants_distance-based_phylori.csv") # save results



