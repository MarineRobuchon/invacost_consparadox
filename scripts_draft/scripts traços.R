
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

## load phylogenetic trees 
plant_tree <- read.tree ("./data/ALLMB.tre") # Careful of the path! ALLMB from Smith and Brown (2018), only one tree
str(plant_tree)
plant_tree
## tree-based phylogenetic originality
#plant_tb_phylori <- distinctTree(plant_tree, method = "ED")
#write.csv2(plant_tb_phylori,"plants_tree-based_phylori.csv") # save results

## load trait data
df<-read.table("./data/traits_to_run.csv", sep=";", header=T, row.names=1 ) #  Careful of the path!
df

planttraits <- df
dim(planttraits)

planttraits <- na.omit(planttraits) # remove empty lines : this goes from 262445 species to 110403 by removing rows containing at least one NA. Necessary?
rownames(planttraits) <- planttraits$species
summary(planttraits)

# trait selection
planttraits <- planttraits[, 1:6] # only keep trait values 
# tree-based originality
class(planttraits)
# the variables are either quantitative (if transformed) or nominal, but not fuzzy!
# try with nominal
dplant_traits <- dist.ktab(ktab.list.df(list(planttraits)), type = "N") # calculation with Gower's general coefficient of distance

