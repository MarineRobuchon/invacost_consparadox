
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
df<-read.table("./data/traits.csv", sep=";", header=T, row.names=1 ) #  Careful of the path!
df

planttraits <- df
dim(planttraits)

planttraits <- na.omit(planttraits) # remove empty lines : this goes from 262445 species to 134032 by removing rows containing at least one NA. Necessary?
rownames(planttraits) <- planttraits$species
summary(planttraits)
planttraits <- planttraits[, c("X1.1.1","X1.2.1", "X1.3.1", "X1.4.1", "X1.5.1")] # only keep trait values 




# trait selection
# plant_traits <- planttraits[, c("X1.1.1","X1.2.1", "X1.3.1", "X1.4.1", "X1.5.1")] # not necessary
# trait transformation : already done (which one?)
# trait correlation
corPlot(planttraits, method = "pearson") # no pair of traits show correlation > 0.7 so we keep them all
# tree-based originality
class(planttraits)
# the variables are either quantitative or nominal, but not fuzzy!
# try with quantitative
dplant_traits <- dist.ktab(ktab.list.df(list(planttraits)), type = "Q") # calculation with Gower's general coefficient of distance

