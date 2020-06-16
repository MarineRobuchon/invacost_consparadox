#############################################################################################
# script to explore the taxa and the available phylogenies for the species in Invacost
#############################################################################################

### load packages/functions ----
library(ggplot2)
library(ape)
library(phytools)
library(phylobase)
n_fun <- function(x){
  return(data.frame(y = min(x)-1, label = paste0("n = ",length(x))))
}

### load data ----
taxo_invacost <- read.csv2(paste0(getwd(), "/data/taxo_invacost.csv"))


### explore data to see which classes are the most represented
levels(taxo_invacost$Class) # 35 classes dont 7 sont des mix/diverses
taxo_invacost$Presence <- 1
class_freq <- aggregate(taxo_invacost$Presence, by = list(taxo_invacost$Class), FUN = sum)
colnames(class_freq) <- c("Class", "Class_freq")

taxo_invacost <- merge(taxo_invacost, class_freq, by = "Class")
taxo_invacost <- taxo_invacost[order(taxo_invacost$Class_freq),]
levels(factor(taxo_invacost$Class)) # niveaux ordre alphabétique
unique(factor(taxo_invacost$Class)) # niveaux dans l'ordre voulu
taxo_invacost$Class <- factor(taxo_invacost$Class, levels = unique(factor(taxo_invacost$Class)))

plot_taxo <- ggplot(taxo_invacost, aes(Class)) + geom_bar() + coord_flip() +
  ggtitle("Number of species present in Invacost by class")
  
plot_taxo
ggsave(filename = paste0(getwd(), "/outputs/plot_taxo.png"), plot = plot_taxo,
       units = "cm", width = 10, height = 6, dpi = 300, scale = 2)

# les 4 classes pour lesquels il y a le plus de données sont 
# 1) Insecta (n = 128)
# 2) Mammalia (n = 90)
# 3) Magnoliopsida (= Dicotylédones, n = 41)
# 4) Aves (n = 21)

## pour les insectes, exploration par ordre
insecta_invacost <- taxo_invacost[taxo_invacost$Class=="Insecta" ,]
order_freq <- aggregate(insecta_invacost$Presence, by = list(insecta_invacost$Order), FUN = sum)
colnames(order_freq) <- c("Order", "Order_freq")
insecta_invacost <- merge(insecta_invacost, order_freq, by = "Order")
insecta_invacost <- insecta_invacost[order(insecta_invacost$Order_freq),]
insecta_invacost$Order <- factor(insecta_invacost$Order, levels = unique(factor(insecta_invacost$Order)))

ggplot(insecta_invacost, aes(Order)) + geom_bar() + coord_flip()
# les 4 ordres pour lesquels il y a le plus de données sont 
# 1) Lepidoptera (n = 29)
# 2) Coleoptera(n = 27)
# 3) Diptera (n = 25)
# 4) Hemiptera (n = 20)  
# 5) Hymenoptera (n = 11)
  

## pour les magnoliopsida, exploration par ordre
magnoliopsida_invacost <- taxo_invacost[taxo_invacost$Class=="Magnoliopsida" ,]
order_freq <- aggregate(magnoliopsida_invacost$Presence, by = list(magnoliopsida_invacost$Order), FUN = sum)
colnames(order_freq) <- c("Order", "Order_freq")
magnoliopsida_invacost <- merge(magnoliopsida_invacost, order_freq, by = "Order")
magnoliopsida_invacost <- magnoliopsida_invacost[order(magnoliopsida_invacost$Order_freq),]
magnoliopsida_invacost$Order <- factor(magnoliopsida_invacost$Order, levels = unique(factor(magnoliopsida_invacost$Order)))

ggplot(magnoliopsida_invacost, aes(Order)) + geom_bar() + coord_flip()
# l'ordre le plus représenté est celui des Fabales
  
### look/load phylogenies for these Classes ----
## 1) Insecta
# a time-calibrated phylogeny at the family level (Rainford et al., 2014, PLoS ONE)
insecta_tree <- read.nexus(paste0(getwd(), "/data/Insecta_tree_Rainford.txt"))$Dated_Tree
str(insecta_tree)
plot(insecta_tree)
insecta_tree <- as(insecta_tree, "phylo4")
tipLabels(insecta_tree) # les 874 familles/super familles

# Espeland et al. (2018, PLoS One) published a time-calibrated phylogeny of Lepidoptera
# but it contains only 207 species (on more than 18 000 described species)

# Zhang et al. (2018, Nat Com) published a time-calibrated phylogeny of Coleoptera
# but it contains only 373 species (on more than 380 000 described extant species)

# Wiegmann et al. (2011, PNAS) published a time-calibrated phylogeny of Diptera
# but it is at the family level

# Li et al. (2018, PNAS) published a time-calibrated phylogeny of Diptera
# but it contains only 128 species (on approx. 100 000 species described)

# Peters et al. (2017, PNAS) published a time-calibrated phylogeny of Hymenoptera
# but it contains only 163 species (on approx. 153 000 species described)

## 2) Mammalia
# 1000 trees available in Phylacine
# a new phylogeny will be soon available (Upham et al., submitted)
mammalia_trees <- read.nexus(paste0(getwd(), "/data/Mammalia_tree_Phylacine.nex"))
# we can calculate ED metric from this tree 
# or use the median ED calculated in hedgeledge script (see below)
mammalia_scores <- read.csv2(paste0(getwd(), "/data/mammals03072019.csv"))
# see which species are in invacost and what thair ED scores are
mammalia_scores$invacost <- NA
mammalia_scores$invacost[mammalia_scores$species_currentIUCN%in%taxo_invacost$Species] <- "yes"
mammalia_scores$invacost[!mammalia_scores$species_currentIUCN%in%taxo_invacost$Species] <- "no"
mammalia_scores$ED_rank <- rank(-mammalia_scores$ED, ties.method = "average")

plot_ED_mam <- ggplot(mammalia_scores, aes(invacost, log(ED))) +  geom_boxplot() + 
  ggtitle("Distribution of ED scores for mammal species\npresent in Invacost (yes) or not (no)") +
  stat_summary(fun.data = n_fun, geom = "text")
plot_ED_mam
ggsave(filename = paste0(getwd(), "/outputs/plot_ED_mam.png"), plot = plot_ED_mam,
       units = "cm", width = 6, height = 8, dpi = 300, scale = 2)

ggplot(mammalia_scores, aes(invacost, ED_rank)) +  geom_boxplot() + scale_y_reverse()
#species in invacost seem to have very slightly higher scores/ranks than species which are not in invacost

## 3) Magnolopsida
plant_tree <- read.tree(paste0(getwd(), "/data/Plant_tree_Harris.data"))
str(plant_tree)
plot(plant_tree)
plant_tree <- as(plant_tree, "phylo4")
tipLabels(plant_tree) # the 425 families of seed plants in the phylogeny

# pour les Fabales, il existe a priori 100 arbres simulés pour les 20 431 espèces estimées
# il faut demander aux auteurs si on peut avoir accès à ces 100 arbres
# >> 100 arbres simulés demandés
# autre arbre avec de vraies espèces existant : Azani et al. (2017)
# but not complete, ca. 20% (3696) of known species


## 4) Aves
# # ED scores can be calculated from the bird synthetic tree of Brown et al. (2017)
# bird_tree <- read.tree(paste0(getwd(), "/data/birds_tree_Brown2017.tre"))
# str(bird_tree)
# plot(bird_tree)
# bird_tree <- as(bird_tree, "phylo4")
# tipLabels(bird_tree) # careful, these are not species names
# # or from trees available on https://birdtree.org/ (Jetz et al 2014)
# # these are the first 1000 trees on the Ericson blackbone
# bird_trees_Jetz <- read.tree(paste0(getwd(), "/data/AllBirdsEricson1.tre"))
# bird_tree_Jetz <- bird_trees_Jetz [[1]]
# str(bird_tree_Jetz)
# plot(bird_tree_Jetz)
# bird_tree_Jetz <- as(bird_tree_Jetz, "phylo4")
# tipLabels(bird_tree_Jetz) # this time they are the good species names

# alternatively we can directly use the scores calculated in Jetz 2014
bird_scores <- read.csv2(paste0(getwd(), "/data/bird_scores_Jetz2014.csv"))
bird_scores$invacost <- NA
bird_scores$invacost[bird_scores$Species.Scientific%in%taxo_invacost$Species] <- "yes"
bird_scores$invacost[!bird_scores$Species.Scientific%in%taxo_invacost$Species] <- "no"



plot_ED_birds <- ggplot(bird_scores, aes(invacost, log(ED.Median))) +  geom_boxplot() +
ggtitle("Distribution of ED scores for bird species\npresent in Invacost (yes) or not (no)") +
  stat_summary(fun.data = n_fun, geom = "text")
plot_ED_birds
ggsave(filename = paste0(getwd(), "/outputs/plot_ED_birds.png"), plot = plot_ED_birds,
       units = "cm", width = 6, height = 8, dpi = 300, scale = 2)

ggplot(bird_scores, aes(invacost, ED.Rank.All)) +  geom_boxplot() + scale_y_reverse()
#species in invacost seem to have lower scores/ranks than species which are not in invacost



