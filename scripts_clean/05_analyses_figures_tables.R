#############################################################################################
# Script to run the analyses and make figures/tables of the paper
# original script by Marine Robuchon with contributions of Céline Bellard, Camille Bernery, 
# Vanessa Rezende, Gustavo Heringer & Cheikh Dia
#############################################################################################

### Load packages ----
library(plyr)
library(dplyr)
library(ggplot2)
library(DataCombine)
library(ggpubr)
library(cowplot)
library(forcats)

### Load and organise data ----
data_all <-  read.csv2(paste0(getwd(), "/outputs/final_db.csv"))[, -1]
data_all$redlistCategory[which(is.na(data_all$redlistCategory))] <- "not assessed"
data_all$redlistCategory <- factor(data_all$redlistCategory)

# transform Lower Risk/conservation dependent in lower risk, Lower Risk/least concern in least concern, and Lower Risk/near threatened in near threatened
levels(data_all$redlistCategory) <- c("critically endangered", "data deficient", "endangered", "extinct", "extinct in the wild", "least concern", 
                                      "lower risk", "least concern", "near threatened", "near threatened", "not assessed", "vulnerable")

# suppress extinct and extinct in the wild species
data_all <- data_all[-which(data_all$redlistCategory %in% c("extinct", "extinct in the wild")),]

# reorder levels of factors for redlistCategory
data_all$redlistCategory <- factor(data_all$redlistCategory, levels = c("least concern", "near threatened", "lower risk", 
                                                                        "vulnerable", "endangered", "critically endangered", 
                                                                        "data deficient", "not assessed"))

### Threat status and distinctiveness of the costliest invasive species in InvaCost ----
## identification of the costliest species by type of costs & taxon ----
# damage
damage <- data_all[-which(is.na(data_all$damage_cost)),]

mam_damage <- damage[which(damage$taxon=="MAMMALS"),]
nrow(mam_damage) # 20 mammal species have a damage cost
top5_mam_damage <- head(mam_damage[order(mam_damage$damage_cost, decreasing = TRUE),], 5)

bird_damage <- damage[which(damage$taxon=="BIRDS"),]
nrow(bird_damage) # 10 bird species have a damage cost
top5_bird_damage <- head(bird_damage[order(bird_damage$damage_cost, decreasing = TRUE),], 5)

plant_damage <- damage[which(damage$taxon=="PLANTS"),]
nrow(plant_damage)# 49 plant species have a damage cost
top5_plant_damage <- head(plant_damage[order(plant_damage$damage_cost, decreasing = TRUE),], 5)

# management
management <- data_all[-which(is.na(data_all$management_cost)),]

mam_management <- management[which(management$taxon=="MAMMALS"),]
nrow(mam_management) # 39 mammal species have a management cost
top5_mam_management <- head(mam_management[order(mam_management$management_cost, decreasing = TRUE),], 5)

bird_management <- management[which(management$taxon=="BIRDS"),]
nrow(bird_management) # 14 bird species have a management cost
top5_bird_management <- head(bird_management[order(bird_management$management_cost, decreasing = TRUE),], 5)

plant_management <- management[which(management$taxon=="PLANTS"),]
nrow(plant_management) # 273 plant species have a management cost
top5_plant_management <- head(plant_management[order(plant_management$management_cost, decreasing = TRUE),], 5)

## figure showing costs and threat status of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(species, damage_cost), y = damage_cost, fill = redlistCategory)) + 
  ggtitle("A  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) + 
  labs(fill = "Red List category", x = "DAMAGE\ntop 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
A_mammals

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(species, damage_cost), y = damage_cost, fill = redlistCategory)) + 
  ggtitle("B  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List category", x = "DAMAGE\ntop 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
C_birds

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(species, damage_cost), y = damage_cost, fill = redlistCategory)) + 
  ggtitle("C PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List category", x = "DAMAGE\ntop 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
E_plants

# management
B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(species, management_cost), y = management_cost, fill = redlistCategory)) + 
  #ggtitle("B  MAMMALS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List category", x = "MANAGEMENT\ntop 5 costliest species", y = "Average annual management cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
B_mammals

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(species, management_cost), y = management_cost, fill = redlistCategory)) + 
  #ggtitle("D  BIRDS") + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List category", x = "MANAGEMENT\ntop 5 costliest species", y = "Average annual management cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
D_birds

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(species, management_cost), y = management_cost, fill = redlistCategory)) + 
  #ggtitle("F PLANTS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) + 
  labs(fill = "Red List category", x = "MANAGEMENT\ntop 5 costliest species", y = "Average annual management cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
F_plants

# to be checked: removal of F catus, taxonomic ref for birds as P krameri is the name to be used while IUCN uses Alexandrinus krameri
