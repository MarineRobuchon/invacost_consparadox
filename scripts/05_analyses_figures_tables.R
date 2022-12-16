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
levels(data_all$redlistCategory) <- c("critically endangered", "data deficient", "endangered", "least concern", 
                                      "lower risk", "least concern", "near threatened", "near threatened", "not assessed", "vulnerable")

# reorder levels of factors for redlistCategory
data_all$redlistCategory <- factor(data_all$redlistCategory, levels = c("least concern", "near threatened", "lower risk", 
                                                                        "vulnerable", "endangered", "critically endangered", 
                                                                        "data deficient", "not assessed"))

# add a second column that will contribute to show them on two line on the graph
data_all$species2lines <- gsub(" ", "\n", data_all$species)

# transform oriFtree in numeric
data_all$oriFtree <- as.numeric(data_all$oriFtree)

### Threat status and distinctiveness of the costliest invasive species in InvaCost ----
## identification of the costliest species by type of costs & taxon ----
# damage
damage <- data_all[-which(is.na(data_all$damage_cost)),]

mam_damage <- damage[which(damage$taxon=="MAMMALS"),]
nrow(mam_damage) # 19 mammal species have a damage cost
top5_mam_damage <- head(mam_damage[order(mam_damage$damage_cost, decreasing = TRUE),], 5)

bird_damage <- damage[which(damage$taxon=="BIRDS"),]
nrow(bird_damage) # 10 bird species have a damage cost
top5_bird_damage <- head(bird_damage[order(bird_damage$damage_cost, decreasing = TRUE),], 5)

plant_damage <- damage[which(damage$taxon=="PLANTS"),]
nrow(plant_damage)# 53 plant species have a damage cost
top5_plant_damage <- head(plant_damage[order(plant_damage$damage_cost, decreasing = TRUE),], 5)

# management
management <- data_all[-which(is.na(data_all$management_cost)),]

mam_management <- management[which(management$taxon=="MAMMALS"),]
nrow(mam_management) # 33 mammal species have a management cost
top5_mam_management <- head(mam_management[order(mam_management$management_cost, decreasing = TRUE),], 5)

bird_management <- management[which(management$taxon=="BIRDS"),]
nrow(bird_management) # 14 bird species have a management cost
top5_bird_management <- head(bird_management[order(bird_management$management_cost, decreasing = TRUE),], 5)

plant_management <- management[which(management$taxon=="PLANTS"),]
nrow(plant_management) # 287 plant species have a management cost
top5_plant_management <- head(plant_management[order(plant_management$management_cost, decreasing = TRUE),], 5)

## figure showing costs and threat status of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(species2lines, damage_cost), y = damage_cost, fill = redlistCategory)) + 
  ggtitle("A  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) + 
  labs(fill = "Red List category", x = "DAMAGE\ntop 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
A_mammals

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(species2lines, damage_cost), y = damage_cost, fill = redlistCategory)) + 
  ggtitle("B  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("not assessed", "least concern", "endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List category", x = "DAMAGE\ntop 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
C_birds

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(species2lines, damage_cost), y = damage_cost, fill = redlistCategory)) + 
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
B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(species2lines, management_cost), y = management_cost, fill = redlistCategory)) + 
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

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(species2lines, management_cost), y = management_cost, fill = redlistCategory)) + 
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

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(species2lines, management_cost), y = management_cost, fill = redlistCategory)) + 
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

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom", align = "hv")
dev.copy(png, file = paste0(getwd(), "/outputs/FIGURES1.png"), res = 600, height = 7, width = 6.85, units = "in")
dev.off()

## figure showing phylogenetic distinctiveness scores of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
mammals <- data_all[which(data_all$taxon == "MAMMALS"),]
summary(mammals$oriPtree)[3]  # median
summary(mammals$oriPtree)[5]  # 3rd quantile
quantile(mammals$oriPtree[!is.na(mammals$oriPtree)], 0.95) # 95th percentile - not shown on the graph

mammals$rank_oriPtree <- rank(-mammals$oriPtree, na.last = "keep", ties.method = "average")
top5_mam_damage <- merge(top5_mam_damage, mammals)

A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(species2lines, oriPtree), y = oriPtree)) + 
  ggtitle("A  MAMMALS (indicated rank is out of 5,626 species)") + geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "DAMAGE\ntop 5 costliest species", y = "Phylogenetic distinctiveness (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriPtree)), y = oriPtree + 0.2*4.5), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 6)) +
  coord_flip()
A_mammals

birds <- data_all[which(data_all$taxon == "BIRDS"),]
summary(birds$oriPtree)[3]  # median
summary(birds$oriPtree)[5]  # 3rd quantile
birds$rank_oriPtree <- rank(-birds$oriPtree, na.last = "keep", ties.method = "average")
top5_bird_damage <- merge(top5_bird_damage, birds)

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(species2lines, oriPtree), y = oriPtree)) + 
  ggtitle("B  BIRDS (indicated rank is out of 9,758 species)") + geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "DAMAGE\ntop 5 costliest species", y = "Phylogenetic distinctiveness (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriPtree)), y = oriPtree + 0.2*7.5), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 9.5)) +
  coord_flip()
C_birds

plants <- data_all[which(data_all$taxon == "PLANTS"),]
summary(plants$oriPtree)[3]  # median
summary(plants$oriPtree)[5]  # 3rd quantile
plants$rank_oriPtree <- rank(-plants$oriPtree, na.last = "keep", ties.method = "average")
top5_plant_damage <- merge(top5_plant_damage, plants)

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(species2lines, oriPtree), y = oriPtree)) + 
  ggtitle("C PLANTS (indicated rank is out of 287,346 species)") + geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "DAMAGE\ntop 5 costliest species", y = "Phylogenetic distinctiveness (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriPtree)), y = oriPtree + 0.2*16), size = 2, color = "black") +
  geom_text(aes(label = "phylogenetic distinctiveness not assessed", x = 5, y = 7), size = 2) +
  scale_y_continuous(limits = c(0, 17)) +
  coord_flip()
E_plants

# management
top5_mam_management <- merge(top5_mam_management, mammals)

B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(species2lines, oriPtree), y = oriPtree)) + 
  #ggtitle("B  MAMMALS") + 
  geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "MANAGEMENT\ntop 5 costliest species", y = "Phylogenetic distinctiveness (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriPtree)), y = oriPtree + 0.2*7.5), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 9.5)) +
  coord_flip()
B_mammals

top5_bird_management <- merge(top5_bird_management, birds)

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(species2lines, oriPtree), y = oriPtree)) + 
  #ggtitle("D  BIRDS") + 
  geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "MANAGEMENT\ntop 5 costliest species", y = "Phylogenetic distinctiveness (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriPtree)), y = oriPtree + 0.2*7.5), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 9.5)) +
  coord_flip()
D_birds

top5_plant_management <- merge(top5_plant_management, plants)

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(species2lines, oriPtree), y = oriPtree)) + 
  #ggtitle("F PLANTS") + 
  geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "MANAGEMENT\ntop 5 costliest species", y = "Phylogenetic distinctiveness (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriPtree)), y = oriPtree + 0.2*16), size = 2, color = "black") +
  geom_text(aes(label = "phylogenetic distinctiveness not assessed", x = 5, y = 7), size = 2) +
  scale_y_continuous(limits = c(0, 17)) +
  coord_flip()
F_plants

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom", align = "hv")
dev.copy(png, file = paste0(getwd(), "/outputs/FIGURE3.png"), res = 600, height = 7, width = 6.85, units = "in")
dev.off()

## figure showing functional distinctiveness scores of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
summary(mammals$oriFtree)[3]  # median
summary(mammals$oriFtree)[5]  # 3rd quantile

mammals$rank_oriFtree <- rank(-mammals$oriFtree, na.last = "keep", ties.method = "average")
top5_mam_damage <- merge(top5_mam_damage, mammals)

A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(species2lines, oriFtree), y = oriFtree)) + 
  ggtitle("A  MAMMALS (indicated rank is out of 5,107 species)") + geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "DAMAGE\ntop 5 costliest species", y = "Functional distinctiveness") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$oriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$oriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriFtree)), y = oriFtree + 0.15*0.0115), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.0125)) +
  coord_flip()
A_mammals


summary(birds$oriFtree)[3]  # median
summary(birds$oriFtree)[5]  # 3rd quantile
birds$rank_oriFtree <- rank(-birds$oriFtree, na.last = "keep", ties.method = "average")
top5_bird_damage <- merge(top5_bird_damage, birds)

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(species2lines, oriFtree), y = oriFtree)) + 
  ggtitle("B  BIRDS (indicated rank is out of 9,758 species)") + geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "DAMAGE\ntop 5 costliest species", y = "Functional distinctiveness") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$oriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriFtree)), y = oriFtree + 0.2*0.04), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.05)) +
  coord_flip()
C_birds


summary(plants$oriFtree)[3]  # median
summary(plants$oriFtree)[5]  # 3rd quantile
plants$rank_oriFtree <- rank(-plants$oriFtree, na.last = "keep", ties.method = "average")
top5_plant_damage <- merge(top5_plant_damage, plants)

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(species2lines, oriFtree), y = oriFtree)) + 
  ggtitle("C PLANTS (indicated rank is out of 245,405 species)") + geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "DAMAGE\ntop 5 costliest species", y = "Functional distinctiveness") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$oriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriFtree)), y = oriFtree + 0.2*0.3), size = 2, color = "black") +
  geom_text(aes(label = "Functional distinctiveness not assessed", x = 5, y = 0.15), size = 2) +
  scale_y_continuous(limits = c(0, 0.4)) +
  coord_flip()
E_plants

# management
top5_mam_management <- merge(top5_mam_management, mammals)

B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(species2lines, oriFtree), y = oriFtree)) + 
  #ggtitle("B  MAMMALS") + 
  geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "MANAGEMENT\ntop 5 costliest species", y = "Functional distinctiveness") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$oriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$oriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriFtree)), y = oriFtree + 0.2*0.019), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.025)) +
  coord_flip()
B_mammals

top5_bird_management <- merge(top5_bird_management, birds)

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(species2lines, oriFtree), y = oriFtree)) + 
  #ggtitle("D  BIRDS") + 
  geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "MANAGEMENT\ntop 5 costliest species", y = "Functional distinctiveness") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$oriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriFtree)), y = oriFtree + 0.2*0.04), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.05)) +
  coord_flip()
D_birds

top5_plant_management <- merge(top5_plant_management, plants)

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(species2lines, oriFtree), y = oriFtree)) + 
  #ggtitle("F PLANTS") + 
  geom_point(stat="identity", size = 2) + theme_minimal() + 
  labs(x = "MANAGEMENT\ntop 5 costliest species", y = "Functional distinctiveness") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$oriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste("rank", round(rank_oriFtree)), y = oriFtree + 0.2*0.4), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.5)) +
  coord_flip()
F_plants

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom", align = "hv")
dev.copy(png, file = paste0(getwd(), "/outputs/FIGURE4.png"), res = 600, height = 7, width = 6.85, units = "in")
dev.off()
### Presence in InvaCost and costs of the threatened and most distinctive species ----
## identification of the threatened and most original species by taxon ----
threatened <- data_all[which(data_all$redlistCategory%in%c("vulnerable", "endangered", "critically endangered")),]
threatened_mammals <- threatened[which(threatened$taxon=="MAMMALS") ,]
threatened_birds <- threatened[which(threatened$taxon=="BIRDS") ,]
threatened_plants <- threatened[which(threatened$taxon== "PLANTS"),]

TOP25PO_mammals <- mammals[which(mammals$oriPtree > summary(mammals$oriPtree)[5]),]
TOP25PO_birds <- birds[which(birds$oriPtree > summary(birds$oriPtree)[5]),]
TOP25PO_plants <- plants[which(plants$oriPtree > summary(plants$oriPtree)[5]),]

TOP25FO_mammals <- mammals[which(mammals$oriFtree > summary(mammals$oriFtree)[5]),]
TOP25FO_birds <- birds[which(birds$oriFtree > summary(birds$oriFtree)[5]),]
TOP25FO_plants <- plants[which(plants$oriFtree > summary(plants$oriFtree)[5]),]

TOP5PO_mammals <- mammals[which(mammals$oriPtree > quantile(mammals$oriPtree[!is.na(mammals$oriPtree)], 0.95)),]
TOP5PO_birds <- birds[which(birds$oriPtree > quantile(birds$oriPtree[!is.na(birds$oriPtree)], 0.95)),]
TOP5PO_plants <- plants[which(plants$oriPtree > quantile(plants$oriPtree[!is.na(plants$oriPtree)], 0.95)),]

TOP5FO_mammals <- mammals[which(mammals$oriFtree > quantile(mammals$oriFtree[!is.na(mammals$oriFtree)], 0.95)),]
TOP5FO_birds <- birds[which(birds$oriFtree > quantile(birds$oriFtree[!is.na(birds$oriFtree)], 0.95)),]
TOP5FO_plants <- plants[which(plants$oriFtree > quantile(plants$oriFtree[!is.na(plants$oriFtree)], 0.95)),]

## presence in InvaCost and costs of the threatened species ----
threatened_mammals_inv <- threatened_mammals[-which(is.na(threatened_mammals$damage_cost|threatened_mammals$management_cost)),]
threatened_mammals_inv[order(threatened_mammals_inv$redlistCategory, decreasing = TRUE),] 

nrow(threatened_mammals_inv) # 5 mammal species are both threatened and have a cost in InvaCost
nrow(threatened_mammals_inv[!is.na(threatened_mammals_inv$damage_cost),]) # 1 mammal species has a damage cost
nrow(threatened_mammals_inv[!is.na(threatened_mammals_inv$management_cost),]) # 5 mammal species has a management cost

threatened_birds_inv <- threatened_birds[-which(is.na(threatened_birds$damage_cost|threatened_birds$management_cost)),]
threatened_birds_inv # 0 species of birds are threatened and in InvaCost

threatened_plants_inv <- threatened_plants[-which(is.na(threatened_plants$damage_cost|threatened_plants$management_cost)),]
threatened_plants_inv[order(threatened_plants_inv$redlistCategory, decreasing = TRUE),]
nrow(threatened_plants_inv)# 5 species of plants are both threatened and have a cost in InvaCost 
nrow(threatened_plants_inv[!is.na(threatened_plants_inv$damage_cost),]) # 1 plant species has a damage cost
nrow(threatened_plants_inv[!is.na(threatened_plants_inv$management_cost),]) # 5 plant species has a management cost

threatened_inv <- rbind(threatened_mammals_inv[order(threatened_mammals_inv$redlistCategory, decreasing = TRUE),], 
                        threatened_plants_inv[order(threatened_plants_inv$redlistCategory, decreasing = TRUE),])
colnames(threatened_inv)

table_invacost_threatened <- threatened_inv[c("taxon", "species", "redlistCategory", "damage_cost", "management_cost")]
write.csv2(table_invacost_threatened, paste0(getwd(), "/outputs/table_invacost_threatened.csv"))
