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
library(eulerr)

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
mammals$rank_dietoriFtree <- rank(-mammals$dietoriFtree, na.last = "keep", ties.method = "average")
mammals$rank_activityoriFtree <- rank(-mammals$activityoriFtree, na.last = "keep", ties.method = "average")
mammals$rank_massoriFtree <- rank(-mammals$massoriFtree, na.last = "keep", ties.method = "average")
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
birds$rank_dietoriFtree <- rank(-birds$dietoriFtree, na.last = "keep", ties.method = "average")
birds$rank_activityoriFtree <- rank(-birds$activityoriFtree, na.last = "keep", ties.method = "average")
birds$rank_massoriFtree <- rank(-birds$massoriFtree, na.last = "keep", ties.method = "average")
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
plants$rank_dietoriFtree <- rank(-plants$dietoriFtree, na.last = "keep", ties.method = "average")
plants$rank_activityoriFtree <- rank(-plants$activityoriFtree, na.last = "keep", ties.method = "average")
plants$rank_massoriFtree <- rank(-plants$massoriFtree, na.last = "keep", ties.method = "average")
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
## identification of the threatened and most distinctive species by taxon ----
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

## presence in InvaCost and costs of the TOP25 PO species ----
TOP25PO_mammals_inv <- TOP25PO_mammals[-which(is.na(TOP25PO_mammals$damage_cost|TOP25PO_mammals$management_cost)),]
TOP25PO_mammals_inv[order(TOP25PO_mammals_inv$oriPtree, decreasing = TRUE),]
nrow(TOP25PO_mammals_inv) # 11 mammal species are both in TOP25PO and have a cost in InvaCost
nrow(TOP25PO_mammals_inv[!is.na(TOP25PO_mammals_inv$damage_cost),]) # 3 mammal species have a damage cost
nrow(TOP25PO_mammals_inv[!is.na(TOP25PO_mammals_inv$management_cost),]) # 11 mammal species have a management cost

TOP25PO_birds_inv <- TOP25PO_birds[-which(is.na(TOP25PO_birds$damage_cost|TOP25PO_birds$management_cost)),]
TOP25PO_birds_inv[order(TOP25PO_birds_inv$oriPtree, decreasing = TRUE),]
nrow(TOP25PO_birds_inv) # 2 bird species are both in TOP25PO and have a cost in InvaCost
nrow(TOP25PO_birds_inv[!is.na(TOP25PO_birds_inv$damage_cost),]) # none have a damage cost
nrow(TOP25PO_birds_inv[!is.na(TOP25PO_birds_inv$management_cost),]) # 2 bird species have a management cost

TOP25PO_plants_inv <- TOP25PO_plants[-which(is.na(TOP25PO_plants$damage_cost|TOP25PO_plants$management_cost)),]
TOP25PO_plants_inv[order(TOP25PO_plants_inv$oriPtree, decreasing = TRUE),]
nrow(TOP25PO_plants_inv) # 28 plant species are both in TOP25PO and have a cost in InvaCost
nrow(TOP25PO_plants_inv[!is.na(TOP25PO_plants_inv$damage_cost),]) # 5 plant species have a damage cost
nrow(TOP25PO_plants_inv[!is.na(TOP25PO_plants_inv$management_cost),]) # 26 plant species have a management cost

TOP25PO_inv <- rbind(TOP25PO_mammals_inv[order(TOP25PO_mammals_inv$oriPtree, decreasing = TRUE),],
                     TOP25PO_birds_inv[order(TOP25PO_birds_inv$oriPtree, decreasing = TRUE),],
                     TOP25PO_plants_inv[order(TOP25PO_plants_inv$oriPtree, decreasing = TRUE),])

colnames(TOP25PO_inv)

table_invacost_TOP25PO <- TOP25PO_inv[c("taxon", "species", "oriPtree", "rank_oriPtree","damage_cost", "management_cost")]
write.csv2(table_invacost_TOP25PO, paste0(getwd(), "/outputs/table_invacost_TOP25PO.csv"))

## presence in InvaCost and costs of the TOP25 FO species ---- 
TOP25FO_mammals_inv <- TOP25FO_mammals[-which(is.na(TOP25FO_mammals$damage_cost|TOP25FO_mammals$management_cost)),]
TOP25FO_mammals_inv[order(TOP25FO_mammals_inv$oriFtree, decreasing = TRUE),]
nrow(TOP25FO_mammals_inv) # 14 mammal species are both in TOP25FO and have a cost in InvaCost
nrow(TOP25FO_mammals_inv[!is.na(TOP25FO_mammals_inv$damage_cost),]) # 7 mammal species have a damage cost
nrow(TOP25FO_mammals_inv[!is.na(TOP25FO_mammals_inv$management_cost),]) # 12 mammal species have a management cost

TOP25FO_birds_inv <- TOP25FO_birds[-which(is.na(TOP25FO_birds$damage_cost|TOP25FO_birds$management_cost)),]
TOP25FO_birds_inv[order(TOP25FO_birds_inv$oriFtree, decreasing = TRUE),]
nrow(TOP25FO_birds_inv) # 8 bird species are both in TOP25FO and have a cost in InvaCost
nrow(TOP25FO_birds_inv[!is.na(TOP25FO_birds_inv$damage_cost),]) # 4 have a damage cost
nrow(TOP25FO_birds_inv[!is.na(TOP25FO_birds_inv$management_cost),]) # 7 bird species have a management cost

TOP25FO_plants_inv <- TOP25FO_plants[-which(is.na(TOP25FO_plants$damage_cost|TOP25FO_plants$management_cost)),]
TOP25FO_plants_inv[order(TOP25FO_plants_inv$oriFtree, decreasing = TRUE),]
nrow(TOP25FO_plants_inv) # 26 plant species are both in TOP25FO and have a cost in InvaCost
nrow(TOP25FO_plants_inv[!is.na(TOP25FO_plants_inv$damage_cost),]) # 5 plant species have a damage cost
nrow(TOP25FO_plants_inv[!is.na(TOP25FO_plants_inv$management_cost),]) # 24 plant species have a management cost

TOP25FO_inv <- rbind(TOP25FO_mammals_inv[order(TOP25FO_mammals_inv$oriFtree, decreasing = TRUE),],
                     TOP25FO_birds_inv[order(TOP25FO_birds_inv$oriFtree, decreasing = TRUE),],
                     TOP25FO_plants_inv[order(TOP25FO_plants_inv$oriFtree, decreasing = TRUE),])

colnames(TOP25FO_inv)

table_invacost_TOP25FO <- TOP25FO_inv[c("taxon", "species", "oriFtree", "rank_oriFtree",
                                        "dietoriFtree", "rank_dietoriFtree",
                                        "activityoriFtree", "rank_activityoriFtree",     
                                        "massoriFtree", "rank_massoriFtree",
                                        "damage_cost", "management_cost")]
write.csv2(table_invacost_TOP25FO, paste0(getwd(), "/outputs/table_invacost_TOP25FO.csv"))

## presence in InvaCost and costs of the TOP5 PO species ----
TOP5PO_mammals_inv <- TOP5PO_mammals[-which(is.na(TOP5PO_mammals$damage_cost|TOP5PO_mammals$management_cost)),]
TOP5PO_mammals_inv[order(TOP5PO_mammals_inv$oriPtree, decreasing = TRUE),]
nrow(TOP5PO_mammals_inv) # 1 mammal species is both in TOP5PO and has a cost in InvaCost
nrow(TOP5PO_mammals_inv[!is.na(TOP5PO_mammals_inv$damage_cost),]) # it has no damage cost
nrow(TOP5PO_mammals_inv[!is.na(TOP5PO_mammals_inv$management_cost),]) # it has a management cost

TOP5PO_birds_inv <- TOP5PO_birds[-which(is.na(TOP5PO_birds$damage_cost|TOP5PO_birds$management_cost)),]
TOP5PO_birds_inv[order(TOP5PO_birds_inv$oriPtree, decreasing = TRUE),]
nrow(TOP5PO_birds_inv) # none bird species is both in TOP5PO and has a cost in InvaCost

TOP5PO_plants_inv <- TOP5PO_plants[-which(is.na(TOP5PO_plants$damage_cost|TOP5PO_plants$management_cost)),]
TOP5PO_plants_inv[order(TOP5PO_plants_inv$oriPtree, decreasing = TRUE),]
nrow(TOP5PO_plants_inv) # 7 plant species are both in TOP5PO and have a cost in InvaCost
nrow(TOP5PO_plants_inv[!is.na(TOP5PO_plants_inv$damage_cost),]) # 2 plant species have a damage cost
nrow(TOP5PO_plants_inv[!is.na(TOP5PO_plants_inv$management_cost),]) # 6 plant species have a management cost

TOP5PO_inv <- rbind(TOP5PO_mammals_inv[order(TOP5PO_mammals_inv$oriPtree, decreasing = TRUE),],
                     TOP5PO_birds_inv[order(TOP5PO_birds_inv$oriPtree, decreasing = TRUE),],
                     TOP5PO_plants_inv[order(TOP5PO_plants_inv$oriPtree, decreasing = TRUE),])

table_invacost_TOP5PO <- TOP5PO_inv[c("taxon", "species", "oriPtree", "rank_oriPtree","damage_cost", "management_cost")]
write.csv2(table_invacost_TOP5PO, paste0(getwd(), "/outputs/table_invacost_TOP5PO.csv"))

## presence in InvaCost and costs of the TOP5 FO species ---- 
TOP5FO_mammals_inv <- TOP5FO_mammals[-which(is.na(TOP5FO_mammals$damage_cost|TOP5FO_mammals$management_cost)),]
TOP5FO_mammals_inv[order(TOP5FO_mammals_inv$oriFtree, decreasing = TRUE),]
nrow(TOP5FO_mammals_inv) # 8 mammal species are both in TOP5FO and have a cost in InvaCost
nrow(TOP5FO_mammals_inv[!is.na(TOP5FO_mammals_inv$damage_cost),]) # 4 mammal species have a damage cost
nrow(TOP5FO_mammals_inv[!is.na(TOP5FO_mammals_inv$management_cost),]) # 6 mammal species have a management cost

TOP5FO_birds_inv <- TOP5FO_birds[-which(is.na(TOP5FO_birds$damage_cost|TOP5FO_birds$management_cost)),]
TOP5FO_birds_inv[order(TOP5FO_birds_inv$oriFtree, decreasing = TRUE),]
nrow(TOP5FO_birds_inv) # 3 bird species are both in TOP5FO and have a cost in InvaCost
nrow(TOP5FO_birds_inv[!is.na(TOP5FO_birds_inv$damage_cost),]) # 1 has a damage cost
nrow(TOP5FO_birds_inv[!is.na(TOP5FO_birds_inv$management_cost),]) # 3 bird species have a management cost

TOP5FO_plants_inv <- TOP5FO_plants[-which(is.na(TOP5FO_plants$damage_cost|TOP5FO_plants$management_cost)),]
TOP5FO_plants_inv[order(TOP5FO_plants_inv$oriFtree, decreasing = TRUE),]
nrow(TOP5FO_plants_inv) # 8 plant species are both in TOP5FO and have a cost in InvaCost
nrow(TOP5FO_plants_inv[!is.na(TOP5FO_plants_inv$damage_cost),]) # 2 plant species have a damage cost
nrow(TOP5FO_plants_inv[!is.na(TOP5FO_plants_inv$management_cost),]) # 7 plant species have a management cost

TOP5FO_inv <- rbind(TOP5FO_mammals_inv[order(TOP5FO_mammals_inv$oriFtree, decreasing = TRUE),],
                     TOP5FO_birds_inv[order(TOP5FO_birds_inv$oriFtree, decreasing = TRUE),],
                     TOP5FO_plants_inv[order(TOP5FO_plants_inv$oriFtree, decreasing = TRUE),])

table_invacost_TOP5FO <- TOP5FO_inv[c("taxon", "species", "oriFtree", "rank_oriFtree",
                                        "dietoriFtree", "rank_dietoriFtree",
                                        "activityoriFtree", "rank_activityoriFtree",     
                                        "massoriFtree", "rank_massoriFtree",
                                        "damage_cost", "management_cost")]
write.csv2(table_invacost_TOP5FO, paste0(getwd(), "/outputs/table_invacost_TOP5FO.csv"))


## build a table with quantitative info about all species in InvaCost being either threatened 
## and/or in TOP25PO and/or in TOP25FO and/or in TOP5PO and/or in TOP5FO----
quanti_all <- rbind(mammals, birds, plants) # all mammals, birds, and plants
quanti_inv <- quanti_all[-which(is.na(quanti_all$damage_cost|quanti_all$management_cost)),] # mammals, birds, and plants having a cost in InvaCost

consparadox <- unique(c(threatened_inv$species, TOP25PO_inv$species, TOP25FO_inv$species,
                        TOP5PO_inv$species, TOP5FO_inv$species)) # list of species in InvaCost being either threatened and/or in TOP25PO and/or in TOP25FO and/or in TOP5PO and/or in TOP5FO

quanti_consparadox <- quanti_inv[which(quanti_inv$species %in% consparadox),]

colnames(quanti_consparadox)

quanti_consparadox <- quanti_consparadox[c("taxon", "species", "redlistCategory", "oriPtree", "rank_oriPtree", "dietoriFtree", "rank_dietoriFtree",
                                           "activityoriFtree", "rank_activityoriFtree",  "massoriFtree", "rank_massoriFtree",
                                           "oriFtree", "rank_oriFtree", "damage_cost", "management_cost")]

quanti_consparadox$threatened <- 0
quanti_consparadox$threatened[which(quanti_consparadox$species %in% threatened_inv$species)] <- 1

quanti_consparadox$TOP25PO <- 0
quanti_consparadox$TOP25PO[which(quanti_consparadox$species %in% TOP25PO_inv$species)] <- 1

quanti_consparadox$TOP25FO <- 0
quanti_consparadox$TOP25FO[which(quanti_consparadox$species %in% TOP25FO_inv$species)] <- 1

quanti_consparadox$TOP5PO <- 0
quanti_consparadox$TOP5PO[which(quanti_consparadox$species %in% TOP5PO_inv$species)] <- 1

quanti_consparadox$TOP5FO <- 0
quanti_consparadox$TOP5FO[which(quanti_consparadox$species %in% TOP5FO_inv$species)] <- 1

quanti_consparadox$N_criteriaTOP25 <- rowSums(quanti_consparadox[c("threatened", "TOP25PO", "TOP25FO")])
quanti_consparadox$N_criteriaTOP5 <- rowSums(quanti_consparadox[c("threatened", "TOP5PO", "TOP5FO")])

quanti_consparadox$species[quanti_consparadox$N_criteriaTOP25==2]
quanti_consparadox$species[quanti_consparadox$N_criteriaTOP5==2]

str(quanti_consparadox)

write.csv2(quanti_consparadox, paste0(getwd(), "/outputs/quanti_consparadox.csv"))

# quantitative information on mammal conservation paradoxes
quanti_consparadox_mammals <- quanti_consparadox[which(quanti_consparadox$taxon=="MAMMALS"),]
nrow(quanti_consparadox_mammals) # 23 mammal species constitute a conservation paradox, of which
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteriaTOP25 == 1 & quanti_consparadox_mammals$threatened == 1),]) # 3 are threatened only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteriaTOP25 == 1 & quanti_consparadox_mammals$TOP25PO == 1),]) # 3 belong to the TOP25% PO only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteriaTOP25 == 1 & quanti_consparadox_mammals$TOP25FO == 1),]) # 9 belong to the TOP25% FO only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$threatened == 1 & quanti_consparadox_mammals$TOP25PO == 1),]) # 2 are both threatened and belong to the TOP25% PO
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$threatened == 1 & quanti_consparadox_mammals$TOP25FO == 1),]) # 0 are both threatened and belong to the TOP25% FO
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$TOP25PO == 1 & quanti_consparadox_mammals$TOP25FO == 1),]) # 5 both belong to the TOP25% PO and the TOP25% FO

nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteriaTOP5 == 1 & quanti_consparadox_mammals$TOP5PO == 1),]) # 0 belong to the TOP5% PO only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteriaTOP5 == 1 & quanti_consparadox_mammals$TOP5FO == 1),]) # 8 belong to the TOP5% FO only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$TOP5PO == 1 & quanti_consparadox_mammals$TOP5FO == 1),]) # 0 both belong to the TOP5% PO and the TOP5% FO

# quantitative information on bird conservation paradoxes
quanti_consparadox_birds <- quanti_consparadox[which(quanti_consparadox$taxon=="BIRDS"),]
nrow(quanti_consparadox_birds) # 9 bird species constitute a conservation paradox, of which
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteriaTOP25 == 1 & quanti_consparadox_birds$threatened == 1),]) # 0 are threatened only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteriaTOP25 == 1 & quanti_consparadox_birds$TOP25PO == 1),]) # 1 belongs to the TOP25% PO only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteriaTOP25 == 1 & quanti_consparadox_birds$TOP25FO == 1),]) # 7 belong to the TOP25% FO only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$TOP25PO == 1 & quanti_consparadox_birds$TOP25FO == 1),]) # 1 both belongs to the TOP25% PO and the TOP25% FO

nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteriaTOP5 == 1 & quanti_consparadox_birds$TOP5PO == 1),]) # 0 belong to the TOP5% PO only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteriaTOP5 == 1 & quanti_consparadox_birds$TOP5FO == 1),]) # 3 belong to the TOP5% FO only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$TOP5PO == 1 & quanti_consparadox_birds$TOP5FO == 1),]) # 0 both belong to the TOP5% PO and the TOP5% FO

# quantitative information on plant conservation paradoxes
quanti_consparadox_plants <- quanti_consparadox[which(quanti_consparadox$taxon=="PLANTS"),]
nrow(quanti_consparadox_plants) # 57 plant species constitute a conservation paradox, of which
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteriaTOP25 == 1 & quanti_consparadox_plants$threatened == 1),]) # 5 are threatened only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteriaTOP25 == 1 & quanti_consparadox_plants$TOP25PO == 1),]) # 26 belong to the TOP25% PO only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteriaTOP25 == 1 & quanti_consparadox_plants$TOP25FO == 1),]) # 24 belong to the TOP25% FO only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$threatened == 1 & quanti_consparadox_plants$TOP25PO == 1),]) # 0 are both threatened and belong to the TOP25% PO
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$threatened == 1 & quanti_consparadox_plants$TOP25FO == 1),]) # 0 are both threatened and belong to the TOP25% FO
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$TOP25PO == 1 & quanti_consparadox_plants$TOP25FO == 1),]) # 2 both belong to the TOP25% PO and the TOP25% FO

nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteriaTOP5 == 1 & quanti_consparadox_plants$TOP5PO == 1),]) # 7 belong to the TOP5% PO only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteriaTOP5 == 1 & quanti_consparadox_plants$TOP5FO == 1),]) # 8 belong to the TOP5% FO only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$TOP5PO == 1 & quanti_consparadox_plants$TOP5FO == 1),]) # 0 both belong to the TOP5% PO and the TOP5% FO

## build euler diagram to show which species have both a cost in Invacost and are threatened 
## and/or belong to TOP25PO and/or belong to TOP25FO and/or belong to TOP5PO and/or belong to TOP5FO ----
# prepare the input tables
quanti_all$invacost <- 0
quanti_all$invacost[-which(is.na(quanti_all$damage_cost|quanti_all$management_cost))] <- 1
  
quanti_all$threatened <- 0
quanti_all$threatened[which(quanti_all$species %in% threatened$species)] <- 1

quanti_all$TOP25PO <- 0
quanti_all$TOP25PO[which(quanti_all$species %in% c(TOP25PO_mammals$species, TOP25PO_birds$species, TOP25PO_plants$species))] <- 1

quanti_all$TOP25FO <- 0
quanti_all$TOP25FO[which(quanti_all$species %in% c(TOP25FO_mammals$species, TOP25FO_birds$species, TOP25FO_plants$species))] <- 1

quanti_all$TOP5PO <- 0
quanti_all$TOP5PO[which(quanti_all$species %in% c(TOP5PO_mammals$species, TOP5PO_birds$species, TOP5PO_plants$species))] <- 1

quanti_all$TOP5FO <- 0
quanti_all$TOP5FO[which(quanti_all$species %in% c(TOP5FO_mammals$species, TOP5FO_birds$species, TOP5FO_plants$species))] <- 1

colnames(quanti_all)

# make and save the plots
euler_input_TOP25 <- quanti_all[, c("invacost", "threatened", "TOP25PO", "TOP25FO")] 
fit_TOP25 <-  euler(euler_input_TOP25, shape = "ellipse")
fit_TOP25
plot(fit_TOP25) # the number of species in InvaCost is low compared to the number of species investigated that it is worth representing only InvaCost species

euler_input_TOP25_inv <- euler_input_TOP25[which(euler_input_TOP25$invacost==1), c("threatened", "TOP25PO", "TOP25FO")]
fit_TOP25_inv <-  euler(euler_input_TOP25_inv, shape = "ellipse")
fit_TOP25_inv

windows(6.85, 5)
plot(fit_TOP25_inv, quantities = TRUE, 
     labels = c("Threatened", "Phylogenetically distinctive", "Functionally distinctive"),
     fill = RColorBrewer::brewer.pal(3, "Pastel1"))
dev.copy(png, file = paste0(getwd(), "/outputs/FIGURE2_TOP25.png"), res = 600, height = 5, width = 6.85, units = "in")
dev.off()

euler_input_TOP5 <- quanti_all[, c("invacost", "threatened", "TOP5PO", "TOP5FO")] 
fit_TOP5 <-  euler(euler_input_TOP5, shape = "ellipse")
fit_TOP5
plot(fit_TOP5) # the number of species in InvaCost is low compared to the number of species investigated that it is worth representing only InvaCost species

euler_input_TOP5_inv <- euler_input_TOP5[which(euler_input_TOP5$invacost==1), c("threatened", "TOP5PO", "TOP5FO")]
fit_TOP5_inv <-  euler(euler_input_TOP5_inv, shape = "ellipse")
fit_TOP5_inv

windows(6.85, 5)
plot(fit_TOP5_inv, quantities = TRUE, 
     labels = c("Threatened", "Phylogenetically\ndistinctive", "Functionally\ndistinctive"),
     fill = RColorBrewer::brewer.pal(3, "Pastel1"))
dev.copy(png, file = paste0(getwd(), "/outputs/FIGURE2_TOP5.png"), res = 600, height = 5, width = 6.85, units = "in")
dev.off()






### Showing threat status and distinctiveness of species in InvaCost in in comparison of all species of their taxonomic group ----
## threat status ----
# mammals
mammals <- quanti_all[which(quanti_all$taxon=="MAMMALS"),]
Nb <- rep(1, length(mammals[,1]))
mammals$Nb <- Nb
mammals$invacost <- factor(mammals$invacost, levels = c("1", "0"))
levels(mammals$invacost)
mammals$redlistCategory <- factor(mammals$redlistCategory)
levels(mammals$redlistCategory)

threatmammals <- ddply(mammals, c("redlistCategory", "invacost"), summarise, Number=sum(Nb), .drop = FALSE)

plot_threatmammals <- ggplot(data = threatmammals, aes(x = redlistCategory, fill = invacost, y = Number)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("MAMMALS") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust = 1), axis.title.x = element_blank(), plot.title = element_text(size = 10))+
  guides(fill = guide_legend(title = "Presence in InvaCost"))+
  labs(y = "Number of species") +
  scale_fill_manual(values = c("black","grey")) +
  scale_y_continuous(trans = "sqrt")

plot_threatmammals

# birds
birds <- quanti_all[which(quanti_all$taxon=="BIRDS"),]
Nb <- rep(1, length(birds[,1]))
birds$Nb <- Nb
birds$invacost <- factor(birds$invacost, levels = c("1", "0"))
levels(birds$invacost)
birds$redlistCategory <- factor(birds$redlistCategory)
levels(birds$redlistCategory)

threatbirds <- ddply(birds, c("redlistCategory", "invacost"), summarise, Number=sum(Nb), .drop = FALSE)

plot_threatbirds <- ggplot(data = threatbirds, aes(x = redlistCategory, fill = invacost, y = Number)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("BIRDS") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust = 1), axis.title.x = element_blank(), plot.title = element_text(size = 10)) +
  guides(fill = guide_legend(title = "Presence in InvaCost"))+
  labs(y = "Number of species") +
  scale_fill_manual(values = c("black","grey")) +
  scale_y_continuous(trans = "sqrt")

plot_threatbirds

# plants
plants <- quanti_all[which(quanti_all$taxon=="PLANTS"),]
Nb <- rep(1, length(plants[,1]))
plants$Nb <- Nb

plants$invacost <- factor(plants$invacost, levels = c("1", "0"))
levels(plants$invacost)
plants$redlistCategory <- factor(plants$redlistCategory)
levels(plants$redlistCategory)

threatplants <- ddply(plants, c("redlistCategory", "invacost"), summarise, Number=sum(Nb), .drop = FALSE)

plot_threatplants <- ggplot(data = threatplants, aes(x = redlistCategory, fill = invacost, y = Number)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("PLANTS" ) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust = 1), axis.title.x = element_blank(), plot.title = element_text(size = 10))+
  guides(fill = guide_legend(title = "Presence in InvaCost"))+
  labs(y = "Number of species") +
  scale_fill_manual(values = c("black","grey")) +
  scale_y_continuous(trans = "sqrt")

plot_threatplants

## phylogenetic distinctiveness ---- HERE!!!
# mammals
phylomammals <- mammals[!is.na(mammals$oriPtree),]

plot_phylomammals <- ggplot(phylomammals, aes(x = oriPtree, y = "")) +
  geom_violin(fill = "grey", scale = "area") +
  geom_jitter(data = phylomammals[which(phylomammals$invacost == 1),], aes(x = oriPtree, y = ""),
              shape = 16, size = 1, binwidth = 0.005, color = "black", position = position_jitter(0)) +
  geom_vline(aes(xintercept = summary(oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_vline(aes(xintercept = summary(oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  theme_bw() +  theme(plot.title = element_text(size = 10)) +
  labs(x = "Phylogenetic distinctiveness (Ma)", y = "") +
  ggtitle("MAMMALS") +
  scale_x_continuous(trans = "log", breaks = c(5, 15, 25, 35, 45, 55)) +
  coord_flip()

plot_phylomammals





