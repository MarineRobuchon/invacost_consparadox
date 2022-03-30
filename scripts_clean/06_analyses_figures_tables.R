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
data_all <-  read.csv2(paste0(getwd(), "/outputs/data_all.csv"))[, -1]
data_all$Species2 <- gsub(" ", "\n", data_all$Species)
data_all$redlistCategory_version_2020.2[which(is.na(data_all$redlistCategory_version_2020.2))] <- "Unknown"
data_all$redlistCategory_version_2020.2 <- factor(data_all$redlistCategory_version_2020.2)
levels(data_all$redlistCategory_version_2020.2)
data_all$redlistCategory_version_2020.2 <- factor(data_all$redlistCategory_version_2020.2, 
                                                  levels = c("Unknown", "Data Deficient", "Least Concern", "Near Threatened",
                                                  "LR", "Vulnerable", "Endangered", "Critically Endangered"))


### Threat status and originality of the costliest invasive species in InvaCost ----
## identification of the costliest species by type of costs & taxon ----
# damage
damage <- data_all[-which(is.na(data_all$Average.annual.cost_damage)),]

mam_damage <- damage[which(damage$className=="MAMMALIA"),]
nrow(mam_damage) # 21 mammal species have a damage cost
top5_mam_damage <- head(mam_damage[order(mam_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

bird_damage <- damage[which(damage$className=="AVES"),]
nrow(bird_damage) # 13 bird species have a damage cost
top5_bird_damage <- head(bird_damage[order(bird_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

plant_damage <- damage[which(damage$className=="PLANTS"),]
nrow(plant_damage)# &&é plant species have a damage cost
top5_plant_damage <- head(plant_damage[order(plant_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

# management
management <- data_all[-which(is.na(data_all$Average.annual.cost_management)),]

mam_management <- management[which(management$className=="MAMMALIA"),]
nrow(mam_management) # 36 mammal species have a management cost
top5_mam_management <- head(mam_management[order(mam_management$Average.annual.cost_management, decreasing = TRUE),], 5)

bird_management <- management[which(management$className=="AVES"),]
nrow(bird_management) # 15 bird species have a management cost
top5_bird_management <- head(bird_management[order(bird_management$Average.annual.cost_management, decreasing = TRUE),], 5)

plant_management <- management[which(management$className=="PLANTS"),]
nrow(plant_management) # 287 plant species have a management cost
top5_plant_management <- head(plant_management[order(plant_management$Average.annual.cost_management, decreasing = TRUE),], 5)

## figure showing costs and threat status of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(Species2, Average.annual.cost_damage), y = Average.annual.cost_damage, fill = redlistCategory_version_2020.2)) + 
  ggtitle("A  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Not assessed", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) + 
  labs(fill = "Red List Category", x = "DAMAGE\nTOP 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
A_mammals
  
C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(Species2, Average.annual.cost_damage), y = Average.annual.cost_damage, fill = redlistCategory_version_2020.2)) + 
  ggtitle("B  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Not assessed", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "DAMAGE\nTOP 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
C_birds

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(Species2, Average.annual.cost_damage), y = Average.annual.cost_damage, fill = redlistCategory_version_2020.2)) + 
  ggtitle("C PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Not assessed", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "DAMAGE\nTOP 5 costliest species", y = "Average annual damage cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
E_plants
 
# management
B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(Species2, Average.annual.cost_management), y = Average.annual.cost_management, fill = redlistCategory_version_2020.2)) + 
  #ggtitle("B  MAMMALS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Not assessed", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "MANAGEMENT\nTOP 5 costliest species", y = "Average annual management cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
B_mammals

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(Species2, Average.annual.cost_management), y = Average.annual.cost_management, fill = redlistCategory_version_2020.2)) + 
  #ggtitle("D  BIRDS") + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(limits = c("Not assessed", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "MANAGEMENT\nTOP 5 costliest species", y = "Average annual management cost\n (2017 million US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
D_birds

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(Species2, Average.annual.cost_management), y = Average.annual.cost_management, fill = redlistCategory_version_2020.2)) + 
  #ggtitle("F PLANTS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Not assessed", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) + 
  labs(fill = "Red List Category", x = "MANAGEMENT\nTOP 5 costliest species", y = "Average annual management cost\n (2017 million US$)") + 
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
dev.copy(png, file = paste0(getwd(), "/outputs/clean_fig1.png"), res = 600, height = 7, width = 6.85, units = "in")
dev.off()

## figure showing phylogenetic originality scores of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
mammals <- data_all[which(data_all$className == "MAMMALIA"),]
summary(mammals$oriPtree)[3]  # median
summary(mammals$oriPtree)[5]  # 3rd quantile
mammals$rank_oriPtree <- rank(-mammals$oriPtree, na.last = "keep", ties.method = "average")
top5_mam_damage <- merge(top5_mam_damage, mammals)

A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("A  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/5425"), y = oriPtree + 0.4), size = 2, color = "black") +
  coord_flip()
A_mammals

birds <- data_all[which(data_all$className == "AVES"),]
summary(birds$oriPtree)[3]  # median
summary(birds$oriPtree)[5]  # 3rd quantile
birds$rank_oriPtree <- rank(-birds$oriPtree, na.last = "keep", ties.method = "average")
top5_bird_damage <- merge(top5_bird_damage, birds)

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("B  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/8095"), y = oriPtree + 0.6), size = 2, color = "black") +
  coord_flip()
C_birds

plants <- data_all[which(data_all$className == "PLANTS"),]
summary(plants$oriPtree)[3]  # median
summary(plants$oriPtree)[5]  # 3rd quantile
plants$rank_oriPtree <- rank(-plants$oriPtree, na.last = "keep", ties.method = "average")
top5_plant_damage <- merge(top5_plant_damage, plants)

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("C PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/356184"), y = oriPtree + 0.15 * max(oriPtree)), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, max(top5_plant_damage$oriPtree) + 0.2 * max(top5_plant_damage$oriPtree))) +
  coord_flip()
E_plants

# management
top5_mam_management <- merge(top5_mam_management, mammals)

B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  #ggtitle("B  MAMMALS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/5425"), y = oriPtree + 0.4), size = 2, color = "black") +
  coord_flip()
B_mammals

top5_bird_management <- merge(top5_bird_management, birds)

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  #ggtitle("D  BIRDS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/8095"), y = oriPtree + 0.6), size = 2, color = "black") +
  coord_flip()
D_birds

top5_plant_management <- merge(top5_plant_management, plants)

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  #ggtitle("F PLANTS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/356184"), y = oriPtree + 0.15 * max(oriPtree)), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, max(top5_plant_management$oriPtree) + 0.3 * max(top5_plant_management$oriPtree))) +
  coord_flip()
F_plants

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom", align = "hv")
dev.copy(png, file = paste0(getwd(), "/outputs/clean_fig2.png"), res = 600, height = 7, width = 6.85, units = "in")
dev.off()

## figure showing functional originality scores of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
summary(mammals$meanoriFtree)[3]  # median
summary(mammals$meanoriFtree)[5]  # 3rd quantile
mammals$rank_meanoriFtree <- rank(-mammals$meanoriFtree, na.last = "keep", ties.method = "average")
top5_mam_damage <- merge(top5_mam_damage, mammals)

A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("A  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/4798"), y = meanoriFtree + 0.0007), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.011)) +
  coord_flip()
A_mammals

summary(birds$meanoriFtree)[3]  # median
summary(birds$meanoriFtree)[5]  # 3rd quantile
birds$rank_meanoriFtree <- rank(-birds$meanoriFtree, na.last = "keep", ties.method = "average")
top5_bird_damage <- merge(top5_bird_damage, birds)

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("B  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/8095"), y = meanoriFtree + 0.005), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.05)) +
  coord_flip()
C_birds

summary(plants$meanoriFtree)[3]  # median
summary(plants$meanoriFtree)[5]  # 3rd quantile
plants$rank_meanoriFtree <- rank(-plants$meanoriFtree, na.last = "keep", ties.method = "average")
top5_plant_damage <- merge(top5_plant_damage, plants)

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("C  PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/212530"), y = meanoriFtree + 0.05), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.5)) +
  coord_flip()
E_plants

# management
top5_mam_management <- merge(top5_mam_management, mammals)

B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  #ggtitle("B  MAMMALS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/4798"), y = meanoriFtree + 0.002), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.02)) +
  coord_flip()
B_mammals


top5_bird_management <- merge(top5_bird_management, birds)

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  #ggtitle("D  BIRDS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/8095"), y = meanoriFtree + 0.005), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.05)) +
  coord_flip()
D_birds


top5_plant_management <- merge(top5_plant_management, plants)

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  #ggtitle("F  PLANTS") + 
  geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/212530"), y = meanoriFtree + 0.04), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.45)) +
  coord_flip()
F_plants

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom", align = "hv")
dev.copy(png, file = paste0(getwd(), "/outputs/clean_fig3.png"), res = 600, height = 7, width = 6.85, units = "in")
dev.off()

### Presence in InvaCost and costs of the threatened and most original species ----
## identification of the threatened and most original species by taxon ----
threatened <- data_all[which(data_all$redlistCategory_version_2020.2%in%c("Near Threatened", "Vulnerable", 
                                                                          "Endangered", "Critically Endangered")),]
threatened_mammals <- threatened[which(threatened$className=="MAMMALIA") ,]
threatened_birds <- threatened[which(threatened$className=="AVES") ,]
threatened_plants <- threatened[which(threatened$className == "PLANTS"),]

TOP25PO_mammals <- mammals[which(mammals$oriPtree > summary(mammals$oriPtree)[5]),]
TOP25PO_birds <- birds[which(birds$oriPtree > summary(birds$oriPtree)[5]),]
TOP25PO_plants <- plants[which(plants$oriPtree > summary(plants$oriPtree)[5]),]

TOP25FO_mammals <- mammals[which(mammals$meanoriFtree > summary(mammals$meanoriFtree)[5]),]
TOP25FO_birds <- birds[which(birds$meanoriFtree > summary(birds$meanoriFtree)[5]),]
TOP25FO_plants <- plants[which(plants$meanoriFtree > summary(plants$meanoriFtree)[5]),]

## presence in InvaCost and costs of the threatened species ----
threatened_mammals_inv <- threatened_mammals[which(threatened_mammals$invacostY=="Y"),]
threatened_mammals_inv[order(threatened_mammals_inv$redlistCategory_version_2020.2, decreasing = TRUE),] 

nrow(threatened_mammals_inv) # 7 mammal species are in InvaCost
nrow(threatened_mammals_inv[which(!is.na(threatened_mammals_inv$Average.annual.cost_damage) 
                                  | !is.na(threatened_mammals_inv$Average.annual.cost_management)),]) # 6 mammal species have cost
nrow(threatened_mammals_inv[!is.na(threatened_mammals_inv$Average.annual.cost_damage),]) # 2 mammal species has a damage cost
nrow(threatened_mammals_inv[!is.na(threatened_mammals_inv$Average.annual.cost_management),]) # 6 mammal species has a management cost

threatened_birds_inv <- threatened_birds[which(threatened_birds$invacostY=="Y"),]
threatened_birds_inv # 0 species of birds are threatened and in InvaCost

threatened_plants_inv <- threatened_plants[which(threatened_plants$invacostY=="Y"),]
threatened_plants_inv[order(threatened_plants_inv$redlistCategory_version_2020.2, decreasing = TRUE),]# 2 species of plants are threatened and in InvaCost 
# 2 species of plants are threatened and in InvaCost
# Kalanchoe daigremontiana, EN, no damage cost, management cost =  4.470462e-05
# Citharexylum gentryi, VU, no damage cost, management cost = 1.359312e-02  

threatened_inv <- rbind(threatened_mammals_inv[order(threatened_mammals_inv$redlistCategory_version_2020.2, decreasing = TRUE),], 
                        threatened_plants_inv[order(threatened_plants_inv$redlistCategory_version_2020.2, decreasing = TRUE),])
colnames(threatened_inv)
table1 <- threatened_inv[c("Species", "className", "redlistCategory_version_2020.2", "Average.annual.cost_damage", "Average.annual.cost_management")]
colnames(table1) <- c("Species", "Class", "Red List Category", "Damage cost", "Management cost")
write.csv2(table1, paste0(getwd(), "/outputs/table1.csv"))
 
## presence in InvaCost and costs of the TOP25 PO species ----
TOP25PO_mammals_inv <- TOP25PO_mammals[which(TOP25PO_mammals$invacostY=="Y"),]
TOP25PO_mammals_inv[order(TOP25PO_mammals_inv$oriPtree, decreasing = TRUE),]

nrow(TOP25PO_mammals_inv) # 12 mammal species are in InvaCost
nrow(TOP25PO_mammals_inv[which(!is.na(TOP25PO_mammals_inv$Average.annual.cost_damage) 
                                  | !is.na(TOP25PO_mammals_inv$Average.annual.cost_management)),]) # 11 mammal species have a cost
nrow(TOP25PO_mammals_inv[!is.na(TOP25PO_mammals_inv$Average.annual.cost_damage),]) # 4 mammal species has a damage cost
nrow(TOP25PO_mammals_inv[!is.na(TOP25PO_mammals_inv$Average.annual.cost_management),]) # 11 mammal species has a management cost

TOP25PO_birds_inv <- TOP25PO_birds[which(TOP25PO_birds$invacostY=="Y"),]
TOP25PO_birds_inv[order(TOP25PO_birds_inv$oriPtree, decreasing = TRUE),]
# 2 species of birds belonging to the TOP25 PO are also in InvaCost
# Threskiornis aethiopicus, TOP PO 571, no damage cost, management cost = 0.00210740 
# Aquila chrysaetos, TOP PO 1909, no damage cost, management cost =  0.07180562

TOP25PO_plants_inv <- TOP25PO_plants[which(TOP25PO_plants$invacostY=="Y"),]
TOP25PO_plants_inv[order(TOP25PO_plants_inv$oriPtree, decreasing = TRUE),]
nrow(TOP25PO_plants_inv) # 76 species of plants belonging to the TOP25 PO are also in InvaCost
nrow(TOP25PO_plants_inv[which(!is.na(TOP25PO_plants_inv$Average.annual.cost_damage) 
                               | !is.na(TOP25PO_plants_inv$Average.annual.cost_management)),]) # 72 plant species have a cost
nrow(TOP25PO_plants_inv[!is.na(TOP25PO_plants_inv$Average.annual.cost_damage),]) # 30 plant species has a damage cost
nrow(TOP25PO_plants_inv[!is.na(TOP25PO_plants_inv$Average.annual.cost_management),]) # 60 plant species has a management cost

TOP25PO_inv <- rbind(TOP25PO_mammals_inv[order(TOP25PO_mammals_inv$oriPtree, decreasing = TRUE),],
                     TOP25PO_birds_inv[order(TOP25PO_birds_inv$oriPtree, decreasing = TRUE),],
                     TOP25PO_plants_inv[order(TOP25PO_plants_inv$oriPtree, decreasing = TRUE),])

colnames(TOP25PO_inv)

table2 <- TOP25PO_inv[c("Species", "className", "oriPtree", "rank_oriPtree", "Average.annual.cost_damage", "Average.annual.cost_management")]
colnames(table2) <- c("Species", "Class", "Phylogenetic originality score", "Phylogenetic originality rank", "Damage cost", "Management cost")
write.csv2(table2, paste0(getwd(), "/outputs/table2.csv"))

## presence in InvaCost and costs of the TOP25 FO species ----
TOP25FO_mammals_inv <- TOP25FO_mammals[which(TOP25FO_mammals$invacostY=="Y"),]
TOP25FO_mammals_inv[order(TOP25FO_mammals_inv$meanoriFtree, decreasing = TRUE),]
nrow(TOP25FO_mammals_inv) # 13 species of mammals belonging to the TOP25 FO are also in InvaCost
nrow(TOP25FO_mammals_inv[which(!is.na(TOP25FO_mammals_inv$Average.annual.cost_damage) 
                               | !is.na(TOP25FO_mammals_inv$Average.annual.cost_management)),]) # 12 mammal species have a cost
nrow(TOP25FO_mammals_inv[!is.na(TOP25FO_mammals_inv$Average.annual.cost_damage),]) # 6 mammal species have a damage cost
nrow(TOP25FO_mammals_inv[!is.na(TOP25FO_mammals_inv$Average.annual.cost_management),]) # 11 mammal species have a management cost

TOP25FO_birds_inv <- TOP25FO_birds[which(TOP25FO_birds$invacostY=="Y"),]
TOP25FO_birds_inv[order(TOP25FO_birds_inv$meanoriFtree, decreasing = TRUE),]
nrow(TOP25FO_birds_inv)# 10 species of birds belonging to the TOP25 FO are also in InvaCost
nrow(TOP25FO_birds_inv[which(!is.na(TOP25FO_birds_inv$Average.annual.cost_damage) 
                               | !is.na(TOP25FO_birds_inv$Average.annual.cost_management)),]) # 10 bird species have a cost
nrow(TOP25FO_birds_inv[!is.na(TOP25FO_birds_inv$Average.annual.cost_damage),]) # 6 bird species have a damage cost
nrow(TOP25FO_birds_inv[!is.na(TOP25FO_birds_inv$Average.annual.cost_management),]) #  8 bird species have a management cost

TOP25FO_plants_inv <- TOP25FO_plants[which(TOP25FO_plants$invacostY=="Y"),]
TOP25FO_plants_inv[order(TOP25FO_plants_inv$meanoriFtree, decreasing = TRUE),]
nrow(TOP25FO_plants_inv) # 22 species of plants belonging to the TOP25 FO are also in InvaCost
nrow(TOP25FO_plants_inv[which(!is.na(TOP25FO_plants_inv$Average.annual.cost_damage) 
                             | !is.na(TOP25FO_plants_inv$Average.annual.cost_management)),]) # 21 plant species have a cost
nrow(TOP25FO_plants_inv[!is.na(TOP25FO_plants_inv$Average.annual.cost_damage),]) # 7 plant species has a damage cost
nrow(TOP25FO_plants_inv[!is.na(TOP25FO_plants_inv$Average.annual.cost_management),]) # 18 plant species has a management cost

TOP25FO_inv <- rbind(TOP25FO_mammals_inv[order(TOP25FO_mammals_inv$meanoriFtree, decreasing = TRUE),],
                     TOP25FO_birds_inv[order(TOP25FO_birds_inv$meanoriFtree, decreasing = TRUE),],
                     TOP25FO_plants_inv[order(TOP25FO_plants_inv$meanoriFtree, decreasing = TRUE),])

colnames(TOP25FO_inv)

table3 <- TOP25FO_inv[c("Species", "className", "meanoriFtree", "rank_meanoriFtree", "Average.annual.cost_damage", "Average.annual.cost_management")]
colnames(table3) <- c("Species", "Class", "Functional originality score", "Functional originality rank", "Damage cost", "Management cost")
write.csv2(table3, paste0(getwd(), "/outputs/table3.csv"))

## build a table with quantitative info about all species in InvaCost being either threatened and/or in TOP 25PO and/or in TOP25FO ----
quanti_all <- rbind(mammals, birds, plants) # all mammals, birds, and plants
quanti_inv <- quanti_all[which(quanti_all$invacostY=="Y"),] # mammals, birds, and plants in InvaCost
consparadox <- unique(c(threatened_inv$Species, TOP25PO_inv$Species, TOP25FO_inv$Species)) # list of species in InvaCost being either threatened and/or in TOP 25PO and/or in TOP25FO
quanti_consparadox <- quanti_inv[which(quanti_inv$Species %in% consparadox),]
colnames(quanti_consparadox)
quanti_consparadox <- quanti_consparadox[c("Species", "className", "Average.annual.cost_damage", "Average.annual.cost_management",
                                           "redlistCategory_version_2020.2",
                                           "oriPtree", "rank_oriPtree",
                                           "dietoriFtree", "activityoriFtree", "massoriFtree", "meanoriFtree", "rank_meanoriFtree")]
colnames(quanti_consparadox) <- c("Species", "Class", "Damage cost", "Management cost", 
                                  "Red List Category",
                                  "Phylogenetic originality score", "Phylogenetic originality rank",
                                  "Diet originality score", "Activity originality score", "Mass originality score", "Functional originality score", "Functional originality rank")

quanti_consparadox$threatened <- 0
quanti_consparadox$threatened[which(quanti_consparadox$Species %in% threatened_inv$Species)] <- 1

quanti_consparadox$TOP25PO <- 0
quanti_consparadox$TOP25PO[which(quanti_consparadox$Species %in% TOP25PO_inv$Species)] <- 1

quanti_consparadox$TOP25FO <- 0
quanti_consparadox$TOP25FO[which(quanti_consparadox$Species %in% TOP25FO_inv$Species)] <- 1

quanti_consparadox$N_criteria <- rowSums(quanti_consparadox[c("threatened", "TOP25PO", "TOP25FO")])

quanti_consparadox$Species[quanti_consparadox$N_criteria==2]

str(quanti_consparadox)

write.csv2(quanti_consparadox, paste0(getwd(), "/outputs/quanti_consparadox.csv"))

# quantitative information on mammal conservation paradoxes
quanti_consparadox_mammals <- quanti_consparadox[which(quanti_consparadox$Class=="MAMMALIA"),]
nrow(quanti_consparadox_mammals) # 25 mammal species constitute a conservation paradox, of which
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteria == 1 & quanti_consparadox_mammals$threatened == 1),]) # 5 are threatened only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteria == 1 & quanti_consparadox_mammals$TOP25PO == 1),]) # 5 belong to the TOP25% PO only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$N_criteria == 1 & quanti_consparadox_mammals$TOP25FO == 1),]) # 8 belong to the TOP25% FO only
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$threatened == 1 & quanti_consparadox_mammals$TOP25PO == 1),]) # 2 are both threatened and belong to the TOP25% PO
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$threatened == 1 & quanti_consparadox_mammals$TOP25FO == 1),]) # 0 are both threatened and belong to the TOP25% FO
nrow(quanti_consparadox_mammals[which(quanti_consparadox_mammals$TOP25PO == 1 & quanti_consparadox_mammals$TOP25FO == 1),]) # 5 both belong to the TOP25% PO and the TOP25% FO

# quantitative information on bird conservation paradoxes
quanti_consparadox_birds <- quanti_consparadox[which(quanti_consparadox$Class=="AVES"),]
nrow(quanti_consparadox_birds) # 11 bird species constitute a conservation paradox, of which
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteria == 1 & quanti_consparadox_birds$threatened == 1),]) # 0 are threatened only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteria == 1 & quanti_consparadox_birds$TOP25PO == 1),]) # 1 belongs to the TOP25% PO only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$N_criteria == 1 & quanti_consparadox_birds$TOP25FO == 1),]) # 9 belong to the TOP25% FO only
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$threatened == 1 & quanti_consparadox_birds$TOP25PO == 1),]) # 0 are both threatened and belong to the TOP25% PO
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$threatened == 1 & quanti_consparadox_birds$TOP25FO == 1),]) # 0 are both threatened and belong to the TOP25% FO
nrow(quanti_consparadox_birds[which(quanti_consparadox_birds$TOP25PO == 1 & quanti_consparadox_birds$TOP25FO == 1),]) # 1 both belong to the TOP25% PO and the TOP25% FO

# quantitative information on plant conservation paradoxes
quanti_consparadox_plants <- quanti_consparadox[which(quanti_consparadox$Class=="PLANTS"),]
nrow(quanti_consparadox_plants) # 90 plant species constitute a conservation paradox, of which
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteria == 1 & quanti_consparadox_plants$threatened == 1),]) # 2 are threatened only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteria == 1 & quanti_consparadox_plants$TOP25PO == 1),]) # 66 belong to the TOP25% PO only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$N_criteria == 1 & quanti_consparadox_plants$TOP25FO == 1),]) # 12 belong to the TOP25% FO only
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$threatened == 1 & quanti_consparadox_plants$TOP25PO == 1),]) # 0 are both threatened and belong to the TOP25% PO
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$threatened == 1 & quanti_consparadox_plants$TOP25FO == 1),]) # 0 are both threatened and belong to the TOP25% FO
nrow(quanti_consparadox_plants[which(quanti_consparadox_plants$TOP25PO == 1 & quanti_consparadox_plants$TOP25FO == 1),]) # 10 both belong to the TOP25% PO and the TOP25% FO



### Showing threat status and originality of species in InvaCost in in comparison of all species of their taxonomic group ----
## threat status ----
# mammals
Nb <- rep(1, length(mammals[,1]))
mammals$Nb <- Nb
mammals$invacostY[is.na(mammals$invacostY)] <- "No"
mammals$invacostY[which(mammals$invacostY =="Y")] <- "Yes"
mammals$invacostY <- factor(mammals$invacostY, levels = c("Yes", "No"))
mammals$redlistCategory_version_2020.2 <- factor(mammals$redlistCategory_version_2020.2, 
                                                       levels = c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered", 
                                                                  "Data Deficient"))

threatmammals <- ddply(mammals, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb), .drop = FALSE)

plot_threatmammals <- ggplot(data = threatmammals, aes(x = redlistCategory_version_2020.2, fill = invacostY, y = Number)) + 
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
Nb <- rep(1, length(birds[,1]))
birds$Nb <- Nb
birds$invacostY[which(is.na(birds$invacostY))] <- "No"
birds$invacostY[which(birds$invacostY =="Y")] <- "Yes"
birds$invacostY <- factor(birds$invacostY, levels = c("Yes", "No"))
birds$redlistCategory_version_2020.2 <- factor(birds$redlistCategory_version_2020.2, 
                                                     levels = c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered", "Data Deficient"))


threatbirds <- ddply(birds, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb), .drop = FALSE)

plot_threatbirds <- ggplot(data = threatbirds, aes(x = redlistCategory_version_2020.2, fill = invacostY, y = Number)) + 
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
Nb <- rep(1, length(plants[,1]))
plants$Nb <- Nb

plants$invacostY[which(plants$invacostY =="N")] <- "No"
plants$invacostY[which(plants$invacostY =="Y")] <- "Yes"
plants$invacostY <- factor(plants$invacostY, levels = c("Yes", "No"))
factor(plants$redlistCategory_version_2020.2)
plants$redlistCategory_version_2020.2 <- factor(plants$redlistCategory_version_2020.2, 
                                                      levels = c("Least Concern", "Near Threatened", "LR", "Vulnerable", "Endangered", "Critically Endangered", 
                                                                 "Data Deficient", "Unknown"))
levels(plants$redlistCategory_version_2020.2) <- c("Least Concern", "Near Threatened", "Lower Risk", "Vulnerable", "Endangered", "Critically Endangered", 
                                                         "Data Deficient", "Not assessed")

threatplants <- ddply(plants, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb), .drop = FALSE)

plot_threatplants <- ggplot(data = threatplants, aes(x = redlistCategory_version_2020.2, fill = invacostY, y = Number)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("PLANTS" ) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust = 1), axis.title.x = element_blank(), plot.title = element_text(size = 10))+
  guides(fill = guide_legend(title = "Presence in InvaCost"))+
  labs(y = "Number of species") +
  scale_fill_manual(values = c("black","grey")) +
  scale_y_continuous(trans = "sqrt")

plot_threatplants

## phylogenetic originality ----
# mammals
phylomammals <- mammals[!is.na(mammals$oriPtree),]
  
phylomammals$invacostY[which(phylomammals$invacostY == "N")] <- "No"
phylomammals$invacostY[which(phylomammals$invacostY =="Y")] <- "Yes"

plot_phylomammals <- ggplot(phylomammals, aes(x = oriPtree, y = "")) +
  geom_jitter(shape = 16, size = 1, color = "grey", position = position_jitter(0)) +
  geom_jitter(data = phylomammals[which(phylomammals$invacostY == "Yes"),], aes(x = oriPtree, y = ""),
              shape = 16, size = 1, color = "black", position = position_jitter(0)) +
  #geom_boxplot(alpha = 0.2, color = "purple", outlier.size = 1) +
  geom_vline(aes(xintercept = summary(oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_vline(aes(xintercept = summary(oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  theme_bw() +  theme(plot.title = element_text(size = 10)) +
  labs(x = "Phylogenetic originality (Ma)", y = "") +
  ggtitle("MAMMALS") +
  scale_x_continuous(trans = "log", breaks = c(5, 15, 25, 35, 45, 55)) +
  coord_flip()

plot_phylomammals

# birds
phylobirds <- birds[!is.na(birds$oriPtree),]

phylobirds$invacostY[is.na(phylobirds$invacostY)] <- "No"
phylobirds$invacostY[which(phylobirds$invacostY =="Y")] <- "Yes"

plot_phylobirds <- ggplot(phylobirds, aes(x = oriPtree, y = "")) +
  geom_jitter(shape = 16, size = 1, color = "grey", position = position_jitter(0)) +
  geom_jitter(data = phylobirds[which(phylobirds$invacostY == "Yes"),], aes(x = oriPtree, y = ""),
              shape = 16, size = 1, color = "black", position = position_jitter(0)) +
  #geom_boxplot(alpha = 0.2, color = "purple", outlier.size = 1) +
  geom_vline(aes(xintercept = summary(oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_vline(aes(xintercept = summary(oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  theme_bw() +  theme(plot.title = element_text(size = 10)) +
  labs(x = "Phylogenetic originality (Ma)", y = "") +
  ggtitle("BIRDS") +
  scale_x_continuous(trans = "log", breaks = c(5, 15, 25, 35, 45, 55)) +
  coord_flip()

plot_phylobirds

# plants
phyloplants <- plants[!is.na(plants$oriPtree),]

phyloplants$invacostY[which(phyloplants$invacostY == "N")] <- "No"
phyloplants$invacostY[which(phyloplants$invacostY =="Y")] <- "Yes"

summary(phyloplants$oriPtree)

plot_phyloplants <- ggplot(phyloplants, aes(x = oriPtree, y = "")) +
  geom_jitter(shape = 16, size = 1, color = "grey", position = position_jitter(0)) +
  geom_jitter(data = phyloplants[which(phyloplants$invacostY == "Yes"),], aes(x = oriPtree, y = ""),
              shape = 16, size = 1, color = "black", position = position_jitter(0)) +
  #geom_boxplot(alpha = 0.2, color = "purple", outlier.size = 1) +
  geom_vline(aes(xintercept = summary(oriPtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_vline(aes(xintercept = summary(oriPtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  theme_bw() +  theme(plot.title = element_text(size = 10)) +
  labs(x = "Phylogenetic originality (Ma)", y = "") +
  ggtitle("PLANTS") +
  scale_x_continuous(trans = "log", breaks = c(10^10, 10^11, 10^12, 10^13, 10^14, 10^15)) +
  coord_flip()

plot_phyloplants

## functional originality ----
# mammals
functiomammals <- mammals[!is.na(mammals$meanoriFtree),]

functiomammals$invacostY[which(functiomammals$invacostY == "N")] <- "No"
functiomammals$invacostY[which(functiomammals$invacostY =="Y")] <- "Yes"

summary(functiomammals$meanoriFtree)

plot_functiomammals <- ggplot(functiomammals, aes(x = meanoriFtree, y = "")) +
  geom_jitter(shape = 16, size = 1, color = "grey", position = position_jitter(0)) +
  geom_jitter(data = functiomammals[which(functiomammals$invacostY == "Yes"),], aes(x = meanoriFtree, y = ""),
              shape = 16, size = 1, color = "black", position = position_jitter(0)) +
  #geom_boxplot(alpha = 0.2, color = "purple", outlier.size = 1) +
  geom_vline(aes(xintercept = summary(meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_vline(aes(xintercept = summary(meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  theme_bw() +  theme(plot.title = element_text(size = 10)) +
  labs(x = "Functional originality", y = "") +
  ggtitle("MAMMALS") +
  scale_x_continuous(trans = "log", breaks = c(0.0001, 0.001, 0.01, 0.1), labels = c("0.0001", "0.001", "0.01", "0.1")) +
  coord_flip()

plot_functiomammals

# birds
functiobirds <- birds[!is.na(birds$meanoriFtree),]

functiobirds$invacostY[is.na(functiobirds$invacostY)] <- "No"
functiobirds$invacostY[which(functiobirds$invacostY =="Y")] <- "Yes"

plot_functiobirds <- ggplot(functiobirds, aes(x = meanoriFtree, y = "")) +
  geom_jitter(shape = 16, size = 1, color = "grey", position = position_jitter(0)) +
  geom_jitter(data = functiobirds[which(functiobirds$invacostY == "Yes"),], aes(x = meanoriFtree, y = ""),
              shape = 16, size = 1, color = "black", position = position_jitter(0)) +
  #geom_boxplot(alpha = 0.2, color = "purple", outlier.size = 1) +
  geom_vline(aes(xintercept = summary(meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_vline(aes(xintercept = summary(meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  theme_bw() +  theme(plot.title = element_text(size = 10)) +
  labs(x = "Functional originality", y = "") +
  ggtitle("BIRDS") +
  scale_x_continuous(trans = "log", breaks = c(0.0001, 0.001, 0.01, 0.1), labels = c("0.0001", "0.001", "0.01", "0.1")) +
  coord_flip()

plot_functiobirds

# plants
functioplants <- plants[!is.na(plants$meanoriFtree),]

functioplants$invacostY[which(functioplants$invacostY == "N")] <- "No"
functioplants$invacostY[which(functioplants$invacostY =="Y")] <- "Yes"

plot_functioplants <- ggplot(functioplants, aes(x = meanoriFtree, y = "")) +
  geom_jitter(shape = 16, size = 1, color = "grey", position = position_jitter(0)) +
  geom_jitter(data = functioplants[which(functioplants$invacostY == "Yes"),], aes(x = meanoriFtree, y = ""),
              shape = 16, size = 1, color = "black", position = position_jitter(0)) +
  #geom_boxplot(alpha = 0.2, color = "purple", outlier.size = 1) +
  geom_vline(aes(xintercept = summary(meanoriFtree)[3]), color= "red", size = 0.5, alpha = 0.4) +
  geom_vline(aes(xintercept = summary(meanoriFtree)[5]), color= "red", size = 0.5, alpha = 0.4, linetype = "twodash") +
  theme_bw() +  theme(plot.title = element_text(size = 10)) +
  labs(x = "Functional originality", y = "") +
  ggtitle("PLANTS") +
  scale_x_continuous(trans = "log", breaks = c(0.001, 0.01, 0.1, 1), labels = c("0.001", "0.01", "0.1", "1")) +
  coord_flip()

plot_functioplants

## complete figure ----
jpeg(paste0(getwd(), "/outputs/newFigure1.jpeg"), width = 7, height = 9, units = "in", res = 600)
plot_grid(plot_threatmammals + theme(legend.position = "none"), 
          plot_threatbirds + theme(legend.position = "none"), 
          plot_threatplants + theme(legend.position = "none"),
          plot_phylomammals, plot_phylobirds, plot_phyloplants,
          plot_functiomammals, plot_functiobirds, plot_functioplants,
          rel_widths = c(2, 2, 2), ncol = 3, nrow = 3, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"))
dev.off()
  
  
  
  

