#############################################################################################
# Script to run the analyses and make figures/tables of the paper
# original script by Marine Robuchon with contributions of Céline Bellard, Camille Bernery, 
# Vanessa Rezende, Gustavo Heringer & Cheikh Dia
#############################################################################################

### Load packages ----
library(dplyr)
library(ggplot2)
library(DataCombine)
library(ggpubr)
library(cowplot)


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
top5_mam_damage <- head(mam_damage[order(mam_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

bird_damage <- damage[which(damage$className=="AVES"),]
top5_bird_damage <- head(bird_damage[order(bird_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

plant_damage <- damage[which(damage$className=="PLANTS"),]
top5_plant_damage <- head(plant_damage[order(plant_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

# management
management <- data_all[-which(is.na(data_all$Average.annual.cost_management)),]

mam_management <- management[which(management$className=="MAMMALIA"),]
top5_mam_management <- head(mam_management[order(mam_management$Average.annual.cost_management, decreasing = TRUE),], 5)

bird_management <- management[which(management$className=="AVES"),]
top5_bird_management <- head(bird_management[order(bird_management$Average.annual.cost_management, decreasing = TRUE),], 5)

plant_management <- management[which(management$className=="PLANTS"),]
top5_plant_management <- head(plant_management[order(plant_management$Average.annual.cost_management, decreasing = TRUE),], 5)

## figure showing costs and threat status of the TOP 5 costliest species (by type of cost and taxon) ----
# damage
A_mammals <- ggplot(data = top5_mam_damage, aes(x = reorder(Species2, Average.annual.cost_damage), y = Average.annual.cost_damage, fill = redlistCategory_version_2020.2)) + 
  ggtitle("A  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Unknown", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) + 
  labs(fill = "Red List Category", x = "DAMAGE\nTOP 5 costliest species", y = "Average annual damage cost\n (2017 US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
A_mammals
  
C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(Species2, Average.annual.cost_damage), y = Average.annual.cost_damage, fill = redlistCategory_version_2020.2)) + 
  ggtitle("C  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Unknown", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "DAMAGE\nTOP 5 costliest species", y = "Average annual damage cost\n (2017 US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
C_birds

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(Species2, Average.annual.cost_damage), y = Average.annual.cost_damage, fill = redlistCategory_version_2020.2)) + 
  ggtitle("E PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Unknown", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "DAMAGE\nTOP 5 costliest species", y = "Average annual damage cost\n (2017 US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
E_plants
 
# management
B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(Species2, Average.annual.cost_management), y = Average.annual.cost_management, fill = redlistCategory_version_2020.2)) + 
  ggtitle("B  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Unknown", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "MANAGEMENT\nTOP 5 costliest species", y = "Average annual management cost\n (2017 US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
B_mammals

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(Species2, Average.annual.cost_management), y = Average.annual.cost_management, fill = redlistCategory_version_2020.2)) + 
  ggtitle("D  BIRDS") + geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(limits = c("Unknown", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) +
  labs(fill = "Red List Category", x = "MANAGEMENT\nTOP 5 costliest species", y = "Average annual management cost\n (2017 US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
D_birds

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(Species2, Average.annual.cost_management), y = Average.annual.cost_management, fill = redlistCategory_version_2020.2)) + 
  ggtitle("F PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  scale_fill_manual(limits = c("Unknown", "Least Concern", "Endangered"), values = c("darkgrey", "green4","orange2")) + 
  labs(fill = "Red List Category", x = "MANAGEMENT\nTOP 5 costliest species", y = "Average annual management cost\n (2017 US$)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  coord_flip()
F_plants

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom")
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
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/5425")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
A_mammals

birds <- data_all[which(data_all$className == "AVES"),]
summary(birds$oriPtree)[3]  # median
summary(birds$oriPtree)[5]  # 3rd quantile
birds$rank_oriPtree <- rank(-birds$oriPtree, na.last = "keep", ties.method = "average")
top5_bird_damage <- merge(top5_bird_damage, birds)

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("C  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriPtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(birds$oriPtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/8095")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
C_birds

plants <- data_all[which(data_all$className == "PLANTS"),]
summary(plants$oriPtree)[3]  # median
summary(plants$oriPtree)[5]  # 3rd quantile
plants$rank_oriPtree <- rank(-plants$oriPtree, na.last = "keep", ties.method = "average")
top5_plant_damage <- merge(top5_plant_damage, plants)

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("E PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriPtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(plants$oriPtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/356184")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
E_plants

# management
top5_mam_management <- merge(top5_mam_management, mammals)

B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("B  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(mammals$oriPtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/5425")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
B_mammals

top5_bird_management <- merge(top5_bird_management, birds)

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("D  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$oriPtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(birds$oriPtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/8095")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
D_birds

top5_plant_management <- merge(top5_plant_management, plants)

F_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(Species2, oriPtree), y = oriPtree)) + 
  ggtitle("F PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Phylogenetic originality (Ma)") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$oriPtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(plants$oriPtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_oriPtree, "\n/356184")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
F_plants

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom")
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
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/4798")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
A_mammals

summary(birds$meanoriFtree)[3]  # median
summary(birds$meanoriFtree)[5]  # 3rd quantile
birds$rank_meanoriFtree <- rank(-birds$meanoriFtree, na.last = "keep", ties.method = "average")
top5_bird_damage <- merge(top5_bird_damage, birds)

C_birds <- ggplot(data = top5_bird_damage, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("C  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/8095")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
C_birds

summary(plants$meanoriFtree)[3]  # median
summary(plants$meanoriFtree)[5]  # 3rd quantile
plants$rank_meanoriFtree <- rank(-plants$meanoriFtree, na.last = "keep", ties.method = "average")
top5_plant_damage <- merge(top5_plant_damage, plants)

E_plants <- ggplot(data = top5_plant_damage, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("E  PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "DAMAGE\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/212530")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
E_plants

# management
top5_mam_management <- merge(top5_mam_management, mammals)

B_mammals <- ggplot(data = top5_mam_management, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("B  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(mammals$meanoriFtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/4798")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
B_mammals


top5_bird_management <- merge(top5_bird_management, birds)

D_birds <- ggplot(data = top5_bird_management, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("D  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(birds$meanoriFtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/8095")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
D_birds


top5_plant_management <- merge(top5_plant_management, plants)

F_plants <- ggplot(data = top5_plant_management, aes(x = reorder(Species2, meanoriFtree), y = meanoriFtree)) + 
  ggtitle("F  PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
  labs(x = "MANAGEMENT\nTOP 5 costliest species", y = "Functional originality") + 
  theme(legend.title = element_text(color = "black", size = 7),legend.text = element_text(color = "black", size = 7),
        plot.title = element_text(color="black", size = 9), axis.text.y = element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black", size = 9), axis.title.x = element_text(size = 9, face = "bold"), 
        axis.title.y = element_text(size = 9, face = "bold")) + 
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[3]), color= "red", size = 1) +
  geom_hline(aes(yintercept = summary(plants$meanoriFtree)[5]), color= "red", size = 1, linetype = "dotted") +
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/212530")), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  coord_flip()
F_plants

# save figure
windows(6.85, 7)
ggarrange(A_mammals + rremove("x.title"), B_mammals + rremove("x.title"), C_birds + rremove("x.title"), D_birds + rremove("x.title"), E_plants, F_plants, 
          ncol = 2, nrow = 3 , heights = c(1, 1, 1), common.legend = TRUE, legend = "bottom")
dev.copy(png, file = paste0(getwd(), "/outputs/clean_fig3.png"), res = 600, height = 7, width = 6.85, units = "in")
dev.off()