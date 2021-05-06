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
  ggtitle("C  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
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
  ggtitle("E PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
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
  ggtitle("B  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
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
  ggtitle("D  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
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
  ggtitle("F PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
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
  geom_text(aes(label = paste0(rank_meanoriFtree, "\n/4798"), y = meanoriFtree + 0.0005), size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 0.006)) +
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
  ggtitle("E  PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
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
  ggtitle("B  MAMMALS") + geom_bar(stat="identity") + theme_minimal() + 
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
  ggtitle("D  BIRDS") + geom_bar(stat="identity") + theme_minimal() + 
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
  ggtitle("F  PLANTS") + geom_bar(stat="identity") + theme_minimal() + 
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
# 6 species of mammals are threatened and in InvaCost
# Oryctolagus cuniculus, EN, damage cost =  162.6236, management cost = 6.302057172
# Ammotragus lervia, VU, no damage cost, management cost = 0.002325734 
# Phascolarctos cinereus, VU, no damage cost, management cost =  0.007811451
# Rangifer tarandus, VU, no damage cost, management cost = 0.002031749
# Rusa timorensis, VU, no damage cost, management cost = 0.153711065
# Hemitragus jemlahicus, NT, no damage cost, maanagement cost = 0.447442081

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
# 9 species of mammals belonging to the TOP25 PO are also in InvaCost
# Phascolarctos cinereus, TOP 10 PO, no damage cost, management cost = 7.811451e-03
# Trichosurus vulpecula, TOP 348 PO, damage cost = 2.144539, management cost = 4.532871e+00
# Glis glis, TOP 623 PO, no damage cost, no management cost (but one publication and 2 cost evaluations - check why no annual cost)
# Hystrix brachyura, TOP 631 PO, no damage cost, management cost = 8.661140e-03
# Atelerix albiventris, TOP 699 PO, no damage cost, management cost = 2.999742e-05
# Nyctereutes procyonoides, TOP 1053 PO, no damage cost, management cost =  1.484021e-01
# Sus scrofa, TOP 1243 PO, damage cost = 114.832852, management cost = 1.494243e+00
# Rattus exulans, TOP 1252 PO, no damage cost, management cost = 1.545045e-02
# Myocastor coypus, TOP 1307 PO, damage cost = 126.268702, management cost = 1.510786e+00

TOP25PO_birds_inv <- TOP25PO_birds[which(TOP25PO_birds$invacostY=="Y"),]
TOP25PO_birds_inv[order(TOP25PO_birds_inv$oriPtree, decreasing = TRUE),]
# 2 species of birds belonging to the TOP25 PO are also in InvaCost
# Threskiornis aethiopicus, TOP PO 571, no damage cost, management cost = 0.00210740 
# Aquila chrysaetos, TOP PO 1909, no damage cost, management cost =  0.07180562

TOP25PO_plants_inv <- TOP25PO_plants[which(TOP25PO_plants$invacostY=="Y"),]
TOP25PO_plants_inv[order(TOP25PO_plants_inv$oriPtree, decreasing = TRUE),]
# 62 species of birds belonging to the TOP25 PO are also in InvaCost

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
# 12 species of mammals belonging to the TOP25 FO are also in InvaCost

TOP25FO_birds_inv <- TOP25FO_birds[which(TOP25FO_birds$invacostY=="Y"),]
TOP25FO_birds_inv[order(TOP25FO_birds_inv$meanoriFtree, decreasing = TRUE),]
# 10 species of birds belonging to the TOP25 FO are also in InvaCost

TOP25FO_plants_inv <- TOP25FO_plants[which(TOP25FO_plants$invacostY=="Y"),]
TOP25FO_plants_inv[order(TOP25FO_plants_inv$meanoriFtree, decreasing = TRUE),]
# 17 species of plants belonging to the TOP25 FO are also in InvaCost

TOP25FO_inv <- rbind(TOP25FO_mammals_inv[order(TOP25FO_mammals_inv$meanoriFtree, decreasing = TRUE),],
                     TOP25FO_birds_inv[order(TOP25FO_birds_inv$meanoriFtree, decreasing = TRUE),],
                     TOP25FO_plants_inv[order(TOP25FO_plants_inv$meanoriFtree, decreasing = TRUE),])

colnames(TOP25FO_inv)

table3 <- TOP25FO_inv[c("Species", "className", "meanoriFtree", "rank_meanoriFtree", "Average.annual.cost_damage", "Average.annual.cost_management")]
colnames(table3) <- c("Species", "Class", "Functional originality score", "Functional originality rank", "Damage cost", "Management cost")
write.csv2(table3, paste0(getwd(), "/outputs/table3.csv"))