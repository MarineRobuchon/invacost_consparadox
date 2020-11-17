library(ggplot2)
dataall<- read.table("data_plants2.txt", sep=";", header=T)
head(dataall)
###!Exotic<-filter(dataall, Exotic==TRUE)
###!Exoticplants<-filter(Exotic, className=="Plants")
Exotic<-subset(dataall, Exotic==TRUE)

#plants
###!Exoplantinvacost<-filter(Exoticplants,invacostY=="Y")
Exoplantinvacost<-subset(Exotic,invacostY=="Y")
#plants
###!Exoplantnotinvacost<-anti_join(Exoticplants, Exoplantinvacost)
library(dplyr)
Exoplantnotinvacost<-anti_join(Exotic, Exoplantinvacost)
Exoplantnotinvacost
#plants
mean(Exoplantinvacost$oriFtree, na.rm = T)
median(Exoplantinvacost$oriFtree, na.rm = T)
#plant
mean(Exoplantnotinvacost$oriFtree, na.rm = T)
median(Exoplantnotinvacost$oriFtree, na.rm = T)

#plant
mean(Exoplantinvacost$oriPtree, na.rm = T)
median(Exoplantinvacost$oriPtree, na.rm = T)
#plant
mean(Exoplantnotinvacost$oriPtree, na.rm = T)
median(Exoplantnotinvacost$oriPtree, na.rm = T)

library(ggpubr)
#Plants
##FUNCTIONAL ORI
#Normality test for each modality
shapiro.test(Exoplantinvacost$oriFtree) ##invacostY=Y
ggqqplot(Exoplantinvacost$oriFtree)
shapiro.test(Exoplantnotinvacost$oriFtree)
ggqqplot(Exoplantnotinvacost$oriFtree)
#####PAS NORMALES --> Test U de mann whitney
plantsFUNC<-wilcox.test(Exoplantinvacost$oriFtree,
                        Exoplantnotinvacost$oriFtree)
plantsFUNC  #pas de différences

##PHYLO ORI
shapiro.test(Exoplantinvacost$oriPtree)
ggqqplot(Exoplantinvacost$oriPtree)
shapiro.test(Exoplantnotinvacost$oriPtree)
ggqqplot(Exoplantnotinvacost$oriPtree)
#####PAS NORMALES --> Test U de mann whitney
plantsPHYLO<-wilcox.test(Exoplantinvacost$oriPtree,
                         Exoplantnotinvacost$oriPtree)
plantsPHYLO  #Pas de différences entre les deux groupes

#plants
###!ftreeplants <- ggplot(Exoticplants, aes(x=log(oriFtree), y=invacostY)) +
###!  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
###!  geom_boxplot(alpha = 0.5)+
###!  scale_y_discrete(labels=c("Yes", "No")) +
###!  labs(y="Presence in InvaCost", x="Functional originality (log)")+
###!  ggtitle("PLANTS" ) +
###!  theme_bw()+  theme(plot.title = element_text(size=11))+
###!  coord_flip()

###! ftreeplants
library(cowplot)
###phylogenetic originality####
#Tree

#plants
ptreeplants <- ggplot(Exotic, aes(x=log(oriPtree), y=invacostY)) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  labs(y="Presence in InvaCost", x="Phylogenetic originality (log)")+
  ggtitle("PLANTS" ) +
  guides(fill=guide_legend(title="Exotic status"))+
  theme_bw()+  theme(plot.title = element_text(size=11))+
  coord_flip()

ptreeplants

#plants

Exotic$invacostY[is.na(Exotic$invacostY)]<-"N"
Nb<-rep(1, length(Exotic[,1]))
Exotic$Nb<-Nb
library(plyr)
threatplants<-ddply(Exotic, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb))


library(forcats)
library(dplyr)
library(ggplot2)
threatplants <- threatplants %>% # Reorder following a precise order
  mutate(redlistCategory_version_2020.2 = fct_relevel(redlistCategory_version_2020.2, "Least Concern","Near Threatened","Vulnerable","Endangered", "Critically Endangered")) %>%
  ggplot(aes(x=redlistCategory_version_2020.2, fill=invacostY, y=Number)) +
  geom_jitter(shape=16, size=1, color="black", position=position_jitter(0))+
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("THREAT- PLANTS" ) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  guides(fill=guide_legend(title="Presence in InvaCost"))+
  labs(y="Number of species")+
  scale_fill_manual(values=c("black","grey"))
#scale_fill_discrete(labels=c("Yes","No"))
threatplants

ptreeplants
dev.copy(png, file="treeplantsbox.png", res=200, height=3.3, width=4.4, units="in")
dev.off()

threatplants
dev.copy(png, file="threatplantsbar.png", res=200, height=3.3, width=4.4, units="in")
dev.off()