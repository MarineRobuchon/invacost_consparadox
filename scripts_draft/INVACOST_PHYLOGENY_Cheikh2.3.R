# INVACOST_PHYLOGENY_Cheikh2.3
library(dplyr)
library(ggplot2)
library(DataCombine)
library(ggpubr)
library(cowplot)

##############
#### Load data

setwd("C:/Gustavo/Invacost/Filogenia/figuras para rodar")
dataall <- read.table("dataAllF.csv", sep=",", header=T) # database for birds and mammals ##!##
head(dataall)
str(dataall$invacostY)


##################
#### Organize data

dataall$invacostY[which(is.na(dataall$invacostY))] <- "N"
dataall$invacostY <- factor(dataall$invacostY, levels = c("Y", "N"))
levels(dataall$invacostY)

dataall$Exotic[which(dataall$Exotic == "FALSO")] <- "FALSE" 
dataall$Exotic[which(dataall$Exotic == "VERDADEIRO")] <- "TRUE" 

dataplants <- read.table("dataplants2_NA.csv",sep=",", header=T) # database for plants ##!##
head(dataplants)
str(dataplants$invacostY)
dataplants$invacostY[which(is.na(dataplants$invacostY))] <- "N"
dataplants$invacostY <- factor(dataplants$invacostY, levels = c("Y", "N"))
levels(dataplants$invacostY)

birdsmammals_invacost <- dataall[which(dataall$invacostY=="Y"),]
colnames(birdsmammals_invacost)
plants_invacost <- dataplants[which(dataplants$invacostY=="Y"),]
plants_invacost$className <- "PLANTS"
colnames(plants_invacost)

commoncol <- intersect(colnames(plants_invacost), colnames(birdsmammals_invacost)) 

birdsmammals_invacost <- birdsmammals_invacost[, commoncol]
plants_invacost <- plants_invacost[, commoncol]
dataAllFInvacost <- rbind(birdsmammals_invacost, plants_invacost) 
# add the columns of the different functional originalities for birds & mammals
dataAllFInvacost <- merge(dataAllFInvacost, dataall[, c("Species", "dietoriFtree", "activityoriFtree", "massoriFtree")], by = "Species", all.x = TRUE)
head(dataAllFInvacost)
str(dataAllFInvacost)
summary(dataAllFInvacost)

write.table(dataAllFInvacost, "datallFInvacost.csv")

invmammals<-filter(dataAllFInvacost, className=="MAMMALIA")
invbirds<-filter(dataAllFInvacost, className=="AVES")
invplants<-filter(dataAllFInvacost, className=="PLANTS")

TAB<-dataAllFInvacost


#------------------DATASET MAMMALS-----------------------------------------------
TAB1=subset(TAB, subset = className == "MAMMALIA")

MAM_DAM_ORD <- TAB1[order(TAB1$Average.annual.cost_damage, decreasing = T),]
MAM_DAM_ORD_top5 <- MAM_DAM_ORD[1:5,]
#-MAM_DAM_ORD_top10 <- MAM_DAM_ORD[1:10,]

MAM_MAN_ORD <- TAB1[order(TAB1$Average.annual.cost_management, decreasing = T),]
MAM_MAN_ORD_top5 <- MAM_MAN_ORD[1:5,]
#-MAM_MAN_ORD_top10 <- MAM_MAN_ORD[1:10,]

#------------------DATASET BIRDS-----------------------------------------------
TAB2=subset(TAB, subset = className == "AVES")

AVES_DAM_ORD <- TAB2[order(TAB2$Average.annual.cost_damage, decreasing = T),]
AVES_DAM_ORD_top5 <- AVES_DAM_ORD[1:5,]
#-AVES_DAM_ORD_top10 <- AVES_DAM_ORD[1:10,]

AVES_MAN_ORD <- TAB2[order(TAB2$Average.annual.cost_management, decreasing = T),]
AVES_MAN_ORD_top5 <- AVES_MAN_ORD[1:5,]
#-AVES_MAN_ORD_top10 <- AVES_MAN_ORD[1:10,]


# PLANTS ----------
TAB3=subset(TAB, subset = className == "PLANTS")

PLANTS_DAM_ORD <- TAB3[order(TAB3$Average.annual.cost_damage, decreasing = T),]
PLANTS_DAM_ORD_top5 <- PLANTS_DAM_ORD[1:5,]
#-PLANTS_DAM_ORD_top10 <- PLANTS_DAM_ORD[1:10,]

PLANTS_MAN_ORD <- TAB3[order(TAB3$Average.annual.cost_management, decreasing = T),]
PLANTS_MAN_ORD_top5 <- PLANTS_MAN_ORD[1:5,]
#-PLANTS_MAN_ORD_top10 <- PLANTS_MAN_ORD[1:10,]


###################
#### Create figures

##!## Abbreviated species names #### ####

MAM_DAM_ORD_top5 
MAM_DAM_ORD_top5$Species2 <- MAM_DAM_ORD_top5$Species
MAM_DAM_ORD_top5$Species2 <- gsub(" ", "\n", MAM_DAM_ORD_top5$Species2)
#-gen.abr.m.d <- substr(MAM_DAM_ORD_top5$Species, 1, 1)
#-ept.m.d <- gsub(".* ","", MAM_DAM_ORD_top5$Species)
#-MAM_DAM_ORD_top5$Species2 <- paste(gen.abr.m.d, ept.m.d, sep = ". ")

#-MAM_DAM_ORD_top10 
#-gen.abr.m.d <- substr(MAM_DAM_ORD_top10$Species, 1, 1)
#-ept.m.d <- gsub(".* ","",MAM_DAM_ORD_top10$Species)
#-MAM_DAM_ORD_top10$Species2 <- paste(gen.abr.m.d, ept.m.d, sep = ". ")

MAM_MAN_ORD_top5
MAM_MAN_ORD_top5$Species2 <- MAM_MAN_ORD_top5$Species
MAM_MAN_ORD_top5$Species2 <- gsub(" ", "\n", MAM_MAN_ORD_top5$Species2)
#-gen.abr.m.m <- substr(MAM_MAN_ORD_top5$Species, 1, 1)
#-ept.m.m <- gsub(".* ","",MAM_MAN_ORD_top5$Species)
#-MAM_MAN_ORD_top5$Species2 <- paste(gen.abr.m.m, ept.m.m, sep = ". ")

#-MAM_MAN_ORD_top10
#-gen.abr.m.m <- substr(MAM_MAN_ORD_top10$Species, 1, 1)
#-ept.m.m <- gsub(".* ","",MAM_MAN_ORD_top10$Species)
#-MAM_MAN_ORD_top10$Species2 <- paste(gen.abr.m.m, ept.m.m, sep = ". ")

AVES_DAM_ORD_top5
AVES_DAM_ORD_top5$Species2 <- AVES_DAM_ORD_top5$Species
AVES_DAM_ORD_top5$Species2 <- gsub(" ", "\n", AVES_DAM_ORD_top5$Species2)
#-gen.abr.a.d <- substr(AVES_DAM_ORD_top5$Species, 1, 1)
#-ept.a.d <- gsub(".* ","",AVES_DAM_ORD_top5$Species)
#-AVES_DAM_ORD_top5$Species2 <- paste(gen.abr.a.d, ept.a.d, sep = ". ")

#-AVES_DAM_ORD_top10
#-gen.abr.a.d <- substr(AVES_DAM_ORD_top10$Species, 1, 1)
#-ept.a.d <- gsub(".* ","",AVES_DAM_ORD_top10$Species)
#-AVES_DAM_ORD_top10$Species2 <- paste(gen.abr.a.d, ept.a.d, sep = ". ")

AVES_MAN_ORD_top5
AVES_MAN_ORD_top5$Species2 <- AVES_MAN_ORD_top5$Species
AVES_MAN_ORD_top5$Species2 <- gsub(" ", "\n", AVES_MAN_ORD_top5$Species2)
#-gen.abr.a.m <- substr(AVES_MAN_ORD_top5$Species, 1, 1)
#-ept.a.m <- gsub(".* ","",AVES_MAN_ORD_top5$Species)
#-AVES_MAN_ORD_top5$Species2 <- paste(gen.abr.a.m, ept.a.m, sep = ". ")

#-AVES_MAN_ORD_top10
#-gen.abr.a.m <- substr(AVES_MAN_ORD_top10$Species, 1, 1)
#-ept.a.m <- gsub(".* ","",AVES_MAN_ORD_top10$Species)
#-AVES_MAN_ORD_top10$Species2 <- paste(gen.abr.a.m, ept.a.m, sep = ". ")


PLANTS_MAN_ORD_top5
PLANTS_MAN_ORD_top5$Species2 <- PLANTS_MAN_ORD_top5$Species
PLANTS_MAN_ORD_top5$Species2 <- gsub(" ", "\n", PLANTS_MAN_ORD_top5$Species2)
#-gen.abr.p.m <- substr(PLANTS_MAN_ORD_top5$Species, 1, 1)
#-ept.p.m <- gsub(".* ","",PLANTS_MAN_ORD_top5$Species)
#-PLANTS_MAN_ORD_top5$Species2 <- paste(gen.abr.p.m, ept.p.m, sep = ". ")

#-PLANTS_MAN_ORD_top10
#-gen.abr.p.m <- substr(PLANTS_MAN_ORD_top10$Species, 1, 1)
#-ept.p.m <- gsub(".* ","",PLANTS_MAN_ORD_top10$Species)
#-PLANTS_MAN_ORD_top10$Species2 <- paste(gen.abr.p.m, ept.p.m, sep = ". ")

PLANTS_DAM_ORD_top5
PLANTS_DAM_ORD_top5$Species2 <- PLANTS_DAM_ORD_top5$Species
PLANTS_DAM_ORD_top5$Species2 <- gsub(" ", "\n", PLANTS_DAM_ORD_top5$Species2)
#-gen.abr.p.d <- substr(PLANTS_DAM_ORD_top5$Species, 1, 1)
#-ept.p.d <- gsub(".* ","",PLANTS_DAM_ORD_top5$Species)
#-PLANTS_DAM_ORD_top5$Species2 <- paste(gen.abr.p.d, ept.p.d, sep = ". ")

#-PLANTS_DAM_ORD_top10
#-gen.abr.p.d <- substr(PLANTS_DAM_ORD_top10$Species, 1, 1)
#-ept.p.d <- gsub(".* ","",PLANTS_DAM_ORD_top10$Species)
#-PLANTS_DAM_ORD_top10$Species2 <- paste(gen.abr.p.d, ept.p.d, sep = ". ")


#################
#### Top5 version

##Mammals / Phylogenetic
p1=ggplot(data=MAM_MAN_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic",size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red", size=1)+
  coord_flip()
p1

m.m.ph.top5 <- p1

m.m.ph.top5

##AVES / Phylogenetic
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1) +
  coord_flip()
p1

a.m.ph.top5 <- p1

a.m.ph.top5

##PLANTS / Phylogenetic
p1=ggplot(data=PLANTS_MAN_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="C  PLANTS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
  coord_flip()
p1

p.m.ph.top5 <- p1

p.m.ph.top5

##Mammals / Functional

p1=ggplot(data=MAM_MAN_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="D MAMMALS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

m.m.f.top5 <- p1

m.m.f.top5

##AVES / Functional
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="E BIRDS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB2$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

a.m.f.top5 <- p1

a.m.f.top5


##PLANTS / Functional
p1=ggplot(data=PLANTS_MAN_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="F PLANTS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
  coord_flip()
p1

p.m.f.top5 <- p1 #

p.m.f.top5

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_MAN_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("D  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4", "green")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7), legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"),
                                                                                                       axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

m.m.th.top5 <- p1_4

m.m.th.top5


##-----AVES------
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("E  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1

p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
p1_4

a.m.th.top5 <- p1_4

a.m.th.top5

##-----PLANTS------

PLANTS_MAN_ORD_top5

PLANTS_MAN_ORD_top5_2 <- PLANTS_MAN_ORD_top5 
PLANTS_MAN_ORD_top5_2$redlistCategory_version_2020.2[is.na(PLANTS_MAN_ORD_top5_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

p1=ggplot(data=PLANTS_MAN_ORD_top5_2, aes(x= reorder(Species2, Average.annual.cost_management), y= Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("F  PLANTS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1

p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
p1_4

p.m.th.top5 <- p1_4

p.m.th.top5 


#----------FIGURE 4 : Damage-----------
##Mammals / Phylogenetic
p1=ggplot(data=MAM_DAM_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red",size=1)+
  coord_flip()
p1

m.d.ph.top5 <- p1

m.d.ph.top5

##AVES / Phylogenetic
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1)+
  coord_flip()
p1

a.d.ph.top5 <- p1

a.d.ph.top5

##PLANTS / Phylogenetic
p1=ggplot(data=PLANTS_DAM_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="C PLANTS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
  coord_flip()
p1

p.d.ph.top5 <- p1

p.d.ph.top5


##Mammals / Functional

p1=ggplot(data=MAM_DAM_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="D MAMMALS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

m.d.f.top5 <- p1

m.d.f.top5 


##AVES / Functional
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="E BIRDS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(TAB2$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

a.d.f.top5 <- p1

a.d.f.top5 


##PLANTS / Functional

p1=ggplot(data=PLANTS_DAM_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="F PLANTS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
  coord_flip()
p1

p.d.f.top5 <- p1

p.d.f.top5 


#Figure 4

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_DAM_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("A  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

m.d.th.top5 <- p1_4 

m.d.th.top5 


##-----AVES------
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("B  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

a.d.th.top5 <- p1_4 

a.d.th.top5 


##-----PLANTS------
PLANTS_DAM_ORD_top5_2 <- PLANTS_DAM_ORD_top5
PLANTS_DAM_ORD_top5_2$redlistCategory_version_2020.2[is.na(PLANTS_DAM_ORD_top5_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

p1=ggplot(data=PLANTS_DAM_ORD_top5_2, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("I  PLANTS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

p.d.th.top5 <- p1_4

p.d.th.top5


#################
#### Save figures

#Figura 4 (6.85)
windows(6.85, 7)

ggarrange(m.d.ph.top5, m.d.f.top5, a.d.ph.top5, a.d.f.top5, p.d.ph.top5, p.d.f.top5 + rremove("x.text") , ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/damage_top5.png", res=400, height=7, width=6.85, units="in")
dev.off()

#Figura 5 p.m.f.2.v3
windows(6.85, 7)

ggarrange(m.m.ph.top5, m.m.f.top5, a.m.ph.top5, a.m.f.top5, p.m.ph.top5, p.m.f.top5 + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/management_top5.png", res=400, height=7, width=6.85, units="in")
dev.off()

m.d.th.top5b <- m.d.th.top5+theme(legend.position = c(.7, .4))
m.m.th.top5b <- m.m.th.top5+theme(legend.position = c(.7, .4))
a.d.th.top5b <- a.d.th.top5+theme(legend.position = c(.7, .4))
a.m.th.top5b <- a.m.th.top5+theme(legend.position = c(.7, .4))
p.d.th.top5b <- p.d.th.top5+theme(legend.position = c(.7, .4))
p.m.th.top5b <- p.m.th.top5+theme(legend.position = c(.7, .4))

windows(6.85, 7)

ggarrange(m.d.th.top5b, m.m.th.top5b, a.d.th.top5b, a.m.th.top5b, p.d.th.top5b, p.m.th.top5b + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/threatened_top5.png", res=400, height=7, width=6.85, units="in")
dev.off()


#save(list = ls(all.names = TRUE), file = "C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/result_top5_full_name.RData")
#dev.off()


#################
#### Top5 version 2



##Mammals / Phylogenetic
p1=ggplot(data=MAM_MAN_ORD_top5, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic",size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red", size=1)+
  coord_flip()
p1

m.m.ph.top5 <- p1

m.m.ph.top5

##AVES / Phylogenetic
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1) +
  coord_flip()
p1

a.m.ph.top5 <- p1

a.m.ph.top5

##PLANTS / Phylogenetic
p1=ggplot(data=PLANTS_MAN_ORD_top5, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="C  PLANTS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
  coord_flip()
p1

p.m.ph.top5 <- p1

p.m.ph.top5

##Mammals / Functional

p1=ggplot(data=MAM_MAN_ORD_top5, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="D MAMMALS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

m.m.f.top5 <- p1

m.m.f.top5

##AVES / Functional
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="E BIRDS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB2$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

a.m.f.top5 <- p1

a.m.f.top5


##PLANTS / Functional
p1=ggplot(data=PLANTS_MAN_ORD_top5, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="F PLANTS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
  coord_flip()
p1

p.m.f.top5 <- p1 #

p.m.f.top5

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_MAN_ORD_top5, aes(x=reorder(Species, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("D  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4", "green")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7), legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"),
                                                                                                       axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

m.m.th.top5 <- p1_4

m.m.th.top5


##-----AVES------
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x=reorder(Species, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("E  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1

p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
p1_4

a.m.th.top5 <- p1_4

a.m.th.top5

##-----PLANTS------

PLANTS_MAN_ORD_top5

PLANTS_MAN_ORD_top5_2 <- PLANTS_MAN_ORD_top5 
PLANTS_MAN_ORD_top5_2$redlistCategory_version_2020.2[is.na(PLANTS_MAN_ORD_top5_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

p1=ggplot(data=PLANTS_MAN_ORD_top5_2, aes(x= reorder(Species, Average.annual.cost_management), y= Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("F  PLANTS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1

p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
p1_4

p.m.th.top5 <- p1_4

p.m.th.top5 


#----------FIGURE 4 : Damage-----------
##Mammals / Phylogenetic
p1=ggplot(data=MAM_DAM_ORD_top5, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red",size=1)+
  coord_flip()
p1

m.d.ph.top5 <- p1

m.d.ph.top5

##AVES / Phylogenetic
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1)+
  coord_flip()
p1

a.d.ph.top5 <- p1

a.d.ph.top5

##PLANTS / Phylogenetic
p1=ggplot(data=PLANTS_DAM_ORD_top5, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="C PLANTS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
  coord_flip()
p1

p.d.ph.top5 <- p1

p.d.ph.top5


##Mammals / Functional

p1=ggplot(data=MAM_DAM_ORD_top5, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="D MAMMALS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

m.d.f.top5 <- p1

m.d.f.top5 


##AVES / Functional
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="E BIRDS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(TAB2$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

a.d.f.top5 <- p1

a.d.f.top5 


##PLANTS / Functional

p1=ggplot(data=PLANTS_DAM_ORD_top5, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="F PLANTS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
  coord_flip()
p1

p.d.f.top5 <- p1

p.d.f.top5 


#Figure 4

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_DAM_ORD_top5, aes(x=reorder(Species, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("A  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

m.d.th.top5 <- p1_4 

m.d.th.top5 


##-----AVES------
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x=reorder(Species, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("B  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

a.d.th.top5 <- p1_4 

a.d.th.top5 


##-----PLANTS------
PLANTS_DAM_ORD_top5_2 <- PLANTS_DAM_ORD_top5
PLANTS_DAM_ORD_top5_2$redlistCategory_version_2020.2[is.na(PLANTS_DAM_ORD_top5_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

p1=ggplot(data=PLANTS_DAM_ORD_top5_2, aes(x=reorder(Species, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("I  PLANTS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

p.d.th.top5 <- p1_4

p.d.th.top5


#################
#### Save figures

#Figura 4 (6.85)
windows(6.85, 7)

ggarrange(m.d.ph.top5, m.d.f.top5, a.d.ph.top5, a.d.f.top5, p.d.ph.top5, p.d.f.top5 + rremove("x.text") , ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/damage_top5b.png", res=400, height=7, width=6.85, units="in")
dev.off()

#Figura 5 p.m.f.2.v3
windows(6.85, 7)

ggarrange(m.m.ph.top5, m.m.f.top5, a.m.ph.top5, a.m.f.top5, p.m.ph.top5, p.m.f.top5 + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/management_top5b.png", res=400, height=7, width=6.85, units="in")
dev.off()

m.d.th.top5b <- m.d.th.top5+theme(legend.position = c(.7, .4))
m.m.th.top5b <- m.m.th.top5+theme(legend.position = c(.7, .4))
a.d.th.top5b <- a.d.th.top5+theme(legend.position = c(.7, .4))
a.m.th.top5b <- a.m.th.top5+theme(legend.position = c(.7, .4))
p.d.th.top5b <- p.d.th.top5+theme(legend.position = c(.7, .4))
p.m.th.top5b <- p.m.th.top5+theme(legend.position = c(.7, .4))

windows(6.85, 7)

ggarrange(m.d.th.top5b, m.m.th.top5b, a.d.th.top5b, a.m.th.top5b, p.d.th.top5b, p.m.th.top5b + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/threatened_top5b.png", res=400, height=7, width=6.85, units="in")
dev.off()


#save(list = ls(all.names = TRUE), file = "C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/result_top5_full_name_b.RData")
#dev.off()


########### Deletar o que estivver abaixo assim que termnar
##
###
##
####


##Mammals / Phylogenetic
#-p1=ggplot(data=MAM_MAN_ORD_top10, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic",size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="A MAMMALS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
#-  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red", size=1)+
#-  coord_flip()
#-p1

#-m.m.ph.top10 <- p1
#-m.m.ph.top10

##AVES / Phylogenetic
#-p1=ggplot(data=AVES_MAN_ORD_top10, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="B  BIRDS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
#-  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1) +
#-  coord_flip()
#-p1

#-a.m.ph.top10 <- p1
#-a.m.ph.top10

##PLANTS / Phylogenetic
#-p1=ggplot(data=PLANTS_MAN_ORD_top10, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="C  PLANTS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
#-  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-p.m.ph.top10 <- p1
#-p.m.ph.top10

##Mammals / Functional

#-p1=ggplot(data=MAM_MAN_ORD_top10, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="D MAMMALS", x="Most costly Species (Management)", y = "Functional originality scores")+
#-  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-m.m.f.top10 <- p1
#-m.m.f.top10

##AVES / Functional
#-p1=ggplot(data=AVES_MAN_ORD_top10, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="E BIRDS", x="Most costly Species (Management)", y = "Functional originality scores")+
#-  geom_hline(aes(yintercept=median(TAB2$meanoriFtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-a.m.f.top10 <- p1
#-a.m.f.top10


##PLANTS / Functional
#-p1=ggplot(data=PLANTS_MAN_ORD_top10, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="F PLANTS", x="Most costly Species (Management)", y = "Functional originality scores")+
#-  geom_hline(aes(yintercept=median(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
#-  coord_flip()
#-p1

#-p.m.f.top10 <- p1 #
#-p.m.f.top10

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
#-p1=ggplot(data=MAM_MAN_ORD_top10, aes(x=reorder(Species2, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("D  MAMMALS") +
#-  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4", "green")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7), legend.text = element_text(color = "black", size = 7)) + 
#-  theme(plot.title = element_text(color="black", size = 9))
#-p1
#-p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
#-p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"),
#-                   axis.title.y = element_text(size = 9, face = "bold"))
#-p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

#-m.m.th.top10 <- p1_4
#-m.m.th.top10


##-----AVES------
#-p1=ggplot(data=AVES_MAN_ORD_top10, aes(x=reorder(Species2, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("E  BIRDS") +
#-  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
#-  theme(plot.title = element_text(color="black", size = 9))
#-p1

#-p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=7))
#-p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
#-p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
#-p1_4

#-a.m.th.top10 <- p1_4
#-a.m.th.top10

##-----PLANTS------

#-PLANTS_MAN_ORD_top10

#-PLANTS_MAN_ORD_top10_2 <- PLANTS_MAN_ORD_top10 
#-PLANTS_MAN_ORD_top10_2$redlistCategory_version_2020.2[is.na(PLANTS_MAN_ORD_top10_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

#-p1=ggplot(data=PLANTS_MAN_ORD_top10_2, aes(x= reorder(Species2, Average.annual.cost_management), y= Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("F  PLANTS") +
#-  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
#-  theme(plot.title = element_text(color="black", size = 9))
#-p1

#-p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
#-p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
#-p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
#-p1_4

#-p.m.th.top10 <- p1_4
#-p.m.th.top10 


#----------FIGURE 4 : Damage-----------
##Mammals / Phylogenetic
#-p1=ggplot(data=MAM_DAM_ORD_top10, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="A MAMMALS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
#-  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-m.d.ph.top10 <- p1
#-m.d.ph.top10

##AVES / Phylogenetic
#-p1=ggplot(data=AVES_DAM_ORD_top10, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="B  BIRDS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
#-  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-a.d.ph.top10 <- p1
#-a.d.ph.top10

##PLANTS / Phylogenetic
#-p1=ggplot(data=PLANTS_DAM_ORD_top10, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="C PLANTS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
#-  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-p.d.ph.top10 <- p1
#-p.d.ph.top10


##Mammals / Functional

#-p1=ggplot(data=MAM_DAM_ORD_top10, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="D MAMMALS", x="Most costly Species (Damage)", y = "Functional originality scores")+
#-  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-m.d.f.top10 <- p1
#-m.d.f.top10 


##AVES / Functional
#-p1=ggplot(data=AVES_DAM_ORD_top10, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-        axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="E BIRDS", x="Most costly Species (Damage)", y = "Functional originality scores")+
#-  geom_hline(aes(yintercept=mean(TAB2$meanoriFtree)), color="red",size=1)+
#-  coord_flip()
#-p1

#-a.d.f.top10 <- p1
#-a.d.f.top10 


##PLANTS / Functional

#-p1=ggplot(data=PLANTS_MAN_ORD_top10, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
#-  geom_bar(stat="identity", fill="gray30") +
#-  theme(plot.title = element_text(color="black", size = 9),
#-        axis.text.y= element_text(face="bold.italic", size = 7),
#-       axis.text.x = element_text(face = "bold", color = "black",size = 9),
#-        axis.title.x = element_text(size = 9, face = "bold"),
#-        axis.title.y = element_text(size = 9, face = "bold"),
#-        legend.position="bottom")+
#-  labs(title="F PLANTS", x="Most costly Species (Damage)", y = "Functional originality scores")+
#-  geom_hline(aes(yintercept=mean(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
#-  coord_flip()
#-p1

#-p.d.f.top10 <- p1
#-p.d.f.top10 


#Figure 4

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_DAM_ORD_top10, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("A  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

m.d.th.top10 <- p1_4 

m.d.th.top10 


##-----AVES------
p1=ggplot(data=AVES_DAM_ORD_top10, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("B  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

a.d.th.top10 <- p1_4 

a.d.th.top10 


##-----PLANTS------
PLANTS_DAM_ORD_top10_2 <- PLANTS_DAM_ORD_top10
PLANTS_DAM_ORD_top10_2$redlistCategory_version_2020.2[is.na(PLANTS_DAM_ORD_top10_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

p1=ggplot(data=PLANTS_DAM_ORD_top10_2, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("I  PLANTS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

p.d.th.top10 <- p1_4

p.d.th.top10


#################
#### Save figures

#Figura 4 (6.85)
windows(6.85, 7)

ggarrange(m.d.ph.top10, m.d.f.top10, a.d.ph.top10, a.d.f.top10, p.d.ph.top10, p.d.f.top10 + rremove("x.text") , ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/damage_top10.png", res=400, height=7, width=6.85, units="in")
dev.off()

#Figura 5 p.m.f.2.v3
windows(6.85, 7)

ggarrange(m.m.ph.top10, m.m.f.top10, a.m.ph.top10, a.m.f.top10, p.m.ph.top10, p.m.f.top10 + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/management_top10.png", res=400, height=7, width=6.85, units="in")
dev.off()

m.d.th.top10b <- m.d.th.top10+theme(legend.position = c(.7, .4))
m.m.th.top10b <- m.m.th.top10+theme(legend.position = c(.7, .4))
a.d.th.top10b <- a.d.th.top10+theme(legend.position = c(.7, .4))
a.m.th.top10b <- a.m.th.top10+theme(legend.position = c(.7, .4))
p.d.th.top10b <- p.d.th.top10+theme(legend.position = c(.7, .4))
p.m.th.top10b <- p.m.th.top10+theme(legend.position = c(.7, .4))

windows(6.85, 7)

ggarrange(m.d.th.top10b, m.m.th.top10b, a.d.th.top10b, a.m.th.top10b, p.d.th.top10b, p.m.th.top10b + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/threatened_top10.png", res=400, height=7, width=6.85, units="in")
dev.off()


#################
#### Top5 version

##Mammals / Phylogenetic
p1=ggplot(data=MAM_MAN_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic",size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red", size=1)+
  coord_flip()
p1

m.m.ph.top5 <- p1

m.m.ph.top5

##AVES / Phylogenetic
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1) +
  coord_flip()
p1

a.m.ph.top5 <- p1

a.m.ph.top5

##PLANTS / Phylogenetic
p1=ggplot(data=PLANTS_MAN_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="C  PLANTS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
  coord_flip()
p1

p.m.ph.top5 <- p1

p.m.ph.top5

##Mammals / Functional

p1=ggplot(data=MAM_MAN_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="D MAMMALS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

m.m.f.top5 <- p1

m.m.f.top5

##AVES / Functional
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="E BIRDS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB2$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

a.m.f.top5 <- p1

a.m.f.top5


##PLANTS / Functional
p1=ggplot(data=PLANTS_MAN_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="F PLANTS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
  coord_flip()
p1

p.m.f.top5 <- p1 #

p.m.f.top5

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_MAN_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("D  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4", "green")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7), legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"),
                                                                                                       axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

m.m.th.top5 <- p1_4

m.m.th.top5


##-----AVES------
p1=ggplot(data=AVES_MAN_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_management), y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("E  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1

p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
p1_4

a.m.th.top5 <- p1_4

a.m.th.top5

##-----PLANTS------

PLANTS_MAN_ORD_top5

PLANTS_MAN_ORD_top5_2 <- PLANTS_MAN_ORD_top5 
PLANTS_MAN_ORD_top5_2$redlistCategory_version_2020.2[is.na(PLANTS_MAN_ORD_top5_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

p1=ggplot(data=PLANTS_MAN_ORD_top5_2, aes(x= reorder(Species2, Average.annual.cost_management), y= Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("F  PLANTS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1

p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))
p1_4

p.m.th.top5 <- p1_4

p.m.th.top5 


#----------FIGURE 4 : Damage-----------
##Mammals / Phylogenetic
p1=ggplot(data=MAM_DAM_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree)), color="red",size=1)+
  coord_flip()
p1

m.d.ph.top5 <- p1

m.d.ph.top5

##AVES / Phylogenetic
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree)), color="red",size=1)+
  coord_flip()
p1

a.d.ph.top5 <- p1

a.d.ph.top5

##PLANTS / Phylogenetic
p1=ggplot(data=PLANTS_DAM_ORD_top5, aes(x= reorder(Species2, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="C PLANTS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB3$oriPtree)), color="red",size=1)+
  coord_flip()
p1

p.d.ph.top5 <- p1

p.d.ph.top5


##Mammals / Functional

p1=ggplot(data=MAM_DAM_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="D MAMMALS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

m.d.f.top5 <- p1

m.d.f.top5 


##AVES / Functional
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="E BIRDS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(TAB2$meanoriFtree)), color="red",size=1)+
  coord_flip()
p1

a.d.f.top5 <- p1

a.d.f.top5 


##PLANTS / Functional

p1=ggplot(data=PLANTS_MAN_ORD_top5, aes(x= reorder(Species2, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size = 9),
        axis.text.y= element_text(face="bold.italic", size = 7),
        axis.text.x = element_text(face = "bold", color = "black",size = 9),
        axis.title.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 9, face = "bold"),
        legend.position="bottom")+
  labs(title="F PLANTS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(na.omit(TAB3$meanoriFtree))), color="red",size=1)+
  coord_flip()
p1

p.d.f.top5 <- p1

p.d.f.top5 


#Figure 4

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_DAM_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("A  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

m.d.th.top5 <- p1_4 

m.d.th.top5 


##-----AVES------
p1=ggplot(data=AVES_DAM_ORD_top5, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("B  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

a.d.th.top5 <- p1_4 

a.d.th.top5 


##-----PLANTS------
PLANTS_DAM_ORD_top5_2 <- PLANTS_DAM_ORD_top5
PLANTS_DAM_ORD_top5_2$redlistCategory_version_2020.2[is.na(PLANTS_DAM_ORD_top5_2$redlistCategory_version_2020.2)] <- "Not evaluated" ##!## I believe NA means "Not evaluated" or something like that.

p1=ggplot(data=PLANTS_DAM_ORD_top5_2, aes(x=reorder(Species2, Average.annual.cost_damage), y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("I  PLANTS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("lightgray")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 7),   legend.text = element_text(color = "black", size = 7)) + 
  theme(plot.title = element_text(color="black", size = 9))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size = 7))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage\n (2017 US$)") + theme(axis.title.x = element_text(size = 9, face = "bold"), axis.title.y = element_text(size = 9, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 9))

p.d.th.top5 <- p1_4

p.d.th.top5


#################
#### Save figures

#Figura 4 (6.85)
windows(6.85, 7)

ggarrange(m.d.ph.top5, m.d.f.top5, a.d.ph.top5, a.d.f.top5, p.d.ph.top5, p.d.f.top5 + rremove("x.text") , ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/damage_top5.png", res=400, height=7, width=6.85, units="in")
dev.off()

#Figura 5 p.m.f.2.v3
windows(6.85, 7)

ggarrange(m.m.ph.top5, m.m.f.top5, a.m.ph.top5, a.m.f.top5, p.m.ph.top5, p.m.f.top5 + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/management_top5.png", res=400, height=7, width=6.85, units="in")
dev.off()

m.d.th.top5b <- m.d.th.top5+theme(legend.position = c(.7, .4))
m.m.th.top5b <- m.m.th.top5+theme(legend.position = c(.7, .4))
a.d.th.top5b <- a.d.th.top5+theme(legend.position = c(.7, .4))
a.m.th.top5b <- a.m.th.top5+theme(legend.position = c(.7, .4))
p.d.th.top5b <- p.d.th.top5+theme(legend.position = c(.7, .4))
p.m.th.top5b <- p.m.th.top5+theme(legend.position = c(.7, .4))

windows(6.85, 7)

ggarrange(m.d.th.top5b, m.m.th.top5b, a.d.th.top5b, a.m.th.top5b, p.d.th.top5b, p.m.th.top5b + rremove("x.text"), ncol = 2, nrow=3, heights = c(1, 1, 1))

dev.copy(png, file="C:/Gustavo/Invacost/Filogenia/figuras para rodar/Figs top5 full name/threatened_top5.png", res=400, height=7, width=6.85, units="in")
dev.off()


