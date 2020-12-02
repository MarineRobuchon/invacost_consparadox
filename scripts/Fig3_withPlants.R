#############################################################################################
# script to plot Figure 1
# original script by Camille Bernery
# modifications by: Marine Robuchon, Vanessa Rezende
#############################################################################################


library(dplyr)
library(stringr)
library(tibble)
library(plyr)
library(ggplot2)
library(MASS)
library(scales)
library(cowplot)



############# Load & format data ##############

## load & format data

dataall <- read.table(paste0(getwd(), "/outputs/dataAllF.txt")) # database for birds and mammals
head(dataall)
str(dataall$invacostY)
dataall$invacostY[which(is.na(dataall$invacostY))] <- "N"
dataall$invacostY <- factor(dataall$invacostY, levels = c("Y", "N"))
levels(dataall$invacostY)

dataplants <- read.csv2(paste0(getwd(), "/outputs/data_plants.csv")) # database for plants
head(dataplants)
str(dataplants$invacostY)
dataplants$invacostY[which(is.na(dataplants$invacostY))] <- "N"
dataplants$invacostY <- factor(dataplants$invacostY, levels = c("Y", "N"))
levels(dataplants$invacostY)
colnames(dataplants)[7] <- "meanoriFtree" # change the name of this variable so it has the same name than in the db for birds and mammals

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

invmammals<-filter(dataAllFInvacost, className=="MAMMALIA")
invbirds<-filter(dataAllFInvacost, className=="AVES")
invplants<-filter(dataAllFInvacost, className=="PLANTS")

#####FIGURES - Relation between economic costs and originality#######

###DAMAGE COST
##FUNCTIONAL ORI
#MAMMALS

DAMfuncMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage+1), y=log(meanoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log+1)") + ylab("Functionnal originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


#BIRDS
DAMfuncBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage+1), y=log(meanoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Damage costs (log+1)") + ylab("Functionnal originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")
plot(DAMfuncBIRD)

#PLANTS
DAMfuncPLANTS<-ggplot(invplants, aes(x=log(Average.annual.cost_damage+1), y=log(meanoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("PLANTS" )+
  xlab("Damage costs (log+1)") + ylab("Functionnal originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


##PHYL ORI
#MAMMALS

DAMphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log+1)") + ylab("Phylogenetic originality (Ma) (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


#BIRDS 
DAMphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Damage costs (log+1)") + ylab("Phylogenetic originality (Ma) (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#PLANTS
DAMphylPLANT<-ggplot(invplants, aes(x=log(Average.annual.cost_damage+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("PLANTS" )+
  xlab("Damage costs (log+1)") + ylab("Phylogenetic originality (Ma) (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


###MANAGEMENT COST
##FUNCTIONAL ORI
#MAMMALS

MANfuncMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management+1), y=log(meanoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log+1)") + ylab("Functional originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
MANfuncBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_management+1), y=log(meanoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Management costs (log+1)") + ylab("Functional originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


#PLANTS
MANfuncPLANT<-ggplot(invplants, aes(x=log(Average.annual.cost_management+1), y=log(meanoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("PLANTS" )+
  xlab("Management costs (log+1)") + ylab("Functional originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


##PHYL ORI
#MAMMALS

MANphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log+1)") + ylab("Phylogenetic originality (Ma) (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
MANphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_management+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Management costs (log+1)") + ylab("Phylogenetic originality (Ma) (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


#PLANTS
MANphylPLANT<-ggplot(invplants, aes(x=log(Average.annual.cost_management+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("PLANTS" )+
  xlab("Management costs (log+1)") + ylab("Phylogenetic originality (Ma) (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


jpeg(paste0(getwd(), "/outputs/Figure3.jpeg"), width=9, height=12, units="in", res=600)
plot_grid(DAMphylMAM, DAMphylBIRD,DAMphylPLANT, MANphylMAM,MANphylBIRD,MANphylPLANT, DAMfuncMAM,DAMfuncBIRD,DAMfuncPLANTS, MANfuncMAM,MANfuncBIRD,MANfuncPLANT + theme(legend.position="none") ,
          rel_widths = c(2, 2), ncol = 3, nrow = 4, labels = c("A","B","C","D","E","F", "G", "H", "I", "J", "K", "L"))
dev.off()



###TEST STAT####

axisx<-c("Average.annual.cost_damage","Average.annual.cost_management")
namex<-c("Cost damage", "Cost management")
namey<-c("Phylogenetic originality","Functional originality")
axisy<-c("oriPtree","meanoriFtree")

#MAMMALS
i=0
xx<-0
CorPmammals<-data.frame(Phylogenetic=integer(),  #prepare table to put p-values in it
                        Functional=integer())
CorEstmammals<-data.frame(Phylogenetic=integer(),  #prepare table to put estimations values in it
                          Functional=integer())

for (xaxis in axisx ) {
  xx<-xx+1
  yy<-0
  for (yaxis in axisy){
    yy=yy+1
    Cor<-cor.test(log(invmammals[,xaxis]+1),log(invmammals[,yaxis]+1), method = "pearson" )
    CorPmammals[xx,yy]<-Cor$p.value
    CorEstmammals[xx,yy]<-Cor$estimate
  }
}

row.names(CorPmammals)<-c("Damage","Management")
row.names(CorEstmammals)<-c("Damage","Management")

CorPmammals  #p-values
CorEstmammals #estimations values

##BIRDS
i=0
xx<-0

CorPbirds<-data.frame(Phylogenetic=integer(),  #prepare table to put p-values in it
                      Functional=integer())
CorEstbirds<-data.frame(Phylogenetic=integer(),  #prepare table to put estimations values in it
                        Functional=integer())


for (xaxis in axisx ) {
  xx<-xx+1
  yy<-0
  for (yaxis in axisy){
    yy=yy+1
    Cor<-cor.test(log(invbirds[,xaxis]+1),log(invbirds[,yaxis]+1), method = "pearson" )
    CorPbirds[xx,yy]<-Cor$p.value
    CorEstbirds[xx,yy]<-Cor$estimate
  }
}

row.names(CorPbirds)<-c("Damage","Management")
row.names(CorEstbirds)<-c("Damage","Management")

CorPbirds  #p-values
CorEstbirds  #estimations values




##PLANTS
i=0
xx<-0

CorPplants<-data.frame(Phylogenetic=integer(),  #prepare table to put p-values in it
                      Functional=integer())
CorEstplants<-data.frame(Phylogenetic=integer(),  #prepare table to put estimations values in it
                        Functional=integer())


for (xaxis in axisx ) {
  xx<-xx+1
  yy<-0
  for (yaxis in axisy){
    yy=yy+1
    Cor<-cor.test(log(invplants[,xaxis]+1),log(invplants[,yaxis]+1), method = "pearson" )
    CorPplants[xx,yy]<-Cor$p.value
    CorEstplants[xx,yy]<-Cor$estimate
  }
}

row.names(CorPplants)<-c("Damage","Management")
row.names(CorEstplants)<-c("Damage","Management")

CorPplants  #p-values
CorEstplants  #estimations values




#### FIGURES - Relation between cost and threat status####  # SPECIES IN INVACOST
unique(invmammals$redlistCategory_version_2020.2)
invmammals$redlistCategory_version_2020.2 <- factor(invmammals$redlistCategory_version_2020.2, 
                                                    levels = c ("Least Concern", "Near Threatened", "Vulnerable", "Endangered"))
    
DAMthreatinvmammals <- ggplot(invmammals, aes(x=redlistCategory_version_2020.2, y=log(Average.annual.cost_damage))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Damage costs (log)")+
  ggtitle("MAMMALS" )
DAMthreatinvmammals

MANthreatinvmammals <- ggplot(invmammals, aes(x=redlistCategory_version_2020.2, y=log(Average.annual.cost_management))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Management costs (log)")+
  ggtitle("MAMMALS" )
MANthreatinvmammals

unique(invbirds$redlistCategory_version_2020.2) # no need to do it for birds since they are only least concern

unique(invplants$redlistCategory_version_2020.2)
invplants$redlistCategory_version_2020.2 <- factor(invplants$redlistCategory_version_2020.2, 
                                                    levels = c ("Least Concern", "Vulnerable", "Endangered", NA))

DAMthreatinvplants <- ggplot(invplants, aes(x=redlistCategory_version_2020.2, y=log(Average.annual.cost_damage))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Damage costs (log)")+
  ggtitle("PLANTS" )
DAMthreatinvplants

MANthreatinvplants <- ggplot(invplants, aes(x=redlistCategory_version_2020.2, y=log(Average.annual.cost_management))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Management costs (log)")+
  ggtitle("PLANTS" )
MANthreatinvplants

jpeg(paste0(getwd(), "/outputs/Figure3_cost_threat_relation.jpeg"), width=7, height=12, units="in", res=600)
plot_grid(DAMthreatinvmammals,DAMthreatinvplants, MANthreatinvmammals, MANthreatinvplants,
          rel_widths = c(2, 2), ncol = 2, nrow = 2, labels = c("A","B","C","D"))
dev.off()
