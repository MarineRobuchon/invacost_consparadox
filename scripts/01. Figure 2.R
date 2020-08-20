#############################################################################################
# script to plot Figure 2
# original script by Céline Bellard
# modifications by: Marine Robuchon, XX, 
#############################################################################################

rm(list=ls())
library(dplyr)
library(cowplot)
library(invacost)
library(ggplot2)

#setwd("D:/these/Invacost/Marine/") # to personalise if needed
setwd("D:/Collaboration/Invacost workshop/Phylogenie Marine/") # to personalise if needed


dataAllF<-read.csv2("./outputs/dataAllF.csv")
dataAllFInvacost<-filter(dataAllF, invacostY == "Y") #105 lignes


# Relation between originality and threat status of exotic species
# in INVACOST and their frequency of estimation

#########################################################
###############"" Figure 2 ##############################
#########################################################
colnames(dataAllFInvacost)
length(unique(dataAllF$Species)) # n =17046 -> 3 duplicates

listDupli<-dataAllF[duplicated(dataAllF$Species),"Species"]
dataAllF[which(dataAllF$Species == listDupli[3]),]

dataAllF<-dataAllF[!duplicated(dataAllF$Species), ]

write.csv2(dataAllFInvacost,"./outputs/dataAllFInvacost.csv")

pFori <- 
  ggplot(dataAllFInvacost, aes(x=freq_publi, y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .)+ theme_classic() +
  xlab("Occurences of publications with cost") + ylab("Functionnal originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  

pPdist <- ggplot(dataAllFInvacost, aes(x=freq_publi, y=log(oriPdist))) +
  geom_point()+  facet_grid(className ~ .) + theme_classic() +
  xlab("ccurences of publications with cost") + ylab("Phylo originality (log)")+
  #stat_smooth(method = "loess", formula = y ~  x, size = 0.9, se = TRUE, colour = "black")
stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


p1<-ggplot(dataAllFInvacost, aes(y=freq_publi, x=as.factor(redlistCategory_version_2020.2))) +
  geom_boxplot(trim=FALSE)+   geom_jitter(position=position_jitter(0.2),alpha=0.3)+
  facet_grid(className ~ .)+  theme_classic()+ xlab("") + ylab("Occurences of publications")


prow<-plot_grid(pPdist +theme(legend.position="none"),
                pFori  + theme(legend.position="none"),
                p1 + theme(legend.position="none"),
                labels=c("A", "B", "C"), ncol = 1, nrow = 3)


jpeg("./outputs/Figure 2.jpeg", units="in", width=7, height=9, res=600)

plot_grid(prow, rel_widths = c(3, .4))

dev.off()



