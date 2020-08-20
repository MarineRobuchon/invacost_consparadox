library(dplyr)
library(stringr)
library(tibble)
library(plyr)
library(ggplot2)
library(MASS)
library(scales)

setwd("./Invacost/Marine")

######################################
############# Load data ##############
######################################

dataAllFInvacost<-read.csv2("./dataAllFInvacost.csv")
invmammals<-filter(dataallinva, className=="MAMMALIA")
invbirds<-filter(dataallinva, className=="AVES")



#DAMAGE COST
##FUNCTIONAL ORI

##MAMMALS  #17 rows missing
#avec log
DAMfuncMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log)") + ylab("Functionnal originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#sans log
ggplot(invmammals, aes(x=Average.annual.cost_damage, y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" ) +
  xlab("Damage costs") + ylab("Functionnal originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


##BIRDS #9rows missing
DAMfuncBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Damage costs (log)") + ylab("Functionnal originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

##PHYL ORI
##MAMMALS

DAMphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

##BIRDS #
DAMphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Damage costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


#MANAGEMENT COST
##FUNCTIONAL ORI
##MAMMALS  #

MANfuncMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log)") + ylab("Functional originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

##BIRDS #
MANfuncBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_management), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Management costs (log)") + ylab("Functional originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

##PHYL ORI
##MAMMALS

MANphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

##BIRDS #
MANphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_management), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Management costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")




jpeg("./Figure3_costrelation.jpeg", width=7, height=12, units="in", res=300)
plot_grid(DAMfuncMAM,DAMfuncBIRD, DAMphylMAM, DAMphylBIRD, MANfuncMAM,MANfuncBIRD,MANphylMAM,MANphylBIRD + theme(legend.position="none") ,
          rel_widths = c(2, 2), ncol = 2, nrow = 4, labels = c("A","B","C","D","E","F"))
dev.off()


###THREAT STATUS

DAMthreatinvmammals <- ggplot(invmammals, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_damage)), y=log(Average.annual.cost_damage))) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Damage costs (log)")+
  ggtitle("THREAT- MAMMALS" )
DAMthreatinvmammals

MANthreatinvmammals <- ggplot(invmammals, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_management)), y=log(Average.annual.cost_management))) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Management costs (log)")+
  ggtitle("THREAT- MAMMALS" )
MANthreatinvmammals

DAMthreatinvbirds <- ggplot(invbirds, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_damage)), y=log(Average.annual.cost_damage))) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Damage costs (log)")+
  ggtitle("THREAT- BIRDS" )
DAMthreatinvbirds

MANthreatinvbirds <- ggplot(invbirds, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_management)), y=log(Average.annual.cost_management))) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Management costs (log)")+
  ggtitle("THREAT- BIRDS" )
MANthreatinvbirds


jpeg("./Figure3_cost_threat_relation.jpeg", width=7, height=12, units="in", res=300)
plot_grid(DAMthreatinvmammals,DAMthreatinvbirds, MANthreatinvmammals, MANthreatinvbirds,
          rel_widths = c(2, 2), ncol = 2, nrow = 2, labels = c("A","B","C","D"))
dev.off()
