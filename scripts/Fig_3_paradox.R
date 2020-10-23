library(dplyr)
library(stringr)
library(tibble)
library(plyr)
library(ggplot2)
library(MASS)
library(scales)

#setwd("./Invacost/Marine")

######################################
############# Load data ##############
######################################

dataAllFInvacost<-read.csv2("./dataAllFInvacost.csv")
invmammals<-filter(dataAllFInvacost, className=="MAMMALIA")
invbirds<-filter(dataAllFInvacost, className=="AVES")

#DAMAGE COST
##FUNCTIONAL ORI
##MAMMALS
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

######TEST
cor.test(log(invmammals$Average.annual.cost_damage),log(invmammals$meanoriFtree), method = "pearson" )
cor.test(invmammals$Average.annual.cost_damage,invmammals$meanoriFtree, method = "pearson" )



##BIRDS
DAMfuncBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Damage costs (log)") + ylab("Functionnal originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

######TEST
cor.test(log(invbirds$Average.annual.cost_damage),log(invbirds$meanoriFtree), method = "pearson" ) ##des NA
cor.test(invbirds$Average.annual.cost_damage,invbirds$meanoriFtree, method = "pearson" )


##PHYL ORI
##MAMMALS

DAMphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

######TEST
cor.test(log(invmammals$Average.annual.cost_damage),log(invmammals$oriPtree), method = "pearson" ) ##des NA
cor.test(invmammals$Average.annual.cost_damage,invmammals$oriPtree, method = "pearson" )


##BIRDS 
DAMphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Damage costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

######TEST
cor.test(log(invbirds$Average.annual.cost_damage),(invbirds$oriPtree), method = "pearson" ) ##des NA
cor.test(invbirds$Average.annual.cost_damage,invbirds$oriPtree, method = "pearson" )


#MANAGEMENT COST
##FUNCTIONAL ORI
##MAMMALS

MANfuncMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log)") + ylab("Functional originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

######TEST
cor.test(log(invmammals$Average.annual.cost_management),log(invmammals$meanoriFtree), method = "pearson", use = "complete.obs")
cor.test(invmammals$Average.annual.cost_management,invmammals$meanoriFtree, method = "pearson" )


##BIRDS
MANfuncBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_management), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Management costs (log)") + ylab("Functional originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


######TEST
cor.test(log(invbirds$Average.annual.cost_management),log(invbirds$meanoriFtree), method = "pearson" )
cor.test(invbirds$Average.annual.cost_management,invbirds$meanoriFtree, method = "pearson" )


##PHYL ORI
##MAMMALS

MANphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

######TEST
cor.test(log(invmammals$Average.annual.cost_management),log(invmammals$oriPtree), method = "pearson" ) ##des NA
cor.test(invmammals$Average.annual.cost_management,invmammals$oriPtree, method = "pearson" )


##BIRDS
MANphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_management), y=log(oriPtree))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Management costs (log)") + ylab("Phylogenetic originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

cor.test(log(invbirds$Average.annual.cost_management),log(invbirds$oriPtree), method = "pearson" ) ##des NA
cor.test(invbirds$Average.annual.cost_management,invbirds$oriPtree, method = "pearson" )


jpeg("./Figure3_costrelation.jpeg", width=7, height=12, units="in", res=300)
plot_grid(DAMfuncMAM,DAMfuncBIRD, DAMphylMAM, DAMphylBIRD, MANfuncMAM,MANfuncBIRD,MANphylMAM,MANphylBIRD + theme(legend.position="none") ,
          rel_widths = c(2, 2), ncol = 2, nrow = 4, labels = c("A","B","C","D","E","F", "G", "H"))
dev.off()



# ###THREAT STATUS
# 
# DAMthreatinvmammals <- ggplot(invmammals, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_damage)), y=log(Average.annual.cost_damage))) + 
#   geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
#   geom_boxplot(alpha = 0.5)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
#   labs(y="Damage costs (log)")+
#   ggtitle("THREAT- MAMMALS" )
# DAMthreatinvmammals
# 
# MANthreatinvmammals <- ggplot(invmammals, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_management)), y=log(Average.annual.cost_management))) + 
#   geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
#   geom_boxplot(alpha = 0.5)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
#   labs(y="Management costs (log)")+
#   ggtitle("THREAT- MAMMALS" )
# MANthreatinvmammals
# 
# DAMthreatinvbirds <- ggplot(invbirds, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_damage)), y=log(Average.annual.cost_damage))) + 
#   geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
#   geom_boxplot(alpha = 0.5)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
#   labs(y="Damage costs (log)")+
#   ggtitle("THREAT- BIRDS" )
# DAMthreatinvbirds
# 
# MANthreatinvbirds <- ggplot(invbirds, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_management)), y=log(Average.annual.cost_management))) + 
#   geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
#   geom_boxplot(alpha = 0.5)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
#   labs(y="Management costs (log)")+
#   ggtitle("THREAT- BIRDS" )
# MANthreatinvbirds
# 
# 
# jpeg("./Figure3_cost_threat_relation.jpeg", width=7, height=12, units="in", res=300)
# plot_grid(DAMthreatinvmammals,DAMthreatinvbirds, MANthreatinvmammals, MANthreatinvbirds,
#           rel_widths = c(2, 2), ncol = 2, nrow = 2, labels = c("A","B","C","D"))
# dev.off()
