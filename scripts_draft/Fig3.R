library(dplyr)
library(stringr)
library(tibble)
library(plyr)
library(ggplot2)
library(MASS)
library(scales)
library(cowplot)

setwd("./outputs")

############# Load data ##############
dataAllF<-read.csv2("./dataAllF.csv")
dataAllFInvacost<-filter(dataAllF, invacostY=="Y")
invmammals<-filter(dataAllFInvacost, className=="MAMMALIA")
invbirds<-filter(dataAllFInvacost, className=="AVES")


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

##PHYL ORI
#MAMMALS

DAMphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log+1)") + ylab("Phylogenetic originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


#BIRDS 
DAMphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Damage costs (log+1)") + ylab("Phylogenetic originality (log+1)") +
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


##PHYL ORI
#MAMMALS

MANphylMAM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log+1)") + ylab("Phylogenetic originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
MANphylBIRD<-ggplot(invbirds, aes(x=log(Average.annual.cost_management+1), y=log(oriPtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" )+
  xlab("Management costs (log+1)") + ylab("Phylogenetic originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

jpeg("./Figure3.jpeg", width=7, height=12, units="in", res=300)
plot_grid(DAMphylMAM, DAMphylBIRD, MANphylMAM,MANphylBIRD, DAMfuncMAM,DAMfuncBIRD, MANfuncMAM,MANfuncBIRD + theme(legend.position="none") ,
          rel_widths = c(2, 2), ncol = 2, nrow = 4, labels = c("A","B","C","D","E","F", "G", "H"))
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




#### FIGURES - Relation between cost and threat status####  # SPECIES IN INVACOST
DAMthreatinvmammals <- ggplot(invmammals, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_damage)), y=log(Average.annual.cost_damage))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Damage costs (log)")+
  ggtitle("MAMMALS" )
DAMthreatinvmammals

MANthreatinvmammals <- ggplot(invmammals, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_management)), y=log(Average.annual.cost_management))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Management costs (log)")+
  ggtitle("MAMMALS" )
MANthreatinvmammals

DAMthreatinvbirds <- ggplot(invbirds, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_damage)), y=log(Average.annual.cost_damage))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Damage costs (log)")+
  ggtitle("BIRDS" )
DAMthreatinvbirds

MANthreatinvbirds <- ggplot(invbirds, aes(x=reorder(redlistCategory_version_2020.2, -log(Average.annual.cost_management)), y=log(Average.annual.cost_management))) +
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Management costs (log)")+
  ggtitle("BIRDS" )
MANthreatinvbirds


jpeg("./Figure3_cost_threat_relation.jpeg", width=7, height=12, units="in", res=300)
plot_grid(DAMthreatinvmammals,DAMthreatinvbirds, MANthreatinvmammals, MANthreatinvbirds,
          rel_widths = c(2, 2), ncol = 2, nrow = 2, labels = c("A","B","C","D"))
dev.off()
