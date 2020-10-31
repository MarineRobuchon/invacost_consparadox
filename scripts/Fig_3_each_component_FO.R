library(dplyr)
library(stringr)
library(tibble)
library(plyr)
library(ggplot2)
library(MASS)
library(scales)
library(cowplot)

#setwd("./Invacost/Marine")
setwd("./outputs")


############# Load data ##############


dataAllF<-read.csv2("./dataAllF.csv")
dataAllFInvacost<-filter(dataAllF, invacostY=="Y")
invmammals<-filter(dataAllFInvacost, className=="MAMMALIA")
invbirds<-filter(dataAllFInvacost, className=="AVES")


###### DAMAGE COST######
##DIET
#MAMMALS
dietDM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage+1), y=log(dietoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log+1)") + ylab("Diet originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
dietDB<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage+1), y=log(dietoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" ) +
  xlab("Damage costs (log+1)") + ylab("Diet originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

##ACTIVITY
#MAMMALS
actDM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage+1), y=log(activityoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log+1)") + ylab("Activity originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
actDB<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage+1), y=log(activityoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" ) +
  xlab("Damage costs (log+1)") + ylab("Activity originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


##MASS
#MAMMALS
massDM<-ggplot(invmammals, aes(x=log(Average.annual.cost_damage+1), y=log(massoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Damage costs (log+1)") + ylab("Mass originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
massDB<-ggplot(invbirds, aes(x=log(Average.annual.cost_damage+1), y=log(massoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" ) +
  xlab("Damage costs (log+1)") + ylab("Mass originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")



######MANAGEMENT COST######
##DIET
#MAMMALS
dietMM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management+1), y=log(dietoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log+1)") + ylab("Diet originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
dietMB<-ggplot(invbirds, aes(x=log(Average.annual.cost_management+1), y=log(dietoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" ) +
  xlab("Management costs (log+1)") + ylab("Diet originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

##ACTIVITY
#MAMMALS
actMM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management+1), y=log(activityoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log+1)") + ylab("Activity originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
actMB<-ggplot(invbirds, aes(x=log(Average.annual.cost_management+1), y=log(activityoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" ) +
  xlab("Management costs (log+1)") + ylab("Activity originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


##MASS
#MAMMALS
massMM<-ggplot(invmammals, aes(x=log(Average.annual.cost_management+1), y=log(massoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("MAMMALS" ) +
  xlab("Management costs (log+1)") + ylab("Mass originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

#BIRDS
massMB<-ggplot(invbirds, aes(x=log(Average.annual.cost_management+1), y=log(massoriFtree+1))) +
  geom_point(alpha=0.5)+ theme_classic() +
  ggtitle("BIRDS" ) +
  xlab("Management costs (log+1)") + ylab("Mass originality (log+1)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


jpeg("./Figure3_MAMMALS_each_component_FO.jpeg", width=7, height=12, units="in", res=300)
plot_grid(dietDM, dietMM, actDM, actMM, massDM, massMM + 
            theme(legend.position="none") ,
          rel_widths = c(2, 2), 
          ncol = 2, nrow = 3, 
          labels = c("A","B","C","D","E","F"))
dev.off()

jpeg("./Figure3_BIRDS_each_component_FO.jpeg", width=7, height=12, units="in", res=300)
plot_grid(dietDB, dietMB,actDB,  actMB, massDB,  massMB + 
            theme(legend.position="none") ,
          rel_widths = c(2, 2), 
          ncol = 2, nrow = 3, 
          labels = c("A","B","C","D","E","F"))
dev.off()



###TEST STAT####
axisx<-c("Average.annual.cost_damage","Average.annual.cost_management")
namex<-c("Cost damage", "Cost management")
namey<-c("Diet originality","Activity originality", "Mass originality")
axisy<-c("dietoriFtree","activityoriFtree","massoriFtree")

##MAMMALS
i=0
xx<-0

CorPmammals<-data.frame(Diet=integer(),   #prepare table to put p-values in it
                           Activity=integer(),
                           Mass=integer())     
CorEstmammals<-data.frame(Diet=integer(),## #prepare table to put estimate values in it
                        Activity=integer(),
                        Mass=integer())


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

CorPmammals  #pvalues
CorEstmammals #estimate values



##birds
i=0
xx<-0

CorPbirds<-data.frame(Diet=integer(),  #pvalue
                        Activity=integer(),
                        Mass=integer())
CorEstbirds<-data.frame(Diet=integer(),##estimate
                          Activity=integer(),
                          Mass=integer())


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
CorPbirds
CorEstbirds
