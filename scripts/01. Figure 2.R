#############################################################################################
# script to plot Figure 2
# original script by C?line Bellard
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
head(dataAllF)
#########################################################
###############"" Figure 2 ##############################
#########################################################
colnames(dataAllFInvacost)
length(unique(dataAllF$Species)) # n =16796 

listDupli<-dataAllF[duplicated(dataAllF$Species),"Species"]

write.csv2(dataAllFInvacost,"./outputs/dataAllFInvacost.csv")


colnames(dataAllFInvacost)
className.labs <- c("Birds", "Mammals")
names(className.labs) <- c("AVES", "MAMMALIA")

head(dataAllFInvacost)

pFori <- 
  ggplot(dataAllFInvacost, aes(x=log(freq_publi), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .,labeller = labeller(className = className.labs))+ theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Functionnal originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")


pPdist <- ggplot(dataAllFInvacost, aes(x=log(freq_publi), y=log(oriPtree))) +
  geom_point()+  facet_grid(className ~ .,labeller = labeller(className = className.labs)) + theme_classic() +
  xlab("ccurences of publications with cost (log)") + ylab("Phylo originality (log)")+
  #stat_smooth(method = "loess", formula = y ~  x, size = 0.9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

filter(dataAllFInvacost, className== "AVES")
table(dataAllFInvacost$redlistCategory_version_2020.2)

p1<-ggplot(dataAllFInvacost, aes(y=log(freq_publi), x=as.factor(redlistCategory_version_2020.2))) +
  geom_boxplot(trim=FALSE)+   geom_jitter(position=position_jitter(0.2),alpha=0.3)+
  facet_grid(className ~ .,labeller = labeller(className = className.labs))+  theme_classic()+ xlab("") +
  ylab("Occurences of publications (log)")




prow<-plot_grid(pPdist +theme(legend.position="none"),
                pFori  + theme(legend.position="none"),
                p1 + theme(legend.position="none"),
                labels=c("A", "B", "C"), ncol = 1, nrow = 3)


jpeg("./outputs/Figure 2.jpeg", units="in", width=7, height=9, res=600)

plot_grid(prow, rel_widths = c(3, .4))

dev.off()


#########################################################
#### Figure 2 Supplementary (diet, Act, Mass)  ##########
#########################################################

colnames(dataAllFInvacost)

# dietoriFtree activityoriFtree massoriFtree 

pDiet <- 
  ggplot(dataAllFInvacost, aes(x=log(freq_publi), y=log(dietoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .,labeller = labeller(className = className.labs))+ theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Functionnal diet originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")



pAct <- 
  ggplot(dataAllFInvacost, aes(x=log(freq_publi), y=log(activityoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .,labeller = labeller(className = className.labs))+ theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Functionnal activity originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")



pMass <- 
  ggplot(dataAllFInvacost, aes(x=log(freq_publi), y=log(massoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .,labeller = labeller(className = className.labs))+ theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Functionnal mass originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")






prow<-plot_grid(pDiet +theme(legend.position="none"),
                pAct  + theme(legend.position="none"),
                pMass + theme(legend.position="none"),
                ncol = 1, nrow = 3)


jpeg("./outputs/Figure 2dietMassAct.jpeg", units="in", width=7, height=9, res=600)

plot_grid(prow, rel_widths = c(3, .4))

dev.off()



# Pearson correlation test
library(dplyr)
filter(dataAllFInvacost, className= "MAMMALIA")

Mammals<-filter(dataAllFInvacost, className == "MAMMALIA") #105 lignes
colnames(Mammals)
mammals <- Mammals[,c(23,15,16,17,18,20)]


colnames(mammals) <- c("Frequency of publications","Funct. originality (diet)", 
                       "Funct. originality (activity)","Funct. originality (mass)", "Funct. originality (All)",
                       "Phylo. originality (ALl)")

corrplot2(
  data = log(mammals),
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

str(mammals)
mammals[,1]<-as.numeric(mammals[,1])
test <- cor.test(log(mammals[,1]), log(mammals[,3]))
test

Aves<-filter(dataAllFInvacost, className == "AVES") #105 lignes
colnames(Aves)
aves <- Aves[,c(23,15,16,17,18,20)]
colnames(aves) <- c("Frequency of publications","Funct. originality (diet)", 
                    "Funct. originality (activity)","Funct. originality (mass)", "Funct. originality (All)",
                    "Phylo. originality (ALl)")

corrplot2(
  data = log(aves),
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

p1



test <- cor.test(dataAllFInvacost$meanoriFtree, dataAllFInvacost$dietoriFtree)
test

# Pearson's product-moment correlation
# 
# data:  log(mammals[, 1]) and log(mammals[, 3])
# t = -2.2003, df = 33, p-value = 0.03489
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.61734310 -0.02774159
# sample estimates:
# cor 
# -0.3576814




####################
##### BOXPLOT ######
####################


library(tidyverse)
library(ggpubr)
library(rstatix)


p1<-ggplot(dataAllFInvacost, aes(y=log(freq_publi), x=as.factor(redlistCategory_version_2020.2))) +
  geom_boxplot(trim=FALSE)+   geom_jitter(position=position_jitter(0.2),alpha=0.3)+
  facet_grid(className ~ .,labeller = labeller(className = className.labs))+  theme_classic()+ xlab("") +
  ylab("Occurences of publications (log)")


mammals<-filter(dataAllFInvacost, className == "MAMMALIA")

str(aves)
mammals$redlistCategory_version_2020.2 <- as.factor(mammals$redlistCategory_version_2020.2)

res.aov <- mammals %>% anova_test(freq_publi ~ redlistCategory_version_2020.2)
res.aov

TukeyHSD(res.aov)

library(ggplot2)
library(ggpubr)

p1<-ggplot(dataAllFInvacost, aes(y=log(freq_publi), x=as.factor(redlistCategory_version_2020.2))) +
  geom_boxplot(trim=FALSE)+   geom_jitter(position=position_jitter(0.2),alpha=0.3)+
  facet_grid(className ~ .,labeller = labeller(className = className.labs))+  theme_classic()+ xlab("") +
  ylab("Occurences of publications (log)")



p1

compare_means(formula, data, method = "anova", paired = FALSE,
              group.by = NULL, ref.group = NULL)

my_comparisons <- list( c("Least Concern", "Endangered"), c("Near Threatened", "Endangered"), c("Vulnerable", "Endangered") )
t<-ggplot(mammals, aes(x = as.factor(redlistCategory_version_2020.2), y = log(freq_publi))) +
  geom_boxplot(trim=FALSE)+geom_jitter(position=position_jitter(0.2),alpha=0.3)

t +
  stat_compare_means(label = "p.signif", method = "t.test",ref.group = "Endangered", comparisons = my_comparisons)  +
  #  stat_compare_means(label.y = max(log(mammals$freq_publi))+ 2, method="t.test")+
  theme_classic()+ xlab("") +
  ylab("Occurences of publications (log)")

TukeyHSD(res.aov)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = freq_publi ~ redlistCategory_version_2020.2, data = mammals)
# 
# $redlistCategory_version_2020.2
# diff       lwr        upr     p adj
# Least Concern-Endangered      -35.7333333 -62.91263  -8.554041 0.0062218 ***
# Near Threatened-Endangered    -43.0000000 -80.81229  -5.187714 0.0209514 ***
# Vulnerable-Endangered         -42.6666667 -73.54027 -11.793065 0.0038493 **
# Near Threatened-Least Concern  -7.2666667 -34.44596  19.912626 0.8860934
# Vulnerable-Least Concern       -6.9333333 -23.12359   9.256920 0.6545387
# Vulnerable-Near Threatened      0.3333333 -30.54027  31.206935 0.9999907
# 
# 

# Compute the analysis of variance
res.aov <- aov(freq_publi ~ redlistCategory_version_2020.2, data = mammals)
# Summary of the analysis
summary(res.aov)





