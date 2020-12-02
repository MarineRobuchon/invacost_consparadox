#############################################################################################
# script to plot Figure 2
# original script by Céline Bellard
# modifications by: Marine Robuchon, Vanessa Rezende
#############################################################################################

rm(list=ls())

library(dplyr)
library(cowplot)
library(invacost)
library(ggplot2)


#########################################################
###############"" Figure 2 ##############################
#########################################################

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

colnames(dataAllFInvacost)

className.labs <- c("Birds", "Mammals", "Plants")
names(className.labs) <- c("AVES", "MAMMALIA", "PLANTS")

unique(dataAllFInvacost$className)
dataAllFInvacost$className <- factor(dataAllFInvacost$className, levels = c ("MAMMALIA", "AVES", "PLANTS"))


## make plots

pFori <- 
  ggplot(dataAllFInvacost, aes(x=log(freq_publi), y=log(meanoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .,labeller = labeller(className = className.labs), scales = "free_y")+ theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Functionnal originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")
plot(pFori)

nrow(dataAllFInvacost[which(is.na(dataAllFInvacost$meanoriFtree)| is.na((dataAllFInvacost$freq_publi))),]) # the missing rows are NAs

pPori <- ggplot(dataAllFInvacost, aes(x=log(freq_publi), y=log(oriPtree))) +
  geom_point()+  facet_grid(className ~ .,labeller = labeller(className = className.labs), scales = "free_y") + theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Phylogenetic originality (Ma) (log)")+
  #stat_smooth(method = "loess", formula = y ~  x, size = 0.9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")
plot(pPori)

levels(factor(dataAllFInvacost$redlistCategory_version_2020.2))
str(dataAllFInvacost$redlistCategory_version_2020.2)

p1<-ggplot(dataAllFInvacost, aes(y=log(freq_publi), 
                                 x=factor(redlistCategory_version_2020.2,
                                          levels = c("Least Concern", "Near Threatened", "Vulnerable", "Endangered")))) +
  geom_boxplot()+   geom_jitter(position=position_jitter(0.2),alpha=0.3)+
  facet_grid(className ~ .,labeller = labeller(className = className.labs))+  theme_classic()+ xlab("") +
  ylab("Occurences of publications (log)")
p1

prow <-plot_grid(pPori +theme(legend.position="none"),
                pFori  + theme(legend.position="none"),
                p1 + theme(legend.position="none"),
                labels=c("A", "B", "C"), ncol = 1, nrow = 3)

jpeg(paste0(getwd(), "/outputs/Figure2.jpeg"), units="in", width=7, height=12, res=600)

plot_grid(prow, rel_widths = c(3, .4))

dev.off()


#########################################################
#### Figure 2 Supplementary (diet, Act, Mass)  ##########
#########################################################

dataFInvacost_mammalsbirds <- dataAllFInvacost[which(dataAllFInvacost$className %in% c("AVES", "MAMMALIA")),]

pDiet <- 
  ggplot(dataFInvacost_mammalsbirds, aes(x=log(freq_publi), y=log(dietoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .,labeller = labeller(className = className.labs))+ theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Functionnal diet originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

pAct <- 
  ggplot(dataFInvacost_mammalsbirds, aes(x=log(freq_publi), y=log(activityoriFtree))) +
  geom_point(alpha=0.5)+  facet_grid(className ~ .,labeller = labeller(className = className.labs))+ theme_classic() +
  xlab("Occurences of publications with cost (log)") + ylab("Functionnal activity originality (log)") +
  #  stat_smooth(method = "loess", formula = y ~  x, size = .9, se = TRUE, colour = "black")
  stat_smooth(method = "lm", formula = y ~  x, size = .9, se = TRUE, colour = "black")

pMass <- 
  ggplot(dataFInvacost_mammalsbirds, aes(x=log(freq_publi), y=log(massoriFtree))) +
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


#########################################################
## Pearson correlation tests 
#########################################################

Mammals<-filter(dataall, className == "MAMMALIA" & invacostY == "Y") # 35 lines
colnames(Mammals)
mammals <- Mammals[, c("freq_publi", "dietoriFtree", "activityoriFtree", "massoriFtree", "meanoriFtree", "oriPtree")]


colnames(mammals) <- c("Frequency of publications","Funct. originality (diet)", 
                       "Funct. originality (activity)","Funct. originality (mass)", "Funct. originality (All)",
                       "Phylo. originality (All)")

### HERE - trying to replicate the corrplot2 but cannot find the function

corrplot(
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

#### End of the script ####
###########################


