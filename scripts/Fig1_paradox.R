library(dplyr)
library(stringr)
library(tibble)
library(plyr)
library(ggplot2)
library(MASS)
library(scales)
library(forcats)

setwd("./outputs")

######################################
############# Load data ##############
######################################

dataall<-read.table("./dataAllF.txt")
iucnsp<-read.csv("./IUCN_exotic_mammal_bird.csv") ##from IUCN red list (bird + mammals) 
gavia<-read.csv("./GAVIA_exotic_bird.csv") #only birds, from there : https://www.nature.com/articles/sdata201741#Tab2
gisd<-read.csv("./gisd_exotic_mammal_bird.csv", sep=";") #from gisd (mammals+birds) 
seebensdata<-read.csv("D:/these/Review/GASFRD_v1.2.csv")

#####KEEP ONLY mammals from the seebens database####
#Remove "casual" and "unknow" categories
seebensMA<-subset(seebensdata, Class=="Mammalia" & !PresentStatus=="casual" & !PresentStatus=="")
table(seebensMA$PresentStatus)
seebens<-unique(seebensMA$Taxon)

##### Filter GAVIA - exotic bird database #######527 species
#Remove "unsuccessfull" and "unknow" categories
newgavia <- gavia[which(gavia$StatusCat!='Unsuccessful' & gavia$StatusCat!='Unknown'),] #527 species
##only keep one repetition per species
gaviasp<-unique(newgavia$Binomial)

###Reunite all databasis
Allbasis<-c(levels(factor(iucnsp$scientificName)),levels(factor(gisd$Species)), levels(factor(gaviasp)), seebens) #all exotic species found in all basis
Exotic2<- dataall$Species %in% Allbasis #which species are in the global database
dataall<-add_column(dataall, Exotic2, .after = "oriPtree") #add a column with TRUE = exotic species and FALSE = not an exotic species
#Verify the number of exotic species in the global database
trueexo<-subset(dataall, Exotic2==TRUE)
falseexo<-subset(dataall, Exotic2==F)
Allexoticsp<-unique(trueexo[c("Species", "className")])  
table(Allexoticsp$className)

########FIGURE 1######

# #verify wich species are not considered exotic but in invacost:
# ExoticInvacost<-filter(Exotic, invacostY == "Y") 
# Invacostobs<-filter(dataall, invacostY == "Y")
# invnotindata<-!Invacostobs$Species %in% ExoticInvacost$Species #find the name of the not exotic but in invacost species
# Invacostobs[invnotindata==TRUE,1]
# #==> "Atelerix albiventris"   "Phascolarctos cinereus" "Aquila chrysaetos"

Exotic<-filter(dataall, Exotic2==TRUE)
Exoticmammals<-filter(Exotic, className=="MAMMALIA")
notexoinvacostM<-subset(dataall, Species=="Phascolarctos cinereus"|Species=="Atelerix albiventris")
Exoticmammals<-rbind(Exoticmammals, notexoinvacostM)

Exoticbirds<-filter(Exotic, className=="AVES")
notexoinvacost<-subset(dataall, Species=="Aquila chrysaetos")
Exoticbirds<-rbind(Exoticbirds, notexoinvacost)

####MEAN AND MEDIAN FOR EACH ORIGINALITY

##Filter
Exobirdinvacost<-filter(Exoticbirds,invacostY=="Y")
Exomammalinvacost<-filter(Exoticmammals,invacostY=="Y")

Exobirdnotinvacost<-anti_join(Exoticbirds, Exobirdinvacost)
Exomammalnotinvacost<-anti_join(Exoticmammals, Exomammalinvacost)


#FUNCTIONAL
#in invacost
#bird
mean(Exobirdinvacost$meanoriFtree, na.rm = T)
median(Exobirdinvacost$meanoriFtree, na.rm = T) 

#mammal
mean(Exomammalinvacost$meanoriFtree, na.rm = T)
median(Exomammalinvacost$meanoriFtree, na.rm = T)

#not in invacost

mean(Exobirdnotinvacost$meanoriFtree, na.rm = T) 
median(Exobirdnotinvacost$meanoriFtree, na.rm = T) 

#mammal
mean(Exomammalnotinvacost$meanoriFtree, na.rm = T)
median(Exomammalnotinvacost$meanoriFtree, na.rm = T)


#phylo
#in invacost
#bird
mean(Exobirdinvacost$oriPtree, na.rm = T)
median(Exobirdinvacost$oriPtree, na.rm = T)

#mammal
mean(Exomammalinvacost$oriPtree, na.rm = T)
median(Exomammalinvacost$oriPtree, na.rm = T)

#not in invacost

mean(Exobirdnotinvacost$oriPtree, na.rm = T)
median(Exobirdnotinvacost$oriPtree, na.rm = T) 

#mammal
mean(Exomammalnotinvacost$oriPtree, na.rm = T)
median(Exomammalnotinvacost$oriPtree, na.rm = T)

####TESTS DE COMPARAISON DE MOYENNES####

library(ggpubr)
#BIRDS
##FUNCTIONAL ORI
#Normality test for each modality
shapiro.test(Exobirdinvacost$meanoriFtree)
ggqqplot(Exobirdinvacost$meanoriFtree)
shapiro.test(Exobirdnotinvacost$meanoriFtree)
ggqqplot(Exobirdnotinvacost$meanoriFtree)
#####PAS NORMALES --> Test U de mann whitney
birdsFUNC<-wilcox.test(Exobirdinvacost$meanoriFtree, 
                           Exobirdnotinvacost$meanoriFtree)
birdsFUNC  #pas de différences

##PHYLO ORI
shapiro.test(Exobirdinvacost$oriPtree)
ggqqplot(Exobirdinvacost$oriPtree)
shapiro.test(Exobirdnotinvacost$oriPtree)
ggqqplot(Exobirdnotinvacost$oriPtree)
#####PAS NORMALES --> Test U de mann whitney
birdsPHYLO<-wilcox.test(Exobirdinvacost$oriPtree, 
                           Exobirdnotinvacost$oriPtree)
birdsPHYLO  #Pas de différences entre les deux groupes

#MammalS
##FUNCTIONAL ORI
#Normality test for each modality
shapiro.test(Exomammalinvacost$meanoriFtree) ##invacostY=Y
ggqqplot(Exomammalinvacost$meanoriFtree)
shapiro.test(Exomammalnotinvacost$meanoriFtree)
ggqqplot(Exomammalnotinvacost$meanoriFtree)
#####PAS NORMALES --> Test U de mann whitney
mammalsFUNC<-wilcox.test(Exomammalinvacost$meanoriFtree, 
                       Exomammalnotinvacost$meanoriFtree)
mammalsFUNC  #pas de différences

##PHYLO ORI
shapiro.test(Exomammalinvacost$oriPtree)
ggqqplot(Exomammalinvacost$oriPtree)
shapiro.test(Exomammalnotinvacost$oriPtree)
ggqqplot(Exomammalnotinvacost$oriPtree)
#####PAS NORMALES --> Test U de mann whitney
mammalsPHYLO<-wilcox.test(Exomammalinvacost$oriPtree, 
                        Exomammalnotinvacost$oriPtree)
mammalsPHYLO  #Pas de différences entre les deux groupes

#Tree
ftreebirds <- ggplot(Exoticbirds, aes(x=log(meanoriFtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  labs(y="Presence in InvaCost", x="Functional originality (log)")+
  ggtitle("BIRDS" ) +
  theme_bw()+  theme(plot.title = element_text(size=11))+
  coord_flip()
ftreebirds

ftreemammals <- ggplot(Exoticmammals, aes(x=log(meanoriFtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  labs(y="Presence in InvaCost", x="Functional originality (log)")+
  ggtitle("MAMMALS" ) +
  theme_bw()+  theme(plot.title = element_text(size=11))+
  coord_flip()

ftreemammals

library(cowplot)
f<-plot_grid(ftreemammals + theme(legend.position="none"),
                ftreebirds + theme(legend.position="none"), labels=c("C","D"), ncol = 2, nrow = 1)

plot_grid(f, rel_widths = c(3, .4))

###phylogenetic originality####
#Tree
ptreebirds <- ggplot(Exoticbirds, aes(x=log(oriPtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  #scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y="Presence in InvaCost", x="Phylogenetic originality (log)")+
  ggtitle("BIRDS" ) + theme_bw()+
  theme(plot.title = element_text(size=11))+
  coord_flip()

ptreebirds


ptreemammals <- ggplot(Exoticmammals, aes(x=log(oriPtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  labs(y="Presence in InvaCost", x="Phylogenetic originality (log)")+
  ggtitle("MAMMALS" ) +
  guides(fill=guide_legend(title="Exotic status"))+
  theme_bw()+  theme(plot.title = element_text(size=11))+
  coord_flip()

ptreemammals

p<-plot_grid(ptreemammals + theme(legend.position="none"),
                ptreebirds + theme(legend.position="none"), labels=c("A", "B"), ncol = 2, nrow = 1)

plot_grid(p, rel_widths = c(3, .4))


######threat status####
##Birds
#count the number of species per threat
Exoticbirds$invacostY[is.na(Exoticbirds$invacostY)]<-"N"
Nb<-rep(1, length(Exoticbirds[,1]))
Exoticbirds$Nb<-Nb
threatbird<-ddply(Exoticbirds, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb))

#####Compare several mean
# resaovbird<-aov(Number~invacostY,data=threatbird)
# summary(resaovbird)
# plot(resaovbird)
# ###Les conditions d'égalité des variances et de distribution normale des résidus du modèle ne sont pas respectés
# ##test de kruskal wallis
# kruskal.test(Number~invacostY,data=threatbird) ###il ne semble pas avoir assez de données pour tester cet aspect

threatbirds <- threatbird %>% # Reorder following a precise order
  mutate(redlistCategory_version_2020.2 = fct_relevel(redlistCategory_version_2020.2, 
                             "Least Concern","Near Threatened","Vulnerable",  
                             "Endangered", "Critically Endangered")) %>%
  ggplot(aes(x=redlistCategory_version_2020.2, fill=invacostY, y=Number)) + 
  geom_jitter(shape=16, size=1, color="black", position=position_jitter(0))+
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("THREAT- BIRDS" ) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  guides(fill=guide_legend(title="Presence in InvaCost"))+
  labs(y="Number of species")+
  scale_fill_manual(values=c("black","grey"))
  #scale_fill_discrete(labels=c("Yes","No"))
threatbirds

##mammals
#count the number of species per threat
Exoticmammals$invacostY[is.na(Exoticmammals$invacostY)]<-"N"
Nb2<-rep(1, length(Exoticmammals[,1]))
Exoticmammals$Nb<-Nb2
threatmammal<-ddply(Exoticmammals, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb))

#subset(Exoticmammals,redlistCategory_version_2020.2=="Near Threatened"& invacostY=="Y" )

##ANOVA TEST
# resaovmammal<-aov(Number~invacostY,data=threatmammal)
# summary(resaovmammal)
# plot(resaovmammal)
# ###Les conditions d'égalité des variances et de distribution normale des résidus du modèle d$ne sont pas respectés
# ##test de kruskal wallis
# kruskal.test(Number~invacostY,data=threatmammal)

threatmammals <- threatmammal%>% # Reorder following a precise order
  mutate(redlistCategory_version_2020.2 = fct_relevel(redlistCategory_version_2020.2, 
                                                      "Least Concern","Near Threatened","Vulnerable",  
                                                      "Endangered", "Critically Endangered")) %>%
  ggplot(aes(x=redlistCategory_version_2020.2, fill=invacostY, y=Number)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_bar(position = "dodge", stat="identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title.x = element_blank())+
  labs(y="Number of species")+
  ggtitle("THREAT- MAMMALS" ) +
  guides(fill=guide_legend(title="Presence in InvaCost"))+
  scale_fill_manual(values=c("black","grey"))
  #scale_fill_discrete(labels=c("Yes","No"))

threatmammals


t<-plot_grid(threatmammals,
             threatbirds, labels=c("E","F"), ncol = 1, nrow = 2)

plot_grid(t, rel_widths = c(3, .4))


##save plots in pdf
pdf("./Figure1.pdf")
plot_grid(p, rel_widths = c(3, .4))
plot_grid(f, rel_widths = c(3, .4))
plot_grid(t, rel_widths = c(3, .4))
dev.off()

jpeg("./Figure1.jpeg", width=7, height=12, units="in", res=300)
plot_grid(ptreebirds,ptreemammals, ftreebirds, ftreemammals, threatmammals + theme(legend.position="none") ,
          threatbirds+ theme(legend.position="none"), rel_widths = c(2, 2), ncol = 2, nrow = 3, labels = c("A","B","C","D","E","F"))
dev.off()

plot_grid(threatmammals + theme(legend.position="none") ,
          threatbirds+ theme(legend.position="bottom"), ncol = 1, nrow = 2)
