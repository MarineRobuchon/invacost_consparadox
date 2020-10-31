library(dplyr)
library(stringr)
library(tibble)
library(plyr)
library(ggplot2)
library(MASS)
library(scales)
library(forcats)

setwd("./outputs")

############# Load data ##############

dataall<-read.table("./dataAllF.txt")
iucnsp<-read.csv("./IUCN_exotic_mammal_bird.csv") ##from IUCN red list (bird + mammals) 
gavia<-read.csv("./GAVIA_exotic_bird.csv") #only birds, from there : https://www.nature.com/articles/sdata201741#Tab2
gisd<-read.csv("./gisd_exotic_mammal_bird.csv", sep=";") #from gisd (mammals+birds) 
seebensdata<-read.csv("D:/these/Review/GASFRD_v1.2.csv")

##BUILT THE EXOTIC SPECIES DATABASE####

###KEEP ONLY mammals from the seebens database
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

# #verify wich species are not considered exotic but in InvaCostt:
# ExoticInvacost<-filter(Exotic, invacostY == "Y") 
# Invacostobs<-filter(dataall, invacostY == "Y")
# invnotindata<-!Invacostobs$Species %in% ExoticInvacost$Species #find the name of the not exotic but in invacost species
# Invacostobs[invnotindata==TRUE,1]
# #==> "Atelerix albiventris"   "Phascolarctos cinereus" "Aquila chrysaetos"

#add species are not considered exotic but in invacost in InvaCost
Exotic<-filter(dataall, Exotic2==TRUE)
Exoticmammals<-filter(Exotic, className=="MAMMALIA")
notexoinvacostM<-subset(dataall, Species=="Phascolarctos cinereus"|Species=="Atelerix albiventris")
Exoticmammals<-rbind(Exoticmammals, notexoinvacostM)

Exoticbirds<-filter(Exotic, className=="AVES")
notexoinvacost<-subset(dataall, Species=="Aquila chrysaetos")
Exoticbirds<-rbind(Exoticbirds, notexoinvacost)


## Filter mammals and birds in and not in InvaCost
Exobirdinvacost<-filter(Exoticbirds,invacostY=="Y")
Exomammalinvacost<-filter(Exoticmammals,invacostY=="Y")

Exobirdnotinvacost<-anti_join(Exoticbirds, Exobirdinvacost)
Exomammalnotinvacost<-anti_join(Exoticmammals, Exomammalinvacost)

####Who are the outliers?##
Exobirdinvacost[order(Exobirdinvacost[,"oriPtree"],decreasing=T), ]
Exobirdnotinvacost[order(Exobirdnotinvacost[,"oriPtree"],decreasing=T), ]

Exomammalinvacost[order(Exomammalinvacost[,"oriPtree"],decreasing=T), ]
Exomammalnotinvacost[order(Exomammalnotinvacost[,"oriPtree"],decreasing=T), ]

####MEAN AND MEDIAN FOR EACH ORIGINALITY####

groups<-list(Exobirdinvacost,Exobirdnotinvacost,Exomammalinvacost, Exomammalnotinvacost)
ori<-c("meanoriFtree", "oriPtree")
i=0
xx<-0
meansp<-data.frame(bird_invacost=integer(),  
                  mammal_invacost=integer(),
                   bird_not_in_invacost=integer(),
                  mammal_not_in_invacost=integer())
mediansp<-data.frame(bird_invacost=integer(),  
                     mammal_invacost=integer(),
                     bird_not_in_invacost=integer(),
                     mammal_not_in_invacost=integer())


for (orig in ori) {
  xx<-xx+1
  yy<-0
  for (group in 1:4 ){
    yy=yy+1
    meansp[xx,yy]<-mean(groups[[group]][,orig], na.rm = T)
    mediansp[xx,yy]<-median(groups[[group]][,orig], na.rm = T)
  }
}

row.names(meansp)<-c("Functional","Phylogeny")
row.names(mediansp)<-c("Functional","Phylogeny")


meansp #TABLE OF MEANS
mediansp #TABLE OF MEDIANS



####STATISTICAL TESTS####

#BIRDS
###FUNCTIONAL ORI
#Normality test for each modality to choose the test
shapiro.test(log(Exobirdinvacost$meanoriFtree))
ggqqplot(log(Exobirdinvacost$meanoriFtree))
shapiro.test(log(Exobirdnotinvacost$meanoriFtree))
ggqqplot(log(Exobirdnotinvacost$meanoriFtree))#####none of the modalities are normaly distributed --> Mann whitney test

birdsFUNC<-wilcox.test(log(Exobirdinvacost$meanoriFtree), 
                           log(Exobirdnotinvacost$meanoriFtree))
birdsFUNC  

###PHYLO ORI
#Normality test for each modality to choose the test
shapiro.test(log(Exobirdinvacost$oriPtree))
ggqqplot(log(Exobirdinvacost$oriPtree))
shapiro.test(log(Exobirdnotinvacost$oriPtree))
ggqqplot(log(Exobirdnotinvacost$oriPtree))#####none of the modalities are normaly distributed --> Mann whitney test

birdsPHYLO<-wilcox.test(log(Exobirdinvacost$oriPtree), 
                           log(Exobirdnotinvacost$oriPtree))
birdsPHYLO  

#MAMMALS
##FUNCTIONAL ORI
#Normality test for each modality to choose the test
shapiro.test(log(Exomammalinvacost$meanoriFtree)) 
ggqqplot(log(Exomammalinvacost$meanoriFtree))
shapiro.test(log(Exomammalnotinvacost$meanoriFtree))
ggqqplot(log(Exomammalnotinvacost$meanoriFtree))#####none of the modalities are normaly distributed --> Mann whitney test

mammalsFUNC<-wilcox.test(log(Exomammalinvacost$meanoriFtree), 
                       log(Exomammalnotinvacost$meanoriFtree))
mammalsFUNC  

###PHYLO ORI
#Normality test for each modality to choose the test
shapiro.test(log(Exomammalinvacost$oriPtree))
ggqqplot(log(Exomammalinvacost$oriPtree))
shapiro.test(log(Exomammalnotinvacost$oriPtree))
ggqqplot(log(Exomammalnotinvacost$oriPtree)) #####none of the modalities are normaly distributed --> Mann whitney test

mammalsPHYLO<-wilcox.test(log(Exomammalinvacost$oriPtree), 
                        log(Exomammalnotinvacost$oriPtree))
mammalsPHYLO  


#####FIGURES####

#####FUNCTIONNAL ORIGINALITY
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

###PHYLOGENETIC ORIGINALITY
ptreebirds <- ggplot(Exoticbirds, aes(x=log(oriPtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  #scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y="Presence in InvaCost", x="Phylogenetic originality (Ma) (log)")+
  ggtitle("BIRDS" ) + theme_bw()+
  theme(plot.title = element_text(size=11))+
  coord_flip()

ptreebirds


ptreemammals <- ggplot(Exoticmammals, aes(x=log(oriPtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  labs(y="Presence in InvaCost", x="Phylogenetic originality (Ma) (log)")+
  ggtitle("MAMMALS" ) +
  guides(fill=guide_legend(title="Exotic status"))+
  theme_bw()+  theme(plot.title = element_text(size=11))+
  coord_flip()

ptreemammals

p<-plot_grid(ptreemammals + theme(legend.position="none"),
                ptreebirds + theme(legend.position="none"), labels=c("A", "B"), ncol = 2, nrow = 1)

plot_grid(p, rel_widths = c(3, .4))


######THREAT STATUS
##BIRDS
#Prepare data to do the plot
Exoticbirds$invacostY[is.na(Exoticbirds$invacostY)]<-"N"
Nb<-rep(1, length(Exoticbirds[,1]))
Exoticbirds$Nb<-Nb
threatbird<-ddply(Exoticbirds, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb))


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

##MAMMALS
#Prepare data to do the plot
Exoticmammals$invacostY[is.na(Exoticmammals$invacostY)]<-"N"
Nb2<-rep(1, length(Exoticmammals[,1]))
Exoticmammals$Nb<-Nb2
threatmammal<-ddply(Exoticmammals, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb))



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

##save plots in jpeg
jpeg("./Figure1.jpeg", width=7, height=12, units="in", res=300)
plot_grid(ptreemammals,ptreebirds, ftreemammals, ftreebirds, threatmammals + theme(legend.position="none") ,
          threatbirds+ theme(legend.position="none"), rel_widths = c(2, 2), ncol = 2, nrow = 3, labels = c("A","B","C","D","E","F"))
dev.off()

plot_grid(threatmammals + theme(legend.position="none") ,
          threatbirds+ theme(legend.position="bottom"), ncol = 1, nrow = 2)
