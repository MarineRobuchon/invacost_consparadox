setwd("~/Desktop/PHYLOGENY INVACOST")
TAB=read.table("PHYLOCOST.csv", h=T, sep=";", dec=",", na.strings = "NA")
library(ggplot2)
#------------------DATASET MAMMALS-----------------------------------------------
TAB1=subset(TAB, subset = className == "MAMMALIA")
MAM_DAM_QUA=quantile(TAB1$Average.annual.cost_damage, probs = seq(0, 1, 0.25), na.rm = T)
MAM_MAN_QUA=quantile(TAB1$Average.annual.cost_damage, probs = seq(0, 1, 0.25), na.rm = T)
#50% des esp??ces de mammif??res les plus couteuses en termes de dommage
MAM_DAM_QUA_pp50=subset(TAB1, subset = Average.annual.cost_damage >= MAM_DAM_QUA[3])
#50% des esp??ces de mammif??res les plus couteuses en termes de MANAGEMENT
MAM_MAN_QUA_pp50=subset(TAB1, subset = Average.annual.cost_management >= MAM_MAN_QUA[3])
#------------------DATASET BIRDS-----------------------------------------------
TAB2=subset(TAB, subset = className == "AVES")
AVES_DAM_QUA=quantile(TAB2$Average.annual.cost_damage, probs = seq(0, 1, 0.25), na.rm = T)
AVES_MAN_QUA=quantile(TAB2$Average.annual.cost_management, probs = seq(0, 1, 0.25), na.rm = T)
#50% des esp??ces d'oiseaux les plus couteuses en termes de dommage
AVES_DAM_QUA_pp50=subset(TAB2, subset = Average.annual.cost_damage >= AVES_DAM_QUA[3])
#50% des esp??ces d'oiseaux les plus couteuses en termes de MANAGEMENT
AVES_MAN_QUA_pp50=subset(TAB2, subset = Average.annual.cost_management >= AVES_MAN_QUA[3])

#------------BARPLOTS------------------------
#----------FIGURE 4 : Management-----------
##Mammals / Phylogenetic
p1=ggplot(data=MAM_MAN_QUA_pp50, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree),linetype="Median originality score"), color="red",size=2)+
  coord_flip()
p1

##AVES / Phylogenetic
p1=ggplot(data=AVES_MAN_QUA_pp50, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Management)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree),linetype="Median originality score"), color="red",size=2)+
  coord_flip()
p1

##Mammals / Functional

p1=ggplot(data=MAM_MAN_QUA_pp50, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="C MAMMALS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree),linetype="Median originality score"), color="red",size=2)+
  coord_flip()
p1

##AVES / Functional
p1=ggplot(data=AVES_MAN_QUA_pp50, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="D BIRDS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB2$meanoriFtree),linetype="Median originality score"), color="red",size=2)+
  coord_flip()
p1


#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_MAN_QUA_pp50, aes(x=Species, y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("E  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25),   legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=30))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management (2017 US$)") + theme(axis.title.x = element_text(size = 20, face = "bold"),
                                                                                      axis.title.y = element_text(size = 20, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 13))
p1_4 + scale_x_discrete(limits=c("Sciurus carolinensis","Myocastor coypus","Mus musculus","Vulpes vulpes","Ondatra zibethicus","Canis lupus"))

##-----AVES------
p1=ggplot(data=AVES_MAN_QUA_pp50, aes(x=Species, y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) + ggtitle("F  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25),   legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=30))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Management (2017 US$)") + theme(axis.title.x = element_text(size = 20, face = "bold"),
                                                                                      axis.title.y = element_text(size = 20, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1_4
p1_4 + scale_x_discrete(limits=c("Acridotheres tristis","Corvus splendens","Aquila chrysaetos","Branta canadensis","Myiopsitta monachus","Sturnus vulgaris","Oxyura jamaicensis","Columba livia"))

#----------FIGURE 4 : Damage-----------
##Mammals / Phylogenetic
p1=ggplot(data=MAM_DAM_QUA_pp50, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB1$oriPtree),linetype="Median originality score"), color="red",size=2)+
  coord_flip()
p1

##AVES / Phylogenetic
p1=ggplot(data=AVES_DAM_QUA_pp50, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Damage)", y = "Phylogenetic originality scores (Ma)")+
  geom_hline(aes(yintercept=median(TAB2$oriPtree),linetype="Median originality score"), color="red",size=2)+
  coord_flip()
p1

##Mammals / Functional

p1=ggplot(data=MAM_DAM_QUA_pp50, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="C MAMMALS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree),linetype="Median originality score"), color="red",size=2)+
  coord_flip()
p1

##AVES / Functional
p1=ggplot(data=AVES_DAM_QUA_pp50, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="D BIRDS", x="Most costly Species (Damage)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(TAB2$meanoriFtree),linetype="Average originality score"), color="red",size=2)+
  coord_flip()
p1

#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_DAM_QUA_pp50, aes(x=Species, y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("E  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25),   legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=30))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage (2017 US$)") + theme(axis.title.x = element_text(size = 20, face = "bold"),
                                                                                      axis.title.y = element_text(size = 20, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1_4 + scale_x_discrete(limits=c("Paguma larvata","Rattus norvegicus","Mus musculus","Ondatra zibethicus","Vulpes vulpes","Canis lupus","Sus scrofa","Oryctolagus cuniculus","Rattus rattus"))

##-----AVES------
p1=ggplot(data=AVES_DAM_QUA_pp50, aes(x=Species, y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("F  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25),   legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=30))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage (2017 US$)") + theme(axis.title.x = element_text(size = 20, face = "bold"),
                                                                                      axis.title.y = element_text(size = 20, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1_4 + scale_x_discrete(limits=c("Phasianus colchicus","Branta canadensis","Psittacula krameri","Ploceus melanocephalus","Columba livia","Sturnus vulgaris"))


#--------FIGURE 6 :STATUT DE MENACE---------
#--------PHYLOGENETIC & FUNCTIONAL----------------
library(dplyr)
MENACE_DOM_MAM=filter(TAB1, redlistCategory_version_2020.2 != "Least Concern")
MENACE_DOM_AVES=filter(TAB2, redlistCategory_version_2020.2 != "Least Concern")
#----PHYLOGENETIC------
#MAMMALS Phylogenetic
p1=ggplot(data=MENACE_DOM_MAM, aes(x=Species, y=oriPtree, fill=redlistCategory_version_2020.2)) +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green", "yellow")) + 
  labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25), legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=20)) +
  geom_hline(aes(yintercept=median(TAB1$oriPtree),linetype="Median originality score (Ma)"), color="red",size=2) +
  coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20)) + 
  labs(x = "Threatened exotic mammalian species in INVACOST", y = "Phylogenetic originality scores (Ma)") + 
  theme(axis.title.x = element_text(size = 20, face = "bold"), axis.title.y = element_text(size = 23, face = "bold")) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1 + scale_x_discrete(limits=c("Hemitragus jemlahicus","Oryctolagus cuniculus","Ammotragus lervia","Rangifer tarandus","Phascolarctos cinereus"))
#BIRDS Phylogenetic- (Aucune esp??ce d'oisieaux n'est en danger)
p1=ggplot(data=MENACE_DOM_AVES, aes(x=Species, y=oriPtree, fill=redlistCategory_version_2020.2)) +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green", "yellow")) + 
  labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25), legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=20)) +
  geom_hline(aes(yintercept=median(TAB2$oriPtree),linetype="Median originality score (Ma)"), color="red",size=2) +
  coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20)) + 
  labs(x = "Threatened exotic birds species in INVACOST", y = "Phylogenetic originality scores (Ma)") + 
  theme(axis.title.x = element_text(size = 20, face = "bold"), axis.title.y = element_text(size = 23, face = "bold")) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1
#-------FUNCTIONAL----------
##Mammals - functional
p1=ggplot(data=MENACE_DOM_MAM, aes(x=Species, y=meanoriFtree, fill=redlistCategory_version_2020.2)) +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green", "yellow")) + 
  labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25), legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=20)) +
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree),linetype="Median originality score (Ma)"), color="red",size=2) +
  coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20)) + 
  labs(x = "Threatened exotic mammalian species in INVACOST", y = "Functional originality scores (Ma)") + 
  theme(axis.title.x = element_text(size = 20, face = "bold"), axis.title.y = element_text(size = 23, face = "bold")) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1 + scale_x_discrete(limits=c("Oryctolagus cuniculus","Phascolarctos cinereus","Rangifer tarandus","Hemitragus jemlahicus","Ammotragus lervia"))
##Birds - functional
p1=ggplot(data=MENACE_DOM_AVES, aes(x=Species, y=meanoriFtree, fill=redlistCategory_version_2020.2)) +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green", "yellow")) + 
  labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25), legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=20)) +
  geom_hline(aes(yintercept=median(TAB1$meanoriFtree),linetype="Median originality score (Ma)"), color="red",size=2) +
  coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20)) + 
  labs(x = "Threatened exotic bird species in INVACOST", y = "Fuctional originality scores (Ma)") + 
  theme(axis.title.x = element_text(size = 20, face = "bold"), axis.title.y = element_text(size = 23, face = "bold")) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1

#Mammals - Damage
#MEDIANS CALCULATE
TAB1_filter=filter(TAB1, Average.annual.cost_damage != "NA")#Filter les NA pour calculer la Moyenne
p1=ggplot(data=MENACE_DOM_MAM, aes(x=Species, y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green", "yellow")) + 
  labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25), legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=20)) +
  geom_hline(aes(yintercept=median(TAB1_filter$Average.annual.cost_damage),linetype="Median cost Damage (2017 US$)"), color="red",size=2) +
  coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20)) + 
  labs(x = "Threatened exotic mammalian species in INVACOST", y = "Average annual cost Damage (2017 US$)") + 
  theme(axis.title.x = element_text(size = 20, face = "bold"), axis.title.y = element_text(size = 23, face = "bold")) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1 + scale_x_discrete(limits=c("Hemitragus jemlahicus","Ammotragus lervia","Rangifer tarandus","Phascolarctos cinereus","Oryctolagus cuniculus"))
#Mammals - Management
TAB1_filter=filter(TAB1, Average.annual.cost_management != "NA")#Filter les NA pour calculer la Moyenne
p1=ggplot(data=MENACE_DOM_MAM, aes(x=Species, y=Average.annual.cost_management, fill=redlistCategory_version_2020.2)) +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "yellow","green")) + 
  labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25), legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=20)) +
  geom_hline(aes(yintercept=median(TAB1_filter$Average.annual.cost_management),linetype="Median cost Management (2017 US$)"), color="red",size=2) +
  coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20)) + 
  labs(x = "Threatened exotic mammalian species in INVACOST", y = "Average annual cost Management (2017 US$)") + 
  theme(axis.title.x = element_text(size = 20, face = "bold"), axis.title.y = element_text(size = 23, face = "bold")) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1 + scale_x_discrete(limits=c("Oryctolagus cuniculus","Ammotragus lervia","Rangifer tarandus","Phascolarctos cinereus","Hemitragus jemlahicus"))


#------FIGURE 7 : MOST ORIGINAL SPECIES-------
#------------------DATASET MAMMALS-----------------------------------------------
setwd("~/Desktop/PHYLOGENY INVACOST")
TAB=read.table("PHYLOCOST.csv", h=T, sep=";", dec=",", na.strings = "NA")
library(ggplot2)
TAB1=subset(TAB, subset = className == "MAMMALIA")
#Les mammif??res les plus originaux sur le plan phylog??n??tique
MAM_MOST_ORIPTREE=subset(TAB1, subset = oriPtree >= median(TAB1$oriPtree))
#Les mammif??res les plus originaux sur le plan fonctionnel
MAM_MOST_MEANORIFTREE=subset(TAB1, subset = meanoriFtree >= median(TAB1$meanoriFtree))
#------------------DATASET BIRDS-----------------------------------------------
TAB2=subset(TAB, subset = className == "AVES")
#Les mammif??res les plus originaux sur le plan phylog??n??tique
AVES_MOST_ORIPTREE=subset(TAB2, subset = oriPtree >= median(TAB2$oriPtree))
#Les mammif??res les plus originaux sur le plan fonctionnel
AVES_MOST_MEANORIFTREE=subset(TAB2, subset = meanoriFtree >= median(TAB2$meanoriFtree))

#------------BARPLOTS------------------------
###--------------------------DAMAGE-----------------------------
##Damage costs of the most phylogenetically original mammals
#MEDIANS CALCULATE
TAB1_filter=filter(TAB1, Average.annual.cost_damage != "NA")#Filter les NA pour calculer la Moyenne
TAB2_filter=filter(TAB2, Average.annual.cost_damage != "NA")#Filter les NA pour calculer la Moyenne
p1=ggplot(data=MAM_MOST_ORIPTREE, aes(x= reorder(Species, Average.annual.cost_damage), y=Average.annual.cost_damage))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most phylogenetically original mammals", y = "Average annual cost damage (2017 US$)")+
  geom_hline(aes(yintercept=median(TAB1_filter$Average.annual.cost_damage),linetype="Average cost Damage"), color="red",size=2)+
  coord_flip()
p1 + scale_x_discrete(limits=c("Phascolarctos cinereus","Myocastor coypus","Atelerix albiventris","Glis glis","Sciurus vulgaris","Sciurus niger","Rattus exulans","Hystrix brachyura","Nyctereutes procyonoides","Nasua nasua","Mustela erminea","Castor canadensis","Neovison vison","Sciurus carolinensis","Procyon lotor","Trichosurus vulpecula","Paguma larvata","Sus scrofa"))
##Damage costs of the most phylogenetically original birds
p1=ggplot(data=AVES_MOST_ORIPTREE, aes(x= reorder(Species, Average.annual.cost_damage), y=Average.annual.cost_damage))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="B BIRDS", x="Most phylogenetically original birds", y = "Average annual cost damage (2017 US$)")+
  geom_hline(aes(yintercept=mean(TAB2_filter$Average.annual.cost_damage),linetype="Average cost Damage"), color="red",size=2)+
  coord_flip()
p1  + scale_x_discrete(limits=c("Threskiornis aethiopicus","Meleagris gallopavo","Cygnus atratus","Aquila chrysaetos","Cygnus olor","Pycnonotus cafer","Corvus splendens","Pycnonotus sinensis","Myiopsitta monachus","Sturnus vulgaris"))
##Damage costs of the most functionally original mammals----------------------
p1=ggplot(data=MAM_MOST_MEANORIFTREE, aes(x= reorder(Species, Average.annual.cost_damage), y=Average.annual.cost_damage))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="C MAMMALS", x="Most functionally original mammals", y = "Average annual cost damage (2017 US$)")+
  geom_hline(aes(yintercept=median(TAB1_filter$Average.annual.cost_damage),linetype="Average cost Damage"), color="red",size=2)+
  coord_flip()
p1 + scale_x_discrete(limits=c("Atelerix albiventris","Glis glis","Sciurus vulgaris","Sciurus niger","Nyctereutes procyonoides","Nasua nasua","Callosciurus erythraeus","Herpestes javanicus","Mustela erminea","Macaca mulatta","Neovison vison","Sciurus carolinensis","Procyon lotor","Paguma larvata","Ondatra zibethicus","Vulpes vulpes","Canis lupus","Sus scrofa"))
#Damage costs of the most functionally original birds
p1=ggplot(data=AVES_MOST_MEANORIFTREE, aes(x= reorder(Species, Average.annual.cost_damage), y=Average.annual.cost_damage))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="D BIRDS", x="Most functionally original birds", y = "Average annual cost damage (2017 US$)")+
  geom_hline(aes(yintercept=median(TAB2_filter$Average.annual.cost_damage),linetype="Average cost Damage"), color="red",size=2)+
  coord_flip()
p1 + scale_x_discrete(limits=c("Psittacula krameri","Meleagris gallopavo","Aquila chrysaetos","Cygnus olor","Pycnonotus cafer","Pycnonotus sinensis","Corvus splendens","Pycnonotus jocosus","Acridotheres tristis","Sturnus vulgaris"))
###----------------------------MANAGEMENT---------------------------
##Management costs of the most phylogenetically original mammals
p1=ggplot(data=MAM_MOST_ORIPTREE, aes(x= reorder(Species, Average.annual.cost_management), y=Average.annual.cost_damage))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=20),
        axis.text.x = element_text(face = "bold", color = "black",size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most phylogenetically original mammals", y = "Average annual cost Management (2017 US$)")+
  geom_hline(aes(yintercept=median(TAB1_filter$Average.annual.cost_damage),linetype="Average cost Management"), color="red",size=2)+
  coord_flip()
p1
