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

dataall<-read.table("./dataAllF.txt")
iucnsp<-read.csv("./IUCN_exotic_mammal_bird.csv") ##from IUCN red list (bird + mammals) #314 of the databse in our global base
gavia<-read.csv("./GAVIA/GAVIA_exotic_bird.csv") #only birds, from there : https://www.nature.com/articles/sdata201741#Tab2  #527 species of the database in our global base
gisd<-read.csv("./gisd_exotic_mammal_bird.csv", sep=";") #from gisd (mammals+birds) #86 of the databse in our global base

##### Filter GAVIA - exotic bird database #######527 species
#Remove "unsuccessfull" and "unknow" categories
newgavia <- gavia[which(gavia$StatusCat!='Unsuccessful' & gavia$StatusCat!='Unknown'),] #527 species
##only keep one repetition per species
gaviasp<-unique(newgavia$Binomial)

###Reunite all databasis
Allbasis<-c(levels(iucnsp$scientificName),levels(gisd$Species), levels(factor(gaviasp))) #all exotic species found in all basis
Exotic<-dataall$Species%in% Allbasis #which species are in the global database
dataall<-add_column(dataall, Exotic, .after = "oriPtree") #add a column with TRUE = exotic species and FALSE = not an exotic species
#Verify the number of exotic species in the global database
trueexo<-subset(dataall, Exotic==TRUE)
Allexoticsp<-unique(trueexo[c("Species", "className")])  
summary(Allexoticsp$className)

##3 repetitions


########FIGURE 1######


Exotic<-filter(dataall, Exotic==TRUE)
Exoticmammals<-filter(Exotic, className=="MAMMALIA")
Exoticbirds<-filter(Exotic, className=="AVES")

####MEAN AND MEDIAN FOR EACH ORIGINALITY

##Filter
Exobirdinvacost<-filter(Exoticbirds,invacostY=="Y")
Exomammalinvacost<-filter(Exoticmammals,invacostY=="Y")

Exobirdnotinvacost<-anti_join(Exoticbirds, Exobirdinvacost)
Exomammalnotinvacost<-anti_join(Exoticmammals, Exomammalinvacost)


#FUNCTIONAL
#in invacost
#bird
mean(Exobirdinvacost$meanoriFtree, na.rm = T)#0.01274159 #20 values
median(Exobirdinvacost$meanoriFtree, na.rm = T) #0.003368483 #20 values

#mammal
mean(Exomammalinvacost$meanoriFtree, na.rm = T) #0.006678582 #32 values
median(Exomammalinvacost$meanoriFtree, na.rm = T) #0.001581809 #32 values

#not in invacost
#bird
mean(Exobirdnotinvacost$meanoriFtree, na.rm = T)#0.009675377 
median(Exobirdnotinvacost$meanoriFtree, na.rm = T) #0.003834404 

#mammal
mean(Exomammalnotinvacost$meanoriFtree, na.rm = T) #0.007610121
median(Exomammalnotinvacost$meanoriFtree, na.rm = T) #0.001509373

#verify wich species are not considered exotic but in invacost:
ExoticInvacost<-filter(Exotic, invacostY == "Y") #52 obs of exotic speces in invacost
Invacostobs<-filter(dataall, invacostY == "Y")#58 obs invacost sp in the global database
invnotindata<-!Invacostobs$Species %in% ExoticInvacost$Species #find the name of the not exotic but in invacost species
Invacostobs[invnotindata==TRUE,1]
#==> [1] Callosciurus erythraeus Hystrix brachyura      
#[3] Sciurus niger           Atelerix albiventris   

#Tree
ftreebirds <- ggplot(Exoticbirds, aes(x=log(meanoriFtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  labs(y="Presence in InvaCost", x="Functional originality (log)")+
  ggtitle("BIRDS" ) +
  theme_bw()+  theme(plot.title = element_text(size=11))+
  coord_flip()

ftreebirds ##42 species have no Funct. originality value.


ftreemammals <- ggplot(Exoticmammals, aes(x=log(meanoriFtree), y=invacostY)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_boxplot(alpha = 0.5)+
  scale_y_discrete(labels=c("Yes", "No")) +
  labs(y="Presence in InvaCost", x="Functional originality (log)")+
  ggtitle("MAMMALS" ) +
  theme_bw()+  theme(plot.title = element_text(size=11))+
  coord_flip()

ftreemammals ##8 species have no Funct. originality value.

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

ptreebirds ##42 species have no Phylo. originality value.


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
Nb<-rep(1, length(Exoticbirds[,1]))
Exoticbirds$Nb<-Nb
threatbird<-ddply(Exoticbirds, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb))


threatbirds <- ggplot(threatbird, aes(x=reorder(redlistCategory_version_2020.2, -Number), fill=invacostY, y=Number)) + 
  geom_jitter(shape=16, size=1, color="black", position=position_jitter(0))+
  geom_bar(position = "dodge", stat="identity") +
  labs(y="Number of species")+
  ggtitle("THREAT- BIRDS" ) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title = element_blank())+
  guides(fill=guide_legend(title="Presence in InvaCost"))+
  scale_fill_discrete(labels=c("Yes","No"))
threatbirds


##mammals
#count the number of species per threat
Nb2<-rep(1, length(Exoticmammals[,1]))
Exoticmammals$Nb<-Nb2
threatmammal<-ddply(Exoticmammals, c("redlistCategory_version_2020.2", "invacostY"), summarise, Number=sum(Nb))

threatmammals <- ggplot(threatmammal, aes(x=reorder(redlistCategory_version_2020.2, -Number), fill=invacostY, y=Number)) + 
  geom_jitter(shape=16, size=1, color="grey", position=position_jitter(0))+
  geom_bar(position = "dodge", stat="identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, hjust=1), axis.title = element_blank())+
  labs(y="Number of species")+
  ggtitle("THREAT- MAMMALS" ) +
  guides(fill=guide_legend(title="Presence in InvaCost"))+
  scale_fill_discrete(labels=c("Yes","No"))

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

jpeg("./Figure1.jpeg", res=600)
plot_grid(p, rel_widths = c(3, .4))
plot_grid(f, rel_widths = c(3, .4))
plot_grid(t, rel_widths = c(3, .4))
dev.off()

jpeg("./Figure1all.jpeg", width=7, height=12, units="in", res=300)
plot_grid(ptreebirds,ptreemammals, ftreebirds, ftreemammals, threatmammals + theme(legend.position="none") ,
          threatbirds+ theme(legend.position="none"), rel_widths = c(2, 2), ncol = 2, nrow = 3, labels = c("A","B","C","D","E","F"))
dev.off()

plot_grid(threatmammals + theme(legend.position="none") ,
          threatbirds+ theme(legend.position="bottom"), ncol = 1, nrow = 2)




