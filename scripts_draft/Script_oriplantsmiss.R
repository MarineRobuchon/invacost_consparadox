df<-read.table(file.choose(), sep=";", header=T, row.names=1 ) #  Careful of the path!
dim(df)
Toremove <- (1:262466)[apply(planttraits[1:6], 1, function(x) all(is.na(x)))]  # I remove species with no data at all.

length(Toremove) # This leads to the removal of 1744 species.
## load packages ----
library(adiv)
library(StatMatch)
library(ape)
library(phylobase)
library(cluster)
library(dplyr)
library(Rarity)
library(kader)
library(ade4)


Tab <- planttraits[-Toremove, -7]

rownames(Tab) <- planttraits[-Toremove, 7]

DISJmiss<- acm.disjonctif(Tab)



FAC <- as.vector(unlist(lapply(strsplit(names(DISJmiss),"\\."), function(x) x[1])))

PRESmiss <- apply(DISJmiss, 1 , function(x) tapply(x, as.factor(FAC), sum))





FUNoriplantsmiss <- function(i){
  
  VV <- sapply(1:ncol(DISJmiss), function(k) DISJmiss[i,k]*DISJmiss[-i,k])
  
  WW <- colSums(PRESmiss[,i] * PRESmiss[,-i])
  
  if(all(WW<1)) return(NA)
  
  VVc <- VV[WW>0, ]
  
  WWc <- WW[WW>0]
  
  XX <- sweep(VVc, 1, WWc, "/")
  
  XX <- mean(1-rowSums(XX))
  
  cat(XX, file= "sauvoriplants.txt", sep="\n", append=TRUE)
  
  return(XX)
  
}



oriplantsmiss <- sapply(1:nrow(Tab), FUNoriplantsmiss)

names(oriplantsmiss) <- rownames(Tab)

write.csv(oriplantsmiss, "oriplantsmiss.csv")
