#############################################################################################
# script to explore mammal and bird species in Invacost
#############################################################################################

### load data ----
data_invacost <- read.csv2(paste0(getwd(), "/data/INVACOST-Full-dataset.csv"))

data_invacost[data_invacost$Species,]

levels(factor(data_invacost$Species))

mixed <- unique(union(grep("/", data_invacost$Species, value = TRUE),
               grep("sp.", data_invacost$Species, value = TRUE))) # levels of "species" to be discarded beacause they are mixed
mixed

str(data_invacost$Species)

data_invacost_species <- data_invacost[!data_invacost$Species %in% mixed ,]
levels(factor(data_invacost_species$Species)) # ok

nrow(data_invacost_species[data_invacost_species$Class=="Mammalia",]) # 469 estimations
levels(factor(data_invacost_species$Species[data_invacost_species$Class=="Mammalia"])) # 34 species

nrow(data_invacost_species[data_invacost_species$Class=="Aves",]) # 39 estimations
levels(factor(data_invacost_species$Species[data_invacost_species$Class=="Aves"])) # 14 species

levels(factor(data_invacost_species$Species[data_invacost_species$Class=="Mammalia" 
                                            & data_invacost_species$Method.reliability == "High"
                                            & data_invacost_species$Spatial.scale == "Country"
                                            & data_invacost_species$Implementation == "Observed"])) # 22 species

levels(factor(data_invacost_species$Species[data_invacost_species$Class=="Aves" 
                                            & data_invacost_species$Method.reliability == "High"
                                            & data_invacost_species$Spatial.scale == "Country"
                                            & data_invacost_species$Implementation == "Observed"])) # 7 species
