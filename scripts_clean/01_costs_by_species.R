############################################################################################################
# Script to calculate cost by species in Invacost, based on:
# http://borisleroy.com/invacost/Readme.html#Example_on_many_subsets:_all_taxonsspecies_in_the_database
# and updated by Marine Robuchon
############################################################################################################

## install the invacost package
# install.packages("invacost")

## load packages & data
library(invacost)

# load version 4.1 of InvaCost
library(readxl)
invacost <- as.data.frame(read_xlsx("./data/InvaCost_database_v4.1.xlsx",
                                    na = c("NA", "#N/A", "#DIV/0!", "#VALEUR!", 
                                           "Unspecified", "Unknown",
                                           "unspecified"),
                                    guess_max = 10000)) # 13553 observations of 66 variables

invacost$Applicable_year <- as.numeric(invacost$Applicable_year)
invacost$Publication_year <- as.numeric(invacost$Publication_year)
invacost$Probable_starting_year <- as.numeric(invacost$Probable_starting_year)
invacost$Probable_ending_year <- as.numeric(invacost$Probable_ending_year)
invacost$Probable_starting_year_adjusted <- as.numeric(invacost$Probable_starting_year_adjusted)
invacost$Probable_ending_year_adjusted <- as.numeric(invacost$Probable_ending_year_adjusted)

## remove potential costs and unreliable sources
invacost <- invacost[invacost$Acquisition_method!= "Extrapolation" ,] # 11056 observations left after this step
invacost <- invacost[invacost$Method_reliability!= "Low" ,] # 10067 observations left after this step

## filter subspecies
invacost <- invacost[is.na(invacost$Subspecies),] # 9958 observations after this step

## filter mix of species: none, we apply the calculation of costs to all unique values of invacost$Species
## this will calculate by species for some, and by other units than species for others
## but we will keep only species when we make the match by names in the script 0x_build_database

## check and fix taxon duplicates 
# first we create new columns in character format to avoid factor errors in R
invacost$sp.list <- as.character(invacost$Species)
invacost$genus.list <- as.character(invacost$Genus)
# check what it looks like
invacost$sp.list[1:25] # so even if we create a unique identifier, it is this column we want to merge in the database
# unique identifier
invacost$unique.sp.id <- do.call("paste", invacost[, c("Kingdom", "Phylum", "Class", "Family", "genus.list", "sp.list")])
# check the duplicates identified (to do after the calculation of costs, if there are any duplicates)
# invacost$unique.sp.id[which(invacost$sp.list== "Quercus rubra")] # so we have to change 1
# invacost$unique.sp.id[which(invacost$sp.list== "Quercus rubra")][1] <- "Plantae Tracheophyta Magnoliopsida Fagaceae Quercus Quercus rubra"
# invacost$unique.sp.id[which(invacost$sp.list== "Pelargonium capitatum")] # so we have to change 2
# invacost$unique.sp.id[which(invacost$sp.list== "Pelargonium capitatum")][2] <- "Plantae Tracheophyta Magnoliopsida Geraniaceae Pelargonium Pelargonium capitatum"
# invacost$unique.sp.id[which(invacost$sp.list== "Pilosella officinarum")] # so we have to change 1
# invacost$unique.sp.id[which(invacost$sp.list== "Pilosella officinarum")][1] <- "Plantae Tracheophyta Magnoliopsida Asteraceae Pilosella Pilosella officinarum"
# invacost$unique.sp.id[which(invacost$sp.list== "Arctotheca calendula")] # so we have to change 5
# invacost$unique.sp.id[which(invacost$sp.list== "Arctotheca calendula")][5] <- "Plantae Tracheophyta Magnoliopsida Asteraceae Arctotheca Arctotheca calendula"
# invacost$unique.sp.id[which(invacost$sp.list== "Heracleum mantegazzianum")] # so we have to change 3
# invacost$unique.sp.id[which(invacost$sp.list== "Heracleum mantegazzianum")][3] <- "Plantae Tracheophyta Magnoliopsida Apiaceae Heracleum Heracleum mantegazzianum"
# invacost$unique.sp.id[which(invacost$sp.list== "Arundo donax")] # so we have to change 181 & 182
# invacost$unique.sp.id[which(invacost$sp.list== "Arundo donax")][181] <- "Plantae Tracheophyta Liliopsida Poaceae Arundo Arundo donax"
# invacost$unique.sp.id[which(invacost$sp.list== "Arundo donax")][182] <- "Plantae Tracheophyta Liliopsida Poaceae Arundo Arundo donax"
# invacost$unique.sp.id[which(invacost$sp.list== "Agave sisalana")] # so we have to change 5
# invacost$unique.sp.id[which(invacost$sp.list== "Agave sisalana")][5] <- "Plantae Tracheophyta Liliopsida Asparagaceae Agave Agave sisalana"
# invacost$unique.sp.id[which(invacost$sp.list== "Agave americana")] # so we have to change 117
# invacost$unique.sp.id[which(invacost$sp.list== "Agave americana")][117] <- "Plantae Tracheophyta Liliopsida Asparagaceae Agave Agave americana"

# check entries for Canis lupus and Equus ferus
canislupus <- invacost[which(invacost$sp.list=="Canis lupus"),]
# ok so these are only subpsecies entries even if not encoded as such, we therefore need to remove it later in the script "03_build database mammals_birds"
equusferus <- invacost[which(invacost$sp.list=="Equus ferus"),]
# this is the species, not the subspecies, so we keep it

## remove entries without cost in "Cost estimate per year 2017 exchange rate"
## and those with no information on starting and ending years
invacost_sub <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)),]
invacost_sub <- invacost_sub[-which(is.na(invacost_sub$Probable_starting_year_adjusted)), ]
invacost_sub <- invacost_sub[-which(is.na(invacost_sub$Probable_ending_year_adjusted)), ] # 9824 observations after this step


## second we expand the database
db.over.time <- expandYearlyCosts(invacost_sub,
                                  startcolumn = "Probable_starting_year_adjusted",
                                  endcolumn = "Probable_ending_year_adjusted")

## remove entries for which impact year is before 1960 
## this is to avoid the mistake
## Erreur dans summarizeCosts(cur.db) : 
##  maximum.year is lower than minimum.year.
db.over.time <- db.over.time[which(db.over.time$Impact_year >= 1960) ,]


## finally, we calculate costs by species : damage vs management 
# DAMAGE
# extract damage costs from the expanded database
db.over.time_damage <- db.over.time[which(db.over.time$Type_of_cost_merged=="Damage"), ]
# then we prepare a data.frame in which we will store our results
species.summary_damage <- data.frame()
# we will cycle the loop through all unique identifiers
for(sp in unique(db.over.time_damage$unique.sp.id))
{
  print(sp)
  # we subset the database for our current species
  cur.db <- db.over.time_damage[which(db.over.time_damage$unique.sp.id %in% sp), ]
  # we apply the raw cost function
  cur.raw <- summarizeCosts(cur.db)
  # and from the cur.raw object we extract the specific information we are looking for
  species.summary_damage <- rbind.data.frame(species.summary_damage,
                                      data.frame(
                                        Kingdom = cur.db$Kingdom[1],
                                        Phylum = cur.db$Phylum[1],
                                        Class = cur.db$Class[1],
                                        Family = cur.db$Family[1],
                                        Genus = cur.db$Genus[1],
                                        Species = cur.db$sp.list[1],
                                        Average.annual.cost = cur.raw$average.total.cost$annual_cost,
                                        Cumulated.cost = cur.raw$average.total.cost$total_cost,
                                        Number.estimates = cur.raw$average.total.cost$number_estimates,
                                        Number.year.values = cur.raw$average.total.cost$number_year_values
                                       ))
}


# to make the summary dataframe nicer, we can sort by cost to have the highest groups first
species.summary_damage <- species.summary_damage[order(species.summary_damage$Cumulated.cost, decreasing = TRUE), ]
# have a look at the first groups
species.summary_damage[1:10, ]
# save results
write.csv2(species.summary_damage, "./outputs/damagecost_by_species.csv")

# MANAGEMENT
# extract management costs from the expanded database
db.over.time_management <- db.over.time[which(db.over.time$Type_of_cost_merged=="Management") ,]
# then we prepare a data.frame in which we will store our results
species.summary_management <- data.frame()
# we will cycle the loop through all unique identifiers
for(sp in unique(db.over.time_management$unique.sp.id))
{
  print(sp)
  # we subset the database for our current species
  cur.db <- db.over.time_management[which(db.over.time_management$unique.sp.id %in% sp), ]
  # we apply the raw cost function
  cur.raw <- summarizeCosts(cur.db)
  # and from the cur.raw object we extract the specific information we are looking for
  species.summary_management <- rbind.data.frame(species.summary_management,
                                             data.frame(
                                               Kingdom = cur.db$Kingdom[1],
                                               Phylum = cur.db$Phylum[1],
                                               Class = cur.db$Class[1],
                                               Family = cur.db$Family[1],
                                               Genus = cur.db$Genus[1],
                                               Species = cur.db$sp.list[1],
                                               Average.annual.cost = cur.raw$average.total.cost$annual_cost,
                                               Cumulated.cost = cur.raw$average.total.cost$total_cost,
                                               Number.estimates = cur.raw$average.total.cost$number_estimates,
                                               Number.year.values = cur.raw$average.total.cost$number_year_values
                                             ))
}
# to make the summary dataframe nicer, we can sort by cost to have the highest groups first
species.summary_management <- species.summary_management[order(species.summary_management$Cumulated.cost, decreasing = TRUE), ]
# have a look at the first groups
species.summary_management[1:10, ]
# save results
write.csv2(species.summary_management, "./outputs/managementcost_by_species.csv")






