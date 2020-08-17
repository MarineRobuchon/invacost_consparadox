############################################################################################################
# Script to calculate cost by species in Invacost, based on:
# http://borisleroy.com/invacost/Readme.html#Example_on_many_subsets:_all_taxonsspecies_in_the_database
############################################################################################################

## install the invacost package
# install.packages(paste0(getwd(),"/data/invacost_0.3-4.tar.gz"), repos = NULL, type = "source")

## load packages & data
library(invacost)
data(invacost)

## taxonomic changes : none, we apply the calculation of costs to all unique values of invacost$species
## this will calculate by species for some, and by other units than species for others
## but we will keep only species when we make the match by names in the script 00.BuildDatabase.R

## calculation of costs by species : total 
# first we create new columns in character format to avoid factor errors in R
invacost$sp.list <- as.character(invacost$Species)
invacost$genus.list <- as.character(invacost$Genus)
# check what it looks like
invacost$sp.list[1:25] # so even if we create a unique identifier, it is this column we want to merge in the database
# unique identifier
invacost$unique.sp.id <- do.call("paste", invacost[, c("Kingdom", "Phylum", "Class", "Family", "genus.list", "sp.list")])
# first we expand the database
db.over.time <- expandYearlyCosts(invacost,
                                  startcolumn = "Probable_starting_year_low_margin",
                                  endcolumn = "Probable_ending_year_low_margin")
# then we prepare a data.frame in which we will store our results
species.summary <- data.frame()
# We will cycle the loop through all unique identifiers
for(sp in unique(db.over.time$unique.sp.id))
{
  # we subset the database for our current species
  cur.db <- db.over.time[which(db.over.time$unique.sp.id %in% sp), ]
  # we apply the raw cost function
  cur.raw <- calculateRawAvgCosts(cur.db, minimum.year = 1970)
  # and from the cur.raw object we extract the specific information we are looking for
  species.summary <- rbind.data.frame(species.summary,
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
species.summary <- species.summary[order(species.summary$Cumulated.cost, decreasing = TRUE), ]
# have a look at the first groups
species.summary[1:10, ]
# save results
write.csv2(species.summary, "./outputs/cost_by_species.csv")

# 3 questions
# 1) Why do I have the following warnings: In calculateRawAvgCosts(cur.db, minimum.year = 1970) :
# There are 2 cost values for periods later than 2017 which will be removed.
# 2) Should we use average annual cost or cumulated costs? >> average
# 3) This does not differentiate damage or management costs right? >> see below

## calculation of costs by species : damage vs management 
# DAMAGE
# extract damage costs from the expanded database
db.over.time_damage <- db.over.time[db.over.time$Type_2=="Damage_costs" ,]
# remove na from unique.species.id
db.over.time_damage <- db.over.time_damage[!is.na(db.over.time_damage$unique.sp.id),]
# then we prepare a data.frame in which we will store our results
species.summary_damage <- data.frame()
# we will cycle the loop through all unique identifiers
for(sp in unique(db.over.time_damage$unique.sp.id))
{
  print(sp)
  # we subset the database for our current species
  cur.db <- db.over.time_damage[which(db.over.time_damage$unique.sp.id %in% sp), ]
  # we apply the raw cost function
  cur.raw <- calculateRawAvgCosts(cur.db, minimum.year = 1970)
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
# If I do not remove na from unique.sp.id, I have the following error
# Error in if (any(costdb[, year.column] < minimum.year)) { : 
# missing value where TRUE/FALSE needed
# In addition: There were 17 warnings (use warnings() to see them)
# to make the summary dataframe nicer, we can sort by cost to have the highest groups first
species.summary_damage <- species.summary_damage[order(species.summary_damage$Cumulated.cost, decreasing = TRUE), ]
# have a look at the first groups
species.summary_damage[1:10, ]
# save results
write.csv2(species.summary_damage, "./outputs/damagecost_by_species.csv")

# MANAGEMENT
# extract management costs from the expanded database
db.over.time_management <- db.over.time[db.over.time$Type_2=="Management_costs" ,]
# remove na from unique.species.id
db.over.time_management <- db.over.time_management[!is.na(db.over.time_management$unique.sp.id),]
# then we prepare a data.frame in which we will store our results
species.summary_management <- data.frame()
# we will cycle the loop through all unique identifiers
for(sp in unique(db.over.time_management$unique.sp.id))
{
  print(sp)
  # we subset the database for our current species
  cur.db <- db.over.time_management[which(db.over.time_management$unique.sp.id %in% sp), ]
  # we apply the raw cost function
  cur.raw <- calculateRawAvgCosts(cur.db, minimum.year = 1970)
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
# If I do not remove na from unique.sp.id, I have the following error
# Error in if (any(costdb[, year.column] < minimum.year)) { : 
#     missing value where TRUE/FALSE needed
#   In addition: There were 27 warnings (use warnings() to see them)
# to make the summary dataframe nicer, we can sort by cost to have the highest groups first
species.summary_management <- species.summary_management[order(species.summary_management$Cumulated.cost, decreasing = TRUE), ]
# have a look at the first groups
species.summary_management[1:10, ]
# save results
write.csv2(species.summary_management, "./outputs/managementcost_by_species.csv")
