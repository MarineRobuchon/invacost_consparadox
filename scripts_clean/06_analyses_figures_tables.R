#############################################################################################
# Script to run the analyses and make figures/tables of the paper
# original script by Marine Robuchon with contributions of Céline Bellard, Camille Bernery, 
# Vanessa rezende, Gustavo heringer & Cheikh Dia
#############################################################################################

### Load data ----
data_all <-  read.csv2(paste0(getwd(), "/outputs/data_all.csv"))[, -1]

### Threat status and originality of the costliest invasive species in InvaCost ----
## identification of the costliest species by type of costs & taxon
# damage
damage <- data_all[-which(is.na(data_all$Average.annual.cost_damage)),]

mam_damage <- damage[which(damage$className=="MAMMALIA"),]
top5_mam_damage <- head(mam_damage[order(mam_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

bird_damage <- damage[which(damage$className=="AVES"),]
top5_bird_damage <- head(bird_damage[order(bird_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

plant_damage <- damage[which(damage$className=="PLANTS"),]
top5_plant_damage <- head(plant_damage[order(plant_damage$Average.annual.cost_damage, decreasing = TRUE),], 5)

# management
management <- data_all[-which(is.na(data_all$Average.annual.cost_management)),]

mam_management <- management[which(management$className=="MAMMALIA"),]
top5_mam_management <- head(mam_management[order(mam_management$Average.annual.cost_management, decreasing = TRUE),], 5)

bird_management <- management[which(management$className=="AVES"),]
top5_bird_management <- head(bird_management[order(bird_management$Average.annual.cost_management, decreasing = TRUE),], 5)

plant_management <- management[which(management$className=="PLANTS"),]
top5_plant_management <- head(plant_management[order(plant_management$Average.annual.cost_management, decreasing = TRUE),], 5)
