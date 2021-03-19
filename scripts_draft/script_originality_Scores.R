plant_trees <- read.tree("ALLMB.tre") # the 1000 trees

plant_tb_phylori <- list()
plant_db_phylori <- list()
for (i in 1:length(plant_trees))
  {
       plant_tree <- plant_trees
       plant_tb_phylori[[i]] <- distinctTree(plant_tree)
       d_plant_phylo <- dsimTree(plant_tree, type = "dissimilarity")
       plant_db_phylori[[i]] <- distinctDis(d_plant_phylo)
       
       } 
plant_tb_phylori.allscores <- do.call(cbind, plant_tb_phylori) 
plant_tb_phylori.allscores <- do.call(cbind, plant_tb_phylori) 
plant.EDmed <- apply(X = plant_tb_phylori.allscores[colnames(plant_tb_phylori.allscores)=="ED"], MARGIN = 1, FUN = median)  
plant.ESmed <- apply(X = plant_tb_phylori.allscores[colnames(plant_tb_phylori.allscores)=="ES"], MARGIN = 1, FUN = median)
plant_tb_phylori.med <- data.frame(cbind(ED_median = plant.EDmed, ES_median = plant.ESmed))
write.csv2(plant_tb_phylori.med, paste0(getwd(), "/outputs/plants_tree-based_phylori.csv")) # save results
plant_db_phylori.allscores <- do.call(cbind, plant_db_phylori) 
plant.Rbmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="Rb"], MARGIN = 1, FUN = median)  
plant.AVmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="AV"], MARGIN = 1, FUN = median)
plant.FVmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="FV"], MARGIN = 1, FUN = median)
plant.NNmed <- apply(X = plant_db_phylori.allscores[colnames(plant_db_phylori.allscores)=="NN"], MARGIN = 1, FUN = median)
plant_db_phylori.med <- data.frame(cbind(Rb_median = plant.Rbmed, AV_median = plant.AVmed, FV_median = plant.FVmed, NNmed = plant.NNmed))
write.csv2(plant_db_phylori.med, paste0(getwd(), "/outputs/plants_distance-based_phylori.csv")) # save results