###################################### List of functions


### -- Load libraries
library(tidyverse)


### 1. Simulation of species removal for CP

#This function eliminates randomly the same number of species in each habitat as in the original simulation (Except for crop species) 

sim_sp_removal<- function(edge_list_hab, n_to_remove) {
  
  # Initialize the objects
  edge_list_shuff <- data.frame() 
  iteration <- numeric()
  list_species_rem<- data.frame()
  
  combined_nodes<- edge_list_hab %>% filter (!(taxon_node_from == "Crop"| taxon_node_to == "Crop")) %>% 
    select(node_from,node_to) %>% pivot_longer(cols = c(node_from, node_to)) %>% ungroup() %>% select(-layer_from,-name) %>% 
    unique() %>% pull(value) #vector containing potential species to randomly remove (except crops) 
  
  for (i in 1:600) {
    print(i)
    # Select randomly species to remove
    sp_to_remove <- sample(combined_nodes, n_to_remove[n_to_remove$habitat == unique(edge_list_hab$pre_hab), 2], 
                           replace = FALSE) #n_to_remove correspond to the dataframe containing information of how many species to remove accoring to the habitat
    
    
    # Remove species from the edgelist
    edge_list_remov <- dplyr::filter(edge_list_hab, !(node_from %in% sp_to_remove | node_to %in% sp_to_remove))
    
    # Store results
    edge_list_shuff <- rbind(edge_list_shuff, edge_list_remov)
    iteration <- c(iteration, rep(i, nrow(edge_list_remov))) # number of rep
    
    # Estimate information of the node removed
    for (j in sp_to_remove){
      species_rem = j
      degree<- edge_list_hab %>%ungroup() %>% 
        filter(node_from== j| node_to ==j) %>% distinct(node_from,node_to) %>%  summarise(degree = n())
      degree_sp<-cbind(species_rem = j,degree, iteration =i)
    
      #Store the information
      
      list_species_rem<-rbind(list_species_rem, degree_sp) 
    }

    
  }
  
  output <- cbind(edge_list_shuff, iteration = iteration)
  output2 <- cbind(list_species_rem)
  
  return(list(edge_lists = output,species_removed =output2))
}




### 2. Simulation of species removal for PP

#This function eliminates randomly the same number of species in each habitat as in the original simulation (remove also crops)

sim_sp_removal_pp<- function(edge_list_hab, n_to_remove) {
  
  # Initialize the objects
  edge_list_shuff <- data.frame() 
  iteration <- numeric()
  list_species_rem<- data.frame()
  
  combined_nodes<- edge_list_hab  %>% 
    select(node_from,node_to) %>% pivot_longer(cols = c(node_from, node_to)) %>% ungroup() %>% select(-layer_from,-name) %>% 
    unique() %>% pull(value) #vector containing potential species to randomly remove 
  
  for (i in 1:600) {
    print(i)
    # Select randomly species to remove
    sp_to_remove <- sample(combined_nodes, n_to_remove[n_to_remove$habitat == unique(edge_list_hab$pre_hab), 2], 
                           replace = FALSE) #n_to_remove correspond to the dataframe containing information of how many species to remove accoring to the habitat
    
    
    # Remove species from the edgelist
    edge_list_remov <- dplyr::filter(edge_list_hab, !(node_from %in% sp_to_remove | node_to %in% sp_to_remove))
    
    # Store results
    edge_list_shuff <- rbind(edge_list_shuff, edge_list_remov)
    iteration <- c(iteration, rep(i, nrow(edge_list_remov))) # number of rep
    
    # Estimate information of the node removed
    for (j in sp_to_remove){
      species_rem = j
      degree<- edge_list_hab %>%ungroup() %>% 
        filter(node_from== j| node_to ==j) %>% distinct(node_from,node_to) %>%  summarise(degree = n())
      degree_sp<-cbind(species_rem = j,degree, iteration =i)
      
      #Store the information
      
      list_species_rem<-rbind(list_species_rem, degree_sp) 
    }
    
    
  }
  
  output <- cbind(edge_list_shuff, iteration = iteration)
  output2 <- cbind(list_species_rem)
  
  return(list(edge_lists = output,species_removed =output2))
}



### 3. Combine edge list

#This function combines the edge list of the shuffled habitats with the non-transformed ones to create 1000 trials of
#each habitat management scenario 


comb_edge_list<- function(non_transf_hab,shuff_hab) {
  
  # Initialize the objects
  combined_edge_lists <- list()  

  
  for (i in 1:600) {
    print(i)
    
    # Filter the iterated edge list of the transformed habitat according to the iteration
    iter_edge_list<- shuff_hab %>% filter (iteration == i)
    
    # Combine with edge list of non-transformed habitat
    non_transf_hab_ite<-non_transf_hab %>% mutate(iteration = i)
    combined_list <- rbind(non_transf_hab_ite, iter_edge_list)
    
    # Store the combined list
    combined_edge_lists[[i]] <- combined_list
  }
  
  return(combined_edge_lists)
}




### 4. Create state_node list

#This function create the state_node list of each simulated management scenario


state_node_list <- function(data) {
  
  # Processing node_from
  state_node_sem_ext_from <- data %>%
    select(habitat, node_from, ab_node_from, taxon_node_from, iteration) %>%
    rename(node_id = node_from, abundances = ab_node_from, taxon = taxon_node_from) %>%
    group_by(habitat, node_id) %>%
    unique()  # Eliminate duplicate species within each habitat
  
  # Processing node_to
  state_node_sem_ext_to <- data %>%
    select(habitat, node_to, ab_node_to, taxon_node_to, iteration) %>%
    rename(node_id = node_to, abundances = ab_node_to, taxon = taxon_node_to) %>%
    group_by(habitat, node_id) %>%
    unique()  # Eliminate duplicate species within each habitat
  
  # Aggregating the final state nodes
  state_node_sem_ext_agg <- rbind(state_node_sem_ext_from, state_node_sem_ext_to) %>%
    ungroup() %>%
    select(-habitat) %>%
    group_by(node_id, taxon,iteration) %>%
    summarise(abun = sum(abundances, na.rm = TRUE))  # Calculate sum of abundances
  
  return(state_node_sem_ext_agg)
}






