# In this code, we develop a null model where the same number of species are removed during land conversion as in
# the original simulation, but randomly. This helps disentangle whether the observed NCP loss
# patterns are driven by the number of species that go extinct or by their specific identities.

#In the files, the term “ES” refers to “NCP” and “1 hop” and “2 hop” indicate first-order and second-order pathways, respectively.


## -- Load libraries --------------------------------------------------------------------------------------------------------
library(emln)#multilayer package
library(readr)
library(ggplot2)
library(cowplot)
library(tidyverse)

## -- get_data--------------------------------------------------------------------------------------------------------
#setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")
#source("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio/R/functions.R") # call the functions script

setwd("/Users/agustinvitali/Desktop/Work/Papers/In_prep/Norwood_Farm/GitHub/Norwood_farm")
source("R/functions.R") #call functions file

######### --- Call and arrange dataframes 
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object

## Add the abundances (as state nodes attributes) 
abundances<-read.csv("Data/species_abundances.csv",header=T) #call abundances

state_nodes_ab<-Norwood_farm$state_nodes %>% left_join(abundances, 
                                                       by = c("layer_name" = "habitat",
                                                              "node_name" = "species_name")) %>% #add abundances
  left_join(Norwood_farm$nodes, by = "node_id") %>% 
  select(layer_id,node_id,abundance, taxon) ##add taxon

## Area of each habitat
areas<-read.csv("Data/Raw_data/habitatarea.csv", sep =",") %>% # LU as CP already merged
  filter(HabitatCode != "ST") %>% #remove standing trees
  mutate(HabitatCode = case_when(HabitatCode == "C"~ "CP",
                                 HabitatCode == "WU"~ "WD",
                                 TRUE~HabitatCode))

habitat_area <- areas %>% mutate(area_ave = case_when(
  (Area_2007 >0) & (Area_2008 >0) ~ (Area_2007+Area_2008)/2, #if the same habitat was present in both years do the average
  (Area_2007 >0) & (Area_2008  ==0)~ Area_2007, #if the habitat was present in one year, keep the area of the year
  (Area_2007 ==0) & (Area_2008  >0)~ Area_2008)) %>% 
  mutate(mult_ab = area_ave/46.4) # the factor we should modify the abundance from CP species according to the new habitat




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      NULL MODEL                   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(123)


############# 1. Species to remove

#Here we identify the number of species to randomly remove in each habitat when converting from extensive to 
# intensive scenarios. Under Mechanism 1, some species that would otherwise go extinct can persist via 
# interaction rewiring. The number to remove is updated to reflect only those that fail to rewire.

# Find species present in CP
species_in_cp <- unique(Norwood_farm$state_nodes[Norwood_farm$state_nodes$layer_name == 'CP', 'node_id'])

# Find species in each habitat and compare with CP (this gives the baseline extinction count (no rewiring)
unique_habitats <- unique(Norwood_farm$state_nodes$layer_name)
absent_species_count <- data.frame(habitat = character(), absent_count = integer())

for (hab in unique_habitats) {
  if (hab != 'CP') {
    species_in_habitat <- unique(Norwood_farm$state_nodes[Norwood_farm$state_nodes$layer_name == hab, 'node_id'])
    absent_count <- sum(!species_in_habitat$node_id %in% species_in_cp$node_id)
    absent_species_count <- rbind(absent_species_count, data.frame(habitat = hab, absent_count = absent_count))
  }
}

# Baseline objects for Mechanism 1 (same as in 4_Land_conversion_simulation_M1.R)
metaweb <- Norwood_farm$extended_ids %>% select(node_from, node_to) %>% unique()

# Fixed denominator: total interactions per consumer across the full extensive farm baseline
total_resources_baseline <- Norwood_farm$extended_ids %>%
  group_by(node_to) %>%
  summarise(total_interactions = n()) %>%
  rename("node_id" = "node_to")

abundances_CP  <- state_nodes_ab %>% filter(layer_id == 1) # abundances of CP species in the original farm
species_in_CP  <- abundances_CP %>% pull(node_id) %>% unique() # grows with each conversion scenario


############# 2. Land-use change simulation (Null model)
Norwood_farm$extended_ids<- select(Norwood_farm$extended_ids,-weight) #remove weight (dummy variable) used in previous version


##### -- Extensive scenario
extensive_edgelist<- Norwood_farm$extended_ids %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>% 
  mutate(management = "E") %>% select(-habitat) %>% unique() #aggregate network

 
# estimate relative abundances of species in the aggregated network
ab_ext<-state_nodes_ab %>% select(-layer_id) %>% group_by(node_id,taxon) %>%
  mutate(abun = sum(abundance)) %>% distinct(abun) 


##### -- Semi - extensive (replace "WD" and "RG")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

sem_ext_edgelist_rem<- Norwood_farm$extended_ids %>% filter(layer_from != 8 & layer_from != 10) %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   #links from "WD" and "RG" removed
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

sem_ext_edgelist_rem<-sem_ext_edgelist_rem[,c(1,2,5,4,3,7,6)]


##--  Create new habitats 

# Merge edge list of CP and the habitats to convert
WD_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 10) %>% 
        mutate (pre_hab= "WD",new_habitat = "WD_CP")

RG_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 8) %>% 
        mutate (pre_hab = "RG",new_habitat = "RG_CP")

converted_area<-rbind(WD_CP, RG_CP) 

## -- Add abundances of species and modify it according to the new area (for species from CP that are in the new habitat)

# add abundances and modify those from CP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==1 |layer_id ==8 |
                                         layer_id ==10) 

new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
    mutate(ab_node_from = case_when(
    (layer_from == 1 & new_habitat == "WD_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "WD",6 ],
    (layer_from == 1 & new_habitat == "RG_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "RG",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from CP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 1 & new_habitat == "WD_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "WD",6 ],
      (layer_from == 1 & new_habitat == "RG_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "RG",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from CP according to the area of the new habitat
           

# remove interactions where one partner have less than 1 individual(threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 1 ~ "CP",
    layer_from == 8 ~ "RG",
    layer_from == 10 ~ "WD"),
    layer_to= case_when(
      layer_to == 1 ~ "CP",
      layer_to == 8 ~ "RG",
      layer_to == 10 ~ "WD"
    ))

# apply_rewiring expects only CP species as the new habitat baseline, not the full merged edge list
new_habitats_ab_CP <- new_habitats_ab %>%
  filter(layer_from == 1, ab_node_from >= 1, ab_node_to >= 1) %>%
  mutate(habitat = case_when(new_habitat == "WD_CP" ~ 11L, new_habitat == "RG_CP" ~ 12L)) %>%
  select(habitat, node_from, ab_node_from, taxon_node_from, node_to, ab_node_to, taxon_node_to)

## Run apply_rewiring once to count how many species survived via rewiring per habitat
# This count is used to update the number of species to randomly remove in the null model
new_habitats_emp <- apply_rewiring(new_habitats_ab_CP,
                                   replaced_layer_ids = c(8, 10),
                                   hab_id_map = c("10" = 11, "8" = 12),
                                   state_nodes_ab, total_resources_baseline,
                                   species_in_CP, abundances_CP, metaweb)

# Count species rewired per original habitat (not counting species already in CP)
rewired_SE <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1) %>%
  filter(!node_to %in% species_in_CP) %>%
  mutate(hab_name = case_when(habitat == 11 ~ "WD", habitat == 12 ~ "RG")) %>%
  group_by(hab_name) %>%
  summarise(n_rewired = n_distinct(node_to), .groups = "drop")

# Update absent count: subtract rewired species from the original extinction count
absent_species_count_SE <- absent_species_count %>%
  left_join(rewired_SE, by = c("habitat" = "hab_name")) %>%
  mutate(absent_count = absent_count - replace_na(n_rewired, 0)) %>%
  select(habitat, absent_count)

# Update species_in_CP to include newly rewired species (carried forward to next scenarios)
species_rewired_SE <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1, !node_to %in% species_in_CP) %>%
  pull(node_to) %>% unique()
species_in_CP <- union(species_in_CP, species_rewired_SE)

new_habitats_emp_rem_SE <- new_habitats_emp %>% filter(ab_node_from >= 1 & ab_node_to >= 1)

rescue_SE <- apply_rescue(
  replaced_layer_ids   = c(8, 10),
  hab_id_map           = c("10" = 11, "8" = 12),
  new_habitats_ab_rem  = new_habitats_emp_rem_SE,
  state_nodes_ab       = state_nodes_ab,
  destination_edgelist = sem_ext_edgelist_rem,
  metaweb              = metaweb
)

if (!is.null(rescue_SE$rescue_edges)) {
  type_b_rescued_SE <- rescue_SE$rescue_edges %>%
    filter(!node_to %in% species_in_CP) %>%
    pull(node_to) %>% unique()
  
  rescued_SE <- data.frame(
    habitat   = c("WD", "RG"),
    n_rescued = c(
      sum(type_b_rescued_SE %in% (state_nodes_ab %>% filter(layer_id == 10) %>% pull(node_id))),
      sum(type_b_rescued_SE %in% (state_nodes_ab %>% filter(layer_id == 8)  %>% pull(node_id)))
    )
  )
  
  absent_species_count_SE <- absent_species_count_SE %>%
    left_join(rescued_SE, by = "habitat") %>%
    mutate(absent_count = pmax(0, absent_count - replace_na(n_rescued, 0))) %>%
    select(habitat, absent_count)
  
  species_in_CP <- union(species_in_CP, type_b_rescued_SE)
  
} else {
  type_b_rescued_SE <- character(0)
}

## -- Remove species at random (500 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#WD
WD_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="WD")
shuff_WD<-sim_sp_removal(WD_edge_list,absent_species_count_SE) #function to randomly remove the same number of species according to the original simulation
WD_clean<-shuff_WD[[1]] %>% ungroup() %>% mutate(habitat = 11) 

#RG
RG_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="RG")
shuff_RG<-sim_sp_removal(RG_edge_list,absent_species_count_SE) #function to randomly remove the same number of species according to the original simulation
RG_clean<-shuff_RG[[1]] %>% ungroup() %>% mutate(habitat = 12) 

#Merge shuff habitats and arrange dataframe to merge with the rest of the habitats
shuff_habitats<-rbind(WD_clean,RG_clean) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                  node_to,ab_node_to,taxon_node_to, iteration)

write.csv(shuff_habitats,"Data/shuff_hab_WD_RG_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario (intermediate file)

# Store species removed in every iteration
sps_removed_WD <- shuff_WD[[2]] %>% mutate(habitat_from = "WD", management = "SE")
sps_removed_RG <- shuff_RG[[2]] %>% mutate(habitat_from = "RG", management = "SE")
sps_removed<-rbind(sps_removed_WD,sps_removed_RG)

write.csv(sps_removed,"Data/sps_removed_SE_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario (intermediate file)

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 500 simulation of the management scenario (SE)
sem_ext_sim_no_aggr<- comb_edge_list(sem_ext_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_sem_ext_sim<-lapply(sem_ext_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_sem_ext_sim<-bind_rows(state_node_sem_ext_sim)

write.csv(state_node_sem_ext_sim,"Data/SE_sim_state_node_CP_M1_M2.csv", row.names= FALSE) #(intermediate file)

## -- aggregate habitat within simulated management scenario 
SE_sim<-lapply(sem_ext_sim_no_aggr,function(data) {
  data %>%  mutate(management = "SE") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

SE_sim<-bind_rows(SE_sim)
write.csv(SE_sim,"Data/SE_sim_CP_M1_M2.csv", row.names= FALSE)# (intermediate file)



#####  -- Moderate (replace "WD","RG","MH"and "NH" for "CP")

##-- Remove habitats from Norwood (the ones to replace) and incorporate abundances and taxon 
mod_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 & layer_from != 4 & layer_from != 5) %>% #links from "WD", "RG", "MH", and "NH" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

mod_edgelist_rem<-mod_edgelist_rem[,c(1,2,5,4,3,7,6)]


##--  Create new habitats 

# Merge edge list of CP and the habitats to convert
MH_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 4) %>% 
  mutate (pre_hab= "MH",new_habitat = "MH_CP")

NH_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 5) %>% 
  mutate (pre_hab = "NH",new_habitat = "NH_CP")

converted_area<-rbind(MH_CP, NH_CP) 

## -- Add abundances of species and modify it according to the new area (for species from CP that are in the new habitat)

# add abundances and modify those from CP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==1 |layer_id ==4 |
                                           layer_id ==5) 
new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = case_when(
    (layer_from == 1 & new_habitat == "MH_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "MH",6 ],
    (layer_from == 1 & new_habitat == "NH_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "NH",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from CP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 1 & new_habitat == "MH_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "MH",6 ],
      (layer_from == 1 & new_habitat == "NH_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "NH",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from CP according to the area of the new habitat


# remove interactions where one partner have less than 1 individual(threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 1 ~ "CP",
    layer_from == 4 ~ "MH",
    layer_from == 5 ~ "NH"),
    layer_to= case_when(
      layer_to == 1 ~ "CP",
      layer_to == 4 ~ "MH",
      layer_to == 5 ~ "NH"
    ))

# apply_rewiring expects only CP species as the new habitat baseline, not the full merged edge list
new_habitats_ab_CP <- new_habitats_ab %>%
  ungroup() %>%
  filter(layer_from == 1, ab_node_from >= 1, ab_node_to >= 1) %>%
  mutate(habitat = case_when(new_habitat == "MH_CP" ~ 13L, new_habitat == "NH_CP" ~ 14L)) %>%
  select(habitat, node_from, ab_node_from, taxon_node_from, node_to, ab_node_to, taxon_node_to)

## Run apply_rewiring once to count how many species survived via rewiring per habitat
# This count is used to update the number of species to randomly remove in the null model
new_habitats_emp <- apply_rewiring(new_habitats_ab_CP,
                                   replaced_layer_ids = c(4, 5),
                                   hab_id_map = c("4" = 13, "5" = 14),
                                   state_nodes_ab, total_resources_baseline,
                                   species_in_CP, abundances_CP, metaweb)

# Count species rewired per original habitat (not counting species already in CP)
rewired_M <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1) %>%
  filter(!node_to %in% species_in_CP) %>%
  mutate(hab_name = case_when(habitat == 13 ~ "MH", habitat == 14 ~ "NH")) %>%
  group_by(hab_name) %>%
  summarise(n_rewired = n_distinct(node_to), .groups = "drop")

# Update absent count: subtract rewired species from the original extinction count
absent_species_count_M <- absent_species_count %>%
  left_join(rewired_M, by = c("habitat" = "hab_name")) %>%
  mutate(absent_count = absent_count - replace_na(n_rewired, 0)) %>%
  select(habitat, absent_count)

# Update species_in_CP to include newly rewired species (carried forward to next scenarios)
species_rewired_M <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1, !node_to %in% species_in_CP) %>%
  pull(node_to) %>% unique()
species_in_CP <- union(species_in_CP, species_rewired_M)

new_habitats_emp_rem_M   <- new_habitats_emp %>% filter(ab_node_from >= 1 & ab_node_to >= 1)
new_habitats_emp_rem_all <- bind_rows(new_habitats_emp_rem_SE, new_habitats_emp_rem_M)

rescue_M <- apply_rescue(
  replaced_layer_ids   = c(8, 10, 4, 5),
  hab_id_map           = c("8" = 11, "10" = 12, "4" = 13, "5" = 14),
  new_habitats_ab_rem  = new_habitats_emp_rem_all,
  state_nodes_ab       = state_nodes_ab,
  destination_edgelist = mod_edgelist_rem,
  metaweb              = metaweb
)

if (!is.null(rescue_M$rescue_edges)) {
  type_b_rescued_M <- rescue_M$rescue_edges %>%
    filter(!node_to %in% species_in_CP) %>%
    pull(node_to) %>% unique()
  
  rescued_M <- data.frame(
    habitat   = c("MH", "NH"),
    n_rescued = c(
      sum(type_b_rescued_M %in% (state_nodes_ab %>% filter(layer_id == 4) %>% pull(node_id))),
      sum(type_b_rescued_M %in% (state_nodes_ab %>% filter(layer_id == 5) %>% pull(node_id)))
    )
  )
  absent_species_count_M <- absent_species_count_M %>%
    left_join(rescued_M, by = "habitat") %>%
    mutate(absent_count = pmax(0, absent_count - replace_na(n_rescued, 0))) %>%
    select(habitat, absent_count)
  
  species_in_CP <- union(species_in_CP, type_b_rescued_M)
  
} else {
  type_b_rescued_M <- character(0)
}


## -- Remove species at random (500 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#MH
MH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="MH")
shuff_MH<-sim_sp_removal(MH_edge_list,absent_species_count_M) #function to randomly remove the same number of species according to the original simulation
MH_clean<-shuff_MH[[1]] %>% ungroup() %>% mutate(habitat = 13) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to, iteration)

#NH
NH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NH")
shuff_NH<-sim_sp_removal(NH_edge_list,absent_species_count_M) #function to randomly remove the same number of species according to the original simulation
NH_clean<-shuff_NH[[1]] %>% ungroup() %>% mutate(habitat = 14) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to, iteration)

#Merge shuff habitats from this management scenario (M) with the previous mangaement (SE)
shuff_pre<-read.csv("Data/shuff_hab_WD_RG_CP_M1_M2.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,MH_clean,NH_clean)  
write.csv(shuff_habitats,"Data/shuff_hab_M_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario (intermediate file)

# Store species removed in every iteration
sps_removed_MH <- shuff_MH[[2]] %>% mutate(habitat_from = "MH", management = "M")
sps_removed_NH <- shuff_NH[[2]] %>% mutate(habitat_from = "NH", management = "M")
sps_removed<-rbind(sps_removed_MH,sps_removed_NH)

write.csv(sps_removed,"Data/sps_removed_M_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario (intermediate file)

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (M)
mod_sim_no_aggr<- comb_edge_list(mod_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_mod_sim<-lapply(mod_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_mod_sim<-bind_rows(state_node_mod_sim)
write.csv(state_node_mod_sim,"Data/M_sim_state_node_CP_M1_M2.csv", row.names= FALSE) # (intermediate file)

## -- aggregate habitat within simulated management scenario 
M_sim<-lapply(mod_sim_no_aggr,function(data) {
  data %>%  mutate(management = "M") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

M_sim<-bind_rows(M_sim)
write.csv(M_sim,"Data/M_sim_CP_M1_M2.csv", row.names= FALSE)# (intermediate file)




##### -- Semi - intensive (replace "WD","RG","MH","NH","GM", "SF" and "PP" for "CP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

sem_int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 &  layer_from != 4 &
           layer_from != 5 & layer_from != 2 & layer_from != 9 &
           layer_from != 7 ) %>% #links from "WD", "RG", "MH", "NH","GM", "SF" and "PP"" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

sem_int_edgelist_rem<-sem_int_edgelist_rem[,c(1,2,5,4,3,7,6)]


##--  Create new habitats 

# Merge edge list of CP and the habitats to convert
GM_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 2) %>% 
  mutate (pre_hab= "GM",new_habitat = "GM_CP")

SF_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 9) %>% 
  mutate (pre_hab = "SF",new_habitat = "SF_CP")

PP_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 7) %>% 
  mutate (pre_hab = "PP",new_habitat = "PP_CP")

converted_area<-rbind(GM_CP, SF_CP, PP_CP) 


## -- Add abundances of species and modify it according to the new area (for species from CP that are in the new habitat)

# add abundances and modify those from CP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==1 |layer_id ==2 |
                                           layer_id ==9| layer_id ==7) 
new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = case_when(
    (layer_from == 1 & new_habitat == "GM_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "GM",6 ],
    (layer_from == 1 & new_habitat == "SF_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "SF",6 ],
    (layer_from == 1 & new_habitat == "PP_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "PP",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from CP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 1 & new_habitat == "GM_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "GM",6 ],
      (layer_from == 1 & new_habitat == "SF_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "SF",6 ],
      (layer_from == 1 & new_habitat == "PP_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "PP",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from CP according to the area of the new habitat


# remove interactions where one partner have less than 1 individual(threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 1 ~ "CP",
    layer_from == 2 ~ "GM",
    layer_from == 9 ~ "SF",
    layer_from == 7 ~ "PP"),
    layer_to = case_when(
      layer_to == 1 ~ "CP",
      layer_to == 2 ~ "GM",
      layer_to == 9 ~ "SF",
      layer_to == 7 ~ "PP"
    ))

# apply_rewiring expects only CP species as the new habitat baseline, not the full merged edge list
new_habitats_ab_CP <- new_habitats_ab %>%
  ungroup() %>%
  filter(layer_from == 1, ab_node_from >= 1, ab_node_to >= 1) %>%
  mutate(habitat = case_when(new_habitat == "GM_CP" ~ 15L,
                             new_habitat == "SF_CP" ~ 16L,
                             new_habitat == "PP_CP" ~ 17L)) %>%
  select(habitat, node_from, ab_node_from, taxon_node_from, node_to, ab_node_to, taxon_node_to)

## Run apply_rewiring once to count how many species survived via rewiring per habitat
# This count is used to update the number of species to randomly remove in the null model
new_habitats_emp <- apply_rewiring(new_habitats_ab_CP,
                                   replaced_layer_ids = c(2, 9, 7),
                                   hab_id_map = c("2" = 15, "9" = 16, "7" = 17),
                                   state_nodes_ab, total_resources_baseline,
                                   species_in_CP, abundances_CP, metaweb)

# Count species rewired per original habitat (not counting species already in CP)
rewired_SI <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1) %>%
  filter(!node_to %in% species_in_CP) %>%
  mutate(hab_name = case_when(habitat == 15 ~ "GM", habitat == 16 ~ "SF", habitat == 17 ~ "PP")) %>%
  group_by(hab_name) %>%
  summarise(n_rewired = n_distinct(node_to), .groups = "drop")

# Update absent count: subtract rewired species from the original extinction count
absent_species_count_SI <- absent_species_count %>%
  left_join(rewired_SI, by = c("habitat" = "hab_name")) %>%
  mutate(absent_count = absent_count - replace_na(n_rewired, 0)) %>%
  select(habitat, absent_count)

# Update species_in_CP to include newly rewired species (carried forward to next scenarios)
species_rewired_SI <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1, !node_to %in% species_in_CP) %>%
  pull(node_to) %>% unique()
species_in_CP <- union(species_in_CP, species_rewired_SI)


## -- Apply Mechanism 2 (rescue) for SI: count additional survivors via dispersal

new_habitats_emp_rem_SI  <- new_habitats_emp %>% filter(ab_node_from >= 1 & ab_node_to >= 1)
new_habitats_emp_rem_all <- bind_rows(new_habitats_emp_rem_SE, new_habitats_emp_rem_M, new_habitats_emp_rem_SI)

rescue_SI <- apply_rescue(
  replaced_layer_ids   = c(8, 10, 4, 5, 2, 9, 7),
  hab_id_map           = c("8" = 11, "10" = 12, "4" = 13, "5" = 14, "2" = 15, "9" = 16, "7" = 17),
  new_habitats_ab_rem  = new_habitats_emp_rem_all,
  state_nodes_ab       = state_nodes_ab,
  destination_edgelist = sem_int_edgelist_rem,
  metaweb              = metaweb
)

if (!is.null(rescue_SI$rescue_edges)) {
  type_b_rescued_SI <- rescue_SI$rescue_edges %>%
    filter(!node_to %in% species_in_CP) %>%
    pull(node_to) %>% unique()
  
  rescued_SI <- data.frame(
    habitat   = c("GM", "SF", "PP"),
    n_rescued = c(
      sum(type_b_rescued_SI %in% (state_nodes_ab %>% filter(layer_id == 2) %>% pull(node_id))),
      sum(type_b_rescued_SI %in% (state_nodes_ab %>% filter(layer_id == 9) %>% pull(node_id))),
      sum(type_b_rescued_SI %in% (state_nodes_ab %>% filter(layer_id == 7) %>% pull(node_id)))
    )
  )
  
  absent_species_count_SI <- absent_species_count_SI %>%
    left_join(rescued_SI, by = "habitat") %>%
    mutate(absent_count = pmax(0, absent_count - replace_na(n_rescued, 0))) %>%
    select(habitat, absent_count)
  
  species_in_CP <- union(species_in_CP, type_b_rescued_SI)
  
} else {
  type_b_rescued_SI <- character(0)
}


## -- Remove species at random (500 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#GM
GM_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="GM")
shuff_GM<-sim_sp_removal(GM_edge_list,absent_species_count_SI) #function to randomly remove the same number of species according to the original simulation
GM_clean<-shuff_GM[[1]] %>% ungroup() %>% mutate(habitat = 15) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to, iteration)

#SF
SF_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="SF")
shuff_SF<-sim_sp_removal(SF_edge_list,absent_species_count_SI) #function to randomly remove the same number of species according to the original simulation
SF_clean<-shuff_SF[[1]] %>% ungroup() %>% mutate(habitat = 16) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to, iteration)

#PP
PP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="PP")
shuff_PP<-sim_sp_removal(PP_edge_list,absent_species_count_SI) #function to randomly remove the same number of species according to the original simulation
PP_clean<-shuff_PP[[1]] %>% ungroup() %>% mutate(habitat = 17) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to, iteration)

#Merge shuff habitats from this management scenario (SI) with the previous management (M)
shuff_pre<-read.csv("Data/shuff_hab_M_CP_M1_M2.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,GM_clean,SF_clean,PP_clean)  
write.csv(shuff_habitats,"Data/shuff_hab_SI_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github (intermediate file)

# Store species removed in every iteration
sps_removed_GM <- shuff_GM[[2]] %>% mutate(habitat_from = "GM", management = "SI")
sps_removed_SF <- shuff_SF[[2]] %>% mutate(habitat_from = "SF", management = "SI")
sps_removed_PP <- shuff_PP[[2]] %>% mutate(habitat_from = "PP", management = "SI")
sps_removed<-rbind(sps_removed_GM,sps_removed_SF,sps_removed_PP)

write.csv(sps_removed,"Data/sps_removed_SI_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario (intermediate file)

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (SI)
sem_int_sim_no_aggr<- comb_edge_list(sem_int_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet


## -- create state_node_list of each simulated management scenario
state_node_SI_sim<-lapply(sem_int_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_SI_sim<-bind_rows(state_node_SI_sim)
write.csv(state_node_SI_sim,"Data/SI_sim_state_node_CP_M1_M2.csv", row.names= FALSE) # (intermediate file)

## -- aggregate habitat within simulated management scenario 
SI_sim<-lapply(sem_int_sim_no_aggr,function(data) {
  data %>%  mutate(management = "SI") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

SI_sim<-bind_rows(SI_sim)
write.csv(SI_sim,"Data/SI_sim_CP_M1_M2.csv", row.names= FALSE) # (intermediate file)





##### -- Intensive (replace "WD","RG","MH","NH","GM","SF", "PP", "LP", and"NL"for "CP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 &  layer_from != 4 &
           layer_from != 5  &  layer_from != 2 &layer_from != 9 &
           layer_from != 7& layer_from != 3 &  layer_from != 6 ) %>% #links from "WD", "RG", "MH", "NH","GM","SF", "PP", "LP","LU", and"NL removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

int_edgelist_rem<-int_edgelist_rem[,c(1,2,5,4,3,7,6)]

#--  Create new habitats 

# Merge edge list of CP and the habitats to convert
LP_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 3) %>% 
  mutate (pre_hab= "LP",new_habitat = "LP_CP")

NL_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1 | layer_from == 6) %>% 
  mutate (pre_hab = "NL",new_habitat = "NL_CP")

converted_area<-rbind(LP_CP, NL_CP)


## -- Add abundances of species and modify it according to the new area (for species from CP that are in the new habitat)

# add abundances and modify those from CP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==1 |layer_id ==3 |
                                           layer_id ==6) 
new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = case_when(
    (layer_from == 1 & new_habitat == "LP_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "LP",6 ],
    (layer_from == 1 & new_habitat == "NL_CP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "NL",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from CP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 1 & new_habitat == "LP_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "LP",6 ],
      (layer_from == 1 & new_habitat == "NL_CP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "NL",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from CP according to the area of the new habitat

# remove interactions where one partner have less than 1 individual(threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 1 ~ "CP",
    layer_from == 3 ~ "LP",
    layer_from == 6 ~ "NL"),
    layer_to= case_when(
      layer_to == 1 ~ "CP",
      layer_to == 3 ~ "LP",
      layer_to == 6 ~ "NL"
    ))

# apply_rewiring expects only CP species as the new habitat baseline, not the full merged edge list
new_habitats_ab_CP <- new_habitats_ab %>%
  ungroup() %>%
  filter(layer_from == 1, ab_node_from >= 1, ab_node_to >= 1) %>%
  mutate(habitat = case_when(new_habitat == "LP_CP" ~ 18L, new_habitat == "NL_CP" ~ 19L)) %>%
  select(habitat, node_from, ab_node_from, taxon_node_from, node_to, ab_node_to, taxon_node_to)

## Run apply_rewiring once to count how many species survived via rewiring per habitat
# This count is used to update the number of species to randomly remove in the null model
new_habitats_emp <- apply_rewiring(new_habitats_ab_CP,
                                   replaced_layer_ids = c(3, 6),
                                   hab_id_map = c("3" = 18, "6" = 19),
                                   state_nodes_ab, total_resources_baseline,
                                   species_in_CP, abundances_CP, metaweb)

# Count species rewired per original habitat (not counting species already in CP)
rewired_I <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1) %>%
  filter(!node_to %in% species_in_CP) %>%
  mutate(hab_name = case_when(habitat == 18 ~ "LP", habitat == 19 ~ "NL")) %>%
  group_by(hab_name) %>%
  summarise(n_rewired = n_distinct(node_to), .groups = "drop")

# Update absent count: subtract rewired species from the original extinction count
absent_species_count_I <- absent_species_count %>%
  left_join(rewired_I, by = c("habitat" = "hab_name")) %>%
  mutate(absent_count = absent_count - replace_na(n_rewired, 0)) %>%
  select(habitat, absent_count)

# Update species_in_CP to include newly rewired species (carried forward to next scenarios)
species_rewired_I <- new_habitats_emp %>%
  filter(ab_node_from >= 1 & ab_node_to >= 1, !node_to %in% species_in_CP) %>%
  pull(node_to) %>% unique()
species_in_CP <- union(species_in_CP, species_rewired_I)

## -- Apply Mechanism 2 (rescue) for I: count additional survivors via dispersal

new_habitats_emp_rem_I   <- new_habitats_emp %>% filter(ab_node_from >= 1 & ab_node_to >= 1)
new_habitats_emp_rem_all <- bind_rows(new_habitats_emp_rem_SE, new_habitats_emp_rem_M,
                                      new_habitats_emp_rem_SI, new_habitats_emp_rem_I)

rescue_I <- apply_rescue(
  replaced_layer_ids   = c(8, 10, 4, 5, 2, 9, 7, 3, 6),
  hab_id_map           = c("8" = 11, "10" = 12, "4" = 13, "5" = 14,
                           "2" = 15, "9" = 16, "7" = 17, "3" = 18, "6" = 19),
  new_habitats_ab_rem  = new_habitats_emp_rem_all,
  state_nodes_ab       = state_nodes_ab,
  destination_edgelist = int_edgelist_rem,
  metaweb              = metaweb
)

if (!is.null(rescue_I$rescue_edges)) {
  type_b_rescued_I <- rescue_I$rescue_edges %>%
    filter(!node_to %in% species_in_CP) %>%
    pull(node_to) %>% unique()
  
  rescued_I <- data.frame(
    habitat   = c("LP", "NL"),
    n_rescued = c(
      sum(type_b_rescued_I %in% (state_nodes_ab %>% filter(layer_id == 3) %>% pull(node_id))),
      sum(type_b_rescued_I %in% (state_nodes_ab %>% filter(layer_id == 6) %>% pull(node_id)))
    )
  )
  
  absent_species_count_I <- absent_species_count_I %>%
    left_join(rescued_I, by = "habitat") %>%
    mutate(absent_count = pmax(0, absent_count - replace_na(n_rescued, 0))) %>%
    select(habitat, absent_count)
  
  species_in_CP <- union(species_in_CP, type_b_rescued_I)
  
} else {
  type_b_rescued_I <- character(0)
}


## -- Remove species at random (500 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#LP
LP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="LP")
shuff_LP<-sim_sp_removal(LP_edge_list,absent_species_count_I) #function to randomly remove the same number of species according to the original simulation
LP_clean<-shuff_LP[[1]] %>% ungroup() %>% mutate(habitat = 18) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to, iteration)

#NL
NL_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NL")
shuff_NL<-sim_sp_removal(NL_edge_list,absent_species_count_I) #function to randomly remove the same number of species according to the original simulation
NL_clean<-shuff_NL[[1]] %>% ungroup() %>% mutate(habitat = 19) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to, iteration)

#Merge shuff habitats from this management scenario (I) with the precvious mangaement (SI)
shuff_pre<-read.csv("Data/shuff_hab_SI_CP_M1_M2.csv", sep =,) # shuff habitats from previous habitat management (intermediate file)

shuff_habitats<-rbind(shuff_pre,LP_clean,NL_clean)  
write.csv(shuff_habitats,"Data/shuff_hab_I_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github (intermediate file)


# Store species removed in every iteration
sps_removed_LP <- shuff_LP[[2]] %>% mutate(habitat_from = "LP", management = "I")
sps_removed_NL <- shuff_NL[[2]] %>% mutate(habitat_from = "NL", management = "I")
sps_removed<-rbind(sps_removed_LP,sps_removed_NL)

write.csv(sps_removed,"Data/sps_removed_I_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario (intermediate file)

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (I)
int_sim_no_aggr<- comb_edge_list(int_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_I_sim<-lapply(int_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_I_sim<-bind_rows(state_node_I_sim)
write.csv(state_node_I_sim,"Data/I_sim_state_node_CP_M1_M2.csv", row.names= FALSE) # (intermediate file)


## -- aggregate habitat within simulated management scenario 
I_sim<-lapply(int_sim_no_aggr,function(data) {
  data %>%  mutate(management = "I") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

I_sim<-bind_rows(I_sim)
write.csv(I_sim,"Data/I_sim_CP_M1_M2.csv", row.names= FALSE) # (intermediate file)



##### -- Intensive non-organic
#We remove the weeds and species that only interact with them for all the intensive simulated networks

### Create edge list
I_sim_CP<-read.csv("Data/I_sim_CP_M1_M2.csv", sep =,) %>% 
  mutate(management = "IN")

weeds = 1:93 #weeds nodes 1:93
crops = 94:99
aphid = 337:364
seed_ins = 476:494
seed_bird = 495:506
seed_rod = 507:510
herbivores <- c(aphid, seed_ins, seed_bird, seed_rod)

#Objects to storage
edge_list_shuff <- data.frame() 
list_species_removed<- data.frame()
list_species_survived<- data.frame()


for (i in 1:500){
  print(i)
  iteration_net <- I_sim_CP %>% filter(iteration==i)
  
  ### Calculate edge list after eliminating species
  
  ## Remove weeds
  edge_list_weed_remov<- iteration_net %>%
    filter(!(node_from%in%weeds), !(node_to%in%weeds))   #eliminate weeds and species that only interact with them
  
  ## Remove herbivores that feeds on weeds
  
  # Step 1: Identify herbivores that interact with crops
  interact_with_crops <- iteration_net %>%
    filter((node_from %in% herbivores & node_to %in% crops) | 
             (node_to %in% herbivores & node_from %in% crops)) %>%
    select(node_from, node_to) %>%
    unlist() %>%
    as.numeric() %>%
    unique()
  
  herbivores_crops<-interact_with_crops[interact_with_crops > 99] #list of pest
  
  # Step 2: Filter the dataset to include only interactions between herbivores and weeds, excluding those that interact with crops
  interact_without_crops <- iteration_net %>%
    filter(
      ((node_from %in% herbivores & node_to %in% weeds) | 
         (node_to %in% herbivores & node_from %in% weeds)) & 
        !(node_from %in% herbivores_crops | node_to %in% herbivores_crops)
    ) %>%  select(node_from, node_to) %>%
    unlist() %>%
    as.numeric() %>%
    unique()
  
  herbivores_only_weeds<- interact_without_crops[interact_without_crops > 99] #list of herbivores that only interact with weeds
  
  ## Final edge list: remove herbivores_only_weeds and their interactions in the dataframe
  edge_list_remov<-edge_list_weed_remov %>% 
    filter(
      !(node_from %in% herbivores_only_weeds) & !(node_to %in% herbivores_only_weeds))  # Exclude all interactions of herbivores only feed on weeds
  
  
  # Store new edge list
  edge_list_shuff <- rbind(edge_list_shuff, edge_list_remov)
  
  
  ## Create information of the removed species
  
  # Identify species in the network
  unique_species <- iteration_net %>%
    select(node_from, node_to) %>%
    unlist() %>%
    unique()
  
  # Identify species that remains in the network after removing weeds
  remain_species<- edge_list_remov  %>%
    distinct(node_from, node_to) %>% 
    unlist() %>% 
    unique()
  remain_sps<- cbind(iteration = i, remain_species = remain_species)
  
  #Store the information
  list_species_survived<-rbind(list_species_survived, remain_sps)
  
  
  # Identify species were removed from the network
  sp_removed<- setdiff(unique_species, remain_species)
  
  # Estimate the degree
  for (j in sp_removed){
    sp_removed = j
    degree<- iteration_net %>%ungroup() %>% 
      filter(node_from== j| node_to ==j) %>% distinct(node_from,node_to) %>%  summarise(degree = n())
    degree_sp<-cbind(species_rem = j,degree, iteration =i, habitat_from = "-",management = "IM")
    
    #Store the information
    list_species_removed<-rbind(list_species_removed, degree_sp) 
  }
  
}

IM_sim = edge_list_shuff #edge list
write.csv(IM_sim,"Data/IM_sim_CP_M1_M2.csv", row.names= FALSE) # (intermediate file)

sps_removed = list_species_removed #list of species removed
write.csv(sps_removed,"Data/sps_removed_IM_CP_M1_M2.csv", row.names= FALSE) #save to add in the next management scenario

sps_survived = list_species_survived #remaining species


### Create state node
I_sim_state_node<-read.csv("Data/I_sim_state_node_CP_M1_M2.csv", sep =,) 

# Filter according to the node that remains in each simulation
state_node_IM_sim <- sps_survived %>%  
                  left_join(I_sim_state_node, by= c("iteration","remain_species" = "node_id"))

write.csv(state_node_IM_sim,"Data/IM_sim_state_node_CP_M1_M2.csv", row.names= FALSE) #  (intermediate file)




##### -- Final Dataframe

## - Edge list

#upload simulated habitat management and empirical
SE_sim<-read.csv("Data/SE_sim_CP_M1_M2.csv", sep =,) 
SE_sim$iteration<-as.character(SE_sim$iteration)

M_sim<-read.csv("Data/M_sim_CP_M1_M2.csv", sep =,) 
M_sim$iteration<-as.character(M_sim$iteration)

SI_sim<-read.csv("Data/SI_sim_CP_M1_M2.csv", sep =,) 
SI_sim$iteration<-as.character(SI_sim$iteration)

I_sim<-read.csv("Data/I_sim_CP_M1_M2.csv", sep =,) 
I_sim$iteration<-as.character(I_sim$iteration)

IM_sim<-read.csv("Data/IM_sim_CP_M1_M2.csv", sep =,) 
IM_sim$iteration<-as.character(IM_sim$iteration)

#Upload empirical
Emp<-read.csv("Data/Land_use_edgelist_M1_M2.csv", sep =,) %>% 
    mutate(iteration = "Emp") %>%  select(management,iteration,node_from,node_to)

## Final Edgelist
edge_list_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim,IM_sim)
#write.csv(edge_list_sim,"Data/edge_list_sim_CP_M1_M2.csv", row.names= FALSE)


## - State nodes

#upload simulated habitat management and empirical

SE_sim<-read.csv("Data/SE_sim_state_node_CP_M1_M2.csv", sep =,) %>% 
        mutate(management = "SE") %>%  select(management, iteration,node_id,taxon,abun)
SE_sim$iteration<-as.character(SE_sim$iteration) 

M_sim<-read.csv("Data/M_sim_state_node_CP_M1_M2.csv", sep =,) %>% 
  mutate(management = "M") %>%  select(management, iteration,node_id,taxon,abun)
M_sim$iteration<-as.character(M_sim$iteration)

SI_sim<-read.csv("Data/SI_sim_state_node_CP_M1_M2.csv", sep =,) %>% 
  mutate(management = "SI") %>%  select(management, iteration,node_id,taxon,abun)
SI_sim$iteration<-as.character(SI_sim$iteration)

I_sim<-read.csv("Data/I_sim_state_node_CP_M1_M2.csv", sep =,) %>% 
  mutate(management = "I") %>%  select(management, iteration,node_id,taxon,abun)
I_sim$iteration<-as.character(I_sim$iteration)

IM_sim<-read.csv("Data/IM_sim_state_node_CP_M1_M2.csv", sep =,) %>% rename (node_id=remain_species) %>% 
  mutate(management = "IN") %>%  select(management, iteration,node_id,taxon,abun)
IM_sim$iteration<-as.character(IM_sim$iteration)

#Upload empirical
Emp<-read.csv("Data/Land_use_state_nodes_M1_M2.csv", sep =,) %>% 
  mutate(iteration = "Emp") %>%  select(management,iteration,node_id,taxon,abun)

## Final state node list
state_node_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim,IM_sim)
write.csv(state_node_sim,"Data/state_node_sim_CP_M1_M2.csv", row.names= FALSE)



## - Species removed
SE_sim<-read.csv("Data/sps_removed_SE_CP_M1_M2.csv", sep =,) 
SE_sim$iteration<-as.character(SE_sim$iteration)

M_sim<-read.csv("Data/sps_removed_M_CP_M1_M2.csv", sep =,) 
M_sim$iteration<-as.character(M_sim$iteration) 

SI_sim<-read.csv("Data/sps_removed_SI_CP_M1_M2.csv", sep =,) 
SI_sim$iteration<-as.character(SI_sim$iteration) 

I_sim<-read.csv("Data/sps_removed_I_CP_M1_M2.csv", sep =,) 
I_sim$iteration<-as.character(I_sim$iteration) 

IM_sim<-read.csv("Data/sps_removed_IM_CP_M1_M2.csv", sep =,) 
IM_sim$iteration<-as.character(IM_sim$iteration) 

## Final data of species removed
sps_removed_sim<-rbind(SE_sim,M_sim,SI_sim,I_sim,IM_sim)
write.csv(sps_removed_sim,"Data/sps_removed_sim_CP_M1_M2.csv", row.names= FALSE)  # (intermediate file)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             ESTIMATION OF NCP PROVISION AND INDIRECT EFFECT ON NCP                
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For each shuffled network, we calculate the same variables as we did in the empirical network


############# 1. Calculate NCP provision 

state_node_sim<-read.csv("Data/state_node_sim_CP_M1_M2.csv", sep =,) #load state_node_list
body_mass<-read.csv("Data/biomass.csv",header=T)  #load species' biomass dataframe


## Add information of ES provision to the state_node_list
nodes_ES<- right_join(state_node_sim, Norwood_farm$nodes, by = "node_id")%>% 
  select(management,iteration,node_id,taxon.x,abun, "Crop production",
         "Pollination", "Crop damage", "Pest control", "Seed dispersal", "Butterfly watching", "Bird watching") %>% 
      group_by(management, iteration, node_id) %>% rename("taxon" = "taxon.x") %>% 
  gather("services","value", 6:12) #we conserve species that not directly provide NCP because can serve as intermediate hop 



############# 2.  Estimate the amount of NCP provision per species

#The equation to estimate the amount will change according to the type of NCP.
#For bird watching and butterfly watching is just the abundance. For the rest is the product between abundance and biomass

direct_ES <- nodes_ES %>% filter (value ==1) %>% 
  left_join(body_mass,by = "node_id") %>% select(-node_name,-taxon.y) %>% 
  rename("taxon"="taxon.x", "body_mass" = "biomass.g") %>% 
  mutate (type = "D",
          weight = abun * body_mass) %>% #amount of NCP provision 
  select(-value) 


write.csv(direct_ES,"Data/direct_ES_sim_CP_M1_M2.csv", row.names= FALSE)# (Intermediate file)




############# 3. Estimate indirect effects of species on NCP provision  

### --  Prepare dataframe

# Full list nodes with NCP in the network (considering those that provide and not provide NCP)
list_nodes_ES_provi<-nodes_ES %>% ungroup() %>% select(-management,-iteration,-abun) %>%
  filter (value ==1) %>% unique ()# list of nodes that provide E(D)S

list_nodes_ES_no_provi<-nodes_ES %>% ungroup() %>% select(-management,-iteration,-abun,-services) %>% unique() %>% 
                    group_by(node_id) %>% mutate(tot_serv = sum(value)) %>% select(-value) %>% 
                    filter(tot_serv == 0) %>% mutate(services = "None", value = 1) %>% # filter species that not directly provide any NCP and assign them as None
                    select(-tot_serv)


list_nodes_ES<-rbind(list_nodes_ES_provi,list_nodes_ES_no_provi) #Total list of nodes with NCP (with None)


# Add attributes of nodes in the edgelist
edge_list_sim<-read.csv("Data/edge_list_sim_CP_M1_M2.csv", sep =,) #load edge_list of shuffled networks

edge_list<- left_join(edge_list_sim,list_nodes_ES, by = c("node_from"="node_id")) %>% 
  rename("taxon_from"="taxon", "services_from"="services") %>% select(-value) %>% 
  left_join(list_nodes_ES, by = c("node_to"="node_id"))  %>% 
  rename("taxon_to"="taxon", "services_to"="services") %>% select(-value)


# Add inverted links (to make the code easier to program when calculate indirect interactions. It will not affect the results)
edge_list_inverted<- tibble(values =  edge_list$management,edge_list$iteration,  edge_list$node_to,
                            edge_list$node_from,edge_list$taxon_to,edge_list$services_to,
                            edge_list$taxon_from,edge_list$services_from)
                              
colnames(edge_list_inverted) <- c("management","iteration","node_from", "node_to", "taxon_from", "services_from",
                                   "taxon_to", "services_to")


# Combine both data frame to create the final edge list
edgelist_final<- bind_rows(edge_list, edge_list_inverted)
edgelist_final<-edgelist_final[,c(1,2,3,5,6,4,7,8)]



### --  Calculate 1st order pathway of indirect effects on NCP (considering 1 hop: node-node)

Indirect_1hop_sim<-edgelist_final %>% 
                  select(management,iteration,services_from,node_from,node_to,taxon_from, services_to) %>% 
                  mutate(hop = 1, type = "I")  


# we remove duplicates rows where node_from = birds or butterflies cause they represent the same interaction. 
# This happens because each row represents an attribute and these taxons have 2 and 3 attributes per node.

rows_birds_butt <- Indirect_1hop_sim %>%
  filter(taxon_from == "Butterfly" | taxon_from == "Seed-feeding bird") %>%
  distinct(management, iteration, node_from, node_to, .keep_all = TRUE) # new subset after eliminating duplicate rows for node_from = birds and butterflies

int_without<-Indirect_1hop_sim %>% filter(!(taxon_from == "Butterfly" | 
                                                         taxon_from == "Seed-feeding bird")) #eliminate the interactions containing node_from =birds or butterflies from the original dataframe


Indirect_1hop_sim_2<-rbind(rows_birds_butt,int_without)#final dataframe containing indirect effects on NCP via 1 hop

write.csv(Indirect_1hop_sim_2,"Data/ind_1hop_sim_CP_M1_M2.csv", row.names= FALSE) #3 (Intermediate file)


### --  Calculate 2nd order pathways of indirect effects on NCP (considering 2 hops: node 1 - node 2 - node 3, effect of node 1 on node 3'NCP via node 2)
library(parallel)

Indirect_1hop <- read.csv("Data/ind_1hop_sim_CP_M1_M2.csv", sep = ",")
n_cores <- detectCores() - 1

rm(SE_2hop, M_2hop, SI_2hop, I_2hop)  # remove the old full-size objects if they still exist
gc()
run_2hop_for_stage <- function(stage_name) {
  cat("=== Processing management:", stage_name, "===\n")
  
  stage_data <- Indirect_1hop %>% filter(management == stage_name)
  iter_list <- stage_data %>% group_by(iteration) %>% group_split()
  
  results <- mclapply(iter_list, process_iter, mc.cores = n_cores)
  
  failed <- which(sapply(results, function(x) inherits(x, "try-error")))
  if (length(failed) > 0) {
    cat("FAILED iterations for", stage_name, ":", length(failed), "\n")
  } else {
    cat("All", length(results), "iterations succeeded for", stage_name, "\n")
  }
  
  stage_result <- bind_rows(results)
  n_rows <- nrow(stage_result); n_iter <- n_distinct(stage_result$iteration)
  write.csv(stage_result, paste0("Data/ind_2hop_sim_CP_M1_M2_", stage_name, ".csv"), row.names = FALSE)
  
  rm(stage_result, results, iter_list, stage_data); gc()   # free memory before returning
  
  return(data.frame(stage = stage_name, n_rows = n_rows, n_iter = n_iter, rows_per_iter = n_rows/n_iter))
}

## Run one at a time -- check the console output / failed count after EACH before moving to the next
summary_SE <- run_2hop_for_stage("SE")
summary_M  <- run_2hop_for_stage("M")
summary_SI <- run_2hop_for_stage("SI")
summary_I  <- run_2hop_for_stage("I")
summary_IN <- run_2hop_for_stage("IN")

bind_rows(summary_SE, summary_M, summary_SI, summary_I, summary_IN)


files <- paste0("Data/ind_2hop_sim_CP_M1_M2_", c("SE","M","SI","I","IN"), ".csv")

## Write the first file's header + data, then append the rest without header
file.copy(files[1], "Data/ind_2hop_sim_CP_M1_M2.csv", overwrite = TRUE)
for (f in files[-1]) {
  data_chunk <- read.csv(f)
  write.table(data_chunk, "Data/ind_2hop_sim_CP_M1_M2.csv", sep = ",", 
              row.names = FALSE, col.names = FALSE, append = TRUE)
  rm(data_chunk); gc()
}


## Once all 5 confirm 0 failed iterations, combine into the same file the rest of the pipeline expects
library(data.table)

## Read the small 1-hop file once (this one is manageable in size)
ite <- 1:500
Indirect_1hop_full <- fread("Data/ind_1hop_sim_CP_M1_M2.csv") %>%
  filter((iteration == "Emp" | iteration %in% ite) & services_to != "None") %>%
  rename("services" = "services_from",
         "node_id"  = "node_from",
         "taxon"    = "taxon_from") %>%
  mutate(node_int = NA)

stages <- c("SE", "M", "SI", "I", "IN")
output_file <- "Data/Indirect_ES_sim_CP_M1_M2.csv"
if (file.exists(output_file)) file.remove(output_file)

for (s in stages) {
  cat("=== Merging stage:", s, "===\n")
  
  ## Read only THIS stage's 2-hop file (much smaller than the combined one)
  hop2 <- fread(paste0("Data/ind_2hop_sim_CP_M1_M2_", s, ".csv"))
  setnames(hop2, "taxon_from", "taxon")
  
  hop1 <- Indirect_1hop_full %>% filter(management == s)
  
  stage_combined <- rbindlist(list(hop1, hop2), use.names = TRUE)
  
  fwrite(stage_combined, output_file, append = file.exists(output_file))
  
  rm(hop2, hop1, stage_combined); gc()
  cat("Done:", s, "-- rows written this stage above\n")
}

rm(Indirect_1hop_full); gc()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    STATISTICAL ANALYSIS                                    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


################### Z - SCORE

# In this analysis, we compare the z-score to check if the response variables (Prop. of NCP providers retained,
#relative change in the amount of NCP, and proportion of indirect effects on NCP provision retained) within each management and NCP change between
#the converted and randomized networks


#### 1. Proportion of NCP providers retained across land use change --

#upload and prepare dataframe
ite = 1:500
direct_ES<- read.csv("Data/direct_ES_sim_CP_M1_M2.csv", sep =",") %>% filter(iteration == "Emp"|iteration%in%ite) 
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors

# Observed (empirical)
direct_obs <-direct_ES %>% filter(iteration == "Emp") %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of ES retained across habitat management
  dplyr::select(management,services,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>%  filter(management != "E")


# shuffled 
direct_shuff<- direct_ES %>% filter(!(iteration == "Emp" & management !="E"))%>% group_by(management,iteration,services) %>% 
  mutate(tot = n(),
         tot_emp = case_when( #Values of denominator
           services == "Crop production"~ 6,
           services == "Pollination" ~ 117,
           services == "Crop damage"~ 15,
           services == "Pest control"~ 28,
           services == "Seed dispersal" ~ 5,
           services == "Butterfly watching"~ 16,
           services == "Bird watching"~ 7 )) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/tot_emp) %>%  #prop of ES retained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% filter(management !="E")

# calculate Z-score
dir_ES_z_score <- 
  inner_join(direct_obs,
             direct_shuff %>% select(-iteration) %>% 
               group_by(management,services) %>% 
               summarise(dir_shuff_mean=mean(Prop_mean), dir_shuff_sd=sd(Prop_mean), n=n())) %>% 
          drop_na() %>% 
          mutate(z=(Prop_mean-dir_shuff_mean)/dir_shuff_sd)

dir_ES_z_score %<>%
  dplyr::mutate(signif=case_when(z>1.96 ~ 'above', # Obs is more than the shuffled
                          z< -1.96 ~ 'below', # Obs is lower than the shuffled
                          z<=1.96 | z>=-1.96 | z == "NaN" ~ 'not signif')) 

write.csv(dir_ES_z_score,"Data/z_score_dir_ES_CP_M1_M2.csv", row.names= FALSE)




#### 2. Change in the amount of NCP across land use change --

### upload and prepare dataframe

## Calculate ratio of change for empirical
direct_ES_emp<- read.csv("Data/Land_use_dir_ES_M1_M2.csv", sep =",") #upload empirical

#amount Bird and butterfly watching
tot_services_emp_watching<-direct_ES_emp %>% filter(management=="E" &  (services == "Bird watching" | services == "Butterfly watching" )) %>% 
  group_by(management,services) %>% 
  summarize(tot_empirical_amount = sum(abun))

Prop_weight_watching<-  direct_ES_emp %>% group_by(management,services) %>% 
  filter (services == "Bird watching" | services == "Butterfly watching") %>% 
  summarize(tot= sum(abun))%>% ungroup() %>%  
  mutate(Extensive_tot = case_when(
    services == "Bird watching"~ 2076,
    services == "Butterfly watching"~ 6903),
    ratio_change = tot / Extensive_tot)  

#amount the rest NCPs
tot_services_emp_rest<-direct_ES_emp %>% filter(management=="E" &  !(services == "Bird watching" | services == "Butterfly watching" )) %>% 
  group_by(management,services) %>% 
  summarize(tot_empirical_amount = sum(weight))

Prop_weight_rest<-  direct_ES_emp %>% group_by(management,services) %>% 
  filter (!(services == "Bird watching" | services == "Butterfly watching")) %>% 
  summarize(tot= sum(weight))%>% ungroup() %>%  
  mutate(Extensive_tot = case_when(
    services == "Crop damage"~ 711450.9469,
    services == "Crop production"~ 209300.0000,
    services == "Pest control"~ 7108.3167,
    services == "Pollination"~ 36736.7426,
    services == "Seed dispersal"~ 362197.4900),
    ratio_change = tot / Extensive_tot)  

#merge the data
amount_obs<- rbind(Prop_weight_watching,Prop_weight_rest)

### Calculate ratio of change for shuffled 
direct_ES_sim<- read.csv("Data/direct_ES_sim_CP_M1_M2.csv", sep =",")

#Amount watching
amount_shuff_watch <- direct_ES_sim %>% filter (management !="E") %>% 
  filter(services == "Bird watching" | services == "Butterfly watching" ) %>%
  group_by(management,iteration,services) %>% 
  summarize(tot_sim_amount = sum(abun)) %>% 
  left_join(amount_obs[,c(1,2,4)], by = c("management", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = tot_sim_amount / Extensive_tot)

#Amount of the rest of NCPs
amount_shuff_rest <- direct_ES_sim %>% filter (management !="E") %>% 
  filter(!(services == "Bird watching" | services == "Butterfly watching" )) %>%
  group_by(management,iteration,services) %>% 
  summarize(tot_sim_amount= sum(weight)) %>% 
  left_join(amount_obs[,c(1,2,4)], by = c("management", "services"), suffix = c("", "_extensive")) %>% 
  mutate(ratio_change = tot_sim_amount / Extensive_tot) 

#merge the data
amount_shuff<- rbind(amount_shuff_watch,amount_shuff_rest)


###  calculate Z-score
amount_ES_z_score <- 
  inner_join(amount_obs,
             amount_shuff %>% select(-iteration) %>% 
               group_by(management,services) %>% 
               summarise(amount_shuff_mean=mean(ratio_change), amount_shuff_sd=sd(ratio_change), n=n())) %>% 
  drop_na() %>% 
  mutate(z=(ratio_change-amount_shuff_mean)/amount_shuff_sd)

amount_ES_z_score %<>%
  mutate(signif=case_when(z>1.96 ~ 'above', # Obs is more than the shuffled
                          z< -1.96 ~ 'below', # Obs is lower than the shuffled
                          z<=1.96 | z>=-1.96 ~ 'not signif'))

write.csv(amount_ES_z_score,"Data/z_score_amount_ES_CP_M1_M2.csv", row.names= FALSE)


#### 3. Proportion of indirect effects on NCP provision retained across land use change --
output_ind_ES_emp <- read.csv("Data/Land_use_ind_ES_M1_M2.csv", sep = ",")
output_ind_ES_emp$management <- factor(output_ind_ES_emp$management, levels = c("E", "SE", "M", "SI", "I", "IN"))

output_ind_ES_orig <- fread("Data/Indirect_ES_sim_CP_M1_M2.csv")   
output_ind_ES_orig$management <- factor(output_ind_ES_orig$management, levels = c("E","SE","M","SI","I","IN"))

indirect_shuff_orig <- output_ind_ES_orig %>%
  filter(iteration != "Emp") %>%
  group_by(management, iteration, services_to) %>%
  summarise(tot = n(), .groups = "drop") %>%
  mutate(tot_emp = case_when(
    services_to == "Crop production"    ~ 1129,
    services_to == "Pollination"        ~ 18733,
    services_to == "Crop damage"        ~ 9992,
    services_to == "Pest control"       ~ 3272,
    services_to == "Seed dispersal"     ~ 4224,
    services_to == "Butterfly watching" ~ 3515,
    services_to == "Bird watching"      ~ 5820
  )) %>%
  mutate(Prop_mean = tot / tot_emp) %>%
  select(management, iteration, services_to, Prop_mean)

indirect_obs <- output_ind_ES_emp %>%
  group_by(management, services_to) %>%
  mutate(tot = n()) %>% ungroup() %>%
  group_by(services_to) %>%
  mutate(prop = tot / max(tot)) %>%
  dplyr::select(management, services_to, tot, prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% filter(management != "E")

indir_ES_z_score_orig <- inner_join(
  indirect_obs,
  indirect_shuff_orig %>% group_by(management, services_to) %>%
    summarise(ind_shuff_mean = mean(Prop_mean), ind_shuff_sd = sd(Prop_mean), n = n(), .groups = "drop"),
  by = c("management", "services_to")
) %>%
  drop_na() %>%
  mutate(z = (Prop_mean - ind_shuff_mean) / ind_shuff_sd,
         signif = case_when(z > 1.96 ~ 'above', z < -1.96 ~ 'below', TRUE ~ 'not signif')) %>%
  ungroup()


indir_ES_z_score_orig %>%
  arrange(services_to, management) %>%
  group_by(services_to) %>%
  mutate(prev = lag(ind_shuff_mean), increased = ind_shuff_mean > prev) %>%
  filter(increased == TRUE)

write.csv(indir_ES_z_score_orig, "Data/z_score_ind_ES_CP_M1_M2.csv", row.names = FALSE)






## Load the precomputed counts (tiny file, no memory risk)
ind_counts <- read.csv("Data/ind_ES_counts_by_stage_iter_service.csv")
ind_counts$management <- factor(ind_counts$management, levels = c("E", "SE", "M", "SI", "I", "IN"))

## Shuffled proportions (exclude "Emp", same as before)
indirect_shuff <- ind_counts %>%
  filter(iteration != "Emp") %>%
  mutate(tot_emp = case_when(
    services_to == "Crop production"    ~ 1129,
    services_to == "Pollination"        ~ 18733,
    services_to == "Crop damage"        ~ 9992,
    services_to == "Pest control"       ~ 3272,
    services_to == "Seed dispersal"     ~ 4224,
    services_to == "Butterfly watching" ~ 3515,
    services_to == "Bird watching"      ~ 5820
  )) %>%
  mutate(Prop_mean = tot / tot_emp) %>%
  select(management, iteration, services_to, Prop_mean)

## Z-score against the empirical (indirect_obs should still be in your session from before)
indir_ES_z_score <- inner_join(
  indirect_obs,
  indirect_shuff %>% group_by(management, services_to) %>%
    summarise(ind_shuff_mean = mean(Prop_mean), ind_shuff_sd = sd(Prop_mean), n = n(), .groups = "drop"),
  by = c("management", "services_to")
) %>%
  drop_na() %>%
  mutate(z = (Prop_mean - ind_shuff_mean) / ind_shuff_sd,
         signif = case_when(z > 1.96 ~ 'above', z < -1.96 ~ 'below', TRUE ~ 'not signif'))

## Monotonicity check -- should now come back EMPTY
null_summary <- indirect_shuff %>%
  group_by(management, services_to) %>%
  summarise(ind_shuff_mean = mean(Prop_mean), .groups = "drop") %>%
  arrange(services_to, management)

null_summary %>%
  group_by(services_to) %>%
  mutate(prev = lag(ind_shuff_mean), increased = ind_shuff_mean > prev) %>%
  filter(increased == TRUE)

## "Times higher": converted retained MORE indirect effects than randomized
indir_ratio_above <- indir_ES_z_score %>%
  group_by(services_to) %>% mutate(n_total = n()) %>%
  filter(signif == "above") %>%
  mutate(times_higher = Prop_mean / ind_shuff_mean) %>%
  group_by(services_to) %>%
  summarise(mean_times_higher = mean(times_higher), n_above = n(), n_total = first(n_total), .groups = "drop") %>%
  arrange(desc(mean_times_higher))

print(indir_ratio_above, n = Inf)

indir_ES_z_score <- indir_ES_z_score %>% ungroup()
table(indir_ES_z_score$signif)
indir_ES_z_score %>% select(management, services_to, Prop_mean, ind_shuff_mean, ind_shuff_sd, z, signif) %>% arrange(desc(z)) %>% print(n = Inf)



## Ungroup first -- indir_ES_z_score may be carrying leftover grouping from indirect_obs
indir_ES_z_score <- indir_ES_z_score %>% ungroup()

## How many rows fall into each category overall?
table(indir_ES_z_score$signif)

## Look at the z distribution directly
indir_ES_z_score %>% select(management, services_to, Prop_mean, ind_shuff_mean, ind_shuff_sd, z, signif) %>% arrange(desc(z)) %>% print(n = Inf)

## Empirical
empirical_plot <- indirect_obs %>%
  select(management, services_to, Prop_mean) %>%
  mutate(type = "Empirical")

## Null model: average retained proportion across the 500 iterations, per management & NCP
null_plot <- indirect_shuff %>%
  group_by(management, services_to) %>%
  summarise(Prop_mean = mean(Prop_mean), .groups = "drop") %>%
  mutate(type = "Null")

## Combine into one plotting dataframe
plot_data <- bind_rows(empirical_plot, null_plot)
plot_data$management <- factor(plot_data$management, levels = c("E","SE","M","SI","I","IN"))
plot_data$type <- factor(plot_data$type, levels = c("Empirical","Null"))

## Colors per NCP type (swap in your actual palette if you already have one elsewhere)
color_ncp <- c("Bird watching" = "#1b9e77", "Butterfly watching" = "#d95f02",
               "Crop damage" = "#7570b3", "Crop production" = "#e7298a",
               "Pest control" = "#377eb8", "Pollination" = "#e6ab02",
               "Seed dispersal" = "#a6761d")

## Plot
ggplot(plot_data, aes(x = management, y = Prop_mean)) +
  geom_boxplot(aes(color = type), fill = NA, outlier.shape = NA,
               position = position_dodge(width = 0.6), width = 0.5) +
  geom_point(aes(fill = services_to, shape = type, group = type),
             position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0.08),
             size = 2.8, stroke = 0.4, color = "black") +
  scale_color_manual(values = c(Empirical = "black", Null = "red"), name = "Type") +
  scale_shape_manual(values = c(Empirical = 21, Null = 24), name = "Type") +
  scale_fill_manual(values = color_ncp, name = "NCP") +
  labs(x = "Land conversion", y = "Prop. of indirect effects on NCP provision retained") +
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "grey50"))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

null_summary <- indirect_shuff %>%
  group_by(management, services_to) %>%
  summarise(ind_shuff_mean = mean(Prop_mean), .groups = "drop") %>%
  arrange(services_to, management)

print(null_summary, n = Inf)

## Check monotonicity directly: does retained proportion ever INCREASE as conversion intensifies?
null_summary %>%
  group_by(services_to) %>%
  mutate(prev = lag(ind_shuff_mean),
         increased = ind_shuff_mean > prev) %>%
  filter(increased == TRUE)




ind_1hop_check <- read.csv("Data/ind_1hop_sim_CP_M1_M2.csv", sep = ",")

ind_1hop_check %>%
  filter(iteration != "Emp", services_to != "None") %>%
  group_by(management) %>%
  summarise(n_rows = n(), n_iter = n_distinct(iteration), rows_per_iter = n_rows / n_iter, .groups = "drop")

