################### NULL MODEL (First version) ##################################

#We want to test if the results are related to the identity of species we remove when converting habitats.
#We can test if the identity of species that are lost in each habitat after conversion affects the results or not. 
#This null model maintains the number of species in the new habitat as in the original simulation.


library(emln)#multilayer package
library(readr)
library(ggplot2)
library(cowplot)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")
source("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio/Exploratory/Scenario_ES/functions.R")

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
areas<-read.csv("Data/habitatarea.csv", sep =",") %>% # LU as CP already merged
  filter(HabitatCode != "ST") %>% #remove standing trees
  mutate(HabitatCode = case_when(HabitatCode == "C"~ "CP",
                                 HabitatCode == "WU"~ "WD",
                                 TRUE~HabitatCode))

habitat_area <- areas %>% mutate(area_ave = case_when(
  (Area_2007 >0) & (Area_2008 >0) ~ (Area_2007+Area_2008)/2, #if the same habitat was present in both years do the average
  (Area_2007 >0) & (Area_2008  ==0)~ Area_2007, #if the habitat was present in one year, keep the area of the year
  (Area_2007 ==0) & (Area_2008  >0)~ Area_2008)) %>% 
  mutate(mult_ab = area_ave/49.25) # the factor we should modify the abundance from CP species according to the new habitat






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      SIMULATION                        
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(123)


############# 1. Species to remove

#Here we identify the number of species to randomly remove in each habitat when convert from extensive to intensive scenarios.
#The number of species to remove should be the same as in the original simulation.

# Find species present in PP
species_in_pp <- unique(Norwood_farm$state_nodes[Norwood_farm$state_nodes$layer_name == 'PP', 'node_id'])

# Find species in each habitat and compare with CP
unique_habitats <- unique(Norwood_farm$state_nodes$layer_name)
absent_species_count <- data.frame(habitat = character(), absent_count = integer())

# Identify the number of species to remove 
for (hab in unique_habitats) {
  if (hab != 'PP') {
    species_in_habitat <- unique(Norwood_farm$state_nodes[Norwood_farm$state_nodes$layer_name == hab, 'node_id'])
    absent_count <- sum(!species_in_habitat$node_id %in% species_in_pp$node_id)
    absent_species_count <- rbind(absent_species_count, data.frame(habitat = hab, absent_count = absent_count))
  }
}


############# 2. Land use change simulation


##### -- Extensive scenario

extensive_edgelist<- Norwood_farm$extended_ids %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>% 
  mutate(management = "E") %>% select(-habitat,-weight) %>% unique() #aggregate network

 
# estimate relative abundances of species in the aggregated network
ab_ext<-state_nodes_ab %>% select(-layer_id) %>% group_by(node_id,taxon) %>%
  mutate(abun = sum(abundance)) %>% distinct(abun) %>% group_by(taxon) %>% 
  mutate(tot_ab_taxon = sum(abun)) %>% #total abundance per taxon
  group_by(node_id) %>% 
  mutate(rel_ab=abun/tot_ab_taxon)#relative abundance per sp


# incorporate rel abundances to the edge list and calculate the weight (Product of relative abundances)
ext_edgelist_aggr<- extensive_edgelist %>% left_join(ab_ext, by = c("node_from" = "node_id")) %>% 
    left_join(ab_ext, by = c("node_to" = "node_id")) %>% 
  rename ("rel_ab_from" = "rel_ab.x", "rel_ab_to" = "rel_ab.y") %>% 
  mutate(weight = rel_ab_from * rel_ab_to) %>% #calculate weight
  select(node_from,node_to,weight,management)




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

sem_ext_edgelist_rem<-sem_ext_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


##--  Create new habitats 

# Merge edge list of PP and the habitats to convert
WD_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 10) %>% 
        mutate (pre_hab= "WD",new_habitat = "WD_PP")

RG_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 8) %>% 
        mutate (pre_hab = "RG",new_habitat = "RG_PP")

converted_area<-rbind(WD_PP, RG_PP) 

## -- Add abundances of species and modify it according to the new area (for species from PP that are in the new habitat)

# add abundances and modify those from PP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==7 |layer_id ==8 |
                                         layer_id ==10) 
new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
    mutate(ab_node_from = case_when(
    (layer_from == 7 & new_habitat == "WD_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "WD",6 ],
    (layer_from == 7 & new_habitat == "RG_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "RG",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from PP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 7 & new_habitat == "WD_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "WD",6 ],
      (layer_from == 7 & new_habitat == "RG_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "RG",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from PP according to the area of the new habitat
           

# remove interactions where one partner have less than 1 individual(threshold)

new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 7 ~ "PP",
    layer_from == 8 ~ "RG",
    layer_from == 10 ~ "WD"),
    layer_to= case_when(
      layer_to == 7 ~ "PP",
      layer_to == 8 ~ "RG",
      layer_to == 10 ~ "WD"
    ))


## -- Remove species at random (600 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#WD
WD_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="WD")
shuff_WD<-sim_sp_removal_pp(WD_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
WD_clean<-shuff_WD[[1]] %>% ungroup() %>% mutate(habitat = 11) 

#RG
RG_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="RG")
shuff_RG<-sim_sp_removal_pp(RG_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
RG_clean<-shuff_RG [[1]]%>% ungroup() %>% mutate(habitat = 12) 

#Merge shuff habitats and arrange dataframe to merge with the rest of the habitats
shuff_habitats<-rbind(WD_clean,RG_clean) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                  node_to,ab_node_to,taxon_node_to,weight, iteration)

#write.csv(shuff_habitats,"Data/shuff_hab_WD_RG_PP.csv", row.names= FALSE) #save to add in the next management scenario

# Store species removed in every iteration
sps_removed_WD <- shuff_WD[[2]] %>% mutate(habitat_from = "WD", management = "SE")
sps_removed_RG <- shuff_RG[[2]] %>% mutate(habitat_from = "RG", management = "SE")
sps_removed<-rbind(sps_removed_WD,sps_removed_RG)

#write.csv(sps_removed,"Data/sps_removed_SE_PP.csv", row.names= FALSE) #save to add in the next management scenario


## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (SE)
sem_ext_sim_no_aggr<- comb_edge_list(sem_ext_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_sem_ext_sim<-lapply(sem_ext_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_sem_ext_sim<-bind_rows(state_node_sem_ext_sim)

#write.csv(state_node_sem_ext_sim,"Data/SE_sim_state_node_PP.csv", row.names= FALSE)

## -- aggregate habitat within simulated management scenario 
SE_sim<-lapply(sem_ext_sim_no_aggr,function(data) {
  data %>%  mutate(management = "SE") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

SE_sim<-bind_rows(SE_sim)
#write.csv(SE_sim,"Data/SE_sim_PP.csv", row.names= FALSE)



#####  -- Moderate (replace "WD","RG","MH"and "NH" for PP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 
mod_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 & layer_from != 4 & layer_from != 5) %>% #links from "WD", "RG", "MH", and "NH" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

mod_edgelist_rem<-mod_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


##--  Create new habitats 

# Merge edge list of PP and the habitats to convert
MH_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 4) %>% 
  mutate (pre_hab= "MH",new_habitat = "MH_PP")

NH_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 5) %>% 
  mutate (pre_hab = "NH",new_habitat = "NH_PP")

converted_area<-rbind(MH_PP, NH_PP) 

## -- Add abundances of species and modify it according to the new area (for species from PP that are in the new habitat)

# add abundances and modify those from PP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==7 |layer_id ==4 |
                                           layer_id ==5) 
new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = case_when(
    (layer_from == 7 & new_habitat == "MH_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "MH",6 ],
    (layer_from == 7 & new_habitat == "NH_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "NH",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from CP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 7 & new_habitat == "MH_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "MH",6 ],
      (layer_from == 7 & new_habitat == "NH_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "NH",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from CP according to the area of the new habitat


# remove interactions where one partner have less than 1 individual(threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 7 ~ "PP",
    layer_from == 4 ~ "MH",
    layer_from == 5 ~ "NH"),
    layer_to= case_when(
      layer_to == 7 ~ "PP",
      layer_to == 4 ~ "MH",
      layer_to == 5 ~ "NH"
    ))
  

## -- Remove species at random (600 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#MH
MH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="MH")
shuff_MH<-sim_sp_removal_pp(MH_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
MH_clean<-shuff_MH[[1]] %>% ungroup() %>% mutate(habitat = 13) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#NH
NH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NH")
shuff_NH<-sim_sp_removal_pp(NH_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
NH_clean<-shuff_NH[[1]] %>% ungroup() %>% mutate(habitat = 14) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (M) with the precvious mangaement (SE)
shuff_pre<-read.csv("Data/shuff_hab_WD_RG_PP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,MH_clean,NH_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_M_PP.csv", row.names= FALSE) #save to add in the next management scenario


# Store species removed in every iteration
sps_removed_MH <- shuff_MH[[2]] %>% mutate(habitat_from = "MH", management = "M")
sps_removed_NH <- shuff_NH[[2]] %>% mutate(habitat_from = "NH", management = "M")
sps_removed<-rbind(sps_removed_MH,sps_removed_NH)

#write.csv(sps_removed,"Data/sps_removed_M_PP.csv", row.names= FALSE) #save to add in the next management scenario

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 600 simulation of the management scenario (M)
mod_sim_no_aggr<- comb_edge_list(mod_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_mod_sim<-lapply(mod_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_mod_sim<-bind_rows(state_node_mod_sim)
#write.csv(state_node_mod_sim,"Data/M_sim_state_node_PP.csv", row.names= FALSE)

## -- aggregate habitat within simulated management scenario 
M_sim<-lapply(mod_sim_no_aggr,function(data) {
  data %>%  mutate(management = "M") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

M_sim<-bind_rows(M_sim)
#write.csv(M_sim,"Data/M_sim_PP.csv", row.names= FALSE)




##### -- Semi - intensive (replace "WD","RG","MH","NH","GM", "SF" and "CP" for "PP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 
sem_int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 &  layer_from != 4 &
           layer_from != 5 & layer_from != 2 & layer_from != 9 &
           layer_from != 1 ) %>% #links from "WD", "RG", "MH", "NH","GM", "SF" and "CP"" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

sem_int_edgelist_rem<-sem_int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


##--  Create new habitats 

# Merge edge list of PP and the habitats to convert
GM_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 2) %>% 
  mutate (pre_hab= "GM",new_habitat = "GM_PP")

SF_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 9) %>% 
  mutate (pre_hab = "SF",new_habitat = "SF_PP")

CP_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 1) %>% 
  mutate (pre_hab = "CP",new_habitat = "CP_PP")

converted_area<-rbind(GM_PP, SF_PP,CP_PP) 


## -- Add abundances of species and modify it according to the new area (for species from PP that are in the new habitat)

# add abundances and modify those from PP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==1 |layer_id ==2 |
                                           layer_id ==9| layer_id ==7) 
new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = case_when(
    (layer_from == 7 & new_habitat == "GM_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "GM",6 ],
    (layer_from == 7 & new_habitat == "SF_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "SF",6 ],
    (layer_from == 7 & new_habitat == "CP_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "CP",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from PP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 7 & new_habitat == "GM_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "GM",6 ],
      (layer_from == 7 & new_habitat == "SF_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "SF",6 ],
      (layer_from == 7 & new_habitat == "CP_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "CP",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from PP according to the area of the new habitat


# remove interactions where one partner have less than 1 individual(threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 1 ~ "CP",
    layer_from == 2 ~ "GM",
    layer_from == 9 ~ "SF",
    layer_from == 7 ~ "PP"),
    layer_to= case_when(
      layer_to == 1 ~ "CP",
      layer_from == 2 ~ "GM",
      layer_from == 9 ~ "SF",
      layer_from == 7 ~ "PP"
    ))


## -- Remove species at random (600 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#GM
GM_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="GM")
shuff_GM<-sim_sp_removal_pp(GM_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
GM_clean<-shuff_GM[[1]] %>% ungroup() %>% mutate(habitat = 15) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#SF
SF_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="SF")
shuff_SF<-sim_sp_removal_pp(SF_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
SF_clean<-shuff_SF[[1]] %>% ungroup() %>% mutate(habitat = 16) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#CP
CP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="CP")
shuff_CP<-sim_sp_removal_pp(CP_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
CP_clean<-shuff_CP[[1]] %>% ungroup() %>% mutate(habitat = 17) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (SI) with the previous mangaement (M)
shuff_pre<-read.csv("Data/shuff_hab_M_PP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,GM_clean,SF_clean,CP_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_SI_PP.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github

# Store species removed in every iteration
sps_removed_GM <- shuff_GM[[2]] %>% mutate(habitat_from = "GM", management = "SI")
sps_removed_SF <- shuff_SF[[2]] %>% mutate(habitat_from = "SF", management = "SI")
sps_removed_CP <- shuff_CP[[2]] %>% mutate(habitat_from = "CP", management = "SI")
sps_removed<-rbind(sps_removed_GM,sps_removed_SF,sps_removed_CP)

#write.csv(sps_removed,"Data/sps_removed_SI_PP.csv", row.names= FALSE) #save to add in the next management scenario


## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 600 simulation of the management scenario (SI)
sem_int_sim_no_aggr<- comb_edge_list(sem_int_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet


## -- create state_node_list of each simulated management scenario
state_node_SI_sim<-lapply(sem_int_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_SI_sim<-bind_rows(state_node_SI_sim)
#write.csv(state_node_SI_sim,"Data/SI_sim_state_node_PP.csv", row.names= FALSE)

## -- aggregate habitat within simulated management scenario 
SI_sim<-lapply(sem_int_sim_no_aggr,function(data) {
  data %>%  mutate(management = "SI") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

SI_sim<-bind_rows(SI_sim)
#write.csv(SI_sim,"Data/SI_sim_PP.csv", row.names= FALSE)




##### -- Intensive (replace "WD","RG","MH","NH","GM","SF", "CP", "LP", and"NL"for "PP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 
int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 &  layer_from != 4 &
           layer_from != 5  &  layer_from != 2 &layer_from != 9 &
           layer_from != 1 & layer_from != 3 &  layer_from != 6 ) %>% #links from "WD", "RG", "MH", "NH","GM","SF", "CP", "LP","LU", and"NL removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

int_edgelist_rem<-int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]

#--  Create new habitats 

# Merge edge list of PP and the habitats to convert
LP_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 3) %>% 
  mutate (pre_hab= "LP",new_habitat = "LP_PP")

NL_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7 | layer_from == 6) %>% 
  mutate (pre_hab = "NL",new_habitat = "NL_PP")

converted_area<-rbind(LP_PP, NL_PP)


## -- Add abundances of species and modify it according to the new area (for species from PP that are in the new habitat)

# add abundances and modify those from PP according to the new area
abundances_sp<-state_nodes_ab %>% filter(layer_id ==7 |layer_id ==3 |
                                           layer_id ==6) 
new_habitats_ab<-converted_area %>%  group_by(layer_from) %>% 
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_sp, by = c("layer_from" ="layer_id","node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = case_when(
    (layer_from == 7 & new_habitat == "LP_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "LP",6 ],
    (layer_from == 7 & new_habitat == "NL_PP") ~ ab_node_from * habitat_area[habitat_area$HabitatCode == "NL",6 ],
    TRUE~ab_node_from), #Change the abundance of species (node_from) from CP according to the area of the new habitat
    ab_node_to = case_when(
      (layer_from == 7 & new_habitat == "LP_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "LP",6 ],
      (layer_from == 7 & new_habitat == "NL_PP") ~ ab_node_to * habitat_area[habitat_area$HabitatCode == "NL",6 ],
      TRUE~ab_node_to)) #Change the abundance of species (node_to) from CP according to the area of the new habitat

# remove interactions where one partner have less than 1 individual(threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1) %>% 
  mutate(layer_from = case_when( #change  name of layers
    layer_from == 7 ~ "PP",
    layer_from == 3 ~ "LP",
    layer_from == 6 ~ "NL"),
    layer_to= case_when(
      layer_to == 7 ~ "PP",
      layer_to == 3 ~ "LP",
      layer_to == 6 ~ "NL"
    ))


## -- Remove species at random (600 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#LP
LP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="LP")
shuff_LP<-sim_sp_removal_pp(LP_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
LP_clean<-shuff_LP[[1]] %>% ungroup() %>% mutate(habitat = 18) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#NL
NL_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NL")
shuff_NL<-sim_sp_removal_pp(NL_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
NL_clean<-shuff_NL[[1]] %>% ungroup() %>% mutate(habitat = 19) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (I) with the precvious mangaement (SI)
shuff_pre<-read.csv("Data/shuff_hab_SI_PP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,LP_clean,NL_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_I_PP.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github


# Store species removed in every iteration
sps_removed_LP <- shuff_LP[[2]] %>% mutate(habitat_from = "LP", management = "I")
sps_removed_NL <- shuff_NL[[2]] %>% mutate(habitat_from = "NL", management = "I")
sps_removed<-rbind(sps_removed_LP,sps_removed_NL)

#write.csv(sps_removed,"Data/sps_removed_I_PP.csv", row.names= FALSE) #save to add in the next management scenario



## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (I)
int_sim_no_aggr<- comb_edge_list(int_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_I_sim<-lapply(int_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_I_sim<-bind_rows(state_node_I_sim)
#write.csv(state_node_I_sim,"Data/I_sim_state_node_PP.csv", row.names= FALSE)


## -- aggregate habitat within simulated management scenario 
I_sim<-lapply(int_sim_no_aggr,function(data) {
  data %>%  mutate(management = "I") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

I_sim<-bind_rows(I_sim)
#write.csv(I_sim,"Data/I_sim_PP.csv", row.names= FALSE)




##### -- Final Dataframe

## - Edge list

#upload simulated habitat management and empirical
SE_sim<-read.csv("Data/SE_sim_PP.csv", sep =,) 
SE_sim$iteration<-as.character(SE_sim$iteration)

M_sim<-read.csv("Data/M_sim_PP.csv", sep =,) 
M_sim$iteration<-as.character(M_sim$iteration)

SI_sim<-read.csv("Data/SI_sim_PP.csv", sep =,) 
SI_sim$iteration<-as.character(SI_sim$iteration)

I_sim<-read.csv("Data/I_sim_PP.csv", sep =,) 
I_sim$iteration<-as.character(I_sim$iteration)

#Upload empirical
Emp<-read.csv("Data/Land_use_rat_edgelist_weighted_PP_intense.csv", sep =,) %>% 
    mutate(iteration = "Emp") %>%  select(management,iteration,node_from,node_to)

## Final Edgelist
edge_list_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim)
#write.csv(edge_list_sim,"Data/edge_list_sim_PP.csv", row.names= FALSE)


## - State nodes

#upload simulated habitat management and empirical

SE_sim<-read.csv("Data/SE_sim_state_node_PP.csv", sep =,) %>% 
        mutate(management = "SE") %>%  select(management, iteration,node_id,taxon,abun)
SE_sim$iteration<-as.character(SE_sim$iteration) 

M_sim<-read.csv("Data/M_sim_state_node_PP.csv", sep =,) %>% 
  mutate(management = "M") %>%  select(management, iteration,node_id,taxon,abun)
M_sim$iteration<-as.character(M_sim$iteration)

SI_sim<-read.csv("Data/SI_sim_state_node_PP.csv", sep =,) %>% 
  mutate(management = "SI") %>%  select(management, iteration,node_id,taxon,abun)
SI_sim$iteration<-as.character(SI_sim$iteration)

I_sim<-read.csv("Data/I_sim_state_node_PP.csv", sep =,) %>% 
  mutate(management = "I") %>%  select(management, iteration,node_id,taxon,abun)
I_sim$iteration<-as.character(I_sim$iteration)

#Upload empirical
Emp<-read.csv("Data/Land_use_rat_state_nodes_PP_intense.csv", sep =,) %>% 
  mutate(iteration = "Emp") %>%  select(management,iteration,node_id,taxon,abun)

## Final state node list
state_node_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim)
#write.csv(state_node_sim,"Data/state_node_sim_PP.csv", row.names= FALSE)



## - Species removed
SE_sim<-read.csv("Data/sps_removed_SE_PP.csv", sep =,) 
SE_sim$iteration<-as.character(SE_sim$iteration)

M_sim<-read.csv("Data/sps_removed_M_PP.csv", sep =,) 
M_sim$iteration<-as.character(M_sim$iteration) 

SI_sim<-read.csv("Data/sps_removed_SI_PP.csv", sep =,) 
SI_sim$iteration<-as.character(SI_sim$iteration) 

I_sim<-read.csv("Data/sps_removed_I_PP.csv", sep =,) 
I_sim$iteration<-as.character(I_sim$iteration) 

## Final data of species removed
sps_removed_sim<-rbind(SE_sim,M_sim,SI_sim,I_sim)
#write.csv(sps_removed_sim,"Data/sps_removed_sim_PP.csv", row.names= FALSE) 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ESTIMATION VARIABLES                        
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For each shuffled network, we calculate the same variables as we did in the empirical network


############# 1. Calculate direct E(D)S provision 

state_node_sim<-read.csv("Data/state_node_sim_PP.csv", sep =,) #load state_node_list
body_mass<-read.csv("Data/biomass.csv",header=T)  #load species' biomass dataframe

## Add information of ES provision to the state_node_list
nodes_ES<- right_join(state_node_sim, Norwood_farm$nodes, by = "node_id")%>% 
  select(management,iteration,node_id,taxon.x,abun, "Crop production",
         "Pollination", "Crop damage", "Pest control", "Seed dispersal", "Butterfly watching", "Bird watching") %>% 
      group_by(management, iteration, node_id) %>% rename("taxon" = "taxon.x") %>% 
  gather("services","value", 6:12) #we conserve species that not directly provide ES because can serve as intermediate hop 


##  Estimate the amount of direct E(D)S provision per species (weight = abundance * body mass)

#add body mass to the dataframe
direct_ES <- nodes_ES %>% filter (value ==1) %>% 
  left_join(body_mass,by = "node_id") %>% select(-node_name,-taxon.y) %>% 
  rename("taxon"="taxon.x", "body_mass" = "biomass.g") %>% 
  mutate (type = "D",
           weight = abun * body_mass,
          output = case_when( #output + or - according to the service
            services == "Crop damage"~ "-",
             TRUE ~ "+")) %>% #weight of direct ES provision
          select(-value) 

#write.csv(direct_ES,"Data/direct_ES_sim_PP.csv", row.names= FALSE)




############# 2. Calculate indirect effects of species on E(D)S provision  

### --  Prepare dataframe

# Full list nodes with ES in the network (considering those that provide and not provide direct ES)
list_nodes_ES_provi<-nodes_ES %>% ungroup() %>% select(-management,-iteration,-abun) %>%
  filter (value ==1) %>% unique ()# list of nodes that provide E(D)S

list_nodes_ES_no_provi<-nodes_ES %>% ungroup() %>% select(-management,-iteration,-abun,-services) %>% unique() %>% 
                    group_by(node_id) %>% mutate(tot_serv = sum(value)) %>% select(-value) %>% 
                    filter(tot_serv == 0) %>% mutate(services = "None", value = 1) %>% # filter species that not directly provide any E(D)s and assign them as None
                    select(-tot_serv)


list_nodes_ES<-rbind(list_nodes_ES_provi,list_nodes_ES_no_provi) #Total list of nodes with ES (with None)


# Add attributes of nodes in the edgelist
edge_list_sim<-read.csv("Data/edge_list_sim_PP.csv", sep =,) #load edge_list of shuffled networks

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



### --  Calculate first order indirect effects  on E(D)S (considering 1 hop: node-node)
Indirect_1hop_sim<-edgelist_final %>% 
                  select(management,iteration,services_from,node_from,node_to,taxon_from, services_to) %>% 
                  mutate(hop = 1, type = "I")  


# we remove duplicates rows where node_from = birds or butterflies cause they represent the same interaction. 
# This happens because each row represents an attribute and these taxons have 2 and 3 attributes per node.

rows_birds_butt<- Indirect_1hop_sim %>%
  filter(taxon_from == "Butterfly" | taxon_from == "Seed-feeding bird") %>%
  distinct(management, node_from, node_to, .keep_all = TRUE) # new subset after eliminating duplicate rows for node_from = birds and butterflies

int_without<-Indirect_1hop_sim %>% filter(!(taxon_from == "Butterfly" | 
                                                         taxon_from == "Seed-feeding bird")) #eliminate the interactions containing node_from =birds or butterflies from the original dataframe


Indirect_1hop_sim_2<-rbind(rows_birds_butt,int_without)#final dataframe containing indirect effects on ES via 1 hop

#write.csv(Indirect_1hop_sim_2,"Data/ind_1hop_sim_PP.csv", row.names= FALSE)


### --  Calculate second order indirect effects  on E(D)S (considering 2 hops: node 1 - node 2 - node 3, effect of node 1 on node 3'E(D)S via node 2)

Indirect_1hop<-read.csv("Data/ind_1hop_sim_PP.csv",
                        sep =",") #load dataframe of indirect effects using 1 hop

Indirect_1hop<- Indirect_1hop %>% filter(management == "SE"| management == "SI", iteration ==1)
## Function to identify potential second order indirect effects for node_from in the row
ind_row <- function(df, row) { #for the row
  j <- df$node_to[row] #select node_to 
  l <- df$management[row] #select management 
  k<-df$iteration[row] #select iteration 
  
  df %>% 
    filter(node_from == j, node_to != df$node_from[row],  #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
           iteration == k, management == l) %>% # Filter dataframe (filter node 3's ES affected by node 2)
    mutate(node_id = df$node_from[row], #create dataframe to store
           node_int = j,
           taxon_from = df$taxon_from[row],
           services = df$services_from[row],
           management = l,
           iteration = k,
           type = "I",
           hop = 2)
}

Indirect_2hop_sim<- bind_rows(lapply(1:nrow(Indirect_1hop), function(row) ind_row(Indirect_1hop, row))) %>% 
                              select(management,iteration,node_id,taxon_from,services, node_int,node_to,
                                     services_to,type,hop) #run with the HPC 


#write.csv(Indirect_2hop_sim,"Data/ind_2hop_sim_PP.csv", row.names= FALSE)

# Upload dataframe of indirect effect of each simulation in each management (run in the HPC, keep 500 iterations)
E_ind_2<-read.csv("Data/HPC/E_ind_2hop_PP.csv", sep =",") #empirical extensive
SE_ind_2 <-read.csv("Data/HPC/SE_ind_2hop_sim_PP.csv", sep =",") %>% filter(iteration == "Emp" | iteration <=500)
M_ind_2 <-read.csv("Data/HPC/M_ind_2hop_sim_PP.csv", sep =",") %>% filter(iteration == "Emp" | iteration <=500)
SI_ind_2 <-read.csv("Data/HPC/SI_ind_2hop_sim_PP.csv", sep =",") %>% filter(iteration == "Emp" | iteration <=500)
I_ind_2 <-read.csv("Data/HPC/I_ind_2hop_sim_PP.csv", sep =",") %>% filter(iteration == "Emp" | iteration <=500)

Ind_2hop_sim<-rbind(E_ind_2, SE_ind_2,M_ind_2,SI_ind_2,I_ind_2) 



# Join both 1 and 2 hops indirect effects dataframes
Indirect_1hop<-read.csv("Data/ind_1hop_sim_PP.csv", sep =",") %>% 
  filter((iteration == "Emp" | iteration <=500) & services_to !="None")

# 1 hop
Indirect_1hop_sim<-Indirect_1hop %>%  rename("services" ="services_from",
                                             "node_id" = "node_from",
                                             "taxon" = "taxon_from") %>% 
  mutate(node_int = NA)

Indirect_1hop_sim<-Indirect_1hop_sim[,c(1,2,4,6,3,10,5,7,9,8)]

# 2 hop
Indirect_2hop_sim2<-Ind_2hop_sim %>% rename("taxon" = "taxon_from")

#  Total Indirect effect of ES
I_ES_sim <- rbind(Indirect_1hop_sim,Indirect_2hop_sim2) 

#write.csv(I_ES_sim,"Data/Indirect_ES_sim_PP.csv", row.names= FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Analyses                            
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#In this new manalysis we will compare the results of the empirical model with results of the null ones. WE create a null distribution and test
#it by calculating a p value. Also, we do it for every combination of management.


#### -- Proportion of direct E(D)S retained across land use change --

#upload and prepare dataframe
direct_ES<- read.csv("Data/direct_ES_sim_PP.csv", sep =",")

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


#Empirical
Prop_dir_Emp<-direct_ES %>% filter(iteration == "Emp") %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  dplyr::select(management,services,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") 

# adjust the extreme values according to the beta conditions and remove extensive scenario
Prop_dir_Emp$Prop_mean <- ifelse(Prop_dir_Emp$Prop_mean == 0, 0.000001, 
                                 ifelse(Prop_dir_Emp$Prop_mean == 1, 0.9999999, Prop_dir_Emp$Prop_mean))
# Value of empirical
values_emp<- Prop_dir_Emp %>% filter (management == "E" ) %>% select(management,tot)

#Simulations
Prop_dir_null<-direct_ES %>% filter(!(iteration == "Emp" & management !="E"))%>% group_by(management,iteration,services) %>% 
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
  mutate(prop = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") 


# adjust the extreme values according to the beta conditions and remove extensive scenario
Prop_dir_null$Prop_mean <- ifelse(Prop_dir_null$Prop_mean == 0, 0.000001, 
                                  ifelse(Prop_dir_null$Prop_mean == 1, 0.9999999, Prop_dir_null$Prop_mean))

###  - Models
library("glmmTMB")
library("stats4")
library("bbmle")
library(emmeans)
library(car)

#empirical
Prop_dir_emp<-glmmTMB (Prop_mean ~ management + services, family=beta_family(link="logit"), data = Prop_dir_Emp)
par_emp<-Anova(Prop_dir_emp)
post_man_emp <-emmeans(Prop_dir_emp, ~ management) #posthoc management
man_pairs_emp <- as.data.frame(pairs(post_man_emp))


#Null
#we do the same model for each iteration to create the null distribution of the model coefficient

iterations_coefficients<-NULL
iterations_post_hocs<-NULL


for (i in 1:500){ #for each iteration
  print(i)
  iteration_coeff<-Prop_dir_null %>% filter(iteration == i | iteration == "Emp")
  glmm_val_iterations <- glmmTMB (Prop_mean ~ management + services,family=beta_family(link="logit"), data = iteration_coeff)
  par<-Anova(glmm_val_iterations)
  post_man <-emmeans(glmm_val_iterations, ~ management) #posthoc management
  man_pairs <- as.data.frame(pairs(post_man)) 
  
  iterations_coefficients <- rbind(iterations_coefficients, 
                                   tibble(iteration = i,
                                          chisqr_man = par$Chisq[1],
                                          chisqr_ser = par$Chisq[2],
                                          pval_man = par[1,3],
                                          pval_ser = par[2,3],
                                   ))
  
  iterations_post_hocs <- rbind(iterations_post_hocs, 
                                tibble(man_pairs[,c("contrast","estimate","p.value")],
                                       iteration = rep(i,10)))
}


iterations_coefficients
#write.csv(iterations_coefficients, "Data/iterations_coefficients_direct_PP.csv", row.names = FALSE)

iterations_post_hocs
#write.csv(iterations_post_hocs, "Data/iterations_post_hocs_direct_PP.csv", row.names = FALSE)

# Compare between empirical and simulations
iterations_coefficients<-read.csv("Data/iterations_coefficients_direct_PP.csv", sep =",")

#General
greater <- sum(iterations_coefficients$chisqr_man > par_emp[1,1])
less <- sum(iterations_coefficients$chisqr_man  < par_emp[1,1])
p_management<- 2 * min(greater, less) / 500 #calculate manually t-test two tailed


#Merge simulated and empirical
iterations_sim_empirical<-rbind(iterations_coefficients,tibble(
  iteration = "Empirical",
  chisqr_man = par_emp$Chisq[1],
  chisqr_ser = par_emp$Chisq[2],
  pval_man = par_emp[1,3],
  pval_ser = par_emp[2,3],
)) %>% mutate(iteration = ifelse(iteration == "Empirical", "Empirical","Null")) %>% rename("Type "= "iteration")


#Plot general
iterations_sim_empirical<-iterations_coefficients %>% mutate(type = "Null")

iterations_sim_empirical %>% ggplot(aes(x = chisqr_man, fill= type))+ 
  geom_density(alpha = 0.6)+ 
  geom_vline(xintercept = par_emp[1,1], linetype = "dashed", color = "#FB3B1E") + #line represting rsquared empirical
  labs(x= "Statistic", y="Density")+  
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))
ggsave("distri_direct_PP.png")




# Plot combination of treatments level 
iterations_post_hocs<-read.csv("Data/iterations_post_hocs_direct_PP.csv", sep =",")

#Post hoc (each combination of management )
p_contrast_man<-NULL

for (i in unique(iterations_post_hocs$contrast)){
  
  #filter data
  post_comb<-iterations_post_hocs %>% filter(contrast == i) #filter per combination of treatmens (simulated data)
  post_emp<-man_pairs_emp %>% filter(contrast == i) #filter per combination of treatmens (empirical)
  
  #test
  
  greater <- sum(post_comb$estimate >  post_emp$estimate)
  less <- sum(post_comb$estimate  <  post_emp$estimate)
  p_comb<- 2 * min(greater, less) / 500 #calculate manually t-test two tailed
  
  #storage
  p_contrast_man <- rbind(p_contrast_man,
                          tibble(contrast = i,
                                 p_value = p_comb))
  
}
p_contrast_man

#Prepare dataframe (simulated and empircal)

#smimulated
man_lev<-c("E","SE","M","SI","I")

post_hoc_sim<- iterations_post_hocs %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
  select(-p.value)
post_hoc_sim$man_1 <- factor(post_hoc_sim$man_1, levels = man_lev)
post_hoc_sim$man_2 <- factor(post_hoc_sim$man_2, levels = man_lev)

#empirical
post_hoc_emp_pre<- man_pairs_emp %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
  select(-p.value) %>% mutate (iteration = "0") %>% select(man_1,man_2,estimate,iteration)#empirical


new_combinations_empirical <- data.frame(
  man_1 = c("E", "SE", "M", "SI", "I"),
  man_2 = c("E", "SE", "M", "SI", "I"),
  estimate = rep(0, 5),
  iteration = rep(0, 5)
)

post_hoc_emp_pre$man_1 <- factor(post_hoc_emp_pre$man_1, levels = man_lev)
post_hoc_emp_pre$man_2 <- factor(post_hoc_emp_pre$man_2, levels = man_lev)
post_hoc_emp_pre %<>% arrange(man_1,man_2)


post_hoc_emp_fin<-rbind(post_hoc_emp_pre,new_combinations_empirical)
post_hoc_emp_fin$man_1 <- factor(post_hoc_emp_fin$man_1, levels = man_lev)
post_hoc_emp_fin$man_2 <- factor(post_hoc_emp_fin$man_2, levels = man_lev)
post_hoc_emp_fin3<-post_hoc_emp_fin %>% arrange(man_1,man_2)

table(post_hoc_emp_pre$man_1,post_hoc_emp_pre$man_2)
table(post_hoc_sim$man_2,post_hoc_sim$man_1)
levels(post_hoc_emp_pre$man_2)

# plot
posthoc_distr_PP<- ggplot(data = post_hoc_sim, aes(x=estimate)) +
  geom_histogram(fill = "steelblue") +
  labs(y = "Count", x = "Estimate value") +
  geom_vline(data = post_hoc_emp_pre, mapping = aes(xintercept = estimate), 
             colour="#BB0000", linetype="dashed") +
  facet_grid(man_2 ~ man_1)
posthoc_distr_PP
#ggsave("posthoc_distri_direct_PP.png")




#### -- Proportion of indirect effect on E(D)S retained across land use change --

#upload and prepare dataframe
output_ind_ES<- read.csv("Data/Indirect_ES_sim_PP.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


#Empirical
Prop_indir_Emp<-output_ind_ES %>% filter(iteration == "Emp") %>% 
  group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of indirect effects on E(D)S retained in the empirical
  dplyr::select(management,services_to,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% 
  mutate (type = "Empirical")

# adjust the extreme values according to the beta conditions
Prop_indir_Emp$Prop_mean <- ifelse(Prop_indir_Emp$Prop_mean == 0, 0.000001, 
                                 ifelse(Prop_indir_Emp$Prop_mean == 1, 0.9999999, Prop_indir_Emp$Prop_mean))

#Simulations\
Prop_ind_null<-output_ind_ES %>% filter(!(iteration == "Emp" & management !="E")) %>% 
  group_by(management,iteration,services_to) %>% 
  mutate(tot = n(),
         tot_emp = case_when( #Values of denominator
           services_to == "Crop production"~ 280,
           services_to == "Pollination" ~ 18733,
           services_to == "Crop damage"~ 9816,
           services_to == "Pest control"~ 3261,
           services_to == "Seed dispersal" ~ 4154,
           services_to == "Butterfly watching"~ 3515,
           services_to == "Bird watching"~ 5722 )) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(Prop_mean = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services_to,Prop_mean) %>%
  unique() %>% mutate(type = "Null")

# adjust the extreme values according to the beta conditions
Prop_ind_null$Prop_mean <- ifelse(Prop_ind_null$Prop_mean == 0, 0.000001, 
                                  ifelse(Prop_ind_null$Prop_mean == 1, 0.9999999, Prop_ind_null$Prop_mean))
                                  
#write.csv(Prop_ind_null, "Data/Prop_ind_null_PP.csv",  row.names = FALSE)



###  - Models

# empirical
Prop_ind_emp <- glmmTMB (Prop_mean ~ management + services_to, family=beta_family(link="logit"), 
                         data = Prop_indir_Emp)# the best mode #model used in the empirical

par_emp<-Anova(Prop_ind_emp)
post_man_emp <-emmeans(Prop_ind_emp, ~ management) #posthoc management
man_pairs_emp <- as.data.frame(pairs(post_man_emp))



#Null
#we do the same model for each iteration to create the null distribution of the model coefficient
Prop_ind_null<- read.csv("Data/Prop_ind_null_PP.csv", sep =",")

iterations_coefficients<-NULL
iterations_post_hocs<-NULL

iterations_rem<- unique(Prop_ind_null$iteration)
iterations<- iterations_rem[2:length(iterations_rem)]#separate between interations and Empirical


for (i in iterations){ #for each iteration
  print(i)
  iteration_coeff<-Prop_ind_null %>% filter(iteration == i | iteration == "Emp")
  glmm_val_iterations <- glmmTMB (Prop_mean ~ management + services_to,family=beta_family(link="logit"), data = iteration_coeff)
  par<-Anova(glmm_val_iterations)
  post_man <-emmeans(glmm_val_iterations, ~ management) #posthoc management
  man_pairs <- as.data.frame(pairs(post_man)) 
  
  iterations_coefficients <- rbind(iterations_coefficients, 
                                   tibble(iteration = i,
                                          chisqr_man = par$Chisq[1],
                                          chisqr_ser = par$Chisq[2],
                                          pval_man = par[1,3],
                                          pval_ser = par[2,3],
                                   ))
  
  iterations_post_hocs <- rbind(iterations_post_hocs, 
                                tibble(man_pairs[,c("contrast","estimate","p.value")],
                                       iteration = rep(i,10)))
}


iterations_coefficients
#write.csv(iterations_coefficients, "Data/iterations_coefficients_indirect_PP.csv", row.names = FALSE)

iterations_post_hocs
#write.csv(iterations_post_hocs, "Data/iterations_post_hocs_indirect_PP.csv", row.names = FALSE)


#General Test
greater <- sum(iterations_coefficients$chisqr_man > par_emp[1,1])
less <- sum(iterations_coefficients$chisqr_man  < par_emp[1,1])
p_management<- 2 * min(greater, less) / 500 #calculate manually t-test two tailed


#Merge simulated and empirical
iterations_sim_empirical<-rbind(iterations_coefficients,tibble(
  iteration = "Empirical",
  chisqr_man = par_emp$Chisq[1],
  chisqr_ser = par_emp$Chisq[2],
  pval_man = par_emp[1,3],
  pval_ser = par_emp[2,3],
)) %>% mutate(iteration = ifelse(iteration == "Empirical", "Empirical","Null")) %>% rename("Type "= "iteration")


#Plot general
iterations_sim_empirical<-iterations_coefficients %>% mutate(type = "Null")

iterations_sim_empirical %>% ggplot(aes(x = chisqr_man, fill= type))+ 
  geom_density(alpha = 0.6)+ 
  geom_vline(xintercept = par_emp[1,1], linetype = "dashed", color = "#FB3B1E") + #line represting rsquared empirical
  labs(x= "Statistic", y="Density")+  
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))
ggsave("Graphs/distri_indirect_PP.png")



# Plot combination of treatments level 
iterations_post_hocs<-read.csv("Data/iterations_post_hocs_indirect_PP.csv", sep =",")

#Post hoc (each combination of management )
p_contrast_man<-NULL

for (i in unique(iterations_post_hocs$contrast)){
  
  #filter data
  post_comb<-iterations_post_hocs %>% filter(contrast == i) #filter per combination of treatmens (simulated data)
  post_emp<-man_pairs_emp %>% filter(contrast == i) #filter per combination of treatmens (empirical)
  
  #test
  
  greater <- sum(post_comb$estimate >  post_emp$estimate)
  less <- sum(post_comb$estimate  <  post_emp$estimate)
  p_comb<- 2 * min(greater, less) / 500 #calculate manually t-test two tailed
  
  #storage
  p_contrast_man <- rbind(p_contrast_man,
                          tibble(contrast = i,
                                 p_value = p_comb))
  
}

p_contrast_man


#Prepare dataframe (simulated and empircal)

#smimulated
man_lev<-c("E","SE","M","SI","I")

post_hoc_sim<- iterations_post_hocs %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
  select(man_1,man_2,estimate,iteration)

post_hoc_sim_inv<- post_hoc_sim %>% select(man_2,man_1,estimate, iteration)
colnames(post_hoc_sim_inv) <- c("man_1", "man_2","estimate", "iteration")

post_hoc_sim_fin<- rbind(post_hoc_sim,post_hoc_sim_inv)

post_hoc_sim_fin$man_1 <- factor(post_hoc_sim_fin$man_1, levels = man_lev)
post_hoc_sim_fin$man_2 <- factor(post_hoc_sim_fin$man_2, levels = man_lev)


#empirical
post_hoc_emp_pre<- man_pairs_emp %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
  select(-p.value) %>% mutate (iteration = "0") %>% select(man_1,man_2,estimate,iteration)#empirical

new_combinations_empirical <- data.frame(
  man_1 = c("E", "SE", "M", "SI", "I"),
  man_2 = c("E", "SE", "M", "SI", "I"),
  estimate = rep(0, 5),
  iteration = rep(0, 5)
)


post_hoc_emp_fin<-rbind(post_hoc_emp_pre,new_combinations_empirical)
post_hoc_emp_fin$man_1 <- factor(post_hoc_emp_fin$man_1, levels = man_lev)
post_hoc_emp_fin$man_2 <- factor(post_hoc_emp_fin$man_2, levels = man_lev)


# plot
# Filter data for lower triangle including diagonal, assuming man_1 and man_2 are now ordered factors
lower_triangle_data <- post_hoc_sim_fin[as.numeric(post_hoc_sim_fin$man_2) >= as.numeric(post_hoc_sim_fin$man_1), ]

posthoc_distr_PP<- ggplot(data = lower_triangle_data, aes(x=estimate)) +
  geom_histogram(fill = "steelblue") +
  labs(y = "Count", x = "Estimate value") +
  geom_vline(data = post_hoc_emp_fin[1:10,], mapping = aes(xintercept = estimate), 
             colour="#BB0000", linetype="dashed") +
  facet_grid(man_2 ~ man_1)

#ggsave("Graphs/posthoc_distri_indirect_PP.png")





#### -- Change in the amount of direct E(D)S provided change across land use change --

## - upload and prepare dataframe
direct_ES_emp<- read.csv("Data/Land_use_dir_weighted_PP_intense.csv", sep =",")

extensive_amount <- direct_ES_emp %>% 
  filter(management == "E") %>%
  select(node_id, services, weight)

dir_amount_emp <- direct_ES_emp %>% 
  left_join(extensive_amount, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = weight / weight_extensive) #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S


ratio_empirical<-dir_amount_emp %>%
  select(management, services,node_id, ratio_change) %>% group_by(management,services,node_id) %>% 
  summarise(ratio_change_ave = mean(ratio_change)) %>% 
  mutate(type = "Empirical")

# Calculate the ratio change in the amount of ES per species in each simulation
direct_ES_sim<- read.csv("Data/direct_ES_sim_PP.csv", sep =",")

dir_amount_sim <- direct_ES_sim %>% filter(!(iteration == "Emp" & management !="E"))%>% 
  left_join(extensive_amount, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = weight / weight_extensive) #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S

ratio_sim<-dir_amount_sim  %>% 
  select(management,iteration, services,node_id, ratio_change) %>% group_by(management,iteration,services,node_id) %>% 
  summarise(ratio_change_ave = mean(ratio_change)) 



###  - Models

# empirical
amount_emp <- glmmTMB(ratio_change_ave ~ management + (1|services) + (1|node_id),family = Gamma(link = "log"),
                      data = ratio_empirical) #best model, services not significant, interaciton not enought data
par_emp<-Anova(amount_emp)
post_man_emp <-emmeans(amount_emp,  ~ management) #posthoc management
man_pairs_emp <- as.data.frame(pairs(post_man_emp))

#Null
#we do the same model for each iteration to create the null distribution of the model coefficient

iterations_coefficients<-NULL
iterations_post_hocs<-NULL


for (i in 1:500){ #for each iteration
  print(i)
  iteration_coeff<-ratio_sim %>% filter(iteration == i | iteration == "Emp")
  glmm_val_iterations <- glmmTMB(ratio_change_ave ~ management + (1|services) + (1|node_id),family = Gamma(link = "log"),
                                 data = iteration_coeff)
  par<-Anova(glmm_val_iterations)
  post_man <-emmeans(glmm_val_iterations, ~ management) #posthoc management
  man_pairs <- as.data.frame(pairs(post_man)) 
  
  iterations_coefficients <- rbind(iterations_coefficients, 
                                   tibble(iteration = i,
                                          chisqr_man = par$Chisq[1],
                                          pval_man = par[1,3]
                                   ))
  
  iterations_post_hocs <- rbind(iterations_post_hocs, 
                                tibble(man_pairs[,c("contrast","estimate","p.value")],
                                       iteration = rep(i,10)))
}


iterations_coefficients
#write.csv(iterations_coefficients, "Data/iterations_coefficients_amount_PP.csv", row.names = FALSE)

iterations_post_hocs
#write.csv(iterations_post_hocs, "Data/iterations_post_hocs_amount_PP.csv", row.names = FALSE)


#General

iterations_coefficients<-read.csv("Data/iterations_coefficients_amount_PP.csv", sep =",")
greater <- sum(iterations_coefficients$chisqr_man > par_emp[1,1])
less <- sum(iterations_coefficients$chisqr_man  < par_emp[1,1])
p_management<- 2 * min(greater, less) / 500 #calculate manually t-test two tailed

#Merge simulated and empirical
iterations_sim_empirical<-rbind(iterations_coefficients,tibble(
  iteration = "Empirical",
  chisqr_man = par_emp[1,1],
  pval_man = par_emp[1,3]
)) %>% mutate(iteration = ifelse(iteration == "Empirical", "Empirical","Null")) %>% rename("type"= "iteration")


#Plot general

iterations_sim_empirical %>% ggplot(aes(x = chisqr_man, fill= type))+ 
  geom_density(alpha = 0.6)+ 
  geom_vline(xintercept = par_emp[1,1], linetype = "dashed", color = "#FB3B1E") + #line represting rsquared empirical
  labs(x= "Statistic", y="Density")+  
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))


#ggsave("distri_amount_PP.png")


#Plot combination of treatments level 

#Post hoc (each combination of management )

iterations_post_hocs<-read.csv("Data/iterations_post_hocs_amount_PP.csv", sep =",")

p_contrast_man<-NULL

for (i in unique(iterations_post_hocs$contrast)){
  
  #filter data
  post_comb<-iterations_post_hocs %>% filter(contrast == i) #filter per combination of treatmens (simulated data)
  post_emp<-man_pairs_emp %>% filter(contrast == i) #filter per combination of treatmens (empirical)
  
  #test
  greater <- sum(post_comb$estimate  > post_emp$estimate)
  less <- sum(post_comb$estimate < post_emp$estimate)
  p_comb <- 2 * min(greater, less) / 500
  
  #storage
  p_contrast_man <- rbind(p_contrast_man,
                          tibble(contrast = i,
                                 p_value = p_comb))
  
}
p_contrast_man


#Prepare dataframe (simulated and empircal)

#smimulated
man_lev<-c("E","SE","M","SI","I")

post_hoc_sim<- iterations_post_hocs %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
  select(-p.value)

post_hoc_sim_inv<- post_hoc_sim %>% select(man_2,man_1,estimate, iteration)
colnames(post_hoc_sim_inv) <- c("man_1", "man_2","estimate", "iteration")

post_hoc_sim_fin<- rbind(post_hoc_sim,post_hoc_sim_inv)

post_hoc_sim_fin$man_1 <- factor(post_hoc_sim_fin$man_1, levels = man_lev)
post_hoc_sim_fin$man_2 <- factor(post_hoc_sim_fin$man_2, levels = man_lev)


#empirical
post_hoc_emp_pre<- man_pairs_emp %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
  select(-p.value) %>% mutate (iteration = "0") %>% select(man_1,man_2,estimate,iteration)#empirical

new_combinations_empirical <- data.frame(
  man_1 = c("E", "SE", "M", "SI", "I"),
  man_2 = c("E", "SE", "M", "SI", "I"),
  estimate = rep(0, 5),
  iteration = rep(0, 5)
)

post_hoc_emp_fin<-rbind(post_hoc_emp_pre,new_combinations_empirical)
post_hoc_emp_fin_inv<- post_hoc_emp_fin %>% select(man_2,man_1,estimate, iteration)
colnames(post_hoc_emp_fin_inv) <- c("man_1", "man_2","estimate", "iteration")

post_hoc_emp_fin_2<- rbind(post_hoc_emp_fin,post_hoc_emp_fin_inv)
post_hoc_emp_fin_2$man_1 <- factor(post_hoc_emp_fin_2$man_1, levels = man_lev)
post_hoc_emp_fin_2$man_2 <- factor(post_hoc_emp_fin_2$man_2, levels = man_lev)


# plot

lower_triangle_sim <- post_hoc_sim_fin[as.numeric(post_hoc_sim_fin$man_2) >= as.numeric(post_hoc_sim_fin$man_1), ]
lower_triangle_emp <- post_hoc_emp_fin_2[as.numeric(post_hoc_emp_fin_2$man_2) >= as.numeric(post_hoc_emp_fin_2$man_1), ]

posthoc_distr_PP<- ggplot(data = lower_triangle_sim, aes(x=estimate)) +
  geom_histogram(fill = "steelblue") +
  labs(y = "Count", x = "Estimate value") +
  geom_vline(data = lower_triangle_emp[c(1:6,12:15),], mapping = aes(xintercept = estimate), 
             colour="#BB0000", linetype="dashed") +
  facet_grid(man_2 ~ man_1)

#ggsave("Graphs/posthoc_amount_direct_PP.png")










#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Plots                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#upload and prepare dataframe
direct_ES<- read.csv("Data/direct_ES_sim_PP.csv", sep =",")

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

#indirect
output_ind_ES<- read.csv("Data/Indirect_ES_sim_CP.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors




### Plot of proportions of richness direct and indirect E(D)S retained 

#Empirical
Prop_dir_emp<-direct_ES %>% filter(iteration == "Emp") %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  dplyr::select(management,services,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% mutate(type = "Emp")

# Value of empirical
values_emp<- Prop_dir_emp %>% filter (management == "E") %>% select(management,tot)

#Simulations
Prop_dir_sim<-direct_ES %>% filter(iteration != "Emp") %>% group_by(management,iteration,services) %>% 
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
  mutate(prop = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services,tot,prop) %>%
  unique() %>% group_by(management,services) %>% summarise(Prop_mean = mean(prop)) %>% 
  mutate(type = "Null")


#join 
Prop_direct_sim<-rbind(Prop_dir_emp,Prop_dir_sim)

#Plot 
prop_EDS_direct <- Prop_direct_sim%>% ggplot(aes(x = management, y = Prop_mean)) +
  geom_boxplot(aes(colour = type), 
               outlier.shape = NA) +
  geom_point(aes(fill = factor(services), color = type), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.4),
             pch = 21, size = 2.5, stroke = 1, show.legend = TRUE) +
  scale_fill_brewer(palette = "PRGn")+
  scale_color_manual(values = c("black","#d7301f"))+
  scale_y_continuous(name = "Fraction of E(D)S retained", limits = c(0, 1)) + 
  scale_x_discrete(name = "Management")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major=element_line(color = "gray"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=13, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =14), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

prop_EDS_direct


#ggsave("Direct_ES_sim_PP.png")


### Plot of proportions of indirect effects on E(D)S retained  

#Empirical
Prop_indir_Emp<-output_ind_ES %>% filter(iteration == "Emp") %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of indirect effects on E(D)S retained in the empirical
  dplyr::select(management,services_to,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% 
  mutate (type = "Empirical")



#Simulations
Prop_ind_null<- read.csv("Data/Prop_ind_null_PP.csv", sep =",")
Prop_indir_sim<- Prop_ind_null %>% group_by(management,services_to) %>%
  summarise(Prop_mean = mean(Prop_mean)) %>% 
  filter(management !="E") %>% mutate(type = "Null")


# Merge empirical and simulations and remove extensive scenario
Prop_indir_sim2 <- rbind(Prop_indir_Emp, Prop_indir_sim)
Prop_indir_sim2$management <- factor(Prop_indir_sim2$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


#Plot 
prop_EDS_indirect <- Prop_indir_sim2%>% ggplot(aes(x = management, y = Prop_mean)) +
  geom_boxplot(aes(colour = type), 
               outlier.shape = NA) +
  geom_point(aes(fill = factor(services_to), color = type), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.4),
             pch = 21, size = 2.5, stroke = 1, show.legend = TRUE) +
  scale_fill_brewer(palette = "PRGn")+
  scale_color_manual(values = c("black","#d7301f"))+
  scale_y_continuous(name = "Fraction of E(D)S retained", limits = c(0, 1)) + 
  scale_x_discrete(name = "Management")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major=element_line(color = "gray"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=13, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =14), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

prop_EDS_indirect


#ggsave("Graphs/Indirect_ES_sim_PP.png")

### Prop Amount of direct E(D)S

# Calculate ratio of change for empirical

direct_ES_emp<- read.csv("Data/Land_use_dir_weighted_PP_intense.csv", sep =",") #upload empirical

extensive_amount <- direct_ES_emp %>% 
  filter(management == "E") %>%
  group_by(management,services) %>% 
  summarize(tot_exten = sum(weight))

ratio_empirical <- direct_ES_emp  %>% group_by(management,services) %>% 
  summarize(tot = sum(weight)) %>% 
  left_join(extensive_amount[,2:3], by = "services", suffix = c("", "_extensive")) %>%
  mutate(ratio_change_ave = tot / tot_exten) %>%  #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
  mutate(type = "Empirical")


# Calculate ratio of change for simulations
direct_ES_sim<- read.csv("Data/direct_ES_sim_PP.csv", sep =",")

ratio_sim<- direct_ES_sim %>% filter (management !="E") %>% 
  group_by(management,iteration,services) %>% 
  summarize(tot = sum(weight)) %>% 
  left_join(extensive_amount[,2:3], by = "services", suffix = c("", "_extensive")) %>%
  mutate(ratio_change = tot / tot_exten) %>% #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
  group_by(management,services) %>% 
  summarise(ratio_change_ave = mean(ratio_change)) %>% 
  mutate(type = "Null")

#Join data
ratio_sim2<-rbind(ratio_empirical,ratio_sim)
ratio_sim2$management <- factor(ratio_sim2$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors



#Plot 
prop_EDS_ratio <- ratio_sim2%>% ggplot(aes(x = management, y = ratio_change_ave)) +
  geom_boxplot(aes(colour = type), 
               outlier.shape = NA) +
  geom_point(aes(fill = factor(services), color = type), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.4),
             pch = 21, size = 2.5, stroke = 1, show.legend = TRUE) +
  scale_fill_brewer(palette = "PRGn")+
  scale_color_manual(values = c("black","#d7301f"))+
  scale_y_continuous(name = "Change in the amount of direct E(D)S", limits = c(0, 2.8)) + 
  scale_x_discrete(name = "Management")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major=element_line(color = "gray"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=13, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =14), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

prop_EDS_ratio

#ggsave("ratio_change_sim_PP.png")















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Analyses   DESCARTAR POR AHORA!                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### -- Proportion of direct E(D)S retained across land use change --

#upload and prepare dataframe
direct_ES<- read.csv("Data/direct_ES_sim_PP.csv", sep =",")

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


#Empirical
Prop_dir_Emp<-direct_ES %>% filter(iteration == "Emp") %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  dplyr::select(management,services,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% 
  mutate (type = "Empirical")

# Value of empirical
values_emp<- Prop_dir_Emp %>% filter (management == "E" ) %>% select(management,tot)

#Simulations
Prop_dir_sim<-direct_ES %>% filter(iteration != "Emp") %>% group_by(management,iteration,services) %>% 
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
  mutate(prop = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services,tot,prop) %>%
  unique() %>% group_by(management,services) %>% summarise(Prop_mean = mean(prop)) %>% 
  mutate(type = "Null")


# Merge empirical and simulations and remove extensive scenario
Prop_dir_sim2 <- rbind(Prop_dir_Emp, Prop_dir_sim) %>% filter(management != "E")


# adjust the extreme values according to the beta conditions and remove extensive scenario
Prop_dir_sim2$Prop_mean <- ifelse(Prop_dir_sim2$Prop_mean == 0, 0.000001, 
                                  ifelse(Prop_dir_sim2$Prop_mean == 1, 0.9999999, Prop_dir_sim2$Prop_mean))

# Model
library("glmmTMB")
library("stats4")
library("bbmle")
library(emmeans)
library(car)


#Model
#shapiro.test(Prop_dir_sim2$Prop_mean)#check for normality #it's not normal so we use beta

Prop_dire<-glmmTMB (Prop_mean ~ type + management + ( 1| services), family=beta_family(link="logit"), data = Prop_dir_sim2) # model that best fit
#Prop_dire2<-glmmTMB (Prop_mean ~ type + management, family=beta_family(link="logit"), data = Prop_dir_sim2)
#Prop_dire3<- glmmTMB (Prop_mean ~ type + (1| management/services),family=beta_family(link="logit"),data = Prop_dir_sim2)
#Prop_dire4<-glmmTMB (Prop_mean ~ type:management, family=beta_family(link="logit"), data = Prop_dir_sim2)
Anova(Prop_dire)
summary(Prop_dire)

#AIC(Prop_dire, Prop_dire2, Prop_dire3, Prop_dire4)


#Homogeneity
EM<-resid(Prop_dire, type= "response") 
FM<-fitted(Prop_dire) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(Prop_dire, type= "response") 
boxplot(E1_lme ~ type, data = Prop_dir_sim2, main = "Management")

# posthoc ()
post_dir<- emmeans(Prop_dire, ~ management)
pairs(post_dir)

post_ser<- emmeans(Prop_dire, ~ type)
pairs(post_ser)



#### Proportion of indirect effect on E(D)S retained across land use change --


## CREO QUE ES ESTE SCRIPT

#Simulations
Prop_ind_null<-output_ind_ES %>% filter(!(iteration == "Emp" & management !="E")) %>% 
  group_by(management,iteration,services_to) %>% 
  mutate(tot = n(),
         tot_emp = case_when( #Values of denominator
           services_to == "Crop production"~ 280,
           services_to == "Pollination" ~ 18733,
           services_to == "Crop damage"~ 9816,
           services_to == "Pest control"~ 3261,
           services_to == "Seed dispersal" ~ 4154,
           services_to == "Butterfly watching"~ 3515,
           services_to == "Bird watching"~ 5722 )) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services_to,tot,prop) %>%
  unique() %>% group_by(management,services_to) %>% summarise(Prop_mean = mean(prop)) %>% 
  mutate(type = "Null")

##

#upload and prepare dataframe
output_ind_ES<- read.csv("Data/Indirect_ES_sim_PP.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


#Empirical
Prop_indir_Emp<-output_ind_ES %>% filter(iteration == "Emp") %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of indirect effects on E(D)S retained in the empirical
  dplyr::select(management,services_to,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% 
  mutate (type = "Empirical")


# Value of empirical
values_emp<- Prop_indir_Emp %>% filter (management == "E" ) %>% select(management,tot)


#Simulations
Prop_indir_sim<-output_ind_ES %>% filter(iteration != "Emp" & iteration <=100) %>%  #iteration >100 to filter the indiretc 1 hop effect to the number of iteration in the indirect 2 hops
  group_by(management,iteration,services_to) %>% 
  mutate(tot = n(),
         tot_emp = case_when( #Values of denominator
           services_to == "Crop production"~ 280,
           services_to == "Pollination" ~ 18733,
           services_to == "Crop damage"~ 9816,
           services_to == "Pest control"~ 3261,
           services_to == "Seed dispersal" ~ 4154,
           services_to == "Butterfly watching"~ 3515,
           services_to == "Bird watching"~ 5722 )) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services_to,tot,prop) %>%
  unique() %>% group_by(management,services_to) %>% summarise(Prop_mean = mean(prop)) %>% 
  mutate(type = "Null")


# Merge empirical and simulations and remove extensive scenario
Prop_indir_sim2 <- rbind(Prop_indir_Emp, Prop_indir_sim) %>% filter(management != "E")


# Model
library(lme4)

shapiro.test(Prop_indir_sim2$Prop_mean)#check for normality

Prop_indire_nor1<-lmer (Prop_mean ~ type + management + ( 1| services_to), data = Prop_indir_sim2) #best model
Prop_indire_nor2<-lm (Prop_mean ~ type + management, data = Prop_indir_sim2) #best model
Prop_indire_nor3<-lmer (Prop_mean ~ type + (1| management/services_to) + ( 1| services_to), data = Prop_indir_sim2) #best model
Prop_indire_nor4<-lm (Prop_mean ~ type:management, data = Prop_indir_sim2) #best model
Prop_indire_nor5<-lmer (Prop_mean ~ type:management + ( 1| services_to), data = Prop_indir_sim2) #best model

Anova(Prop_indire_nor4)
summary(Prop_indire_nor4)

AIC(Prop_indire_nor1, Prop_indire_nor2, Prop_indire_nor3, Prop_indire_nor4,Prop_indire_nor5)

#Homogeneity
EM<-resid(Prop_indire_nor4, type= "response") 
FM<-fitted(Prop_indire_nor4) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(Prop_indire_nor4, type= "response") 
boxplot(E1_lme ~ type, data = Prop_indir_sim2, main = "Management")

# posthoc interaction
emm_interaction<- emmeans(Prop_indire_nor4, ~ type:management)
posthoc_interaction <- pairs(emm_interaction)
pairs(posthoc_interaction)

post_type<- emmeans(Prop_indire_nor4, ~ type)
pairs(post_type)


## beta distributon (BORRAR SI DECIDO USAR NORMAL)
Prop_indire<-glmmTMB (Prop_mean ~ type + management + ( 1| services_to), family=beta_family(link="logit"), data = Prop_indir_sim2) #best model
Prop_indire2<-glmmTMB (Prop_mean ~ type + management, family=beta_family(link="logit"), data = Prop_indir_sim2)
Prop_indire3<- glmmTMB (Prop_mean ~ type + (1| management/services_to),family=beta_family(link="logit"),data = Prop_indir_sim2)
Prop_indire4<-glmmTMB (Prop_mean ~ type:management , family=beta_family(link="logit"), data = Prop_indir_sim2) #not working
Prop_indire5<-glmmTMB (Prop_mean ~ type:management + ( 1| services_to), family=beta_family(link="logit"), data = Prop_indir_sim2) #not working
Anova(Prop_indire4)
summary(Prop_indire4)

AIC(Prop_indire, Prop_indire2, Prop_indire3, Prop_indire4)


#Homogeneity
EM<-resid(Prop_indire, type= "response") 
FM<-fitted(Prop_indire) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(Prop_indire, type= "response") 
boxplot(E1_lme ~ type, data = Prop_indir_sim2, main = "Management")

# posthoc ()
post_indir<- emmeans(Prop_indire, ~ management)
pairs(post_indir)

post_type<- emmeans(Prop_indire, ~ type)
pairs(post_type)





#### Change in the amount of direct E(D)S provided change across land use change --

#Amount of ES provided by each species in the empirical (Weight = abundance * biomass)
direct_ES_emp<- read.csv("Data/Land_use_dir_weighted_PP_intense.csv", sep =",")

extensive_amount <- direct_ES_emp %>% 
  filter(management == "E") %>%
  select(node_id, services, weight)

dir_amount_emp <- direct_ES_emp %>% filter(management != "E") %>% 
  left_join(extensive_amount, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = weight / weight_extensive) #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S


ratio_empirical<-dir_amount_emp %>% filter(management != "E") %>% #subset empirical (except "Extensive" management)
  select(management, services,node_id, ratio_change) %>% group_by(management,services,node_id) %>% 
  summarise(ratio_change_ave = mean(ratio_change)) %>% 
  mutate(type = "Empirical")

# Calculate the ratio change in the amount of ES per species in each simulation
direct_ES_sim<- read.csv("Data/direct_ES_sim_PP.csv", sep =",")

dir_amount_sim <- direct_ES_sim %>% filter(iteration != "Emp" ) %>% 
  left_join(extensive_amount, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = weight / weight_extensive) #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S

ratio_sim<-dir_amount_sim  %>% 
  select(management,iteration, services,node_id, ratio_change) %>% group_by(management,services,node_id) %>% 
  summarise(ratio_change_ave = mean(ratio_change)) %>% 
  mutate(type = "Null")

#Join data
ratio_sim2<-rbind(ratio_empirical,ratio_sim)

# Model
#shapiro.test(ratio_sim2$ratio_change_ave) #not normal

m_amount1<- glmmTMB(ratio_change_ave ~ management + type + (1|services) + (1|node_id),family = Gamma(link = "log"), 
                    data = ratio_sim2) # best model
#m_amount2<- glmmTMB(ratio_change_ave ~ management:type + (1|services),family = Gamma(link = "log"),
#                  data = ratio_sim2) #the interaction is not ok according to AIC
Anova(m_amount1)
summary(m_amount1)

#AIC(m_amount1,m_amount2)

#Homogeneity
EM<-resid(m_amount1, type= "response") 
FM<-fitted(m_amount1) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(m_amount1, type= "response") 
boxplot(E1_lme ~ type, data = ratio_sim2, main = "Management")

# posthoc ()
post_man<- emmeans(m_amount1, ~ management)
pairs(post_man)

