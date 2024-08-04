################### NULL MODEL (First version) ##################################

#We want to test if the results are related to the identity of species we remove when converting habitats.
#We can test if the identity of species that are lost in each habitat after conversion affects the results or not. 
#This null model maintains the number of species in the new habitat as in the original simulation.


library(emln)#multilayer package
library(readr)
library(ggplot2)
library(cowplot)
library(tidyverse)

#setwd("/Users/agustinvitali/Desktop/Work/Papers/Norwood_Farm/GitHub/Norwood_farm")
setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")
#source("/Users/agustinvitali/Desktop/Work/Papers/Norwood_Farm/GitHub/Norwood_farm/Exploratory/Scenario_ES/functions.R")
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
  mutate(mult_ab = area_ave/46.4) # the factor we should modify the abundance from CP species according to the new habitat





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      SIMULATION                        
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(123)


############# 1. Species to remove

#Here we identify the number of species to randomly remove in each habitat when convert from extensive to intensive scenarios.
#The number of species to remove should be the same as in the original simulation.

# Find species present in CP
species_in_cp <- unique(Norwood_farm$state_nodes[Norwood_farm$state_nodes$layer_name == 'CP', 'node_id'])

# Find species in each habitat and compare with CP
unique_habitats <- unique(Norwood_farm$state_nodes$layer_name)
absent_species_count <- data.frame(habitat = character(), absent_count = integer())

# Identify the number of species to remove 
for (hab in unique_habitats) {
  if (hab != 'CP') {
    species_in_habitat <- unique(Norwood_farm$state_nodes[Norwood_farm$state_nodes$layer_name == hab, 'node_id'])
    absent_count <- sum(!species_in_habitat$node_id %in% species_in_cp$node_id)
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

## add CP and then modify its abundances according to new area, remove species and add abundnaces

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


## -- Remove species at random (600 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#WD
WD_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="WD")
shuff_WD<-sim_sp_removal(WD_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
WD_clean<-shuff_WD[[1]] %>% ungroup() %>% mutate(habitat = 11) 

#RG
RG_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="RG")
shuff_RG<-sim_sp_removal(RG_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
RG_clean<-shuff_RG[[1]] %>% ungroup() %>% mutate(habitat = 12) 

#Merge shuff habitats and arrange dataframe to merge with the rest of the habitats
shuff_habitats<-rbind(WD_clean,RG_clean) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                  node_to,ab_node_to,taxon_node_to,weight, iteration)
#write.csv(shuff_habitats,"Data/shuff_hab_WD_RG_CP.csv", row.names= FALSE) #save to add in the next management scenario

# Store species removed in every iteration
sps_removed_WD <- shuff_WD[[2]] %>% mutate(habitat_from = "WD", management = "SE")
sps_removed_RG <- shuff_RG[[2]] %>% mutate(habitat_from = "RG", management = "SE")
sps_removed<-rbind(sps_removed_WD,sps_removed_RG)

#write.csv(sps_removed,"Data/sps_removed_SE_CP.csv", row.names= FALSE) #save to add in the next management scenario

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (SE)
sem_ext_sim_no_aggr<- comb_edge_list(sem_ext_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_sem_ext_sim<-lapply(sem_ext_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_sem_ext_sim<-bind_rows(state_node_sem_ext_sim)

#write.csv(state_node_sem_ext_sim,"Data/SE_sim_state_node_CP.csv", row.names= FALSE)

## -- aggregate habitat within simulated management scenario 
SE_sim<-lapply(sem_ext_sim_no_aggr,function(data) {
  data %>%  mutate(management = "SE") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

SE_sim<-bind_rows(SE_sim)
#write.csv(SE_sim,"Data/SE_sim_CP.csv", row.names= FALSE)



#####  -- Moderate (replace "WD","RG","MH"and "NH" for "CP")


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
  

## -- Remove species at random (600 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#MH
MH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="MH")
shuff_MH<-sim_sp_removal(MH_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
MH_clean<-shuff_MH[[1]] %>% ungroup() %>% mutate(habitat = 13) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#NH
NH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NH")
shuff_NH<-sim_sp_removal(NH_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
NH_clean<-shuff_NH[[1]] %>% ungroup() %>% mutate(habitat = 14) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (M) with the previous mangaement (SE)
shuff_pre<-read.csv("Data/shuff_hab_WD_RG_CP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,MH_clean,NH_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_M_CP.csv", row.names= FALSE) #save to add in the next management scenario

# Store species removed in every iteration
sps_removed_MH <- shuff_MH[[2]] %>% mutate(habitat_from = "MH", management = "M")
sps_removed_NH <- shuff_NH[[2]] %>% mutate(habitat_from = "NH", management = "M")
sps_removed<-rbind(sps_removed_MH,sps_removed_NH)

#write.csv(sps_removed,"Data/sps_removed_M_CP.csv", row.names= FALSE) #save to add in the next management scenario

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (M)
mod_sim_no_aggr<- comb_edge_list(mod_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_mod_sim<-lapply(mod_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_mod_sim<-bind_rows(state_node_mod_sim)
#write.csv(state_node_mod_sim,"Data/M_sim_state_node_CP.csv", row.names= FALSE)

## -- aggregate habitat within simulated management scenario 
M_sim<-lapply(mod_sim_no_aggr,function(data) {
  data %>%  mutate(management = "M") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

M_sim<-bind_rows(M_sim)
#write.csv(M_sim,"Data/M_sim_CP.csv", row.names= FALSE)




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

sem_int_edgelist_rem<-sem_int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


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
shuff_GM<-sim_sp_removal(GM_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
GM_clean<-shuff_GM[[1]] %>% ungroup() %>% mutate(habitat = 15) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#SF
SF_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="SF")
shuff_SF<-sim_sp_removal(SF_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
SF_clean<-shuff_SF[[1]] %>% ungroup() %>% mutate(habitat = 16) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#PP
PP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="PP")
shuff_PP<-sim_sp_removal(PP_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
PP_clean<-shuff_PP[[1]] %>% ungroup() %>% mutate(habitat = 17) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (SI) with the previous mangaement (M)
shuff_pre<-read.csv("Data/shuff_hab_M_CP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,GM_clean,SF_clean,PP_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_SI_CP.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github

# Store species removed in every iteration
sps_removed_GM <- shuff_GM[[2]] %>% mutate(habitat_from = "GM", management = "SI")
sps_removed_SF <- shuff_SF[[2]] %>% mutate(habitat_from = "SF", management = "SI")
sps_removed_PP <- shuff_PP[[2]] %>% mutate(habitat_from = "PP", management = "SI")
sps_removed<-rbind(sps_removed_GM,sps_removed_SF,sps_removed_PP)

#write.csv(sps_removed,"Data/sps_removed_SI_CP.csv", row.names= FALSE) #save to add in the next management scenario

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (SI)
sem_int_sim_no_aggr<- comb_edge_list(sem_int_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet


## -- create state_node_list of each simulated management scenario
state_node_SI_sim<-lapply(sem_int_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_SI_sim<-bind_rows(state_node_SI_sim)
#write.csv(state_node_SI_sim,"Data/SI_sim_state_node_CP.csv", row.names= FALSE)

## -- aggregate habitat within simulated management scenario 
SI_sim<-lapply(sem_int_sim_no_aggr,function(data) {
  data %>%  mutate(management = "SI") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

SI_sim<-bind_rows(SI_sim)
#write.csv(SI_sim,"Data/SI_sim_CP.csv", row.names= FALSE)





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

int_edgelist_rem<-int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]

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


## -- Remove species at random (600 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#LP
LP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="LP")
shuff_LP<-sim_sp_removal(LP_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
LP_clean<-shuff_LP[[1]] %>% ungroup() %>% mutate(habitat = 18) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#NL
NL_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NL")
shuff_NL<-sim_sp_removal(NL_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
NL_clean<-shuff_NL[[1]] %>% ungroup() %>% mutate(habitat = 19) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (I) with the precvious mangaement (SI)
shuff_pre<-read.csv("Data/shuff_hab_SI_CP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,LP_clean,NL_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_I_CP.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github


# Store species removed in every iteration
sps_removed_LP <- shuff_LP[[2]] %>% mutate(habitat_from = "LP", management = "I")
sps_removed_NL <- shuff_NL[[2]] %>% mutate(habitat_from = "NL", management = "I")
sps_removed<-rbind(sps_removed_LP,sps_removed_NL)

#write.csv(sps_removed,"Data/sps_removed_I_CP.csv", row.names= FALSE) #save to add in the next management scenario

## -- Merge each simulation of transformed habitats with the non-transformed habitats to create 1000 simulation of the management scenario (I)
int_sim_no_aggr<- comb_edge_list(int_edgelist_rem,shuff_habitats) #call function to merge dataframes. In each iteration, habitats are not aggregated yet

## -- create state_node_list of each simulated management scenario
state_node_I_sim<-lapply(int_sim_no_aggr,state_node_list) #call the function to create node list and apply to every element of the list
state_node_I_sim<-bind_rows(state_node_I_sim)
#write.csv(state_node_I_sim,"Data/I_sim_state_node_CP.csv", row.names= FALSE)


## -- aggregate habitat within simulated management scenario 
I_sim<-lapply(int_sim_no_aggr,function(data) {
  data %>%  mutate(management = "I") %>% 
    select(management,iteration,node_from,node_to) %>% 
    unique()
})

I_sim<-bind_rows(I_sim)
#write.csv(I_sim,"Data/I_sim_CP.csv", row.names= FALSE)



##### -- Intensive non-organic
#We remove the weeds and species that only interact with them for all the intensive simulated networks

### Create edge list
I_sim_CP<-read.csv("Data/I_sim_CP.csv", sep =,) %>% 
  mutate(management = "IM")

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


for (i in 1:600){
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
#write.csv(IM_sim,"Data/IM_sim_CP.csv", row.names= FALSE) 

sps_removed = list_species_removed #list of species removed
#write.csv(sps_removed,"Data/sps_removed_IM_CP.csv", row.names= FALSE) #save to add in the next management scenario

sps_survived = list_species_survived #remaining species


### Create state node
I_sim_state_node<-read.csv("Data/I_sim_state_node_CP.csv", sep =,) 

# Filter according to the node that remains in each simulation
state_node_IM_sim <- sps_survived %>%  
                  left_join(I_sim_state_node, by= c("iteration","remain_species" = "node_id"))

#write.csv(state_node_IM_sim,"Data/IM_sim_state_node_CP.csv", row.names= FALSE)




##### -- Final Dataframe

## - Edge list

#upload simulated habitat management and empirical
SE_sim<-read.csv("Data/SE_sim_CP.csv", sep =,) 
SE_sim$iteration<-as.character(SE_sim$iteration)

M_sim<-read.csv("Data/M_sim_CP.csv", sep =,) 
M_sim$iteration<-as.character(M_sim$iteration)

SI_sim<-read.csv("Data/SI_sim_CP.csv", sep =,) 
SI_sim$iteration<-as.character(SI_sim$iteration)

I_sim<-read.csv("Data/I_sim_CP.csv", sep =,) 
I_sim$iteration<-as.character(I_sim$iteration)

IM_sim<-read.csv("Data/IM_sim_CP.csv", sep =,) 
IM_sim$iteration<-as.character(IM_sim$iteration)

#Upload empirical
Emp<-read.csv("Data/Land_use_rat_edgelist_weighted_CP_intense.csv", sep =,) %>% 
    mutate(iteration = "Emp") %>%  select(management,iteration,node_from,node_to)

## Final Edgelist
edge_list_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim,IM_sim)
#write.csv(edge_list_sim,"Data/edge_list_sim_CP.csv", row.names= FALSE)


## - State nodes

#upload simulated habitat management and empirical

SE_sim<-read.csv("Data/SE_sim_state_node_CP.csv", sep =,) %>% 
        mutate(management = "SE") %>%  select(management, iteration,node_id,taxon,abun)
SE_sim$iteration<-as.character(SE_sim$iteration) 

M_sim<-read.csv("Data/M_sim_state_node_CP.csv", sep =,) %>% 
  mutate(management = "M") %>%  select(management, iteration,node_id,taxon,abun)
M_sim$iteration<-as.character(M_sim$iteration)

SI_sim<-read.csv("Data/SI_sim_state_node_CP.csv", sep =,) %>% 
  mutate(management = "SI") %>%  select(management, iteration,node_id,taxon,abun)
SI_sim$iteration<-as.character(SI_sim$iteration)

I_sim<-read.csv("Data/I_sim_state_node_CP.csv", sep =,) %>% 
  mutate(management = "I") %>%  select(management, iteration,node_id,taxon,abun)
I_sim$iteration<-as.character(I_sim$iteration)

IM_sim<-read.csv("Data/IM_sim_state_node_CP.csv", sep =,) %>% rename (node_id=remain_species) %>% 
  mutate(management = "IM") %>%  select(management, iteration,node_id,taxon,abun)
IM_sim$iteration<-as.character(IM_sim$iteration)

#Upload empirical
Emp<-read.csv("Data/Land_use_rat_state_nodes_CP_intense.csv", sep =,) %>% 
  mutate(iteration = "Emp") %>%  select(management,iteration,node_id,taxon,abun)

## Final state node list
state_node_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim,IM_sim)
#write.csv(state_node_sim,"Data/state_node_sim_CP.csv", row.names= FALSE)



## - Species removed
SE_sim<-read.csv("Data/sps_removed_SE_CP.csv", sep =,) 
SE_sim$iteration<-as.character(SE_sim$iteration)

M_sim<-read.csv("Data/sps_removed_M_CP.csv", sep =,) 
M_sim$iteration<-as.character(M_sim$iteration) 

SI_sim<-read.csv("Data/sps_removed_SI_CP.csv", sep =,) 
SI_sim$iteration<-as.character(SI_sim$iteration) 

I_sim<-read.csv("Data/sps_removed_I_CP.csv", sep =,) 
I_sim$iteration<-as.character(I_sim$iteration) 

IM_sim<-read.csv("Data/sps_removed_IM_CP.csv", sep =,) 
IM_sim$iteration<-as.character(IM_sim$iteration) 

## Final data of species removed
sps_removed_sim<-rbind(SE_sim,M_sim,SI_sim,I_sim,IM_sim)
#write.csv(sps_removed_sim,"Data/sps_removed_sim_CP.csv", row.names= FALSE) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      DIRECT PROVISION AND INDIRECT EFFECTS ON ES                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For each shuffled network, we calculate the same variables as we did in the empirical network


############# 1. Calculate direct E(D)S provision 

state_node_sim<-read.csv("Data/state_node_sim_CP.csv", sep =,) #load state_node_list
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

#write.csv(direct_ES,"Data/direct_ES_sim_CP.csv", row.names= FALSE)




############# 2. Calculate indirect effecs of species on E(D)S provision  

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
edge_list_sim<-read.csv("Data/edge_list_sim_CP.csv", sep =,) #load edge_list of shuffled networks

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

#write.csv(Indirect_1hop_sim_2,"Data/ind_1hop_sim_CP.csv", row.names= FALSE)


### --  Calculate second order indirect effects  on E(D)S (considering 2 hops: node 1 - node 2 - node 3, effect of node 1 on node 3'E(D)S via node 2)

Indirect_1hop<-read.csv("Data/ind_1hop_sim_CP.csv",
                        sep =",") #load dataframe of indirect effects using 1 hop

#RUN for E-SE-M-SI-I with HPC

## Function to identify potential second order indirect effects for node_from in the row
ind_row <- function(df, row) { #for the row
  j <- df$node_to[row] #select node_to 
  l <- df$management[row] #select management 
  k<-df$iteration[row] #select iteration 
  
  df %>% 
    filter(node_from == j, node_to != df$node_from[row],  #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
           iteration == k, management == l, services_to != "None") %>% # Filter dataframe (filter node 3's ES affected by node 2)
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

#write.csv(Indirect_2hop_sim,"Data/ind_2hop_sim_CP.csv", row.names= FALSE)




#RUN IM management here (because it's not heavy)
ite=1:500
Indirect_1hop_IM<-read.csv("Data/ind_1hop_sim_CP.csv",
                        sep =",") %>% filter(management =="IM", iteration == "Emp" | iteration%in%ite) 

ind_row <- function(df, row) { #for the row
  if (row %% 1000 == 0) {
    print(100 * row/nrow(df))
  }
  j <- df$node_to[row] #select node_to 
  l <- df$management[row] #select management 
  k<-df$iteration[row] #select iteration 
  
  df %>% 
    dplyr::filter(node_from == j, node_to != df$node_from[row],  #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
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

IM_indirect_2hop_sim <- bind_rows(lapply(1:nrow(Indirect_1hop_IM), function(row) ind_row(Indirect_1hop_IM, row))) %>% 
  select(management,iteration,node_id,taxon_from,services, node_int,node_to,
         services_to,type,hop)

#write.csv(IM_indirect_2hop_sim,"Data/IM_ind_2hop_sim_CP.csv", row.names= FALSE)

# Upload dataframe of indirect effect of each simulation in each management (run in the HPC, keep 500 iterations)

E_ind_2<-read.csv("Data/E_ind_2hop_CP.csv", sep =",") #empirical extensive
SE_ind_2_300 <-read.csv("Data/HPC/SE_ind_2hop_sim_1_300_CP.csv", sep =",")
SE_ind_2_500 <-read.csv("Data/HPC/SE_ind_2hop_sim_301_500_CP.csv", sep =",") %>% filter(iteration!="Emp")
M_ind_2_300 <-read.csv("Data/HPC/M_ind_2hop_sim_1_300_CP.csv", sep =",")
M_ind_2_500 <-read.csv("Data/HPC/M_ind_2hop_sim_301_500_CP.csv", sep =",") %>% filter(iteration!="Emp")
SI_ind_2_300 <-read.csv("Data/HPC/SI_ind_2hop_sim_1_300_CP.csv", sep =",")
SI_ind_2_500 <-read.csv("Data/HPC/SI_ind_2hop_sim_301_500_CP.csv", sep =",") %>% filter(iteration!="Emp")
I_ind_2_300 <-read.csv("Data/HPC/I_ind_2hop_sim_1_300_CP.csv", sep =",")
I_ind_2_500 <-read.csv("Data/HPC/I_ind_2hop_sim_301_500_CP.csv", sep =",") %>% filter(iteration!="Emp")
IM_ind_2_500<-read.csv("Data/IM_ind_2hop_sim_CP.csv", sep =",")

Ind_2hop_sim<-rbind(E_ind_2, SE_ind_2_300,SE_ind_2_500,M_ind_2_300,M_ind_2_500,
                    SI_ind_2_300,SI_ind_2_500,I_ind_2_300,I_ind_2_500,IM_ind_2_500 ) 


# Join both 1 and 2 hops indirect effects dataframes
ite =1:500
Indirect_1hop<-read.csv("Data/ind_1hop_sim_CP.csv", sep =",") %>% 
  filter((iteration == "Emp" | iteration%in%ite) & services_to != "None")


# 1 hop
Indirect_1hop_sim<-Indirect_1hop %>%  rename("services" ="services_from",
                                             "node_id" = "node_from",
                                             "taxon" = "taxon_from") %>% 
                                      mutate(node_int = NA)

Indirect_1hop_sim<-Indirect_1hop_sim[,c(1,2,4,6,3,10,5,7,9,8)]


# 2 hop
Indirect_2hop_sim2<-Ind_2hop_sim %>% rename("taxon" = "taxon_from")

#  Indirect effect of ES (E,SE,M,SI,I,IM)
I_ES_sim <- rbind(Indirect_1hop_sim,Indirect_2hop_sim2) %>% filter(services_to !="None")

#write.csv(I_ES_sim,"Data/Indirect_ES_sim_CP.csv", row.names= FALSE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Analyses                            
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


################### Z - SCORE

# In this analysis we compare the z-score to check if the response variables (Prop. Direct ES retained,
#relative change in the amount, and proportion of Indirect ES retained) within each management and ES change between
#the empirical and simulated networks


#### -- Proportion of direct E(D)S retained across land use change --
#(we do it for every ES and management scenario)

#upload and prepare dataframe
ite = 1:500
direct_ES<- read.csv("Data/direct_ES_sim_CP.csv", sep =",") %>% filter(iteration == "Emp"|iteration%in%ite) 
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

# Observed (empirical)
direct_obs <-direct_ES %>% filter(iteration == "Emp") %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
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
  mutate(prop = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
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

#write.csv(dir_ES_z_score,"Data/z_score_dir_ES_CP.csv", row.names= FALSE)


## Plot Heat map
#We plot the z_score indicating if the Prortion of each ESs is significantly higher and lower than when we
#reandomly remove species 

# Prepare dataframe
z_score_dir<- dir_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensice and bird watching and seed dispersal for IM (all birds went extinct so there were no z scores)
sd_bw<-data.frame(management = c("E","E","E","E","E","E","E"), 
                  services = c("Bird watching", "Butterfly watching", 
                               "Crop damage", "Crop production","Pest control",
                               "Pollination", "Seed dispersal"),
                  z = c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),
                  signif = c("Benchmark","Benchmark","Benchmark","Benchmark",
                             "Benchmark","Benchmark","Benchmark"))
z_score_tot<- rbind (z_score_dir, sd_bw) %>% rename("Output" = "signif")

z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
z_score_tot$services <- factor(z_score_tot$services, levels = c("Seed dispersal", "Pollination","Pest control",
                                                                "Crop production", "Crop damage",
                                                                "Butterfly watching","Bird watching"))

#Summary averages (across management scenario)
averages_prop <- dir_ES_z_score %>% 
                  mutate(times_emp = dir_shuff_mean/Prop_mean) %>%  #calculate number of times lower the empirical respect the simulated (already averaged across iterations)
                  ungroup() %>% group_by(services) %>% 
                  summarise(average_prop_emp = mean(Prop_mean),
                            sd_prop_emp = sd(Prop_mean), n=n(),
                    average_prop_shuff = mean(dir_shuff_mean),#average of prop in simulated across management scenario
                            sd_prop_shuff = mean(dir_shuff_sd),#sd of prop in simulated across management scenario
                            ave_times_emp = mean(times_emp) #average times lower in the empirical compared null across management scenario
                            )

#Plot
library(ggtext)
color_services<-tibble(
services = unique(direct_ES$services),
 #color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d')),
color = c('#e7298a','#e6ab02','#7570b3','#2c7fb8','#a6761d','#d95f02','#1b9e77'))
 #arrange(desc(services)) #do it manually
# Create the custom legend data

pdf("Graphs/z_score_dir_CP.pdf", width = 12, height = 7)
ggplot(z_score_tot, aes(management, services, fill= Output)) + 
  geom_tile(color = "black")+
  scale_fill_manual(values = c("red","ivory1","ivory1"),
                    labels = c("Lower than random", 
                               "No difference",
                               "Benchmark"))+
  labs(x='Habitat Management', y="Ecosystem (dis)services")+
  theme_minimal()+
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black",fill = NA,size = 1),
          panel.spacing = unit(0.5, "cm", data = NULL),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=13,  color = color_services$color[match(levels(z_score_tot$services), color_services$services)]),
          # axis.text = element_text(size=15, color='black'),
          axis.text.x= element_text(size =15), 
          axis.title = element_text(size=17, color='black'),
          axis.line = element_blank(),
          legend.text.align = 0,
          legend.title =  element_text(size = 13, color = "black"),
          legend.text = element_text(size = 11))   +
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5, 
                   y = as.numeric(services) - 0.5, 
                   xend = as.numeric(management) + 0.5, 
                   yend = as.numeric(services) + 0.5), 
               color = "black", size = 0.5)

dev.off()




#### -- Change in the amount of direct E(D)S provided change across land use change --

## upload and prepare dataframe

#observed (empirical)
direct_ES_emp<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",")

extensive_amount<-direct_ES_emp %>% filter(management=="E") %>% group_by(management,services) %>% 
  summarize(tot_empirical = sum(weight))

amount_obs<-direct_ES_emp %>% group_by(management,services) %>% 
  summarize(tot = sum(weight)) %>% ungroup() %>%  
  mutate(Extensive_tot = case_when(
    services == "Bird watching"~ 381519.3800,
    services == "Butterfly watching"~ 244.7676,
    services == "Crop damage"~ 711450.9469,
    services == "Crop production"~ 209300.0000,
    services == "Pest control"~ 7108.3108,
    services == "Pollination"~ 36736.7426,
    services == "Seed dispersal"~ 362197.4900),
    ratio_change = tot / Extensive_tot  #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
  ) %>% filter(management!="E")


#shuffled 
amount_shuff <- direct_ES %>% filter(!(iteration == "Emp" & management =="E"))%>% 
  group_by(management,iteration,services) %>% 
  summarize(tot = sum(weight)) %>% 
  left_join(extensive_amount[,2:3], by = "services", suffix = c("", "_extensive")) %>%
   group_by(management,iteration,services) %>% 
    mutate(ratio_change = tot / tot_empirical)


# calculate Z-score
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

#write.csv(amount_ES_z_score,"Data/z_score_amount_ES_CP.csv", row.names= FALSE)



## Plot Heat map
#We plot the z_score indicating if the Prortion of each ESs is significantly higher and lower than when we
#reandomly remove species 

amount_ES_z_score<-read.csv("Data/z_score_amount_ES_CP.csv", sep =",")

# Prepare dataframe
z_score_amount<- amount_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensice and bird watching and seed dispersal for IM (all birds went extinct so there were no z scores)
sd_bw<-data.frame(management = c("E","E","E","E","E","E","E"), 
                  services = c("Bird watching", "Butterfly watching", 
                               "Crop damage", "Crop production","Pest control",
                               "Pollination", "Seed dispersal"),
                  z = c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),
                  signif = c("Benchmark","Benchmark","Benchmark","Benchmark",
                             "Benchmark","Benchmark","Benchmark"))
z_score_tot<- rbind (z_score_amount, sd_bw) %>% rename("Output" = "signif")

z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
z_score_tot$services <- factor(z_score_tot$services, levels = c("Seed dispersal", "Pollination","Pest control",
                                                                "Crop production", "Crop damage",
                                                                "Butterfly watching","Bird watching"))




#Plot
library(ggtext)
color_services<-tibble(
  services = unique(direct_ES$services),
  #color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d')),
  color = c('#e7298a','#e6ab02','#7570b3','#2c7fb8','#a6761d','#d95f02','#1b9e77'))
#arrange(desc(services)) #do it manually
# Create the custom legend data

pdf("Graphs/z_score_amount_CP.pdf", width = 12, height = 7)
ggplot(z_score_tot, aes(management, services, fill= Output)) + 
  geom_tile(color = "black")+
  scale_fill_manual(values = c("red","ivory1","ivory1"),
                    labels = c("Lower than random", 
                               "No difference",
                               "Benchmark")) +
  labs(x='Habitat Management', y="Ecosystem (dis)services")+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=13,  color = color_services$color[match(levels(z_score_tot$services), color_services$services)]),
        # axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))   +
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5, 
                   y = as.numeric(services) - 0.5, 
                   xend = as.numeric(management) + 0.5, 
                   yend = as.numeric(services) + 0.5), 
               color = "black", size = 0.5)

dev.off()




#### -- Proportion of indirect effects on E(D)S retained across land use change --

#upload and prepare dataframe
output_ind_ES<- read.csv("Data/Indirect_ES_sim_CP.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

#Observed (empirical)
output_ind_ES_emp <- read.csv("Data/Land_use_output_weighted_CP_intense.csv", sep =",")
output_ind_ES_emp$management <- factor(output_ind_ES_emp$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

#Empirical
indirect_obs<-output_ind_ES_emp  %>% 
  group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of indirect effects on E(D)S retained in the empirical
  dplyr::select(management,services_to,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% filter(management !="E")


# shuffled 
indirect_shuff<- output_ind_ES %>% filter(!(iteration == "Emp" & management =="E")) %>% 
  group_by(management,iteration,services_to) %>% 
  mutate(tot = n(),
         tot_emp = case_when( #Values of denominator
           services_to == "Crop production"~ 1129,
           services_to == "Pollination" ~ 18733,
           services_to == "Crop damage"~ 9992,
           services_to == "Pest control"~ 3272,
           services_to == "Seed dispersal" ~ 4224,
           services_to == "Butterfly watching"~ 3515,
           services_to == "Bird watching"~ 5820 )) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(Prop_mean = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services_to,Prop_mean) %>% 
  unique() 

indirect_shuff<- indirect_shuff %>% filter(!(iteration == "Emp" & management =="E"))

# calculate Z-score
indir_ES_z_score <- 
  inner_join(indirect_obs,
             indirect_shuff %>% select(-iteration) %>% 
               group_by(management,services_to) %>% 
               summarise(ind_shuff_mean=mean(Prop_mean), ind_shuff_sd=sd(Prop_mean), n=n())) %>% 
  drop_na() %>% 
  mutate(z=(Prop_mean-ind_shuff_mean)/ind_shuff_sd)

indir_ES_z_score %<>%
  mutate(signif=case_when(z>1.96 ~ 'above', # Obs is more than the shuffled
                          z< -1.96 ~ 'below', # Obs is lower than the shuffled
                          z<=1.96 | z>=-1.96 ~ 'not signif'))

#write.csv(indir_ES_z_score,"Data/z_score_ind_ES_CP.csv", row.names= FALSE)

#Summary averages (across management scenario)
averages_prop <- indir_ES_z_score %>% 
  mutate( times_emp = case_when(
      ind_shuff_mean > Prop_mean ~ ind_shuff_mean / Prop_mean,
      ind_shuff_mean <= Prop_mean ~ Prop_mean / ind_shuff_mean,
    )) %>%  #calculate number of times lower the empirical respect the simulated (already averaged across iterations)
  ungroup() %>% group_by(services_to) %>% 
  summarise(average_prop_emp = mean(Prop_mean),
            sd_prop_emp = sd(Prop_mean), n=n(),
            average_prop_shuff = mean(ind_shuff_mean),#average of prop in simulated across management scenario
            sd_prop_shuff = mean(ind_shuff_sd),#sd of prop in simulated across management scenario
            ave_times_emp = mean(times_emp) #average times lower in the empirical compared null across management scenario
  )


## Plot Heat map
#We plot the z_score indicating if the Prortion of each ESs is significantly higher and lower than when we
#reandomly remove species 

z_score_ind<-read.csv("Data/z_score_ind_ES_CP.csv", sep =",") %>% rename ("services" = "services_to")

# Prepare dataframe
z_score_ind<- z_score_ind %>% select(management,services,z,signif)

#Add row showing the extensice and bird watching and seed dispersal for IM (all birds went extinct so there were no z scores)
sd_bw<-data.frame(management = c("E","E","E","E","E","E","E"), 
                  services = c("Bird watching", "Butterfly watching", 
                               "Crop damage", "Crop production","Pest control",
                               "Pollination", "Seed dispersal"),
                  z = c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),
                  signif = c("Benchmark","Benchmark","Benchmark","Benchmark",
                             "Benchmark","Benchmark","Benchmark"))

z_score_tot<- rbind (z_score_ind, sd_bw) %>% rename("Output" = "signif")

z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
z_score_tot$services <- factor(z_score_tot$services, levels = c("Seed dispersal", "Pollination","Pest control",
                                                                "Crop production", "Crop damage",
                                                                "Butterfly watching","Bird watching"))


#Plot
library(ggtext)
color_services<-tibble(
  services = unique(direct_ES$services),
  #color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d')),
  color = c('#e7298a','#e6ab02','#7570b3','#2c7fb8','#a6761d','#d95f02','#1b9e77'))
#arrange(desc(services)) #do it manually
# Create the custom legend data

pdf("Graphs/z_score_ind_CP.pdf", width = 12, height = 7)
ggplot(z_score_tot, aes(management, services, fill= Output)) + 
  geom_tile(color = "black")+
  scale_fill_manual(values = c("blue","red","ivory1","ivory1"),
                    labels = c("Greater than random",
                               "Lower than random", 
                               "No difference",
                               "Benchmark"))+
  labs(x='Habitat Management', y="Ecosystem (dis)services")+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=13,  color = color_services$color[match(levels(z_score_tot$services), color_services$services)]),
        # axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))   +
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5, 
                   y = as.numeric(services) - 0.5, 
                   xend = as.numeric(management) + 0.5, 
                   yend = as.numeric(services) + 0.5), 
               color = "black", size = 0.5)

dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Plots                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#upload and prepare dataframe
direct_ES<- read.csv("Data/direct_ES_sim_CP.csv", sep =",")

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

#indirect
output_ind_ES<- read.csv("Data/Indirect_ES_sim_CP_final.csv", sep =",")%>% filter(services_to !="None")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors




### Plot of proportions of richness direct and indirect E(D)S retained 
color_services <-tibble(
  services = unique(direct_ES$services),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))

#direct 

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
  geom_boxplot(aes(colour = type), outlier.shape = NA, size = 0.8, position = position_dodge(width = 1.02)) +
  geom_point(aes(fill = factor(services), shape = factor(ifelse(type == "Null", 24, 21))), 
             position = position_jitterdodge(jitter.width = 1.2, dodge.width = 0.5),
             size = 2.4, stroke = 1, show.legend = c(fill = TRUE, shape = FALSE, colour = FALSE)) +
  scale_fill_manual(values = color_services$color, name = "E(D)S") +
  scale_shape_manual(values = c(21, 24)) + 
  scale_color_manual(values = c("black","firebrick"), name = "Type")+
  scale_y_continuous(name = "Prop. of direct E(D)S retained", limits = c(0, 1)) + 
  scale_x_discrete(name = "Habitat conversion")+
  guides(fill = guide_legend(order = 1,override.aes = list(shape=21)), 
         colour = guide_legend(order = 2), 
         shape = "none") +
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
        legend.title =  element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12))

prop_EDS_direct

#ggsave("Graphs/Direct_ES_sim_CP.png", width = 7, height = 5, dpi = 300)


### Plot of proportions of indirect effects on E(D)S retained  

#upload and prepare dataframe 

#empirical
output_ind_ES_emp <- read.csv("Data/Land_use_output_weighted_CP_intense.csv", sep =",")
output_ind_ES_emp$management <- factor(output_ind_ES_emp$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

#Empirical
Prop_indir_Emp<-output_ind_ES_emp  %>% 
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

#Simulations
output_ind_ES<- read.csv("Data/Indirect_ES_sim_CP.csv", sep =",") %>% filter(services_to !="None")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors


Prop_ind_null<-output_ind_ES %>% filter(!(iteration == "Emp" & management =="E")) %>% 
  group_by(management,iteration,services_to) %>% 
  mutate(tot = n(),
         tot_emp = case_when( #Values of denominator
           services_to == "Crop production"~ 1129,
           services_to == "Pollination" ~ 18733,
           services_to == "Crop damage"~ 9992,
           services_to == "Pest control"~ 3272,
           services_to == "Seed dispersal" ~ 4224,
           services_to == "Butterfly watching"~ 3515,
           services_to == "Bird watching"~ 5820 )) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(Prop_mean = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
  dplyr::select(management,iteration,services_to,Prop_mean) %>%
  unique() %>% mutate(type = "Null")


# adjust the extreme values according to the beta conditions
Prop_ind_null$Prop_mean <- ifelse(Prop_ind_null$Prop_mean == 0, 0.000001, 
                                  ifelse(Prop_ind_null$Prop_mean == 1, 0.9999999, Prop_ind_null$Prop_mean))


#write.csv(Prop_ind_null, "Data/Prop_ind_null_CP.csv",  row.names = FALSE)

Prop_indir_sim<- Prop_ind_null %>% group_by(management,services_to) %>%
  summarise(Prop_mean = mean(Prop_mean)) %>% 
   mutate(type = "Null")


# Merge empirical and simulations and remove extensive scenario
Prop_indir_sim2 <- rbind(Prop_indir_Emp, Prop_indir_sim)
Prop_indir_sim2$management <- factor(Prop_indir_sim2$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors


#Plot 

color_services <-tibble(
  services = unique(Prop_indir_sim2$services_to),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))


prop_EDS_indirect <- Prop_indir_sim2%>% ggplot(aes(x = management, y = Prop_mean)) +
  geom_boxplot(aes(colour = type), outlier.shape = NA, size = 0.8, position = position_dodge(width = 1.02))  +
  geom_point(aes(fill = factor(services_to), shape = factor(ifelse(type == "Null", 24, 21))), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
             size = 2.4, stroke = 1, show.legend = c(fill = TRUE, shape = FALSE, colour = FALSE)) +
  scale_fill_manual(values = color_services$color, name = "E(D)S") +
  scale_shape_manual(values = c(21, 24)) + 
  scale_color_manual(values = c("black","firebrick"))+
  scale_y_continuous(name = "Prop. of indirect effects on E(D)S retained", limits = c(0, 1)) + 
  scale_x_discrete(name = "Habitat conversion")+
  guides(fill = guide_legend(order = 1,override.aes = list(shape=21)), 
         colour = guide_legend(order = 2), 
         shape = "none") +
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
        legend.title =  element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12))

prop_EDS_indirect

#ggsave("Graphs/Indirect_ES_sim_CP.png", width = 7, height = 5, dpi = 300)




#### Prop Amount of direct E(D)S

### Calculate ratio of change for empirical
direct_ES_emp<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",") #upload empirical

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

#amount the rest ESs
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
Prop_amount<- rbind(Prop_weight_watching,Prop_weight_rest) 

### Calculate ratio of change for simulations
direct_ES_sim<- read.csv("Data/direct_ES_sim_CP.csv", sep =",")


## Amount Bird and butterfly watching
dir_amount_watching_sim <- direct_ES_sim %>% filter (management !="E") %>% 
                          filter(services == "Bird watching" | services == "Butterfly watching" ) %>%
                          group_by(management,iteration,services) %>% 
                          summarize(tot_sim_amount = sum(abun)) %>% 
                          left_join(Prop_amount, by = c("management", "services"), suffix = c("", "_extensive")) %>%
                          select(-ratio_change)  %>% 
                           mutate(ratio_change = tot_sim_amount / Extensive_tot) %>% #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
                           select(management,services,ratio_change) %>% 
                         group_by(management,services) %>% 
                        summarise(ratio_change_ave = mean(ratio_change)) %>% 
                       mutate(type = "Null")


## Amount of the rest Es
dir_amount_rest_sim <- direct_ES_sim %>% filter (management !="E") %>% 
  filter(!(services == "Bird watching" | services == "Butterfly watching" )) %>%
  group_by(management,iteration,services) %>% 
  summarize(tot_sim_amount= sum(weight)) %>% 
  left_join(Prop_amount, by = c("management", "services"), suffix = c("", "_extensive")) %>%
  select(-ratio_change)  %>% 
  mutate(ratio_change = tot_sim_amount / Extensive_tot) %>% #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
  select(management,services,ratio_change) %>% 
  group_by(management,services) %>% 
  summarise(ratio_change_ave = mean(ratio_change)) %>% 
  mutate(type = "Null")

#merge the data
Prop_amount_sim<- rbind(dir_amount_watching_sim,dir_amount_rest_sim)



### Final dataframe
Prop_amount_emp<- Prop_amount %>% mutate(type = "Empirical") %>% select(-tot,-Extensive_tot) %>%  rename(ratio_change_ave = ratio_change)

ratio_amount<-rbind(Prop_amount_emp,Prop_amount_sim)
ratio_amount$management <- factor(ratio_amount$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors


#Plot 
prop_EDS_ratio <- ratio_amount%>% ggplot(aes(x = management, y = ratio_change_ave)) +
  geom_boxplot(aes(colour = type), outlier.shape = NA, size = 0.8,  position = position_dodge(width = 1.02)) +
  geom_point(aes(fill = factor(services), shape = factor(ifelse(type == "Null", 24, 21))), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.4),
             size = 2.4, stroke = 1, show.legend = c(fill = TRUE, shape = FALSE, colour = FALSE)) +
  scale_fill_manual(values = color_services$color, name = "E(D)S") +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("black","firebrick"))+
  scale_y_continuous(name = "Relative change in the amount of direct E(D)S provided", limits = c(0, 3)) + 
  scale_x_discrete(name = "Habitat conversion")+
  guides(fill = guide_legend(order = 1,override.aes = list(shape=21)), 
         colour = guide_legend(order = 2), 
         shape = "none") +
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
        legend.title =  element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12))

prop_EDS_ratio


#ggsave("Graphs/ratio_change_sim_CP.png", width = 7, height = 5, dpi = 300)



###### Number of species removed in each step
sps_removed_man <- read.csv("Data/sps_removed_sim_CP.csv", sep =",") %>% 
  group_by(habitat_from, iteration) %>% 
  summarise(count = n()) %>% 
  select(habitat_from, count) %>% 
  unique() %>% 
  group_by(habitat_from) %>% 
  summarise(
    average_sps_lost = mean(count),
    se_sps_lost = sd(count) / sqrt(n())
  )

sps_lost<- tibble(management = c("SE","M","SI","I","IM"),
                  mean_sps_lost = c(194, 211,109,51,349.4),
                  se_sps_lost = c(0, 0,0,0,2.527141))
sps_lost$management <- factor(sps_lost$management, levels = c( "SE", "M", "SI","I","IM")) #change order of factors



ggplot(sps_lost, aes(x = management, y = mean_sps_lost)) +
  geom_point() +                     # Scatter plot
  geom_line(aes(group = 1)) +        # Line connecting the dots
  geom_errorbar(aes(ymin = mean_sps_lost - se_sps_lost, ymax = mean_sps_lost + se_sps_lost), width = 0.2) + # Error bars
  labs(title = "Average Species Lost by Management",
       x = "Management",
       y = "Average species lost") +
  theme_classic()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=13, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9))

ggsave("Graphs/sps_lost_land_conversion_CP.png", width = 7, height = 5, dpi = 300)


###### Average trophic group of species removed

## Final data of species removed (simulation)
nodes_ES<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",") %>% ungroup() %>% 
            select(node_id,taxon,services) %>% unique()


#Try to put empirical
taxon_removed_sim <- read.csv("Data/sps_removed_sim_CP.csv", sep =",") %>% 
                left_join(Norwood_farm$nodes[,c(1,3)], by = c("species_rem" = "node_id")) %>% 
                left_join(nodes_ES[,c(1,3)], by =  c("species_rem" = "node_id") )%>% 
                mutate(services = ifelse(is.na(services), "None", services)) %>% 
                group_by(management,iteration,taxon) %>% mutate(number_taxon= n()) %>% 
                group_by(management, taxon) %>% summarise(ave_taxon = mean(number_taxon)) %>% 
                group_by(management) %>% mutate(tot = sum(ave_taxon),
                                               Prop_taxon = ave_taxon / tot)

taxon_removed_sim$management <- factor(taxon_removed_sim$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
  
color_trophic <-tibble(taxon = c("Plant","Flower-visiting","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                 "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                 "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                       color = c("#33a02c","#a6cee3","#1f78b4","#b2df8a","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6",
                                 "#6a3d9a", "#ffff99", "#e7298a"))



  prop_taxon<- taxon_removed_sim %>% ggplot(aes(y=Prop_taxon, x= management, fill = taxon)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
    scale_fill_manual(values = setNames(color_trophic$color, color_trophic$taxon))+
  labs(x='Management', y="Prop. of taxa sps removed") +theme_bw()+
  theme_classic()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=13, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9))

  ggsave("Graphs/taxon_removed_CP.png", width = 7, height = 5, dpi = 300)
  
  


  ###### Average degree of species removed
  
  ### Calculate sps removed in each management conversion from extrensive empirical to Intensive empirical
  
  CP <- Norwood_farm$state_nodes %>% filter(layer_name =="CP") %>% select(node_id)# species in CP
  
  # List of species that were removed in each habitat 
  sps_removed_emp <- Norwood_farm$state_nodes %>% filter(layer_name !="CP") %>% 
    filter (!node_id %in% CP$node_id) %>% # Identify species that were removed when convert the land from empirical to intensive %>% 
    left_join(Norwood_farm$nodes[,c(1,3)], by = "node_id") %>% 
    left_join(nodes_ES[,c(1,3)], by =  "node_id")%>% 
    mutate(services = ifelse(is.na(services), "None", services))
  
  # Add degree of species removed
  edge_list_layer_name<- Norwood_farm$extended_ids %>% 
    left_join(Norwood_farm$layers,by = c("layer_from"="layer_id")) %>% 
    select(node_from,node_to,layer_name)
  
  edge_list_layer_name_inv<- edge_list_layer_name %>%  select(node_to, node_from,layer_name) #created a inverted version
  colnames(edge_list_layer_name_inv) <- c("node_from", "node_to","layer_name")
  
  edge_list_to_filter <- rbind(edge_list_layer_name, edge_list_layer_name_inv)#bind them
  
  degree_sp_rem<-NULL
  
  for (i in unique(sps_removed_emp$layer_name)){ #for each habitat
    
    #filter data
    list_nodes <- sps_removed_emp%>% filter(layer_name == i) #select species that went extinct
    
    #check degree information in the edgelist
    degree_sp <- edge_list_to_filter %>% 
      filter(layer_name == i & (node_from %in%list_nodes$node_id)) %>% 
      group_by(layer_name,node_from) %>% summarise(degree = n())
    
    #Storage 
    degree_sp_rem<- bind_rows(degree_sp_rem, degree_sp)
  }
  
  
  # Re arrange dataframe  and add taxon information
  degree_sp_rem_emp<- degree_sp_rem %>% 
    mutate (layer_name = case_when( #Put name of conversion
      layer_name == "WD" |  layer_name == "RG"~ "SE",
      layer_name == "MH" |  layer_name == "NH"~ "M",
      layer_name == "PP" |  layer_name == "GM"  |  layer_name == "SF"~ "SI",
      layer_name == "LP" |  layer_name == "NL"  ~ "I")) %>% 
    rename("node_id"="node_from", "management" = "layer_name") %>% 
    left_join(Norwood_farm$nodes[,c(1,3)], by = "node_id") %>%    #add taxon information
    left_join(nodes_ES[,c(1,3)], by =  "node_id")%>% 
    mutate(services = ifelse(is.na(services), "None", services)) %>% 
    group_by(management,taxon)%>% summarise(ave_degree = mean(degree)) 
    

# Simulated network
  degree_removed_sim <- read.csv("Data/sps_removed_sim_CP.csv", sep =",") %>% 
    group_by(management) %>% summarise(ave_degree = mean(degree)) 
  
 taxon_removed_sim$management <- factor(taxon_removed_sim$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
  
 degree_removed_sim <- read.csv("Data/sps_removed_sim_CP.csv", sep =",") %>% 
    left_join(Norwood_farm$nodes[,c(1,3)], by = c("species_rem" = "node_id")) %>% 
    left_join(nodes_ES[,c(1,3)], by =  c("species_rem" = "node_id") )%>% 
    mutate(services = ifelse(is.na(services), "None", services)) %>% 
   group_by(management,taxon)%>% summarise(ave_degree = mean(degree)) 

bird_plants_emp <- degree_sp_rem_emp %>% filter(taxon =="Seed-feeding bird" |
                                                  taxon =="Plant"|
                                                  taxon =="Butterfly")
bird_plants_sim <-  degree_removed_sim %>% filter(taxon =="Seed-feeding bird" |
                                                  taxon =="Plant"|
                                                    taxon =="Butterfly")
  
 #Focus on the SE between NULL and empirical
 SE_emp <-degree_sp_rem_emp %>% filter(management =="SE")
 SE_sim <- degree_removed_sim %>% filter(management =="SE")



 
 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #                      Analyses  EMPIRICAL VS NULL SLOPES        DESCARTADO POR AHORA!                  
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 #################### MODELS COMPARISON
 
 
 #In this analysis we will compare the results of the empirical model with results of the null ones. WE create a null distribution and test
 #it by calculating a p value. Also, we do it for every combination of management.
 
 
 #### -- Proportion of direct E(D)S retained across land use change --
 
 #upload and prepare dataframe
 direct_ES<- read.csv("Data/direct_ES_sim_CP.csv", sep =",") %>% filter(iteration == "Emp"|iteration <=500)
 
 direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
 
 
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
   
   if (length(iteration_coeff$iteration) == 7) {
     next  #if an iteration is missing, then skip to the next value of iteration
   }
   
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
                                        iteration = rep(i,15)))
 }
 
 
 iterations_coefficients
 #write.csv(iterations_coefficients, "Data/iterations_coefficients_direct_CP.csv", row.names = FALSE)
 
 iterations_post_hocs
 #write.csv(iterations_post_hocs, "Data/iterations_post_hocs_direct_CP.csv", row.names = FALSE)
 
 # Compare between empirical and simulations
 iterations_coefficients<-read.csv("Data/iterations_coefficients_direct_CP.csv", sep =",")
 
 #General
 greater <- sum(iterations_coefficients$chisqr_man > par_emp[1,1])
 less <- sum(iterations_coefficients$chisqr_man  < par_emp[1,1])
 p_management<- 2 * min(greater, less) /nrow(iterations_coefficients) #calculate manually t-test two tailed #CAMBIAR ESTO LENGTH..
 
 
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
 
 
 #ggsave("Graphs/distri_direct_CP_IM.png",width = 6, height = 5, dpi = 300)
 
 
 
 
 #Plot combination of treatments level 
 iterations_post_hocs<-read.csv("Data/iterations_post_hocs_direct_CP.csv", sep =",")
 
 #Post hoc (each combination of management )
 p_contrast_man<-NULL
 
 for (i in unique(iterations_post_hocs$contrast)){
   
   #filter data
   post_comb<-iterations_post_hocs %>% filter(contrast == i) #filter per combination of treatmens (simulated data)
   post_emp<-man_pairs_emp %>% filter(contrast == i) #filter per combination of treatmens (empirical)
   
   #test
   greater <- sum(post_comb$estimate >  post_emp$estimate)
   less <- sum(post_comb$estimate  <  post_emp$estimate)
   p_comb<- 2 * min(greater, less) / nrow(iterations_coefficients)  #calculate manually t-test two tailed
   
   #storage
   p_contrast_man <- rbind(p_contrast_man,
                           tibble(contrast = i,
                                  p_value = p_comb))
   
 }
 p_contrast_man
 
 
 #Prepare dataframe (simulated and empircal)
 
 #smimulated
 man_lev<-c("E","SE","M","SI","I","IM")
 
 post_hoc_sim<- iterations_post_hocs %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
   select(-p.value)
 post_hoc_sim$man_1 <- factor(post_hoc_sim$man_1, levels = man_lev)
 post_hoc_sim$man_2 <- factor(post_hoc_sim$man_2, levels = man_lev)
 
 #empirical
 post_hoc_emp_pre<- man_pairs_emp %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
   select(-p.value) %>% mutate (iteration = "0") %>% select(man_1,man_2,estimate,iteration)#empirical
 
 new_combinations_empirical <- data.frame(
   man_1 = c("E", "SE", "M", "SI", "I","IM"),
   man_2 = c("E", "SE", "M", "SI", "I","IM"),
   estimate = rep(0, 6),
   iteration = rep(0, 6)
 )
 
 post_hoc_emp_fin<-rbind(post_hoc_emp_pre,new_combinations_empirical)
 post_hoc_emp_fin$man_1 <- factor(post_hoc_emp_fin$man_1, levels = man_lev)
 post_hoc_emp_fin$man_2 <- factor(post_hoc_emp_fin$man_2, levels = man_lev)
 
 
 # plot
 posthoc_distr_CP<- ggplot(data = post_hoc_sim, aes(x=estimate)) +
   geom_histogram(fill = "steelblue") +
   labs(y = "Count", x = "Estimate value") +
   geom_vline(data = post_hoc_emp_fin[1:15,], mapping = aes(xintercept = estimate), 
              colour="#BB0000", linetype="dashed") +
   facet_grid(man_2 ~ man_1)
 
 #ggsave("Graphs/posthoc_distri_direct_CP_IM.png")
 
 
 
 
 #### -- Proportion of indirect effect on E(D)S retained across land use change --
 
 
 #upload and prepare dataframe 
 output_ind_ES<- read.csv("Data/Indirect_ES_sim_CP_final.csv", sep =",") %>% filter(services_to !="None")
 output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
 
 
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
 
 #Simulations
 Prop_ind_null<-output_ind_ES %>% filter(!(iteration == "Emp" & management !="E")) %>% 
   group_by(management,iteration,services_to) %>% 
   mutate(tot = n(),
          tot_emp = case_when( #Values of denominator
            services_to == "Crop production"~ 1129,
            services_to == "Pollination" ~ 18733,
            services_to == "Crop damage"~ 9992,
            services_to == "Pest control"~ 3272,
            services_to == "Seed dispersal" ~ 4224,
            services_to == "Butterfly watching"~ 3515,
            services_to == "Bird watching"~ 5820 ))  %>% ungroup() %>%  
   group_by(services_to) %>% 
   mutate(Prop_mean = tot/tot_emp) %>%  #prop of E(D)S rtained across habitat management per iteration and habitat management
   dplyr::select(management,iteration,services_to,Prop_mean) %>%
   unique() %>% mutate(type = "Null")
 
 
 # adjust the extreme values according to the beta conditions
 Prop_ind_null$Prop_mean <- ifelse(Prop_ind_null$Prop_mean == 0, 0.000001, 
                                   ifelse(Prop_ind_null$Prop_mean == 1, 0.9999999, Prop_ind_null$Prop_mean))
 
 
 #write.csv(Prop_ind_null, "Data/Prop_ind_null_CP.csv",  row.names = FALSE)
 
 
 ###  - Models
 
 # empirical
 Prop_ind_emp <- glmmTMB (Prop_mean ~ management + services_to, family=beta_family(link="logit"), 
                          data = Prop_indir_Emp)# the best mode #model used in the empirical
 
 par_emp<-Anova(Prop_ind_emp)
 post_man_emp <-emmeans(Prop_ind_emp, ~ management) #posthoc management
 man_pairs_emp <- as.data.frame(pairs(post_man_emp))
 
 
 
 #Null
 #we do the same model for each iteration to create the null distribution of the model coefficient
 Prop_ind_null<- read.csv("Data/Prop_ind_null_CP.csv", sep =",")
 
 iterations_coefficients<-NULL
 iterations_post_hocs<-NULL
 
 #iterations_rem<- unique(Prop_ind_null$iteration)
 #iterations<- iterations_rem[2:length(iterations_rem)]#separate between iterations and Empirical
 
 for (i in 1:500){ #for each iteration
   print(i)
   iteration_coeff<-Prop_ind_null %>% filter(iteration == i | iteration == "Emp")
   
   if (length(iteration_coeff$iteration) == 7) {
     next  #if an iteration is missing, then skip to the next value of iteration
   }
   
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
                                        iteration = rep(i,15)))
 }
 
 
 iterations_coefficients
 #write.csv(iterations_coefficients, "Data/iterations_coefficients_indirect_CP.csv", row.names = FALSE)
 
 iterations_post_hocs
 #write.csv(iterations_post_hocs, "Data/iterations_post_hocs_indirect_CP.csv", row.names = FALSE)
 
 
 #General Test
 iterations_coefficients<-read.csv("Data/iterations_coefficients_indirect_CP.csv", sep =",")
 
 greater <- sum(iterations_coefficients$chisqr_man > par_emp[1,1])
 less <- sum(iterations_coefficients$chisqr_man  < par_emp[1,1])
 p_management<- 2 * min(greater, less) / nrow(iterations_coefficients) #calculate manually t-test two tailed
 
 
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
 #ggsave("Graphs/distri_indirect_CP_IM.png", width = 6, height = 5, dpi = 300)
 
 
 # Plot combination of treatments level 
 iterations_post_hocs<-read.csv("Data/iterations_post_hocs_indirect_CP.csv", sep =",")
 
 #Post hoc (each combination of management )
 p_contrast_man<-NULL
 
 for (i in unique(iterations_post_hocs$contrast)){
   
   #filter data
   post_comb<-iterations_post_hocs %>% filter(contrast == i) #filter per combination of treatmens (simulated data)
   post_emp<-man_pairs_emp %>% filter(contrast == i) #filter per combination of treatmens (empirical)
   
   #test
   
   greater <- sum(post_comb$estimate >  post_emp$estimate)
   less <- sum(post_comb$estimate  <  post_emp$estimate)
   p_comb<- 2 * min(greater, less) /nrow(iterations_coefficients)#calculate manually t-test two tailed
   
   #storage
   p_contrast_man <- rbind(p_contrast_man,
                           tibble(contrast = i,
                                  p_value = p_comb))
   
 }
 
 p_contrast_man
 
 
 #Prepare dataframe (simulated and empircal)
 
 #smimulated
 man_lev<-c("E","SE","M","SI","I","IM")
 
 post_hoc_sim<- iterations_post_hocs %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
   select(man_1,man_2,estimate,iteration)
 
 post_hoc_sim_inv<- post_hoc_sim %>% select(man_2,man_1,estimate, iteration)
 colnames(post_hoc_sim_inv) <- c("man_1", "man_2","estimate", "iteration")
 
 post_hoc_sim_fin<- rbind(post_hoc_sim,post_hoc_sim_inv)
 
 post_hoc_sim_fin$man_1 <- factor(post_hoc_sim_fin$man_1, levels = man_lev)
 post_hoc_sim_fin$man_2 <- factor(post_hoc_sim_fin$man_2, levels = man_lev)
 
 
 #empirical
 post_hoc_emp_pre<- man_pairs_emp %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
   select(-p.value) %>% mutate (iteration = 0) %>% select(man_1,man_2,estimate,iteration)#empirical
 
 new_combinations_empirical <- data.frame(
   man_1 = c("E", "SE", "M", "SI", "I","IM"),
   man_2 = c("E", "SE", "M", "SI", "I","IM"),
   estimate = rep(0, 6),
   iteration = rep(0, 6)
 )
 
 post_hoc_emp_fin<-rbind(post_hoc_emp_pre,new_combinations_empirical)
 post_hoc_emp_fin_inv<- post_hoc_emp_fin %>% select(man_2,man_1,estimate, iteration)
 colnames(post_hoc_emp_fin_inv) <- c("man_1", "man_2","estimate", "iteration")
 
 post_hoc_emp_fin_2<- rbind(post_hoc_emp_fin,post_hoc_emp_fin_inv)
 post_hoc_emp_fin_2$man_1 <- factor(post_hoc_emp_fin_2$man_1, levels = man_lev)
 post_hoc_emp_fin_2$man_2 <- factor(post_hoc_emp_fin_2$man_2, levels = man_lev)
 
 
 # plot
 lower_triangle_data <- post_hoc_sim_fin[as.numeric(post_hoc_sim_fin$man_2) >= as.numeric(post_hoc_sim_fin$man_1), ]
 lower_triangle_emp <- post_hoc_emp_fin_2[as.numeric(post_hoc_emp_fin_2$man_2) >= as.numeric(post_hoc_emp_fin_2$man_1), ]
 
 posthoc_distr_CP<- ggplot(data = lower_triangle_data, aes(x=estimate)) +
   geom_histogram(fill = "steelblue") +
   labs(y = "Count", x = "Estimate value") +
   geom_vline(data = lower_triangle_emp[1:15,], mapping = aes(xintercept = estimate), 
              colour="#BB0000", linetype="dashed") +
   facet_grid(man_2 ~ man_1)
 
 #ggsave("Graphs/posthoc_distri_indirect_CP_fin.png")
 
 
 
 
 
 
 #### -- Change in the amount of direct E(D)S provided change across land use change --
 
 ## - upload and prepare dataframe
 direct_ES_emp<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",")
 
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
 direct_ES_sim<- read.csv("Data/direct_ES_sim_CP.csv", sep =",")
 
 dir_amount_sim <- direct_ES_sim %>% filter(!(iteration == "Emp" & management !="E"))%>% 
   left_join(extensive_amount, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
   mutate(ratio_change = weight / weight_extensive) #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
 
 ratio_sim<-dir_amount_sim  %>% 
   select(management,iteration, services,node_id, ratio_change) %>% group_by(management,iteration,services,node_id) %>% 
   summarise(ratio_change_ave = mean(ratio_change)) 
 
 
 
 ###  - Models
 
 # empirical
 amount_emp <- glmmTMB(ratio_change_ave ~ management + services + (1|node_id),family = Gamma(link = "log"),
                       data = ratio_empirical) 
 
 #amount_emp2 <- glmmTMB(ratio_change_ave ~ management+services + (1|node_id),family = Gamma(link = "log"),
 #                     data = ratio_empirical) #don't converge properly
 par_emp<-Anova(amount_emp)
 post_man_emp <-emmeans(amount_emp, ~ management) #posthoc management
 man_pairs_emp <- as.data.frame(pairs(post_man_emp))
 
 
 # Null
 #we do the same model for each iteration to create the null distribution of the model coefficient
 
 iterations_coefficients<-NULL
 iterations_post_hocs<-NULL
 
 
 for (i in 1:500){ #for each iteration
   print(i)
   iteration_coeff<-ratio_sim %>% filter(iteration == i | iteration == "Emp")
   if (length(iteration_coeff$iteration) == 7) {
     next  #if an iteration is missing, then skip to the next value of iteration
   }
   
   glmm_val_amount <- glmmTMB (ratio_change_ave ~ management + services + (1|node_id),family = Gamma(link = "log"), data = iteration_coeff)
   par<-Anova(glmm_val_amount)
   post_man <-emmeans(glmm_val_amount, ~ management) #posthoc management
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
                                        iteration = rep(i,15)))
 }
 
 
 iterations_coefficients
 #write.csv(iterations_coefficients, "Data/iterations_coefficients_amount_CP.csv", row.names = FALSE)
 
 iterations_post_hocs
 #write.csv(iterations_post_hocs, "Data/iterations_post_hocs_amount_CP.csv", row.names = FALSE)
 
 
 # Compare between empirical and simulations
 iterations_coefficients<-read.csv("Data/iterations_coefficients_amount_CP.csv", sep = ",")
 
 # General Test
 greater <- sum(iterations_coefficients$chisqr_man > par_emp[1,1])
 less <- sum(iterations_coefficients$chisqr_man  < par_emp[1,1])
 p_management<- 2 * min(greater, less) / nrow(iterations_coefficients) #calculate manually t-test two tailed
 
 
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
 #ggsave("Graphs/distri_amount_CP_IM.png", width = 6, height = 5, dpi = 300)
 
 
 
 
 #Plot combination of treatments level 
 iterations_post_hocs<-read.csv("Data/iterations_post_hocs_amount_CP.csv", sep =",")
 
 #Post hoc (each combination of management )
 p_contrast_man<-NULL
 
 for (i in unique(iterations_post_hocs$contrast)){
   
   #filter data
   post_comb<-iterations_post_hocs %>% filter(contrast == i) #filter per combination of treatmens (simulated data)
   post_emp<-man_pairs_emp %>% filter(contrast == i) #filter per combination of treatmens (empirical)
   
   #test
   greater <- sum(post_comb$estimate >  post_emp$estimate)
   less <- sum(post_comb$estimate  <  post_emp$estimate)
   p_comb<- 2 * min(greater, less) / nrow(iterations_coefficients)  #calculate manually t-test two tailed
   
   #storage
   p_contrast_man <- rbind(p_contrast_man,
                           tibble(contrast = i,
                                  p_value = p_comb))
   
 }
 p_contrast_man
 
 
 #Prepare dataframe (simulated and empircal)
 
 #smimulated
 man_lev<-c("E","SE","M","SI","I","IM")
 
 post_hoc_sim<- iterations_post_hocs %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
   select(man_1,man_2,estimate,iteration)
 
 post_hoc_sim_inv<- post_hoc_sim %>% select(man_2,man_1,estimate, iteration)
 colnames(post_hoc_sim_inv) <- c("man_1", "man_2","estimate", "iteration")
 
 post_hoc_sim_fin<- rbind(post_hoc_sim,post_hoc_sim_inv)
 
 post_hoc_sim_fin$man_1 <- factor(post_hoc_sim_fin$man_1, levels = man_lev)
 post_hoc_sim_fin$man_2 <- factor(post_hoc_sim_fin$man_2, levels = man_lev)
 
 
 #empirical
 post_hoc_emp_pre<- man_pairs_emp %>% separate(contrast, into = c("man_1", "man_2"), sep = " - ") %>% 
   select(-p.value) %>% mutate (iteration = 0) %>% select(man_1,man_2,estimate,iteration)#empirical
 
 new_combinations_empirical <- data.frame(
   man_1 = c("E", "SE", "M", "SI", "I","IM"),
   man_2 = c("E", "SE", "M", "SI", "I","IM"),
   estimate = rep(0, 6),
   iteration = rep(0, 6)
 )
 
 post_hoc_emp_fin<-rbind(post_hoc_emp_pre,new_combinations_empirical)
 post_hoc_emp_fin_inv<- post_hoc_emp_fin %>% select(man_2,man_1,estimate, iteration)
 colnames(post_hoc_emp_fin_inv) <- c("man_1", "man_2","estimate", "iteration")
 
 post_hoc_emp_fin_2<- rbind(post_hoc_emp_fin,post_hoc_emp_fin_inv)
 post_hoc_emp_fin_2$man_1 <- factor(post_hoc_emp_fin_2$man_1, levels = man_lev)
 post_hoc_emp_fin_2$man_2 <- factor(post_hoc_emp_fin_2$man_2, levels = man_lev)
 
 
 # plot
 lower_triangle_data <- post_hoc_sim_fin[as.numeric(post_hoc_sim_fin$man_2) >= as.numeric(post_hoc_sim_fin$man_1), ]
 lower_triangle_emp <- post_hoc_emp_fin_2[as.numeric(post_hoc_emp_fin_2$man_2) >= as.numeric(post_hoc_emp_fin_2$man_1), ]
 
 posthoc_distr_CP<- ggplot(data = lower_triangle_data, aes(x=estimate)) +
   geom_histogram(fill = "steelblue") +
   labs(y = "Count", x = "Estimate value") +
   geom_vline(data = lower_triangle_emp[c(1:8,15:21),], mapping = aes(xintercept = estimate), 
              colour="#BB0000", linetype="dashed") +
   facet_grid(man_2 ~ man_1)
 #ggsave("Graphs/posthoc_distri_amount_CP_fin.png")
 
 
 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Analyses           DESCARTADO POR AHORA!                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### -- Proportion of direct E(D)S retained across land use change --

#upload and prepare dataframe
direct_ES<- read.csv("Data/direct_ES_sim_CP.csv", sep =",")

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


#Empirical
Prop_dir_Emp<-direct_ES %>% filter(iteration == "Emp") %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  dplyr::select(management,services,tot,prop) %>%
  unique() %>% rename("Prop_mean" = "prop") %>% 
  mutate (type = "Empirical")

# adjust the extreme values according to the beta conditions and remove extensive scenario
Prop_dir_Emp$Prop_mean <- ifelse(Prop_dir_Emp$Prop_mean == 0, 0.000001, 
                                 ifelse(Prop_dir_Emp$Prop_mean == 1, 0.9999999, Prop_dir_Emp$Prop_mean))
# Value of empirical
values_emp<- Prop_dir_Emp %>% filter (management == "E" ) %>% select(management,tot)

#Simulations #FILTRRAR PARA DEJAR ITERATION 1:1000 AND THE EMPIRICAL
Prop_dir_sim<-direct_ES %>% filter(iteration != "Emp" & management =="E") %>% group_by(management,iteration,services) %>% 
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

# Models
#shapiro.test(Prop_dir_sim2$Prop_mean)#check for normality #it's not normal so we use beta

Prop_dir_emp<-glmmTMB (Prop_mean ~ management + services, family=beta_family(link="logit"), data = Prop_dir_Emp) #empirical

Prop_dir_null<-glmmTMB (Prop_mean ~ management + services, family=beta_family(link="logit"), data = Prop_dir_sim2) # model that best fit
#Prop_dire2<-glmmTMB (Prop_mean ~ type + management, family=beta_family(link="logit"), data = Prop_dir_sim2)
#Prop_dire3<- glmmTMB (Prop_mean ~ type + (1| management/services),family=beta_family(link="logit"),data = Prop_dir_sim2)
#Prop_dire4<-glmmTMB (Prop_mean ~ type:management, family=beta_family(link="logit"), data = Prop_dir_sim2)
Anova(Prop_dire)
summary(Prop_dire)

print(p)

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

#upload and prepare dataframe
output_ind_ES<- read.csv("Data/Indirect_ES_sim_CP.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors


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
#Prop_indire_nor2<-lm (Prop_mean ~ type + management, data = Prop_indir_sim2) #best model
#Prop_indire_nor3<-lmer (Prop_mean ~ type + (1| management/services_to) + ( 1| services_to), data = Prop_indir_sim2) #best model
#Prop_indire_nor4<-lm (Prop_mean ~ type:management, data = Prop_indir_sim2) #not enough data
#Prop_indire_nor5<-lmer (Prop_mean ~ type:management + ( 1| services_to), data = Prop_indir_sim2) #not enough data

Anova(Prop_indire_nor1)
summary(Prop_indire_nor1)

AIC(Prop_indire_nor1, Prop_indire_nor2, Prop_indire_nor3, Prop_indire_nor4)

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

post_man<- emmeans(Prop_indire_nor1, ~ management)
pairs(post_man)



## beta distributon (BORRAR SI DECIDO USAR NORMAL)
Prop_indire<-glmmTMB (Prop_mean ~ type + management + ( 1| services_to), family=beta_family(link="logit"), data = Prop_indir_sim2) #best model
Prop_indire2<-glmmTMB (Prop_mean ~ type + management, family=beta_family(link="logit"), data = Prop_indir_sim2)
Prop_indire3<- glmmTMB (Prop_mean ~ type + (1| management/services_to),family=beta_family(link="logit"),data = Prop_indir_sim2)
Prop_indire4<-glmmTMB (Prop_mean ~ type:management , family=beta_family(link="logit"), data = Prop_indir_sim2) #not working
Prop_indire5<-glmmTMB (Prop_mean ~ type:management + ( 1| services_to), family=beta_family(link="logit"), data = Prop_indir_sim2) #not working
Anova(Prop_indire)
summary(Prop_indire)

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
direct_ES_emp<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",")

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
direct_ES_sim<- read.csv("Data/direct_ES_sim_CP.csv", sep =",")

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
#                   data = ratio_sim2) #the interaction is not ok according to AIC
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



