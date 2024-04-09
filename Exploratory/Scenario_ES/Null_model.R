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
#                      NULL MODEL                        
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


## -- Remove species at random (1000 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#WD
WD_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="WD")
shuff_WD<-sim_sp_removal(WD_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
WD_clean<-shuff_WD %>% ungroup() %>% mutate(habitat = 11) 

#RG
RG_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="RG")
shuff_RG<-sim_sp_removal(RG_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
RG_clean<-shuff_RG %>% ungroup() %>% mutate(habitat = 12) 

#Merge shuff habitats and arrange dataframe to merge with the rest of the habitats
shuff_habitats<-rbind(WD_clean,RG_clean) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                  node_to,ab_node_to,taxon_node_to,weight, iteration)

#write.csv(shuff_habitats,"Data/shuff_hab_WD_RG_CP.csv", row.names= FALSE) #save to add in the next management scenario

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
  

## -- Remove species at random (1000 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#MH
MH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="MH")
shuff_MH<-sim_sp_removal(MH_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
MH_clean<-shuff_MH %>% ungroup() %>% mutate(habitat = 13) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#NH
NH_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NH")
shuff_NH<-sim_sp_removal(NH_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
NH_clean<-shuff_NH %>% ungroup() %>% mutate(habitat = 14) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (M) with the precvious mangaement (SE)
shuff_pre<-read.csv("Data/shuff_hab_WD_RG_CP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,MH_clean,NH_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_M_CP.csv", row.names= FALSE) #save to add in the next management scenario

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


## -- Remove species at random (1000 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#GM
GM_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="GM")
shuff_GM<-sim_sp_removal(GM_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
GM_clean<-shuff_GM %>% ungroup() %>% mutate(habitat = 15) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#SF
SF_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="SF")
shuff_SF<-sim_sp_removal(SF_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
SF_clean<-shuff_SF %>% ungroup() %>% mutate(habitat = 16) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#PP
PP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="PP")
shuff_PP<-sim_sp_removal(PP_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
PP_clean<-shuff_PP %>% ungroup() %>% mutate(habitat = 17) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (SI) with the previous mangaement (M)
shuff_pre<-read.csv("Data/shuff_hab_M_CP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,GM_clean,SF_clean,PP_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_SI_CP.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github

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


## -- Remove species at random (1000 times)

#In each new habitat, we randomly remove the number of species according to the step 1. 

#LP
LP_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="LP")
shuff_LP<-sim_sp_removal(LP_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
LP_clean<-shuff_LP %>% ungroup() %>% mutate(habitat = 18) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#NL
NL_edge_list<-new_habitats_ab_rem %>%  filter(pre_hab =="NL")
shuff_NL<-sim_sp_removal(NL_edge_list,absent_species_count) #function to randomly remove the same number of species according to the original simulation
NL_clean<-shuff_NL %>% ungroup() %>% mutate(habitat = 19) %>% select(habitat,node_from,ab_node_from,taxon_node_from,
                                                                     node_to,ab_node_to,taxon_node_to,weight, iteration)

#Merge shuff habitats from this management scenario (I) with the precvious mangaement (SI)
shuff_pre<-read.csv("Data/shuff_hab_SI_CP.csv", sep =,) # shuff habitats from previous habitat management

shuff_habitats<-rbind(shuff_pre,LP_clean,NL_clean)  
#write.csv(shuff_habitats,"Data/shuff_hab_I_CP.csv", row.names= FALSE) #save to add in the next management scenario. Too heavy to upload on github


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

#Upload empirical
Emp<-read.csv("Data/Land_use_rat_edgelist_weighted_CP_intense.csv", sep =,) %>% 
    mutate(iteration = "Emp") %>%  select(management,iteration,node_from,node_to)

## Final Edgelist
edge_list_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim)
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

#Upload empirical
Emp<-read.csv("Data/Land_use_rat_state_nodes_CP_intense.csv", sep =,) %>% 
  mutate(iteration = "Emp") %>%  select(management,iteration,node_id,taxon,abun)

## Final state node list
state_node_sim<-rbind(Emp,SE_sim,M_sim,SI_sim,I_sim)
#write.csv(state_node_sim,"Data/state_node_sim_CP.csv", row.names= FALSE)



### HASTA ACA!!!

################## --- CALCULATE DIRECT E(D)S PROVISION AND INDIRECT EFFECT ON ES


##### --  DIRECT E(D)S PROVISION


## Add information of ES to the state_node_list (values 0-1)

nodes_ES<- right_join(state_nodes_weighted, Norwood_farm$nodes, by = "node_id")%>% 
  select(management,node_id,taxon.x,abun, "Crop production",
         "Pollination", "Crop damage", "Pest control", "Seed dispersal", "Butterfly watching", "Bird watching") %>% 
  group_by(management,node_id) %>% rename("taxon" = "taxon.x") %>% 
  gather("services","value", 5:11) #we conserve species that not directly provide ES because can serve as intermediate hop

nodes_ES$management <- factor(nodes_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

## -- Estimate the amount of direct E(D)S provision per species (weight = abundance * body mass

#upload file with body mass
body_mass<-read.csv("Data/biomass.csv",header=T)

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

#write.csv(direct_ES,"Data/Land_use_dir_weighted_CP_intense.csv", row.names= FALSE)

## -- Direct provision Ratio ES/E(D)S

# ratio > 1 indicates more benefits, ratio = 1 balance between damages and benefits and ratio < 1 more damage

number_ES<-nodes_ES %>% group_by(management) %>% 
  filter (value ==1 & services != "Crop damage") %>% summarise (ES =sum(value))#count ES provision

number_EDS<-nodes_ES %>% group_by(management) %>% 
  filter (value ==1 & services == "Crop damage") %>% summarise (EDS =sum(value))#count EDS provision

total<-cbind(number_ES,number_EDS[,-1])

ratio_direct<-total %>% mutate(ratio_direct = ES/EDS)

#write.csv(ratio_direct,"Data/Land_use_rat_dir_weighted_CP_intense.csv", row.names= FALSE)





##### --  INDIRECT EFFECT ON E(D)S 


## -- Prepare dataframe

# Full list nodes with ES in the network (considering those that provide and not provide direct ES)

list_nodes_ES_provi<-nodes_ES %>% ungroup() %>% select(-management,-abun) %>%
  filter (value ==1) %>% unique # list of nodes that provide ES ( = no plants and ectoparasites)

list_nodes_ES_no_provi<-nodes_ES %>% ungroup() %>% select(-management,-abun,-services) %>% unique() %>% 
                    group_by(node_id) %>% mutate(tot_serv = sum(value)) %>% select(-value) %>% 
                    filter(tot_serv == 0) %>% mutate(services = "None", value = 1) %>% # filter species that not directly provide any E(D)s and assign them as None
                    select(-tot_serv)


list_nodes_ES<-rbind(list_nodes_ES_provi,list_nodes_ES_no_provi) #Total list of nodes with ES (with None)


# Add attributes of nodes to the edgelist

edge_list<- left_join(land_change_weighted,list_nodes_ES, by = c("node_from"="node_id")) %>% 
  rename("taxon_from"="taxon", "services_from"="services",
         "value_from" = "value") %>% 
  left_join(list_nodes_ES, by = c("node_to"="node_id"))  %>% 
  rename("taxon_to"="taxon", "services_to"="services",
         "value_to" = "value") # %>% distinct() #remove distinct because we have more than one time the same interacition (we summed the links between layers to aggreate the data)


# Add inverted links (to make the code easier to program when calculate indirect interactions. It will not affect the results)

edge_list_inverted<- tibble(values = edge_list$node_to,edge_list$node_from, edge_list$weight,
                            edge_list$management, edge_list$taxon_to,edge_list$services_to,
                            edge_list$value_to,  edge_list$taxon_from,edge_list$services_from,edge_list$value_from)
colnames(edge_list_inverted) <- c("node_from", "node_to","weight", "management", "taxon_from", "services_from",
                                  "value_from", "taxon_to", "services_to", "value_to")


# Combine both data frame to create the final edge list

edgelist_final<- bind_rows(edge_list, edge_list_inverted) %>% 
  select(-value_to,-value_from) # we are not using these anymore

edgelist_final<-edgelist_final[,c(4,1,5,6,2,7,8,3)]



#### - Calculate indirect effects considering 1 hop (node - node)


# Create objects to store

management = c()
services_from = c()
node_from= c()
node_to = c()
taxon_from = c()
services_to = c()
weight = c() #the weight is equal to weight of links (products of relative abundances)

# Reorder the dataframe to check indirect interaction in 1 hop
for (i in 1:nrow(edgelist_final)){
  
  management = c(management, edgelist_final$management[i])
  services_from = c(services_from,edgelist_final$services_from[i])
  node_from = c(node_from,edgelist_final$node_from[i])
  node_to = c(node_to,edgelist_final$node_to[i])
  taxon_from = c(taxon_from, edgelist_final$taxon_from[i])
  services_to = c(services_to, edgelist_final$services_to[i])
  weight = c(weight, edgelist_final$weight[i])
  
}

Indirect_1hop_landuse_weighted<-data.frame(management,services_from,node_from,node_to,taxon_from,
                                           services_to,weight,hop = rep(1, length(services_from)), 
                                           type = rep("I", length(services_from))) 



# Rearrange the output

# we remove duplicates rows where node_from = birds or butterflies cause they represent the same interaction. 
# This happens because each row represents an attribute and these taxons have 2 and 3 attributes per node.

rows_birds_butt<- Indirect_1hop_landuse_weighted %>%
  filter(taxon_from == "Butterfly" | taxon_from == "Seed-feeding bird") %>%
  distinct(management, node_from, node_to, .keep_all = TRUE) # new subset after eliminating duplicate rows for node_from = birds and butterflies

int_without<-Indirect_1hop_landuse_weighted %>% filter(!(taxon_from == "Butterfly" | 
                                                         taxon_from == "Seed-feeding bird")) #eliminate the interactions containing node_from =birds or butterflies from the original dataframe


Indirect_1hop_landuse_weighted_2<-rbind(rows_birds_butt,int_without)#final dataframe containing indirect effects on ES via 1 hop


#write.csv(Indirect_1hop_landuse_weighted_2,"Data/Land_use_ind_1hop_weighted_CP_intense.csv", row.names= FALSE)



#### - Calculate indirect effects considering 2 hops (node 1 - node 2 - node 3, effect of node 1 on node 3'E(D)S via node 2)

Indirect_1hop<-read.csv("Data/Land_use_ind_1hop_weighted_CP_intense.csv",
                        sep =",") #load dataframe of indirect effects using 1 hop


## Add relative abundances of node 3 (to calculate the weight of second order interactions)

# Calculate relative abundances in the state_node

state_node_weighted_abundances<- state_nodes_weighted %>% 
  group_by(management,taxon) %>% mutate(tot_ab_taxon = sum(abun)) %>% #total abundance of each taxon
  group_by(management, node_id) %>% mutate(rel_ab = abun /tot_ab_taxon) #calculate relative abundances
 
# add relative abundances of node 3 to the dataframe

Indirect_1hop_ab_to<-Indirect_1hop %>% 
  left_join(state_node_weighted_abundances, by = c("management", "node_to" = "node_id")) %>%  #incorporate rel abundances of node_to (node 3)
  rename("rel_abun_to" = "rel_ab") %>%  #add abundances of node_to (node 3)
  select(-abun,-taxon,-tot_ab_taxon)

Indirect_1hop<-Indirect_1hop_ab_to


# Calculate indirect effect on ES

# Create empty vectors
management = c()
node_id= c()
node_int = c()
taxon_from = c()
services = c()
node_to = c()
services_to = c()
weight_to=c() #is equal to the product between relative a abundances of the three species


# Iterate to each row
for (i in 1:nrow(Indirect_1hop)){ #each row represents interaction between species
  
  j = Indirect_1hop$node_to[i] # check the node_to (intermediate species: node 2 in the title)
  l = Indirect_1hop$management [i] #check management where the target species for which we are detecting indirect effects on ES
  w = Indirect_1hop$weight [i] #weight of first indirect order interactions (product of abundances between node 1 and 2)
  
  
  # Filter dataframe (filter node 3's ES affected by node 2)
  
  services_int <- Indirect_1hop %>% filter(node_from == j, #filter to show node 2
                                           node_to != Indirect_1hop$node_from[i], #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
                                           management== l) %>% mutate(weight_to = weight *rel_abun_to) %>% #final weight of indirect effect
                                                select(node_to,services_to, weight_to)#select node 3 and its ES
  # Storage the results
  
  services_to <- c(services_to, unlist(services_int$services_to)) # add node 3 ES
  weight_to<-c(weight_to, unlist(services_int$weight_to))
  node_to <- c(node_to, unlist(services_int$node_to))# add identity of node 3
  node_int <- c(node_int,rep(j, nrow(services_int)))
  node_id <- c(node_id,rep(Indirect_1hop$node_from[i], nrow(services_int)))# add target (node 1) for which we are detected indirect effects 
  taxon_from<- c(taxon_from, rep(Indirect_1hop$taxon_from[i], nrow(services_int)))
  services <- c(services, rep (Indirect_1hop$services_from[i], nrow(services_int)))# direct ES provided by the target node
  management <- c(management, rep(l, nrow(services_int)))

}

Indirect_2hop<- data.frame(management,node_id,taxon_from,services,node_int,
                           node_to, services_to,weight_to,type = rep("I", length(services_to)), hop = rep(2,length(services_to)))


#write.csv(Indirect_2hop,"Data/Land_use_ind_2hop_weighted_CP_intense.csv", row.names= FALSE)

Indirect_2hop<-read.csv("Data/Land_use_ind_2hop_weighted_CP_intense.csv",
                        sep =",") #load dataframe of indirect effects using 2 hops


# Join both 1 and 2 hops indirect effects dataframes

# 1 hop
Indirect_1hop_m<-Indirect_1hop  %>% select(-rel_abun_to) %>%  rename("services" ="services_from",
                                           "node_id" = "node_from",
                                           "taxon" = "taxon_from") %>% 
  mutate(node_int = NA)

Indirect_1hop_m<-Indirect_1hop_m[,c(1,2,3,5,10,4,6,7,8,9)]

# 2 hop
Indirect_2hop_m<-Indirect_2hop %>% rename("taxon" = "taxon_from",
                                          "weight" = "weight_to") 
Indirect_2hop_m<-Indirect_2hop_m[,c(1,4,2,3,5,6,7,8,9,10)]

#  Total Indirect effect of ES

I_ES<- rbind(Indirect_1hop_m,Indirect_2hop_m)

#write.csv(I_ES,"Data/Land_use_ind_weighted_CP_intense.csv")


################## --- ESTIMATE OUTPUT OF INDIRECT EFFECTS 

# For each interaction we assigned the following outputs:
# + :provide ES, increase ES or decrease crop damage
# - : provide EDS (crop damage), decrease ES provision or increase crop damage


## -- Define vector of trophic groups to state the conditions

plants = 1:93
crops = 94:99
flow_vis = 100:336
aphid = 337:364
pri_par = 365:375
sec_par = 376:382
leaf_par = 383:475
seed_ins = 476:494
seed_bird = 495:506
seed_rod = 507:510
butt = 511:526
seed_ins_par = 527:543
rod_par = 544:551

## --1 HOP 

hop_1 <- read.csv("Data/Land_use_ind_weighted_CP_intense.csv", sep =",", row.names = 1) %>% 
  filter(hop == 1, services_to !="None") #remove indirect effect that not affect any ES

# We assigned the indirect output according to node_id and node_to

output_ES_1hop<- hop_1 %>%  #Write only the potential negatives and the rest will be assign as positive
  mutate(output = case_when(
    (node_id %in% aphid| node_id %in% seed_bird | node_id %in% seed_ins |
       node_id %in% seed_rod) & services_to == "Crop production"  ~ "-", # if aphids, birds, rodents, seed inds interact with crop, they will reduce crop production
    (node_id %in% plants |node_id %in% crops)  & (node_to %in% aphid| node_to %in% seed_bird | node_to %in% seed_ins |
                             node_to %in% seed_rod) & services_to == "Crop damage"  ~ "-", #plants and crops that increase pest hebirovores' abundance (birds, rodents, insects, aphids which feeds on crop, will increase crop damage
    
    TRUE ~ "+"
    
  ))

# remove from the data set those flower visitors that not provide pollination to crops
output_ES_1hop_fin<- output_ES_1hop %>% filter(!(services == "None" & taxon == "Flower-visiting"))


## -- 2 HOPS  

hop_2 <- read.csv("Data/Land_use_ind_weighted_CP_intense.csv", sep =",",row.names = 1) %>% 
  filter(hop == 2, services_to !="None") #remove indirect effect that not affect any ES


# We assigned the indirect output according from "node_id" to "node_to" via "node_int"

output_ES_2hops<- hop_2 %>%  
  mutate(output = case_when(  #Write only the potential negatives and the rest will be assign as positive
    
    
    (node_id %in% plants | node_id %in% crops) &  (node_int%in%aphid | node_int%in%seed_bird| 
                                                     node_int%in%seed_ins | node_int%in%seed_rod) &
      services_to == "Crop production"  ~ "-",     #plants,crops --> + seed predators --> - crop --> - crop production
    
    
    (node_id%in%flow_vis | node_id%in%butt) & !(services == "None") & (node_int%in%plants| node_int%in%crops) &
      (services_to == "Crop damage")  ~ "-", # flower visitors and butt --> + plants,crops--> + pop seed predators --> + crop damage 
    
    (node_id%in%flow_vis) & (services == "None") & (node_int%in%plants| node_int%in%crops) &
      !(services_to == "Crop damage")  ~ "-",
    
    (node_id%in%flow_vis) & (services == "None") &
      (services_to == "Crop damage")  ~ "+", # flowe visitors that not provide poll --> - plants,crops -- > - pest abundance --> - crop damage
    
    
    (node_id%in%aphid | node_id%in%seed_bird| node_id%in%seed_ins |node_id%in%seed_rod) &
      (node_int%in%plants| node_int%in%crops) & (services_to == "Pollination" | 
                                                   services_to == "Butterfly watching"| services_to == "Bird watching"| services_to == "Pest control"| 
                                                   services_to == "Seed dispersal") ~ "-",     # seed predators --> - pop crops/plants --> - pop birds, flower vis, parasitodes --> - pest control, watching, pollination
    
   
    (node_id%in%leaf_par) & (node_int%in%plants| node_int%in%crops) & 
      (services_to == "Crop damage") ~ "-",  # + leaf miner parasitoid -->+ pop plants --> + pest --> + crop damage
    
  
    
    (node_id%in%pri_par| node_id%in%sec_par |node_id%in%leaf_par |node_id%in%rod_par |
       node_id%in%seed_ins_par) & (node_int%in%aphid | node_int%in%seed_bird| 
                                     node_int%in%seed_ins | node_int%in%seed_rod) &
      services_to == "Pest control" ~ "-", # par --> + pop seed predators --> + pop parasitoide --> - pest control
    
    TRUE ~ "+"
  ))



### -- Final dataframe output of indirect effects  ---

output_ES<-rbind(output_ES_1hop_fin,output_ES_2hops)


#write.csv(output_ES,"Data/Land_use_output_weighted_CP_intense.csv", row.names= FALSE)

##### -- Indirect provision Ratio output +/-
output_ind_ES <- read.csv("Data/Land_use_output_weighted_CP_intense.csv", sep =",") 

# ratio > 1 indicates more benefits, ratio = 1 balance between damages and benefits and ratio < 1 more damage

number_positive<-output_ind_ES %>% group_by(management) %>% 
  filter (output == "+") %>% summarise (positive = n())#count + outputs

number_negative<-output_ind_ES %>% group_by(management) %>% 
  filter (output == "-") %>% summarise (negative = n())#count - outputs

total<-cbind(number_positive,number_negative[,-1])

ratio_indirect<-total %>% mutate(ratio_direct = positive/negative)

ratio_indirect$management <- factor(ratio_indirect$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Analyses                            
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Proportion of direct E(D)S retained across land use change --

#upload and prepare dataframe
direct_ES<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",") %>% 
  mutate(services = case_when(services == "Crop production"~ "Resource provision",
                              TRUE~services))

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

Prop_dir<-direct_ES %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  dplyr::select(management,services,tot,prop) %>%
  unique()


# adjust the extreme values according to the beta conditions
Prop_dir$prop <- ifelse(Prop_dir$prop == 0, 0.000001, 
                        ifelse(Prop_dir$prop == 1, 0.9999999, Prop_dir$prop))

# Model
library("glmmTMB")
library("stats4")
library("bbmle")
library(emmeans)
library(car)

Prop_dire<- glmmTMB (prop ~ management + services, family=beta_family(link="logit"), data = Prop_dir) # model that best fit
#Prop_dire2<-glmmTMB (prop ~ management + ( 1| services), family=beta_family(link="logit"), data = Prop_dir)
Anova(Prop_dire)
summary(Prop_dire)


#Homogeneity
EM<-resid(Prop_dire, type= "response") 
FM<-fitted(Prop_dire) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(Prop_dire, type= "response") 
boxplot(E1_lme ~ management, data = Prop_dir, main = "Management")

# posthoc ()
post_dir<- emmeans(Prop_dire, ~ management)
pairs(post_dir)

post_ser<- emmeans(Prop_dire, ~ services)
pairs(post_ser)

#### Proportion of indirect effect on E(D)S retained across land use change --

#upload and prepare dataframe
output_ind_ES <- read.csv("Data/Land_use_output_weighted_CP_intense.csv", sep =",") 
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

Prop_ind<-output_ind_ES %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  dplyr::select(management,services_to,tot,prop) %>% unique()


# adjust the extreme values according to the beta conditions
Prop_ind$prop <- ifelse(Prop_ind$prop == 0, 0.000001, 
                        ifelse(Prop_ind$prop == 1, 0.9999999, Prop_ind$prop))

# Model
library("glmmTMB")
library("stats4")
library("bbmle")

Prop_indi<-glmmTMB (prop ~ management + services_to, family=beta_family(link="logit"), data = Prop_ind) #model that best fit
#Prop_indi_2<-glmmTMB (prop ~ management :services_to, family=beta_family(link="logit"), data = Prop_ind)
Anova(Prop_indi)
summary(Prop_indi)

#Homogeneity
EM<-resid(Prop_indi, type= "response") 
FM<-fitted(Prop_indi) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(Prop_indi, type= "response") 
boxplot(E1_lme ~ management, data = Prop_ind, main = "Management")

# posthoc
post_ind<- emmeans(Prop_indi, ~ management)
pairs(post_ind)
post_ser<- emmeans(Prop_indi, ~ services_to)
pairs(post_ser)




#### Change in the amount of direct E(D)S provided change across land use change --

#Amount of ES provided by each species
extensive_amount <- direct_ES %>% 
  filter(management == "E") %>%
  select(node_id, services, weight)


# Merging and other managements and calculate ratio of change
dir_amount <- direct_ES %>% 
  left_join(extensive_amount, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = weight / weight_extensive) #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S

# Model
library(glmmTMB)
m_amount<- glmmTMB(ratio_change ~ management:services + (1|node_id),family = Gamma(link = "log"),
                   data = dir_amount) 
Anova(m_amount)
summary(m_amount)


#Homogeneity
EM<-resid(m_amount, type= "response") 
FM<-fitted(m_amount) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(m_amount, type= "response") 
boxplot(E1_lme~dir_amount$management, main="Management")

# posthoc
emms <- emmeans(m_amount, pairwise ~ management | services)
contrast <- contrast(emms, method = "pairwise")  # pairwise comparisons
summary(contrast)
contrast


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Plots                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#direct
direct_ES<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",") %>% 
            mutate(services = case_when(services == "Crop production"~ "Resource provision",
                                 TRUE~services))

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


#indirect
output_ind_ES <- read.csv("Data/Land_use_output_weighted_CP_intense.csv", sep =",") %>% 
                  mutate(services_to = case_when(services_to == "Crop production"~ "Resource provision",
                              TRUE~services_to))

output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors



### Plot of number of direct and indirect int


#direct
Number_direct<-direct_ES %>% group_by(management) %>% summarise(Number = n())


Number_dir<-Number_direct%>%   
  ggplot(aes(y=Number, x=management)) + 
  geom_bar(position="stack", stat="identity")+ ggtitle("Direct")+
  labs(x='Management', y="Number of direct E(D)S provision") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=11, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank())

Number_dir

#indirect

Number_indirect<-output_ind_ES %>% group_by(management) %>% 
  summarise(Number = n())

Number_indir<-Number_indirect%>%   
  ggplot(aes(y=Number, x=management)) + 
  geom_bar(position="stack", stat="identity")+ ggtitle("Indirect")+
  labs(x='Management', y="Number of indirect effects on E(D)S") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=11, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank())

Number_indir

upper_row<- plot_grid(Number_dir,Number_indir ,
                      ncol = 2)
upper_row

#ggsave("land_use_Number_CP_intense.png")


### Plot of proportions of richness direct and indirect E(D)S retained 


#direct 
Prop<-direct_ES %>% group_by(management,services) %>% 
                    mutate(tot = n()) %>% ungroup() %>%  
                  group_by(services) %>% 
                mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
          select(management,services,tot,prop) %>% unique()

prop_EDS_direct<- Prop %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services)), size = 4, show.legend = T) +
  # scale_fill_manual(values = col) + 
  scale_fill_brewer(palette="PRGn") +
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

ggsave("Land_use_retained_direct_CP_intense.png")
#indirect

Prop_ind<-output_ind_ES %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  select(management,services_to,tot,prop) %>% unique()

prop_EDS_indirect<- Prop_ind %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services_to)), size = 4, show.legend = T) +
  # scale_fill_manual(values = col) + 
  scale_fill_brewer(palette="PRGn") +
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

ggsave("Land_use_retained_indirect_CP_intense.png")




### Plot of output according to direct and indirect 

#direct

ratio_direct<-read.csv("Data/Land_use_rat_dir_weighted_CP_intense.csv")

ratio_direct$management <- factor(ratio_direct$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


ratio_direct<-ratio_direct%>% gather("type","value", 2:3) %>% group_by(management) %>% 
  mutate(Total = sum(value)) %>% group_by(management,type) %>% 
  summarise(prop = value /Total) %>%  
  ggplot(aes(y=prop, x=management, fill = type)) + 
  geom_bar(position="stack", stat="identity")+ ggtitle("Direct provision")+
  labs(x='Management', y="Prop of E(D)S") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")

ratio_direct


#indirect

number_positive<-output_ind_ES %>% group_by(management) %>% 
  filter (output == "+") %>% summarise (positive = n())#count + outputs

number_negative<-output_ind_ES %>% group_by(management) %>% 
  filter (output == "-") %>% summarise (negative = n())#count - outputs

total<-cbind(number_positive,number_negative[,-1])

ratio_indirect<-total %>% mutate(ratio_direct = positive/negative)

ratio_indirect$management <- factor(ratio_indirect$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

ratio_indirect<-ratio_indirect%>% gather("type","value", 2:3) %>% group_by(management) %>% 
  mutate(Total = sum(value)) %>% group_by(management,type) %>% 
  summarise(prop = value /Total) %>%  
  ggplot(aes(y=prop, x=management, fill = type)) + 
  geom_bar(position="stack", stat="identity")+ ggtitle("Indirect effects")+
  labs(x='Management', y="Prop of output (+/-)") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")

ratio_indirect

upper_row<- plot_grid(ratio_direct,ratio_indirect ,
                      ncol = 2)
upper_row

#ggsave("Land_use_output_CP_intense.png")





### Prop Amount of direct E(D)S

tot_services_emp<-direct_ES %>% filter(management=="E") %>% group_by(management,services) %>% 
                  summarize(tot_empirical = sum(weight))


Prop_weight<-direct_ES %>% group_by(management,services) %>% 
  summarize(tot = sum(weight)) %>% ungroup() %>%  
  mutate(Extensive_tot = case_when(
    services == "Bird watching"~ 330890.9200,
    services == "Butterfly watching"~ 244.7676,
    services == "Crop damage"~ 645963.6269,
    services == "Resource provision"~ 209300.0000,
    services == "Pest control"~ 7108.3108,
    services == "Pollination"~ 36736.7426,
    services == "Seed dispersal"~ 305215.3300),
    ratio_change = tot / Extensive_tot  #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
  )
  
  
  
prop_weight_direct<- Prop_weight %>% ggplot(aes(x = management, y = ratio_change)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services)), size = 4, show.legend = T) +
  # scale_fill_manual(values = col) + 
  scale_fill_brewer(palette="PRGn") +
  scale_y_continuous(name = "Change in the amount of direct E(D)S provided ", limits = c(0, 2.8)) + 
  scale_x_discrete(name = "Management")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major=element_line(color = "gray"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=13, color='black'),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

prop_weight_direct

#ggsave("Land_use_weight_CP_intense.png")


### Plot of proportion of each direct E(D)S per management (also indicating if it's a services and disservices)

# Direct
D_ES<-direct_ES %>% group_by(management) %>% 
  mutate(Total = n()) %>% group_by(management,services) %>% 
  summarise(Number = n(), Prop = Number /Total) %>% unique()


Direct_ES_management<- D_ES  %>%  
  ggplot(aes(y=Prop, x= management, fill = services)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_brewer(palette="PRGn") +
  ggtitle("Direct provision")+
  labs(x='Management', y="Prop E(D)S per management") +theme_bw()+
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
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom") 

Direct_ES_management
#ggsave("Prop_ES_direct_CP.png")


### Plot of proportion of each indirect effects on E(D)S per management

# Direct
I_ES<-output_ind_ES %>% group_by(management) %>% 
  mutate(Total = n()) %>% group_by(management,services_to) %>% 
  summarise(Number = n(), Prop = Number /Total) %>% unique()


Indirect_ES_management<- I_ES  %>%  
  ggplot(aes(y=Prop, x= management, fill = services_to)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_brewer(palette="PRGn") +
  ggtitle("InDirect provision")+
  labs(x='Management', y="Prop indirect effects on E(D)S per management") +theme_bw()+
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
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom") 

Indirect_ES_management
#ggsave("Prop_ES_indirect_CP.png")





## Direct and indirect two hops (- and +)


# Direct
D_taxon_output<-direct_ES %>% group_by(management,output) %>% 
  mutate(Total = n()) %>% group_by(management,output, taxon) %>% 
  summarise(Number = n(), Prop = Number /Total) %>% unique()


# positive
D_taxon_positive<- D_taxon_output %>% filter(output == "+")
Direct_taxon_positive<- D_taxon_positive  %>%  
  ggplot(aes(y=Prop, x= management, fill = taxon)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Butterfly", "Crop","Flower-visitor ins",
                               "Leaf-miner par", 
                              "Prim aphid par","Rodent ectoparasite","Sec aphid par", "Seed-feeding bird", 
                              "Seed-feeding ins","Seed-feeding rod"), 
                    values = c("#E18A00",  "#BE9C00", "#8CAB00", "#00BE70","#00BBDA", "#00ACFC",
                       "#8B93FF",  "#D575FE","#F962DD", "#FF65AC" ))+
  ggtitle("Direct provision")+
  labs(x='Output', y="Prop output provided per taxon") +theme_bw()+
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
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")
        
Direct_taxon_positive
ggsave("output_taxon_direct_positive_CP.png")

# negative
D_taxon_negative<- D_taxon_output %>% filter(output == "-")
Direct_taxon_negative<- D_taxon_negative  %>%  
  ggplot(aes(y=Prop, x= management, fill = taxon)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Aphid",  "Seed-feeding bird", 
                              "Seed-feeding rod"), 
                    values = c("#F8766D", "#D575FE", "#FF65AC" ))+
  ggtitle("Direct provision")+
  labs(x='Output', y="Prop output provided per taxon") +theme_bw()+
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
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")
Direct_taxon_negative
#ggsave("output_taxon_direct_negative_CP.png")


# Indirect
I_taxon_output<-output_ind_ES %>% group_by(management,output) %>% 
  mutate(Total = n()) %>% group_by(management,output, taxon) %>% 
  summarise(Number = n(), Prop = Number /Total) %>% unique()

#Positive
I_taxon_positive<- I_taxon_output %>% filter(output == "+")

Indirect_taxon_positive<- I_taxon_positive%>% 
  ggplot(aes(y=Prop, x=management, fill = taxon)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Aph","Butt", "Crop","Flower-visitor ins","Insect seed-feeder par", 
                              "Leaf-miner par", "Plants","Prim aphid par", "Rodent ectopar",
                              "Sec aphid par", "Seed-feeding bird", "Seed-feeding ins",
                              "Seed-feeding rod"),
                    values = c("#F8766D", "#E18A00","#BE9C00", "#8CAB00",
                               "#24B700", "#00BE70","#00C1AB", "#00BBDA", "#00ACFC",
                               "#8B93FF", "#D575FE","#F962DD", "#FF65AC"))+
  ggtitle("Indirect provision")+
  labs(x='Output', y="Prop output provided per taxon") +theme_bw()+
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
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")

Indirect_taxon_positive
#ggsave("output_taxon_indirect_positive_CP.png")


#Negative
I_taxon_negative<- I_taxon_output %>% filter(output == "-")

Indirect_taxon_negative<- I_taxon_negative %>% 
  ggplot(aes(y=Prop, x=management, fill = taxon)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Aph","Butt", "Crop","Flower-visitor ins","Insect seed-feeder par", 
                              "Leaf-miner par", "Plants","Prim aphid par", "Rodent ectopar",
                              "Sec aphid par", "Seed-feeding bird", "Seed-feeding ins",
                              "Seed-feeding rod"),
                    values = c("#F8766D", "#E18A00","#BE9C00", "#8CAB00",
                               "#24B700", "#00BE70","#00C1AB", "#00BBDA", "#00ACFC",
                               "#8B93FF", "#D575FE","#F962DD", "#FF65AC"))+
  ggtitle("Indirect provision")+
  labs(x='Output', y="Prop output provided per taxon") +theme_bw()+
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
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")

Indirect_taxon_negative

#ggsave("output_taxon_indirect_negative_CP.png")
