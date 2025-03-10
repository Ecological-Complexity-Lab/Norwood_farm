# In this code, we simulate land conversion, creating different land management scenarios (see list in the main manuscript)

# To create the different management scenarios we:

# 1) Assign abundances as state nodes attributes of the extensive Norwood farm (nodes’ abundances per each habitat)

# 2) Change the habitats to “CP” but modifying the abundance of each species 
#according to the area (removing population below 1 individual)

# 3) Aggregate the habitats to create the Norwood farm network. During this step, 
# we pooled the abundances of the same species across habitats. 

# We then estimate the direct and indirect ecosystem service (ES) provision for each land management scenario and calculate
# the proportion of direct ES provision retained, the proportion of indirect effects on ES provision, and the relative change in 
# direct ES provision after converting an extensive farm into each scenario. Finally, we conduct statistical analyses to 
# assess these changes.

## -- Load libraries --------------------------------------------------------------------------------------------------------
library(emln) # multilayer package
library(readr)
library(ggplot2)
library(tidyverse)

## -- get_data--------------------------------------------------------------------------------------------------------
setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")


######### --- Upload multilayer network
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   LAND-USE CHANGE SIMULATION                    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


########## -- Rearrange dataframe to include species abundance and habitats' area

## Add species abundances per habitat (as state nodes attributes) 
abundances<-read.csv("Data/species_abundances.csv",header=T) #call abundances

state_nodes_ab<-Norwood_farm$state_nodes %>% left_join(abundances, 
                                            by = c("layer_name" = "habitat",
                                                   "node_name" = "species_name")) %>% #add abundances
  left_join(Norwood_farm$nodes, by = "node_id") %>% 
  select(layer_id,node_id,abundance, taxon) ##add taxon


## Call dataframe of habitats' area
areas<-read.csv("Data/Raw_data/habitatarea.csv", sep =",") %>% # LU as CP already merged
  filter(HabitatCode != "ST") %>% #remove standing trees
  mutate(HabitatCode = case_when(HabitatCode == "C"~ "CP",
                                 HabitatCode == "WU"~ "WD",
                                 TRUE~HabitatCode))

habitat_area <- areas %>% mutate(area_ave = case_when(
                          (Area_2007 >0) & (Area_2008 >0) ~ (Area_2007+Area_2008)/2, #if the same habitat was present in both years do the average
                          (Area_2007 >0) & (Area_2008  ==0)~ Area_2007, #if the habitat was present in one year, keep the area of the year
                          (Area_2007 ==0) & (Area_2008  >0)~ Area_2008))
                      



########## -- Create land management scenarios
Norwood_farm$extended_ids<- select(Norwood_farm$extended_ids,-weight) #remove weight (dummy variable) used in previous version

##### -- Extensive

extensive_edgelist<- Norwood_farm$extended_ids %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>% 
  mutate(management = "E") %>% select(-habitat) %>% unique() #aggregate network

 
# estimate abundances of species in the aggregated network
ab_ext<-state_nodes_ab %>% select(-layer_id) %>% group_by(node_id)%>%
  mutate(abun = sum(abundance)) %>% distinct(abun,taxon)
  

##### -- Semi-extensive (replace "WD" and "RG" for "CP")


## -- Remove habitats from Norwood (the ones to replace) and incorporate abundances and taxon 
sem_ext_edgelist_rem<- Norwood_farm$extended_ids %>% filter(layer_from != 8 & layer_from != 10) %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   #links from "WD" and "RG" removed
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
 rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

sem_ext_edgelist_rem<-sem_ext_edgelist_rem[,c(1,2,5,4,3,7,6)]


## --  Create new habitats 

# create habitats CP to replace WD and RG
WD_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
   mutate (new_hab = 11, prev_hab = "WD", hab_cp = "CP")#links from "CP" to add as new habitat (12)

RG_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 12, prev_hab = "RG", hab_cp = "CP")#links from "CP" to add as new habitat (13)

# calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP) %>% left_join(habitat_area, 
                                  by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_CP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_CP)) %>% #multiplied abundances of CP for this value (to estimate according to the new habitat)
  select(-prev_hab,-hab_cp,-area_prev_hab,-area_CP)#clean dataframe


# add abundances and modify it according to the new area
abundances_CP<-state_nodes_ab %>% filter(layer_id ==1)#filter species abundances to show just layer CP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_CP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_CP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_CP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_CP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_CP * mult_ab, 
         ab_node_to = ab_node_to_CP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interaction where a partner have less than 1 indidivual (threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)

## -- create dataframe indicating node id of species in the new habitat

# species in the new habitat
sp_WD_RG <- new_habitats_ab_rem %>%select(habitat,node_from,node_to) %>% group_by(habitat) %>% 
            gather("type","node_id",2:3) %>% select(habitat, node_id) %>% unique() %>% 
            mutate(habitat = case_when (
              habitat == 11 ~ "WD",
              habitat == 12 ~ "RG"
            ))

## -- create state_node_list of the management scenario
sem_ext_edgelist_no_aggr<- rbind(sem_ext_edgelist_rem,new_habitats_ab_rem)  #join new habitats and old habitats

#node_from       
state_node_sem_ext_from<- sem_ext_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                        taxon_node_from) %>% 
                        rename("node_id" ="node_from", "abundances" = "ab_node_from",
                               "taxon" = "taxon_node_from") %>% 
        group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat

#node_to
state_node_sem_ext_to<- sem_ext_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                        taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to") %>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat


# final state nodes (calculate abundance of species)
state_node_sem_ext_agg<-rbind(state_node_sem_ext_from, state_node_sem_ext_to) %>% ungroup() %>% 
                    select(-habitat) %>% group_by(node_id) %>% 
                    mutate(abun = sum(abundances)) %>% distinct(abun,taxon)


## --  Create edge list of new management scenario
sem_ext_edgelist_aggr<-sem_ext_edgelist_no_aggr %>% select(node_from,node_to) %>% 
                      unique() %>%  #aggregated edge list
  mutate(management = "SE") 



##### -- Moderate (replace "WD","RG","MH"and "NH" for "CP")


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

mod_edgelist_rem<-mod_edgelist_rem[,c(1,2,5,4,3,7,6)]


## --  Create new habitats 

# create habitats CP to replace WD,RG, MH and NH (WD and RG were created before)
MH_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 13, prev_hab = "MH", hab_cp = "CP")#links from "CP" to add as new habitat (14)

NH_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 14, prev_hab = "NH", hab_cp = "CP")#links from "CP" to add as new habitat (15)

# calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP, MH_CP, NH_CP) %>% left_join(habitat_area, 
                                                  by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_CP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_CP)) %>% #multplied abundances of CP for this value (to estimate according to the new habitat)
  select(-prev_hab,-hab_cp,-area_prev_hab,-area_CP)#clean dataframe


# add abundances and modify them according to the new area
abundances_CP<-state_nodes_ab %>% filter(layer_id ==1)#filter species abundances to show just layer CP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_CP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_CP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_CP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_CP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_CP * mult_ab, 
         ab_node_to = ab_node_to_CP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interactio where one partner have less than 1 indidivual (threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)


## -- create dataframe indicating node id of species in the new habitat

#species in the new habitat
sp_MH_NH <- new_habitats_ab_rem %>%select(habitat,node_from,node_to) %>% group_by(habitat) %>% 
  gather("type","node_id",2:3) %>% select(habitat, node_id) %>% unique() %>% 
  mutate(habitat = case_when (
    habitat == 13 ~ "MH",
    habitat == 14 ~ "NH"
  ))


## -- create state_node_list of the management scenario
mod_edgelist_no_aggr<- rbind(mod_edgelist_rem,new_habitats_ab_rem)  #join new habitats and old habitats

#node_from       
state_node_mod_from<- mod_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                              taxon_node_from) %>% 
  rename("node_id" ="node_from", "abundances" = "ab_node_from",
         "taxon" = "taxon_node_from") %>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat

#node_to
state_node_mod_to<- mod_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                            taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to") %>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat

# final state nodes (calculate abundance of species)
state_node_mod_agg<-rbind(state_node_mod_from, state_node_mod_to) %>% ungroup() %>% 
  select(-habitat) %>% group_by(node_id) %>% 
  mutate(abun = sum(abundances)) %>% distinct(abun,taxon)



## --  Create edge list of new management scenario
mod_edgelist_aggr<-mod_edgelist_no_aggr %>% select(node_from,node_to) %>% 
  unique() %>%  #aggregated edge list
  mutate(management = "M") 



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


# --  Create new habitats 

# create habitats CP to replace WD,RG, MH,NH and GM (WD,RG,MH and NH were created before)
GM_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 15, prev_hab = "GM", hab_cp = "CP")#links from "CP" to add as new habitat (16)

SF_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 16, prev_hab = "SF", hab_cp = "CP")#links from "CP" to add as new habitat (17)

PP_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 17, prev_hab = "PP", hab_cp = "CP")#links from "CP" to add as new habitat (18)


# calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP, MH_CP, NH_CP, GM_CP,SF_CP,PP_CP) %>%
  left_join(habitat_area,  by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_CP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_CP)) %>% #multplied abundances of CP for this value (to estimate according to the new habitat)
  select(-prev_hab,-hab_cp,-area_prev_hab,-area_CP)#clean dataframe


# add abundances and modify them according to the new area
abundances_CP<-state_nodes_ab %>% filter(layer_id ==1)#filter species abundances to show just layer CP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_CP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_CP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_CP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_CP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_CP * mult_ab, 
         ab_node_to = ab_node_to_CP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interaction where one partner have less than 1 indidivual (threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)


## -- create dataframe indicating node id of species in the new habitat

#species in the new habitat
sp_GM_SF_PP <- new_habitats_ab_rem %>%select(habitat,node_from,node_to) %>% group_by(habitat) %>% 
  gather("type","node_id",2:3) %>% select(habitat, node_id) %>% unique() %>% 
  mutate(habitat = case_when (
    habitat == 15 ~ "GM",
    habitat == 16 ~ "SF",
    habitat == 17 ~ "PP"
  ))



## -- create state_node_list of the management scenario
sem_int_edgelist_no_aggr<- rbind(sem_int_edgelist_rem,new_habitats_ab_rem)  #join new habitats and old habitats

#node_from       
state_node_sem_int_from<- sem_int_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                      taxon_node_from) %>% 
  rename("node_id" ="node_from", "abundances" = "ab_node_from",
         "taxon" = "taxon_node_from")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat

#node_to
state_node_sem_int_to<- sem_int_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                    taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat


# final state nodes (calculate abundance of species)

state_node_sem_int_agg<-rbind(state_node_sem_int_from, state_node_sem_int_to) %>% ungroup() %>% 
  select(-habitat) %>% group_by(node_id) %>% 
  mutate(abun = sum(abundances)) %>% distinct(abun,taxon)


## --  Create edge list of new management scenario
sem_int_edgelist_aggr<-sem_int_edgelist_no_aggr %>% select(node_from,node_to) %>% 
  unique() %>%  #aggregated edge list
  mutate(management = "SI")  




##### -- Intensive organic (replace "WD","RG","MH","NH","GM","SF", "PP", "LP", and"NL"for "CP")

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



# --  Create new habitats 

# create habitats CP to replace LP,LU and NL (the rest were created before)
LP_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 18, prev_hab = "LP", hab_cp = "CP")#links from "CP" to add as new habitat (19)

NL_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 19, prev_hab = "NL", hab_cp = "CP")#links from "CP" to add as new habitat (21)

# calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP, MH_CP, NH_CP, GM_CP,SF_CP,PP_CP, LP_CP,NL_CP) %>%
  left_join(habitat_area,   by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_CP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_CP)) %>% #multplied abundances of CP for this value (to estimate according to the new habitat)
  select(-prev_hab,-hab_cp,-area_prev_hab,-area_CP)#clean dataframe


# add abundances and modify them according to the new area
abundances_CP<-state_nodes_ab %>% filter(layer_id ==1)#filter species abundances to show just layer CP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_CP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_CP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_CP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_CP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_CP * mult_ab, 
         ab_node_to = ab_node_to_CP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interaction where one partner have less than 1 indidivual (threshold)
new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)


## -- create vector indicating species lost  of each trophic group when transforming to CP

#species in the new habitat
sp_LP_NL <- new_habitats_ab_rem %>%select(habitat,node_from,node_to) %>% group_by(habitat) %>% 
  gather("type","node_id",2:3) %>% select(habitat, node_id) %>% unique() %>% 
  mutate(habitat = case_when (
    habitat == 18 ~ "LP",
    habitat == 19 ~ "NL"
  ))


## -- create state_node_list of the management scenario
int_edgelist_no_aggr<- rbind(int_edgelist_rem,new_habitats_ab_rem)  #join new habitats and old habitats

#node_from       
state_node_int_from<- int_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                              taxon_node_from) %>% 
  rename("node_id" ="node_from", "abundances" = "ab_node_from",
         "taxon" = "taxon_node_from")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat

#node_to
state_node_int_to<- int_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                            taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to") %>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat


# final state nodes (calculate abundance of species)
state_node_int_agg<-rbind(state_node_int_from, state_node_int_to) %>% ungroup() %>% 
  select(-habitat) %>% group_by(node_id) %>% 
  mutate(abun = sum(abundances)) %>% distinct(abun,taxon)


## --  Create edge list of new management scenario
int_edgelist_aggr<-int_edgelist_no_aggr %>% select(node_from,node_to) %>% 
  unique() %>%  #aggregated edge list
  mutate(management = "I") 



##### -- Intensive non-organic management (eliminate weeds from the network)
 weeds = 1:93 
 crops = 94:99
 aphid = 337:364
 seed_ins = 476:494
 seed_bird = 495:506
 seed_rod = 507:510
 herbivores <- c(aphid, seed_ins, seed_bird, seed_rod)

 
 ### -- Create edge list
 
 # Remove weeds from the intensive management
int_mon_edgelist_aggr<-int_edgelist_aggr %>% 
                      filter(!(node_from%in%weeds), !(node_to%in%weeds)) %>%  #eliminate weeds and species that only interact with them
                       mutate(management = "IN") 

# Remove herbivores that feed on weeds
# Step 1: Identify herbivores that interact with crops
interact_with_crops <- int_edgelist_aggr %>%
  filter((node_from %in% herbivores & node_to %in% crops) | 
           (node_to %in% herbivores & node_from %in% crops)) %>%
  select(node_from, node_to) %>%
  unlist() %>%
  as.numeric() %>%
  unique()
herbivores_crops<-interact_with_crops[interact_with_crops > 99] #list of pest

# Step 2: Filter the dataset to include only interactions between herbivores and weeds, excluding those that interact with crops
interact_without_crops <- int_edgelist_aggr %>%
  filter(
    ((node_from %in% herbivores & node_to %in% weeds) | 
       (node_to %in% herbivores & node_from %in% weeds)) & 
      !(node_from %in% herbivores_crops | node_to %in% herbivores_crops)
  ) %>%  select(node_from, node_to) %>%
  unlist() %>%
  as.numeric() %>%
  unique()

herbivores_only_weeds<- interact_without_crops[interact_without_crops > 99] #list of herbivores that only interact with weeds

# Remove herbivores_only_weeds and their interactions in the dataframe
int_mon_edgelist_aggr2<-int_mon_edgelist_aggr %>% 
  filter(
    !(node_from %in% herbivores_only_weeds) & !(node_to %in% herbivores_only_weeds))  # Exclude all interactions of herbivores only feed on weeds


# States nodes
state_node_int_mod_agg <- state_node_int_agg %>% 
                          filter(node_id%in%int_mon_edgelist_aggr2$node_from |
                                  node_id%in%int_mon_edgelist_aggr2$node_to) 
    

##### ---  Final dataframe
land_change_weighted<-rbind(extensive_edgelist,sem_ext_edgelist_aggr,mod_edgelist_aggr,
                          sem_int_edgelist_aggr,int_edgelist_aggr,int_mon_edgelist_aggr2)


#write.csv(land_change_weighted,"Data/Land_use_edgelist.csv", row.names= FALSE)

# final state_node list with abundances
state_nodes_weighted_ab<-rbind(ab_ext,state_node_sem_ext_agg,
                                  state_node_mod_agg,state_node_sem_int_agg,
                                  state_node_int_agg, state_node_int_mod_agg)
#add management label
state_nodes_weighted<-cbind(management = rep(c("E","SE","M","SI","I","IN"),
                                                     c(nrow(ab_ext),nrow(state_node_sem_ext_agg),
                                                       nrow(state_node_mod_agg), nrow(state_node_sem_int_agg),
                                                       nrow(state_node_int_agg), nrow(state_node_int_mod_agg))),
                                                    state_nodes_weighted_ab)

#write.csv(state_nodes_weighted,"Data/Land_use_state_nodes.csv", row.names= FALSE)


################## --- ESTIMATION OF DIRECT ES PROVISION AND INDIRECT EFFECT ON ES --


######## --  Direct ES provision

## Add information of ES to the nodes (values 0-1)
nodes_ES<- right_join(state_nodes_weighted, Norwood_farm$nodes, by = "node_id")%>% 
  select(management,node_id,taxon.x,abun, "Crop production",
         "Pollination", "Crop damage", "Pest control", "Seed dispersal", "Butterfly watching", "Bird watching") %>% 
  group_by(management,node_id) %>% rename("taxon" = "taxon.x") %>% 
  gather("services","value", 5:11) #we conserve species that not directly provide ES because can serve as intermediate paths to ES-provider sps

nodes_ES$management <- factor(nodes_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors


####### -- Estimate the amount of direct ES provision per species (weight = abundance * body mass

#The equation to estimate the amount will change according to the type of ES.
#For bird watching and butterfly watching is just the abundance. For the rest is the product between abundance and biomass

#upload file with body mass
body_mass<-read.csv("Data/biomass.csv",header=T)


# Calculate the amount of ES provided as the product between the abundance and biomass (for butterfly and watching we are using just the abundance) 
direct_ES <- nodes_ES %>% filter (value ==1) %>% 
  left_join(body_mass,by = "node_id") %>% select(-node_name,-taxon.y) %>% 
  rename("taxon"="taxon.x", "body_mass" = "biomass.g") %>% 
  mutate (type = "D",
           weight = abun * body_mass) %>% #amount of direct ES provision
          select(-value) 

#write.csv(direct_ES,"Data/Land_use_dir_ES.csv", row.names= FALSE)


######## -- Indirect effects on ES

## -- Prepare dataframe
# Full list nodes with ES in the network (considering those that provide and not provide direct ES)

list_nodes_ES_provi<-nodes_ES %>% ungroup() %>% select(-management,-abun) %>%
  filter (value ==1) %>% unique # list of nodes that provide ES ( = no plants and ectoparasites)

list_nodes_ES_no_provi<-nodes_ES %>% ungroup() %>% select(-management,-abun,-services) %>% unique() %>% 
                    group_by(node_id) %>% mutate(tot_serv = sum(value)) %>% select(-value) %>% 
                    filter(tot_serv == 0) %>% mutate(services = "None", value = 1) %>% # filter species that not directly provide any ES and assign them as None
                    select(-tot_serv)

list_nodes_ES<-rbind(list_nodes_ES_provi,list_nodes_ES_no_provi) #Total list of nodes with ES (with None)


# Add attributes of nodes to the edgelist
edge_list<- left_join(land_change_weighted,list_nodes_ES, by = c("node_from"="node_id")) %>% 
  rename("taxon_from"="taxon", "services_from"="services",
         "value_from" = "value") %>% 
  left_join(list_nodes_ES, by = c("node_to"="node_id"))  %>% 
  rename("taxon_to"="taxon", "services_to"="services",
         "value_to" = "value") 

# Add inverted links (to make the code easier to program when calculate indirect interactions. It will not affect the output)
edge_list_inverted<- tibble(values = edge_list$node_to,edge_list$node_from,
                            edge_list$management, edge_list$taxon_to,edge_list$services_to,
                            edge_list$value_to,  edge_list$taxon_from,edge_list$services_from,edge_list$value_from)
colnames(edge_list_inverted) <- c("node_from", "node_to", "management", "taxon_from", "services_from",
                                  "value_from", "taxon_to", "services_to", "value_to")

# Combine both data frame to create the final edge list
edgelist_final<- bind_rows(edge_list, edge_list_inverted) %>% 
  select(-value_to,-value_from) # we are not using these anymore

edgelist_final<-edgelist_final[,c(3,1,4,5,2,6,7)]



#### - Calculate indirect effects considering 1 hop (node 1 - node 2)

# Create objects to store
management = c()
services_from = c()
node_from= c()
node_to = c()
taxon_from = c()
services_to = c()

# Reorder the dataframe to check indirect interaction in 1 hop
for (i in 1:nrow(edgelist_final)){
  
  management = c(management, edgelist_final$management[i])
  services_from = c(services_from,edgelist_final$services_from[i])
  node_from = c(node_from,edgelist_final$node_from[i])
  node_to = c(node_to,edgelist_final$node_to[i])
  taxon_from = c(taxon_from, edgelist_final$taxon_from[i])
  services_to = c(services_to, edgelist_final$services_to[i])
  
}

Indirect_1hop_landuse_weighted<-data.frame(management,services_from,node_from,node_to,taxon_from,
                                           services_to,hop = rep(1, length(services_from)), 
                                           type = rep("I", length(services_from))) 


  
# Rearrange the output
# we remove duplicates rows where node_from = birds or butterflies cause they represent the same interaction. 
# This happens because each row represents an attribute and these taxons have 2 and 3 attributes per node.

rows_birds_butt<- Indirect_1hop_landuse_weighted %>%
  filter(taxon_from == "Butterfly" | taxon_from == "Seed-feeding bird") %>%
  distinct(management, node_from, node_to, .keep_all = TRUE) # new subset after eliminating duplicate rows for node_from = birds and butterflies

int_without<-Indirect_1hop_landuse_weighted %>% filter(!(taxon_from == "Butterfly" | 
                                                         taxon_from == "Seed-feeding bird")) #eliminate the interactions containing node_from =birds or butterflies from the original dataframe


Indirect_1hop_landuse_weighted_2<-rbind(rows_birds_butt,int_without) #final dataframe containing indirect effects on ES via 1 hop


#write.csv(Indirect_1hop_landuse_weighted_2,"Data/Land_use_ind_1hop.csv", row.names= FALSE) (intermediate file)



#### - Calculate indirect effects considering 2 hops (node 1 - node 2 - node 3, effect of node 1 on node 3'ES via node 2)

Indirect_1hop <-read.csv("Data/Land_use_ind_1hop.csv",
                        sep =",") #load dataframe of indirect effects using 1 hop

 
# Calculate indirect effect on ES

# Create empty vectors
management = c()
node_id= c()
node_int = c()
taxon_from = c()
services = c()
node_to = c()
services_to = c()


# Iterate to each row
for (i in 1:nrow(Indirect_1hop)){ #each row represents interaction between species
  
  j = Indirect_1hop$node_to[i] # check the node_to from the 1 hop indirect effect (intermediate species: node 2 in the title)
  l = Indirect_1hop$management [i] #check management where the target species for which we are detecting indirect effects on ES
  
  # Filter dataframe (filter node 3's ES affected by node 2)
  
  services_int <- Indirect_1hop %>% filter(node_from == j, #filter to show node 2
                                           node_to != Indirect_1hop$node_from[i], #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
                                           management== l) %>% 
                                                select(node_to,services_to)
  # Storage the results
  
  services_to <- c(services_to, unlist(services_int$services_to)) # add node 3 ES
  node_to <- c(node_to, unlist(services_int$node_to))# add identity of node 3
  node_int <- c(node_int,rep(j, nrow(services_int)))
  node_id <- c(node_id,rep(Indirect_1hop$node_from[i], nrow(services_int)))# add target (node 1) for which we are detected indirect effects 
  taxon_from<- c(taxon_from, rep(Indirect_1hop$taxon_from[i], nrow(services_int)))
  services <- c(services, rep (Indirect_1hop$services_from[i], nrow(services_int)))# direct ES provided by the target node
  management <- c(management, rep(l, nrow(services_int)))

}

Indirect_2hop<- data.frame(management,node_id,taxon_from,services,node_int,
                           node_to, services_to,type = rep("I", length(services_to)), hop = rep(2,length(services_to)))


#write.csv(Indirect_2hop,"Data/Land_use_ind_2hop.csv", row.names= FALSE) (intermediate file)

Indirect_2hop<-read.csv("Data/Land_use_ind_2hop.csv",
                        sep =",") #load dataframe of indirect effects using 2 hops


# Join both 1 and 2 hops indirect effects dataframes

# 1 hop (node 1 - node 2)
Indirect_1hop_m<-Indirect_1hop  %>%  rename("services" ="services_from",
                                           "node_id" = "node_from",
                                           "taxon" = "taxon_from") %>% 
                                    mutate(node_int = NA)

Indirect_1hop_m<-Indirect_1hop_m[,c(1,2,3,5,9,4,6,7,8)]

# 2 hop (node 1 - node 2 - node 3)
Indirect_2hop_m<-Indirect_2hop %>% rename("taxon" = "taxon_from") 
Indirect_2hop_m<-Indirect_2hop_m[,c(1,4,2,3,5,6,7,8,9)]

#  Total Indirect effect of ES
I_ES<- rbind(Indirect_1hop_m,Indirect_2hop_m)
I_ES2<- I_ES %>%  filter(services_to != 'None')
#write.csv(I_ES2,"Data/Land_use_ind_ES.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         STATISTICAL ANALYSIS                     
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### -- Proportion of direct ES retained across land use change --

#upload and prepare dataframe
direct_ES<- read.csv("Data/Land_use_dir_ES.csv", sep =",") 
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors

Prop_dir<-direct_ES %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  # Prop. of direct ES retained 
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
library(effects)
library(car)

Prop_dire<- glmmTMB (prop ~ management + services, family=beta_family(link="logit"), data = Prop_dir) # model that best fit
Anova(Prop_dire)

# Summarize the model to view coefficients
summ<-summary(Prop_dire)

# Extract the coefficients (for GLMM it's the average including all the levels)
coefficients <- summ$coefficients$cond

# Extract coefficients for each factor
management_coefs <- coefficients[grep("management", rownames(coefficients)), "Estimate"]
services_coefs <- coefficients[grep("services", rownames(coefficients)), "Estimate"]

# Calculate summary statistics for each factor
management_summary <- mean(management_coefs)
services_summary <- mean(services_coefs)

#Homogeneity
EM<-resid(Prop_dire, type= "response") 
FM<-fitted(Prop_dire) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(Prop_dire, type= "response") 
boxplot(E1_lme ~ management, data = Prop_dir, main = "Management")



#### -- Proportion of indirect effect on ES retained across land use change --

#upload and prepare dataframe
output_ind_ES <- read.csv("Data/Land_use_ind_ES.csv", sep =",") 

output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors


Prop_ind<-output_ind_ES %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of indirect effects on ES retained 
  dplyr::select(management,services_to,tot,prop) %>% unique()


# adjust the extreme values according to the beta conditions
Prop_ind$prop <- ifelse(Prop_ind$prop == 0, 0.000001, 
                        ifelse(Prop_ind$prop == 1, 0.9999999, Prop_ind$prop))

# Model
library("glmmTMB")
library("stats4")
library("bbmle")

Prop_indi<-glmmTMB (prop ~ management + services_to, family=beta_family(link="logit"), data = Prop_ind) #model that best fit
Anova(Prop_indi)

# Summarize the model to view coefficients
summ<-summary(Prop_indi)

# Extract the coefficients (for GLMM it's the average including all the levels)
coefficients <- summ$coefficients$cond

# Extract coefficients for each factor
management_coefs <- coefficients[grep("management", rownames(coefficients)), "Estimate"]
services_coefs <- coefficients[grep("services_to", rownames(coefficients)), "Estimate"]

# Calculate summary statistics for each factor
management_summary <- mean(management_coefs)
services_summary <- mean(services_coefs)


#Homogeneity
EM<-resid(Prop_indi, type= "response") 
FM<-fitted(Prop_indi) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")

abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(Prop_indi, type= "response") 
boxplot(E1_lme ~ management, data = Prop_ind, main = "Management")




#### -- Relative change in the amount of direct ESs provision after land-use change 

#the equation to estimate the amount will change according to the type of ES.
#For bird watching and butterfly watching is just the abundance. For the rest is the product between abundance and biomass (more details in the manuscript)

## Amount Bird and butterfly watching
extensive_amount_watching<-direct_ES %>% filter(management=="E" &  (services == "Bird watching" | services == "Butterfly watching" )) %>% 
  select(node_id, services, abun)

# Merging with other management scenarios and calculate ratio of change
dir_amount_watching <- direct_ES %>% filter(services == "Bird watching" | services == "Butterfly watching" ) %>% 
  left_join(extensive_amount_watching, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = abun / abun_extensive) %>% #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
  select(management,node_id,services,ratio_change)

## Amount if the rest ESs
extensive_amount_rest<-direct_ES %>% filter(management=="E" &  !(services == "Bird watching" | services == "Butterfly watching" )) %>% 
  select(node_id, services, weight)

# Merging with other management scenarios and calculate ratio of change
dir_amount_rest <- direct_ES %>% filter(!(services == "Bird watching" | services == "Butterfly watching" )) %>% 
  left_join(extensive_amount_rest, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = weight / weight_extensive) %>%  #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S
  select(management,node_id,services,ratio_change)

## Final amount
dir_amount<- rbind(dir_amount_watching,dir_amount_rest)


# Model
library(glmmTMB)
m_amount<- glmmTMB(ratio_change ~ management+services + (1|node_id),family = Gamma(link = "log"),
                   data = dir_amount) #the lowest AIC, interaction did not converge
Anova(m_amount)

summary(dir_amount$ratio_change)
sum(dir_amount$ratio_change <= 0)  
# Summarize the model to view coefficients
summ<-summary(m_amount)

# Extract the coefficients (for GLMM it's the average including all the levels)
coefficients <- summ$coefficients$cond

# Extract coefficients for each factor
management_coefs <- coefficients[grep("management", rownames(coefficients)), "Estimate"]
services_coefs <- coefficients[grep("services", rownames(coefficients)), "Estimate"]

# Calculate summary statistics (e.g., mean) for each factor
management_summary <- mean(management_coefs)
services_summary <- mean(services_coefs)

#Homogeneity
EM<-resid(m_amount, type= "response") 
FM<-fitted(m_amount) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(m_amount, type= "response") 
boxplot(E1_lme~dir_amount$management, main="Management")



