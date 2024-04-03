################### CHANGE IN LAND USE SIMULATION (Permanent pastures intense) ##################################



# In this code we create the different proposed management scenarios by aggregating the layers
#to create the final network. 

# To create the different management scenarios we:

# 1) Create multilayer network with abundances as state nodes attributes (nodes’ abundances per each habitat)

# 2) Change the habitats to permanent pasture "PP” but modifying the abundance of each species 
#according to the area 

# 3) Aggregate the habitats to create the Norwoodfarm network. During this step, 
# we add the abundances of the same species across habitats. Then, we calculate the weight of links between two species

library(emln)#multilayer package
library(readr)
library(ggplot2)
library(cowplot)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")

######### --- Upload multilayer network
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
 


################## --- CREATE MANAGAMENT SCENARIOS 


##### -- Rearrange dataframe to include in the simulation

## Add the abundances (as state nodes attributes) 
abundances<-read.csv("Data/species_abundances.csv",header=T) #call abundances

state_nodes_ab<-Norwood_farm$state_nodes %>% left_join(abundances, 
                                                       by = c("layer_name" = "habitat",
                                                              "node_name" = "species_name")) %>% #add abundances
  left_join(Norwood_farm$nodes, by = "node_id") %>% 
  select(layer_id,node_id,abundance, taxon) ##add taxon



## Call dataframe of habitats' area

areas<-read.csv("Data/habitatarea.csv", sep =,) %>% 
  filter(HabitatCode != "ST") %>% #remove standing trees
  mutate(HabitatCode = case_when(HabitatCode == "C"~ "CP",
                                 HabitatCode == "WU"~ "WD",
                                 TRUE~HabitatCode))

habitat_area <- areas %>% mutate(area_ave = case_when(
  (Area_2007 >0) & (Area_2008 >0) ~ (Area_2007+Area_2008)/2, #if the same habitat was present in both years do the average
  (Area_2007 >0) & (Area_2008  ==0)~ Area_2007, #if the habitat was present in one year, keep the area of the year
  (Area_2007 ==0) & (Area_2008  >0)~ Area_2008))


########## -- Create Management scenarios

##### -- Extensive

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


##### -- Semi - extensive (replace "WD" and "RG" for "PP")


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

# create habitats PP to replace WD and RG
WD_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
   mutate (new_hab = 11, prev_hab = "WD", hab = "PP")#links from "PP" to add as new habitat (12)

RG_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 12, prev_hab = "RG", hab = "PP")#links from "PP" to add as new habitat (13)

#calculate changes in the area between PP and the habitat to replace 
converted_area<-rbind(WD_PP, RG_PP) %>% left_join(habitat_area, 
                                  by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_PP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_PP)) %>% #multplied abundances of PP for this value (to estimate according to the new habitat)
  select(-prev_hab,-hab,-area_prev_hab,-area_PP)#clean dataframe


# add abundances and modify it according to the new area
abundances_PP<-state_nodes_ab %>% filter(layer_id ==7)#filter species abundances to show just layer PP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_PP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_PP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_PP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_PP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_PP * mult_ab, 
         ab_node_to = ab_node_to_PP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interactio where one partner have less than 1 indidivual (threshold)

new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)


## -- create state_node_list of the management scenario

sem_ext_edgelist_no_aggr<- rbind(sem_ext_edgelist_rem,new_habitats_ab_rem)  #join new habitats and old habitats

#node_from       
state_node_sem_ext_from<- sem_ext_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                        taxon_node_from) %>% 
                        rename("node_id" ="node_from", "abundances" = "ab_node_from",
                               "taxon" = "taxon_node_from")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat

#node_to
state_node_sem_ext_to<- sem_ext_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                        taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat


# final state nodes (calculate abundance and relative abundance of species)

state_node_sem_ext_agg<-rbind(state_node_sem_ext_from, state_node_sem_ext_to)%>% ungroup() %>% 
  select(-habitat) %>% group_by(node_id,taxon) %>% 
  mutate(abun = sum(abundances)) %>% distinct(abun) %>% group_by(taxon) %>% 
  mutate(tot_ab_taxon = sum(abun)) %>% #total abundance per taxon
  group_by(node_id) %>% 
  mutate(rel_ab=abun/tot_ab_taxon)#rel abundance of species per taxon


## --  Recalculate weight according to the new habitats and abundances

# Edge list new management scenario

sem_ext_edgelist_aggr<-sem_ext_edgelist_no_aggr %>% select(node_from,node_to) %>% 
                      unique() %>%  #aggregated edge list
      left_join(state_node_sem_ext_agg, by = c("node_from" = "node_id")) %>%  #incorporate rel abundances of node_from
       left_join(state_node_sem_ext_agg, by = c("node_to" = "node_id")) %>%  #incorporate rel abundances of nodes_to
       select(node_from,node_to, rel_ab.x,rel_ab.y) %>% 
  mutate(weight = rel_ab.x * rel_ab.y,management = "SE") %>% #calculate weight 
  select(-rel_ab.x,-rel_ab.y)
  





##### -- Moderate (replace "WD","RG","MH"and "NH" for "PP")


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


#--  Create new habitats 

# create habitats PP to replace WD,RG, MH and NH (WD and RG were created before)
MH_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 13, prev_hab = "MH", hab = "PP")#links from "PP" to add as new habitat (14)

NH_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 14, prev_hab = "NH", hab = "PP")#links from "PP" to add as new habitat (15)

# calculate changes in the area between PP and the habitat to replace 
converted_area<-rbind(WD_PP, RG_PP, MH_PP, NH_PP) %>% left_join(habitat_area, 
                                                  by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_PP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_PP)) %>% #multplied abundances of PP for this value (to estimate according to the new habitat)
  select(-prev_hab,-hab,-area_prev_hab,-area_PP)#clean dataframe


# add abundances and modify them according to the new area
abundances_PP<-state_nodes_ab %>% filter(layer_id ==7)#filter species abundances to show just layer PP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_PP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_PP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_PP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_PP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_PP * mult_ab, 
         ab_node_to = ab_node_to_PP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interactio where one partner have less than 1 indidivual (threshold)

new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)


## -- create state_node_list of the management scenario

mod_edgelist_no_aggr<- rbind(mod_edgelist_rem,new_habitats_ab_rem)  #join new habitats and old habitats

#node_from       
state_node_mod_from<- mod_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                              taxon_node_from) %>% 
  rename("node_id" ="node_from", "abundances" = "ab_node_from",
         "taxon" = "taxon_node_from")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat

#node_to
state_node_mod_to<- mod_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                            taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat


# final state nodes (calculate abundance and relative abundance of species)

state_node_mod_agg<-rbind(state_node_mod_from, state_node_mod_to)%>% ungroup() %>% 
  select(-habitat) %>% group_by(node_id,taxon) %>% 
  mutate(abun = sum(abundances)) %>% distinct(abun) %>% group_by(taxon) %>% 
  mutate(tot_ab_taxon = sum(abun)) %>% #total abundance per taxon
  group_by(node_id) %>% 
  mutate(rel_ab=abun/tot_ab_taxon)#rel abundance of species per taxon



## --  Recalculate weight according to the new habitats and abundances

# Edge list new management scenario

mod_edgelist_aggr<-mod_edgelist_no_aggr %>% select(node_from,node_to) %>% 
  unique() %>%  #aggregated edge list
  left_join(state_node_mod_agg, by = c("node_from" = "node_id")) %>%  #incorporate rel abundances of node_from
  left_join(state_node_mod_agg, by = c("node_to" = "node_id")) %>%  #incorporate rel abundances of nodes_to
  select(node_from,node_to, rel_ab.x,rel_ab.y) %>% 
  mutate(weight = rel_ab.x * rel_ab.y,management = "M") %>% #calculate weight 
  select(-rel_ab.x,-rel_ab.y)





##### -- Semi - intensive (replace "WD","RG","MH","NH","GM", "SF", and "CP" for "PP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

sem_int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 &  layer_from != 4 &
           layer_from != 5 & layer_from != 2 & layer_from != 9 &
           layer_from != 1 ) %>% #links from "WD", "RG", "MH", "NH"" and "GM"","SF","CP" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

sem_int_edgelist_rem<-sem_int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


#--  Create new habitats 

# create habitats PP to replace WD,RG, MH,NH,GM,SF and CP (WD,RG,MH and NH were created before)
GM_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 15, prev_hab = "GM", hab = "PP")#links from "PP" to add as new habitat (16)

SF_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 16, prev_hab = "SF", hab = "PP")#links from "PP" to add as new habitat (17)

CP_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 17, prev_hab = "CP", hab = "PP")#links from "PP" to add as new habitat (18)


# calculate changes in the area between PP and the habitat to replace 
converted_area<-rbind(WD_PP, RG_PP, MH_PP, NH_PP, GM_PP,SF_PP,CP_PP) %>% 
  left_join(habitat_area, by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_PP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_PP)) %>% #multplied abundances of PP for this value (to estimate according to the new habitat)
  select(-prev_hab,-area_prev_hab,-area_PP)#clean dataframe


# add abundances and modify them according to the new area
abundances_PP<-state_nodes_ab %>% filter(layer_id ==7)#filter species abundances to show just layer PP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_PP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_PP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_PP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_PP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_PP * mult_ab, 
         ab_node_to = ab_node_to_PP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interactio where one partner have less than 1 indidivual (threshold)

new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)



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



# final state nodes (calculate abundance and relative abundance of species)

state_node_sem_int_agg<-rbind(state_node_sem_int_from, state_node_sem_int_to)%>% ungroup() %>% 
  select(-habitat) %>% group_by(node_id,taxon) %>% 
  mutate(abun = sum(abundances)) %>% distinct(abun) %>% group_by(taxon) %>% 
  mutate(tot_ab_taxon = sum(abun)) %>% #total abundance per taxon
  group_by(node_id) %>% 
  mutate(rel_ab=abun/tot_ab_taxon)#rel abundance of species per taxon



## --  Recalculate weight according to the new habitats and abundances

# Edge list new management scenario

sem_int_edgelist_aggr<-sem_int_edgelist_no_aggr %>% select(node_from,node_to) %>% 
  unique() %>%  #aggregated edge list
  left_join(state_node_sem_int_agg, by = c("node_from" = "node_id")) %>%  #incorporate rel abundances of node_from
  left_join(state_node_sem_int_agg, by = c("node_to" = "node_id")) %>%  #incorporate rel abundances of nodes_to
  select(node_from,node_to, rel_ab.x,rel_ab.y) %>% 
  mutate(weight = rel_ab.x * rel_ab.y,management = "SI") %>% #calculate weight 
  select(-rel_ab.x,-rel_ab.y)





##### -- Intensive (replace "WD","RG","MH","NH","GM","SF","CP", "LP", and "NL" for "PP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 8 & layer_from != 10 &  layer_from != 4 &
           layer_from != 5 & layer_from != 2 & layer_from != 9 &
           layer_from != 1 & layer_from != 3 & layer_from != 6 ) %>% #links from habitats removed (except from PP)
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

int_edgelist_rem<-int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]



#--  Create new habitats 

# create habitats PP to replace WD,RG, MH,NH,GM and SF (WD,RG,MH,NH andGM were created before)
LP_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 18, prev_hab = "LP", hab = "PP")#links from "PP" to add as new habitat (19)

NL_PP<- Norwood_farm$extended_ids %>% filter(layer_from  == 7) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 19, prev_hab = "NL", hab = "PP")#links from "PP" to add as new habitat (21)


# calculate changes in the area between PP and the habitat to replace 
converted_area<-rbind(WD_PP, RG_PP, MH_PP, NH_PP, GM_PP,SF_PP,CP_PP,LP_PP,
                      NL_PP) %>%
  left_join(habitat_area, by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_PP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_PP)) %>% #multplied abundances of PP for this value (to estimate according to the new habitat)
  select(-prev_hab,-hab,-area_prev_hab,-area_PP)#clean dataframe


# add abundances and modify them according to the new area
abundances_PP<-state_nodes_ab %>% filter(layer_id ==7)#filter species abundances to show just layer PP

new_habitats_ab<-converted_area %>%  
  left_join(abundances_PP, by = c("node_from" = "node_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(abundances_PP, by = c("node_to" = "node_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from_PP" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to_PP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
  mutate(ab_node_from = ab_node_from_PP * mult_ab, 
         ab_node_to = ab_node_to_PP * mult_ab ) %>% #estimate the new abundances
  select(new_hab,node_from,ab_node_from,taxon_node_from,node_to,ab_node_to,
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist

# remove interactio where one partner have less than 1 indidivual (threshold)

new_habitats_ab_rem<- new_habitats_ab %>% filter(ab_node_from >=1 & ab_node_to >=1)



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
         "taxon" = "taxon_node_to")%>% 
  group_by(habitat,node_id) %>% unique() #eliminate duplicate species within each habitat



# final state nodes (calculate abundance and relative abundance of species)

state_node_int_agg<-rbind(state_node_int_from, state_node_int_to) %>% ungroup() %>% 
  select(-habitat) %>% group_by(node_id,taxon) %>% 
  mutate(abun = sum(abundances)) %>% distinct(abun) %>% group_by(taxon) %>% 
  mutate(tot_ab_taxon = sum(abun)) %>% #total abundance per taxon
  group_by(node_id) %>% 
  mutate(rel_ab=abun/tot_ab_taxon)#rel abundance of species per taxon


## --  Recalculate weight according to the new habitats and abundances

# Edge list new management scenario

int_edgelist_aggr<-int_edgelist_no_aggr %>% select(node_from,node_to) %>% 
  unique() %>%  #aggregated edge list
  left_join(state_node_int_agg, by = c("node_from" = "node_id")) %>%  #incorporate rel abundances of node_from
  left_join(state_node_int_agg, by = c("node_to" = "node_id")) %>%  #incorporate rel abundances of nodes_to
  select(node_from,node_to, rel_ab.x,rel_ab.y) %>% 
  mutate(weight = rel_ab.x * rel_ab.y,management = "I") %>% #calculate weight 
  select(-rel_ab.x,-rel_ab.y)


##### ---  Final dataframe

land_change_weighted<-rbind(ext_edgelist_aggr,sem_ext_edgelist_aggr,mod_edgelist_aggr,
                          sem_int_edgelist_aggr,int_edgelist_aggr)


#write.csv(land_change_weighted,"Data/Land_use_rat_edgelist_weighted_PP_intense.csv", row.names= FALSE)

#final state_node list with abundances
state_nodes_weighted_ab<-rbind(ab_ext[,1:3],state_node_sem_ext_agg[,1:3],
                                  state_node_mod_agg[,1:3],state_node_sem_int_agg[,1:3],
                                  state_node_int_agg[,1:3])
#add management label
state_nodes_weighted<-cbind(management = rep(c("E","SE","M","SI","I"),
                                                     c(nrow(ab_ext),nrow(state_node_sem_ext_agg),
                                                       nrow(state_node_mod_agg), nrow(state_node_sem_int_agg),
                                                       nrow(state_node_int_agg))), state_nodes_weighted_ab)

#write.csv(state_nodes_weighted,"Data/Land_use_rat_state_nodes_PP_intense.csv", row.names= FALSE)


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


#write.csv(direct_ES,"Data/Land_use_dir_weighted_PP_intense.csv", row.names= FALSE)

## -- Direct provision Ratio ES/E(D)S

# ratio > 1 indicates more benefits, ratio = 1 balance between damages and benefits and ratio < 1 more damage

number_ES<-nodes_ES %>% group_by(management) %>% 
  filter (value ==1 & services != "Crop damage") %>% summarise (ES =sum(value))#count ES provision

number_EDS<-nodes_ES %>% group_by(management) %>% 
  filter (value ==1 & services == "Crop damage") %>% summarise (EDS =sum(value))#count EDS provision

total<-cbind(number_ES,number_EDS[,-1])

ratio_direct<-total %>% mutate(ratio_direct = ES/EDS)

#write.csv(ratio_direct,"Data/Land_use_rat_dir_weighted_PP_intense.csv", row.names= FALSE)





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

Indirect_1hop_landuse_weighted<-data.frame(management,services_from,node_from,
                                           node_to,taxon_from,services_to,weight,
                                           hop = rep(1, length(services_from)), 
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


#write.csv(Indirect_1hop_landuse_weighted_2,"Data/Land_use_ind_1hop_weighted_PP_intense.csv", row.names= FALSE)



#### - Calculate indirect effects considering 2 hops (node 1 - node 2 - node 3, effect of node 1 on node 3'E(D)S via node 2)

Indirect_1hop<-read.csv("Data/Land_use_ind_1hop_weighted_PP_intense.csv",
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


#write.csv(Indirect_2hop,"Data/Land_use_ind_2hop_weighted_PP_intense.csv", row.names= FALSE)

Indirect_2hop<-read.csv("Data/Land_use_ind_2hop_weighted_PP_intense.csv",
                        sep =",") #load dataframe of indirect effects using 1 hop


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

#write.csv(I_ES,"Data/Land_use_ind_weighted_PP_intense.csv")


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

hop_1 <- read.csv("Data/Land_use_ind_weighted_PP_intense.csv", sep =",", row.names = 1) %>% 
  filter(hop == 1, services_to !="None") #remove indirect effect that not affect any ES


# We assigned the indirect output according to node_id and node_to

output_ES_1hop<- hop_1 %>%  
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

hop_2 <- read.csv("Data/Land_use_ind_weighted_PP_intense.csv", sep =",",row.names = 1) %>% 
  filter(hop == 2, services_to !="None") #remove indirect effect that not affect any ES


# We assigned the indirect output according from "node_id" to "node_to" via "node_int"

output_ES_2hops<- hop_2 %>%  
  mutate(output = case_when( 
    
    (node_id %in% plants | node_id %in% crops) &  (node_int%in%aphid | node_int%in%seed_bird| 
                                                     node_int%in%seed_ins | node_int%in%seed_rod) &
      services_to == "Crop production"  ~ "-",     #plants,crops --> + seed predators --> - crop --> - crop production
    
    
    (node_id%in%flow_vis | node_id%in%butt) & (node_int%in%plants| node_int%in%crops) &
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

#write.csv(output_ES,"Data/Land_use_output_weighted_PP_intense.csv", row.names= FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Analyses                            
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Proportion of direct E(D)S retained across land use change --

#upload and prepare dataframe
direct_ES<- read.csv("Data/Land_use_dir_weighted_PP_intense.csv", sep =",") 

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

Prop_dire<-glmmTMB (prop ~ management + ( 1| services), family=beta_family(link="logit"), data = Prop_dir)
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


#### Proportion of indirect effect on E(D)S retained across land use change --

#upload and prepare dataframe
output_ind_ES <- read.csv("Data/Land_use_output_weighted_PP_intense.csv", sep =",") 
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
Prop_indi<-glmmTMB (prop ~ management + services_to, family=beta_family(link="logit"), data = Prop_ind)# the best model
#Prop_indi_2<-glmmTMB (prop ~ management + ( 1| services_to), family=beta_family(link="logit"), data = Prop_ind)

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
  dplyr::select(node_id, services, weight)


# Merging and other managements and calculate ratio of change
dir_amount <- direct_ES %>% 
  left_join(extensive_amount, by = c("node_id", "services"), suffix = c("", "_extensive")) %>%
  mutate(ratio_change = weight / weight_extensive) #ratio of change: values higher than 1 indicates increasing in the amount of E(D)S

# Model
library(glmmTMB)
m_amount<- glmmTMB(ratio_change ~ management + (1|node_id),family = Gamma(link = "log"),
                   data = dir_amount) #model that best fit

#m_amount_2<- glmmTMB(ratio_change ~ management + services (1|node_id),family = Gamma(link = "log"),
                  #   data = dir_amount) 
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
post_amount<- emmeans(m_amount, ~ management)
pairs(post_amount)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Plots                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#direct
direct_ES<- read.csv("Data/Land_use_dir_weighted_PP_intense.csv", sep =",") %>% 
            mutate(services = case_when(services == "Crop production"~ "Resource provision",
                              TRUE~services))

direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

#indirect
output_ind_ES <- read.csv("Data/Land_use_output_weighted_PP_intense.csv", sep =",") %>% 
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
        axis.text.x  = element_text(size=15, color='black'),
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

#ggsave("Land_use_number_PP_intense.png")


### Plot of proportions of direct and indirect E(D)S retained 


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
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))
prop_EDS_direct

#ggsave("Land_use_retained_direct_PP_intense.png")

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
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))
prop_EDS_indirect

#ggsave("Land_use_retained_indirect_PP_intense.png")



### Plot of output according to direct and indirect 

#direct

ratio_direct<-read.csv("Data/Land_use_rat_dir_weighted_PP_intense.csv")

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

#ggsave("Land_use_output_PP_intense.png")





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

#ggsave("Land_use_weight_PP_intense.png")




### Plot of proportion of output provided per taxon according to trophic group


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
#ggsave("output_taxon_direct_positive_PP.png")

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
#ggsave("output_taxon_direct_negative_PP.png")


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
#ggsave("output_taxon_indirect_positive_PP.png")


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

#ggsave("output_taxon_indirect_negative_PP.png")

