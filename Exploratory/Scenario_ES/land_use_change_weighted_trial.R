################### CHANGE IN LAND USE SIMULATION (abundances_trial) ##################################

# In this code we create the different proposed management scenarios by aggregating the layers
#to create the final network. 

# To create the different management scenarios we:

# 1) Create multilayer network with abundances as state nodes attributes (nodes’ abundances per each habitat)

# 2) Change the habitats to “CP” but modifying the abundance of each species 
#according to the area.

# 3) Aggregate the habitats to create the Norwoodfarm network. During this step, 
# we add the abundances of the same species across habitats. Then, we calculate the weight of links between two species


library(emln)#multilayer package
library(readr)
library(ggplot2)
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

######### --- Upload multilayer network
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
Norwood_farm$extended_ids$weight<-1 # non-weighted for now 


################## --- CREATE MANAGAMENT SCENARIOS (abundances_trial)


##### -- Rearrange dataframe to include in the simulation

## Simulate abundances per habitats (as state nodes attributes)

abundances_sim<-sample(1:50, nrow(Norwood_farm$state_nodes),replace = T)#simulate abundances

state_nodes_ab<-cbind(Norwood_farm$state_nodes,abundances_sim) %>% 
  rename("abundance" = "abundances_sim") %>%  #add it to the state_nodes file
  left_join(Norwood_farm$nodes, by = "node_id") %>% select(layer_id,node_id,
                                          abundance, taxon) ##add taxon


## Call dataframe of habitats'area (summing different years -maybe the best option is to average it.. check later,
#standting tree doesn't have area.. so we used the area of the excluded (roads, buildings) to try)

areas<-read.csv("Data/habitatarea.csv", sep =";") %>% 
  filter(HabitatCode != "ST") %>% 
  mutate(HabitatCode = case_when(HabitatCode == "C"~ "CP",
                                 HabitatCode == "WU"~ "WD",
                                 TRUE~HabitatCode))

habitat_area <- areas %>% mutate(area_ave = (areas$Area_2007+  areas$Area_2008)/2)#area averaged across seasons
 

########## -- Create Management scenarios

##### -- Extensive

extensive_edgelist<- Norwood_farm$extended_ids %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>% 
  mutate(management = "E") %>% select(-habitat) %>% unique() #aggregate network

# estimate relative abundances of species in the aggregated network
ab_ext<-state_nodes_ab %>% select(-layer_id) %>% group_by(node_id,taxon) %>%
  mutate(abun = sum(abundance)) %>% distinct(abun) %>% group_by(taxon) %>% 
  mutate(tot_ab_taxon = sum(abun)) %>% #total abundance per taxon
  group_by(node_id) %>% 
  mutate(rel_ab=abun/tot_ab_taxon)#relative abundance per sp



# incorporate rel abundances to the edge list and calculate the weight (Product of relative abundances)
ext_edgelist_aggr<- extensive_edgelist %>% left_join(ab_ext, by = c("node_from" = "node_id")) %>% 
    left_join(ab_ext, by = c("node_to" = "node_id")) %>% 
  rename ("rel_ab_from" = "rel_ab.x", "rel_ab_to" = "rel_ab.y") %>% select(-weight) %>% 
  mutate(weight = rel_ab_from * rel_ab_to) %>% #calculate weight
  select(node_from,node_to,weight,management)

  


##### -- Semi - extensive (replace "WD" and "RG" for "CP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

sem_ext_edgelist_rem<- Norwood_farm$extended_ids %>% filter(layer_from != 9 & layer_from != 11) %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   #links from "WD" and "RG" removed
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

sem_ext_edgelist_rem<-sem_ext_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


##--  Create new habitats 

# create habitats CP to replace WD and RG
WD_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
   mutate (new_hab = 12, prev_hab = "WD", hab_cp = "CP")#links from "CP" to add as new habitat (12)

RG_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 13, prev_hab = "RG", hab_cp = "CP")#links from "CP" to add as new habitat (13)

#calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP) %>% left_join(habitat_area, 
                                  by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
  rename("area_prev_hab" ="area_ave.x", "area_CP" = "area_ave.y") %>% 
  mutate(mult_ab = (area_prev_hab/area_CP)) %>% #multplied abundances of CP for this value (to estimate according to the new habitat)
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
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist
  

## -- create state_node_list of the management scenario

sem_ext_edgelist_no_aggr<- rbind(sem_ext_edgelist_rem,new_habitats_ab)  #join new habitats and old habitats

#node_from       
state_node_sem_ext_from<- sem_ext_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                        taxon_node_from) %>% 
                        rename("node_id" ="node_from", "abundances" = "ab_node_from",
                               "taxon" = "taxon_node_from")
#node_to
state_node_sem_ext_to<- sem_ext_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                        taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to")


# final state nodes (calculate abundance and relative abundance of species)

state_node_sem_ext_agg<-rbind(state_node_sem_ext_from, state_node_sem_ext_to) %>% 
                    unique() %>% select(-habitat) %>% group_by(node_id,taxon) %>% 
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
  




##### -- Moderate (replace "WD","RG","MH"and "NH" for "CP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

mod_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 9 & layer_from != 11 & layer_from != 5 & layer_from != 6) %>% #links from "WD", "RG", "MH", and "NH" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

mod_edgelist_rem<-mod_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


#--  Create new habitats 

# create habitats CP to replace WD,RG, MH and NH (WD and RG were created before)
MH_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 14, prev_hab = "MH", hab_cp = "CP")#links from "CP" to add as new habitat (14)

NH_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 15, prev_hab = "NH", hab_cp = "CP")#links from "CP" to add as new habitat (15)

# calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP, MH_CP, NH_CP) %>% left_join(habitat_area, 
                                                  by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
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
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist


## -- create state_node_list of the management scenario

mod_edgelist_no_aggr<- rbind(mod_edgelist_rem,new_habitats_ab)  #join new habitats and old habitats

#node_from       
state_node_mod_from<- mod_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                              taxon_node_from) %>% 
  rename("node_id" ="node_from", "abundances" = "ab_node_from",
         "taxon" = "taxon_node_from")
#node_to
state_node_mod_to<- mod_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                            taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to")

# final state nodes (calculate abundance and relative abundance of species)

state_node_mod_agg<-rbind(state_node_mod_from, state_node_mod_to) %>% 
  unique() %>% select(-habitat) %>% group_by(node_id,taxon) %>% 
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





##### -- Semi - intensive (replace "WD","RG","MH","NH"and "GM" for "CP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

sem_int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 9 & layer_from != 11 &  layer_from != 5 &
           layer_from != 6 & layer_from != 2) %>% #links from "WD", "RG", "MH", "NH" and "GM" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

sem_int_edgelist_rem<-sem_int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]


#--  Create new habitats 

# create habitats CP to replace WD,RG, MH,NH and GM (WD,RG,MH and NH were created before)
GM_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 16, prev_hab = "GM", hab_cp = "CP")#links from "CP" to add as new habitat (16)


# calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP, MH_CP, NH_CP, GM_CP) %>% left_join(habitat_area, 
                                                                by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
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
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist


## -- create state_node_list of the management scenario

sem_int_edgelist_no_aggr<- rbind(sem_int_edgelist_rem,new_habitats_ab)  #join new habitats and old habitats

#node_from       
state_node_sem_int_from<- sem_int_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                      taxon_node_from) %>% 
  rename("node_id" ="node_from", "abundances" = "ab_node_from",
         "taxon" = "taxon_node_from")
#node_to
state_node_sem_int_to<- sem_int_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                    taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to")


# final state nodes (calculate abundance and relative abundance of species)

state_node_sem_int_agg<-rbind(state_node_sem_int_from, state_node_sem_int_to) %>% 
  unique() %>% select(-habitat) %>% group_by(node_id,taxon) %>% 
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





##### -- Intensive (replace "WD","RG","MH","NH","GM" and "SF" for "CP")


##-- Remove habitats from norwood (the ones to replace) and incorporate abundances and taxon 

int_edgelist_rem<- Norwood_farm$extended_ids %>% 
  filter(layer_from != 9 & layer_from != 11 &  layer_from != 5 &
           layer_from != 6 & layer_from != 2 & layer_from != 10) %>% #links from "WD", "RG", "MH", "NH","GM" and "SF" removed
  select(-layer_to) %>% rename("habitat" = "layer_from") %>%   
  left_join(state_nodes_ab, by = c("node_from" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of node_from
  left_join(state_nodes_ab, by = c("node_to" = "node_id",
                                   "habitat" = "layer_id")) %>%  #incorporate abundances and taxa of nodes_to
  rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
         "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y")

int_edgelist_rem<-int_edgelist_rem[,c(1,2,5,6,3,7,8,4)]



#--  Create new habitats 

# create habitats CP to replace WD,RG, MH,NH,GM and SF (WD,RG,MH,NH andGM were created before)
SF_CP<- Norwood_farm$extended_ids %>% filter(layer_from  == 1) %>% select(-layer_to,-layer_from) %>% 
  mutate (new_hab = 17, prev_hab = "SF", hab_cp = "CP")#links from "CP" to add as new habitat (17)


# calculate changes in the area between CP and the habitat to replace 
converted_area<-rbind(WD_CP, RG_CP, MH_CP, NH_CP, GM_CP, SF_CP) %>% left_join(habitat_area, 
                                                                       by = c("prev_hab" = "HabitatCode")) %>% 
  left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
  select(node_from,node_to,weight,new_hab,prev_hab,hab_cp,area_ave.x,area_ave.y) %>% 
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
         taxon_node_to,weight) %>% rename ("habitat" = "new_hab") #clean to match the rest of farm edgelist


## -- create state_node_list of the management scenario

int_edgelist_no_aggr<- rbind(int_edgelist_rem,new_habitats_ab)  #join new habitats and old habitats

#node_from       
state_node_int_from<- int_edgelist_no_aggr %>% select(habitat,node_from,ab_node_from,
                                                              taxon_node_from) %>% 
  rename("node_id" ="node_from", "abundances" = "ab_node_from",
         "taxon" = "taxon_node_from")
#node_to
state_node_int_to<- int_edgelist_no_aggr %>% select(habitat,node_to,ab_node_to,
                                                            taxon_node_to) %>% 
  rename("node_id" ="node_to", "abundances" = "ab_node_to",
         "taxon" = "taxon_node_to")


# final state nodes (calculate abundance and relative abundance of species)

state_node_int_agg<-rbind(state_node_int_from, state_node_int_to) %>% 
  unique() %>% select(-habitat) %>% group_by(node_id,taxon) %>% 
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

land_change_weighted_trial<-rbind(ext_edgelist_aggr,sem_ext_edgelist_aggr,mod_edgelist_aggr,
                          sem_int_edgelist_aggr,int_edgelist_aggr)



###### -- Exploratory analysis (TO CHECK)

## Histogram
library(cowplot)


hist_0.025<-ggplot(land_change_weighted_trial, aes(x=weight, fill =management))+
  geom_histogram(bins = 100,color='#e9ecef', alpha=0.55, position='identity')+
  scale_x_continuous(limits = c(0,0.025))
hist_0.025

hist_0.025_0.7<-ggplot(land_change_weighted_trial, aes(x=weight, fill =management))+
  geom_histogram(bins = 100,color='#e9ecef', alpha=0.55, position='identity')+
  scale_x_continuous(limits = c(0.025,0.7))
hist_0.025_0.7

upper_row<- plot_grid(hist_0.025,hist_0.025_0.7, 
                      ncol= 2)
upper_row


#x1<-land_change_weighted_trial %>% filter(management == "E")
#x2<-land_change_weighted_trial %>% filter(management == "SE")
#x3<-land_change_weighted_trial %>% filter(management == "M")
#x4<-land_change_weighted_trial %>% filter(management == "SI")
#x5<-land_change_weighted_trial %>% filter(management == "I")

# create multiple histogram
#hist_E<-hist(x1$weight, col='red', xlim=c(0,0.5))
#hist_SE<-hist(x2$weight,col='green',xlim=c(0,0.5))
#hist_M<-hist(x3$weight, col='blue', xlim=c(0,0.5))
#hist_SI<-hist(x4$weight, col='yellow', xlim=c(0,0.5))
#hist_I<-hist(x5$weight,col='purple',xlim=c(0,0.5))

#upper_row<- plot_grid(hist_E,hist_SE,hist_M,hist_SI,hist_I, 
                    #  ncol= 5)
#upper_row










################## --- CALCULATE DIRECT E(D)S PROVISION AND INDIRECT EFFECT ON ES


##### --  DIRECT E(D)S PROVISION


##  Create node_list per habitat/management 
node_list<-land_change_binary %>% pivot_longer(cols = 2:3, names_to = "species", values_to = "node_id") %>% 
                              select(-species) %>%  unique()


## Add information of ES per node (values 0-1)

nodes_ES_binary<- right_join(node_list, Norwood_farm$nodes, by = "node_id")%>% 
  select(habitat,management,node_id,taxon,"Crop production",
         "Pollination", "Crop damage", "Pest control", "Seed dispersal", "Butterfly watching", "Bird watching") %>% 
  group_by(management,habitat,node_id) %>% 
  gather("services","value", 5:11) #we conserve species that not directly provide ES because can serve as intermediate hop

nodes_ES_binary$management <- factor(nodes_ES_binary$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

  
## -- Estimate direct E(D)S provision
direct_ES_binary <- nodes_ES_binary %>% filter (value ==1) %>% 
  mutate (type = "D") %>% select(-value)

#write.csv(direct_ES_binary,"Data/Land_use_dir_binary.csv", row.names= FALSE)


## -- Direct provision Ratio ES/E(D)S

# ratio > 1 indicates more benefits, ratio = 1 balance between damages and benefits and ratio < 1 more damage

number_ES<-nodes_ES_binary %>% group_by(management) %>% 
  filter (value ==1 & services != "Crop damage") %>% summarise (ES =sum(value))#count ES provision

number_EDS<-nodes_ES_binary %>% group_by(management) %>% 
  filter (value ==1 & services == "Crop damage") %>% summarise (EDS =sum(value))#count EDS provision

total<-cbind(number_ES,number_EDS[,-1])

ratio_direct<-total %>% mutate(ratio_direct = ES/EDS)

#write.csv(ratio_direct,"Data/Land_use_rat_dir_binary.csv", row.names= FALSE)


## -- Exploratory plots direct provision

## ratio direct
  
ratio_direct_prov<-ratio_direct%>% gather("type","value", 2:3) %>% group_by(management) %>% 
  mutate(Total = sum(value)) %>% group_by(management,type) %>% 
  summarise(prop = value /Total) %>%  
  ggplot(aes(y=prop, x=management, fill = type)) + 
  geom_bar(position="stack", stat="identity")+ ggtitle("Ratio direct ES/EDS")+
  labs(x='Manegement', y="Prop of E(D)S") +theme_bw()+
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

ratio_direct_prov


## Num of EDS provided per management
  
ggplot_EDS_management<-direct_ES_binary %>% group_by(management) %>% count(services) %>% 
  ggplot(aes(y=n, x=management, fill = services)) + 
  geom_bar(position = "dodge" ,stat="identity")+ ggtitle("Direct EDS")+
  labs(x='Management', y="Direct EDS provided") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=13, color='black'),
        axis.text.x= element_text(size =11, angle = 90), 
        axis.text.y= element_text(size =11, angle = 90), 
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_blank(),
        legend.text = element_text(size = 5),
        legend.position = "bottom",
        legend.key.size = unit(0.7,"line"))

ggplot_EDS_management


#Parece que los servicios ecosystemicos no cambia mucho. Causas:

# 1) CP has a lot of species, why? so when we replace most habitats to CP we are increasing the richness 
#and its triggering the ES provision

# 2) Abundance-Area. Probablemente algunas areas son menores, entonces deberiamos reducir la abundnaia
# 3) Puede ser que E tenia muchas plantas que no producian un ES directo pero si muchos indirectos (eso lo vamos a ver cuando calculemos los efectos indirectos)





##### --  INDIRECT EFFECT ON E(D)S 


## -- Prepare dataframe

# Full list nodes with ES in the network (considering those that provide and not provide direct ES)

list_nodes_ES_provi<-nodes_ES_binary %>% ungroup() %>% select(-habitat,-management) %>%
  filter (value ==1) %>% unique # list of nodes that provide ES ( = no plants and ectoparasites)
list_nodes_ES_no_provi<- nodes_ES_binary %>% filter(node_id <=87 | node_id >=542) %>% 
  mutate(services = "None", value = 1) %>% ungroup() %>% 
  select (-habitat,-management) %>%  unique() # nodes that not provide any ES (plants and ectoparsites), ES assigned as None

list_nodes_ES<-rbind(list_nodes_ES_provi,list_nodes_ES_no_provi) #Total list of nodes with ES (with None)


# Add attributes of nodes to the edgelist

edge_list<- left_join(land_change_binary,list_nodes_ES, by = c("node_from"="node_id")) %>% 
  rename("taxon_from"="taxon", "services_from"="services",
         "value_from" = "value")%>% 
  left_join(list_nodes_ES, by = c("node_to"="node_id"))  %>% 
  rename("taxon_to"="taxon", "services_to"="services",
         "value_to" = "value") # %>% distinct() #remove distinct because we have more than one time the same interacition (we summed the links between layers to aggreate the data)


# Add inverted links (to make the code easier to program when calculate indirect interactions. It will not affect the results)

edge_list_inverted<- tibble(values = edge_list$habitat,edge_list$node_to,edge_list$node_from, edge_list$weight,
                            edge_list$management, edge_list$taxon_to,edge_list$services_to,
                            edge_list$value_to,  edge_list$taxon_from,edge_list$services_from,edge_list$value_from)
colnames(edge_list_inverted) <- c("habitat","node_from", "node_to","weight", "management", "taxon_from", "services_from",
                                  "value_from", "taxon_to", "services_to", "value_to")


# Combine both data frame to create the final edge list

edgelist_final<- bind_rows(edge_list, edge_list_inverted) %>% 
                select(-weight,-value_to,-value_from) # we are not using these anymore
  
edgelist_final<-edgelist_final[,c(4,1,2,3,5,6,7,8)]






#### - Calculate indirect effects considering 1 hop (node - node)


# Create objects to store

management = c()
habitat = c()
services_from = c()
node_from= c()
node_to = c()
taxon_from = c()
services_to = c()


# Reorder the dataframe to check indirect interaction in 1 hop
for (i in 1:nrow(edgelist_final)){
  
  management = c(management, edgelist_final$management[i])
  habitat = c(habitat, edgelist_final$habitat[i])
  services_from = c(services_from,edgelist_final$services_from[i])
  node_from = c(node_from,edgelist_final$node_from[i])
  node_to = c(node_to,edgelist_final$node_to[i])
  taxon_from = c(taxon_from, edgelist_final$taxon_from[i])
  services_to = c(services_to, edgelist_final$services_to[i])
  
  
}


Indirect_1hop_landuse_binary<-data.frame(management,habitat,services_from,node_from,node_to,taxon_from,services_to,hop = rep(1, length(services_from)), type = rep("I", length(services_from))) 


# Rearrange the output

# we remove duplicates rows where node_from = birds or butterflies cause they represent the same interaction. 
# This happens because each row represents an attribute and these taxons have 2 and 3 attributes per node.

rows_birds_butt<- Indirect_1hop_landuse_binary %>%
  filter(taxon_from == "Butterfly" | taxon_from == "Seed-feeding bird") %>%
  distinct(management, habitat, node_from, node_to, .keep_all = TRUE) # new subset after eliminating duplicate rows for node_from = birds and butterflies

int_without<-Indirect_1hop_landuse_binary %>% filter(!(taxon_from == "Butterfly" | 
                                                       taxon_from == "Seed-feeding bird")) #eliminate the interactions containing node_from =birds or butterflies from the original dataframe


Indirect_1hop_landuse_binary_2<-rbind(rows_birds_butt,int_without)#final dataframe containing indirect effects on ES via 1 hop


#write.csv(Indirect_1hop_landuse_binary_2,"Data/Land_use_ind_1hop_binary.csv", row.names= FALSE)





#### - Calculate indirect effects considering 2 hops (node 1 - node 2 - node 3, effect of node 1 on node 3'E(D)S via node 2)

Indirect_1hop<-read.csv("Data/Land_use_ind_1hop_binary.csv",
                                  sep =",") #load dataframe of indirect effects using 1 hop



# Calculate indirect effect on ES

# Create empty vectors
management = c()
habitat = c()
node_id= c()
node_int = c()
taxon_from = c()
services = c()
node_to = c()
services_to = c()



# Iterate to each row
for (i in 1:nrow(Indirect_1hop)){ #each row represents interaction between species
  
  j = Indirect_1hop$node_to[i] # check the node_to (intermediate species: node 2 in the title)
  l = Indirect_1hop$management [i] #check management where the target species for which we are detecting indirect effects on ES
  h = Indirect_1hop$habitat [i]
  
  
  # Filter dataframe (filter node 3's ES affected by node 2)
  
  services_int <- Indirect_1hop %>% filter(node_from == j, #filter to show node 2
                                           node_to != Indirect_1hop$node_from[i], #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
                                           management== l) %>% select(node_to,services_to)#select node 3 and its ES
  # Storage the results
  
  services_to <- c(services_to, unlist(services_int$services_to)) # add node 3 ES
  node_to <- c(node_to, unlist(services_int$node_to))# add identity of node 3
  node_int <- c(node_int,rep(j, nrow(services_int)))
  node_id <- c(node_id,rep(Indirect_1hop$node_from[i], nrow(services_int)))# add target (node 1) for which we are detected indirect effects 
  taxon_from<- c(taxon_from, rep(Indirect_1hop$taxon_from[i], nrow(services_int)))
  services <- c(services, rep (Indirect_1hop$services_from[i], nrow(services_int)))# direct ES provided by the target node
  management <- c(management, rep(l, nrow(services_int)))
  habitat <- c(habitat, rep(h, nrow(services_int)))
}

Indirect_2hop<- data.frame(management,habitat,node_id,taxon_from,services,node_int,
                node_to, services_to,type = rep("I", length(services_to)), hop = rep(2,length(services_to)))


# Remove duplicate interactions when node_to = birds or butterflies

Indirect_2hop_final<- Indirect_2hop %>% unique() 

#write.csv(Indirect_2hop_final,"Data/Land_use_ind_2hop_binary.csv", row.names= FALSE)

Indirect_2hop<-read.csv("Data/Land_use_ind_2hop_binary.csv",
                        sep =",") #load dataframe of indirect effects using 1 hop



# Join both 1 and 2 hops indirect effects dataframes

# 1 hop
Indirect_1hop_m<-Indirect_1hop  %>% rename("services" ="services_from",
                                           "node_id" = "node_from",
                                           "taxon" = "taxon_from") %>% 
  mutate(node_int = NA)

Indirect_1hop_m<-Indirect_1hop_m[,c(1,2,3,4,6,10,5,7,8,9)]

# 2 hop
Indirect_2hop_m<-Indirect_2hop %>% rename("taxon" = "taxon_from") 
Indirect_2hop_m<-Indirect_2hop_m[,c(1,2,5,3,4,6,7,8,9,10)]

#  Total Indirect effect of ES

I_ES<- rbind(Indirect_1hop_m,Indirect_2hop_m)

#write.csv(I_ES,"Data/Land_use_ind_binary.csv")



################## --- ESTIMATE OUTPUT OF INDIRECT EFFECTS 

# For each interaction we assigned the following outputs:
# + :provide ES, increase ES or decrease crop damage
# - : provide EDS (crop damage), decrease ES provision or increase crop damage


## -- Define vector of trophic groups to state the conditions

plants = 1:87
crops = 88:93
flow_vis = 94:334
aphid = 335:362
pri_par = 363:373
sec_par = 374:380
leaf_par = 381:473
seed_ins = 474:492
seed_bird = 493:504
seed_rod = 505:508
butt = 509:524
seed_ins_par = 525:541
rod_par = 542:549



## --1 HOP 

hop_1 <- read.csv("Data/Land_use_ind_binary.csv", sep =",", row.names = 1) %>% 
  filter(hop == 1, services_to !="None") #remove indirect effect that not affect any ES


# We assigned the indirect output according to node_id and node_to

output_ES_1hop<- hop_1 %>%  
  mutate(output = case_when(
    (node_id %in% aphid| node_id %in% seed_bird | node_id %in% seed_ins |
       node_id %in% seed_rod) & services_to == "Crop production"  ~ "-", #aphids, birds, rodents, seed inds reduce crop production
    
    node_id %in% plants & node_to %in% seed_bird & services_to == "Crop damage" ~ "-",#plants--->+birds--->+seed predation
    
    node_id %in% plants & (node_to %in% aphid| node_to %in% seed_bird | node_to %in% seed_ins |
                             node_to %in% seed_rod) & services_to == "Crop production"  ~ "-", #plants that increase birds, rodents, insects, aphids population, increase crop damage
    
    node_id %in% crops & (node_to %in% aphid| node_to %in% seed_bird | node_to %in% seed_ins |
                            node_to %in% seed_rod) & services_to == "Crop production"  ~ "-", #same for crops
    
    node_id %in% aphid & services_to == "Pest control" ~ "+", #aphids eat plants and crops inceasing their abundances and increasing the resource for pest control sps
    
    
    (node_id %in% plants |  node_id %in% crops)  &   
      (services_to == "Seed dispersal" | services_to == "Bird watching") ~ "+", #plants and crops increase population of birds and hence SD and bird watching
    
    (node_id %in% plants |  node_id %in% crops)  &   
      services_to == "Crop damage" ~ "-", #plants and crops increase the population of birds, aphids, rodents and insects  and hence the crop damage
    
    TRUE ~ "+"
  ))


## -- 2 HOPS  

hop_2 <- read.csv("Data/Land_use_ind_binary.csv", sep =",",row.names = 1) %>% 
  filter(hop == 2, services_to !="None") #remove indirect effect that not affect any ES


# We assigned the indirect output according from "node_id" to "node_to" via "node_int"

output_ES_2hops<- hop_2 %>%  
  mutate(output = case_when( 
    
    (node_id %in% plants | node_id %in% crops) &  (node_int%in%aphid | node_int%in%seed_ins) &
      services_to == "Pest control"  ~ "+",    #plants,crops --> +aphids,ins -->+parasitoides -->+ pestcontrol
    
    
    (node_id %in% plants | node_id %in% crops) &  (node_int%in%aphid | node_int%in%seed_bird| 
                                                     node_int%in%seed_ins | node_int%in%seed_rod) &
      services_to == "Crop production"  ~ "-",     #plants,crops --> + seed predators --> - crop --> - crop production
    
    
    (node_id %in% plants | node_id %in% crops) &  (node_int%in%flow_vis | node_int%in%pri_par|
                                                     node_int%in%sec_par | node_int%in%leaf_par | node_int%in%butt |  node_int%in%rod_par |
                                                     node_int%in%seed_ins_par) & services_to == "Crop production" ~ "+",      #plants,crops --> + seed predators par  or poll--> + crop --> + crop production
    
    
    (node_id %in% plants | node_id %in% crops) &  (node_int%in%pri_par| node_int%in%sec_par |
                                                     node_int%in%leaf_par |   node_int%in%rod_par |  node_int%in%seed_ins_par) &
      (services_to == "Crop damage")  ~ "+",      # plants, crops --> + parasitoides --> - herbuvires and seed predators crop damage (es un efecto +)
    
    
    (node_id%in%aphid | node_id%in%seed_bird| node_id%in%seed_ins |node_id%in%seed_rod) &
      (node_int%in%pri_par| node_int%in%sec_par |node_int%in%leaf_par | node_int%in%rod_par |
         node_int%in%seed_ins_par) & (services_to == "Crop production" | 
                                        services_to == "Crop damage") ~ "+",      #seed predators--> + pop. parasitoides --> - seed predators -> - crop damage and + crop productio
    
    
    (node_id%in%flow_vis | node_id%in%butt) & (node_int%in%plants| node_int%in%crops) &
      (services_to == "Pollination" | services_to == "Seed dispersal" | services_to == "Butterfly watching"| 
         services_to == "Bird watching"| services_to == "Pest control") ~ "+", # flower visitors and butt --> + plants,crops--> + pop, pollinators, parasitoides, birds --> + poll, pest control, seed disp and bird watching
    
    
    (node_id%in%flow_vis | node_id%in%butt) & (node_int%in%plants| node_int%in%crops) &
      (services_to == "Crop damage")  ~ "-", # flower visitors and butt --> + plants,crops--> + pop seed predators --> + crop damage 
    
    
    (node_id%in%aphid | node_id%in%seed_bird| node_id%in%seed_ins |node_id%in%seed_rod) &
      (node_int%in%plants| node_int%in%crops) & (services_to == "Pollination" | 
                                                   services_to == "Butterfly watching"| services_to == "Bird watching"| services_to == "Pest control"| 
                                                   services_to == "Seed dispersal") ~ "-",     # seed predators --> - pop crops --> - pop birds, flower vis, parasitodes --> - pest control, watching, pollination
    
    (node_id%in%aphid | node_id%in%seed_bird| node_id%in%seed_ins |node_id%in%seed_rod) &
      (node_int%in%plants| node_int%in%crops) & (services_to == "Crop damage") ~ "+",     # seed predators --> - pop crops --> - pop seed predators --> - crop damage (+)
    
    
    (node_id%in%pri_par| node_id%in%sec_par |node_id%in%leaf_par |node_int%in%rod_par |
       node_id%in%seed_ins_par) & (node_int%in%plants| node_int%in%crops) & 
      (services_to == "Pollination" | services_to == "Butterfly watching"|
         services_to == "Bird watching"| services_to == "Pest control"| 
         services_to == "Seed dispersal") ~ "+",    # par --> + pop plants --> + pop birds, flower vis, par --> + pest control, poll, bird and butt watching
    
    
    (node_id%in%pri_par| node_id%in%sec_par |node_id%in%leaf_par |node_int%in%rod_par |
       node_int%in%seed_ins_par) & (node_int%in%plants| node_int%in%crops) & 
      (services_to == "Crop damage") ~ "-",  # par --> + pop plants --> + crop damage (-)
    
    
    (node_id%in%pri_par| node_id%in%sec_par |node_id%in%leaf_par |node_int%in%rod_par |
       node_id%in%seed_ins_par) & (node_int%in%aphid | node_int%in%seed_bird| 
                                     node_int%in%seed_ins | node_int%in%seed_rod) &
      services_to == "Crop production" ~ "+", # par --> + pop seed predators --> + pop crops --> + crop production
    
    
    (node_id%in%pri_par| node_id%in%sec_par |node_id%in%leaf_par |node_int%in%rod_par |
       node_id%in%seed_ins_par) & (node_int%in%aphid | node_int%in%seed_bird| 
                                     node_int%in%seed_ins | node_int%in%seed_rod) &
      services_to == "Pest control" ~ "-" # par --> + pop seed predators --> + pop parasitoide --> - pest control
    
  ))


### -- Final dataframe output of indirect effects  ---

output_ES<-rbind(output_ES_1hop,output_ES_2hops)

#write.csv(output_ES,"Data/Land_use_output_ind_binary.csv", row.names= FALSE)



##### -- Indirect provision Ratio output +/-
output_ind_ES <- read.csv("Data/Land_use_output_ind_binary.csv", sep =",") 

# ratio > 1 indicates more benefits, ratio = 1 balance between damages and benefits and ratio < 1 more damage

number_positive<-output_ind_ES %>% group_by(management) %>% 
  filter (output == "+") %>% summarise (positive = n())#count + outputs

number_negative<-output_ind_ES %>% group_by(management) %>% 
  filter (output == "-") %>% summarise (negative = n())#count - outputs

total<-cbind(number_positive,number_negative[,-1])

ratio_indirect<-total %>% mutate(ratio_direct = positive/negative)



## -- Exploratory plots direct provision

## ratio direct

ratio_indirect_prov<-ratio_indirect%>% gather("type","value", 2:3) %>% group_by(management) %>% 
  mutate(Total = sum(value)) %>% group_by(management,type) %>% 
  summarise(prop = value /Total) %>%  
  ggplot(aes(y=prop, x=management, fill = type)) + 
  geom_bar(position="stack", stat="identity")+ ggtitle("Ratio indirect +/-")+
  labs(x='Manegement', y="Prop of outputs") +theme_bw()+
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

ratio_indirect_prov




  
  

################## --- CREATE MANAGAMENT SCENARIOS (habitat loss)
#in this scenario of presence/absence of links, change habitats are equal to eliminate habitats.. 

##### -- Extensive

extensive_edgelist<- Norwood_farm$extended_ids %>% 
  select(-layer_from,-layer_to) %>% unique() %>% #unique because it's presence/absence
mutate(management = "E") 

##### -- Semi - extensive (replace "WD" and "RG" for "CP")
sem_ext_edgelist<- Norwood_farm$extended_ids %>% filter(layer_from != 9 & layer_from != 11) %>% 
  select(-layer_from,-layer_to) %>% unique() %>% mutate(management = "SE") 


##### -- Moderate (replace "WD","RG","MH"and "NH" for "CP")
mod_edgelist<- Norwood_farm$extended_ids %>% filter(layer_from != 9 & layer_from != 11 &
                                                          layer_from != 5 & layer_from != 6) %>% 
  select(-layer_from,-layer_to) %>% unique() %>% mutate(management = "M") 


##### -- Semi - intensive (replace "WD","RG","MH","NH"and "GM" for "CP")
sem_int_edgelist<- Norwood_farm$extended_ids %>% filter(layer_from != 9 & layer_from != 11 &
                                                      layer_from != 5 & layer_from != 6 &
                                                      layer_from != 2) %>% 
  select(-layer_from,-layer_to) %>% unique() %>% mutate(management = "SI") 


##### -- Intensive (replace "WD","RG","MH","NH", "GM" and "SF" for "CP")
int_edgelist<- Norwood_farm$extended_ids %>% filter(layer_from != 9 & layer_from != 11 &
                                                          layer_from != 5 & layer_from != 6 &
                                                          layer_from != 2 & layer_from != 10) %>% 
  select(-layer_from,-layer_to) %>% unique() %>% mutate(management = "I") 


##### --  Final dataframe

land_change_removal<-rbind(extensive_edgelist,sem_ext_edgelist,mod_edgelist,
                          sem_int_edgelist,int_edgelist)


################## --- CALCULATE DIRECT E(D)S PROVISION AND INDIRECT EFFECT ON ES


##### --  Direct E(D)S provision

## Add information of ES per node (values 0-1)

nodes_ES<- Norwood_farm$nodes %>% 
  select(node_id,taxon,"Crop production",
         Pollination, "Crop damage", "Pest control", "Seed dispersal", 
         "Butterfly watching", "Bird watching") %>% 
  gather("services","value", 3:8) #we conserve species that not directly provide ES because can serve as intermediate hop


## Estimate direct provision (PENSAR ACA EL NUEVO SET DE DATOS QUE TENGO, CAPAS PONER UN JOINT CON LOS DATOS QUE GENERE)

direct_ES <- nodes_ES %>% filter (value ==1) %>% 
  mutate (type = "D") %>% select(-value)

