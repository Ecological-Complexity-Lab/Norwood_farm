# In this code, we create a multilayer object to represent the farm network. While we do not analyze the network as
#a multilayer structure, the object organizes information such as the node list for each habitat, the edge list, 
#and the attributes of nodes within each habitat


## -- Load libraries --------------------------------------------------------------------------------------------------------
library(emln) # multilayer package
library(readr)
library(ggplot2)
library(tidyverse)

## -- get_data--------------------------------------------------------------------------------------------------------
setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")


###### -- Create multilayer object  ------------------------------------------------------------------------
#To create the multilayer object we need:

# 1) List of interactions between species (presence/absence), 
# 2) habitats (layers) attribute (names of habitats),
# 3) attributes of species (physical nodes), such as trophic guild and potential ES provided



## -- Interaction list
edges <- read.csv("Data/elist_nore.csv", sep = ",") # generated in "1_Abundances.R"

edges_format<-edges %>% rename("from"="lower") %>% rename("to"="upper") %>% 
  mutate(weight =1) # weight is presence/absence

list_of_layers<- split(edges_format, edges_format$habitat)
list_of_layers <- lapply(list_of_layers, function(sub_df) subset(sub_df, select = -habitat)) # remove habitat from the edge list to keep the format (from, to, weight)


## -- habitats' attributes
layer_attrib <- tibble(layer_id=1:10,
                       layer_name=c('CP','GM','LP','MH','NH', 'NL','PP','RG',
                                    'SF','WD'))

## -- physical nodes' attributes
nodes <- tibble(read.csv("Data/nodes.csv")) # generated in "1_Abundances.R"

# add potential provision of ES to species according to criteria for each trophic guild (detailed in the supplementary material)
services_pre <- tibble(read.csv("Data/Raw_data/service_edgelist.csv", sep = ";")) %>%  #data frame without antagonist
rename (node_name = lower, ESS = upper) %>%
  pivot_wider(names_from = ESS, values_from = weight, values_fill = 0) %>%
  left_join(nodes, by = "node_name") %>% select(-node_id)  #list of ecosystem (dis)services that each species provides
  
# remove duplicate rows containing butterflies in 02fv (because they are a separated trophic guild)
list.butt.flw<- c("02FV.Maniola jurtina","02FV.Pieris brassicae","02FV.Polyommatus icarus",
                    "02FV.Pyronia tithonus") #list of butterflies

services<-services_pre %>% filter(!(node_name %in%list.butt.flw)) #remove duplicated butt from dataset of services

# we keep pollination  service to just for flower visitors recorded transporting pollen in repositories 
flw_poll  <- read.csv("Data/pollinators_sp.csv", sep = ",") #list of flower visitors that transport pollen grain

tot_pol<-services %>% filter(taxon == "Flower-visiting" )

flw_nopoll<- services %>% select(node_name,taxon) %>% 
        filter(taxon == "Flower-visiting") %>% filter (!(node_name%in%flw_poll$upper)) #flower visitors that not provide pollination

services_pre2<-services %>% filter (!(node_name%in%flw_nopoll$node_name)) 

# we incorporate species that not directly provide E(D)S
phy_node_atr_tax_es<- dplyr::left_join(nodes, services_pre2, by = "node_name") %>% select(-node_id, -taxon.y) %>% 
  rename("taxon" = "taxon.x") %>% 
  replace(is.na(.), 0) %>% rowwise() %>%  #we assigned "0" to species that we don't know if provide ES
  mutate(ES = rowSums(across(c("Crop production","Pollination","Pest control",
                               "Seed dispersal", "Butterfly watching", "Bird watching"))),
         DES = rowSums(across(c("Crop damage"))))
                              


## -- Create multilayer object
Norwood_farm<-create_multilayer_network(list_of_layers = list_of_layers, bipartite = F, directed = T,
                                  interlayer_links = NULL, layer_attributes = layer_attrib, state_node_attributes = NULL, physical_node_attributes = phy_node_atr_tax_es)
#view(Norwood_farm$layers)
#view(Norwood_farm$nodes)
#view(Norwood_farm$extended_ids)
#view(Norwood_farm$state_nodes)

#saveRDS(Norwood_farm, file="Data/Norwood_farm.RData")



