
#We try to create the simplest scenario and check the degree of ES that provided

library(tidyverse)
library(readr)
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

######################## LOAD DATA FILES #######################
edges <- tibble(read.csv("Data/norwood_cleanest_edgelist.csv")) 

services <- tibble(read.csv("Data/service_edgelist.csv")) %>% #Some nodes don't have ES
  rename (taxon = lower, ESS = upper) %>%
  pivot_wider(names_from = ESS, values_from = weight, values_fill = 0) %>%
  mutate(taxa_type = str_split(taxon, pattern = "[.]")[[2]][1]) #list of ecosystem (dis)services that each species provides

#### Ecosystem services provided by Extensive scenario

#Node list of Extensive scenario 
nodes_lower<-edges %>% select(habitat,lower)
nodes_upper<-edges %>% select(habitat,upper)
names(nodes_lower)<-names(nodes_upper)

nodes_extensive<-rbind(nodes_lower,nodes_upper) %>% group_by(habitat) %>% unique() %>% 
  rename("taxon"="upper")

#Create services list of Extensive scenario (considering every time a species appears in a habitat)

services_extensive <- services %>%  filter(taxon %in% c(nodes_extensive$taxon))

#joint dataframes
nodes_services_ext<- dplyr::right_join(nodes_extensive, services_extensive, by = "taxon") %>% select(-taxa_type)

#Degree of ecosystem provision

ES_habitat_ext<-nodes_services_ext %>% select(-habitat,-taxon) %>%  summarise(across(everything(), ~ sum(., na.rm = TRUE)))

ES_scenario_ext<-ES_habitat_ext %>% select(-habitat) %>%  summarise(across(everything(), ~ sum(., na.rm = TRUE)))



#### Ecosystem services provided by Intensive scenario

#Create edgelist of intensive scenario
edges_intensive<- edges %>%  filter(habitat == "CP" | habitat == "LP" |habitat == "LU" | 
                                      habitat == "NL" |habitat == "PP")

#Node list of intensive scenario 
nodes_lower<-edges_intensive %>% select(habitat,lower)
nodes_upper<-edges_intensive %>% select(habitat,upper)
names(nodes_lower)<-names(nodes_upper)

nodes_intensive<-rbind(nodes_lower,nodes_upper) %>% group_by(habitat) %>% unique() %>% 
  rename("taxon"="upper")

#Create services list of intensive scenario (considering every time a species appears in a habitat)

services_intensive <- services %>%  filter(taxon %in% c(nodes_intensive$taxon))

#joint dataframes
nodes_services_int<- dplyr::right_join(nodes_intensive, services_intensive, by = "taxon") %>% select(-taxa_type)

#Degree of ecosystem provision

ES_habitat_int<-nodes_services_int %>% select(-habitat,-taxon) %>%  summarise(across(everything(), ~ sum(., na.rm = TRUE)))

ES_scenario_int<-ES_habitat_int %>% select(-habitat) %>%  summarise(across(everything(), ~ sum(., na.rm = TRUE)))



## Box Plots comparing ecosystem services between Extensive and Intensive

ES_general<-rbind(ES_scenario_ext,ES_scenario_int)

Es_general<-cbind(Scenario =c("Ext","Int"), ES_general)

ES_general_final<-Es_general %>% gather("ES","state_node_providers",2:8)

ggplot(ES_general_final, aes(x = factor(ES), y = state_node_providers, fill = Scenario, colour = Scenario)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x="Ecosystem services (ES)", y="Number of species providing ES") +
  theme_bw() + 
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=11, color='black'),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.title = element_text(size=13, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

## SUBIR GITHUB!
