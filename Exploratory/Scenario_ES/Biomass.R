####################### Assing the biomass per ind/species or trophic group

library(bipartite)
library(vegan)
library(sna)
library(tidyverse)

setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

### Upload list of nodes
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object

### Create list of species
species_list<-Norwood_farm$nodes %>% select(node_id,node_name,taxon) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) #keep just the species name of most rows



####### ---  Look for biomass/ind per species in different repositories

## Aphid-parasitoids (Brose et al 2005 - repository (https://doi.org/10.1890/05-0379)

#SOMETHING IS WRONG BECAUSE THE MEAN BIOMASS IS THE SAME FOR ALMOST ALL SPECIES

aphid.par.repo<-read.delim("Data/biomass_repositories/bodysizes_2008.txt") 

filter_sps<-aphid.par.repo %>% filter(Taxonomy.consumer%in%species_list$node_name |
                                        Taxonomy.resource%in%species_list$node_name) %>% 
            select(Taxonomy.consumer,Mean.mass..g..consumer,Taxonomy.resource,
                   Mean.mass..g..resource) %>%  #filter sps from the database  
                unique() #the biomass is already averaged

#Split consumer and resource information
upper<- filter_sps[,1:2] %>% rename("node_name" = "Taxonomy.consumer", "biomass.g" ="Mean.mass..g..consumer")
lower<-filter_sps[,3:4] %>% rename("node_name" = "Taxonomy.resource", "biomass.g" ="Mean.mass..g..resource")

aphid.par.bio<-rbind(upper,lower)

#function to keep the first two words in species name from the repository
keep_first_two_words <- function(text) {
  words <- str_split(text, "\\s+")[[1]]
  paste(words[1:2], collapse = " ")
}

#apply function 
aphid.par.bio<-rbind(upper,lower) %>% mutate( node_name = sapply(node_name, keep_first_two_words)) %>% 
                                  mutate(node_name = gsub("\\bNA\\b", "", node_name)) #eliminate extra text in some columns

#check for those species in the repository that match in our dataframe
biomass.aphid.par<- species_list %>% left_join(aphid.par.bio, by = "node_name") %>% unique()


##  Bird and Mammals species

birds.repo<-read.delim("Data/biomass_repositories/BirdFuncDat.txt")

prueba<-birds.repo %>% select(Scientific,BodyMass.Value) %>% group_by(Scientific,BodyMass.Value) %>% 
        count()


mam.repo<-read.delim("Data/biomass_repositories/MamFuncDat.txt")
