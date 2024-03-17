############################### We calculated pagerank of species using the code written by Aislyn Keyes (Keyes, A.A., McLaughlin, J.P., Barner, A.K. et al. An ecological network approach to predict ecosystem service vulnerability to species losses. Nat Commun 12, 1586 (2021). https://doi.org/10.1038/s41467-021-21824-x)

#PageRank allow us to identify the important of species on indirectly supporting ecosystem services

#load packages
library(igraph)
library(dplyr)

#Before estimating PageRank we have to create a network with:

# Nodes as E(D)S and species.
# Directed edges from E(D)S to species when the species provide directly the E(D)S.
# Directed edges between species pointing from consumers to resources.


setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio") #set directory




#### 1. Arrange the data

### Create node list including species and E(D)S
node_species_list<-read.csv("Data/Land_use_rat_state_nodes_CP_intense.csv", sep = ",") %>%  #load list of species 
                  ungroup() %>% select(node_id)%>% unique() %>% arrange(node_id) %>% 
                  mutate(type = "species") 

ES_list<-read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep = ",") %>% #load edgelist of nodes and direct ES
                  ungroup() %>% select(services) %>% unique() %>% 
                  mutate(node_id = seq(max(node_species_list$node_id)+1,max(node_species_list$node_id)+7)) %>% 
                  rename ("type" = "services")#list of ES as nodes

nodes<-rbind(node_species_list,ES_list)


### Create edge list representing species-species and E(D)S-species

## edge list species-species from consumers to resources
species.species_edge<- read.csv("Data/Land_use_rat_edgelist_weighted_CP_intense.csv", sep = ",") %>% #load edge list species-species
                      select(-weight)

#flip direction of edges from consumer to resources
species.species_edge_inverted<- tibble(values = species.species_edge$node_to,species.species_edge$node_from, 
                                       species.species_edge$management)
colnames(species.species_edge_inverted) <- c("node_from", "node_to", "management")

edge.list.sp_sp<-species.species_edge_inverted #edge list sp-sp
  

## edge list E(D)S- species pointing from E(D)S to species so random walker walks from E(D)S
ES.species_edge<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep = ",") %>% #load edge list species-E(D)S
                select(node_id,services,management) %>% 
                left_join(ES_list, by = c ("services" = "type")) %>% #assign node_id to each services (previously defined) %>% 
                rename ("node_from" = "node_id.y", #flip directions of edges from ES to species
                                                    "node_to" = "node_id.x") %>% 
                select (node_from,node_to,management)


### Merge both edge lists
edge_list<-rbind(edge.list.sp_sp,ES.species_edge)


#### PRUEBA FILTER JUST EMPIRICAL

edge_list
#edge_list_E<-edge_list %>% filter(management == "E") %>% select(-management)

# create graph object using edges.ES
net.ES <- graph.data.frame(edge_list_E,
                           directed = T,
                           vertices = nodes)
plot(net.ES) # check to make sure arrows are flipped

net.ES




######## 1. Estimate species importance (Pagerank)

# we run pagerank for each ES to look at species importance.  The higher probability can be
#interpreted as “more important” to the ecosystem service node.

# for each of the services, we'll run a pagerank algorithm
# to account for spp directly providing, we will replace prob. with 0 or NA
# if they are directly linked to a service




### ACA INTENTAR HACER EL LOOP CONSIDERANDO CADA MANEJO










##Now I will try to do a loop for the differents ES

management = c()
services = c()
sp_rank = NULL


for (i in 552:558){ #for each E(D)S
  
  j = nodes[i,2] #identity of E(D)S
  
  #Pagerank function
  pers.page <- rep(0, nrow(nodes)) %>% replace(i,1) #create personalized pagerank 
 
  page.rank<-data.frame (NodesID=nodes$node_id, 
              prob=page_rank(graph=net.ES, damping = 0.85, directed = T, 
              personalized = pers.page)$vector)

  page.rank.spp<- page.rank %>% filter(!(NodesID>551)) # remove nodes representing E(D)S
  
  #ACA ES DONDE TENGO QUE FILTRAR POR MANAGEMENT
  page.rank.direct <- data.frame(NodesID = ifelse(edge_list_E$node_from == i,
                                                 edge_list_E$node_to,NA))  # assign NA to species providing directly the particular ES
 
  page.rank.direct <- c(na.omit(page.rank.direct)) # and remove those species
  
  page.rank.ind.support <- page.rank.spp[!(page.rank.spp$NodesID %in% page.rank.direct$NodesID),] # keep only ind supporting spp! 
  
  # Storage the results)
  
  sp_rank<- rbind(sp_rank, page.rank.ind.support)
  services <- c(services, rep(j,nrow(page.rank.ind.support))) # services
  #management  CREATE WHEN I DO NESTED

}

page_rank_sp<-data.frame(sp_rank,services)







##########################################################################
##########################################################################
##########################################################################


# we have already exported .csv file for each service individually
# we want to get an mean prob for each species across all services for the main ES robustness

all.support <- rbind(water.filt.support,wathunt.support,carbon.seq.support,fishery.support,birdwatch.support)
all.support$SpeciesID <- as.factor(all.support$SpeciesID)
mean.support <- aggregate(prob ~ SpeciesID, all.support, mean) # calculate mean prob. for each spp.

attach(mean.support)
mean.support <- mean.support[order(-prob),]
detach(mean.support)

write.csv(mean.support, "EPB_IndirectAll.csv")



## Crop production (node 522)

crop.pro <- data.frame(NodesID=nodes$node_id, prob=page_rank(graph=net.ES, damping = 0.85, directed = T, 
                                                             personalized = c(rep(0,times=nrow(nodes)-nrow(nodes[nodes$type!="species",])),
                                                                              1,0,0,0,0,0,0))$vector)

crop.pro.SPP <- crop.pro%>% filter(!(NodesID>521)) # remove nodes representing E(D)S

crop.pro.direct <- data.frame(NodesID = ifelse(edge_list_E$node_from=="552",
                                               edge_list_E$node_to,NA))  # assign NA to species providing directly the particular ES
crop.pro.direct <- c(na.omit(crop.pro.direct)) # and remove those species 

crop.pro.ind.support <- crop.pro.SPP[!(crop.pro.SPP$NodesID %in% crop.pro.direct$NodesID),] # keep only ind supporting spp! 

crop.pro.ind.sup<- crop.pro.ind.support %>% arrange(desc(prob)) #order species according to their ind importance in affecting the ES






