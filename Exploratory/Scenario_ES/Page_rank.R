############################### We calculated pagerank of species using the code written by Aislyn Keyes (Keyes, A.A., McLaughlin, J.P., Barner, A.K. et al. An ecological network approach to predict ecosystem service vulnerability to species losses. Nat Commun 12, 1586 (2021). https://doi.org/10.1038/s41467-021-21824-x)

#PageRank allow us to identify the important of species on indirectly supporting ecosystem services

#load packages
library(igraph)
library(dplyr)
library(tidyverse)

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




######## 1. Estimate species importance (Pagerank)


### Create the network for each management using the edge list

network.ES<-list()

for (i in unique(edge_list$management)){# for each treatment
  
  #Filter data
  edge_list_management<-edge_list %>% filter(management== i) #filter edge list according to the management
  
  #Create igraph object
  net.ES <- graph.data.frame(edge_list_management, 
                             directed = T,
                             vertices = nodes)
  
  # Storage the results
  list_name <- paste0(i, i) 
  network.ES[[i]] <- net.ES
}




### Test the importance of species using PageRank

# we run pagerank for each ES to look at species importance.  The higher probability can be
#interpreted as “more important” to the ecosystem service node.

# for each of the services, we'll run a pagerank algorithm
# to account for spp directly providing, we will replace prob. with 0 or NA
# if they are directly linked to a service



management = c()
services = c()
sp_rank = NULL


for (i in 552:558){ #for each E(D)S
  
  j = nodes[i,2] #identity of E(D)S
  
for (m in unique(edge_list$management)) {  #for each habitat management
  
    edge_list_management <- edge_list %>% filter(management == m) #select the edge list
    
  #Pagerank function
  pers.page <- rep(0, nrow(nodes)) %>% replace(i,1)  
  
  page.rank<-data.frame (NodesID=nodes$node_id, 
                         prob=page_rank(graph=network.ES[[m]], #create personalized pagerank for each habitat management
                                        damping = 0.85, directed = T, 
                                        personalized = pers.page)$vector)
  
  page.rank.spp<- page.rank %>% filter(!(NodesID>551)) # remove nodes representing E(D)S
 
  page.rank.direct <- data.frame(NodesID = ifelse(edge_list_management$node_from == i,
                                                  edge_list_management$node_to,NA))  # assign NA to species providing directly the particular ES
  
  page.rank.direct <- c(na.omit(page.rank.direct)) # and remove those species
  
  page.rank.ind.support <- page.rank.spp[!(page.rank.spp$NodesID %in% page.rank.direct$NodesID),] # keep only ind supporting spp! 
  
  # Storage the results
  
  sp_rank<- rbind(sp_rank, page.rank.ind.support)
  services <- c(services, rep(j, nrow(page.rank.ind.support))) # services
  management <- c(management, rep (m,nrow(page.rank.ind.support))) #management
  
  }
}

page_rank_sp<-data.frame(sp_rank,services,management) %>% rename("pagerank" = "prob") %>% 
              select(management,NodesID,services,pagerank)

## Calculate the mean prob (importance) for each species across all services

mean_page_rank<-page_rank_sp %>% group_by(management,NodesID)  %>% 
  mutate(all_mean = mean(pagerank), #calculate the mean and standard error
         all_std = sd(pagerank)/ sqrt(n())) %>% 
  gather("services","pagerank",5:6) %>% select(management,NodesID,services,pagerank)


## Merge into the final dataframe
page_rank_sp_final<-rbind(page_rank_sp,mean_page_rank) %>% ungroup() %>% unique() #eliminate duplicated rows

#write.csv(page_rank_sp_final,"Data/page_rank_sp.csv", row.names= FALSE)





### PLOT CIRCULAR GRAPH OF THE FIRST 50 MOST IMPORTANT SPECIES IN EXTENSIVE
library(ggplot2)
library(circlize)
library(viridis)
library(ComplexHeatmap)

#we plot a circular plot using each layer as management and values the average importance of species to
#indirectly provide E(D)S

###### Arrange dataframe

page_rank_circ<-read.csv("Data/page_rank_sp.csv", sep = ",") %>%
                  filter(services == "all_mean") %>% #just average for now (maybe add error bars in the future)
                  spread(management,pagerank) %>% #rearrange dataframe
                  select(NodesID,services,E,SE,M,SI,I)  %>% 
                 arrange(desc(E)) %>% slice(1:50) %>% #select the fifty more important species
                  sample_n(nrow(.))

sp_names<-page_rank_circ$NodesID #create temporal species name to filter the big database

##### set up parameters and structure


# Define color of each layer and sps

#layer
color = colorRamp2(seq(min(0.000088), max(0.06), length = 100),viridis(100)) #color layer


#sps
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object

species_list<-Norwood_farm$nodes %>% select(node_id,node_name,taxon) %>% #clean names
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) %>% #keep just the species name of most rows
  filter(node_id%in%sp_names) %>% #filter species in the dataframe
  mutate(color_sp =case_when(taxon == "Plant"~ "#00C1AB", #assign color to species according to the taxon
                             taxon == "Crop"~ "#BE9C00",
                             taxon == "Flower-visiting"~ "#8CAB00",
                             taxon == "Aphid"~ "#F8766D",
                             taxon == "Primary aphid parasitoid"~ "#00BBDA",
                             taxon == "Secondary aphid parasitoid"~ "#8B93FF",
                             taxon == "Leaf-miner parasitoid"~ "#00BE70",
                             taxon == "Seed-feeding insect"~ "#F962DD",
                             taxon == "Seed-feeding bird"~ "#D575FE",
                             taxon == "Seed-feeding rodent"~ "#FF65AC",
                             taxon == "Butterfly"~ "#E18A00",
                             taxon == "Insect seed-feeder parasitoid"~ "#24B700",
                             taxon == "Rodent ectoparasite"~ "#00ACFC") ) %>% 
                              arrange(taxon) #order according to taxon

color_sp <- as.vector(species_list$color_sp)


# Arrange pagerankvalues order and prepare the final version of species list

page_rank_circ2<-page_rank_circ %>%  #arrange order according to rows in species_list
  arrange(match(NodesID, species_list$node_id)) 

sp_names<-page_rank_circ2$NodesID#vector with nodes ID
sp_names<-as.factor(species_list$node_name) # to plot species name

page_rank_values<- page_rank_circ2 %>% select(-NodesID,-services)
rownames(page_rank_values) <- sp_names




#PLOT

circos.clear()

png(file="Page_rank_ave.png",
    width=1600, height=1200)

# Extensive layer
mean_E<- page_rank_values[,1, drop= FALSE]

circos.par(gap.after = c("Creeping Buttercup"= 30))
circos.heatmap(mean_E, col = color, rownames.side = "outside", rownames.col	=color_sp,
rownames.cex = 1.4, track.height = 0.1)



# SemI-Extensive layer
mean_SE<- page_rank_values[,2,drop= FALSE]
color = colorRamp2(seq(min(0.000088), max(0.06), length = 100),viridis(100))

circos.heatmap(mean_SE, col = color, track.height = 0.1)

# Moderate layer
mean_M<- page_rank_values[,3,drop= FALSE]
circos.heatmap(mean_M, col = color, track.height = 0.1)


# Semi-intensive layer
mean_SI<- page_rank_values[,4,drop= FALSE]
circos.heatmap(mean_SI, col = color, track.height = 0.1)


# Intensive layer
mean_I<- page_rank_values[,5,drop= FALSE]
circos.heatmap(mean_I, col = color, track.height = 0.1)

#Legend
lgd_mult = Legend(col_fun =color,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 16),  title_position = "topleft", title = "Page Rank", direction = "horizontal",
                  grid_height = unit(1.6,"cm"),  grid_width = unit(5,"cm"),title_gp = gpar(fontsize = 20, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(90, "mm"), 
     just = c("left", "bottom"))

legend("bottomleft", inset=.02, title="Guild", c ("Aphid","Crop","Plant","Seed-feeding rodent"), fill= unique(color_sp), horiz=FALSE, cex=2)

dev.off()


##ADD THE TREATMENTS MANUALLY



