######### In this code we create a multilayer object representing the network farm
# We don't analyse the multilayer network but it will organize the information, such as the node list per habitat,
#edge list and attributes of nodes in each habitat.

#Layers: Habitats
#Nodes: Species with attributes (trophic groups and ecosystem services)
#intraedges: presence/absence of interactions


library(emln)#multilayer package
library(readr)
library(ggplot2)
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

###############  CREATE MULTILAYER NETWORK #######################

edges <- tibble(read.csv("Data/elist_nore.csv")) 

###  1) Create list of link lists to use as input in create_multilayer_network function 
edges_format<-edges %>% rename("from"="lower") %>% rename("to"="upper") %>% 
  mutate(weight =1)# weight is presence/absence

list_of_layers<- split(edges_format, edges_format$habitat)
list_of_layers <- lapply(list_of_layers, function(sub_df) subset(sub_df, select = -habitat))#remove habitat from the edge list to keep the format (from, to, weight)


### 2) Create input with layer's attributes (habitat names)
layer_attrib <- tibble(layer_id=1:11,
                       layer_name=c('CP','GM','LP', 'LU','MH','NH', 'NL','PP','RG',
                                    'SF','WD'))


### 3) Create input with attributes of physical nodes (taxon and ecosystem services provided)
nodes <- tibble(read.csv("Data/nodes.csv")) #previous dataframe containing node's information

## add potential direct ES to species according to the guild group
services_pre <- tibble(read.csv("Data/service_edgelist.csv", sep = ";")) %>%  #data frame without antagonist
rename (node_name = lower, ESS = upper) %>%
  pivot_wider(names_from = ESS, values_from = weight, values_fill = 0) %>%
  left_join(nodes, by = "node_name") %>% select(-node_id)  #list of ecosystem (dis)services that each species provides
  
#remove duplicate rows containing butterflies in 02fv (because they are a separated guild)
list.butt.flw<- c("02FV.Maniola jurtina","02FV.Pieris brassicae","02FV.Polyommatus icarus",
                    "02FV.Pyronia tithonus") #list of butterflies

services<-services_pre %>% filter(!(node_name %in%list.butt.flw)) #remove dupicated butt from dataset of services
  

## we include species that not directly provide E(D)S
phy_node_atr_tax_es<- dplyr::left_join(nodes, services, by = "node_name") %>% select(-node_id, -taxon.y) %>% 
  rename("taxon" = "taxon.x") %>% 
  replace(is.na(.), 0) %>% rowwise() %>%  #we assigned "0" to species that we don't know if provide ES
  mutate(ES = rowSums(across(c("Crop production","Pollination","Pest control",
                               "Seed dispersal", "Butterfly watching", "Bird watching"))),
         DES = rowSums(across(c("Crop damage"))))
                              


### Create multilayer object
Norwood_farm<-create_multilayer_network(list_of_layers = list_of_layers, bipartite = F, directed = T,
                                  interlayer_links = NULL, layer_attributes = layer_attrib, state_node_attributes = NULL, physical_node_attributes = phy_node_atr_tax_es)
view(Norwood_farm$layers)
view(Norwood_farm$nodes)
view(Norwood_farm$extended_ids)
view(Norwood_farm$state_nodes)

#saveRDS(Norwood_farm, file="Norwood_farm.RData")

### Species list (without crops)

species_list<-Norwood_farm$nodes %>% select(node_id,node_name,taxon) %>% 
  filter(taxon != "Crop") %>% 
          separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
          select(-trophic_lower)
  
#write.csv(species_list,"Data/species_list.csv", row.names= FALSE)



################ --- Exploratory analysis
state_nodes_attributes<-right_join(Norwood_farm$state_nodes,Norwood_farm$nodes, by = "node_name") %>% 
  select(-node_id.x,-node_id.y)#create state node list with attributes to use for explorating data


# - Nodes per habitat (layer)

  Nodes_habitat<-Norwood_farm$state_nodes %>% 
  ggplot(aes(x=layer_name))+ geom_histogram(stat= "count", alpha= 0.70, color= "black")+ 
  labs(x='Habitat', y="Number of species") +theme_bw()+
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
  Nodes_habitat
  
  
# - Nodes distribution per trophic group and habitat
  
nodes_hab_tax<- state_nodes_attributes %>%  select(layer_id,layer_name,node_name,taxon) %>% 
  group_by(layer_name) %>% count(taxon)
    
 nodes_habitat_taxon<- nodes_hab_tax %>%
   ggplot(aes(fill=taxon, y=n, x=layer_name)) + 
   geom_bar(position="stack", stat="identity")+
   labs(x='Habitat', y="Number of species") +theme_bw()+
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
 
 nodes_habitat_taxon
 
 
# - ES or DES provided by taxon group
 
 ES_taxon<- state_nodes_attributes %>%  group_by(taxon) %>% distinct(node_name, .keep_all = TRUE) %>% select(node_name,taxon,ES,DES) %>% 
   gather(key = "provision", value= "value", 3:4) %>% group_by(taxon,provision) %>% 
   summarise(values = sum(value))
   
 Nodes_ES<-ES_taxon %>% 
   ggplot(aes(y=values, x=taxon, fill = provision)) + 
   geom_bar(position="stack", stat="identity")+
   labs(x='Taxon', y="Number of services provided") +theme_bw()+
   theme_classic()+
   theme(panel.grid = element_blank(),
         panel.border = element_rect(color = "black",fill = NA,size = 1),
         panel.spacing = unit(0.5, "cm", data = NULL),
         axis.text = element_text(size=15, color='black'),
         axis.text.x= element_text(size =13, angle = 90), 
         axis.title = element_text(size=17, color='black'),
         axis.line = element_blank(),
         legend.text.align = 0,
         legend.title =  element_text(size = 13, color = "black"),
         legend.text = element_text(size = 11))
 
 Nodes_ES


# - ES or DES provided by each habitat (specifying which ES or DES)
 
ES_habitat <-   state_nodes_attributes %>%  group_by(layer_name, node_name) %>% select(-taxon) %>% 
  gather(key = "provision", value= "value", 4:10)%>% group_by(layer_name,provision) %>% 
  summarise(values = sum(value))
  
Habitat_ES<-ES_habitat %>% 
  ggplot(aes(y=values, x=layer_name, fill = provision)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x='Habitat', y=" (Dis)services provided") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =13, angle = 90), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

  Habitat_ES

# - Distribution of links
  ecological_links <- read.csv("Data/species_edgelist.csv")
  species_links <- ecological_links %>% 
    group_by(lower, upper) %>% 
    summarise(weight = sum(weight))
  
  # Group links according to the trophic groups (not specified + or - int for birds )
  trophic<- species_links %>% 
    separate(lower, c("trophic_lower", "lower"),  "[A-Z]\\.[A-Z]") %>%
    separate(upper, c("trophic_upper", "upper"),  "[A-Z]\\.[A-Z]")#separate information
  
  trophic2<-trophic %>%
    mutate(trophic_upper = recode(trophic_upper, '01PLAN' = 'Plant',
                                  "02F" = "Flower visitor",
                                  "03AP" = "Aphidos", "04PRIMARYPAR" = "Par. 1",
                                  "05SECONDARYPAR" = "Par. 2","06MINERPAR" = "Leaf min.","07SEEDINVER" = "Seed pred (Inv)", "08BIR" = "Bird", "09MAMMA" = "Seed pred (Mam)", "12BFL" = "Butterfly", "13SFPAR" = "Par. (Syrph)", "14FLE" = "Flea"),
           
           trophic_lower = recode(trophic_lower, '01PLAN' = 'Plant', 
                                  "01PLANT.zCROP Barley" = "Plant", "01PLANT.zCROP Lucerne" = "Plant", "01PLANT.zCROP Oat spring" = "Plant", "01PLANT.zCROP Oat winter" = "Plant", "01PLANT.zCROP Triticale" = "Plant", "01PLANT.zCROP Wheat" = "Plant",
                                  "02F" = "Flower visitor",
                                  "03AP" = "Aphidos", "04PRIMARYPAR" = "Par. 1",
                                  "05SECONDARYPAR" = "Par. 2","06MINERPAR" = "Leaf min.","07SEEDINVER" = "Seed pred (Inv)", "08BIR" = "Bird", "09MAMMA" = "Seed pred (Mam)", "12BFL" = "Butterfly", "13SFPAR" = "Par. (Syrph)", "14FLE" = "Flea"))
  
  
  #Merge trophic groups to create interaction dataframe
  trophic2$Int <- paste(trophic2$trophic_lower, trophic2$trophic_upper, sep= "-")
  
  Int_trophic<-trophic2 %>% select(Int,weight)
  Int_trophic$Int<-as.character(Int_trophic$Int)
  
  #Plot trophic interactions distribution
  
  Int_trophic %>%
    ggplot(aes(x=weight, fill=Int))+ geom_histogram(position= "identity", alpha= 0.70, color= "black")+ 
    labs(x='Weight', y="Count") +theme_bw()+
    theme_classic()+ 
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "black",fill = NA,size = 1),
          panel.spacing = unit(0.5, "cm", data = NULL),
          axis.text = element_text(size=10, color='black'),
          axis.title = element_text(size=17, color='black'),
          axis.line = element_blank(),
          legend.text.align = 0,
          legend.title =  element_text(size = 13, color = "black"),
          legend.text = element_text(size = 11))
  

  

  
  ################ --- Playing with the data and package
  
  
  
### --- Calculate eigenvector centrality to check the importance of the nodes 
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
Norwood_farm$extended_ids$weight<-1 #to do it now unweighted (created in multilayer

## - in each habitat

# get a list of igraph objects as layers igraph 
g_layer <- get_igraph(Norwood_farm, bipartite = F, directed = T)

# get Eigenvector scores of igraph layers
eigen_igraph = c()
for (layer in g_layer$layers_igraph){
  eigen_igraph <- append(eigen_igraph, igraph::eigen_centrality(layer, scale = T)$vector)
}


## - considering all habitats

#IT'S
# get the SAM
sam_multilayer <- get_sam(multilayer = Norwood_farm, bipartite = F, directed = T, sparse = F, remove_zero_rows_cols = F)
  
# converting SAM to igraph object
graph_sam <- graph_from_adjacency_matrix(sam_multilayer$M,mode = "directed",weighted = T,diag = T,add.colnames = NULL,add.rownames = NA)
  
# get Eigenvector scores
eigen_sam <- igraph::eigen_centrality(graph_sam, directed = T, scale = T, weights = T)
  
ggplot_orueba<-eigen_sam %>% 
  ggplot(aes(y=Prop, x=layer, fill = taxon)) + 
  geom_bar(position="stack", stat="identity")+ ggtitle("Negative - Indirect")+
  labs(x='Habitat', y="Prop ES provided per taxon") +theme_bw()+
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

## - comparing between methods

# merge to data frame eigenvector comparison
ec_comparison <- data.frame(eigen_sam, eigen_igraph)
colnames(ec_comparison) <- c("multilayer","monolayer")

# scatter plot using ggplot2
ggplot(data = ec_comparison, aes(monolayer, multilayer)) + 
  geom_point(color = "blue", size = 0.75) + 
  labs(title = "Eigenvector Centrality (EC) comparison", x = "EC (monolayer)", y = "EC (multilayer)")+
  geom_abline()+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))+
  coord_fixed() +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(color='black',size = 10),
        legend.text =  element_text(size=15),
        legend.title = element_text(size=20))

#Just few species have the same centrality when considering mono or multilayer



### --- Estimate multilayer modularity
library(infomapecology)

# create multilayer object to run infomap

Norwood_for_mod<-create_multilayer_object(extended = Norwood_farm$extended_ids,
                                          nodes = Norwood_farm$nodes, layers = Norwood_farm$layers,
                                          intra_output_extended = F) #intra_output_extended = F to run with relax rate

Norwood_modularity <- run_infomap_multilayer(Norwood_for_mod,
                                             relax = T, silent = TRUE,
                                             trials = 100, seed = 497294,
                                             multilayer_relax_rate = 0.15,
                                             multilayer_relax_limit_up = 1,
                                             multilayer_relax_limit_down = 0,
                                             temporal_network = F)

mod<-Norwood_modularity$modules

#write.csv(mod, "./Data/Norwood_modules.csv",  row.names = FALSE)

## -- Exploratory plots modules


#arrange data to include modules sizes
size <- count(mod, module)  #create a data frame of all modules and how many nodes are in each (size of module)
module_data <- merge(mod , size, by=c("module","module")) #merge size of module with all the other info about the modules
colnames(module_data)[17] <- "size_of_module" #rename column


# - Modules' size

#how many layers are within a module
module_data <- module_data %>% select(layer_id, module, size_of_module) %>% unique() #take only certain columns
module_data$count <- c(1)

module_data %>% 
  ggplot(aes(x=module, y= size_of_module))+ geom_bar(stat= "identity")+ theme_classic()+ labs(y="Size", x="Modules")+
 theme(axis.text.x = element_blank(),
                                         axis.text.y=element_text(size=15), axis.title = element_text(size=17),
                                          legend.text=element_text(size=12.5),legend.title =element_text(size=14))

# - Modules'composion (trophic group)

mod_trophic <- mod %>%  left_join(size,mod,by = "module") %>% group_by(module, taxon) %>% 
  summarise(taxon_count = n(), taxon_prop = taxon_count/n) %>% unique()

mod_trophic %>% 
  ggplot(aes(x=module, y= taxon_prop, fill= factor(taxon)))+ geom_bar(stat= "identity", width = 0.9)+ theme_classic()+ labs(y="Prop of trophic/module", x="Modules")+
  guides(fill=guide_legend(title="Taxon"))+ theme(axis.text.x = element_blank(),
                                                    axis.text.y=element_text(size=15), axis.title = element_text(size=17),
                                                    legend.text=element_text(size=12.5),legend.title =element_text(size=14))



# - Modules'composion (ES) how many ES provide each module

ES_taxon<- state_nodes_attributes %>%  group_by(taxon) %>% distinct(node_name, .keep_all = TRUE) %>% select(node_name,taxon,ES,DES) %>% 
  gather(key = "provision", value= "value", 3:4) %>% group_by(taxon,provision) %>% 
  summarise(values = sum(value))

mod_es <- mod %>%  gather(key = "provision", value= "value", 8:13) %>% group_by(module, provision) %>% 
  summarise(es_count = sum(value)) 
  
  

mod_es %>% 
  ggplot(aes(x=module, y= es_count, fill= factor(provision)))+ geom_bar(stat= "identity", width = 0.9)+ theme_classic()+ labs(y="Number of (Dis)services", x="Modules")+
  guides(fill=guide_legend(title="Type of ES"))+ theme(axis.text.x = element_blank(),
                                                  axis.text.y=element_text(size=15), axis.title = element_text(size=17),
                                                  legend.text=element_text(size=12.5),legend.title =element_text(size=14))



# -  Distribution of modules across habitats

mod_hab <- module_data %>% 
  mutate(layer_name= case_when(layer_id == '1' ~ 'CP',
                               layer_id == '2' ~ 'GM',
                               layer_id == '3' ~ 'LP',
                               layer_id == '4' ~ 'LU',
                               layer_id == '5' ~ 'MH',
                               layer_id == '6' ~ 'NH',
                               layer_id == '7' ~ 'NL',
                               layer_id == '8' ~ 'PP',
                               layer_id == '9' ~ 'RG',
                               layer_id == '10' ~ 'SF',
                               layer_id == '11' ~ 'WD',
                               ))

mod_hab <- mod_hab %>% unique()  %>% group_by(module) %>%  
  mutate(tot_hab = sum (count)) %>% arrange(desc(tot_hab))

mod_hab$module<- as.factor(mod_hab$module)

mod_hab %>% 
  ggplot(aes(x=module, y= count ,fill= factor(layer_name)))+ geom_bar(stat= "identity")+ theme_classic()+ labs(y="Number of habitats", x="Modules")+
  guides(fill=guide_legend(title="Habitat"))+ theme(axis.text.x = element_blank(),
                                                     axis.text.y=element_text(size=15), axis.title = element_text(size=17),
                                                     legend.text=element_text(size=12.5),legend.title =element_text(size=14))



## See which node produces more DES and which one DES (in which habitat)



### Graphical (VER COMO MODIIFCAR..)
library(igraph)

gg <- graph.data.frame(Norwood_farm$extended_ids, directed=T)
plot(prueba, vertex_size=10,
     vertex_color= Norwood_farm$nodes$taxon,
     vertex_label= FALSE)




