################### CHANGE IN LAND USE SIMULATION (aggregated, no abundances) ##################################

# In this code we create the different proposed management scenarios by aggregating the layers
#to create the final network. 

# To create the different management scenarios we will replace the specific habitats to "CP" (including all species, assuming that community reached the equilibrium).
# When we have the same link in different habitats, we keep it just once (cause we cant' add it)

# In this particular scenario, this simulares habitat loss according to the management scenario


library(emln)#multilayer package
library(readr)
library(ggplot2)
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

######### --- Upload multilayer network
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
Norwood_farm$extended_ids$weight<-1 # non-weighted for now 


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

land_change_removal<-land_change_removal[,c(4,1,2,3)]







################## --- CALCULATE DIRECT E(D)S PROVISION AND INDIRECT EFFECT ON ES


##### --  Direct E(D)S provision

##  Create node_list per management 
node_list<-land_change_removal %>% pivot_longer(cols = 2:3, names_to = "species", values_to = "node_id") %>% 
  select(-species) %>%  unique()


## Add information of ES per node (values 0-1)

nodes_ES<- right_join(node_list, Norwood_farm$nodes, by = "node_id")%>% 
  select(management,node_id,taxon,"Crop production",
         "Pollination", "Crop damage", "Pest control", "Seed dispersal", "Butterfly watching", "Bird watching") %>% 
  group_by(management,node_id) %>% 
  gather("services","value", 4:10) #we conserve species that not directly provide ES because can serve as intermediate hop

nodes_ES$management <- factor(nodes_ES$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors


## -- Estimate direct E(D)S provision
direct_ES<- nodes_ES %>% filter (value ==1) %>% 
  mutate (type = "D") %>% select(-value)

#write.csv(direct_ES,"Data/Land_use_dir_binary_removal.csv", row.names= FALSE)

## -- Direct provision Ratio ES/E(D)S

# ratio > 1 indicates more benefits, ratio = 1 balance between damages and benefits and ratio < 1 more damage

number_ES<-nodes_ES %>% group_by(management) %>% 
  filter (value ==1 & services != "Crop damage") %>% summarise (ES =sum(value))#count ES provision

number_EDS<-nodes_ES %>% group_by(management) %>% 
  filter (value ==1 & services == "Crop damage") %>% summarise (EDS =sum(value))#count EDS provision

total<-cbind(number_ES,number_EDS[,-1])

ratio_direct<-total %>% mutate(ratio_direct = ES/EDS)

#write.csv(ratio_direct,"Data/Land_use_rat_dir_binary_removal.csv", row.names= FALSE)

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

ggplot_EDS_management<-direct_ES%>% group_by(management) %>% count(services) %>% 
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




##### --  INDIRECT EFFECT ON E(D)S 


## -- Prepare dataframe

# Full list nodes with ES in the network (considering those that provide and not provide direct ES)

list_nodes_ES_provi<-nodes_ES%>% ungroup() %>% select(-management) %>%
  filter (value ==1) %>% unique # list of nodes that provide ES ( = no plants and ectoparasites)
list_nodes_ES_no_provi<- nodes_ES %>% filter(node_id <=87 | node_id >=542) %>% 
  mutate(services = "None", value = 1) %>% ungroup() %>% 
  select (-management) %>%  unique() # nodes that not provide any ES (plants and ectoparsites), ES assigned as None

list_nodes_ES<-rbind(list_nodes_ES_provi,list_nodes_ES_no_provi) #Total list of nodes with ES (with None)


# Add attributes of nodes to the edgelist

edge_list<- left_join(land_change_removal,list_nodes_ES, by = c("node_from"="node_id")) %>% 
  rename("taxon_from"="taxon", "services_from"="services",
         "value_from" = "value")%>% 
  left_join(list_nodes_ES, by = c("node_to"="node_id"))  %>% 
  rename("taxon_to"="taxon", "services_to"="services",
         "value_to" = "value")

# Add inverted links (to make the code easier to program when calculate indirect interactions. It will not affect the results)

edge_list_inverted<- tibble(values = edge_list$node_to,edge_list$node_from, edge_list$weight,
                            edge_list$management, edge_list$taxon_to,edge_list$services_to,
                            edge_list$value_to,  edge_list$taxon_from,edge_list$services_from,edge_list$value_from)
colnames(edge_list_inverted) <- c("node_from", "node_to","weight", "management", "taxon_from", "services_from",
                                  "value_from", "taxon_to", "services_to", "value_to")


# Combine both data frame to create the final edge list

edgelist_final<- bind_rows(edge_list, edge_list_inverted) %>% 
  select(-weight,-value_to,-value_from) # we are not using these anymore

edgelist_final<-edgelist_final[,c(1,2,4,5,3,6,7)]


#### - Calculate indirect effects considering 1 hop (node - node)


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


Indirect_1hop_landuse_removal<-data.frame(management,services_from,node_from,node_to,taxon_from,services_to,hop = rep(1, length(services_from)), type = rep("I", length(services_from))) 


# Rearrange the output

# we remove duplicates rows where node_from = birds or butterflies cause they represent the same interaction. 
# This happens because each row represents an attribute and these taxons have 2 and 3 attributes per node.

rows_birds_butt<- Indirect_1hop_landuse_removal %>%
  filter(taxon_from == "Butterfly" | taxon_from == "Seed-feeding bird") %>%
  distinct(management, node_from, node_to, .keep_all = TRUE) # new subset after eliminating duplicate rows for node_from = birds and butterflies

int_without<-Indirect_1hop_landuse_removal %>% filter(!(taxon_from == "Butterfly" | 
                                                       taxon_from == "Seed-feeding bird")) #eliminate the interactions containing node_from =birds or butterflies from the original dataframe


Indirect_1hop_landuse_removal_2<-rbind(rows_birds_butt,int_without)#final dataframe containing indirect effects on ES via 1 hop


#write.csv(Indirect_1hop_landuse_removal_2,"Data/Land_use_ind_1hop_binary_removal.csv", row.names= FALSE)




#### - Calculate indirect effects considering 2 hops (node 1 - node 2 - node 3, effect of node 1 on node 3'E(D)S via node 2)

Indirect_1hop<-read.csv("Data/Land_use_ind_1hop_binary_removal.csv",
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
  
  j = Indirect_1hop$node_to[i] # check the node_to (intermediate species: node 2 in the title)
  l = Indirect_1hop$management [i] #check management where the target species for which we are detecting indirect effects on ES

  
  
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
  
}

Indirect_2hop<- data.frame(management,node_id,taxon_from,services,node_int,
                node_to, services_to,type = rep("I", length(services_to)), hop = rep(2,length(services_to)))


# Remove duplicate interactions when node_to = birds or butterflies

Indirect_2hop_final<- Indirect_2hop %>% unique() 

#write.csv(Indirect_2hop_final,"Data/Land_use_ind_2hop_binary_removal.csv", row.names= FALSE)

Indirect_2hop<-read.csv("Data/Land_use_ind_2hop_binary_removal.csv",
                        sep =",") #load dataframe of indirect effects using 1 hop



# Join both 1 and 2 hops indirect effects dataframes

# 1 hop
Indirect_1hop_m<-Indirect_1hop  %>% rename("services" ="services_from",
                                           "node_id" = "node_from",
                                           "taxon" = "taxon_from") %>% 
  mutate(node_int = NA)

Indirect_1hop_m<-Indirect_1hop_m[,c(1,2,3,5,9,4,6,7,8)]

# 2 hop
Indirect_2hop_m<-Indirect_2hop %>% rename("taxon" = "taxon_from") 
Indirect_2hop_m<-Indirect_2hop_m[,c(1,4,2,3,5,6,7,8,9)]

#  Total Indirect effect of ES

I_ES<- rbind(Indirect_1hop_m,Indirect_2hop_m)

#write.csv(I_ES,"Data/Land_use_ind_binary_removal.csv")



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

hop_1 <- read.csv("Data/Land_use_ind_binary_removal.csv", sep =",", row.names = 1) %>% 
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

hop_2 <- read.csv("Data/Land_use_ind_binary_removal.csv", sep =",",row.names = 1) %>% 
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

#write.csv(output_ES,"Data/Land_use_output_ind_binary_removal.csv", row.names= FALSE)



##### -- Indirect provision Ratio output +/-
output_ind_ES <- read.csv("Data/Land_use_output_ind_binary_removal.csv", sep =",") 

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




  
  



