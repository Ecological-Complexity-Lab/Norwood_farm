############################### We calculated the importance of species on indirecting affecting E(D)S


#To do this, we use shortestpath which allow as to see how far each node is from other nodes. Then we assign the direct provision of E(D)S
#to nodes, so we can see how far each species is from those species directly providing ES.


#load packages
library(igraph)
library(dplyr)
library(tidyverse)
library(emln)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio") #set directory


### INTENSIVE CROP PRODUCTION

#### 1. Arrange dataframe

# List of noes with attributes
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
nodes<- Norwood_farm$nodes %>% #list of nodes with attributes
        gather(services,value,4:10) %>% select(-ES,-DES)  %>% 
        filter(value>0)

# Edge list of each management scenario
edge_list<- read.csv("Data/Land_use_rat_edgelist_weighted_CP_intense.csv", sep = ",") %>% 
  select(-weight)


####### 2. Estimate the shortest path

## Create an igraph object for each management using the edge list

network.ES<-list()

for (i in unique(edge_list$management)){# for each treatment
  
  #Filter data
  edge_list_management<-edge_list %>% filter(management== i) #filter edge list according to the management
  
  #Create igraph object
  net.ES <- graph.data.frame(edge_list_management, 
                             directed = F,
                             vertices = NULL)
  
  # Storage the results
  list_name <- paste0(i, i) 
  network.ES[[i]] <- net.ES
}


## Estimate shortest path between  species in each network 


short_man<-NULL
management<-c()

for (m in names(network.ES)) { #for each management
  
  # Filter data
  igraph_management<-network.ES[[m]]
  
  # Calculate distance between nodes (shortest path)
  dis<-distances(igraph_management)
  
  
  # Convert adjacency matrix to edge list 
  short_1<-matrix_to_list_unipartite(dis, directed =FALSE)
  short <- as.data.frame(short_1$edge_list)%>%
            rename("node_from"="from","node_to" = "to", "short_path"= "weight") %>% 
            filter (short_path!=Inf) #eliminate isolated nodes

  
  #Store 
  short_man<- rbind(short_man, short) #
  management <- c(management, rep (m, nrow(short))) # services
  
} 


species_shortpath_raw<- cbind(short_man,management)
species_shortpath_raw$node_to<-as.integer(species_shortpath_raw$node_to)
species_shortpath_raw$node_from<-as.integer(species_shortpath_raw$node_from)


## Create the inverted link version 
species_shortpath_raw_inverted<- tibble(values = species_shortpath_raw$node_to,species_shortpath_raw$node_from, 
                                        species_shortpath_raw$short_path,species_shortpath_raw$management)
colnames(species_shortpath_raw_inverted) <- c("node_from", "node_to","short_path", "management")

## Combine both dataframe
species_shortpath_raw_fin<- bind_rows(species_shortpath_raw, species_shortpath_raw_inverted)


## Add services_to to the data frame 
short_serv<- species_shortpath_raw_fin %>% left_join(nodes, by = c("node_to" = "node_id"), relationship = "many-to-many") %>% 
              select(-node_name,-taxon,-value) %>%  
              filter(!(is.na(services)))  #remove when node_from don't provide any dirct ES

 
 

## Calculate the average of shortest path according to each ecosystem services
species_shortpath<- short_serv %>% group_by(management,node_from,services) %>% 
  summarise(short_ave = mean(short_path)) %>% rename ("node_id" = "node_from")


## Add more attributes of node_from to the dataset (node_name,taxon)
species_shortpath_fin<- species_shortpath %>%  
                    left_join(Norwood_farm$nodes, by = "node_id", relationship = "many-to-many") %>%  #add name of species and taxon of species
                    select(management,node_id,node_name,taxon,services,short_ave)



#write.csv(species_shortpath_fin, "Data/Land_use_shortpath_weighted_CP_intense.csv")


########## PLOTS

## Arrange the matrix to fix the names of species
short_path<- read.csv("Data/Land_use_shortpath_weighted_CP_intense.csv", sep =",", row.names = 1) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) %>% 
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



########## Plot 1: Extensive plot

# Using ggplot

#Create a multi donut plot for the extensive scenario. Each ring represents a particular ES.
short_path_E <- short_path %>% filter(management == "E") 

#### Considering the 25 most important species

# Select the most important species to indirectly affect ES 
species_imp<- short_path_E %>% 
  group_by(node_id) %>% 
  summarise(short_aver_ES = mean(short_ave)) %>% 
  arrange(short_aver_ES)

top_25<-species_imp[1:25,1] #identify the list of species that make up the top 25%

top_25_sp<-short_path_E %>% filter(node_id%in%top_25$node_id) %>%  #Data frame containing the 25% most important species
  mutate(services_code = case_when(
    services == "Crop production"~ 1,
    services == "Pollination"~ 2,
    services == "Butterfly watching"~ 3,
    services == "Seed dispersal"~ 4,
    services == "Bird watching"~ 5,
    services == "Pest control"~ 6,
    services == "Crop damage"~ 7
    
  ))


# Step 1: Identify the External Ring Data Points
external_ring <- top_25_sp %>%
  group_by(node_name) %>%
  filter(services_code == 7) %>%
  ungroup()

# Step 2: Create the plot and add labels
ggplot(top_25_sp, aes(x = node_name, y = services_code, fill = short_ave)) +
  geom_tile(width = 1, height = 0.85, color = "black") + 
  scale_fill_gradient(low = "#ef3b2c", high = "#fff5f0") +
  coord_polar() +
  geom_label(data = external_ring, aes(label = node_name),show.legend = FALSE
  )+
  ylim(c(2, NA)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())




### SAME PLOT WITH CIRCLIZE
library(circlize)
library(viridis)
library(ComplexHeatmap)
top_25_sp


short_top_25<-top_25_sp %>% #just average for now (maybe add error bars in the future)
  select(node_name,services,short_ave) %>% 
  spread(services,short_ave)  #rearrange dataframe


sp_names <- short_top_25$node_name #create temporal species name to filter the big database


##### set up parameters and structure

# Define color of each layer and sps

#layer
color = colorRamp2(seq(max(short_top_25[,2:8]), min(short_top_25[,2:8]), length = 50),viridis(50)) #color layer


#sps
species_list<- read.csv("Data/Land_use_shortpath_weighted_CP_intense.csv", sep =",", row.names = 1) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) %>% 
  filter(node_name%in%sp_names) %>% #filter species in the dataframe
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
  ungroup() %>% select(node_name,color_sp) %>% unique()



  

# Arrange short path order and prepare the final version of species list

sp_names<-short_top_25$node_name#vector with nodes ID
sp_names<-as.factor(sp_names) # to plot species name

top_25_values<- short_top_25 %>% select(-node_name)
rownames(top_25_values) <- sp_names

species_list_ordered <- species_list[match(sp_names, species_list$node_name), ]

color_sp<-species_list_ordered$color_sp

#Plotting

circos.clear()

png(file="Short_top25_Ext.png",
   width=1600, height=1200)

# Crop production
CP<- top_25_values[,4, drop= FALSE]

# Define the border color for the heatmap cells
border_color = "black"  # You can choose any color

circos.heatmap(CP, col = color, rownames.side = "outside", rownames.col	=color_sp,
               rownames.cex = 1.06, track.height = 0.1, cell.border = "black")

# Pollination
PO<- top_25_values[,6,drop= FALSE]

circos.heatmap(PO, col = color, track.height = 0.05,  cell.border = "black")

# Seed dispersal
SD<- top_25_values[,7,drop= FALSE]

circos.heatmap(SD, col = color, track.height = 0.05,  cell.border = "black")

# Pest control
PC<- top_25_values[,5,drop= FALSE]

circos.heatmap(PC, col = color, track.height = 0.05,  cell.border = "black")

# Bird watching
BW<- top_25_values[,1,drop= FALSE]

circos.heatmap(BW, col = color, track.height = 0.05,  cell.border = "black")

# Butterfly watching
BTW<- top_25_values[,2,drop= FALSE]

circos.heatmap(BTW, col = color, track.height = 0.05,  cell.border = "black")

# Crop damage
CD<- top_25_values[,3,drop= FALSE]

circos.heatmap(CD, col = color, track.height = 0.05,  cell.border = "black")

#Legend
lgd_mult = Legend(col_fun =color,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 20),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(1.6,"cm"),  grid_width = unit(11,"cm"),title_gp = gpar(fontsize = 20, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(90, "mm"), 
     just = c("left", "bottom"))

#legend("bottomleft", inset=.02, title="Guild", c ("Aphid","Crop","Plant","Seed-feeding rodent"), fill= unique(color_sp), horiz=FALSE, cex=2)

dev.off()







##### DESPUES DE ARREGLAR ESO MOVERSE AL OTRO PLOT!

#### All species

##COPIAR ACA QUEONDA

short_path_E <- short_path %>% filter(management == "E")

#PROBAR ABRIR EL ANILLO PARA PONER EL NOMBRE DE LOS SERVICIOS


ggplot(short_path_E, aes(x = node_name, y = services, fill = short_ave)) +
  geom_tile(width = 3, height = 3,  color = "white") + # Adjust tile size for a clearer donut shape
  scale_fill_gradient(low = "green", high = "red") + # Color gradient
  coord_polar() + # Transform to polar coordinates
  #geom_text(aes(label = services), position = position_stack(vjust = 0.5)) + # Add ring names
  # ylim(c(1.5, 3.5)) + # Adjust y-limits to create the donut's hole
 # scale_y_continuous(limits = c(0, max(short_path_E$services) * 0.9), expand = c(0, 0)) + # Adjust y-axis limits to create a hole in 
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())







## Plot 2: Each ring represents a particular habitat management and values indicate the average importance of a species to indirectly
#affect ES


short_path_aver<-short_path %>% group_by(management,node_id) %>% 
                summarise(short_aver_ES = mean(short_ave))

short_path_aver$management <- factor(short_path_aver$management, levels = c("I", "SI", "M", "SE","E")) #change order of factors


#PLOT ANILLO UNO CONN TODOS LOS HABITAT MANAGEMENT Y AVERAGE Y OTRO DESCRIPTIVO DE EMPIRICAL CON TODOS LOS ES


ggplot(short_path_aver, aes(x = node_id, y = management, fill = short_aver_ES)) +
  geom_tile(width = 3, height = 3,  color = "black") + # Adjust tile size for a clearer donut shape
  scale_fill_gradient(low = "green", high = "red", na.value = 'gray') + # Color gradient
  coord_polar() + # Transform to polar coordinates
  #geom_text(aes(label = services), position = position_stack(vjust = 0.5)) + # Add ring names
  # ylim(c(1.5, 3.5)) + # Adjust y-limits to create the donut's hole
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())





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
                  select(NodesID,services,E,SE,M,SI,I)#  %>% 
                # arrange(desc(E)) %>% slice(1:50) %>% #select the fifty more important species
                #  sample_n(nrow(.))

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



