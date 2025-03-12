# In this code, we created the ecological network graph used in the figure 2. 
#

## -- Load libraries --------------------------------------------------------------------------------------------------------
library(igraph)
library(tidyverse)

## -- get_data--------------------------------------------------------------------------------------------------------
setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")



######### --- Upload data and arrange data
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object

nodes<- Norwood_farm$nodes %>% 
  mutate(taxon = str_replace(taxon, "Flower-visiting", "Flower visitor")) #node list with attributes

edge_list<-read.csv("Data/Land_use_edgelist.csv", sep =",") %>%  #edgelist
  mutate(weight = 1) #edge list


##############  --  Extensive crop production management (E)

E_edge <- edge_list %>% filter(management == "E") %>% select(node_from,node_to)

# create igraph object
E<- graph_from_edgelist(as.matrix(E_edge), directed = FALSE) 

# define color according to taxon
color_trophic <-tibble(taxon = c("Plant","Crop","Flower visitor","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                 "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                 "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                       color = c("#33a02c","#b15928","#a6cee3","#1f78b4","#b2df8a","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6",
                                 "#6a3d9a", "#ffff99", "#e7298a"))

# assign attributes according to the taxon
V(E)$taxon<-nodes$taxon #create taxon attribute

#create color of taxon as attributes
vcol=V(E)$taxon
vcol[vcol== "Plant"]="#33a02c"
vcol[vcol=="Crop"]="#b15928"
vcol[vcol=="Flower visitor"]="#a6cee3"
vcol[vcol== "Aphid"]="#1f78b4"
vcol[vcol=="Primary aphid parasitoid"]="#b2df8a"
vcol[vcol=="Secondary aphid parasitoid"]="#fb9a99"
vcol[vcol=="Leaf-miner parasitoid"]="#e31a1c"
vcol[vcol== "Seed-feeding insect"]="#fdbf6f"
vcol[vcol=="Seed-feeding bird"]="#ff7f00"
vcol[vcol=="Seed-feeding rodent"]="#cab2d6"
vcol[vcol== "Butterfly"]="#6a3d9a"
vcol[vcol=="Insect seed-feeder parasitoid"]="#ffff99"
vcol[vcol=="Rodent ectoparasite"]="#e7298a"

V(E)$color=vcol

  
# Plot
pdf("Graphs/Extensive_network.pdf", width = 5, height = 7)
E_network<-plot(E,edge.arrow.size=.5, vertex.color= V(E)$color, vertex.size=4, vertex.label=NA,
     vertex.frame.color="black", vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=0.5, edge.curved=0.2)

dev.off()

pdf("Graphs/Legend_network.pdf")
legend_network<-plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("bottom", legend = color_trophic$taxon, pch = 21,
       col = "black",
       pt.bg = color_trophic$color,
       pt.cex = 2, cex = 1, bty = "n", ncol = 2, title = "Trophic guild")

dev.off()


##############  --  Moderate crop production management (M)
MCP_edge <- edge_list %>% filter(management == "M") %>% select(node_from,node_to)

#Create igraph object
M_CP<- graph_from_edgelist(as.matrix(MCP_edge), directed = FALSE)

#Identify isolate nodes and remove from the igraph object
isolated_nodes <- which(degree(M_CP) == 0)
M_CP2<- delete_vertices(M_CP, isolated_nodes)

# assign attributes according to the taxon
nodes_in_network<- nodes %>% filter(node_id%in%MCP_edge$node_from | #create the nodelist according to the nodes present in the network
                                      node_id%in%MCP_edge$node_to )

V(M_CP2)$taxon<-nodes_in_network$taxon #create taxon attribute

#create color of taxon as attributes
vcol=V(M_CP2)$taxon
vcol[vcol== "Plant"]="#33a02c"
vcol[vcol=="Crop"]="#b15928"
vcol[vcol=="Flower visitor"]="#a6cee3"
vcol[vcol== "Aphid"]="#1f78b4"
vcol[vcol=="Primary aphid parasitoid"]="#b2df8a"
vcol[vcol=="Secondary aphid parasitoid"]="#fb9a99"
vcol[vcol=="Leaf-miner parasitoid"]="#e31a1c"
vcol[vcol== "Seed-feeding insect"]="#fdbf6f"
vcol[vcol=="Seed-feeding bird"]="#ff7f00"
vcol[vcol=="Seed-feeding rodent"]="#cab2d6"
vcol[vcol== "Butterfly"]="#6a3d9a"
vcol[vcol=="Insect seed-feeder parasitoid"]="#ffff99"
vcol[vcol=="Rodent ectoparasite"]="#e7298a"
V(M_CP2)$color=vcol


#Plot 
pdf("Graphs/moderate_network.pdf", width = 5, height = 7)
M_CPplot<-plot(M_CP2,edge.arrow.size=.5, vertex.color= V(M_CP2)$color, vertex.size=6, vertex.label=NA,
               vertex.frame.color="black", vertex.label.color="black", 
               #vertex.label.color="black",  #vertex.label.cex=.5, vertex.label.dist=0.5, 
               edge.curved=0.2)
dev.off()



##############  --  Intensive non-organic management (IN)

IMCP_edge <- edge_list %>% filter(management == "IN") %>% select(node_from,node_to)

#Create igraph object
IM_CP<- graph_from_edgelist(as.matrix(IMCP_edge), directed = FALSE)

#Identify isolate nodes and remove from the igraph object
isolated_nodes <- which(degree(IM_CP) == 0)
IM_CP2<- delete_vertices(IM_CP, isolated_nodes)

## assign attributes according to the taxon
nodes_in_network<- nodes %>% filter(node_id%in%IMCP_edge$node_from | #create the nodelist according to the nodes present in the network
                                      node_id%in%IMCP_edge$node_to )

V(IM_CP2)$taxon<-nodes_in_network$taxon #create taxon attribute

#create color of taxon as attributes
vcol=V(IM_CP2)$taxon
vcol[vcol== "Plant"]="#33a02c"
vcol[vcol=="Crop"]="#b15928"
vcol[vcol=="Flower visitor"]="#a6cee3"
vcol[vcol== "Aphid"]="#1f78b4"
vcol[vcol=="Primary aphid parasitoid"]="#b2df8a"
vcol[vcol=="Secondary aphid parasitoid"]="#fb9a99"
vcol[vcol=="Leaf-miner parasitoid"]="#e31a1c"
vcol[vcol== "Seed-feeding insect"]="#fdbf6f"
vcol[vcol=="Seed-feeding bird"]="#ff7f00"
vcol[vcol=="Seed-feeding rodent"]="#cab2d6"
vcol[vcol== "Butterfly"]="#6a3d9a"
vcol[vcol=="Insect seed-feeder parasitoid"]="#ffff99"
vcol[vcol=="Rodent ectoparasite"]="#e7298a"
V(IM_CP2)$color=vcol


#Plot 
pdf("Graphs/Intensive_nonorganic_network.pdf", width = 5, height = 7)
IM_CPplot<-plot(IM_CP2,edge.arrow.size=.5, vertex.color= V(IM_CP2)$color, vertex.size=6, vertex.label=NA,
               vertex.frame.color="black", vertex.label.color="black", 
               #vertex.label.color="black",  #vertex.label.cex=.5, vertex.label.dist=0.5, 
               edge.curved=0.2)
dev.off()



