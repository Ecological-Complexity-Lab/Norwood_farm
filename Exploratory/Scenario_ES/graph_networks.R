############################## Script to create figure of ecological networks ################################

library(igraph)
library(tidyverse)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")


############### Land use change from EXTENSIVE TO CROP PRODUCTION

Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object to get the node's list

nodes<- Norwood_farm$nodes %>% 
  mutate(taxon = str_replace(taxon, "Flower-visiting", "Flower visitor")) #node list with attributes
edge_list<-read.csv("Data/Land_use_rat_edgelist_weighted_CP_intense.csv", sep =",") %>% 
                    mutate(weight == 1) #edge list


###### EXTENSIVE

E_edge <- edge_list %>% filter(management == "E") %>% select(node_from,node_to)

E<- graph_from_edgelist(as.matrix(E_edge), directed = FALSE) #create igraph object

S <- vcount(E)#number of species
L <- ecount(E) #number of links
#C <- ecount(E) / vcount(E)^2 

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

  
 
##Plot

#png("Graphs/E_network.png", width = 800, height = 600)
pdf("Graphs/E_network.pdf", width = 5, height = 7)
E_network<-plot(E,edge.arrow.size=.5, vertex.color= V(E)$color, vertex.size=4, vertex.label=NA,
     vertex.frame.color="black", vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=0.5, edge.curved=0.2)

dev.off()

pdf("Graphs/Legend_network.pdf", width = 4, height = 5)
legend_network<-plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("bottom", legend = color_trophic$taxon, pch = 21,
       col = "black",
       pt.bg = color_trophic$color,
       pt.cex = 2, cex = 1, bty = "n", ncol = 2, title = "Trophic guild")

dev.off()


###### MODERATE
MCP_edge <- edge_list %>% filter(management == "M") %>% select(node_from,node_to)

#Create igraph object
M_CP<- graph_from_edgelist(as.matrix(MCP_edge), directed = FALSE)

#Identify isolate nodes and remove from the igraph object
isolated_nodes <- which(degree(M_CP) == 0)
M_CP2<- delete_vertices(M_CP, isolated_nodes)


S <- vcount(M_CP2)#number of species
L <- ecount(M_CP2) #number of links
#C <- ecount(M_CP2) / vcount(M_CP2)^2  #connectance

## assign attributes according to the taxon
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
#png("Graphs/M_CP_network.png", width = 800, height = 600)
pdf("Graphs/M_network.pdf", width = 5, height = 7)
M_CPplot<-plot(M_CP2,edge.arrow.size=.5, vertex.color= V(M_CP2)$color, vertex.size=6, vertex.label=NA,
               vertex.frame.color="black", vertex.label.color="black", 
               #vertex.label.color="black",  #vertex.label.cex=.5, vertex.label.dist=0.5, 
               edge.curved=0.2)
dev.off()



###### INTENSIVE NON-ORGANIC

IMCP_edge <- edge_list %>% filter(management == "IM") %>% select(node_from,node_to)

#Create igraph object
IM_CP<- graph_from_edgelist(as.matrix(IMCP_edge), directed = FALSE)

#Identify isolate nodes and remove from the igraph object
isolated_nodes <- which(degree(IM_CP) == 0)
IM_CP2<- delete_vertices(IM_CP, isolated_nodes)


S <- vcount(IM_CP2)#number of species
L <- ecount(IM_CP2) #number of links
#C <- ecount(IM_CP2) / vcount(IM_CP2)^2  #connectance

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
pdf("Graphs/IN_network.pdf", width = 5, height = 7)
IM_CPplot<-plot(IM_CP2,edge.arrow.size=.5, vertex.color= V(IM_CP2)$color, vertex.size=6, vertex.label=NA,
               vertex.frame.color="black", vertex.label.color="black", 
               #vertex.label.color="black",  #vertex.label.cex=.5, vertex.label.dist=0.5, 
               edge.curved=0.2)
dev.off()





#################### OLD 

#### INTENSIVE CROP PRODUCTION

ICP_edge <- edge_list %>% filter(management == "I") %>% select(node_from,node_to)

#Create igraph object
I_CP<- graph_from_edgelist(as.matrix(ICP_edge), directed = FALSE)

#Identify isolate nodes and remove from the igraph object
isolated_nodes <- which(degree(I_CP) == 0)
I_CP2<- delete_vertices(I_CP, isolated_nodes)


S <- vcount(I_CP2)#number of species
L <- ecount(I_CP2) #number of links
C <- ecount(I_CP2) / vcount(I_CP2)^2  #connectance

## assign attributes according to the taxon
nodes_in_network<- nodes %>% filter(node_id%in%ICP_edge$node_from | #create the nodelist according to the nodes present in the network
                                      node_id%in%ICP_edge$node_to )

V(I_CP2)$taxon<-nodes_in_network$taxon #create taxon attribute

#create color of taxon as attributes
vcol=V(I_CP2)$taxon
vcol[vcol== "Plant"]="#33a02c"
vcol[vcol=="Crop"]="#b15928"
vcol[vcol=="Flower-visiting"]="#a6cee3"
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
V(I_CP2)$color=vcol

  
#Plot 

png("Graphs/I_CP_network.png", width = 800, height = 600)
I_CPplot<-plot(I_CP2,edge.arrow.size=.5, vertex.color= V(I_CP2)$color, vertex.size=6, vertex.label=NA,
               vertex.frame.color="black", vertex.label.color="black", 
       #vertex.label.color="black",  #vertex.label.cex=.5, vertex.label.dist=0.5, 
       edge.curved=0.2)
dev.off()




################# INTENSIVE PASTURE

edge_list<-read.csv("Data/Land_use_rat_edgelist_weighted_PP_intense.csv", sep =",") %>% 
  mutate(weight == 1) #edge list

IPP_edge <- edge_list %>% filter(management == "I") %>% select(node_from,node_to)

#Create igraph object
I_PP<- graph_from_edgelist(as.matrix(IPP_edge), directed = FALSE)

#Identify isolate nodes and remove from the igraph object
isolated_nodes <- which(degree(I_PP) == 0)
I_PP2<- delete_vertices(I_PP, isolated_nodes)

## assign attributes according to the taxon
nodes_in_network<- nodes %>% filter(node_id%in%IPP_edge$node_from | #create the nodelist according to the nodes present in the network
                                      node_id%in%IPP_edge$node_to )

V(I_PP2)$taxon<-nodes_in_network$taxon #create taxon attribute

#create color of taxon as attributes
vcol=V(I_PP2)$taxon
vcol[vcol== "Plant"]="#33a02c"
vcol[vcol=="Crop"]="#b15928"
vcol[vcol=="Flower-visiting"]="#a6cee3"
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
V(I_PP2)$color=vcol


#Plot 

png("Graphs/I_PP_network.png", width = 800, height = 600)
I_PPplot<-plot(I_PP2,edge.arrow.size=.5, vertex.color= V(I_PP2)$color, vertex.size=6, vertex.label=NA,
               vertex.frame.color="black", vertex.label.color="black", 
               #vertex.label.color="black",  #vertex.label.cex=.5, vertex.label.dist=0.5, 
               edge.curved=0.2)
dev.off()





###OLD COLORS

vcol=V(I_PP2)$taxon
vcol[vcol== "Plant"]="#00C1AB"
vcol[vcol=="Crop"]="#BE9C00"
vcol[vcol=="Flower-visiting"]="#8CAB00"
vcol[vcol== "Aphid"]="#F8766D"
vcol[vcol=="Primary aphid parasitoid"]="#00BBDA"
vcol[vcol=="Secondary aphid parasitoid"]="#8B93FF"
vcol[vcol=="Leaf-miner parasitoid"]="#00BE70"
vcol[vcol== "Seed-feeding insect"]="#F962DD"
vcol[vcol=="Seed-feeding bird"]="#D575FE"
vcol[vcol=="Seed-feeding rodent"]="#FF65AC"
vcol[vcol== "Butterfly"]="#E18A00"
vcol[vcol=="Insect seed-feeder parasitoid"]="#24B700"
vcol[vcol=="Rodent ectoparasite"]="#00ACFC"

