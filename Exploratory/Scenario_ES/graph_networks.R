############################## Script to create figure of ecological networks ################################

library(igraph)
library(tidyverse)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")


############### Land use change from EXTENSIVE TO CROP PRODUCTION

Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object to get the node's list

nodes<- Norwood_farm$nodes #node list with attributes
edge_list<-read.csv("Data/Land_use_rat_edgelist_weighted_CP_intense.csv", sep =",") %>% 
                    mutate(weight == 1) #edge list


###### EXTENSIVE

E_edge <- edge_list %>% filter(management == "E") %>% select(node_from,node_to)

E<- graph_from_edgelist(as.matrix(E_edge), directed = FALSE) #create igraph object


# assign attributes according to the taxon
V(E)$taxon<-nodes$taxon #create taxon attribute

#create color of taxon as attributes
vcol=V(E)$taxon
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

V(E)$color=vcol

  
 
##Plot

png("E_network.png", width = 800, height = 600)
E_network<-plot(E,edge.arrow.size=.5, vertex.color= V(E)$color, vertex.size=4, vertex.label=NA,
     vertex.frame.color="black", vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=0.5, edge.curved=0.2)

dev.off()

png("Legend_network.png", width = 800, height = 600)
legend_network<-plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

legend("bottom", legend = c("Plant","Crop","Flower-visiting","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                           "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                           "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"), pch=21,
       col=c("#00C1AB","#BE9C00","#8CAB00","#F8766D","#00BBDA","#8B93FF","#00BE70","#F962DD","#D575FE","#FF65AC","#E18A00","#24B700",
             "#00ACFC"), 
       pt.bg=c("#00C1AB","#BE9C00","#8CAB00","#F8766D","#00BBDA","#8B93FF","#00BE70","#F962DD","#D575FE","#FF65AC","#E18A00","#24B700",
               "#00ACFC"), pt.cex=2, cex=1, bty="n")

dev.off()


#### INTENSIVE CROP PRODUCTION

ICP_edge <- edge_list %>% filter(management == "I") %>% select(node_from,node_to)

#Create igraph object
I_CP<- graph_from_edgelist(as.matrix(ICP_edge), directed = FALSE)

#Identify isolate nodes and remove from the igraph object
isolated_nodes <- which(degree(I_CP) == 0)
I_CP2<- delete_vertices(I_CP, isolated_nodes)

## assign attributes according to the taxon
nodes_in_network<- nodes %>% filter(node_id%in%ICP_edge$node_from | #create the nodelist according to the nodes present in the network
                                      node_id%in%ICP_edge$node_to )

V(I_CP2)$taxon<-nodes_in_network$taxon #create taxon attribute

#create color of taxon as attributes
vcol=V(I_CP2)$taxon
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

V(I_CP2)$color=vcol

  
#Plot 

png("I_CP_network.png", width = 800, height = 600)
I_CPplot<-plot(I_CP2,edge.arrow.size=.5, vertex.color= V(I_CP2)$color, vertex.size=6, vertex.label=NA,
       vertex.frame.color="black", 
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

V(I_PP2)$color=vcol


#Plot 

png("I_PP_network.png", width = 800, height = 600)
I_PPplot<-plot(I_PP2,edge.arrow.size=.5, vertex.color= V(I_PP2)$color, vertex.size=6, vertex.label=NA,
               vertex.frame.color="black", 
               #vertex.label.color="black",  #vertex.label.cex=.5, vertex.label.dist=0.5, 
               edge.curved=0.2)
dev.off()




