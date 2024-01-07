########################## Centrality measure ################

library(emln)#multilayer package
library(readr)
library(ggplot2)
#library(centiserve)
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")


######### --- CALCULATE CENTRALITY TO CHECK THE IMPORTANCE OF NODES ON ES (INDIRECTLY) 
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
Norwood_farm$extended_ids$weight<-1 # non-weighted for now 


##### -- Eigenvector centrality (MUXVIZ PACKAGE)
library(muxViz)

# assigns higher scores to nodes that are connected to other nodes with high
#centrality scores

## -- considering all habitats together

# get the SAM
sam_multilayer <- get_sam(multilayer = Norwood_farm, bipartite = F, directed = F,
                          sparse = F, remove_zero_rows_cols = F)

# get the eigen vector centrality

eigen_multilayer<-GetMultiEigenvectorCentrality(sam_multilayer$M,
                                              Layers = 11, Nodes =549) #Number of physical nodes

eigen_multi<-cbind(node_id = seq(1:549),eigen_multilayer) %>% as.data.frame() %>% rename("eigen"= "V2")



##### -- Katz centrality (MUXVIZ PACKAGE)

#takes into account both direct connections and indirect connections through paths
#of varying lengths. Also we can consider different path lengths adjusting the parameter alpha
#Also, it solves the directed problem with eigen vectors


## -- considering all habitats together
library(calibrate)

# get the SAM
sam_multilayer <- get_sam(multilayer = Norwood_farm, bipartite = F, directed = F,
                          sparse = F, remove_zero_rows_cols = F)

#Function "GetMultiKatzCentrality" to calculate Katz centrality using muxviz package
  
#Input
  SupraAdjacencyMatrix = sam_multilayer$M
  Layers = 11
  Nodes = 549
  
  # we pass the transpose of the transition matrix to get the left eigenvectors
  tmp <- GetLargestEigenv(Matrix::t(SupraAdjacencyMatrix))
  LeadingEigenvalue <- tmp$LMatrix
  
  #Katz kernel tensor
  deltaTensor <- kronecker(speye(Nodes), speye(Layers))
  
  #this ensures convergence of the Katz kernel tensor
  a <- 0.9999 / abs(LeadingEigenvalue) #I think this is aplha, which we could change to affect the path's length
  
  KatzKernelTensor <- solve(deltaTensor - a * SupraAdjacencyMatrix)
  
  KatzCentralitySupraVector <-
    KatzKernelTensor %*% ones(Nodes * Layers, 1)
  CentralityVector <-
    sumR(reshapeR(KatzCentralitySupraVector, Nodes, Layers), 2)
  katz_multilayer <- CentralityVector / max(CentralityVector)

  
katz_multi<-cbind(node_id = seq(1:549),katz_multilayer) %>% as.data.frame() %>% rename("katz"= "V2")


# Add information as species name and taxon

katz_multi_fin<-right_join(katz_multi, Norwood_farm$nodes, by = "node_id") 


##### --  Final dataframe with both centralities measures

centrality_sps<-cbind(katz_multi_fin,eigen = eigen_multi$eigen)  %>% 
  dplyr::select("node_id", "katz", "node_name", "taxon", "eigen")

#write.csv(centrality_sps,"Data/centrality_sps.csv")



##### -- Plots

centrality_sps<-read.csv("Data/centrality_sps.csv", sep =",",row.names = 1)

## - Plot eigen vector centrality per species (few species are very important)

distr_eigen<-hist(centrality_sps$eigen)
eigen_sps<- centrality_sps %>% dplyr::select (-katz) %>% 
  ggplot(aes(y=eigen, x= node_id)) + geom_bar(position="dodge", stat="identity")+
  labs(x='Species', y="Eigen Centrality") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =6, angle = 90), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

eigen_sps





## - Plot Katz centrality per species (few species are very important)

# Histogram
distr_katz<-hist(centrality_sps$katz, breaks = 100,
                 main = "Histrogram of Katz centrality", 
                 xlab = "Katz centrality") #histogram

# Plot 10 species with the highest centrality
katz_sps<-  centrality_sps %>% arrange(desc(katz)) %>% head(10) %>%  
  ggplot(aes(y=katz, x= node_name)) + geom_bar(position="dodge", stat="identity")+
  labs(x='Species', y="Katz Centrality") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =9, angle = 90), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

katz_sps


################### HASTA ACA POR AHORA

















##### -- Eigenvector centrality (EMLN PACKAGE)

# assigns higher scores to nodes that are connected to other nodes with high
#centrality scores


## -- in each habitat as monolayer

# get a list of igraph objects as layers igraph 
g_layer <- get_igraph(Norwood_farm, bipartite = F, directed = F)

# get Eigenvector scores of igraph layers
eigen_igraph = c()
for (layer in g_layer$layers_igraph){
  eigen_igraph <- append(eigen_igraph, igraph::eigen_centrality(layer, scale = T)$vector)
}



## -- considering all habitats together (we put indirected otherwise it will not close the loop)

# get the SAM
sam_multilayer <- get_sam(multilayer = Norwood_farm, bipartite = F, directed = F,
                          sparse = F, remove_zero_rows_cols = T)


# converting SAM to igraph object
graph_sam <- graph_from_adjacency_matrix(sam_multilayer$M,mode = "undirected",weighted = TRUE,
                                         diag = TRUE,add.colnames = NULL,add.rownames = TRUE)

# get eigenvector scores
eigen_sam <- igraph::eigen_centrality(graph_sam, directed = NULL, scale = T, 
                                      weights = NULL)

#check
sps_list<-sam_multilayer$state_nodes_map %>% filter(!is.na(layer_id)) %>% 
  select(layer_name,node_id,node_name)# the eigen values correspond to these species

cent_multi<-cbind(sps_list,eigen_centrality =eigen_sam$vector)

# Add information of taxon

cent_multi_fin<-right_join(cent_multi, Norwood_farm$nodes, by = "node_id") %>% 
  select(layer_name,node_id,node_name.x,taxon, eigen_centrality) %>% rename("node_name" = "node_name.x")


## - check if eigen values differ between mono and multilayer approach 

# This results is strange but similar to the example on the wiki)

# merge to data frame eigenvector comparison
ec_comparison <- data.frame(eigen_sam$vector, eigen_igraph)#eigen_sam without considering NA
colnames(ec_comparison) <- c("multilayer","monolayer")

# scatter plot using r ggplot2
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




## -- Plot: importance of sps affecting indirectly ES 

#The layer CP is triggering all the weights..

cent_multi_fin_sps<- cent_multi_fin %>% group_by(node_id) %>% 
  summarise(mean_cent = mean(eigen_centrality), sd_cent = sd(eigen_centrality))

centrality_sps<- cent_multi_fin_sps %>%
  ggplot(aes(y=mean_cent, x= node_id)) + geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin = 0, ymax = mean_cent + sd_cent)) +
  labs(x='Species', y="Eigen Centrality") +theme_bw()+
  scale_x_continuous(breaks = seq(0, 600, by = 10))+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =7, angle = 90), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11))

centrality_sps




##### -- Katz centrality

#takes into account both direct connections and indirect connections through paths
#of varying lengths. Also we can consider different path lengths adjusting the parameter alfa


## -- in each habitat as monolayer

# get a list of igraph objects as layers igraph 
g_layer <- get_igraph(Norwood_farm, bipartite = F, directed = F)

# get Eigenvector scores of igraph layers
katz_igraph = c()

for (layer in g_layer$layers_igraph){
  katz_igraph <- append(katz_igraph, centiserve::katzcent(layer)) # which alpha should I use?
}

# Normalize centrality scores (normalized for the maximum)
#katz_centrality_normalized <- katz_centrality / max(katz_centrality)



alpha <- 0.8  # Adjust this value based on your preference

# Set the beta parameter (inherent influence)
beta <- 1 - alpha

# Calculate Katz centrality for nodes within 2 hops
ketz_igraph_prueba = c()
for (layer in g_layer$layers_igraph){
  ketz_igraph_prueba <- append(ketz_igraph_prueba, alpha * as.matrix(layer) %*% matrix(1, nrow = vcount(layer), ncol = 1) + beta)
}




################# CHAT GTP #############################
##Ninguna de las anteriores parece funcionar bien.. calcularla segun chat gpt funcinó

#To do: check the formula

#apply it for each layer in a loop

#then multilayer

###

Prueba_CP<-Norwood_farm$extended_ids %>% filter(layer_from == 1)

#to igraph
graph_CP <- graph_from_data_frame(Prueba_CP, directed = FALSE)

# Convert the graph to an adjacency matrix
adjacency_matrix_CP <- as.matrix(get.adjacency(graph_CP, sparse = FALSE))

# Set the alpha parameter (attenuation factor)
alpha <- 0.1  # Adjust this value based on your preference

# Set the beta parameter (inherent influence)
beta <- 1 - alpha

# Calculate Katz centrality using matrix multiplication
katz_centrality_cp <- alpha * solve(diag(vcount(graph_CP)) - alpha * as.matrix(adjacency_matrix_CP)) %*%
  matrix(1, nrow = vcount(graph_CP), ncol = 1) + beta













####prueba kefi
kefi <- load_emln(13)


# get the SAM
sam_multilayer_kefi_d <- get_sam(multilayer = kefi, bipartite = F, directed = T, sparse = F, remove_zero_rows_cols = F)

# converting SAM to igraph object
graph_sam_kefi_d <- graph_from_adjacency_matrix(sam_multilayer_kefi$M,mode = "directed",weighted = NULL,diag = TRUE,add.colnames = NULL,add.rownames = NA)

# get Eigenvector scores
eigen_sam_kefi_d <- igraph::eigen_centrality(graph_sam_kefi, directed = T, scale = T)$vector
