#In this code, we estimate the indirect contribution of species to ecosystem service (ES) provision in each land management
#type using the shortest path. Additionally, we test how land conversion affects the indirect contribution of core 
#species to ES.


## -- Load libraries --------------------------------------------------------------------------------------------------------
library(igraph)
library(dplyr)
library(tidyverse)
library(emln)

## -- get_data--------------------------------------------------------------------------------------------------------
setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio") #set directory


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   SHORT-PATH ESTIMATION                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### 1. Arrange dataframe

# List of nodes with attributes
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
nodes<- Norwood_farm$nodes %>% #list of nodes with attributes
        gather(services,value,4:10) %>% select(-ES,-DES)  %>% 
        filter(value>0)

# Upload edge list of each management scenario
edge_list<- read.csv("Data/Land_use_edgelist.csv", sep = ",")


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


## Estimate shortest path between species in each network 
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

#write.csv(species_shortpath_fin, "Data/Land_use_shortpath.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      STATISTICAL ANALYSIS                              
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### upload dataframe
short_path_land_change<-read.csv("Data/Land_use_shortpath.csv", row.names = 1) 


##### 1. Test if land use change affect the average indirect important of species

## Calculate average short path of each species to all ES in each habitat management
short_path_land_change_ave<- short_path_land_change %>% group_by(management,node_id) %>% 
                            mutate(short_path_ave = mean(short_ave)) %>% select(-services, - short_ave) %>% unique()


## Model
library(glmmTMB)
library(emmeans)
library(car)

short<- glmmTMB(short_path_ave~management + taxon, 
                  family = Gamma(link = "log"), data = short_path_land_change_ave) # model that best fit

Anova(short)
model_summary<-summary(short)


# Extract the coefficients (for GLMM it's the average including all the levels)
coefficients <- model_summary$coefficients$cond

# Extract coefficients for each factor
management_coefs <- coefficients[grep("management", rownames(coefficients)), "Estimate"]
taxon_coefs <- coefficients[grep("taxon", rownames(coefficients)), "Estimate"]

# Calculate summary statistics for each factor
management_summary <- mean(management_coefs)
taxon_summary <- mean(taxon_coefs)

#Homogeneity
EM<-resid(short, type= "response") 
FM<-fitted(short) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(short, type= "response") 
boxplot(E1_lme~short_path_land_change_ave$management, main="Management")



##### 2. Test if the indirect role of core species per trophic group (extensive) change across land conversion

## Identify the 5 most important species per trophic group in the extensive scenario
top_5_taxon_extensive<-short_path_land_change_ave %>%
  filter(management == "E") %>% group_by(management, taxon) %>% 
  arrange(short_path_ave) %>% # Arrange by short_path_ave within each group
  slice_head(n = 5) # Take the first 5 rows within each group


## Filter the importance of the top 5 species (from the extensive) across management scenarios
top_5_average<- short_path_land_change_ave %>%filter(node_id%in%top_5_taxon_extensive$node_id) 


## Model
top_5<- glmmTMB(short_path_ave~management+ taxon+(1|node_id), 
                    family = Gamma(link = "log"), data = top_5_average) #best model

Anova(top_5)
model_summary<-summary(top_5) #this model fits the best (drop taxon no significant difference)


# Extract the coefficients (for GLMM it's the average including all the levels)
coefficients <- model_summary$coefficients$cond

# Extract coefficients for each factor
management_coefs <- coefficients[grep("management", rownames(coefficients)), "Estimate"]
taxon_coefs <- coefficients[grep("taxon", rownames(coefficients)), "Estimate"]

# Calculate summary statistics for each factor
management_summary <- mean(management_coefs)
taxon_summary <- mean(taxon_coefs)


#Homogeneity
EM<-resid(top_5) 
FM<-fitted(top_5) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(top_5) 
boxplot(E1_lme~top_5_average$management, main="Tratamiento")


