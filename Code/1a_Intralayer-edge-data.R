## Multilayer analysis of the Norwood Farm Network ##


## 1 - Intralayer edges


#### Setup ####
rm(list=ls())
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

library(infomapecology); library(igraph); library(bipartite); library(tidyverse); library(magrittr)
library(betalink); library(readxl); library(ggalluvial); library(reshape2); library(betapart)




#### Functions #### 

# Function for converting an edgelist to a matrix
edgelist2Matrix <- function(elist){
  elist$lower <- as.character(elist$lower)
  elist$upper <- as.character(elist$upper)
  rows <- unique(elist$lower)
  cols <- unique(elist$upper)
  network <- matrix(0,nrow = length(rows),ncol = length(cols),dimnames = list(rows,cols))
  for (r in 1:nrow(elist)){
    lowerTaxon <- elist$lower[r]
    upperTaxon <- elist$upper[r]
    network[lowerTaxon,upperTaxon] <- elist$weight[r]
  }
  return(network)
}




#### Intralayer edges #### 

## Raw data in one data frame
edgelist <- read.csv("Data/nore2_aggregated.csv") # Load in long edgelist of intra-layer edges
edgelist_clean <- subset(edgelist, habitat != "") # Remove the abitrary self loops added for previous robustness calculations
edgelist_clean$uppercode <- substr(edgelist_clean$upper, 1, 4) 
edgelist_cleaner <- subset(edgelist_clean, uppercode != "01PL")


## Split out into individual lists
edgelist_split <- split(edgelist_cleaner, edgelist_cleaner$habitat) # Split out into different habitats 

## Simplify the graphs to have the sum of the unique links
edgelist_list <- NULL
for (i in 1:length(edgelist_split)){ 
  edgelist_list[[i]] <- edgelist_split[[i]] %>% 
    group_by(lower, upper) %>% 
    summarise(weight=sum(fortotals))
}

## We need to add the interactions from the layer "all" to others based on whether the plant is present

# Get the list of plants for each layer
plant_list <- lapply(edgelist_list, function(x){unique(x[,"lower"])})

# Subset "all" based on the plants present in all of the other layers
edgelist_list_adapted <- NULL
for (i in 2:length(edgelist_list)){ 
  
  edges <- edgelist_list[[1]]
  plants <- edgelist_list[[i]]
  
  sub <- edges[edges$lower %in% plants$lower,]
  
  edgelist_list_adapted[[i]] <- rbind(edgelist_list[[i]], sub)
  
}

## We need to merge several of the layers together to match the analyses in Evans et al. (2013) Ecology Letters

# Name the list with the appropriate layer names
names(edgelist_list_adapted) <- names(edgelist_split)

# Remove the "all" layer as it is now redundant
edgelist_list_adapted <- edgelist_list_adapted[2:15] # Remove the "all" layer

# Merge "RG" and "RGyard" 
edgelist_list_adapted[["RGtotal"]] <- rbind(edgelist_list_adapted[["RG"]], edgelist_list_adapted[["RGyard"]])

# Merge "W" and "WU" 
edgelist_list_adapted[["WD"]] <- rbind(edgelist_list_adapted[["W"]], edgelist_list_adapted[["WU"]])

edgelist_list_final <- edgelist_list_adapted[c(1:9,15,16)]

names(edgelist_list_final) <- c("CP", "SF", "GM", "LP", "LU", "MH", "NH", "NL", "PP", "RG", "WD")

# Standardisation function
weight_st <- function(x){ 
  x$weight_s <- x$weight/sum(x$weight)
  return(select(x, lower, upper, weight_s))
}

# Standardise edge weights within layers
edgelist_list_st <- lapply(edgelist_list_final, weight_st)

# Create a long dataframe
intralayer_edges <- bind_rows(edgelist_list_final, .id = "habitat")

## Create matrices from the edgelists
matrix_list <- lapply(edgelist_list_final, edgelist2Matrix)

## Create igraph objects from the edgelists 
graphs_list <- lapply(edgelist_list_final, function(x){graph_from_data_frame(x, directed = T)})

## Create igraph objects from the standardised edgelists 
graphs_list_st <- lapply(edgelist_list_st, function(x){graph_from_data_frame(x, directed = T)})
