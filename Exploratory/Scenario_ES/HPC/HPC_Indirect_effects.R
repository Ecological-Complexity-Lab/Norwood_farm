# bash intro
#! /gpfs0/shai/projects/R4/R-4.2.0/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.2.0/lib64/R/library")
print(.libPaths())
print(sessionInfo())
Sys.setlocale(category = "LC_ALL", locale = "")

library(readr)
library(tidyverse)
library(magrittr)


# this script uses the received arguments to produce a calculation

Indirect_1hop <- read.csv("./ind_1hop_sim_CP.csv")

## Function to identify potential second order indirect effects for node_from in the row
ind_row <- function(df, row) { #for the row
  if (row %% 1000 == 0) {
    print(100 * row/nrow(df))
  }
  j <- df$node_to[row] #select node_to 
  l <- df$management[row] #select management 
  k<-df$iteration[row] #select iteration 
  
  df %>% 
    dplyr::filter(node_from == j, node_to != df$node_from[row],  #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
           iteration == k, management == l) %>% # Filter dataframe (filter node 3's ES affected by node 2)
    mutate(node_id = df$node_from[row], #create dataframe to store
           node_int = j,
           taxon_from = df$taxon_from[row],
           services = df$services_from[row],
           management = l,
           iteration = k,
           type = "I",
           hop = 2)
}

Indirect_2hop_sim <- bind_rows(lapply(1:nrow(Indirect_1hop), function(row) ind_row(Indirect_1hop, row))) %>% 
  select(management,iteration,node_id,taxon_from,services, node_int,node_to,
         services_to,type,hop)

write.csv(Indirect_2hop_sim,"ind_2hop_sim_CP.csv", row.names= FALSE)

