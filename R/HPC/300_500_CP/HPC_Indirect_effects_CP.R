# bash intro
#! /gpfs0/shai/projects/R4/R-4.2.0/bin/Rscript
.libPaths("/gpfs0/shai/projects/R4/R-4.2.0/lib64/R/library")
print(.libPaths())
print(sessionInfo())
Sys.setlocale(category = "LC_ALL", locale = "")

library(readr)
library(tidyverse)
library(magrittr)

# ------------- parsing arguments -----------
# read args given in command line:
JOB_ID <- Sys.getenv("JOB_ID")
if (length(commandArgs(trailingOnly=TRUE))==0) { # make sure we have commands
  stop('No arguments were found!') # the script will not run without arguments
} else {
  args <- commandArgs(trailingOnly=TRUE)
  mngmnt <- as.character(args[1])
}

# this script uses the received arguments to produce a calculation
data <- read.csv("./ind_1hop_sim_CP.csv") %>% filter(management == mngmnt)

ite =301:500
Indirect_1hop <-data %>% filter(iteration == "Emp" | iteration%in%ite)

## Function to identify potential second order indirect effects for node_from in the row
ind_row <- function(df, row) { #for the row
  if (row %% 1000 == 0) {
    print(100 * row/nrow(df))
  }
  j <- df$node_to[row] #select node_to 
  k<-df$iteration[row] #select iteration 
  
  df %>% 
    dplyr::filter(node_from == j, node_to != df$node_from[row],  #filter to avoid counting the interaction from node 2 to node 1 because the edgelist is directed 
           iteration == k, services_to != "None") %>% # Filter dataframe (filter node 3's ES affected by node 2)
    mutate(node_id = df$node_from[row], #create dataframe to store
           node_int = j,
           taxon_from = df$taxon_from[row],
           services = df$services_from[row],
           management = mngmnt,
           iteration = k,
           type = "I",
           hop = 2)
}

Indirect_2hop_sim <- bind_rows(lapply(1:nrow(Indirect_1hop), function(row) ind_row(Indirect_1hop, row))) %>% 
  select(management,iteration,node_id,taxon_from,services, node_int,node_to,
         services_to,type,hop)
print(100)

output_file <- paste("Results/", mngmnt, "_ind_2hop_sim_301_500_CP.csv", sep = "")
write.csv(Indirect_2hop_sim, output_file, row.names= FALSE)

