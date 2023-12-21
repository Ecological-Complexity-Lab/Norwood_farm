require(igraph)
require(fs)
require(Rcpp)
require(tidyverse)


# Pure functions --------------------------------------------------------------

# Source C file with a single function, marginal_persistence_probs():
# it performs Monte Carlo simulation of Bayesian network, returning
# species' marginal persistence probabilities
# Input:
# - A: adjacency matrix of the food web
# - Pb: vector of baseline extinction probabilities
# - alpha: first parameter of the Beta distribution
# - beta: second parameter of the Beta distribution
# - nreps: number of iterations for Monte Carlo sim of Bayesian network
# Output:
# - vector of marginal persistence probabilities
sourceCpp("Code/BBN_analyses/code/bayesian_nw_monte_carlo.cpp")

# Make a digraph, given with the adjacency matrix A, acyclic
# Input:
# - A: adjacency matrix, sorted so producers are the first rows/columns
# Output:
# - another adjacency matrix, with cycles removed
DFS <- function(A) {
  DFSCOLOR <- numeric(0)
  DFSBACKEDGE <- numeric(0)
  ORDERVERTICES <- numeric(0)
  DFSVisit <- function(A, i) {
    DFSCOLOR[i] <<- 1
    for (j in 1:dim(A)[1]) {
      if(A[i,j] != 0) {
        if (DFSCOLOR[j] == 0) {
          DFSVisit(A,j)
        } else {
          if(DFSCOLOR[j] == 1) {
            DFSBACKEDGE[i,j] <<- 1 # It's a back edge: list for removal
          }
        }
      }
    }
    DFSCOLOR[i] <<- 2
    ORDERVERTICES <<- c(i, ORDERVERTICES)
  }
  run_DFS <- function(A) {
    S <- dim(A)[1]
    DFSCOLOR <<- rep(0, S)
    DFSBACKEDGE <<- matrix(0, S, S)
    ORDERVERTICES <<- numeric(0)
    for (i in 1:S) if (DFSCOLOR[i] == 0) DFSVisit(A, i)
    return(A - DFSBACKEDGE)
  }
  return(run_DFS(A))
}

# Create adjacency matrix from a data frame of edges
# Input:
# - edgelist: data frame with two columns of taxon names, where consumers
#             are in column 1 and resources in column 2
# Output:
# - The adjacency matrix, with A[i,j] = 1 if species i eats j and 0 otherwise.
#   Species are topologically sorted to make the matrix lower triangular.
#   The rows and columns of the matrix are labeled by taxon names.
create_adj_matrix <- function(edgelist) {
  # Create adjacency matrix A; links point from resource to consumer, so
  # the two columns of the edge list data frame are flipped
  A <- graph_from_data_frame(d = edgelist[,2:1]) %>% # create igraph graph
    as_adjacency_matrix %>% # convert to igraph adjacency matrix
    as.matrix %>% # convert to a regular matrix
    t # transpose result (so A[i,j] is 1 if i eats j and 0 otherwise)
  # Make sure producers are the first rows; this is needed for the DFS
  # algorithm to work (which removes all cycles from the digraph)
  A <- A[order(rowSums(A)),order(rowSums(A))]
  # Now make the digraph acyclic
  A <- DFS(A)
  # Find a sorting of A's rows and columns to make A lower triangular
  o <- graph_from_adjacency_matrix(A) %>% topo_sort(mode = "in")
  # Return sorted, lower triangular adjacency matrix A
  return(A[o,o])
}

# Obtain table of persistence probabilities
# Input:
# - edges: data frame with two columns of taxon names, where consumers
#          are in column 1 and resources in column 2
# - threats: data frame with its 1st column corresponding to taxon names,
#            and the rest to specific threats (and the baseline extinction
#            probabilities for each taxon listed under them)
# - alpha: 1st parameter of beta distribution, governing consumer response
#          to prey loss
# - beta: 2nd parameter of beta distribution, governing consumer response
#         to prey loss
# - nreps: number of iterations for Monte Carlo sim of the Bayesian network
# Output:
# - Data frame with taxon names, threats, and persistence probabilities
persistence_table <- function(edges, threats, alpha, beta, nreps) {
  A <- create_adj_matrix(unique(edges)) # sorted, lower triangular adj. matrix A
  threats <- threats[match(rownames(A), threats$taxon),] # match row order of A
  threatlist <- threats %>% # get vector of threat names
    select(-c(X, taxon, taxa_type, link_direction)) %>% # all columns except `taxon`
    colnames # extract names of remaining columns
  persistence <- tibble() # persistence table
 
   for (threat in threatlist) { # for each threat in list of threats:
    Pb <- threats %>% # obtain baseline extinction probs. from table of threats
      pull(threat) %>% # column `threat` as a vector
      replace_na(0) # replace NAs (for taxa not in the threat table) with 0s
    persistence <- tibble( # create data frame with:
      taxon = colnames(A), # taxon names
      threat = threat, # threat names
      persist = marginal_persistence_probs(A, Pb, alpha, beta, nreps) # persistence
    ) %>%
      bind_rows(persistence) # attach to main persistence table
  }
  return(persistence)
}


# Functions relying on external state -----------------------------------------

# Obtain persistence probabilities given a web and threat file,
# plus model parameters alpha, beta, and nreps
# Input:
# - webname: name of (sub-)web to analyze (with path & extension)
# - threatfile: file (with path & extension) containing threat information
# - alpha: 1st parameter of beta distribution, governing consumer response
#          to prey loss
# - beta: 2nd parameter of beta distribution, governing consumer response
#         to prey loss
# - nreps: number of iterations for Monte Carlo sim of the Bayesian network
# Output:
# - Data frame with taxon names, threats, persistence probabilities,
#   and parameters alpha, beta, and nreps

library(magrittr)
threat_analysis <- function(edges, threats, alpha, beta, nreps, webname) {
  threats <- threats %>% 
    filter(taxon %in% unique(c(edges$consumer, edges$resource)))
  persistence_table(edges, threats, alpha, beta, nreps) %>%
    mutate(alpha = alpha, beta = beta, nreps = nreps, web = webname) %>%
    return()
}







