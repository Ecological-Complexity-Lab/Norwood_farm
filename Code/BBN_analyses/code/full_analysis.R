
library(tidyverse)
library(readr)
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")


####################### HABITAT LOSS ###################################

#We calculate how species are affected by loss of the various habitats. To do this, we extract the fraction of all its links 
#a taxon have in a specific habitat. These values are then directly used to increase 
#the taxa probability of going extinct (by increasing baseline extinction.

# read in original file showing which interactions are found in which habitat
habitat_all <- tibble(read.csv("./Data/nore2_aggregated.csv"))

# clean the data
habitat_all <- habitat_all %>%
  separate(lower, c("lower_type"), remove = FALSE) %>%
  separate(upper, c("upper_type"), remove = FALSE) %>%
  unite("link_type",lower_type, upper_type, remove = FALSE) %>%
  relocate(link_type, .after = upper_type) %>% 
  select(-c(fortotals, round)) %>%
  filter(!(lower_type == upper_type)) %>% #
  filter(!(upper_type == "01PLANT")) %>% # filter away entities where plants are not the lowest level 
  distinct() 


# combine lower and upper so all in a new column taxon
habitat_lower <- habitat_all %>% # separate lower and upper
  select(lower, lower_type, link_type, habitat)  %>%
  rename(taxon = lower, taxa_type = lower_type) %>%
  mutate(link_direction = "lower")
# now each taxon 

habitat_upper <- habitat_all %>%
  select(upper, upper_type, link_type, habitat) %>%
  rename(taxon = upper, taxa_type = upper_type) %>%
  mutate(link_direction = "higher")

habitat_lower_upper <- union_all(habitat_lower, habitat_upper) 
# each species in each habitat has its own row and taxa_type, link_type and link_direction are included.

# get the fraction of a species total number of interactions that take place in each habitat
habitat_fracs <- habitat_lower_upper%>%
  group_by(taxon, habitat, link_direction, taxa_type) %>%   # OR count(lower, habitat) %>% 
  tally() %>%
  ungroup() %>%
  rename(int_per_hab = n) %>%
  group_by(taxon) %>%
  mutate(tot_interactions = sum(int_per_hab)) %>%
  ungroup() %>%
  mutate(frac_per_hab = int_per_hab / tot_interactions) %>%
  select(-c(int_per_hab, tot_interactions)) %>%
  # frac_per_hab shows the fraction of all its links a taxon have in a specific habitat
  # now make a data frame with habitats in one column each 
  pivot_wider(names_from = habitat, values_from = frac_per_hab, values_fill = 0)
#select only the plants
#filter(taxa_type == "01PLANT")

write.csv(habitat_fracs, file =  "Code/BBN_analyses/new_data_files/norwood_therats_plants.csv")  

######################### PARAMETERS #############################

#set the parameters to use in the code that should be executed

alpha <- 1 # alpha and beta determines how a consumer respond to loss of resources (1,1) means a linear response.
beta <- 1
nreps <- 10 # Number of iterations, set to 10.000 for real runs
webname <- "norwood"


######################## LOAD DATA FILES #######################

edges <- tibble(read.csv("Data/species_edgelist.csv"))  %>%
  select(lower, upper) %>%
  rename(resource = lower, consumer = upper) #List of links

threats <- tibble(read.csv("Code/BBN_analyses/new_data_files/norwood_therats_plants.csv")) # constructed before in habitat_loss

services <- tibble(read.csv("Data/service_edgelist.csv")) %>%
  rename (taxon = lower, ESS = upper) %>%
  pivot_wider(names_from = ESS, values_from = weight, values_fill = 0) %>%
  mutate(taxa_type = str_split(taxon, pattern = "[.]")[[2]][1]) #list of ecosystem (dis)services that each species provides


####################### THREAT ANALYSIS #######################

#here we calculated the persistence of each species. persistence´gives the likelihoods 
#for being extant for all species depending on threat (here habitat loss) realized 

source("Code/BBN_analyses/code/threat_functions_Norwood.R") # functions for analyzing how different threats affect persistence of different species 
persistence <- threat_analysis(edges, threats, alpha, beta, nreps, webname) 
write_rds(persistence, "Code/BBN_analyses/Results/persistence_norwood_tinio.rds", compress = "xz")


###################### ESS ANALYSIS ###########################

#Functions for analyzing the how changes in species persistence affect ecosystem services

services[,2:9] <- lapply(services[,2:9], as.character)
source("Code/BBN_analyses/code/get_ESS_norwood.R")  # functions for analyzing the how changes in species persistence affect ecosystem services 


##################### PLOTTING RESULTS ######################

ESS <- read_rds("Code/BBN_analyses/Results/norwood_ESS_changes_tinio.rds")
persistence <-read_rds("Code/BBN_analyses/Results/persistence_norwood_tinio.rds")

#Plot showing how habitat loss (x-axis) affect the fraction of services reatined in the farm (y-axis)

ESS %>% 
  filter(ESS_alpha==1, ESS_beta ==1) %>% #linear response
  filter(!threat == "all") %>%
  ggplot(aes(x = threat, y = service_total)) +
  geom_boxplot() +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(ESS)), size = 3, show.legend = T) +
  # scale_fill_manual(values = col) + 
  scale_fill_brewer(palette="PRGn") +
  scale_y_continuous(name = "Fraction of service retained", limits = c(0.2, 1)) + 
  scale_x_discrete(name = "Habitat lost")



# Plot showing species role in the network. With role means here if the species is:
#1) only directly affected by the threat, 2) only directly providing an ESS,
#3) is both directly affected by the threat and directly provides ESS, or 
#4) is neither affected by the threat or directly provides ESS.

plot_overlap_roles <- function(sp_def, threats, ess) {
  new <- NULL
  for(t in threat){
    for(e in ess){
      sp_overlap <- sp_def %>%
        select(taxon, t, e) %>%
        mutate_if(is.numeric, ~1 * (. != 0)) %>%
        mutate(ess = e, threat = t) %>%
        unite("overlap", t:e, sep = "_") %>%
        mutate(overlap = case_when(overlap == '1_0' ~ 'threatened',
                                   overlap == '0_1' ~ 'ESS provider',
                                   overlap == '1_1' ~ 'both',
                                   overlap == '0_0' ~ 'none'))
      
      new <- rbind(sp_overlap, new)
    }
  }
  level_order <- ess
  OL<- new %>%
    ggplot(aes(x = factor(ess, level = level_order), fill = overlap)) + 
    geom_bar() +
    scale_fill_brewer(palette = "RdBu", direction = -1) +
    # scale_fill_viridis_d(alpha = 0.8) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10)) +
    theme(strip.text = element_text(colour = 'black', size=10)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Ecosystem service", y="Number of species", size = 15) +
    facet_wrap(~ threat, nrow = 2) %>%
    return(OL)
  
}

# needed for general properties
#threats<-  tibble(read.csv("../new_data_files/norwood_therats_plants.csv")) # needed for counting species threatened/ESS providers
threatlist <- threats %>%
  select(!X)
ESSlist <- services

sp_def <- ESSlist %>%
  left_join(threatlist, by = "taxon")

a <- 1
b <- 1
a_ess <- 1
b_ess <- 1

ess <- names(ESSlist[2:8])
threat <- names(threatlist[4:18])

fname <- paste0("../figures/ESS_changes/species_roles_overlap_Tinio",
                a, "-", b, "-ESSsum-", a_ess, "-", b_ess,".pdf")
overlap_roles <- plot_overlap_roles(sp_def, threats, ess) # Create figure
plot(overlap_roles)
