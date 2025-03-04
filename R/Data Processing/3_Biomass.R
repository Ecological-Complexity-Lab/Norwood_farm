# In this code we assigned biomass to ind/species using databases and literature.


## -- Load libraries --------------------------------------------------------------------------------------------------------
library(vegan)
library(sna)
library(tidyverse)
library(readxl)

## -- get_data--------------------------------------------------------------------------------------------------------
setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")

## -- Upload list of nodes
Norwood_farm<-readRDS("Data/Norwood_farm.RData") # call multilayer object previously generated

## -- Create list of species
species_list<-Norwood_farm$nodes %>% select(node_id,node_name,taxon) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) 


## -- Check for biomass/ind per species in different repositories or using literature -------------------------------------------------------


## -- Birds and mammals species (extracted from: Wilman et al. (2016). Repository: https://doi.org/10.6084/m9.figshare.3559887.v1)
  
# Rearrange dataframe
birds.repo<-read.delim ("Data/repositories/BirdFuncDat.txt") %>% 
          select(Scientific,BodyMass.Value) %>% rename("node_name" = "Scientific",  "biomass.g" = "BodyMass.Value")

mam.repo<-read.delim ("Data/repositories/MamFuncDat.txt") %>% 
          select(Scientific,BodyMass.Value) %>% 
          rename("node_name" = "Scientific",   "biomass.g" = "BodyMass.Value")                                                       

# Check for those species in the repository that match in our dataframe
mass.birds<- species_list %>% left_join(birds.repo, by = "node_name") %>% 
  filter(taxon =="Seed-feeding bird")

mass.mam<- species_list %>% left_join(mam.repo, by = "node_name") %>% 
  filter(taxon =="Seed-feeding rodent")


## -- Aphid species (extracted from: Brose et al. (2005). Repository: https://doi.org/10.1890/05-0379)

## Rearrange dataframe
aphid.par.repo<-read.delim("Data/repositories/bodysizes_2008.txt") 

filter_sps<-aphid.par.repo %>% filter(Taxonomy.consumer%in%species_list$node_name |
                                     Taxonomy.resource%in%species_list$node_name) %>% 
                                select(Taxonomy.consumer,Mean.mass..g..consumer,Taxonomy.resource,
                               Mean.mass..g..resource) %>%  #filter sps from the database  
                               unique() #the biomass is already averaged

# split consumer and resource information
upper<- filter_sps[,1:2] %>% rename("node_name" = "Taxonomy.consumer", "biomass.g" ="Mean.mass..g..consumer")
lower<-filter_sps[,3:4] %>% rename("node_name" = "Taxonomy.resource", "biomass.g" ="Mean.mass..g..resource")

# function to keep the first two words in species name from the repository
keep_first_two_words <- function(text) {
  words <- str_split(text, "\\s+")[[1]]
  paste(words[1:2], collapse = " ")
}

# apply function
aphid.par.bio<-rbind(upper,lower) %>% unique() %>% 
  filter(biomass.g >0) %>% # eliminate species with - biomass (error in dataframe)
  mutate( node_name = sapply(node_name, keep_first_two_words)) %>% 
  mutate(node_name = gsub("\\bNA\\b", "", node_name)) # eliminate extra text in some columns
 
# add data of biomass manually and do the average for the rest. Data were extracted from literature (see list of papers in the appendix)
mass.aphid<- species_list %>% left_join(aphid.par.bio, by = "node_name") %>% 
  unique() %>%  mutate (biomass.g = case_when(node_name == "Acyrthosiphon pisum"~ 0.0008623,
                                              node_name == "Myzus persicae"~ 0.0002042,
                                              node_name == "Sitobion avenae"~ 0.0031632,
                                              TRUE~biomass.g)) %>% 
  filter(taxon == "Aphid") %>% mutate(biomass.g =if_else(is.na(biomass.g), 
                                                         mean(biomass.g, na.rm = TRUE), biomass.g))

## -- Primary aphid parasitoids 

#add data of biomass manually and do the average for the rest
mass.1.par<-species_list %>% left_join(aphid.par.bio, by = "node_name") %>% 
  unique() %>% filter(taxon == "Primary aphid parasitoid") %>% 
  mutate (biomass.g = case_when(node_name == "Aphidius rhopalosiphi"~ 0.000161,
                                node_name == "Aphidius matricariae"~ 0.00088,
                                    TRUE~biomass.g)) %>% # add manually more data
      mutate(biomass.g =if_else(is.na(biomass.g), 
                               mean(biomass.g, na.rm = TRUE), biomass.g))
  
## -- Secondary aphid parasitoids 
mass.2.par<-species_list %>% left_join(aphid.par.bio, by = "node_name") %>% 
            unique() %>% filter(taxon == "Secondary aphid parasitoid")%>% 
             mutate (biomass.g = case_when(node_name == "Asaphes suspensus "~ 0.00000927,
                                  TRUE~biomass.g)) %>% # add manually more data extracted from literature
              mutate(biomass.g =if_else(is.na(biomass.g), 
                          mean(biomass.g, na.rm = TRUE), biomass.g)) # do the average for the rest



  
##  -- Insect seed feeder parasitoid 
mass.ins.par<-species_list %>% left_join(aphid.par.bio, by = "node_name") %>% 
  unique() %>% filter(taxon == "Insect seed-feeder parasitoid") %>% 
  mutate (biomass.g = case_when(node_name == "Pteromalus albipennis"~ 0.0013,
                                node_name == "Mesopolobus incultus"~ 0.00045,
                                node_name == "Pteromalus elevatus"~ 0.0121,
                                node_name == "Bracon immutator"~ 0.0025,
                                node_name == "Bracon osculator"~ 0.0025,
                                node_name == "Bracon praecox"~ 0.0025,
                                TRUE~biomass.g)) %>% # add manually more data extracted from literature
                              mutate(biomass.g =if_else(is.na(biomass.g), 
                             mean(biomass.g, na.rm = TRUE), biomass.g)) # do the average for the rest


## -- Seed-feeding insect
mass.seed.ins<-species_list %>% left_join(aphid.par.bio, by = "node_name") %>% 
  unique() %>% filter(taxon == "Seed-feeding insect")  %>% 
  mutate (biomass.g = case_when(node_name == "Bruchidius varius"~ 0.003,
                                node_name == "Olibrus aeneus"~ 0.000251,
                                node_name == "Rhinocyllus conicus"~ 0.00845,
                                node_name == "Sitona lineatus"~ 0.00165,
                                node_name == "Urophora stylata"~ 0.0041,
                                TRUE~biomass.g)) %>% # add manually more data extracted from literature
          mutate(biomass.g =if_else(is.na(biomass.g), 
          mean(biomass.g, na.rm = TRUE), biomass.g)) # do the average for the rest



## -- Leaf miners
mass.leaf.min.par<-species_list %>% left_join(aphid.par.bio, by = "node_name") %>% 
  unique() %>% filter(taxon == "Leaf-miner parasitoid")  %>% 
  mutate (biomass.g = case_when(node_name == "Bracon sp"~ 0.0025,
                                node_name == "Apanteles circumscriptus" ~ 0.0089,
                                node_name == "Anagrus sp A" ~ 0.0000288,
                                node_name == "Anagrus sp B" ~ 0.0000288,
                                node_name == "Anagrus sp C" ~ 0.0000288,
                                node_name == "Asaphes suspensus"~ 0.00000927,
                                node_name == "Chelonus sp"~ 0.0069,
                                node_name == "Chelonus sp aff rimatus"~ 0.0069,
                                node_name == "Chelonus scabrosus"~ 0.0069,
                                node_name == "Diadegma crataegi"~ 0.000674,
                                TRUE~biomass.g)) %>% # add manually more data extracted from literature
    mutate(biomass.g =if_else(is.na(biomass.g), 
                              mean(biomass.g, na.rm = TRUE), biomass.g)) #do the average for the rest



## -- Fleas (Extracted from literature)
mass.flea<-species_list %>% filter (taxon =="Rodent ectoparasite") %>% 
            mutate(biomass.g = 0.000765) 


## -- Crops (Extracted from literature)
mass.crop<-species_list %>% filter (taxon =="Crop") %>%
  mutate (biomass.g = case_when(node_name == " Barley"~ 200,
                                node_name == " Lucerne" ~ 500,
                                node_name == " Oat spring" ~ 300,
                                node_name == " Oat winter" ~ 300,
                                node_name == " Triticale" ~ 500,
                                node_name == " Wheat"~ 250))#add manually more data


### -- Flower visitors. Repository stored in the packaged "pollimetry".

## Extract biomass of species from general repository stored in the package 'pollimetry'.

# install packages of repository of pollinators
if(!requireNamespace("devtools")) {
  install.packages("devtools")
}
devtools::install_github("liamkendall/pollimetry")

if (!requireNamespace("devtools")) {
 install.packages("devtools")
}
devtools::install_github("liamkendall/pollimetrydata")

library(pollimetry)
library(pollimetrydata)

# rearrange repository
pollimetry_dataset$Weight<-as.numeric(pollimetry_dataset$Weight) #repository of pollinators

poll_list_repo<-pollimetry_dataset  %>%  select(Region,Country,Species,Weight) %>% 
  filter(Region == "Europe" & !(is.na(Weight))) %>% 
  mutate(Species = gsub("_", " ", Species),
         Weight = Weight / 1000) %>% #to gr
  group_by(Species) %>% summarise(biomass.g = mean (Weight)) # calculate average of Weight   


# split the flower visitor dataset into guilds based on weight variations, which allowed us to easily identify repositories
#Separate "flower visitors" group (02FV) into: 1) 02FV: (bees, bumblebees) . 2) 10HO: hover flies, and 3)15FVOTHER: (beetles,etc)

# Check row dataframe to separate the flower visitor guild
nore<-read.csv("Data/raw_data/nore2.csv",header=T)
lower.guild<-substr(nore$lower,1,4)
upper.guild<-substr(nore$upper,1,4)
nore<-cbind(nore,lower.guild,upper.guild)

## -- Bees, bumblebees
bees<-nore %>% filter(upper.guild == "10BE") %>% select(upper,upper.guild) %>% 
  unique() %>% mutate(upper = substring(upper, 7)) #list of bees
bees.list<-species_list %>% filter(node_name%in%bees$upper)# select bees from the whole species list

# check for those species in the repository that match in our dataframe
mass.bees<- bees.list %>% left_join(poll_list_repo, by = c("node_name"="Species")) %>% 
                            mutate(biomass.g =if_else(is.na(biomass.g), 
                            mean(biomass.g, na.rm = TRUE), biomass.g)) # do the average for the rest


## -- Hover flies
hov<-nore %>% filter(upper.guild == "11HO") %>% select(upper,upper.guild) %>% 
  unique() %>% mutate(upper = substring(upper, 7),
                      upper = gsub("\\.", " ", upper)) # list of hover flies
hov.list<-species_list %>% filter(node_name%in%hov$upper)# select hover flies from the whole species list

#check for those species in the repository that match in our dataframe
mass.hov<- hov.list %>% left_join(poll_list_repo, by = c("node_name"="Species")) %>% 
  mutate(biomass.g =if_else(is.na(biomass.g), 
                            mean(biomass.g, na.rm = TRUE), biomass.g)) #do the average for the rest

  
  
## Other flower visitors (beetles, flies, etc.)
ot.flw<-nore %>% filter(upper.guild == "15FV") %>% select(upper,upper.guild) %>% 
unique() %>% mutate(upper = substring(upper, 11)) %>%  # list of other flower visitors
  mutate (upper =  gsub(c("\\?"), "", upper)) %>% 
  mutate (upper =  gsub(c("1"), "", upper)) %>% 
  mutate (upper =  gsub(c("zCROP"), "", upper)) %>% 
  mutate (upper = gsub("\\.", " ", upper))# keep just the species name of most rows

ot.flw.list<-species_list %>% filter(node_name%in%ot.flw$upper) %>%  #select other flower visitors from the whole species list
  filter(taxon == "Flower-visiting")

# upload repository and rearrange dataframe
repo_1<-read_excel("Data/repositories/coleoptera.xlsx", sheet = 3) 

repo_1_clean<-repo_1%>% select(Species, 'Mean mass (mg)') 
repo_1_clean$`Mean mass (mg)`<-as.numeric(repo_1_clean$`Mean mass (mg)`)
repo_1_final<-repo_1_clean %>% 
  mutate(biomass.g = `Mean mass (mg)`/1000) # convert to g

# check the repository for species that match those in our dataframe, and manually add any additional species from the literature
mass.ot.flw1<- ot.flw.list %>% left_join(repo_1_final, by = c("node_name"="Species")) 
mass.ot.flw<- mass.ot.flw1 %>% mutate (biomass.g = case_when(node_name == "Bracon sp"~ 0.0025,
                                                            node_name == "Olibrus aeneus"~ 0.000251,
                                                            node_name == "Agriotes pallidulus"~  0.01966667,
                                                            node_name == "Aprostocetus sp"~  0.00008,
                                                            node_name == "Grammoptera ruficornis"~  0.005143,
                                                            node_name == "Hemicrepidius memnonius"~  0.0327,
                                                            node_name == "Rhagonycha fulva"~ 0.0015275,
                                                            node_name == "Sitona puncticollis"~ 0.00165)) %>% 
                              mutate(biomass.g =if_else(is.na(biomass.g), 
                              mean(biomass.g, na.rm = TRUE), biomass.g)) %>% 
                              select (node_id,node_name,taxon,biomass.g)#do the average for the rest



## -- Butterflies 

# manually add the biomass extracted from litetature
mass.butt<-species_list %>% filter (taxon =="Butterfly") %>% 
  mutate (biomass.g = case_when(node_name == "Comma"~ 0.0413,
                                node_name == "Green-veined White"~ 0.056,
                                node_name == "Large White"~ 0.128,
                                node_name == "Gatekeeper"~ 0.040,
                                node_name == "Common blue"~  0.0118,
                                node_name == "Meadow Brown"~  0.0215,
                                node_name == "Painted Lady"~  0.0725,
                                node_name == "Peacock"~  0.078,
                                node_name == "Red Admiral"~ 0.0725,
                                node_name == "Ringlet"~ 0.0376,
                                node_name == "Small copper"~  0.051,
                                node_name == "Small Skipper"~  0.061,
                                node_name == "Large Skipper"~  0.088,
                                node_name == "Small tortoiseshell"~ 0.136,
                                node_name == "Small white"~ 0.067,
                                node_name == "Speckled wood"~ 0.041)) %>% #add manually
  select (node_id,node_name,taxon,biomass.g)#do the average for the rest




## -- Create final biomass dataset of species -------------------------------------------------------
# there are some NA for some species but they don't provide direct ES.
full_list<-rbind(mass.leaf.min.par,mass.seed.ins,mass.ins.par,mass.2.par,mass.1.par,
                 mass.aphid,mass.hov,mass.bees,mass.flea,mass.birds,mass.crop, mass.mam,mass.ot.flw,mass.butt) %>% 
            select(node_id,biomass.g)

mass.species.list<-species_list %>% left_join(full_list,by="node_id")

#write.csv(mass.species.list,"Data/biomass.csv", row.names= FALSE)




