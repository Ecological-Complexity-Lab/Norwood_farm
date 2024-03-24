####################### ESTIMATE CLEAN ABUNDANCES AND CREATE EDGE LIST AND NODE LIST (separate flower visitors into 
#flower visitors (directly provide pollination) and other flower visitors (do not provide pollination ))


library(bipartite)
library(vegan)
library(sna)
library(tidyverse)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")


##### ESTIMATE ABUNDANCES ############

#Data frame 
plants<-read.csv("Data/PLANT_HABITAT3.csv",header=T, sep =";") # abundance of plants
nore<-read.csv("Data/nore2.csv",header=T) #potential dataframe of abundances


##### Plants

# Rearrange dataframe 
plants_habitat<-plants %>% select(-ST) %>%  # remove standing trees (ST)
  rename ("WD" = "W", "SF" ="CSF") %>% 
  mutate( CP = C + LU) %>% # Merge LU and CP 
  select (-LU,-C)

# Fix abundances of crops in non-crop production habitats
noncrop_hab<- c("SF", "GM","MH","NH","LP","PP","RG","NL")  # Replace values from these habitats
crop_sp <- c(96:101) # rows indicating crops

plants_noncrop<- plants_habitat %>% 
  mutate((across(all_of(noncrop_hab), ~if_else(row_number() %in% crop_sp, 0, .))))

# rearranged dataframe
plants_ab<-plants_noncrop %>% gather("habitat","abundance",2:11) %>% 
  filter(abundance >0) %>% rename("species_name" = "lower")


##### Animals 

# summary tables
guild.names<-sort(unique(substr(nore$lower,1,4)))  #define guild names          # MJOP: added 'sort' to this line
full.guild.names<-c("plants","flower visitors","aphids","aphid parasitoids (primary)","aphid parasitoids (secondary)","leaf miner parasitoids","seed-feeding invertebrates","birds (seed feeders)","mammals (seed feeders)","bees","hoverflies","butterflies","fleas","other flower visitors")
lower.guild<-substr(nore$lower,1,4)
upper.guild<-substr(nore$upper,1,4)
nore<-cbind(nore,lower.guild,upper.guild)


# Remove the arbitrary self loops added for previous robustness calculations
direct.link<-rep(1,dim(nore)[1])
direct.link[lower.guild==upper.guild]<-0; #remove link between same trophic group species 
direct.link[lower.guild=="01PL" & upper.guild=="04PR"]<-0;#remove link between plant and aphid parasitoid
direct.link[lower.guild=="01PL" & upper.guild=="05SE"]<-0; #remove link between plant and second parastoid of aphids 
direct.link[lower.guild=="02FV" & upper.guild=="01PL"]<-0; #remove duplicated links between flower visitors and plants
direct.link[lower.guild=="01PL" & upper.guild=="14FL"]<-0; #Remove link between plants and fleas
direct.link[lower.guild=="01PL" & upper.guild=="13SF"]<-0 #Remove links between plants and seed-feedigin parastioid

nore.direct.only<-nore[direct.link==1,] 


#### Rearrange habitats to match the original paper  (Evans et al. (2013) Ecology Letters

# Remove "ST" habitats
nore_2<-nore.direct.only %>% filter(!(habitat == "ST")) 

## Change names of habitats
nore_names<- nore_2%>% mutate(habitat = case_when(habitat == "P"~ "PP",#typing error
                                                  habitat == "C"~ "CP",
                                                  habitat == "CSF"~ "SF",
                                                  habitat == "RGyard"~"RG", # to merge RG and RGyard into "RG"
                                                  habitat == "W"~ "WD", 
                                                  habitat == "LU"~ "CP", # to merge LU and CP
                                                  habitat == "WU"~ "WD", # to merge W and WU into "WD"
                                                  TRUE~habitat))


## Remove interactions involving crops in non-CP habitats

nore_intcrop_clean<- nore_names %>% filter( (!(habitat == "CP")) & # dataframe of non-CP habitat without crops
                                              !(lower == '01PLANT.zCROP Barley' |
                                                  lower == '01PLANT.zCROP Lucerne' |
                                                  lower == '01PLANT.zCROP Oat spring' |
                                                  lower == '01PLANT.zCROP Oat winter' |
                                                  lower == '01PLANT.zCROP Triticale' |
                                                  lower == '01PLANT.zCROP Wheat' ))

nore_CP<- nore_names %>%  filter(habitat == "CP") # dataframe of CP habitat

nore_ready<- rbind(nore_intcrop_clean,nore_CP) #merge dataframes


### Separate "flower visitors" group (02FV) based on the potential of species as pollinators:

# 1) 02FV (Flower visitors): species with high pollination potential. Includes hoverflies, bees, bumblembees and also species from other guilds (ants, beetles, non-hoverflies diptera, etc) that were recorded transporting high amount of pollen grains  
# 2) 15FVOTHER (Other flower visitors): species with low/null potential as pollinators (mosquitos, beetles).


## Keep just 02FV and 12BF as pollinator trophic groups (02FV already contains the guild 15FV, 11HO and 10BE) 
nore_flowervis<-nore_ready %>%   filter(!(upper.guild == "11HO" |
                                            upper.guild == "10BE")) 

## remove duplicate rows containing butterflies in 02FV
list.butt.flw<- c("02FV.Maniola jurtina","02FV.Pieris brassicae","02FV.Polyommatus icarus",
                  "02FV.Pyronia tithonus") #list of butt classified as flw visitor
nore_flowervis_fin<-nore_flowervis %>%   filter(!(upper %in%list.butt.flw))#remove butterflies



## Filter in 15FVOTHER those species according to its potential role as pollinator (using bibliography)

# Diptera, coleoptera and hymenoptera (bees and bumbebless) species

#We assume that species will be classified as 02FV if they were recorded transporting at least one grain of pollen in the data base, otherwise they will be classified as 15FV.

list_to_compare<- read.csv("Data/Biomass_repositories/Orford_et_al_2015.csv",header=T, sep =",") %>%  #data containing list of visitors transporting pollen grain from data from Orford et al 2015 (https://doi.org/10.1098/rspb.2014.2934)
                  select(-Plant.species) %>% group_by(Insect.species) %>% 
                  summarise (Tot_pol_grain= sum(Pollen.grains)) %>% #calculate the total grain of pollen per sp
                  mutate(Insect.species = paste0("15FVOTHER.", Insect.species)) %>%  # arrange the format before filtering
                  rename ( "upper "= "Insect.species")

coleop_dipt<-nore_flowervis_fin %>% filter(upper.guild == "15FV" & upper%in%list_to_compare$`upper `) %>% 
      ungroup() %>% select(upper) %>% unique() # species (coleoptera, diptera and hymenoptera) that we keep in the 02FV


# Do something similar with hymenptera parasitoids.
#PONER LA LISTA ENORME



#Then remove those species from 15FV






FV<- nore_names %>% filter(upper.guild=="02FV") %>% select(upper,upper.guild) %>% 
  unique() #list of all flower visitors 
FV.sp<-substr(FV$upper,6,nchar(FV$upper)) #substract species name
FV.list<-cbind(FV,FV.sp) 

FVOTHER<- nore_names %>% filter(upper.guild=="15FV") %>% select(upper,upper.guild) %>% 
  unique() #list of flower visitor others
FVOTHER.sp<-substr(FVOTHER$upper,11,nchar(FVOTHER$upper)) #substract species name
FVOTHER.list<-cbind(FVOTHER,FVOTHER.sp) 

# Eliminate flower visitor others from the flower visitor list (02FV)
FV_justpoll<-FV.list %>% filter(!(FV.sp %in% FVOTHER.list$FVOTHER.sp))

# Eliminate duplicated flower visitors in 02FV from the dataframe
nore_fvjust_poll<-nore_names %>% filter(upper %in% FV_justpoll$upper)
nore_flowervis<-nore_names %>% filter(upper.guild != "02FV")

nore_flowervis<-rbind(nore_flowervis,nore_fvjust_poll) #remove "15FVOTHERS" pollinators from "02FV"pollinators
nore_fwvisitor<- nore_flowervis %>% filter (!(upper.guild== "11HO" | 
                                                upper.guild == "10BE")) #remove hoverflies and bees because they are already in 02FV

## OTHERWISE: Keep just 02FV and 12BF as pollinator trophic groups  

#nore_fwvisitor<-nore_names %>%   filter(!(upper.guild == "15FV" |
                       #                              upper.guild == "11HO" |
                        #                             upper.guild == "10BE"))



## Rearrange bird abundances according to the original paper

birds_WD_RG<-nore_flowervis %>% filter(upper.guild == "08BI", habitat =="RG" |
                                         habitat == "WD")  #abundances birds in WD and RG

birds_rest<-nore_flowervis %>% filter(upper.guild == "08BI", habitat =="all") #Birds move widely over the landscape, and the habitats in which they were mostly observed (e.g. hedgerows) were often not the habitats in which they were feeding, so we pooled them together as abundance of all habitats (RG and WD as exception)

#abundances of birds in the rest of the habitats
birds_CP<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "CP"))
birds_SF<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "SF"))
birds_GM<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "GM"))
birds_LP<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "LP"))
birds_LU<- birds_rest %>% mutate(habitat = ifelse(habitat =="all", "LU"))
birds_MH<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "MH"))
birds_NH<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "NH"))
birds_NL<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "NL"))
birds_PP<-birds_rest %>% mutate(habitat = ifelse(habitat =="all", "PP"))

# Merge to create dataframe of birds abundances
birds_abundances<-rbind(birds_WD_RG,birds_CP,birds_SF,birds_GM,birds_LP,birds_LU,
                        birds_MH,birds_NH,birds_NL,birds_PP)
  

## Final dataframe to calculate abundances of animals
nore_without_bird<- nore_fwvisitor %>% filter (!(upper.guild== "08BI")) #remove old data of birds
nore_to_abundances<-rbind (nore_without_bird,birds_abundances)


## Calculate the abundances of animals 
animals_ab<-nore_to_abundances %>%  group_by(upper,habitat, upper.guild) %>% 
  summarize(abundance =sum(fortotals)) %>% select(-upper.guild) %>% 
  rename("species_name" = "upper")


### Final dataframe of species abundances per habitat

species_abundances<-rbind(plants_ab,animals_ab)

#write.csv(species_abundances,"Data/species_abundances_anta.csv", row.names= FALSE)


########## CREATE EDGELIST

edge_list_nore_pre<-nore_to_abundances %>% select(-fortotals,-round,-lower.guild) %>% 
  unique()
  
## Filtering bird-plant interactions in each habitat according to the list of plants 
elist_birds<-edge_list_nore_pre %>% filter(upper.guild== "08BI")

## Split out into individual bird and plant  lists
plant_list<-split(plants_ab,plants_ab$habitat)#list of plants in each habitat
elistbirds_split <- split(elist_birds, elist_birds$habitat) # Split out the bird edgelist into different habitats 

## Keep interactions when the plant is in the specific habitat
edgelist_birds<- NULL
for (i in 1:length(elistbirds_split)){ 
  edgelist_birds[[i]] <- elistbirds_split[[i]] %>% 
    filter(lower %in% plant_list[[i]]$species_name)
}

edgelist_birds_long <- bind_rows(edgelist_birds, .id = "habitat") %>% #convert to edgelist again
  mutate(habitat= case_when(
    habitat== 1 ~ "CP", habitat== 2 ~ "GM", habitat== 3 ~ "LP" , habitat== 4 ~ "LU", habitat== 5 ~ "MH", habitat== 6 ~ "NH",
    habitat== 7 ~ "NL", habitat== 8 ~ "PP", habitat== 9 ~ "RG" , habitat== 10 ~ "SF", 
    habitat== 11 ~ "WD")) 

## Merge real bird-plant interaction per habitat to the previous dataframe
edge_list_nore_withoutbird<- edge_list_nore_pre %>% filter(upper.guild != "08BI") 
edge_list_nore_final<-rbind(edge_list_nore_withoutbird,edgelist_birds_long) %>% select(habitat,lower,upper) 

#write.csv(edge_list_nore_final,"Data/elist_nore_anta.csv", row.names= FALSE)



####### CREATE NODELIST
guild.names<-sort(unique(substr(nore$lower,1,4)))  #define guild names
full.guild.names<-c("plants","flower visitors","aphids","aphid parasitoids (primary)","aphid parasitoids (secondary)","leaf miner parasitoids","seed-feeding invertebrates","birds (seed feeders)","mammals (seed feeders)","bees","hoverflies","butterflies","fleas","other flower visitors")
lower.species<-edge_list_nore_final$lower%>% as.data.frame()
upper.species<-edge_list_nore_final$upper %>% as.data.frame()

nodes_1<-rbind(lower.species,upper.species) %>% rename("node_name"=".") %>% unique() %>% 
  arrange(node_name) %>% cbind(node_id = 1:nrow(nodes_1)) 

#trophic groups
plants = 1:93
crops = 94:99
flw_vis = 100:153
aphid = 154:181
pri_par = 182:192
sec_par = 193:199
leaf_par = 200:292
seed_ins = 293:311
seed_bird = 312:323
seed_rod = 324:327
butt = 328:343
seed_ins_par = 344:360
rod_par = 361:368
flw_vis_oth = 369:555

nodes_anta<-nodes_1 %>% mutate(taxon = case_when(
                          node_id %in% plants  ~ "Plant",
                          node_id %in% crops  ~ "Crop",
                          node_id %in% flw_vis  ~ "Flower-visiting",
                          node_id %in% aphid  ~ "Aphid",
                          node_id %in% pri_par  ~ "Primary aphid parasitoid",
                          node_id %in% sec_par  ~ "Secondary aphid parasitoid",
                          node_id %in% leaf_par  ~ "Leaf-miner parasitoid",
                          node_id %in% seed_ins  ~ "Seed-feeding insect",
                          node_id %in% seed_bird  ~ "Seed-feeding bird",
                          node_id %in% seed_rod  ~ "Seed-feeding rodent",
                          node_id %in% butt  ~ "Butterfly",
                          node_id %in% seed_ins_par  ~ "Insect seed-feeder parasitoid",
                          node_id %in% rod_par  ~ "Rodent ectoparasite",
                          node_id %in% flw_vis_oth  ~ "Flower-visiting others"
))

#write.csv(nodes_anta,"Data/nodes_anta.csv", row.names= FALSE)
