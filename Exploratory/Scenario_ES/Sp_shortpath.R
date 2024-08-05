############################### We calculated the importance of species on indirecting affecting E(D)S


#To do this, we use shortestpath which allow as to see how far each node is from other nodes across all management scenarios.
#Then we assign the direct provision of E(D)S to nodes, so we can see how far each species is from those species directly providing ES.


#load packages
library(igraph)
library(dplyr)
library(tidyverse)
library(emln)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio") #set directory


#### 1. Arrange dataframe

# List of noes with attributes
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object
nodes<- Norwood_farm$nodes %>% #list of nodes with attributes
        gather(services,value,4:10) %>% select(-ES,-DES)  %>% 
        filter(value>0)

## Upload edge list of each management scenario according to the land use change (CP: extensive to crop production; PP: extensive to permanent pasture)

#Select the type of land use change

Intensive = "CP"
#Intensive = "PP"

if (Intensive=="CP"){
  edge_list<- read.csv("Data/Land_use_rat_edgelist_weighted_CP_intense.csv", sep = ",") %>% 
    select(-weight)
}

if (Intensive=="PP"){
  edge_list<- read.csv("Data/Land_use_rat_edgelist_weighted_PP_intense.csv", sep = ",") %>% 
    select(-weight)
  }



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


## Estimate shortest path between  species in each network 

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


## Save file according to the land use change selected at the begging of the code

#write.csv(species_shortpath_fin, "Data/Land_use_shortpath_weighted_CP_intense.csv")
#write.csv(species_shortpath_fin, "Data/Land_use_shortpath_weighted_PP_intense.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Analyses                            
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#upload and arrange dataframe
short_path_CP<-read.csv("Data/Land_use_shortpath_weighted_CP_intense.csv", row.names = 1) %>% mutate(land_use = "CP")
#short_path_PP<-read.csv("Data/Land_use_shortpath_weighted_PP_intense.csv", row.names = 1)%>% mutate(land_use = "PP")
short_path_land_change<-short_path_CP
#short_path_land_change<-rbind(short_path_CP,short_path_PP)


## check exploratory tendency
exploratory_taxon<-short_path_land_change %>% group_by(land_use,taxon) %>% 
            summarise(mean_short = mean(short_ave),
                      se_short = sd(short_ave)/ sqrt(n())) %>% 
            arrange(mean_short)

exploratory_management<-short_path_land_change %>% group_by(land_use,management) %>% 
  summarise(mean_short = mean(short_ave),
            se_short = sd(short_ave)/ sqrt(n()))


########### Test if land use change affect the average indirect important of species

#calculate average short path of each species to all E(D)S in each habitat management
short_path_land_change_ave<- short_path_land_change %>% group_by(land_use, management,node_id) %>% 
                            mutate(short_path_ave = mean(short_ave)) %>% select(-services, - short_ave) %>% unique()

## From Extensive to intensive CP
short_path_land_change_CP<- short_path_land_change_ave %>% filter(land_use== "CP") 

prueba_type <- short_path_land_change %>% filter(land_use== "CP") 
# Model
library(glmmTMB)
library(emmeans)
library(car)

short_CP<- glmmTMB(short_path_ave~management + taxon, 
                  family = Gamma(link = "log"), data = short_path_land_change_CP) #we already check and this is the best model
#short_CP_2<- glmmTMB(short_path_ave~management + taxon + 1|node_id , 
           #        family = Gamma(link = "log"), data = short_path_land_change_CP) #we already check and this is the best model
#short_CP_3<- glmmTMB(short_path_ave~management + taxon + node_id, 
            #       family = Gamma(link = "log"), data = short_path_land_change_CP) #we already check and this is the best model


Anova(short_CP)
model_summary<-summary(short_CP)

#AIC(short_CP,short_CP_2,short_CP_3) #the first one is the best model, we discard id because it's not imporant and the model is too complex to put it as random


# Extract the coefficients (for GLMM It's the average including all the levels)
coefficients <- model_summary$coefficients$cond

# Extract coefficients for each factor
management_coefs <- coefficients[grep("management", rownames(coefficients)), "Estimate"]
taxon_coefs <- coefficients[grep("taxon", rownames(coefficients)), "Estimate"]

# Calculate summary statistics (e.g., mean) for each factor
management_summary <- mean(management_coefs)
taxon_summary <- mean(taxon_coefs)

#Homogeneity
EM<-resid(short_CP, type= "response") 
FM<-fitted(short_CP) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(short_CP, type= "response") 
boxplot(E1_lme~short_path_land_change_CP$management, main="Management")

# posthoc
post_amount<- emmeans(short_CP, ~ management)#management
pairs(post_amount)

post_amount_taxon<- emmeans(short_CP, ~ taxon)#management
pairs(post_amount_taxon)


##stats
#average per management
ave_management<-short_path_land_change_CP %>% group_by(management) %>% 
                summarise(ave_short = mean(short_path_ave),
                          se_short = sd(short_path_ave) / sqrt(n()))

#average per taxon
ave_taxon<-short_path_land_change_CP %>% group_by(taxon) %>% 
  summarise(ave_short = mean(short_path_ave),
            se_short = sd(short_path_ave) / sqrt(n())) %>% 
            arrange(ave_short)

#average per management and taxon
ave_management_taxon<-short_path_land_change_CP %>% group_by(management,taxon) %>% 
  summarise(ave_short = mean(short_path_ave),
            se_short = sd(short_path_ave) / sqrt(n()))

ave_management_taxon_I_IM <-ave_management_taxon %>% filter(management =="I" |
                                                              management =="IM" )

#Rta: The indirect role of species on affecting ES is, in general, reduced by land conversion. Species tends to be
#more distant of species that provide directly an ES after land conversion (mainly in IM). Also, the indirect role
#of species to affect ES provision changed according to the taxon. Birds, rodents, and birds were more closer to
#species that provides directly an ES.


### Plot mean of shortest path of each trophoc group across land conversion

#Preparing dataframe
#Vector of colors
color_trophic <-tibble(taxon = c("Plant","Crop","Flower-visiting","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                 "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                 "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                       color = c("#33a02c","#b15928","#a6cee3","#1f78b4","#b2df8a","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6",
                                 "#6a3d9a", "#cccc7a", "#e7298a"))

ave_management_taxon$management <- factor(ave_management_taxon$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

ave_management_taxon$taxon<-as.factor(ave_management_taxon$taxon)

#Plot

#
ggplot(ave_management_taxon, aes(x = management, y = ave_short, group =taxon, color= taxon)) +
  scale_color_manual(values = color_trophic$color[match(levels(ave_management_taxon$taxon), color_trophic$taxon)])+
  geom_point() +
  geom_line(size = 1.4)  +
  geom_errorbar(aes(ymin = ave_short - se_short, ymax = ave_short + se_short), width = 0.2) +
  labs(x = "Land conversion",
       y = "Shorthest path ",
       color = "Trophic Group") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=13, color='black'),
        # axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.position = "bottom") 

#grid
pdf("Graphs/short_path_taxon_landconv.pdf", width = 12, height = 7)
ggplot(ave_management_taxon, aes(x = management, y = ave_short, group =taxon, color= taxon)) +
scale_color_manual(values = color_trophic$color[match(levels(ave_management_taxon$taxon), color_trophic$taxon)])+
  geom_point() +
  geom_line()  +
  geom_ribbon(aes(ymin = ave_short - se_short, ymax = ave_short + se_short, fill = taxon), alpha = 0.2) +
  labs(x = "Land conversion",
       y = "Shorthest path ",
       color = "Trophic Group") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=13, color='black'),
        # axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(size =15), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.position = "none") +
  facet_wrap(~taxon, scales = "fixed")
#dev.off()
#ggsave("Graphs/short_path_taxon_landconv.pdf", width = 12, height = 7, dpi = 300)




########## Plot change in the indirect role of the 5 most important species per trophic group (Extensive) across
#land conversion

#### Check the 5 most important species per trophic group in Extensive
top_5_taxon_extensive<-short_path_land_change_CP %>%
  filter(management == "E") %>% group_by(management, taxon) %>% 
  arrange(short_path_ave) %>% # Arrange by short_path_ave within each group
  slice_head(n = 5) # Take the first 5 rows within each group


#Filter the importance of the top 5 species (from the extensive) across management scenarios
top_5_average<- short_path_land_change_CP %>%  filter(node_id%in%top_5_taxon_extensive$node_id)


# Model
top_5<- glmmTMB(short_path_ave~management+ taxon+(1|node_id), 
                    family = Gamma(link = "log"), data = top_5_average) #we already check and this is the best model
Anova(top_5)
model_summary<-summary(top_5) #this model fits the best (drop taxon no significant difference)


# Extract the coefficients (for GLMM It's the average including all the levels)
coefficients <- model_summary$coefficients$cond

# Extract coefficients for each factor
management_coefs <- coefficients[grep("management", rownames(coefficients)), "Estimate"]
taxon_coefs <- coefficients[grep("taxon", rownames(coefficients)), "Estimate"]

# Calculate summary statistics (e.g., mean) for each factor
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

# posthoc
post_short<- emmeans(top_5, ~ management)
pairs(post_short)
post_taxon<- emmeans(top_5, ~ taxon)
pairs(post_taxon)

#average per management
ave_management<-top_5_average %>% group_by(management) %>% 
  summarise(ave_short = mean(short_path_ave),
            se_short = sd(short_path_ave) / sqrt(n()))

#average per taxon
ave_taxon<-top_5_average %>% group_by(taxon) %>% 
  summarise(ave_short = mean(short_path_ave),
            se_short = sd(short_path_ave) / sqrt(n())) %>% 
  arrange(ave_short)

#average per management and taxon
ave_management_taxon<-top_5_average %>% group_by(management,taxon) %>% 
  summarise(ave_short = mean(short_path_ave),
            se_short = sd(short_path_ave) / sqrt(n()))

#Rta: The indirect role of the most important species tend to change across land conversion but it varied also according
#to trophic guild. For most important species, their indirect provison of ES tend to decreased as in the general pattern. EVen
#the most important species went extinct (plants, ectoparasites, etc.)

#In the circular plot you can see that core of flowering visitors,plants, ectoparasites,aphid parasitoid went extinct.
#While the rest of core species reduced thir indirect improtance. For some trophic groups, such as 1 and 2 aphid parasitodis,
#tend to be resilience to the change.


#### Circular plot


#TO DO: TRY TO ADD A LAYER WITH COLORS OR DO IT MANUALLY. Separate ring

library(circlize)
library(viridis)
library(ComplexHeatmap)

##### set up parameters and structure

# Define color of each layer and sps
top_5_average$management <- factor(top_5_average$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

top_5_ave <- top_5_average %>% ungroup() %>% 
              select(node_id,taxon,management,short_path_ave) %>%
              spread(management,short_path_ave) %>%  #rearrange dataframe
               ungroup() 
top_5_ave<-top_5_ave[,c(1,2,3,7,6,8,4,5)]
  

# layer
color = colorRamp2(seq(max(top_5_ave[,3:8], na.rm = TRUE), min(top_5_ave[,3:8], na.rm = TRUE),
                       length =5),viridis(5))#color layer


# Arrange short path order and prepare the final version of species list

sp_names <- top_5_ave$node_id #create temporal species name to filter the big database
sp_names<-as.factor(sp_names) # to plot species name

top_5_ave_values<- as.data.frame(top_5_ave) %>% select(-node_id,-taxon)
rownames(top_5_ave_values) <- sp_names 
top_5_ave_values<-top_5_ave_values[,c(1,5,6,3,2,4)]

color_trophic <-tibble(taxon = c("Plant","Crop","Flower-visiting","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                 "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                 "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                       color = c("#33a02c","#b15928","#a6cee3","#1f78b4","#b2df8a","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6",
                                 "#6a3d9a", "#cccc7a", "#e7298a")) #color trophic group

species_list_color <- top_5_ave %>% select(node_id,taxon) %>%  #list of species and color according to the taxon
                      left_join(color_trophic, by = "taxon") 

#species_list_color_ordered<- species_list_color[match(sp_names, species_list_color$node_id), ] #order to match the database



#TRY TO SPLIT (SEPARATE TE FIRST AND THE LAST ONE A BIT MORE)



#Plotting

circos.clear()

png(file="Graphs/short_path_top_5.png",
    width=1600, height=1200)


# Extensive
E<- top_5_ave_values[,1,drop=FALSE]


circos.par(start.degree = 10, gap.degree = 1)

circos.heatmap(E, col = color, #rownames.side = "outside", rownames.col= species_list_color$color,
               rownames.cex = 0.7, track.height = 0.11, cell.border = "black",cluster = FALSE,
               split = factor(species_list_color$taxon, levels = unique(species_list_color$taxon)))



#name2<-rownames(E)
#color_sp

# Semi Extensive
SE<- top_5_ave_values[,2, drop= FALSE]


circos.heatmap(SE, col = color, track.height = 0.11, cell.border = "black")


# Moderate
M<- top_5_ave_values[,3, drop= FALSE]

circos.heatmap(M, col = color,  track.height = 0.11, cell.border = "black")

# Semi-Intensive
SI<- top_5_ave_values[,4, drop= FALSE]

circos.heatmap(SI, col = color, track.height = 0.11, cell.border = "black")

# Intensive
I<- top_5_ave_values[,5, drop= FALSE]

circos.heatmap(I, col = color, track.height = 0.11, cell.border = "black")

# Intensive non-organic
IM<- top_5_ave_values[,6, drop= FALSE]

circos.heatmap(IM, col = color, track.height = 0.11, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 12),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(2,"cm"),  grid_width = unit(4.5,"cm"),title_gp = gpar(fontsize = 13, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(10, "mm"), 
     just = c("bottom"))

#legend("bottomleft", inset=.02, title="Guild", c (unique(species_list_color$taxon)) , fill= unique(species_list_color$color), horiz=FALSE, cex=1)

dev.off()




##Caption with management scenario information

#create dummy dataframe
ref <- data.frame(
  E = 1,
  SE = 1,
  M = 1,
  SI = 1,
  I = 1,
  IM = 1
)

#Plotting


circos.clear()

png(file="Graphs/short_path_top_5.png",
    width=1600, height=1200)


# Extensive
E<- ref[,1,drop=FALSE]
circos.par(gap.degree = 18)

circos.heatmap(E, col = col_ref, 
               rownames.cex = 0.7, track.height = 0.11, cell.border = "black",cluster = FALSE)
#name2<-rownames(E)
#color_sp

# Semi Extensive
SE<- ref[,2, drop= FALSE]
circos.heatmap(SE, col = "azure3", track.height = 0.11, cell.border = "black")

# Moderate
M<- ref[,3, drop= FALSE]
circos.heatmap(M, col = "azure3",  track.height = 0.11, cell.border = "black")

# Semi-Intensive
SI<- ref[,4, drop= FALSE]
circos.heatmap(SI, col = "azure3", track.height = 0.11, cell.border = "black")

# Intensive
I<- ref[,5, drop= FALSE]
circos.heatmap(I, col = "azure3", track.height = 0.11, cell.border = "black")

# Intensive non-organic
IM<- ref[,6, drop= FALSE]
circos.heatmap(IM, col = "azure3", track.height = 0.11, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 12),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(2,"cm"),  grid_width = unit(4.5,"cm"),title_gp = gpar(fontsize = 13, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(10, "mm"), 
     just = c("bottom"))

#legend("bottomleft", inset=.02, title="Guild", c (unique(species_list_color$taxon)) , fill= unique(species_list_color$color), horiz=FALSE, cex=1)

dev.off()






#PREVIOUS VERSION (NO SEPARATION BETWEEN TROPHIC GROUPS)

#Plotting

circos.clear()

png(file="Graphs/short_path_top_5.png",
    width=1600, height=1200)


# Extensive
E<- top_5_ave_values[,1,drop=FALSE]

circos.par(gap.degree = 18)


circos.heatmap(E, col = color,rownames.side = "outside", rownames.col= species_list_color$color,
               rownames.cex = 0.7, track.height = 0.11, cell.border = "black", cluster = FALSE)


#name2<-rownames(E)
#color_sp

# Semi Extensive
SE<- top_5_ave_values[,2, drop= FALSE]


circos.heatmap(SE, col = color, track.height = 0.11, cell.border = "black")


# Moderate
M<- top_5_ave_values[,3, drop= FALSE]

circos.heatmap(M, col = color,  track.height = 0.11, cell.border = "black")

# Semi-Intensive
SI<- top_5_ave_values[,4, drop= FALSE]

circos.heatmap(SI, col = color, track.height = 0.11, cell.border = "black")

# Intensive
I<- top_5_ave_values[,5, drop= FALSE]

circos.heatmap(I, col = color, track.height = 0.11, cell.border = "black")

# Intensive non-organic
IM<- top_5_ave_values[,6, drop= FALSE]

circos.heatmap(IM, col = color, track.height = 0.11, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 8),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(1.4,"cm"),  grid_width = unit(3,"cm"),title_gp = gpar(fontsize = 9, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(10, "mm"), 
     just = c("bottom"))

#legend("bottomleft", inset=.02, title="Guild", c (unique(species_list_color$taxon)) , fill= unique(species_list_color$color), horiz=FALSE, cex=1)

dev.off()















########### Test if land use change produce a replacement of the top 25 most important species in each treatment
# Check if the 25 species persist across land use change

## From Extensive to intensive CP

# Step 1: Get Top 25 species in 'extensive' management
top_25_extensive <- short_path_land_change_CP %>%
  filter(management == "E") %>% group_by(management) %>% 
  slice_min(order_by = short_path_ave, n = 25) %>% 
  select(land_use,management,node_id,taxon) %>% 
  mutate(presence = 1)

# Step 2: Get Top 25 species in other managements
top_25_other_managements <- short_path_land_change_CP %>%
  filter(management != "E") %>%
  group_by(management) %>%
  slice_min(order_by = short_path_ave, n = 25) 


# Step 3: Check for persistence
change_top_25_CP<- unique(top_25_other_managements$management) %>%
  expand.grid(management = ., node_id = top_25_extensive$node_id) %>%
  left_join(top_25_other_managements, by = c("management", "node_id" )) %>%
  mutate(presence = ifelse(is.na(short_path_ave), 0, 1)) %>% #assign 1 when the species is still in the top 25
  select(management, node_id = node_id, presence) %>% 
  left_join(top_25_extensive, by = "node_id") %>% #add taxon information
  select(land_use,management.x,node_id,taxon,presence.x) %>% rename("management" = "management.x",
                                                                    "presence" = "presence.x")

# Merge dataset
change_top_25_CP_fin<- rbind(top_25_extensive,change_top_25_CP)

# Model
library(lme4)
library("stats4")
library("bbmle")
library(emmeans)

pers_1<-glmer (presence ~ management + ( 1| node_id), family = binomial(link="logit"), data = change_top_25_CP_fin,
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000)))

Anova(pers_1)
summary(pers_1)

#Homogeneity
EM<-resid(pers_1, type= "deviance") 
FM<-fitted(pers_1) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(pers_1, type= "deviance") 
boxplot(E1_lme~change_top_25_CP_fin$management, main="Tratamiento")




########### Test if land use change in the importance of the top 25 most important species across land use change
# Check if the indirect importance (average short path) of the top 25 species change in each treatment

#Filter the improtance of the top25 species (from the extensive) across treatment
top_25_average<- short_path_land_change_ave %>%  filter(node_id%in%top_25_extensive$node_id)

## From Extensive to intensive CP
top_25_average_CP<- top_25_average %>% filter(land_use== "CP") 

# exploratory
average_change_CP<- top_25_average_CP %>% group_by(management) %>%  summarise(average = mean(short_path_ave))


# Model
top_25_CP<- glmmTMB(short_path_ave~management+(1|node_id), 
                   family = Gamma(link = "log"), data = top_25_average_CP) #we already check and this is the best model
Anova(top_25_CP)
summary(top_25_CP) #this model fits the best (drop taxon no significant difference)


#Homogeneity
EM<-resid(top_25_CP) 
FM<-fitted(top_25_CP) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(top_25_CP) 
boxplot(E1_lme~top_25_average_CP$management, main="Tratamiento")

# posthoc
post_short<- emmeans(top_25_CP, ~ management)
pairs(post_short)


## From Extensive to intensive PP
top_25_average_PP<- top_25_average %>% filter(land_use== "PP") 

#exploratory 
average_change_PP<- top_25_average_PP %>% group_by(management) %>%  summarise(average = mean(short_path_ave))

# Model
top_25_PP<- glmmTMB(short_path_ave~management +(1|node_id), 
                    family = Gamma(link = "log"), data = top_25_average_PP) #we already check and this is the best model
Anova(top_25_PP)
summary(top_25_PP) #this model fits the best (drop it because was not significant)

#Homogeneity
EM<-resid(top_25_PP) 
FM<-fitted(top_25_PP) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(top_25_PP) 
boxplot(E1_lme~top_25_average_PP$management, main="Tratamiento")

# posthoc
post_short<- emmeans(top_25_PP, ~ management)
pairs(post_short)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Plots                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(circlize)
library(viridis)
library(ComplexHeatmap)


#### FROM EXTENSIVE TO INTENSIE CROP PRODUCTION

## Arrange the matrix to fix the names of species
short_path<- read.csv("Data/Land_use_shortpath_weighted_CP_intense.csv", sep =",", row.names = 1) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) %>% 
  mutate(color_sp =case_when(taxon == "Plant"~ "#00C1AB", #assign color to species according to the taxon
                             taxon == "Crop"~ "#BE9C00",
                             taxon == "Flower-visiting"~ "#8CAB00",
                             taxon == "Aphid"~ "#F8766D",
                             taxon == "Primary aphid parasitoid"~ "#00BBDA",
                             taxon == "Secondary aphid parasitoid"~ "#8B93FF",
                             taxon == "Leaf-miner parasitoid"~ "#00BE70",
                             taxon == "Seed-feeding insect"~ "#F962DD",
                             taxon == "Seed-feeding bird"~ "#D575FE",
                             taxon == "Seed-feeding rodent"~ "#FF65AC",
                             taxon == "Butterfly"~ "#E18A00",
                             taxon == "Insect seed-feeder parasitoid"~ "#24B700",
                             taxon == "Rodent ectoparasite"~ "#00ACFC") ) %>% 
  arrange(taxon) #order according to taxon

short_path$taxon<-factor(short_path$taxon, levels = c("Plant","Leaf-miner parasitoid",
                                                      "Seed-feeding bird","Seed-feeding rodent" ))


########## Plot 1: Extensive plot


#Create a multi donut plot for the extensive scenario. Each ring represents a particular ES.
short_path_E <- short_path %>% filter(management == "E") 

#### Considering the 25 most important species

# Select the most important species to indirectly affect ES 
species_imp<- short_path_E %>% 
  group_by(node_id) %>% 
  summarise(short_aver_ES = mean(short_ave)) %>% 
  arrange(short_aver_ES)

top_25<-species_imp[1:25,1] #identify the list of species that make up the top 25%

top_25_sp<-short_path_E %>% filter(node_id%in%top_25$node_id) %>%  #Data frame containing the 25% most important species
  mutate(services_code = case_when(
    services == "Crop production"~ 1,
    services == "Pollination"~ 2,
    services == "Butterfly watching"~ 3,
    services == "Seed dispersal"~ 4,
    services == "Bird watching"~ 5,
    services == "Pest control"~ 6,
    services == "Crop damage"~ 7
    
  )) %>% arrange(taxon)



short_top_25<-top_25_sp %>% #just average for now (maybe add error bars in the future)
  select(node_name,services,short_ave) %>% 
  spread(services,short_ave)  #rearrange dataframe


short_top_25_ordered <- short_top_25[match(unique(top_25_sp$node_name), short_top_25$node_name), ] #order according to axon

##### set up parameters and structure

# Define color of each layer and sps

#layer
color = colorRamp2(seq(max(short_top_25[,2:8]), min(short_top_25[,2:8]), length = 50),viridis(50)) #color layer


#sps
species_list<- read.csv("Data/Land_use_shortpath_weighted_CP_intense.csv", sep =",", row.names = 1) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) %>% 
  filter(node_name%in%sp_names) %>% #filter species in the dataframe
  mutate(color_sp =case_when(taxon == "Plant"~ "#00C1AB", #assign color to species according to the taxon
                             taxon == "Crop"~ "#BE9C00",
                             taxon == "Flower-visiting"~ "#8CAB00",
                             taxon == "Aphid"~ "#F8766D",
                             taxon == "Primary aphid parasitoid"~ "#00BBDA",
                             taxon == "Secondary aphid parasitoid"~ "#8B93FF",
                             taxon == "Leaf-miner parasitoid"~ "#00BE70",
                             taxon == "Seed-feeding insect"~ "#F962DD",
                             taxon == "Seed-feeding bird"~ "#D575FE",
                             taxon == "Seed-feeding rodent"~ "#FF65AC",
                             taxon == "Butterfly"~ "#E18A00",
                             taxon == "Insect seed-feeder parasitoid"~ "#24B700",
                             taxon == "Rodent ectoparasite"~ "#00ACFC") ) %>% 
  ungroup() %>% select(node_name,color_sp) %>% unique()



  

# Arrange short path order and prepare the final version of species list

sp_names<-short_top_25_ordered$node_name#vector with nodes ID
sp_names<-as.factor(sp_names) # to plot species name

top_25_values<- short_top_25_ordered %>% select(-node_name)
rownames(top_25_values) <- sp_names

species_list_ordered <- species_list[match(sp_names, species_list$node_name), ]

color_sp<-species_list_ordered$color_sp

#Plotting

circos.clear()

png(file="Short_top25_Ext.png",
   width=1600, height=1200)

# Crop production
CP<- top_25_values[,4, drop= FALSE]

circos.par(gap.degree = 20)


circos.heatmap(CP, col = color, rownames.side = "outside", rownames.col	=color_sp,
               rownames.cex = 1.06, track.height = 0.05, cell.border = "black", cluster = FALSE)

# Pollination
PO<- top_25_values[,6,drop= FALSE]

circos.heatmap(PO, col = color, track.height = 0.05,  cell.border = "black")

# Seed dispersal
SD<- top_25_values[,7,drop= FALSE]

circos.heatmap(SD, col = color, track.height = 0.05,  cell.border = "black")

# Pest control
PC<- top_25_values[,5,drop= FALSE]

circos.heatmap(PC, col = color, track.height = 0.05,  cell.border = "black")

# Bird watching
BW<- top_25_values[,1,drop= FALSE]

circos.heatmap(BW, col = color, track.height = 0.05,  cell.border = "black")

# Butterfly watching
BTW<- top_25_values[,2,drop= FALSE]

circos.heatmap(BTW, col = color, track.height = 0.05,  cell.border = "black")

# Crop damage
CD<- top_25_values[,3,drop= FALSE]

circos.heatmap(CD, col = color, track.height = 0.05,  cell.border = "black")

#Legend
lgd_mult = Legend(col_fun =color,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 26),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(1.8,"cm"),  grid_width = unit(1,"cm"),title_gp = gpar(fontsize = 28, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(90, "mm"), 
     just = c("left", "bottom"))

legend("bottomleft", inset=.02, title="Guild", c ("Plant", "Leaf-miner parasitoid","Seed-feeding bird","Seed-feeding rodent")
       , fill= unique(color_sp), horiz=FALSE, cex=2)

dev.off()



## Using ggplot

# Step 1: Identify the External Ring Data Points
#external_ring <- top_25_sp %>%
 # group_by(node_name) %>%
 # filter(services_code == 7) %>%
#  ungroup()

# Step 2: Create the plot and add labels
#ggplot(top_25_sp, aes(x = node_name, y = services_code, fill = short_ave)) +
#  geom_tile(width = 1, height = 0.85, color = "black") + 
#  scale_fill_gradient(low = "#ef3b2c", high = "#fff5f0") +
#  coord_polar() +
#  geom_label(data = external_ring, aes(label = node_name),show.legend = FALSE
#  )+
##  ylim(c(2, NA)) +
#  theme_minimal() +
#  theme(axis.text = element_blank(),
#        axis.title = element_blank(),
#        panel.grid = element_blank(),
  #      axis.ticks = element_blank())






########### PLOT 2: Each ring represents a particular habitat management and values
#indicate the average importance of the top 25 species  in extesive to indirectly affect ES

#This graph shows if the most important species changes with habitat management


##### set up parameters and structure

# Define color of each layer and sps

# species
short_path_aver_25<- short_path %>% filter (node_name %in%short_top_25$node_name) %>% 
                  group_by(management,node_id) %>% 
                  mutate(short_aver_ES = mean(short_ave)) %>% 
                  select(-services,-short_ave) %>% unique()
  
short_path_aver_25$management <- factor(short_path_aver_25$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

short_top_aver_25<-short_path_aver_25 %>% 
  select(node_name,management,short_aver_ES) %>% 
  spread(management,short_aver_ES) %>%  #rearrange dataframe
ungroup() %>% select(-node_id)

# layer
color = colorRamp2(seq(max(3), min(1.6), length = 10),viridis(10)) #color layer

# Arrange short path order and prepare the final version of species list

sp_names <- short_top_aver_25$node_name #create temporal species name to filter the big database
sp_names<-as.factor(sp_names) # to plot species name

top_aver_25_values<- as.data.frame(short_top_aver_25) %>% select(-node_name)
rownames(top_aver_25_values) <- sp_names

species_list_ordered <- species_list[match(sp_names, species_list$node_name), ]

color_sp<-species_list_ordered$color_sp



#Plotting

circos.clear()

png(file="change_top_25_CP.png",
    width=1600, height=1200)


# Extensive
E<- top_aver_25_values[,1,drop=FALSE]

circos.par(gap.degree = 16)

circos.heatmap(E, col = color, rownames.side = "outside", rownames.col	=color_sp,
               rownames.cex = 1.06, track.height = 0.1, cell.border = "black", cluster = FALSE)

# Semi Extensive
SE<- top_aver_25_values[,2, drop= FALSE]

circos.heatmap(SE, col = color, track.height = 0.1, cell.border = "black")

# Moderate
M<- top_aver_25_values[,3, drop= FALSE]

circos.heatmap(M, col = color,  track.height = 0.1, cell.border = "black")

# Semi-Intensive
SI<- top_aver_25_values[,4, drop= FALSE]

circos.heatmap(SI, col = color, track.height = 0.1, cell.border = "black")

# Intensive
I<- top_aver_25_values[,5, drop= FALSE]

circos.heatmap(I, col = color, track.height = 0.1, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 20),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(1.6,"cm"),  grid_width = unit(8,"cm"),title_gp = gpar(fontsize = 23, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(90, "mm"), 
     just = c("left", "bottom"))

legend("bottomleft", inset=.02, title="Guild", c ("Plant", "Leaf-miner parasitoid","Seed-feeding bird","Seed-feeding rodent")
                                                  , fill= unique(color_sp), horiz=FALSE, cex=2)

dev.off()


## Using ggplot
#ggplot(short_path_aver, aes(x = node_id, y = management, fill = short_aver_ES)) +
# geom_tile(width = 3, height = 3,  color = "black") + # Adjust tile size for a clearer donut shape
#  scale_fill_gradient(low = "green", high = "red", na.value = 'gray') + # Color gradient
#  coord_polar() + # Transform to polar coordinates
#geom_text(aes(label = services), position = position_stack(vjust = 0.5)) + # Add ring names
# ylim(c(1.5, 3.5)) + # Adjust y-limits to create the donut's hole
# theme_minimal() +
#  theme(axis.text = element_blank(),
#        axis.title = element_blank(),
#        panel.grid = element_blank(),
#        axis.ticks = element_blank())






########### PLOT 3: Each ring represents a particular habitat management and values
#indicate the average importance of species to indirectly affect ES (all species)



##### set up parameters and structure

# Define color of each layer and sps

# species
short_path_all<- short_path  %>% 
  group_by(management,node_id) %>% 
  mutate(short_aver_ES = mean(short_ave)) %>% 
  select(-services,-short_ave) %>% unique()

short_path_all$management <- factor(short_path_all$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

short_all_aver<-short_path_all %>% 
  select(node_id,taxon,management,short_aver_ES) %>% 
  spread(management,short_aver_ES) %>%  #rearrange dataframe
  ungroup() %>% 
  arrange(taxon) #rearrange according to trophic group


# layer
color = colorRamp2(seq(max(short_all_aver[,3:5], na.rm = TRUE), min(short_all_aver[,3:5], na.rm = TRUE),
                       length =50),viridis(50))#color layer



# Arrange short path order and prepare the final version of species list

sp_names <- short_all_aver$node_id #create temporal species name to filter the big database
sp_names<-as.factor(sp_names) # to plot species name

short_all_aver_values<- as.data.frame(short_all_aver) %>% select(-node_id,-taxon)
rownames(short_all_aver_values) <- sp_names

species_list_color <- short_path %>% select(node_id,color_sp) %>% unique() #list of species and color according to the taxon
  
species_list_color_ordered<- species_list_color[match(sp_names, species_list_color$node_id), ] #order to match the database

color_sp<-species_list_color_ordered$color_sp #vector of color assigned to each species


#Plotting

circos.clear()

png(file="short_path_ave_all_CP.png",
    width=1600, height=1200)


# Extensive
E<- short_all_aver_values[,1,drop=FALSE]

circos.par(gap.degree = 16)

circos.heatmap(E, col = color, rownames.side = "outside", rownames.col	=color_sp,
               rownames.cex = 1, track.height = 0.11, cell.border = "black", cluster = FALSE)

#name2<-rownames(E)
#color_sp
# Semi Extensive
SE<- short_all_aver_values[,2, drop= FALSE]


circos.heatmap(SE, col = color, track.height = 0.11, cell.border = "black")


# Moderate
M<- short_all_aver_values[,3, drop= FALSE]

circos.heatmap(M, col = color,  track.height = 0.11, cell.border = "black")

# Semi-Intensive
SI<- short_all_aver_values[,4, drop= FALSE]

circos.heatmap(SI, col = color, track.height = 0.11, cell.border = "black")

# Intensive
I<- short_all_aver_values[,5, drop= FALSE]

circos.heatmap(I, col = color, track.height = 0.11, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 20),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(1.6,"cm"),  grid_width = unit(8,"cm"),title_gp = gpar(fontsize = 23, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(100, "mm"), 
     just = c("left", "bottom"))

legend("bottomleft", inset=.02, title="Guild", c ("Aphid", "Butterfly","Crop","Flower-visiting",
          "Insect seed-feeder parasitoid","Leaf-miner parasitoid","Plant","Primary aphid parasitoid", "Rodent ectoparasite",
          "Secondary aphid parasitoid",  "Seed-feeding bird","Seed-feeding insect","Seed-feeding rodent") , fill= unique(color_sp), horiz=FALSE, cex=1)

dev.off()





#################

#### FROM EXTENSIVE TO INTENSIVE PERMANENT PASTURE

## Arrange the matrix to fix the names of species
short_path<- read.csv("Data/Land_use_shortpath_weighted_PP_intense.csv", sep =",", row.names = 1) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) %>% 
  mutate(color_sp =case_when(taxon == "Plant"~ "#00C1AB", #assign color to species according to the taxon
                             taxon == "Crop"~ "#BE9C00",
                             taxon == "Flower-visiting"~ "#8CAB00",
                             taxon == "Aphid"~ "#F8766D",
                             taxon == "Primary aphid parasitoid"~ "#00BBDA",
                             taxon == "Secondary aphid parasitoid"~ "#8B93FF",
                             taxon == "Leaf-miner parasitoid"~ "#00BE70",
                             taxon == "Seed-feeding insect"~ "#F962DD",
                             taxon == "Seed-feeding bird"~ "#D575FE",
                             taxon == "Seed-feeding rodent"~ "#FF65AC",
                             taxon == "Butterfly"~ "#E18A00",
                             taxon == "Insect seed-feeder parasitoid"~ "#24B700",
                             taxon == "Rodent ectoparasite"~ "#00ACFC") ) %>% 
  arrange(taxon) #order according to taxon


########### PLOT 2: Each ring represents a particular habitat management and values
#indicate the average importance of the top 25 species  in extesive to indirectly affect ES

#This graph shows if the most important species changes with habitat management


# identify the top 25 species that indirectly affect E(D)S in th extensive scenario
short_path_E <- short_path %>% filter(management == "E") 

# Select the most important species to indirectly affect ES 
species_imp<- short_path_E %>% 
  group_by(node_id) %>% 
  summarise(short_aver_ES = mean(short_ave)) %>% 
  arrange(short_aver_ES)

top_25<-species_imp[1:25,1] #identify the list of species that make up the top 25%

top_25_sp<-short_path_E %>% filter(node_id%in%top_25$node_id) %>%  #Data frame containing the 25% most important species
  mutate(services_code = case_when(
    services == "Crop production"~ 1,
    services == "Pollination"~ 2,
    services == "Butterfly watching"~ 3,
    services == "Seed dispersal"~ 4,
    services == "Bird watching"~ 5,
    services == "Pest control"~ 6,
    services == "Crop damage"~ 7
    
  ))


short_top_25<-top_25_sp %>% #just average for now (maybe add error bars in the future)
  select(node_name,services,short_ave) %>% 
  spread(services,short_ave)  #rearrange dataframe


sp_names <- short_top_25$node_name #create temporal species name to filter the big database

#sps
species_list<- read.csv("Data/Land_use_shortpath_weighted_CP_intense.csv", sep =",", row.names = 1) %>% 
  separate(node_name, c("trophic_lower", "node_name"),  "[A-Z]\\.") %>% 
  select(-trophic_lower) %>% mutate (node_name =  gsub(c("\\?"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("1"), "", node_name)) %>% 
  mutate (node_name =  gsub(c("zCROP"), "", node_name)) %>% 
  mutate (node_name = gsub("\\.", " ", node_name)) %>% 
  filter(node_name%in%sp_names) %>% #filter species in the dataframe
  mutate(color_sp =case_when(taxon == "Plant"~ "#00C1AB", #assign color to species according to the taxon
                             taxon == "Crop"~ "#BE9C00",
                             taxon == "Flower-visiting"~ "#8CAB00",
                             taxon == "Aphid"~ "#F8766D",
                             taxon == "Primary aphid parasitoid"~ "#00BBDA",
                             taxon == "Secondary aphid parasitoid"~ "#8B93FF",
                             taxon == "Leaf-miner parasitoid"~ "#00BE70",
                             taxon == "Seed-feeding insect"~ "#F962DD",
                             taxon == "Seed-feeding bird"~ "#D575FE",
                             taxon == "Seed-feeding rodent"~ "#FF65AC",
                             taxon == "Butterfly"~ "#E18A00",
                             taxon == "Insect seed-feeder parasitoid"~ "#24B700",
                             taxon == "Rodent ectoparasite"~ "#00ACFC") ) %>% 
  ungroup() %>% select(node_name,color_sp) %>% unique()




##### set up parameters and structure

# Define color of each layer and sps

# species
short_path_aver_25<- short_path %>% filter (node_name %in%short_top_25$node_name) %>% 
  group_by(management,node_id) %>% 
  mutate(short_aver_ES = mean(short_ave)) %>% 
  select(-services,-short_ave) %>% unique()

short_path_aver_25$management <- factor(short_path_aver_25$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

short_top_aver_25<-short_path_aver_25 %>% 
  select(node_name,management,short_aver_ES) %>% 
  spread(management,short_aver_ES) %>%  #rearrange dataframe
  ungroup() %>% select(-node_id)

# layer
color = colorRamp2(seq(max(3), min(1.6), length = 10),viridis(10)) #color layer

# Arrange short path order and prepare the final version of species list

sp_names <- short_top_aver_25$node_name #create temporal species name to filter the big database
sp_names<-as.factor(sp_names) # to plot species name

top_aver_25_values<- as.data.frame(short_top_aver_25) %>% select(-node_name)
rownames(top_aver_25_values) <- sp_names

species_list_ordered <- species_list[match(sp_names, species_list$node_name), ]

color_sp<-species_list_ordered$color_sp



#Plotting

circos.clear()

png(file="change_top_25_PP.png",
    width=1600, height=1200)


# Extensive
E<- top_aver_25_values[,1,drop=FALSE]

circos.par(gap.degree = 16)

circos.heatmap(E, col = color, rownames.side = "outside", rownames.col	=color_sp,
               rownames.cex = 1.06, track.height = 0.1, cell.border = "black",  cluster = FALSE)

# Semi Extensive
SE<- top_aver_25_values[,2, drop= FALSE]

circos.heatmap(SE, col = color, track.height = 0.1, cell.border = "black")

# Moderate
M<- top_aver_25_values[,3, drop= FALSE]

circos.heatmap(M, col = color,  track.height = 0.1, cell.border = "black")

# Semi-Intensive
SI<- top_aver_25_values[,4, drop= FALSE]

circos.heatmap(SI, col = color, track.height = 0.1, cell.border = "black")

# Intensive
I<- top_aver_25_values[,5, drop= FALSE]

circos.heatmap(I, col = color, track.height = 0.1, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 20),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(1.6,"cm"),  grid_width = unit(8,"cm"),title_gp = gpar(fontsize = 23, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(90, "mm"), 
     just = c("left", "bottom"))

legend("bottomleft", inset=.02, title="Guild", c ("Plant", "Leaf-miner parasitoid","Seed-feeding bird","Seed-feeding rodent")
       , fill= unique(color_sp), horiz=FALSE, cex=2)

dev.off()




########### PLOT 3: Each ring represents a particular habitat management and values
#indicate the average importance of species to indirectly affect ES (all species)



##### set up parameters and structure

# Define color of each layer and sps

# species
short_path_all<- short_path  %>% 
  group_by(management,node_id) %>% 
  mutate(short_aver_ES = mean(short_ave)) %>% 
  select(-services,-short_ave) %>% unique()

short_path_all$management <- factor(short_path_all$management, levels = c("E", "SE", "M", "SI","I")) #change order of factors

short_all_aver<-short_path_all %>% 
  select(node_id,taxon,management,short_aver_ES) %>% 
  spread(management,short_aver_ES) %>%  #rearrange dataframe
  ungroup() %>% 
  arrange(taxon) #rearrange according to trophic group


# layer
color = colorRamp2(seq(max(short_all_aver[,3:5], na.rm = TRUE), min(short_all_aver[,3:5], na.rm = TRUE),
                       length =50),viridis(50))#color layer



# Arrange short path order and prepare the final version of species list

sp_names <- short_all_aver$node_id #create temporal species name to filter the big database
sp_names<-as.factor(sp_names) # to plot species name

short_all_aver_values<- as.data.frame(short_all_aver) %>% select(-node_id,-taxon)
rownames(short_all_aver_values) <- sp_names

species_list_color <- short_path %>% select(node_id,color_sp) %>% unique() #list of species and color according to the taxon

species_list_color_ordered<- species_list_color[match(sp_names, species_list_color$node_id), ] #order to match the database

color_sp<-species_list_color_ordered$color_sp #vector of color assigned to each species


#Plotting

circos.clear()

png(file="short_path_ave_all_PP.png",
    width=1600, height=1200)


# Extensive
E<- short_all_aver_values[,1,drop=FALSE]

circos.par(gap.degree = 16)

circos.heatmap(E, col = color, rownames.side = "outside", rownames.col	=color_sp,
               rownames.cex = 1, track.height = 0.11, cell.border = "black", cluster = FALSE)

#name2<-rownames(E)
#color_sp
# Semi Extensive
SE<- short_all_aver_values[,2, drop= FALSE]


circos.heatmap(SE, col = color, track.height = 0.11, cell.border = "black")


# Moderate
M<- short_all_aver_values[,3, drop= FALSE]

circos.heatmap(M, col = color,  track.height = 0.11, cell.border = "black")

# Semi-Intensive
SI<- short_all_aver_values[,4, drop= FALSE]

circos.heatmap(SI, col = color, track.height = 0.11, cell.border = "black")

# Intensive
I<- short_all_aver_values[,5, drop= FALSE]

circos.heatmap(I, col = color, track.height = 0.11, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 20),  title_position = "topleft", title = "Short path", direction = "horizontal",
                  grid_height = unit(1.6,"cm"),  grid_width = unit(8,"cm"),title_gp = gpar(fontsize = 23, fontface = "bold"))

draw(lgd_mult, x = unit(20, "mm"), y = unit(100, "mm"), 
     just = c("left", "bottom"))

legend("bottomleft", inset=.02, title="Guild", c ("Aphid", "Butterfly","Crop","Flower-visiting",
                                                  "Insect seed-feeder parasitoid","Leaf-miner parasitoid","Plant","Primary aphid parasitoid", "Rodent ectoparasite",
                                                  "Secondary aphid parasitoid",  "Seed-feeding bird","Seed-feeding insect","Seed-feeding rodent") , fill= unique(color_sp), horiz=FALSE, cex=1)

dev.off()





################ PP ANALYSIS (NOT INCLUDED FOR NOW) ############

## From Extensive to intensive PP
short_path_land_change_PP<- short_path_land_change_ave %>% filter(land_use== "PP") 

# Model
short_PP<- glmmTMB(short_path_ave~management + taxon+ (1|node_id), 
                   family = Gamma(link = "log"), data = short_path_land_change_PP) #we already check and this is the best model
Anova(short_PP)
summary(short_PP)

#Homogeneity
EM<-resid(short_PP, type= "response") 
FM<-fitted(short_PP) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(short_PP, type= "response") 
boxplot(E1_lme~short_path_land_change_PP$management, main="Management")

# posthoc
post_amount<- emmeans(short_PP, ~ management)#management
pairs(post_amount)

post_amount_taxon<- emmeans(short_PP, ~ taxon)#management
pairs(post_amount_taxon)


################# CORE SPECIES

## From Extensive to intensive PP

# Step 1: Get Top 25 species in 'extensive' management
top_25_extensive <- short_path_land_change_PP %>%
  filter(management == "E") %>% group_by(management) %>% 
  slice_min(order_by = short_path_ave, n = 25) %>% 
  select(land_use,management,node_id,taxon) %>% 
  mutate(presence = 1)

# Step 2: Get Top 25 species in other managements
top_25_other_managements <- short_path_land_change_PP %>%
  filter(management != "E") %>%
  group_by(management) %>%
  slice_min(order_by = short_path_ave, n = 25) 


# Step 3: Check for persistence
change_top_25_PP<- unique(top_25_other_managements$management) %>%
  expand.grid(management = ., node_id = top_25_extensive$node_id) %>%
  left_join(top_25_other_managements, by = c("management", "node_id" )) %>%
  mutate(presence = ifelse(is.na(short_path_ave), 0, 1)) %>% #assign 1 when the species is still in the top 25
  select(management, node_id = node_id, presence) %>% 
  left_join(top_25_extensive, by = "node_id") %>% #add taxon information
  select(land_use,management.x,node_id,taxon,presence.x) %>% rename("management" = "management.x",
                                                                    "presence" = "presence.x")


# Merge dataset
change_top_25_PP_fin<- rbind(top_25_extensive,change_top_25_PP)

# Model
pers_2<-glmer (presence ~ management + ( 1| node_id), family = binomial(link= "logit"), data = change_top_25_PP_fin,
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 90000)))
Anova(pers_2)
summary(pers_2)

#Homogeneity
EM<-resid(pers_2, type= "deviance") 
FM<-fitted(pers_2) 
plot(x=FM, y=EM, xlab = "Ajustados", ylab = "Residuales normalizados")
abline(0,0, col="red", lwd= 3) 

#independence 
E1_lme<-resid(pers_2, type= "deviance") 
boxplot(E1_lme~change_top_25_PP$management, main="Tratamiento")


