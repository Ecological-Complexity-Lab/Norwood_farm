
################## EMPIRICAL NETWORK #################################

library(emln)#multilayer package
library(readr)
library(ggplot2)
library(cowplot)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")

######### --- Upload multilayer network
Norwood_farm<-readRDS("Data/Norwood_farm.RData") #read multilayer object



################## --- CREATE MANAGAMENT SCENARIOS (abundances_trial)


### Add the abundances (as state nodes attributes) 
abundances<-read.csv("Data/species_abundances.csv",header=T) #call abundances

state_nodes_ab<-Norwood_farm$state_nodes %>% left_join(abundances, 
                                                       by = c("layer_name" = "habitat",
                                                              "node_name" = "species_name")) %>% #add abundances
  left_join(Norwood_farm$nodes, by = "node_id") %>% 
  select(layer_id,node_id,abundance, taxon) ##add taxon



########## -- Create Management scenarios

##### -- Extensive

extensive_edgelist<- Norwood_farm$extended_ids %>% 
  select(-layer_to) %>% rename("habitat" = "layer_from") %>% 
  mutate(management = "E") %>% select(-habitat,-weight) %>% unique() #aggregate network


# estimate relative abundances of species in the aggregated network
ab_ext<-state_nodes_ab %>% select(-layer_id) %>% group_by(node_id,taxon) %>%
  mutate(abun = sum(abundance)) %>% distinct(abun) %>% group_by(taxon) %>% 
  mutate(tot_ab_taxon = sum(abun)) %>% #total abundance per taxon
  group_by(node_id) %>% 
  mutate(rel_ab=abun/tot_ab_taxon)#relative abundance per sp



# incorporate rel abundances to the edge list and calculate the weight (Product of relative abundances)
ext_edgelist_aggr<- extensive_edgelist %>% left_join(ab_ext, by = c("node_from" = "node_id")) %>% 
  left_join(ab_ext, by = c("node_to" = "node_id")) %>% 
  rename ("rel_ab_from" = "rel_ab.x", "rel_ab_to" = "rel_ab.y") %>% 
  mutate(weight = rel_ab_from * rel_ab_to) %>% #calculate weight
  select(node_from,node_to,weight,management)



#### Check for species that provide pest control and crop damage
crops = 94:99
aphid = 337:364
seed_ins = 476:494
seed_bird = 495:506
seed_rod = 507:510
butt = 511:526
flow_vis = 100:336



#crop damage

seed_rod_crop<- ext_edgelist_aggr %>% filter(node_to %in%seed_rod & node_from %in%crops)
aphid_crop<- ext_edgelist_aggr %>% filter(node_to %in%aphid & node_from %in%crops)
seed_ins_crop<- ext_edgelist_aggr %>% filter(node_to %in%seed_ins & node_from %in%crops)
seed_bird_crop<- ext_edgelist_aggr %>% filter(node_to %in%seed_bird & node_from %in%crops)


#pest control
pests<-rbind(seed_rod_crop,aphid_crop,seed_bird_crop) %>% select(node_to) %>% unique()
pest = as.vector(pests$node_to)

#potential species controling pest 
pri_par = 365:375
sec_par = 376:382
seed_ins_par = 527:543
rod_par = 544:551
leaf_par = 383:475


par_1_pest<- ext_edgelist_aggr %>% filter(node_to %in%pri_par & node_from %in%pest)
par_2_crop<- ext_edgelist_aggr %>% filter(node_to %in%sec_par & node_from %in%pest)
seed_ins_par<- ext_edgelist_aggr %>% filter(node_to %in%seed_ins_par & node_from %in%pest)
rod_par_pest<- ext_edgelist_aggr %>% filter(node_to %in%rod_par & node_from %in%pest)
leaf_par_pest<- ext_edgelist_aggr %>% filter(node_to %in%leaf_par & node_from %in%crops)
#las potenciales sps que generan polinizacion en cultivo son 4



################## --- CALCULATE DIRECT E(D)S PROVISION AND INDIRECT EFFECT ON ES

##### --  DIRECT E(D)S PROVISION


## Add information of ES to the state_node_list (values 0-1)

nodes_ES<- right_join(state_nodes_ab, Norwood_farm$nodes, by = "node_id")%>% 
  select(node_id,taxon.x,abundance, "Crop production",
         "Pollination", "Crop damage", "Pest control", "Seed dispersal", "Butterfly watching", "Bird watching") %>% 
  group_by(node_id) %>% rename("taxon" = "taxon.x") %>% 
  gather("services","value",4 :10) #we conserve species that not directly provide ES because can serve as intermediate hop


## -- Estimate direct E(D)S provision with the abundances (however, we multiply for trophic group's body size when differenr trophic groups provide the same ES (case of crop damage))

direct_ES <- nodes_ES %>% filter (value ==1) %>% 
  mutate (type = "D",
          body_size =  case_when( #to control ES for body size
            services != "Crop damage"~ 1,
            (services == "Crop damage") &
              (taxon == "Seed-feeding bird"|taxon == "Seed-feeding rodent")~1, #birds and rodents receives a 1 cause they are the biggest producing crop damage 
            (services == "Crop damage") & (taxon == "Aphid"|
                                             taxon == "Seed-feeding insect")~0.5),
          weight = abundance * body_size,
          output = case_when( #output + or - according to the service
            services == "Crop damage"~ "-",
            TRUE ~ "+")) %>% #weight of direct ES provision
  select(-value) 


## -- Add indirect effect of species on E(D)S (previously calculated)

Indirect_ES<-read.csv("Data/Land_use_output_weighted_CP_intense.csv", sep = ",") %>% 
  filter(management == "E") %>% select(-management) #change for "Data/Land_use_output_weighted.csv" 


# Rearrange direct_ES before merge

direct_1<-direct_ES %>% mutate(node_int = NA,
                               node_to = NA,
                               services_to = NA,
                               hop = NA) %>% select(-body_size,-abundance)

direct_1<-direct_1[,c(3,1,2,7,8,9,5,10,4,6)]  

## Final dataframe 

Final_ES<-rbind(direct_1,Indirect_ES) %>%
  mutate(type = case_when(type == "D"~ "Direct",
                          type == "I"~ "Indirect"),
         output = case_when (output == "+" ~ "Positive",
                             output == "-" ~ "Negative"))



#####   EXPLORATORY GRAPHS


### Proportion of Direct provision and indirect effects on E(D)S

Number<-Final_ES %>% group_by(type) %>% summarize(Number = n()) %>% mutate(provision = "E(D)S")

Number$type <- factor(Number$type, levels = c("Indirect","Direct")) #change order of factors

Number<-Number%>%   
  ggplot(aes(y=Number, x= "E(D)S", fill =type)) + 
  geom_bar(position="stack", stat="identity", color = "black")+
  labs(x='Type of provision', y="Number of E(D)S provided") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank())

#ggsave("Empirical_number_EDS.png")

### Proportion of output per type of provision

output_type<-Final_ES %>% group_by(type) %>% 
  mutate(Total = n()) %>% group_by(type,output) %>% 
  summarise(prop = n() /Total) %>% unique()

Prop_output_type<-output_type %>% 
  ggplot(aes(y=prop, x=type, fill = output)) + 
  geom_bar(position="stack", stat="identity", color = "black")+
  ggtitle("Ratio direct +/- per type")+
  labs(x='Type of E(D)S provision', y="Prop of output") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=13, color='black'),
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")

#ggsave("empirical_output.png")


### Proportion of output provided per taxon


## Direct and indirect two hops (- and +)


# Direct

D_taxon_output<-Final_ES %>% filter(type == "Direct") %>% group_by(output) %>% 
  mutate(Total = n()) %>% group_by(output, taxon) %>% 
  summarise(Number = n(), Prop = Number /Total) %>% unique()

D_taxon_output$output <- factor(D_taxon_output$output, levels = c("Positive","Negative")) #change order of factors


D_taxon_output<- D_taxon_output %>% 
  ggplot(aes(y=Prop, x=output, fill = taxon)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Aph","Butt", "Crop","Flower-visitor ins",
                              "Insect seed-feeder par", "Leaf-miner par", 
                              "Prim aphid par","Sec aphid par", "Seed-feeding bird", 
                              "Seed-feeding ins","Seed-feeding rod"), values = c("#F8766D",
                            "#E18A00","#BE9C00", "#8CAB00", "#24B700", "#00BE70","#00BBDA", 
                            "#8B93FF", "#D575FE","#F962DD", "#FF65AC"))+
  ggtitle("Direct provision")+
  labs(x='Output', y="Prop output provided per taxon") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=13, color='black'),
        axis.text.x= element_text(size =11, angle = 90), 
        axis.text.y= element_text(size =11, angle = 90), 
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_blank(),
        legend.text = element_text(size = 8))

#ggsave("empirical_output_taxon_direct.png")

# Indirect

I_taxon_output<-Final_ES %>% filter(type == "Indirect") %>% group_by(output) %>% 
  mutate(Total = n()) %>% group_by(output, taxon) %>% 
  reframe(Number = n(), Prop = Number /Total) %>% unique()

I_taxon_output$output <- factor(I_taxon_output$output, levels = c("Positive","Negative")) #change order of factors


I_taxon_output<- I_taxon_output %>% 
  ggplot(aes(y=Prop, x=output, fill = taxon)) + 
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Aph","Butt", "Crop","Flower-visitor ins","Insect seed-feeder par", 
                              "Leaf-miner par", "Plants","Prim aphid par", "Rodent ectopar",
                              "Sec aphid par", "Seed-feeding bird", "Seed-feeding ins",
                              "Seed-feeding rod"),
                    values = c("#F8766D", "#E18A00","#BE9C00", "#8CAB00",
                               "#24B700", "#00BE70","#00C1AB", "#00BBDA", "#00ACFC",
                               "#8B93FF", "#D575FE","#F962DD", "#FF65AC"))+
  ggtitle("Indirect provision")+
  labs(x='Output', y="Prop output provided per taxon") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=13, color='black'),
        axis.text.x= element_text(size =11, angle = 90), 
        axis.text.y= element_text(size =11, angle = 90), 
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_blank(),
        legend.text = element_text(size = 8))

#ggsave("empirical_output_taxon_ind.png")


### Proportion of trophic groups mediating indirect effects

plants = 1:93
crops = 94:99
flow_vis = 100:336
aphid = 337:364
pri_par = 365:375
sec_par = 376:382
leaf_par = 383:475
seed_ins = 476:494
seed_bird = 495:506
seed_rod = 507:510
butt = 511:526
seed_ins_par = 527:543
rod_par = 544:551


int_trophic<-Final_ES %>% filter(!is.na(node_int)) %>% 
  mutate(taxon_int =case_when(
    node_int%in%plants ~ "Plant",
    node_int%in%crops ~ "Crop",
    node_int%in%flow_vis ~ "Flower visitor",
    node_int%in%aphid ~ "Aphid",
    node_int%in%pri_par ~ "Primary aphid parasitoid",
    node_int%in%sec_par ~ "Secondary aphid parasitoid",
    node_int%in%leaf_par ~ "Leaf-miner parasitoid",
    node_int%in%seed_ins ~ "Seed-feeding insect",
    node_int%in%seed_bird ~ "Seed-feeding bird",
    node_int%in%seed_rod ~ "Seed-feeding rodent",
    node_int%in%butt ~ "Butterfly",
    node_int%in%seed_ins_par ~ "Insect seed-feeder parasitoid",
    node_int%in%rod_par ~ "Rodent ectoparasite",
  )) %>% select(node_int,taxon_int) %>%ungroup() %>%  
  mutate(Total = n()) %>% group_by(taxon_int) %>% 
  reframe(Number = n(), Perc = (Number /Total)*100) %>% unique() %>% 
  rename("taxon" = "taxon_int") 

color_trophic <-tibble(taxon = c("Plant","Crop","Flower visitor","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                 "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                 "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                       color = c("#33a02c","#b15928","#a6cee3","#1f78b4","#b2df8a","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6",
                                 "#6a3d9a", "#cccc7a", "#e7298a"))



#plot
pdf("Graphs/ind_effects_mediated.pdf", width = 12, height = 7)

int_trophic %>% 
  ggplot(aes(y=Perc, x= taxon, fill = taxon)) + 
  geom_bar(position="stack", stat="identity")+ 
  geom_text(aes(label = Number),  # Replace 'abundance_variable' with the actual column name for abundance
            position = position_stack(vjust = 1),  # Adjust the position of the labels
            size = 5, color = "black",
            vjust = -0.5) + 
  scale_fill_manual(values = setNames(color_trophic$color, color_trophic$taxon))+
  scale_color_manual(values = setNames(color_trophic$color, color_trophic$taxon)) + 
  scale_y_continuous(breaks = seq(0, 100, by = 15)) + 
  labs(x = "Trophic guild",
       y = "Percentage of indirect effects on ES\nmediated by trophic guilds",
       fill = "Trophic guild")+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.spacing = unit(0.5, "cm"),
        axis.text = element_text(size = 15, color = 'black'),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.title = element_text(size = 17, color = 'black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title = element_text(size = 13, color = "black"),
        legend.text = element_text(size = 12))
dev.off()






#### Weight of direct and indirect E(D)S provision

weight<-Final_ES %>% group_by(type,output) %>% 
  summarise(ave_weight = mean(weight),
         se_weight =  sd(weight) / sqrt(n())) %>% unique()


# Direct

#barplot
weights_output_dir<-weight %>%  filter (type == "Direct") %>% 
  ggplot(aes(y=ave_weight, x=type, fill = output)) + 
  geom_errorbar(
    aes(ymin =0 , ymax = ave_weight + se_weight),
    position = position_dodge(width = 0.9),
    width = 0.25) +
  geom_bar(position=position_dodge(), stat="identity")+
  labs(x='Output', y="Weight") +theme_bw()+ ggtitle("Direct E(D)S provision")+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")


#Indirect

weights_output_ind<-weight %>%  filter (type == "Indirect") %>% 
  ggplot(aes(y=ave_weight, x=type, fill = output)) + 
  geom_errorbar(
    aes(ymin =0 , ymax = ave_weight + se_weight),
    position = position_dodge(width = 0.9),
    width = 0.25) +
  geom_bar(position=position_dodge(), stat="identity")+
  labs(x='Output', y="Weight") +theme_bw()+ ggtitle("Indirect E(D)S provision")+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=15, color='black'),
        axis.text.x= element_text(), 
        axis.title = element_text(size=17, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 13, color = "black"),
        legend.text = element_text(size = 11),
        legend.position = "bottom")

upper_row<- plot_grid(weights_output_dir,weights_output_ind ,
                      ncol = 2)
upper_row
#ggsave("empirical_weight.png")


#### Abundance per trophic group

abundances<-ab_ext %>% group_by(taxon) %>% summarise(ab_taxon_mean = mean(abun),
                                                     ab_taxon_se = sd(abun)/ sqrt(length(abun)))


ab_taxon<-  abundances %>%
  ggplot(aes(y=ab_taxon_mean, x=taxon, fill = taxon)) + 
  geom_errorbar(
    aes(ymin =0 , ymax = ab_taxon_mean + ab_taxon_se),
    position = position_dodge(width = 0.9),
    width = 0.25) +
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Aph","Butt", "Crop","Flower-visitor ins","Insect seed-feeder par", 
                              "Leaf-miner par", "Plants","Prim aphid par", "Rodent ectopar",
                              "Sec aphid par", "Seed-feeding bird", "Seed-feeding ins", "Rodents"
  ),
  values = c("#F8766D", "#E18A00","#BE9C00", "#8CAB00",
              "#24B700", "#00BE70","#00C1AB", "#00BBDA", "#00ACFC",
              "#8B93FF", "#D575FE","#F962DD",  "#FF65AC"))+
  labs(x='Taxon', y="Abundances") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=9, color='black'),
        axis.text.x= element_text(size =9, angle = 90), 
        axis.text.y= element_text(size =11),
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_blank(),
        legend.text = element_text(size = 8))
#ggsave("empirical_abundances_taxon.png")



ab_taxon_without_aphid<-  abundances %>%  filter(taxon != "Aphid") %>% 
  ggplot(aes(y=ab_taxon_mean, x=taxon, fill = taxon)) + 
  geom_errorbar(
    aes(ymin =0 , ymax = ab_taxon_mean + ab_taxon_se),
    position = position_dodge(width = 0.9),
    width = 0.25) +
  geom_bar(position="stack", stat="identity", color = "black")+ 
  scale_fill_manual(label = c("Butt", "Crop","Flower-visitor ins","Insect seed-feeder par", 
                              "Leaf-miner par", "Plants","Prim aphid par", "Rodent ectopar",
                              "Sec aphid par", "Seed-feeding bird", "Seed-feeding ins", "Rodents"
  ),
  values = c( "#E18A00","#BE9C00", "#8CAB00",
             "#24B700", "#00BE70","#00C1AB", "#00BBDA", "#00ACFC",
             "#8B93FF", "#D575FE","#F962DD",  "#FF65AC"))+
  labs(x='Taxon', y="Abundances") +theme_bw()+
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text = element_text(size=9, color='black'),
        axis.text.x= element_text(size =9, angle = 90), 
        axis.text.y= element_text(size =11),
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_blank(),
        legend.text = element_text(size = 8))
#ggsave("empirical_abundances_withoutaphid.png")
