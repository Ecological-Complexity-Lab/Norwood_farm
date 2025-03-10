############################## Script to create figure of ecological networks ################################

library(igraph)
library(tidyverse)
library(ggtext)
library(cowplot)
library(circlize)
library(viridis)
library(ComplexHeatmap)

setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")



####### Figure 2: 
#Panel (A): Prop. of direct ES retained (empirical). Panel (B): Heat map null model.

direct_ES<- read.csv("Data/Land_use_dir_ES.csv", sep =",")
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors

color_services_prop <-tibble(
  services = unique(direct_ES$services),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))


#Panel A
Prop<-direct_ES %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
  select(management,services,tot,prop) %>% unique() 

perc_direct_ES<- Prop %>% group_by(management) %>% 
  summarise(perc_lost = (1 - mean(prop)) *100)

Panel_A<- Prop %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services)), size = 3.5, show.legend = T) +
  scale_fill_manual(values = color_services_prop$color) + 
  scale_y_continuous(name = "Prop. of direct ES retained", limits = c(0, 1)) + 
  scale_x_discrete(name = "Management")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major=element_line(color = "gray"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=11, color='black'),
        axis.text = element_text(size=14, color='black'),
        axis.text.x= element_text(size =12), 
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 14),  # Add margin around the legend box
        legend.margin = margin(t = 5, r = 5, b = 5, l = 14)) +  # Adjust margin within the legend box)
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))+
         labs(fill = "ES")

Panel_A

  
#Panel B
dir_ES_z_score<-read.csv("Data/z_score_dir_ES_CP.csv")

z_score_dir<- dir_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensice and bird watching and seed dispersal for IM (all birds went extinct so there were no z scores)
sd_bw<-data.frame(management = c("E","E","E","E","E","E","E"), 
                  services = c("Bird watching", "Butterfly watching", 
                               "Crop damage", "Crop production","Pest control",
                               "Pollination", "Seed dispersal"),
                  z = c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),
                  signif = c("Benchmark","Benchmark","Benchmark","Benchmark",
                             "Benchmark","Benchmark","Benchmark"))
z_score_tot<- rbind (z_score_dir, sd_bw) %>% rename("Output" = "signif")



#Plot
z_score_tot$services<-factor(z_score_tot$services, levels = c("Seed dispersal", "Pollination","Pest control",
                                                                          "Crop production", "Crop damage",
                                                                          "Butterfly watching","Bird watching"))
z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

dir_ES_z_score<- dir_ES_z_score %>% rename("Output" = "signif")

color_services <-tibble(
  services = unique(z_score_tot$services),
  color = c('#e7298a','#e6ab02','#7570b3','#2c7fb8','#a6761d','#d95f02','#1b9e77'))



Panel_B<- ggplot(z_score_tot, aes(management, services, fill= Output)) + 
  geom_tile(color = "black")+
  scale_fill_manual(values = c("red","ivory1","ivory1"),
                    labels = c("Lower than random", 
                               "No difference",
                               "Benchmark"))+
  labs(x='Management', y="Ecosystem services (ES)")+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10,  
        color = color_services$color[match(levels(z_score_tot$services), color_services$services)], face = "bold"),
        axis.text = element_text(size=12, color='black'),
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 16, b = 5, l = 5),  # Add margin around the legend box
        legend.margin = margin(t = 5, r = 16, b = 5, l = 5)) +
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))+
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5, 
                   y = as.numeric(services) - 0.5, 
                   xend = as.numeric(management) + 0.5, 
                   yend = as.numeric(services) + 0.5), 
               color = "black", size = 0.5)  +
  geom_text(data = filter(dir_ES_z_score, Output == "below"),
            aes(label = round(dir_shuff_mean,3)),  # Display the mean value at the top
            size = 3.5, color = "black", vjust = -0.5) +  # Adjust vjust to move the text higher
  geom_text(data = filter(dir_ES_z_score, Output == "below"),
            aes(label = paste0("(", round(dir_shuff_sd,3),")")),  # Display the sd value at the bottom
              size = 3, color = "black", vjust = 1.5)

Panel_B

# Figure 3 (all panels together). Put manually the figures of the farms

pdf("Graphs/Figure_2_pre_final.pdf", width = 9, height = 5)
upper_row<- plot_grid(Panel_A + theme(plot.margin = unit(c(0.1,0.1, 0.1,0.1), "cm")),
                      Panel_B + theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")), 
                      ncol = 2, labels = c('(A)', "(B)"),# Panel A is 90% of its size and Panel B is 110%
                      label_x = c(-0.033, 0),  # Adjust the position of the labels
                      rel_widths = c(0.9, 1.1), 
                      align = "h",  # Aligns panels vertically
                      axis = "b"# Aligns both top and bottom axes
)
upper_row

dev.off()




####### Figure 4: 
#Panel (A): Relative change in the amount of ES (empirical). Panel (B): Heat map null model.
direct_ES<- read.csv("Data/Land_use_dir_ES.csv", sep =",")
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors

color_services <-tibble(
  services = unique(direct_ES$services),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))


#Panel A

#amount Bird and butterfly watching
tot_services_emp_watching<-direct_ES %>% filter(management=="E" &  (services == "Bird watching" | services == "Butterfly watching" )) %>% 
  group_by(management,services) %>% 
  summarize(tot_empirical_amount = sum(abun))

Prop_weight_watching<-  direct_ES %>% group_by(management,services) %>% 
  filter (services == "Bird watching" | services == "Butterfly watching") %>% 
  summarize(tot= sum(abun))%>% ungroup() %>%  
  mutate(Extensive_tot = case_when(
    services == "Bird watching"~ 2076,
    services == "Butterfly watching"~ 6903),
    ratio_change = tot / Extensive_tot)  

#amount the rest ESs
tot_services_emp_rest<-direct_ES %>% filter(management=="E" &  !(services == "Bird watching" | services == "Butterfly watching" )) %>% 
  group_by(management,services) %>% 
  summarize(tot_empirical_amount = sum(weight))

Prop_weight_rest<-  direct_ES %>% group_by(management,services) %>% 
  filter (!(services == "Bird watching" | services == "Butterfly watching")) %>% 
  summarize(tot= sum(weight))%>% ungroup() %>%  
  mutate(Extensive_tot = case_when(
    services == "Crop damage"~ 711450.9469,
    services == "Crop production"~ 209300.0000,
    services == "Pest control"~ 7108.3167,
    services == "Pollination"~ 36736.7426,
    services == "Seed dispersal"~ 362197.4900),
    ratio_change = tot / Extensive_tot)  

#merge the data
Prop_amount<- rbind(Prop_weight_watching,Prop_weight_rest)


#Plot 
Panel_A<- Prop_amount %>% ggplot(aes(x = management, y = ratio_change)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services)), size = 3.5, show.legend = T) +
  scale_fill_manual(values = color_services$color, name = "ES") + 
  scale_y_continuous(name = "Relative change in the amount \n of direct ES provided", limits = c(0, 3)) + 
  scale_x_discrete(name = "Management")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major=element_line(color = "gray"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=11, color='black'),
        axis.text = element_text(size=14, color='black'),
        axis.text.x= element_text(size =12), 
        axis.title = element_text(size=13, color='black'),
      axis.title.y = element_text(size=13, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 20),  # Add margin around the legend box
        legend.margin = margin(t = 5, r = 5, b = 5, l = 20)) +  # Adjust margin within the legend box)
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))+
  labs(fill = "ES")


Panel_A



#Panel B
amount_ES_z_score<-read.csv("Data/z_score_amount_ES_CP.csv")

# Prepare dataframe
z_score_amount<- amount_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensice and bird watching and seed dispersal for IM (all birds went extinct so there were no z scores)
sd_bw<-data.frame(management = c("E","E","E","E","E","E","E"), 
                  services = c("Bird watching", "Butterfly watching", 
                               "Crop damage", "Crop production","Pest control",
                               "Pollination", "Seed dispersal"),
                  z = c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),
                  signif = c("Benchmark","Benchmark","Benchmark","Benchmark",
                             "Benchmark","Benchmark","Benchmark"))
z_score_tot<- rbind (z_score_amount, sd_bw) %>% rename("Output" = "signif")


#Plot
z_score_tot$services<-factor(z_score_tot$services, levels = c("Seed dispersal", "Pollination","Pest control",
                                                              "Crop production", "Crop damage",
                                                              "Butterfly watching","Bird watching"))
z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

amount_ES_z_score<- amount_ES_z_score %>% rename("Output" = "signif")

color_services <-tibble(
  services = unique(z_score_tot$services),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))


Panel_B<- ggplot(z_score_tot, aes(management, services, fill= Output)) + 
  geom_tile(color = "black")+
  scale_fill_manual(values = c("red","ivory1","ivory1"),
                    labels = c("Lower than random", 
                               "No difference",
                               "Benchmark"))+
  labs(x='Management', y="Ecosystem services (ES)")+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10,  
                                   color = color_services$color[match(levels(z_score_tot$services), color_services$services)], face = "bold"),
        axis.text = element_text(size=12, color='black'),
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 16, b = 5, l = 5),  # Add margin around the legend box
        legend.margin = margin(t = 5, r = 16, b = 5, l = 5)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))+
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5, 
                   y = as.numeric(services) - 0.5, 
                   xend = as.numeric(management) + 0.5, 
                   yend = as.numeric(services) + 0.5), 
               color = "black", size = 0.5)  +
  geom_text(data = filter(amount_ES_z_score, Output == "below"),
            aes(label = round(amount_shuff_mean,3)),  # Display the mean value at the top
            size = 3.5, color = "black", vjust = -0.5) +  # Adjust vjust to move the text higher
  geom_text(data = filter(amount_ES_z_score, Output == "below"),
            aes(label = paste0("(", round(amount_shuff_sd,3),")")),  # Display the sd value at the bottom
            size = 3, color = "black", vjust = 1.5)

Panel_B

# Figure 4 (all panels together). Put manually the figures of the farms

pdf("Graphs/Figure_3_pre_final.pdf", width = 9, height = 5)
upper_row<- plot_grid(Panel_A + theme(plot.margin = unit(c(0.1,0.1, 0.1,0.1), "cm")),
                      Panel_B + theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")), 
                      ncol = 2, labels = c('(A)', "(B)"),# Panel A is 90% of its size and Panel B is 110%
                      label_x = c(-0.033, 0),  # Adjust the position of the labels
                      rel_widths = c(0.9, 1.1), 
                      align = "h",  # Aligns panels vertically
                      axis = "b"# Aligns both top and bottom axes
)
upper_row

dev.off()




####### Figure 5: 
#Panel (A): Prop. of indirect effects on ES retained (empirical). Panel (B): Heat map null model.
output_ind_ES <- read.csv("Data/Land_use_ind_ES.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors

color_services_prop <-tibble(
  services = unique(output_ind_ES$services_to),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))


#Panel A
Prop_ind<-output_ind_ES %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of indirect effects on ES retained
  select(management,services_to,tot,prop) %>% unique()

perc_indirect_ES<- Prop_ind %>% group_by(management) %>% 
  summarise(perc_lost = (1 - mean(prop)) *100)

Panel_A<- Prop_ind %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services_to)), size = 3.5, show.legend = T) +
  scale_fill_manual(values = color_services_prop$color) + 
  scale_y_continuous(name = "Prop. of indirect effects on ES retained", limits = c(0, 1)) + 
  scale_x_discrete(name = "Management")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major=element_line(color = "gray"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        axis.text.y = element_text(size=11, color='black'),
        axis.text = element_text(size=14, color='black'),
        axis.text.x= element_text(size =12), 
        axis.title = element_text(size=13, color='black'),
        axis.title.y = element_text(size=13, color='black'),
        axis.line = element_blank(),
        legend.text.align = 0,
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 20, b = 5, l = 1),  # Add margin around the legend box
        legend.margin = margin(t = 5, r = 20, b = 5, l = 1))+
  coord_cartesian(clip = "off")+
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))+
  labs(fill = "ES")

Panel_A



#Panel B
indir_ES_z_score<-read.csv("Data/z_score_ind_ES_CP.csv", sep =",") %>% rename ("services" = "services_to")

# Prepare dataframe
z_score_ind<- indir_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensice and bird watching and seed dispersal for IM (all birds went extinct so there were no z scores)
sd_bw<-data.frame(management = c("E","E","E","E","E","E","E"), 
                  services = c("Bird watching", "Butterfly watching", 
                               "Crop damage", "Crop production","Pest control",
                               "Pollination", "Seed dispersal"),
                  z = c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),
                  signif = c("Benchmark","Benchmark","Benchmark","Benchmark",
                             "Benchmark","Benchmark","Benchmark"))

z_score_tot<- rbind (z_score_ind, sd_bw) %>% rename("Output" = "signif")

z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors
z_score_tot$services <- factor(z_score_tot$services, levels = c("Seed dispersal", "Pollination","Pest control",
                                                                "Crop production", "Crop damage",
                                                                "Butterfly watching","Bird watching"))

#Plot
indir_ES_z_score<- indir_ES_z_score %>% rename("Output" = "signif")

color_services <-tibble(
  services = rev(levels(z_score_tot$services)),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))


Panel_B<- ggplot(z_score_tot, aes(management, services, fill= Output)) + 
  geom_tile(color = "black")+
  scale_fill_manual(values = c("dodgerblue3","red","ivory1","ivory1"),
                    labels = c("Greater than random",
                               "Lower than random", 
                               "No difference",
                               "Benchmark"))+
  labs(x='Management', y="Ecosystem services (ES)")+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=10,  
                                   color = color_services$color[match(levels(z_score_tot$services), color_services$services)], face = "bold"),
        axis.text = element_text(size=12, color='black'),
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 1, b = 5, l = 5),  # Add margin around the legend box
        legend.margin = margin(t = 5, r = 1, b = 5, l = 5),
        legend.key.height = unit(0.6, "cm"),  # Reduce the height of the legend keys
        legend.key.width = unit(0.6, "cm"))+    # Optionally reduce the width of the legend keys) 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2))+
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5, 
                   y = as.numeric(services) - 0.5, 
                   xend = as.numeric(management) + 0.5, 
                   yend = as.numeric(services) + 0.5), 
               color = "black", size = 0.5)  +
  geom_text(data = filter(indir_ES_z_score, Output %in% c("below","above")),
            aes(label = round(ind_shuff_mean,3)),  # Display the mean value at the top
            size = 3.5, color = "black", vjust = -0.5) +  # Adjust vjust to move the text higher
  geom_text(data = filter(indir_ES_z_score,Output %in% c("below","above")),
            aes(label = paste0("(", round(ind_shuff_sd,3),")")),  # Display the sd value at the bottom
            size = 3, color = "black", vjust = 1.5)

Panel_B


# Figure 4 (all panels together). Put manually the figures of the farms
pdf("Graphs/Figure_4_pre_final.pdf", width = 9, height = 5)
upper_row<- plot_grid(Panel_A + theme(plot.margin = unit(c(0.8,0.1, 0.1,0.1), "cm")),
                      Panel_B + theme(plot.margin = unit(c(0.8,0.1,0.1,0.5), "cm")), 
                      ncol = 2, labels = c('(A)', "(B)"),
                      label_x = c(-0.02, 0),# Adjust the position of the labels (A, B)
                      rel_widths = c(0.9, 1.1), # Panel A is 90% of its size and Panel B is 110%
                      align = "h",  # Aligns panels vertically
                      axis = "b"# Aligns both top and bottom axes
)
upper_row

dev.off()




####### Figure 6: 
#Panel (A): Indirect effects on ES (general pattern). Panel (B): Top 5. Because of
#the circular plot. we should do it manually.


## Panel A

## upload and arrange dataframe
short_path_land_change<-read.csv("Data/Land_use_shortpath_weighted_CP_intense.csv", row.names = 1) 

short_path_land_change_ave<- short_path_land_change %>% group_by(management,node_id) %>% 
  mutate(short_path_ave = mean(short_ave)) %>% select(-services, - short_ave) %>% unique() %>%  #calculate average short path of each species to all ES in each habitat management
  mutate(taxon = str_replace(taxon, "Flower-visiting", "Flower visitor"))

ave_management_taxon<-short_path_land_change_ave %>% group_by(management,taxon) %>% 
  summarise(ave_short = mean(short_path_ave),
            sd_short = sd(short_path_ave)) 

ave_management_taxon$management <- factor(ave_management_taxon$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

ave_management_taxon$taxon<-as.factor(ave_management_taxon$taxon)


#Plot
color_trophic <-tibble(taxon = c("Plant","Crop","Flower visitor","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                 "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                 "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                       color = c("#33a02c","#b15928","#a6cee3","#1f78b4","#b2df8a","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6",
                                 "#6a3d9a", "#cccc7a", "#e7298a"))

pdf("Graphs/Figure_5_panel_A.pdf", width = 4.5, height = 5)
Panel_A<- ggplot(ave_management_taxon, aes(x = management, y = ave_short, group =taxon, color= taxon)) +
  scale_color_manual(values = color_trophic$color[match(levels(ave_management_taxon$taxon), color_trophic$taxon)])+
  geom_point() +
  geom_line(size = 1.4)  +
  geom_errorbar(aes(ymin = ave_short - sd_short, ymax = ave_short + sd_short), width = 0.2) +
  labs(x = "Land conversion",
       y = "Shortest path ",
       color = "Trophic guild") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black",fill = NA,size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=12, color='black'),
        axis.title = element_text(size=15, color='black'),
        axis.line = element_blank(),
        legend.title =  element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 1, b = 5, l = 5),  # Add margin around the legend box
        legend.margin = margin(t = 5, r = 1, b = 5, l = 5),
        legend.key.height = unit(0.6, "cm"),  # Reduce the height of the legend keys
        legend.key.width = unit(0.6, "cm")) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 3))

Panel_A

dev.off()



### Panel B

## Prepare and arrange dataframe

# Check the 5 most important species per trophic group in Extensive
top_5_taxon_extensive<-short_path_land_change_ave %>%
  filter(management == "E") %>% group_by(management, taxon) %>% 
  arrange(short_path_ave) %>% # Arrange by short_path_ave within each group
  slice_head(n = 5) # Take the first 5 rows within each group

#Filter the importance of the top 5 species (from the extensive) across management scenarios
top_5_average<- short_path_land_change_ave %>%  filter(node_id%in%top_5_taxon_extensive$node_id)

## Plot

# set up parameters and structure
# Define color of each layer and sps
top_5_average$management <- factor(top_5_average$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

top_5_ave <- top_5_average %>% ungroup() %>% 
  select(node_id,taxon,management,short_path_ave) %>%
  spread(management,short_path_ave) %>%  #rearrange dataframe
  ungroup() 
top_5_ave<-top_5_ave[,c(1,2,3,7,6,8,4,5)]

color = colorRamp2(seq(max(top_5_ave[,3:8], na.rm = TRUE), min(top_5_ave[,3:8], na.rm = TRUE),
                       length =5),viridis(5))#color layer

# Arrange short path order and prepare the final version of species list

sp_names <- top_5_ave$node_id #create temporal species name to filter the big database
sp_names<-as.factor(sp_names) # to plot species name

top_5_ave_values<- as.data.frame(top_5_ave) %>% select(-node_id,-taxon)
rownames(top_5_ave_values) <- sp_names 
top_5_ave_values<-top_5_ave_values[,c(1,5,6,3,2,4)]

color_trophic <-tibble(taxon = c("Plant","Crop","Flower visitor","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                 "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                 "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                       color = c("#33a02c","#b15928","#a6cee3","#1f78b4","#b2df8a","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6",
                                 "#6a3d9a", "#cccc7a", "#e7298a")) #color trophic group

species_list_color <- top_5_ave %>% select(node_id,taxon) %>%  #list of species and color according to the taxon
  left_join(color_trophic, by = "taxon") 


#Plotting
circos.clear()

pdf("Graphs/Figure_5_panel_B.pdf", width = 4.5, height = 5)

# Extensive
E<- top_5_ave_values[,1,drop=FALSE]
circos.par(start.degree = 10, gap.degree = 1)
circos.heatmap(E, col = color, #rownames.side = "outside", rownames.col= species_list_color$color,
               rownames.cex = 0.7, track.height = 0.11, cell.border = "black",cluster = FALSE,
               split = factor(species_list_color$taxon, levels = unique(species_list_color$taxon)))

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
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 9),  title_position = "topleft", title = "Shortest path", direction = "horizontal",
                  grid_height = unit(0.5,"cm"),  grid_width = unit(0.7,"cm"),title_gp = gpar(fontsize = 10))
draw(lgd_mult, x = unit(15, "mm"), y = unit(10, "mm"), 
     just = c("bottom"))
dev.off()


