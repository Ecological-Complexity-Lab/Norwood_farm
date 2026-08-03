# In this code, we created the preliminary version of the figures 3-5 used in the main manuscript, 
# and the figures for the supplementary information (S1, S3, S4-S8). The figures were later edited 
# in Adobe Illustrator to improve their aesthetics and to add labels and annotations. Some of them 
# were later edited to include pictures of the farm.


#In the files, the term "ES" refers to "NCP" and "1 hop" and "2 hop" indicate first-order and second-order pathways, respectively.

## -- Load libraries --------------------------------------------------------------------------------------------------------
library(igraph)
library(tidyverse)
library(ggtext)
library(cowplot)
library(circlize)
library(viridis)
library(ComplexHeatmap)

## -- get_data--------------------------------------------------------------------------------------------------------
#setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")
setwd("/Users/agustinvitali/Desktop/Work/Papers/In_prep/Norwood_Farm/GitHub/Norwood_farm")
source("R/functions.R") #call functions file


############## -- Shared color palettes used across figures 
## Color assigned to each NCP (used in Figures 3, 4, S4-S7)
color_services <- tibble(
  services = c("Bird watching", "Butterfly watching", "Crop damage",
               "Crop production", "Pest control", "Pollination", "Seed dispersal"),
  color = c('#6F2A56','#F2520D','#99E5B9', '#F09942', '#F0E675', '#47ACEB', '#0F74BD'))

## Color assigned to each trophic guild (used in Figure 5, S1, S8)
color_trophic <- tibble(taxon = c("Non-cultivated plant","Crop","Flower visitor","Aphid","Primary aphid parasitoid","Secondary aphid parasitoid",
                                  "Leaf-miner parasitoid","Seed-feeding insect","Seed-feeding bird",
                                  "Seed-feeding rodent","Butterfly","Insect seed-feeder parasitoid","Rodent ectoparasite"),
                        color = c("#90B477","#D3D64C","#C2DCF6","#4BD7B9","#82B2EC","#2E2BD4",
                                  "#DC972E","#AF645F","#F7F7D0","#4E8BE5","#DE2421","#7954AB","#5C90A3"))



##############  --  Figure 3 


### Panel (A): Prop. of direct ES retained (empirical). Panel (B): Heat map null model.
direct_ES<- read.csv("Data/Land_use_dir_ES_M1_M2.csv", sep =",")
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors


#Panel A
Prop<-direct_ES %>% group_by(management,services) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services) %>% 
  mutate(prop = tot/max(tot)) %>%  #Prop of NCP providers retained across habitat management
  select(management,services,tot,prop) %>% unique() 

perc_direct_ES<- Prop %>% group_by(management) %>% 
  summarise(perc_lost = (1 - mean(prop)) *100)

Panel_A<- Prop %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services)), size = 3.5, show.legend = T) +
  scale_fill_manual(values = color_services$color[match(levels(factor(Prop$services)), color_services$services)]) +
  scale_y_continuous(name = "Prop. of NCP providers retained", limits = c(0, 1)) + 
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
         labs(fill = "NCP")

Panel_A

  
#Panel B
dir_ES_z_score<-read.csv("Data/z_score_dir_ES_CP_M1_M2_A2.csv")

z_score_dir<- dir_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensive and bird watching and seed dispersal for IN (all birds went extinct so there were no z scores)
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
z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors

dir_ES_z_score<- dir_ES_z_score %>% rename("Output" = "signif")

z_score_tot$Output <- factor(z_score_tot$Output, levels = c("above", "below", "not signif", "Benchmark"))

Panel_B <- ggplot(z_score_tot, aes(management, services, fill = Output)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("dodgerblue2", "#E12E45", "ivory1", "ivory1"),
                    labels = c("Higher than random", "Lower than random", "No difference", "Benchmark")) +
  labs(x = 'Management', y = "Nature's contribution to people (NCP)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10,
                                   color = color_services$color[match(levels(z_score_tot$services), color_services$services)], face = "bold"),
        axis.text = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        axis.line = element_blank(),
        legend.title = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 16, b = 5, l = 5),
        legend.margin = margin(t = 5, r = 16, b = 5, l = 5)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5,
                   y = as.numeric(services) - 0.5,
                   xend = as.numeric(management) + 0.5,
                   yend = as.numeric(services) + 0.5),
               color = "black", size = 0.5) +
  geom_text(data = filter(dir_ES_z_score, Output %in% c("below","above")),
            aes(label = round(dir_shuff_mean,3)),
            size = 3.5, color = "black", vjust = -0.5) +
  geom_text(data = filter(dir_ES_z_score, Output %in% c("below","above")),
            aes(label = paste0("(", round(dir_shuff_sd,3),")")),
            size = 3, color = "black", vjust = 1.5)

Panel_B


# Figure 3_AB (panels AB_together). Put manually the figures of the farms
pdf("Graphs/Figure_3_pre_final_MI_M2_panelAB.pdf", width = 9, height = 5)
upper_row<- plot_grid(Panel_A + theme(plot.margin = unit(c(0.1,0.1, 0.1,0.1), "cm")),
                      Panel_B + theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")), 
                      ncol = 2, labels = c('(A)', "(B)"),
                      label_x = c(-0.033, 0),  
                      rel_widths = c(0.9, 1.1), 
                      align = "h",
                      axis = "b"
)
upper_row

dev.off()


### Panel (C): Prop. of indirect effects on NCP provision (empirical). Panel (D): Heat map null model.

output_ind_ES <- read.csv("Data/Land_use_ind_ES_M1_M2.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors


#Panel C
Prop_ind<-output_ind_ES %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of indirect effects on NCP provision retained
  select(management,services_to,tot,prop) %>% unique()

perc_indirect_ES<- Prop_ind %>% group_by(management) %>% 
  summarise(perc_lost = (1 - mean(prop)) *100)

Panel_C<- Prop_ind %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0.5), 
             pch=21, aes(fill=factor(services_to)), size = 3.5, show.legend = T) +
  scale_fill_manual(values = color_services$color) + 
  scale_y_continuous(name = "Prop. of indirect effects on NCP provision retained", limits = c(0, 1)) + 
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
  labs(fill = "NCP")

Panel_C



#Panel D
indir_ES_z_score<-read.csv("Data/z_score_ind_ES_CP_M1_M2_A2.csv", sep =",") %>% rename ("services" = "services_to") %>%
  mutate(management = ifelse(management == "IM", "IN", management))

# Prepare dataframe
z_score_ind<- indir_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensice and bird watching and seed dispersal for IN (all birds went extinct so there were no z scores)
sd_bw<-data.frame(management = c("E","E","E","E","E","E","E"), 
                  services = c("Bird watching", "Butterfly watching", 
                               "Crop damage", "Crop production","Pest control",
                               "Pollination", "Seed dispersal"),
                  z = c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),
                  signif = c("Benchmark","Benchmark","Benchmark","Benchmark",
                             "Benchmark","Benchmark","Benchmark"))

z_score_tot<- rbind (z_score_ind, sd_bw) %>% rename("Output" = "signif")

z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors
z_score_tot$services <- factor(z_score_tot$services, levels = c("Seed dispersal", "Pollination","Pest control",
                                                                "Crop production", "Crop damage",
                                                                "Butterfly watching","Bird watching"))

#Plot
indir_ES_z_score<- indir_ES_z_score %>% rename("Output" = "signif")


Panel_D<- ggplot(z_score_tot, aes(management, services, fill= Output)) + 
  geom_tile(color = "black")+
  scale_fill_manual(values = c("dodgerblue3","#E12E45","ivory1","ivory1"),
                    labels = c("Greater than random",
                               "Lower than random", 
                               "No difference",
                               "Benchmark"))+
  labs(x='Management', y="Nature's contribution to people (NCP)")+
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

Panel_D


# Figure 3 (panels C and D together). Put manually the figures of the farms
pdf("Graphs/Figure_3b_pre_final_MI_M2_panelCD.pdf", width = 9, height = 5)
upper_row<- plot_grid(Panel_C + theme(plot.margin = unit(c(0.8,0.1, 0.1,0.1), "cm")),
                      Panel_D + theme(plot.margin = unit(c(0.8,0.1,0.1,0.5), "cm")), 
                      ncol = 2, labels = c('(A)', "(B)"),
                      label_x = c(-0.02, 0),
                      rel_widths = c(0.9, 1.1), 
                      align = "h", 
                      axis = "b")
upper_row

dev.off()







##############  --  Figure 4
#Panel (A): Relative change in the amount of NCP provision (empirical). Panel (B): Heat map null model.
direct_ES<- read.csv("Data/Land_use_dir_ES_M1_M2.csv", sep =",")
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors


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

#amount the rest NCPs
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
  scale_fill_manual(values = color_services$color) + 
  scale_y_continuous(name = "Relative change in the amount \n of NCP provision", limits = c(0, 3)) +
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
  labs(fill = "NCP")


Panel_A



#Panel B
amount_ES_z_score<-read.csv("Data/z_score_amount_ES_CP_M1_M2_A2.csv")


# Prepare dataframe
z_score_amount<- amount_ES_z_score %>% select(management,services,z,signif)

#Add row showing the extensive and bird watching and seed dispersal for IN (all birds went extinct so there were no z scores)
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
z_score_tot$management <- factor(z_score_tot$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors
z_score_tot$Output <- factor(z_score_tot$Output, levels = c("above", "below", "not signif", "Benchmark"))
amount_ES_z_score<- amount_ES_z_score %>% rename("Output" = "signif")

amount_ES_z_score$management <- factor(amount_ES_z_score$management, levels = c("E", "SE", "M", "SI", "I", "IN"))
amount_ES_z_score$services   <- factor(amount_ES_z_score$services,   levels = levels(z_score_tot$services))


Panel_B <- ggplot(z_score_tot, aes(management, services, fill = Output)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("above" = "dodgerblue2", "below" = "#E12E45",
                               "not signif" = "ivory1", "Benchmark" = "ivory1"),
                    labels = c("above" = "Higher than random", "below" = "Lower than random",
                               "not signif" = "No difference", "Benchmark" = "Benchmark")) +
  labs(x = 'Management', y = "Nature's contribution to people (NCP)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10,
                                   color = color_services$color[match(levels(z_score_tot$services), color_services$services)], face = "bold"),
        axis.text = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        axis.line = element_blank(),
        legend.title = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 9),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.margin = margin(t = 5, r = 16, b = 5, l = 5),
        legend.margin = margin(t = 5, r = 16, b = 5, l = 5)) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  geom_segment(data = filter(z_score_tot, Output == "Benchmark"),
               aes(x = as.numeric(management) - 0.5,
                   y = as.numeric(services) - 0.5,
                   xend = as.numeric(management) + 0.5,
                   yend = as.numeric(services) + 0.5),
               color = "black", size = 0.5) +
  geom_text(data = filter(amount_ES_z_score, Output %in% c("below", "above")),
            aes(x = management, y = services, label = round(amount_shuff_mean, 3)),
            size = 3.5, color = "black", vjust = -0.5) +
  geom_text(data = filter(amount_ES_z_score, Output %in% c("below", "above")),
            aes(x = management, y = services, label = paste0("(", round(amount_shuff_sd, 3), ")")),
            size = 3, color = "black", vjust = 1.5)

Panel_B

# Figure 4 (all panels together). Put manually the figures of the farms
pdf("Graphs/Figure_4_pre_final_M1_M2.pdf", width = 9, height = 5)
upper_row<- plot_grid(Panel_A + theme(plot.margin = unit(c(0.1,0.1, 0.1,0.1), "cm")),
                      Panel_B + theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")), 
                      ncol = 2, labels = c('(A)', "(B)"),
                      label_x = c(-0.033, 0),  
                      rel_widths = c(0.9, 1.1), 
                      align = "h",  
                      axis = "b")
upper_row
dev.off()






##############  --  Figure 5
#Panel (A): Indirect effects on NCP (general pattern). Panel (B): Top 5. Because of
#the circular plot, we combined them manually.


## Panel A

## upload and arrange dataframe
short_path_land_change<-read.csv("Data/Land_use_shortpath_M1_M2.csv", row.names = 1) 

short_path_land_change_ave<- short_path_land_change %>% group_by(management,node_id) %>% 
  mutate(short_path_ave = mean(short_ave)) %>% select(-services, - short_ave) %>% unique() %>%  #calculate average short path of each species to all NCP in each habitat management
  mutate(taxon = str_replace(taxon, "Flower-visiting", "Flower visitor")) %>% 
           mutate(taxon = str_replace(taxon, "Plant", "Non-cultivated plant"))
                  
ave_management_taxon<-short_path_land_change_ave %>% group_by(management,taxon) %>% 
  summarise(ave_short = mean(short_path_ave),
            sd_short = sd(short_path_ave)) 

ave_management_taxon$management <- factor(ave_management_taxon$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors

ave_management_taxon$taxon<-as.factor(ave_management_taxon$taxon)

#Plot
pdf("Graphs/Figure_5_panel_A_M1_M2.pdf", width = 8, height = 6.5)
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
        legend.key.width = unit(0.6, "cm"),
        legend.key = element_rect(fill = "grey92", color = NA)) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 3))

Panel_A

dev.off()



### Panel B (circular plot)
#Just the ring, the label for every trophic guild goes manuall

## Prepare and arrange dataframe
# Check the 5 most important species per trophic group in Extensive (functional modulators)
top_5_taxon_extensive<-short_path_land_change_ave %>%
  filter(management == "E") %>% group_by(management, taxon) %>% 
  arrange(short_path_ave) %>% # Arrange by short_path_ave within each group
  slice_head(n = 5) # Take the first 5 rows within each group

#Filter the importance of the top 5 species (from the extensive) across management scenarios
top_5_average<- short_path_land_change_ave %>%  filter(node_id%in%top_5_taxon_extensive$node_id)

## Plot

# Define color of each layer and sps
top_5_average$management <- factor(top_5_average$management, levels = c("E", "SE", "M", "SI","I","IN")) #change order of factors
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

# Only need taxon (not color) since row labels stay black 
species_list <- top_5_ave %>% select(node_id,taxon)

#Plotting
circos.clear()
pdf("Graphs/Figure_5_panel_B_M1_M2.pdf", width = 4.5, height = 5)

# Extensive
E<- top_5_ave_values[,1,drop=FALSE]
circos.par(start.degree = 10, gap.degree = 1)
circos.heatmap(E, col = color, #rownames.side = "outside", rownames.col= species_list_color$color,
               rownames.cex = 0.7, track.height = 0.11, cell.border = "black",cluster = FALSE,
               split = factor(species_list$taxon, levels = unique(species_list$taxon)))

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
IN<- top_5_ave_values[,6, drop= FALSE]
circos.heatmap(IN, col = color, track.height = 0.11, cell.border = "black")

#Legend
lgd_mult = Legend(col_fun = color ,
                  legend_gp = gpar(col = 1), labels_gp = gpar(fontsize = 9),  title_position = "topleft", title = "Shortest path", direction = "horizontal",
                  grid_height = unit(0.5,"cm"),  grid_width = unit(0.7,"cm"),title_gp = gpar(fontsize = 10))
draw(lgd_mult, x = unit(15, "mm"), y = unit(10, "mm"), 
     just = c("bottom"))

dev.off()





##############  --  Figures Supplementary


##############  --  Figure S1
#Relative percentage of species belonging to each trophic guild

# Prepare dataframe
state_nodes <- read.csv("Data/Land_use_state_nodes_M1_M2.csv") %>%
  filter(management == "E") %>%
  distinct(node_id, taxon) %>%                     # one row per unique species
  count(taxon, name = "n_species") %>%              # total species per trophic guild
  mutate(rel_perc = n_species / sum(n_species) * 100)

# Change the name 'Plant' for "Non-cultivated plant"
state_nodes <- state_nodes %>%
  mutate(taxon = ifelse(taxon == "Plant", "Non-cultivated plant", taxon)) %>%
  mutate(taxon = ifelse(taxon == "Flower-visiting", "Flower visitor", taxon))


# Order factor alphabetically
state_nodes$taxon <- factor(state_nodes$taxon, levels = sort(color_trophic$taxon))

pdf("Graphs/Figure_S1_M1_M2.pdf", width = 9, height = 7) #to add drawings

Fig_S1 <- ggplot(state_nodes, aes(x = taxon, y = rel_perc, fill = taxon)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n_species), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = color_trophic$color[match(levels(state_nodes$taxon), color_trophic$taxon)]) +
  labs(x = "Trophic guild", y = "Relative percentage of trophic guilds", fill = "Trophic guild") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

Fig_S1

dev.off()



##############  --  Figure S3

# Percentage of species remaining per trophic guild, along the land-conversion gradient

## Prepare dataframe
state_nodes <- read.csv("Data/Land_use_state_nodes_M1_M2.csv") %>%
  mutate(taxon = str_replace(taxon, "Plant", "Non-cultivated plant")) %>%
  mutate(taxon = str_replace(taxon, "Flower-visiting", "Flower visitor")) %>%
  filter(taxon != "Crop")  # crop richness is fixed by design, not informative here

sps_remaining <- state_nodes %>%
  group_by(management, taxon) %>%
  summarise(n_sps = n_distinct(node_id), .groups = "drop") %>%
  complete(management, taxon, fill = list(n_sps = 0))

baseline <- sps_remaining %>%
  filter(management == "E") %>%
  select(taxon, n_sps_E = n_sps)

sps_remaining <- sps_remaining %>%
  left_join(baseline, by = "taxon") %>%
  mutate(pct_remaining = n_sps / n_sps_E * 100)

sps_remaining$management <- factor(sps_remaining$management, levels = c("E", "SE", "M", "SI", "I", "IN"))
sps_remaining$taxon <- as.factor(sps_remaining$taxon)


## Plot
pdf("Graphs/Figure_S3_pre_final_M1_M2.pdf", width = 9, height = 7) #to add drawings

Fig_S3 <- ggplot(sps_remaining, aes(x = management, y = pct_remaining, group = taxon, color = taxon)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = n_sps), vjust = -1, size = 3, color = "black", show.legend = FALSE) +
  scale_color_manual(values = color_trophic$color[match(levels(sps_remaining$taxon), color_trophic$taxon)]) +
  facet_wrap(~taxon, ncol = 4) +
  ylim(0, 110) +
  labs(x = "Land conversion", y = "Percentage of species remaining (%)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.spacing = unit(0.5, "cm", data = NULL),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey90", color = "black"),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        axis.line = element_blank(),
        legend.position = "none")
Fig_S3

dev.off()


##############  --  Figure S4 

#Proportion of 1st and 2nd order effects to NCP provision retained after land conversion

## Load data
I_ES2 <- read.csv("Data/Land_use_ind_ES_M1_M2.csv")
I_ES2$management <- factor(I_ES2$management, levels = c("E", "SE", "M", "SI", "I", "IN"))

palette_services_named <- setNames(color_services$color, color_services$services) 

## Panel A: 1st order effects retained 
Prop_1hop <- I_ES2 %>%
  filter(hop == 1) %>%
  group_by(management, services_to) %>%
  mutate(tot = n()) %>% ungroup() %>%
  group_by(services_to) %>%
  mutate(prop = tot / max(tot)) %>%
  select(management, services_to, tot, prop) %>% unique()

Panel_A <- Prop_1hop %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position = position_jitterdodge(jitter.width = 2, dodge.width = 0.5),
             pch = 21, aes(fill = factor(services_to)), size = 3, show.legend = TRUE) +
  scale_fill_manual(values = palette_services_named) +
  scale_y_continuous(name = "Prop. of 1st order effects retained", limits = c(0, 1)) +
  scale_x_discrete(name = "Land conversion") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        legend.position = "none") +
  labs(fill = "NCP")

## Panel B: 2nd order effects retained
Prop_2hop <- I_ES2 %>%
  filter(hop == 2) %>%
  group_by(management, services_to) %>%
  mutate(tot = n()) %>% ungroup() %>%
  group_by(services_to) %>%
  mutate(prop = tot / max(tot)) %>%
  select(management, services_to, tot, prop) %>% unique()

Panel_B <- Prop_2hop %>% ggplot(aes(x = management, y = prop)) +
  geom_boxplot(color = "black") +
  geom_point(position = position_jitterdodge(jitter.width = 2, dodge.width = 0.5),
             pch = 21, aes(fill = factor(services_to)), size = 3, show.legend = TRUE) +
  scale_fill_manual(values = palette_services_named) +
  scale_y_continuous(name = "Prop. of 2nd order effects retained", limits = c(0, 1)) +
  scale_x_discrete(name = "Land conversion") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        legend.position = "none") +
  labs(fill = "NCP")

## Combine panels 
library(cowplot)
Fig_S4 <- plot_grid(Panel_A, Panel_B, ncol = 2, labels = c("(A)", "(B)"), label_x = -0.02)

## Save 
pdf("Graphs/Sup_Fig_S4_pre_M1_M2.pdf", width = 9, height = 6)
print(Fig_S4)
dev.off()



##############  --  Figure S5

# Proportion of NCP providers retained after land conversion in the original simulation and null model

## ---- Load the direct-effects (PD_x) Z-score results (already computed) ----
dir_ES_z_score <- read.csv("Data/z_score_dir_ES_CP_M1_M2_A2.csv")
services_list <- unique(dir_ES_z_score$services)

## ---- Build plotting dataframe: Empirical vs Null side by side ----
empirical_plot <- dir_ES_z_score %>%
  select(management, services, Prop_mean) %>%
  mutate(type = "Empirical")

null_plot <- dir_ES_z_score %>%
  select(management, services, Prop_mean = dir_shuff_mean) %>%
  mutate(type = "Null")

## E baseline: only Empirical (no null comparison at E, nothing has been randomized yet)
e_baseline <- tibble(
  management = "E",
  services = services_list,
  Prop_mean = 1,
  type = "Empirical"
)

plot_data <- bind_rows(e_baseline, empirical_plot, null_plot)
plot_data$management <- factor(plot_data$management, levels = c("E", "SE", "M", "SI", "I", "IN"))
plot_data$type <- factor(plot_data$type, levels = c("Empirical", "Null"))

## ---- Plot ----
Sup_Fig_dir_ES_null <- ggplot(plot_data, aes(x = management, y = Prop_mean)) +
  geom_boxplot(aes(color = type), fill = NA, outlier.shape = NA, linewidth = 1,
               position = position_dodge(width = 0.6), width = 0.5) +
  geom_point(aes(fill = services, shape = type, group = type),
             position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0.08),
             size = 3.2, stroke = 1, color = "black") +
  scale_color_manual(values = c(Empirical = "black", Null = "firebrick"), name = "Type") +
  scale_shape_manual(values = c(Empirical = 21, Null = 24), name = "Type ") +
  scale_fill_manual(values = color_services$color[match(levels(factor(plot_data$services)), color_services$services)], name = "NCP") +
  labs(x = "Land conversion", y = "Prop. of NCP providers retained") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 17, color = "black"),
        axis.line = element_blank(),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.box = "vertical")


## ---- Save ----
pdf("Graphs/Sup_Fig_dir_ES_null_pre_M1_M2.pdf", width = 9, height = 6)
print(Sup_Fig_dir_ES_null)
dev.off()



##############  --  Figure S6

# Relative change in the amount of NCP provision after land conversion in the original simulation and null model.

## ---- Load Amount (A_x) Z-score results (already computed) ----
amount_ES_z_score <- read.csv("Data/z_score_amount_ES_CP_M1_M2_A2.csv")

services_list <- unique(amount_ES_z_score$services)

## ---- Build plotting dataframe: Empirical vs Null side by side ----
empirical_plot <- amount_ES_z_score %>%
  select(management, services, Prop_mean = ratio_change) %>%
  mutate(type = "Empirical")

null_plot <- amount_ES_z_score %>%
  select(management, services, Prop_mean = amount_shuff_mean) %>%
  mutate(type = "Null")

## E baseline: only Empirical (ratio_change = 1 by definition at E, no null comparison yet)
e_baseline <- tibble(
  management = "E",
  services = services_list,
  Prop_mean = 1,
  type = "Empirical"
)

plot_data <- bind_rows(e_baseline, empirical_plot, null_plot)
plot_data$management <- factor(plot_data$management, levels = c("E", "SE", "M", "SI", "I", "IN"))
plot_data$type <- factor(plot_data$type, levels = c("Empirical", "Null"))

## ---- Plot ----
Sup_Fig_amount_ES_null <- ggplot(plot_data, aes(x = management, y = Prop_mean)) +
  geom_boxplot(aes(color = type), fill = NA, outlier.shape = NA, linewidth = 1,
               position = position_dodge(width = 0.6), width = 0.5) +
  geom_point(aes(fill = services, shape = type, group = type),
             position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0.08),
             size = 3.2, stroke = 1, color = "black") +
  scale_color_manual(values = c(Empirical = "black", Null = "firebrick"), name = "Type") +
  scale_shape_manual(values = c(Empirical = 21, Null = 24), name = "Type ") +
  scale_fill_manual(values = color_services$color[match(levels(factor(plot_data$services)), color_services$services)], name = "NCP") +
  labs(x = "Land conversion", y = "Relative change in amount of NCP provision") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 17, color = "black"),
        axis.line = element_blank(),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.box = "vertical")

## ---- Save ----
pdf("Graphs/Sup_Fig_amount_ES_null_pre_M1_M2.pdf", width = 9, height = 6)
print(Sup_Fig_amount_ES_null)
dev.off()



##############  --  Figure S7

# Proportion of indirect effects on NCP provision retained after land conversion in the original simulation and null model.

## ---- Load Prop of indirect effect on NCP retained (PI_x) Z-score results (already computed) ----
indir_ES_z_score <- read.csv("Data/z_score_ind_ES_CP_M1_M2_A2.csv")
services_list <- unique(indir_ES_z_score$services_to)

## ---- Build plotting dataframe: Empirical vs Null side by side ----
empirical_plot <- indir_ES_z_score %>%
  select(management, services_to, Prop_mean) %>%
  mutate(type = "Empirical")

null_plot <- indir_ES_z_score %>%
  select(management, services_to, Prop_mean = ind_shuff_mean) %>%
  mutate(type = "Null")

## E baseline: only Empirical (no null comparison at E, nothing has been randomized yet)
e_baseline <- tibble(
  management = "E",
  services_to = services_list,
  Prop_mean = 1,
  type = "Empirical"
)

plot_data <- bind_rows(e_baseline, empirical_plot, null_plot)
plot_data$management <- factor(plot_data$management, levels = c("E", "SE", "M", "SI", "I", "IN"))
plot_data$type <- factor(plot_data$type, levels = c("Empirical", "Null"))

## ---- Plot ----
Sup_Fig_ind_ES_null <- ggplot(plot_data, aes(x = management, y = Prop_mean)) +
  geom_boxplot(aes(color = type), fill = NA, outlier.shape = NA, linewidth = 1,
               position = position_dodge(width = 0.6), width = 0.5) +
  geom_point(aes(fill = services_to, shape = type, group = type),
             position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0.08),
             size = 3.2, stroke = 1, color = "black") +
  scale_color_manual(values = c(Empirical = "black", Null = "firebrick"), name = "Type") +
  scale_shape_manual(values = c(Empirical = 21, Null = 24), name = "Type ") +
  scale_fill_manual(values = color_services$color[match(levels(factor(plot_data$services_to)), color_services$services)], name = "NCP") +
  labs(x = "Land conversion", y = "Prop. of indirect effects on NCP provision retained") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 17, color = "black"),
        axis.line = element_blank(),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.box = "vertical")

pdf("Graphs/Sup_Fig_ind_ES_null_pre_M1_M2.pdf", width = 9, height = 6)
print(Sup_Fig_ind_ES_null)
dev.off()



##############  --  Figure S8

# Percentage of indirect effects on NCP providers mediated by trophic guild

## Prepare dataframe: count how often each trophic guild's species act as
## intermediate (mediating) nodes in indirect effects, for the Extensive (E) network only

## Load combined indirect-effects dataframe (1-hop + 2-hop)
Norwood_farm <- readRDS("Data/Norwood_farm.RData")
nodes <- Norwood_farm$nodes %>% 
  mutate(taxon = str_replace(taxon, "Flower-visiting", "Flower visitor"))

I_ES2 <- read.csv("Data/Land_use_ind_ES_M1_M2.csv")

## Only 2-hop paths have a true intermediate/mediating node (node_int).
## 1-hop paths have node_int = NA, so filter to hop == 2.
mediators_E <- I_ES2 %>%
  filter(management == "E", hop == 2) %>%
  select(node_int) %>%
  left_join(nodes %>% select(node_id, taxon), by = c("node_int" = "node_id")) %>%
  mutate(taxon = str_replace(taxon, "Plant", "Non-cultivated plant")) %>%
  count(taxon, name = "n_mediating") %>%
  mutate(rel_perc = n_mediating / sum(n_mediating) * 100)

mediators_E$taxon <- as.factor(mediators_E$taxon)


# Order bars alphabetically for display; color still matched correctly by name
mediators_E$taxon <- factor(mediators_E$taxon, levels = sort(color_trophic$taxon))

# Build a named color vector: names = taxon, values = color
palette_named <- setNames(color_trophic$color, color_trophic$taxon)



## Plot
Fig_S8 <- ggplot(mediators_E, aes(x = taxon, y = rel_perc, fill = taxon)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n_mediating), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = palette_named)  +
  labs(x = "Trophic guild",
       y = "Percentage of indirect effects on NCP\nmediated by trophic guilds",
       fill = "Trophic guild") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

Fig_S8

pdf("Graphs/Sup_Fig_ind_effect_mediated_trophic.pdf", width = 9, height = 6)
print(Fig_S8)
dev.off()

