############################## Script to create figure of ecological networks ################################

library(igraph)
library(tidyverse)
library(ggtext)
library(cowplot)
setwd("/Users/agustin/Desktop/Papers/Norwood_farm/Norwood_Tinio")



####### Figure 2: 
#Panel (A): Prop. of direct ES retained (empirical). Panel (B): Heat map null model.

direct_ES<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",")
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

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

# Figure 2 (all panels together). Put manually the figures of the farms

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




####### Figure 3: 
#Panel (A): Relative change in the amount of ES (empirical). Panel (B): Heat map null model.
direct_ES<- read.csv("Data/Land_use_dir_weighted_CP_intense.csv", sep =",")
direct_ES$management <- factor(direct_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

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

# Figure 3 (all panels together). Put manually the figures of the farms

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




####### Figure 4: 
#Panel (A): Prop. of indirect effects on ES retained (empirical). Panel (B): Heat map null model.
output_ind_ES <- read.csv("Data/Land_use_output_weighted_CP_intense.csv", sep =",")
output_ind_ES$management <- factor(output_ind_ES$management, levels = c("E", "SE", "M", "SI","I","IM")) #change order of factors

color_services_prop <-tibble(
  services = unique(output_ind_ES$services_to),
  color = c('#1b9e77','#d95f02','#7570b3','#e7298a','#2c7fb8','#e6ab02','#a6761d'))


#Panel A
Prop_ind<-output_ind_ES %>% group_by(management,services_to) %>% 
  mutate(tot = n()) %>% ungroup() %>%  
  group_by(services_to) %>% 
  mutate(prop = tot/max(tot)) %>%  #prop of E(D)S rtained across habitat management
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


#



