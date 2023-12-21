# Plotting the socio-ecological network

rm(list=ls())
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

library(magrittr); library(tidyverse); library(igraph); library(ggnetwork)

# Read in and manipulate the data
nodes <- read.csv("Data/nodes.csv")
habitat_links <- read.csv("Data/habitat_edgelist.csv")
service_links <- read.csv("Data/service_edgelist.csv")
ecological_links <- read.csv("Data/species_edgelist.csv")
species_links <- ecological_links %>% 
                    group_by(lower, upper) %>% 
                      summarise(weight = sum(weight))
merged_links <- bind_rows(species_links, habitat_links, service_links)
se_network <- graph_from_data_frame(merged_links,
                                    vertices = nodes, directed = TRUE)

# Create some x coordinates to use in plotting later
nodes$x.coord <- 1
nodes[nodes$type == 1, "x.coord"] <- seq(0, 1,
                                         length.out = (sum(nodes$type == 1)))
nodes[nodes$type == 2, "x.coord"] <- seq(0, 1,
                                         length.out = (sum(nodes$type == 2)))
nodes[nodes$type == 3, "x.coord"] <- seq(0, 1,
                                         length.out = (sum(nodes$type == 3)))
nodes[nodes$type == 4, "x.coord"] <- seq(0, 1,
                                         length.out = (sum(nodes$type == 4)))
nodes[nodes$type == 5, "x.coord"] <- seq(0, 1,
                                         length.out = (sum(nodes$type == 5)))

# Extract a layout
net <- ggnetwork::fortify(se_network, 
                          layout = as.matrix(nodes[, c("x.coord", "type")]))

# Change the colours of the links 
net$color <- net$group
test <- net[!is.na(net$weight), ]

test[test$y == 0, "color"] <- "darkblue"
test[test$weight < 1, "color"] <- "black"
test[test$weight > 1, "color"] <- "black"
test[test$yend > 0.9 & test$xend > 0.9, "color"] <- "darkred"
test[test$yend > 0.9 & test$xend < 0.9, "color"] <- "darkgreen"

net[!is.na(net$weight), ] <- test

net[net$y == 0, "color"] <- "darkblue"
net[net$weight < 1, "color"] <- "black"
net[net$weight > 1, "color"] <- "black"
net[net$color == "service", "color"] <- "darkgreen"
net[net$yend > 0.9, "color"] <- "darkred"


net[net$color == "species" && net$yend < 0.8 && net$xend < 0.8,
    "color"] <- "darkgreen"
net[net$color == "habitat", "color"] <- "darkblue"

# Adjust as necessary for plotting
nodes$height <- (nodes$type - 1) / 4
nodes$test <- factor(nodes$taxon, levels = unique(nodes$taxon))
nodes[nodes$height == 0.25, "height"] <- nodes[nodes$height == 0.25, "height"] - 0.01
nodes[nodes$height == 0.5, "height"] <- nodes[nodes$height == 0.5, "height"] - 0.01
nodes[nodes$height == 0.75, "height"] <- nodes[nodes$height == 0.75, "height"] - 0.01
nodes[nodes$height == 1, "height"] <- nodes[nodes$height == 1, "height"] - 0.01

# Plotting the network
plot <- ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(color = color), alpha = 0.25) +
  geom_point(data = nodes,
   aes(x = x.coord, y = height, shape = group, fill = test, size = size),
   inherit.aes = FALSE) +
  scale_shape_manual(guide = "none", values = c(25, 22, 24, 21)) +
  scale_color_manual(guid = "none", values = c("black", "darkblue", "darkgreen", "darkred")) + 
  scale_size_manual(guide = "none", values = c(10,8)) +
  scale_fill_manual(name = "", guide = "none",
    values = c("green", "yellowgreen", "yellow",
               "lightblue1", "royalblue", "royalblue4",
               "sienna1", "tomato3", "orchid", "red",
               "orange", "red4", "deeppink", "navy",
               "darkgreen", "orangered3")) +
  #guides(fill = guide_legend(override.aes = list(shape = c(rep(21,13),22,24,25)))) + 
  theme_blank() +
  coord_flip()
plot

## MAKE LINKS DIFFERENT COLOURS (use the net object and taxon == Habitat <- "blue")

tiff(file = "Norwood_sen.tiff", width = 300, height = 200, res = 600, units = "mm")
plot
dev.off()
