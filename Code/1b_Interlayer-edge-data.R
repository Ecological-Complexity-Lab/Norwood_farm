## Multilayer analysis of the Norwood Farm Network ##


## 1 - Interlayer edges


#### Setup ####
rm(list=ls())
setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")

source("Code/1a_Intralayer-edge-data.R") #modified from the original version




#### Interlayer edges #### 

# Read in data on plant density for each habitat
pl <- read.csv("Data/Plant_data.csv")
fv <- read.csv("Data/Flowervis_data.csv")
apara <- read.csv("Data/Aphidpara_data.csv")
aphid <- read.csv("Data/Aphid_data.csv")
minerpara <- read.csv("Data/Minerpara_data.csv")
sf <- read.csv("Data/Seedfeeder_data.csv")

# Get sum values for estimates (same as for interactions on Line 45-49)
pl_density <- pl %>% 
  group_by(Habitat, Plant.sp) %>% 
  summarise(density=sum(Density)/length(Density)) %>%
  set_colnames(c("Habitat", "Species", "Density")) %>%
  subset(Species != "")
pl_density$Species <- paste("01PLANT", pl_density$Species, sep = ".")

# Flower visitor densities 
fv_density <- fv %>% 
  group_by(Habitat, flowervisitor) %>% 
  summarise(density=sum(SubsamplingMultipFactor)/length(SubsamplingMultipFactor)) %>%
  set_colnames(c("Habitat", "Species", "Density"))
fv_density$Species <- paste("02FV", fv_density$Species, sep = ".")
fv_density$Density_norm <- fv_density$Density/sum(fv_density$Density)

# Aphid primary parasitoid densities
apara_pri <- subset(apara, Notes == "primary")
apara_pri_density <- apara_pri %>% 
  group_by(Habitat, INVERT_SpecimenID.Invertsp_Michael) %>% 
  summarise(density=sum(DensityOfParasPerM2)/length(DensityOfParasPerM2)) %>%
  set_colnames(c("Habitat", "Species", "Density"))
apara_pri_density$Species <- paste("04PRIMARYPARA", apara_pri_density$Species, sep = ".")
apara_pri_density$Density_norm <- apara_pri_density$Density/sum(apara_pri_density$Density)

# Aphid secondary parasitoid densities
apara_sec <- subset(apara, Notes != "primary")
apara_sec_density <- apara_sec %>% 
  group_by(Habitat, INVERT_SpecimenID.Invertsp_Michael) %>% 
  summarise(density=sum(DensityOfParasPerM2)/length(DensityOfParasPerM2)) %>%
  set_colnames(c("Habitat", "Species", "Density"))
apara_sec_density$Species <- paste("05SECONDARYPARA", apara_sec_density$Species, sep = ".")
apara_sec_density$Density_norm <- apara_sec_density$Density/sum(apara_sec_density$Density)

# Aphid densities
aphid_density <- aphid %>% 
  group_by(Habitat, Invertsp_Michael) %>% 
  summarise(density=sum(DensityOfAphidsPerM2)/length(DensityOfAphidsPerM2)) %>%
  set_colnames(c("Habitat", "Species", "Density"))
aphid_density$Species <- paste("03APH", aphid_density$Species, sep = ".")
aphid_density$Density_norm <- aphid_density$Density/sum(aphid_density$Density)

minerpara_density <- minerpara %>% 
  group_by(Habitat, Invertsp_Michael) %>% 
  summarise(density=sum(DensityOfParasPerM2)/length(DensityOfParasPerM2)) %>%
  set_colnames(c("Habitat", "Species", "Density"))
minerpara_density$Species <- paste("06MINERPARA", minerpara_density$Species, sep = ".")
minerpara_density$Density_norm <- minerpara_density$Density/sum(minerpara_density$Density)


taxon_density <- bind_rows(fv_density, apara_pri_density, apara_sec_density, aphid_density, minerpara_density)
taxon_density_clean <- subset(taxon_density, Habitat != "FH")
taxon_density_clean <- subset(taxon_density_clean, Habitat != "")
taxon_density_clean <- subset(taxon_density_clean, Species != "")


# Split out into the 12 habitats
taxon_density_list <- split(taxon_density_clean, taxon_density_clean$Habitat)

# Get the dispersal matrix, for upper nodes only
intralayer_edges_st <- bind_rows(edgelist_list_st, .id = "habitat"); colnames(intralayer_edges_st) <- c("habitat", "lower", "upper", "weight")
dispersal <- intralayer_edges_st %>% group_by(upper) %>% dplyr::select(habitat) %>% table()
dispersal <- 1*(dispersal>0)

# Calculate the interlayer edges
interlayer_edges_rough<-NULL
for (s in rownames(dispersal)){ 
  x <- dispersal[s,]
  print(s)
  locations <- names(which(x!=0)) # locations where upper nodes occurs
  if (length(locations)<2){next}
  pairwise <- combn(locations, 2)
  # Create interlayer edges between pairwise combinations of locations
  for (i in 1:ncol(pairwise)){
    a <- pairwise[1,i]
    b <- pairwise[2,i]
    
    r1 <- which(taxon_density_list[[a]]$Species == s)
    r2 <- which(taxon_density_list[[b]]$Species == s)
    
    weight <- (as.numeric(taxon_density_list[[a]][r1,"Density_norm"]) / as.numeric(taxon_density_list[[a]][r1,"Density_norm"]) + as.numeric(taxon_density_list[[b]][r2,"Density_norm"])) - 1 # Get the interlayer edge weight (differences in density between layers)
    
    interlayer_edges_rough %<>% bind_rows(tibble(layer_from=a, node_from=s, layer_to=b, node_to=s, weight=weight))
  }
}

interlayer_edges <- subset(interlayer_edges_rough, weight != "NA")
hist(interlayer_edges$weight)
