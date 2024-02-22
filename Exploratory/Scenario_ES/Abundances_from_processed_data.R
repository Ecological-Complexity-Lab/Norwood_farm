
##### ESTIMATE ABUNDANCES ############

library(dplyr)
library(tidyr)

setwd("D:/Trabajo/Papers/Norwood_Farm/norwood-ecosystem-services-main_Tinio")


#Data frame 
plants<-read.csv("Data/PLANT_HABITAT3.csv",header=T) # abundance of plants
nore<-read.csv("Data/nore2.csv",header=T) #potential dataframe of abundances


##### Plants

# Rearrange dataframe 
plants_habitat<-plants %>% select(-ST) %>%  # remove standing trees (ST)
  rename ("WD" = "W", "SF" ="CSF", "CP" = "C")

plants_ab<-plants_habitat %>% gather("habitat","abundance",2:12) %>% 
  filter(abundance >0) %>% rename("species_name" = "lower")


##### Animals 
nore2<-nore

# plant / flower visitors
nore2.02FV <- nore2 %>%
  filter(substr(lower, 1, 4) == "01PL" & substr(upper, 1, 4) == "02FV")
# plant / aphid
nore2.03AP <- nore2 %>%
  filter(substr(lower, 1, 4) == "01PL" & substr(upper, 1, 4) == "03AP")
# aphid / primary parasitoid
nore2.04PR <- nore2 %>%
  filter(substr(lower, 1, 4) == "03AP" & substr(upper, 1, 4) == "04PR")
# aphid / secondary parasitoid
nore2.05SE <- nore2 %>%
  filter(substr(lower, 1, 4) == "03AP" & substr(upper, 1, 4) == "05SE")
# plant / leaf miner parasitoid
nore2.06MI <- nore2 %>%
  filter(substr(lower, 1, 4) == "01PL" & substr(upper, 1, 4) == "06MI")
# plant / invert seed feeder
nore2.07SE <- nore2 %>%
  filter(substr(lower, 1, 4) == "01PL" & substr(upper, 1, 4) == "07SE")
# plant / invert seed feeder
nore2.12BF <- nore2 %>%
  filter(substr(lower, 1, 4) == "01PL" & substr(upper, 1, 4) == "12BF")
# invert seed feeder / seed feeder parasitoid
nore2.13SF <- nore2 %>%
  filter(substr(lower, 1, 4) == "07SE" & substr(upper, 1, 4) == "13SF")

## the following 3 are based on inferred networks.
## be thoughtful whether you want to use them or not
# plant / bird seed feeder
nore2.08BI <- nore2 %>%
  filter(substr(lower, 1, 4) == "01PL" & substr(upper, 1, 4) == "08BI")
# plant / mammal seed feeder
nore2.09MA <- nore2 %>%
  filter(substr(lower, 1, 4) == "01PL" & substr(upper, 1, 4) == "09MA")
# mammal seed feeder / flea
nore2.14FL <- nore2 %>%
  filter(substr(lower, 1, 4) == "09MA" & substr(upper, 1, 4) == "14FL")

# now to get the abundances for any month and/or habitat,
# sum across the 'fortotals' column
# BUT NOTE that flowervisitors (nore2.02FV) were the only group that were sampled in years 1 and 2
# so, if you want abundances for flower visitors to be comparable to others
# you will need to sum and then halve those totals
# (or just use?: 
### nore2.02FV$fortotals <- nore2.02FV$fortotals/2
# before aggregating into the combined dataset

nore2a <- rbind(
  nore2.02FV,
  nore2.03AP,
  nore2.04PR,
  nore2.05SE,
  nore2.06LE,
  nore2.07SE,
  nore2.13SF,
  nore2.12BF,
  # be thoughtful if you want to include these other ones
  nore2.08BI,
  nore2.09MA,
  nore2.14FL)
)

nore2b <- nore2a %>%
  group_by(lower, upper) %>% 
  summarize (abundances = sum(fortotals))


nore2b.summary <- nore2b %>%
  group_by(substr(lower, 1, 4), substr(upper, 1, 4)) %>%
  summarize (counts = length(abundances))
# these numbers match with the Science paper dataset in Dryad
# here I only include the direct links between guilds
# the Dryad dataset includes indirect links between plants and guilds
# i.e. if aphid1 was on plant1 and plant2, but parasitoid1 only reared from aphid1 on plant2, 
# it means that the direct and indirect info could be important, depending on your analysis question
# so the spreadsheet 'info' sheet for more information
