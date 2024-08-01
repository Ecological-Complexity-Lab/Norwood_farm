## Previous version

We reclassified the habitats:

* Move Lucerne (LU) into Crop production (CP).
* Clean the abundances of crops in the data.
* Change the name CSF -> SF

  

# Assumptions:

Here we reclassified the species in a different way:

1. Species (birds, rodents, seed inv, and aphids) are classified as pest when interact with crops and potentially provide direct crop damage. Pests indirectly will reduce crop production (EDS) when interacting with crops. Otherwise, they are classified as birds, rodents, seed inv, and aphids. We did it for birds according to the literature.
   
2. Species (parasitoids and parasites) will potentially provide direct pest control only when they interact with pests. They indirectly will reduce crop damage when interact with (hence, provide an ES) when interacting with crops.

1. Assume no competition between crops and weeds for pollinators because most crops are self pollinated.
2. Assume no competition between pollinators for resources because there are plenty plant species (weeds) providing resources.
3. Assume no competition between crops and weeds for resources.


# Classification of pollinators

* Reclassify the group flower visitors as:
  1) 02FV (Flower visitors): species that were recorded transporting pollen grains (mainly bumblebees, bees, hoverflies, butterflies). We assume they directly provide pollination.
  2) 15FVOTHER (Other flower visitors): species that were not recorded transporting pollen grains (mosquitos, beetles). We assume they don't directly provide pollination.

To reclassify species, we used two databases: DoPI repository (doi: 10.1002/ecy.3801) and database from Orford et al 2015 (https://doi.org/10.1098/rspb.2014.2934). 

## What's new?
We include a new management scenario: intensive non-organic crop management. This would help us simulate management approaches used to produce food in monoculture countries such as Argentina and Brazil, which typically cultivate a single crop species over large areas.


# Estimation of the amount of ESs
We estimate the amount of ESs provided according to the type of ES.  For butterfly and bird watching, we used abundance alone, while for the other categories, we used the product of abundance and biomass.