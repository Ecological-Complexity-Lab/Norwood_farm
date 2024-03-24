## Previous version

We reclassified the habitats:

* Move Lucerne (LU) into Crop production (CP).
* Clean the abundances of crops in the data.
* Change the name CSF -> SF

  

# Assumptions:

Here we reclassified the species in a different way:

1. Species (birds, rodents, seed inv, and aphids) are classified as pest when interact with crops and potentially provide direct crop damage. Pests indirectly will reduce crop production (EDS) when interacting with crops. Otherwise, they are classified as birds, rodents, seed inv, and aphids.
   
2. Species (parasitoids and parasites) will potentially provide direct pest control only when they interact with pests. They indirectly will reduce crop damage when interact with (hence, provide an ES) when interacting with crops.

1. Assume no competition between crops and weeds for pollinators because most crops are self pollinated.
2. Assume no competition between pollinators for resources because there are plenty plant species (weeds) providing resources.
3. Assume no competition between crops and weeds for resources.


## What's new?

* Reclassify the group flower visitors as:
  1) FV (Flower visitors): species with high pollination potential (bumblebees, bees, hoverflies, butterflies). We assume they directly provide pollination.
  2) OFV (Other flower visitors): species with low/null potential as pollinators (mosquitos, beetles). We assume they don't directly provide pollination.
