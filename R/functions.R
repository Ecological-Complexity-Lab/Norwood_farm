# This file contains the functions used across the manuscript.


### -- Load libraries
library(tidyverse)


#===============================================================================
#                        LAND CONVERSION FUNCTIONS
#              (used in 4_Land_conversion_simulation.R)
#===============================================================================


##### 1. Copies CP's own interactions and relabels them as the replacement for one converted habitat
build_cp_habitat <- function(new_hab, prev_hab_code) {
  Norwood_farm$extended_ids %>% 
    filter(layer_from == 1) %>% 
    select(-layer_to, -layer_from) %>% 
    mutate(new_hab = new_hab, prev_hab = prev_hab_code, hab_cp = "CP")
}

##### 2. Computes the area ratio used to rescale CP's own species abundances
build_converted_area <- function(cp_habitat_table, habitat_area) {
  cp_habitat_table %>% 
    left_join(habitat_area, by = c("prev_hab" = "HabitatCode")) %>% 
    left_join(habitat_area, by = c("hab_cp" = "HabitatCode")) %>% 
    select(node_from, node_to, new_hab, prev_hab, hab_cp, area_ave.x, area_ave.y) %>% 
    rename("area_prev_hab" = "area_ave.x", "area_CP" = "area_ave.y") %>% 
    mutate(mult_ab = area_prev_hab / area_CP) %>% 
    select(-prev_hab, -hab_cp, -area_prev_hab, -area_CP)
}

##### 3. Apply that multiplier to CP's actual abundances to get the new habitat's abundance table. 
build_new_habitats_ab <- function(converted_area, abundances_CP) {
  converted_area %>%  
    left_join(abundances_CP, by = c("node_from" = "node_id")) %>%
    left_join(abundances_CP, by = c("node_to" = "node_id")) %>%
    rename("ab_node_from_CP" = "abundance.x", "taxon_node_from" = "taxon.x",
           "ab_node_to_CP" = "abundance.y", "taxon_node_to" = "taxon.y") %>% 
    mutate(ab_node_from = ab_node_from_CP * mult_ab, 
           ab_node_to = ab_node_to_CP * mult_ab) %>%
    select(new_hab, node_from, ab_node_from, taxon_node_from, node_to, ab_node_to, taxon_node_to) %>% 
    rename("habitat" = "new_hab")
}

##### 4. Keeps every habitat except the ones being replaced this round, with abundances/taxa attached.
remove_replaced_habitats <- function(replaced_layers) {
  Norwood_farm$extended_ids %>% 
    filter(!layer_from %in% replaced_layers) %>% 
    select(-layer_to) %>% rename("habitat" = "layer_from") %>%
    left_join(state_nodes_ab, by = c("node_from" = "node_id", "habitat" = "layer_id")) %>%
    left_join(state_nodes_ab, by = c("node_to" = "node_id", "habitat" = "layer_id")) %>%
    rename("ab_node_from" = "abundance.x", "taxon_node_from" = "taxon.x",
           "ab_node_to" = "abundance.y", "taxon_node_to" = "taxon.y") %>%
    select(habitat, node_from, node_to, ab_node_from, taxon_node_from, ab_node_to, taxon_node_to)
}



##### 5. Mechanism 1 — Rewiring
# This function applies interaction-based retention (rewiring) to species in replaced habitats.
# Species from the replaced habitat survive in the new CP habitat if they have at least one observed resource in CP that is viable (abundance >= 1 after area scaling), 
# detected anywhere in the multilayer network (bottom-up). 
# Their retained abundance is scaled by the proportion of interactions with viable CP resources relative to total interactions across the extensive farm baseline — weighting
# resources by how spatially widespread the interaction is across habitats, rather than treating all resource partners equally.

apply_rewiring <- function(new_habitats_ab, replaced_layer_ids, hab_id_map,
                           state_nodes_ab, total_resources_baseline,
                           species_in_CP, abundances_CP, metaweb) {
  
  # Step 1: Identify species from replaced habitats, keeping layer_id
  sp_replaced <- state_nodes_ab %>%
    filter(layer_id %in% replaced_layer_ids) %>%
    select(node_id, abundance, taxon, layer_id)
  
  # Step 1b: Compute rewiring_ratio using only CP species viable in the new habitat (ab >= 1)
  species_viable_CP <- new_habitats_ab %>%
    filter(ab_node_from >= 1) %>%
    pull(node_from) %>%
    unique()
  
  rewiring_ratio <- total_resources_baseline %>%
    left_join(
      metaweb %>%
        group_by(node_to) %>%
        summarise(cp_interactions = sum(node_from %in% species_viable_CP),
                  has_cp_resource = cp_interactions > 0) %>%
        rename("node_id" = "node_to"),
      by = "node_id") %>%
    mutate(rewiring_ratio = cp_interactions / total_interactions)
  
  # Step 2: Scale abundance per habitat, apply threshold per habitat
  sp_rewired <- sp_replaced %>%
    filter(!node_id %in% species_in_CP) %>%
    left_join(rewiring_ratio, by = "node_id") %>%
    filter(has_cp_resource == TRUE) %>%
    mutate(retained_ab = abundance * rewiring_ratio,
           new_hab = hab_id_map[as.character(layer_id)]) %>%
    filter(retained_ab >= 1) %>%
    select(node_id, taxon, retained_ab, new_hab)
  
  # Step 3: Build rewired interactions
  rewired_interactions <- metaweb %>%
    filter(node_to %in% sp_rewired$node_id,
           node_from %in% species_viable_CP) %>%
    left_join(sp_rewired,
              by = c("node_to" = "node_id"),
              relationship = "many-to-many") %>%
    rename("ab_node_to"    = "retained_ab",
           "taxon_node_to" = "taxon",
           "habitat"       = "new_hab") %>%
    left_join(new_habitats_ab %>%
                select(habitat, node_from, ab_node_from, taxon_node_from) %>%
                distinct(habitat, node_from, .keep_all = TRUE),
              by = c("habitat", "node_from")) %>%
    filter(!is.na(ab_node_from)) %>%
    select(habitat, node_from, ab_node_from, taxon_node_from,
           node_to, ab_node_to, taxon_node_to)
  
  # Step 4: Add rewired interactions to new_habitats_ab
  bind_rows(new_habitats_ab, rewired_interactions) %>%
    group_by(habitat, node_from, node_to) %>%
    summarise(
      ab_node_from = first(ab_node_from),
      taxon_node_from = first(taxon_node_from),
      ab_node_to = sum(ab_node_to),
      taxon_node_to = first(taxon_node_to),
      .groups = "drop")
}


##### 6. Mechanism 2 — Rescue by animal movement
# Species from replaced habitats that could not fully persist via M1 (rewiring) can disperse to any remaining habitat where they have at least one resource.
# Individuals are distributed proportionally by plant community similarity (Sørensen).
# A species establishes only if arriving + existing abundance >= threshold.
# Excluded groups (plants, crops, aphids, rodent ectoparasites) cannot disperse.

apply_rescue <- function(replaced_layer_ids,   # layer IDs being converted (e.g. c(8, 10) for WD, RG)
                         hab_id_map,            # maps replaced layer → new hab id (e.g. c("8"=11, "10"=12))
                         new_habitats_ab_rem,   # M1 output after ab >= 1 filter — to get retained abundances
                         state_nodes_ab,        # original state nodes: layer_id, node_id, abundance, taxon
                         destination_edgelist,  # edge list of valid destination habitats (remaining + prior new habs)
                         metaweb,               # full metaweb: node_from, node_to
                         excluded_taxa = c("Plant", "Crop", "Aphid", "Rodent ectoparasite", "Seed-feeding bird"), #birds move freely and it's considered in the abundance (see 1_Abundance)
                         threshold = 10) {
  
  ## 1. Build disperser pool
  # Two types of dispersers:
  #   Type A — species retained by M1 but with reduced abundance;
  #             available individuals = original abundance - retained abundance
  #   Type B — species with no CP resources (not retained by M1 at all);
  #             available individuals = full original abundance
  
  # All species present in replaced habitats (excluding non-dispersing groups)
  species_replaced <- state_nodes_ab %>%
    filter(layer_id %in% replaced_layer_ids,
           !taxon %in% excluded_taxa,
           abundance >= 1)
  
  # Abundance retained by M1: in new CP-based habitats, retained species appear as node_to
  new_hab_ids <- unname(hab_id_map)
  
  retained <- new_habitats_ab_rem %>%
    filter(habitat %in% new_hab_ids) %>%
    mutate(layer_id = as.integer(names(hab_id_map)[match(habitat, hab_id_map)])) %>%
    group_by(layer_id, node_to) %>%
    summarise(retained_ab = first(ab_node_to), .groups = "drop") %>%
    rename(node_id = node_to)
  
  # Calculate available individuals per species (Type A: partial; Type B: full original)
  dispersers <- species_replaced %>%
    left_join(retained, by = c("layer_id", "node_id")) %>%
    mutate(retained_ab = replace_na(retained_ab, 0),   # 0 for Type B (not retained at all)
           available   = abundance - retained_ab) %>%
    filter(available > 0) %>%
    select(layer_id, node_id, available, taxon)
  
  if (nrow(dispersers) == 0) return(list(rescue_edges = NULL, ab_increments = NULL))
  
  ## 2. Sørensen similarity: source habitats vs destination habitats
  # Plant presence/absence similarity determines how individuals are distributed across destination habitats. A habitat with more 
  #similar plant community to the source receives more dispersers.
  
  # Plant species present in replaced (source) habitats
  plants_source <- state_nodes_ab %>%
    filter(layer_id %in% replaced_layer_ids,
           taxon %in% c("Plant", "Crop"),
           abundance >= 1) %>%
    select(layer_id, node_id)
  
  # Plant species present in each destination habitat (after ab >= 1 filter)
  plants_dest <- destination_edgelist %>%
    filter(taxon_node_from %in% c("Plant", "Crop"), ab_node_from >= 1) %>%
    select(habitat, node_id = node_from) %>%
    distinct()
  
  # Classic Sørensen: 2|A∩B| / (|A| + |B|)
  sorensen_sim <- function(a, b) {
    if (length(a) == 0 || length(b) == 0) return(0)
    2 * length(intersect(a, b)) / (length(a) + length(b))
  }
  
  # Compute similarity for every source-destination habitat pair
  sim_grid <- expand.grid(layer_id = unique(dispersers$layer_id),
                          habitat  = unique(plants_dest$habitat),
                          stringsAsFactors = FALSE)
  sim_grid$sim <- mapply(function(lid, hab) {
    sorensen_sim(
      plants_source %>% filter(layer_id == lid) %>% pull(node_id),
      plants_dest   %>% filter(habitat  == hab) %>% pull(node_id)
    )
  }, sim_grid$layer_id, sim_grid$habitat)
  
  ## 3. Existing species abundance per destination habitat 
  # Needed for the establishment threshold: arriving + existing >= threshold.
  # Derived from both sides of the edge list since a species can appear as node_from (resource) or node_to (consumer).
  
  existing_ab <- bind_rows(
    destination_edgelist %>% select(habitat, node_id = node_from, ab = ab_node_from),
    destination_edgelist %>% select(habitat, node_id = node_to,   ab = ab_node_to)
  ) %>%
    group_by(habitat, node_id) %>%
    summarise(existing_ab = max(ab), .groups = "drop")
  
  ## 4. Distribute individuals and apply threshold 
  # For each disperser species:
  #   (a) find destination habitats with resources AND similarity > 0
  #   (b) distribute individuals proportionally to similarity weights
  #   (c) apply threshold: only establish if arriving + existing >= threshold
  #   (d) if new to habitat → add interactions; if already present → record increment
  rescue_new  <- list()  # interactions for newly arriving species
  rescue_incr <- list()  # abundance increments for already-present species
  
  for (i in seq_len(nrow(dispersers))) {
    sp       <- dispersers$node_id[i]
    src_lyr  <- dispersers$layer_id[i]
    avail    <- dispersers$available[i]
    sp_taxon <- dispersers$taxon[i]
    
    # Find destination habitats where at least one of sp's resources is present
    sp_resources <- metaweb %>% filter(node_to == sp) %>% pull(node_from)
    has_resource <- destination_edgelist %>%
      filter(node_from %in% sp_resources) %>%
      pull(habitat) %>% unique()
    
    valid <- sim_grid %>%
      filter(layer_id == src_lyr, habitat %in% has_resource, sim > 0)
    
    if (nrow(valid) == 0) next
    
    # Distribute individuals proportionally to similarity and apply threshold
    valid <- valid %>%
      mutate(weight   = sim / sum(sim),
             arriving = avail * weight) %>%
      left_join(existing_ab %>% filter(node_id == sp) %>% select(habitat, existing_ab),
                by = "habitat") %>%
      mutate(existing_ab = replace_na(existing_ab, 0),
             total_ab    = arriving + existing_ab) %>%
      filter(total_ab >= threshold)  # below threshold: individuals go extinct
    
    if (nrow(valid) == 0) next
    
    for (j in seq_len(nrow(valid))) {
      hab_j   <- valid$habitat[j]
      arr_j   <- valid$arriving[j]
      exist_j <- valid$existing_ab[j]
      
      if (exist_j == 0) {
        # Species not yet in this habitat: add interactions with its resources present here
        as_consumer <- destination_edgelist %>%
          filter(habitat == hab_j, node_from %in% sp_resources) %>%
          select(habitat, node_from, ab_node_from, taxon_node_from) %>%
          distinct() %>%
          mutate(node_to = sp, ab_node_to = arr_j, taxon_node_to = sp_taxon)
        
        if (nrow(as_consumer) > 0)
          rescue_new[[length(rescue_new) + 1]] <- as_consumer
        
      } else {
        # Already present: record how many individuals arrive (abundance update only)
        rescue_incr[[length(rescue_incr) + 1]] <- data.frame(
          habitat      = hab_j,
          node_id      = sp,
          ab_increment = arr_j,
          taxon        = sp_taxon,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  rescue_edges  <- if (length(rescue_new)  > 0) bind_rows(rescue_new) %>% distinct() else NULL
  ab_increments <- if (length(rescue_incr) > 0) bind_rows(rescue_incr) else NULL
  
  list(rescue_edges = rescue_edges, ab_increments = ab_increments)
}






#===============================================================================
#                            NULL MODEL FUNCTIONS
#                       (used in 5_Null_model.R)
#===============================================================================

##### 7. Mechanism 1 (rewiring) proxy
# checks whether a species has at least one resource partner among CP species that are still viable (abundance >= 1) in the new habitat (the same criterion apply_rewiring()
# uses to decide whether a species can persist).
# Needed because the null model tests this same criterion 500 times per habitat across random draws, so it's computed once here as a reusable lookup.

get_rewiring_eligible <- function(candidate_pool, new_habitats_ab_CP, metaweb, total_resources_baseline) {
  species_viable_CP <- new_habitats_ab_CP %>% filter(ab_node_from >= 1) %>% pull(node_from) %>% unique()
  
  rewiring_lookup <- total_resources_baseline %>%
    left_join(
      metaweb %>%
        filter(node_to %in% candidate_pool) %>%
        group_by(node_to) %>%
        summarise(cp_interactions = sum(node_from %in% species_viable_CP), .groups = "drop") %>%
        rename(node_id = node_to),
      by = "node_id"
    ) %>%
    mutate(cp_interactions = replace_na(cp_interactions, 0),
           rewire_eligible = cp_interactions > 0) %>%
    filter(node_id %in% candidate_pool) %>%
    select(node_id, rewire_eligible)
  
  tibble(node_id = candidate_pool) %>%
    left_join(rewiring_lookup, by = "node_id") %>%
    mutate(rewire_eligible = replace_na(rewire_eligible, FALSE))
}


##### 8. Mechanism 2 (rescue) proxy
# excludes non-dispersing taxa (same exclusions as apply_rescue()) and checks whether the species has a resource in some remaining habitat. 
# Needed because the null model tests this same criterion 500 times per habitat across random draws, so it's computed once here as a reusable lookup.

get_rescue_eligible <- function(candidate_pool, state_nodes_ab, metaweb, destination_edgelist,
                                excluded_taxa = c("Plant", "Crop", "Aphid", "Rodent ectoparasite", "Seed-feeding bird")) {
  
  taxon_lookup <- state_nodes_ab %>% filter(node_id %in% candidate_pool) %>% select(node_id, taxon) %>% distinct()
  
  dest_resources <- destination_edgelist %>% pull(node_from) %>% unique()
  
  resource_check <- metaweb %>%
    filter(node_to %in% candidate_pool) %>%
    mutate(resource_in_dest = node_from %in% dest_resources) %>%
    group_by(node_to) %>%
    summarise(has_dest_resource = any(resource_in_dest), .groups = "drop") %>%
    rename(node_id = node_to)
  
  tibble(node_id = candidate_pool) %>%
    left_join(taxon_lookup, by = "node_id") %>%
    left_join(resource_check, by = "node_id") %>%
    mutate(has_dest_resource = replace_na(has_dest_resource, FALSE),
           taxon = replace_na(taxon, "unknown"),
           rescue_eligible = !taxon %in% excluded_taxa & has_dest_resource) %>%
    select(node_id, rescue_eligible)
}


##### 9. Combines both mechanisms
# a species counts as "protected" if it's eligible for rewiring OR rescue, matching how the real simulation lets a species survive via either mechanism.

get_mechanism_eligibility <- function(candidate_pool, new_habitats_ab_CP, state_nodes_ab, metaweb,
                                      total_resources_baseline, destination_edgelist) {
  rewire <- get_rewiring_eligible(candidate_pool, new_habitats_ab_CP, metaweb, total_resources_baseline)
  rescue <- get_rescue_eligible(candidate_pool, state_nodes_ab, metaweb, destination_edgelist)
  
  rewire %>%
    left_join(rescue, by = "node_id") %>%
    mutate(rescue_eligible = replace_na(rescue_eligible, FALSE),
           protected = rewire_eligible | rescue_eligible)
}


##### 10. Simulation of species removal
# Randomly removes species down to the same final richness as the real land conversion simulation, but gives each species the same shot at rewiring/rescue protection first.
# This is what makes the null model comparable: same number of direct extinctions as the real simulation, random identity, same survival rules.

sim_sp_removal_mechanism <- function(edge_list_hab, n_target, eligibility_table) {
  
  edge_list_shuff <- data.frame()
  iteration <- numeric()
  list_species_rem <- data.frame()
  
  combined_nodes <- edge_list_hab %>%
    filter(!(taxon_node_from == "Crop" | taxon_node_to == "Crop")) %>%
    select(node_from, node_to) %>%
    pivot_longer(cols = c(node_from, node_to)) %>%
    ungroup() %>% select(-name) %>%
    unique() %>% pull(value)
  
  this_habitat <- unique(edge_list_hab$pre_hab)
  target_n <- n_target[n_target$habitat == this_habitat, 2]
  
  # Protected status per candidate; species not in the eligibility table (shouldn't normally happen)
  # default to "not protected" so they can still be counted toward the target.
  protected_map <- eligibility_table %>% filter(node_id %in% combined_nodes) %>% select(node_id, protected)
  
  for (i in 1:500) {
    print(i)
    
    shuffled <- sample(combined_nodes, length(combined_nodes), replace = FALSE)
    
    shuffled_df <- tibble(node_id = shuffled, draw_order = seq_along(shuffled)) %>%
      left_join(protected_map, by = "node_id") %>%
      mutate(protected = replace_na(protected, FALSE)) %>%
      arrange(draw_order)
    
    not_protected <- shuffled_df %>% filter(!protected)
    
    if (nrow(not_protected) < target_n) {
      warning(paste0("Habitat ", this_habitat, ", iteration ", i, ": only ", nrow(not_protected),
                     " non-protected candidates available, fewer than the target (", target_n,
                     "). Removing all available non-protected candidates."))
      sp_to_remove <- not_protected$node_id
    } else {
      sp_to_remove <- not_protected$node_id[1:target_n]
    }
    
    edge_list_remov <- dplyr::filter(edge_list_hab, !(node_from %in% sp_to_remove | node_to %in% sp_to_remove))
    edge_list_shuff <- rbind(edge_list_shuff, edge_list_remov)
    iteration <- c(iteration, rep(i, nrow(edge_list_remov)))
    
    for (j in sp_to_remove) {
      degree <- edge_list_hab %>% ungroup() %>%
        filter(node_from == j | node_to == j) %>%
        distinct(node_from, node_to) %>% summarise(degree = n())
      degree_sp <- cbind(species_rem = j, degree, iteration = i)
      list_species_rem <- rbind(list_species_rem, degree_sp)
    }
  }
  
  edge_list_shuff$iteration <- iteration
  return(list(edge_list_shuff, list_species_rem))
}


##### 11. Builds a habitat's pool of candidate species (no crop species)
get_candidate_pool <- function(edge_list_hab) {
  edge_list_hab %>%
    filter(!(taxon_node_from == "Crop" | taxon_node_to == "Crop")) %>%
    select(node_from, node_to) %>%
    pivot_longer(cols = c(node_from, node_to)) %>%
    ungroup() %>% select(-name) %>%
    unique() %>% pull(value)
}



##### 12. Combine edge list
#This function combines the edge list of the shuffled habitats with the non-transformed ones to create 500 trials of each habitat management scenario in the null model.

comb_edge_list<- function(non_transf_hab,shuff_hab) {
  
  # Initialize the objects
  combined_edge_lists <- list()  

  for (i in 1:500) {
    print(i)
    
    # Filter the iterated edge list of the transformed habitat according to the iteration
    iter_edge_list<- shuff_hab %>% filter (iteration == i)
    
    # Combine with edge list of non-transformed habitat
    non_transf_hab_ite<-non_transf_hab %>% mutate(iteration = i)
    combined_list <- rbind(non_transf_hab_ite, iter_edge_list)
    
    # Store the combined list
    combined_edge_lists[[i]] <- combined_list
  }
  
  return(combined_edge_lists)
}




#####  13. Create state_node list
#This function create the state_node list of each randomized management scenario in the null model.

state_node_list <- function(data) {
  
  # Processing node_from
  state_node_sem_ext_from <- data %>%
    select(habitat, node_from, ab_node_from, taxon_node_from, iteration) %>%
    rename(node_id = node_from, abundances = ab_node_from, taxon = taxon_node_from) %>%
    group_by(habitat, node_id) %>%
    unique()  # Eliminate duplicate species within each habitat
  
  # Processing node_to
  state_node_sem_ext_to <- data %>%
    select(habitat, node_to, ab_node_to, taxon_node_to, iteration) %>%
    rename(node_id = node_to, abundances = ab_node_to, taxon = taxon_node_to) %>%
    group_by(habitat, node_id) %>%
    unique()  # Eliminate duplicate species within each habitat
  
  # Aggregating the final state nodes
  state_node_sem_ext_agg <- rbind(state_node_sem_ext_from, state_node_sem_ext_to) %>%
    ungroup() %>%
    select(-habitat) %>%
    group_by(node_id, taxon,iteration) %>%
    summarise(abun = sum(abundances, na.rm = TRUE))  # Calculate sum of abundances
  
  return(state_node_sem_ext_agg)
}




