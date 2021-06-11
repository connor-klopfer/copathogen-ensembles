#' KW Tests for running the KW analysis on pairs. 
#' 
#' Files Needed for Analysis: 
#' 
#' MALED Files 
#' 
#' Provide Environmental variables, aka days of diarrhea: 
#'     - copath_fractions_maled_provide_env_Oct20.csv
#'     
#' ID's of participants in the PROVIDE dataset that were missing data, and the number
#' of epsiodes that were missing: 
#'    - provide_missed_specs.csv

library(dplyr)
library(ggplot2)
library(copathogenTools)

source("scripts/load_all_results.R", echo = F)

MINIMUM_CO <- 10

##### Functions ##### 
#' get_shared_sig_pairs <- function(pairs){
#'   #' Deprecated: We moved away from thresholding by significance. 
#'   return(pairs %>%
#'            filter(Flag > 0) %>%
#'            filter_out_symmetric() %>%
#'            # filter_out_redundancies() %>%
#'            mutate(direction = ifelse(actual > average, "Higher", "Lower")) %>%
#'            group_by(stool_type, path1, path2, direction) %>%
#'            summarise(count = n()) %>% filter(count > 1))
# }


rank_pathogen_pairs <- function(all_results){
  return(
    all_results %>%
      filter_out_symmetric() %>%               # Remove upper half of diagonal matrix
      # filter_out_redundancies() %>%
      # filter(actual > MINIMUM_CO) %>%          # Only inlcude minimum number of co-occurences 
      group_by(study, stool_type) %>%
      mutate(percentile = as.numeric(percentile)) %>% 
      mutate(percentile_rank = rank(percentile, ties.method = "average"), # rank the percentiles 
             distance_rank = rank(-abs(0.5 - percentile), ties.method = "average"), # get the dsintance from 0 and rank
             distance = abs(0.5 - percentile)) %>%
      mutate(percentile_rank_n = percentile_rank/max(percentile_rank), # Normalise percentile rank
             distance_rank_n = distance_rank/max(distance_rank)) %>%  # Normalise distance rank 
      mutate(top10_distance = distance_rank < max(distance_rank) * 0.1) %>% # Flag if it's the top 10 distance 
      # mutate(direction = ifelse(actual > average, "Higher", "Lower")) %>%
      group_by(stool_type, path1, path2, 
               # direction
               ) %>%
      mutate(count = n()) %>% 
      # filter(count > 1) %>%
      ungroup() %>% group_by(stool_type, path1, path2) %>% mutate(ave_rank  = mean(distance_rank_n)) %>% 
      ungroup()
  )
}

get_target_pairs <- function(){
  #' For indentifying specific pathogens to use in the manuscript based on what 
  #' Ross ID'd 
  #' 
  pathogens_and_colors <- c(
    "#ddb310" = "Campylobacter spp.+Rotavirus", 
    "#5eccab" = "EAEC+Norovirus GII",
    "#e41a1c" = "EAEC+Adenovirus 40/41",
    "#1f78b4" = "B. fragilis+Norovirus GII",
    "#00beff" = "ETEC+Rotavirus",
    # "#7FC97F" = "B. fragilis+Norovirus GII",
    "#33a02c" = "Shigella spp.+Rotavirus",
    "#999999" = "EPEC+Rotavirus",
    "#b15928" = "EPEC+Astrovirus",
    "#984ea3" = "ETEC+Astrovirus",
    # "#7FC97F" = "EPEC+Rotavirus",
    "#fb49b0" = "Norovirus GII+Astrovirus",
    "#C8B1C7" = "ETEC+EPEC",
    "#f781bf" = "ETEC+Campylobacter spp.",
    "navy" = "EPEC+Campylobacter spp.",
    "#537eff" = "EAEC+Campylobacter spp.",
    "#ff7f00" = "ETEC+EAEC",
    "orange" = "EPEC+B. fragilis",
    "#4daf4a" = "ETEC+B. fragilis",
    "gray32" = "EPEC+Cryptosporidium spp.",
    "#FDD58C" = "Shigella spp.+Campylobacter spp.",
    "#9BB5A4" = "Shigella spp.+ETEC",
    "#9BB5A4" = "Shigella spp.+ETEC",
    "#9BB5A4" = "Shigella spp.ETEC",
    "white" = " "
  )
  return(pathogens_and_colors)
}

get_top_ranked_pathogens <- function(all_results, shared = F){
  #' @title Get the top ranked pathogens between the two studies
  #'
  #' Get the top ranked pathogens between the two sutdies. Both have to occur > 0 times, and
  #' they need to be ranked. They also need to be going in the same direction.
  #'
  #' for Shared:
  #'      1.) Get the pathogens that occur > the MINIMUM CO number of times.
  #'      2.)
  #'
  #'
  #' @return Pathogen pairs that appear in the same direction and thir rank.
  #'
  ranked_results <- rank_pathogen_pairs(all_results)

  if(shared){
    top_pathogens <- ranked_results %>%
      select(path1, path2, stool_type, ave_rank) %>%
      distinct() %>%
      group_by(stool_type) %>%
      slice_min(ave_rank, n = floor(choose(length(union(all_results$path1, all_results$path2)), 2)* 0.1)) %>% select(-ave_rank)
    return(ranked_results %>% inner_join(top_pathogens, by = c("path1", "path2", "stool_type")))
  }else{
    top_pathogens <- ranked_results %>%
      select(path1, path2, stool_type, distance_rank_n, study) %>%
      distinct() %>%
      group_by(stool_type, study) %>%
      slice_min(distance_rank_n, n = floor(choose(length(union(all_results$path1, all_results$path2)), 2)* 0.1))

    return(ranked_results %>% inner_join(top_pathogens, by = c("path1", "path2", "stool_type", "study")))
  }
  # Get the top ranked pathogens

  # Filter by the pathogens found in the top ranked selection.
}



select_target_pathogens <- function(d_f){
  return(d_f %>% 
           mutate(path1_old = path1, path2_old = path2) %>% 
           readable_path_names() %>% 
           mutate(combined = paste(path1, path2, sep = "+")) %>% 
           filter(combined %in% get_target_pairs()) %>%
           mutate(path1 = path1_old, path2 = path2_old, direction = NA) %>% 
           select(-path1_old, -path2_old))
    
}

count_shared_sids<- function(sig_pairs, original, method = "sid", shared = F){
  #'@title COunt the number of significant pathogens
  # 
  
  # distinct_pairs <- sig_pairs %>% filter(Flag > 0) %>% ungroup() %>% 
  #   group_by(study, stool_type) %>% summarise(distinct_sids = n())
  
  # INstead of shared SIDs, get the top ranked pathogens 
  pathogens_to_count <- sig_pairs %>%
    # get_top_ranked_pathogens() %>% 
    select_target_pathogens() %>% 
    # filer9get_target_pairs()
    # select_target_pathogens() %>% 
    # get_shared_sig_pairs() %>%
    mutate(combined = paste(path1, path2, sep = " + "))
  
  # # If not shared, dont get the top ranked pathogens. 
  # if(shared){
  #   sig_pairs <- shared_between_studies
  # }else{
    sig_pairs <- pathogens_to_count %>%
      # filter(Flag > 0) %>%
      # filter(actual > MINIMUM_CO) %>%
      filter_out_symmetric()# %>%
    # filter_out_redundancies()
  # }
  # 
  vec_length <- nrow(sig_pairs)
  # get the distinct number of SIDs for the stool type. 
  
  sids <- vector(mode = "integer", length = vec_length)
  studies <- vector(mode = "character", length = vec_length)
  stool_type <- vector(mode = "character", length = vec_length)
  path1 <- vector(mode = "character", length = vec_length)
  path2 <- vector(mode = "character", length = vec_length)
  
  idx <- 1
  
  all_datasets  <- list()
  
  # for each stool type:
  for(st in c("Diarrhea", "Asymptomatic")){
    for(s in c("maled", "provide")){
      # if(shared){
      #   subset <- sig_pairs %>% filter(stool_type == st)
      # }else{
      #   # subset <- sig_pairs %>% filter(stool_type == st)
      #   subset <- sig_pairs %>% filter(stool_type == st, study == s)
      # }
      
      subset <- sig_pairs %>% filter(study == s)
      
      data_subsets <- list()
      sub_idx <- 1
      for(r in 1:nrow(subset)){
        path_positive <- original %>% filter(
          study == s, 
          stool_type == st
          ) %>% 
          select(participant, collection_date, subset$path1[r], subset$path2[r]) %>% 
          rename_with(~c("participant", "collection_date", "path1_present", "path2_present")) %>% 
          filter(path1_present > 0, path2_present > 0) %>% 
          mutate(
            combined = paste(subset$path1[r], subset$path2[r], sep = " + "), 
            study = s, 
            stool_type = st,
            path1 = subset$path1[r],
            path2 = subset$path2[r]
          )
        
        data_subsets[[sub_idx]] <- path_positive
        sub_idx <- sub_idx + 1
      }
      all_datasets[[idx]] <- do.call(rbind, data_subsets) #%>% 
        # left_join(shared_between_studies %>% select(-combined), by = c("path1", "path2", "stool_type", "study"))
      idx <- idx + 1
    }
  }
  
  appended <- do.call(rbind, all_datasets) #%>% 
    # mutate(is_shared = case_when(
    #   is.na(direction) ~ "Not Shared", 
    #   !is.na(direction) ~ "Shared"))
  
  if(method == "sid"){
    final <- appended %>%
      group_by(study, 
               stool_type,
               participant, 
               # is_shared
               ) %>% 
      summarise(distinct_pairs = n_distinct(combined))
  }else if(method == "pathogen"){
    # Gorup by study, stool_type, and combined, and count participants 
    final <- left_join(
      appended %>% group_by(study, 
                            stool_type,
                            combined#, 
                            # is_shared
                            ) %>%
        summarise(distinct_sids = n_distinct(participant)), 
      appended %>% group_by(study, 
                            stool_type
                            ) %>% 
        summarise(distinct_by_study_stooltype = n_distinct(participant)), 
      by = c("study"#, 
             # "stool_type"
             ))
    
  }else{
    # Group by the study, stool_type, and participant
    final <- appended %>% group_by(study, 
                                   stool_type,
                                   participant#, 
                                   # is_shared
                                   ) %>% 
      summarise(n_episodes = n_distinct(collection_date))}
  
  return(final)
}

fraction_of_observations <- function(sig_pairs, original, method = "sid", pool_stool){
  #' For finding all the significant pairs 
  
  # Get the target pathogens 
  sig_pairs <- sig_pairs %>% 
    # readable_path_names() %>% 
    # mutate(combined = paste(path1, path2, sep = "+")) %>% 
    # filter(combined %in% get_target_pairs()) %>% # COntinue here 04Jun21
    # get_top_ranked_pathogens(shared = T) %>% 
    select_target_pathogens() %>% 
    # group_by(path1, path2, 
    #          # direction
    #          ) %>% 
    # summarise(n()) %>% 
    select(path1, path2, 
           # direction
           )
  
  # Times two because we need PORVIDE and MAL-ED Datasets
  vec_length <- nrow(sig_pairs) * 2
  
  # get the distinct number of SIDs for the stool type. 
  sids <- vector(mode = "integer", length = vec_length)
  studies <- vector(mode = "character", length = vec_length)
  stool_type <- vector(mode = "character", length = vec_length)
  path1 <- vector(mode = "character", length = vec_length)
  path2 <- vector(mode = "character", length = vec_length)
  
  idx <- 1
  
  all_datasets  <- list()
  
  # for each Study as of 06 Jun21: No longer doing the higher and lower, only the selected pathogens. 
  # for(d in c("Higher", "Lower")){
    
    subset <- sig_pairs# %>% filter(direction == d)
    
    for(s in c("maled", "provide")){
      
      
      data_subsets <- list()
      
      # Go thorugh all the positive pathogen pairs in the dataset, then count how 
      # many were positive
      sub_idx <- 1
      for(r in 1:nrow(subset)){
        path_positive <- original %>% filter(study == s) %>% 
          select(participant, collection_date, subset$path1[r], subset$path2[r]) %>% 
          rename_with(~c("participant", "collection_date", "path1", "path2")) %>% 
          # filter(path1 > 0, path2 > 0) %>%
          mutate(
            path_positive = sum(path1 > 0 & path2 > 0),
            combined = paste(subset$path1[r], subset$path2[r], sep = " + "), 
            study = s#, 
            # direction = d
          )
        
        # Combine the dataset for each pathogen pair to a large subset for the 
        #Stool type 
        data_subsets[[sub_idx]] <- path_positive
        sub_idx <- sub_idx + 1
        
        
      }
      
      all_datasets[[idx]] <- do.call(rbind, data_subsets)
      
      idx <- idx + 1
    }
  # }
  
  
  
  # Combine into one large master subset.  
  appended <- do.call(rbind, all_datasets)
  
  if(method == "sid"){
    final <- appended %>% group_by(study, stool_type, participant) %>% 
      summarise(distinct_pairs = n_distinct(combined))
  }else if(method == "pathogen"){
    
    final <- left_join(
      appended %>% group_by(study, stool_type, combined) %>%
        summarise(distinct_pairs = n_distinct(participant)), 
      appended %>% group_by(study, stool_type) %>% 
        summarise(distinct_sids = n_distinct(participant)), 
      by = c("study", "stool_type"))
    
  }else if(method == "pathogen_episodes"){
    final <- appended %>% group_by(study, 
                                   # direction, 
                                   combined, participant) %>% 
      # summarise(episodes_w_pathogen = n_distinct(collection_date)) %>% 
      summarise(episodes_w_pathogen = sum(path1 > 0 &path2 > 0)) %>% 
      left_join(original_df %>% group_by(study, participant) %>% 
                  # summarise(all_episodes = n_distinct(collection_date))
                  summarise(all_episodes = n())
                , by = c("participant", "study"))
  }else{
    final <- appended %>% group_by(study, stool_type, participant) %>% 
      summarise(n_episodes = n_distinct(collection_date))
  }
  
  return(final)
  
}

pivot_sid_fraction_wide <- function(d_f, observations_df, co_results){
  
  
  
  final <- d_f %>% 
    mutate(frac_of_episodes = episodes_w_pathogen / all_episodes) %>%
    select(-episodes_w_pathogen) %>%
    tidyr::pivot_wider(names_from = combined, values_from = frac_of_episodes, values_fill = 0) %>% 
    tidyr::pivot_longer(-c("study", 
                           # "direction", 
                           "participant", "all_episodes"), 
                        names_to = "pathogens", values_to = "frac_episodes") %>% 
    semi_join(
      co_results %>%
        select_target_pathogens() %>% 
        # get_top_ranked_pathogens(shared = T) %>% 
      # get_shared_sig_pairs(all_simple_results) %>% 
        tidyr::unite(pathogens, c("path1", "path2"), sep = " + ")%>% 
                select(
                  # direction, 
                  pathogens), 
              by = c(
                # "direction", 
                "pathogens"))  %>% 
    tidyr::pivot_wider(names_from = pathogens, values_from = frac_episodes) %>%
    rename(n_collections = all_episodes) %>% 
    left_join(
      full_join(
        observations_df %>% 
          filter(`Age (days) [EUPATH_0000579]` <=372, !is.na(`Length-for-age z-score [EUPATH_0000689]`)) %>% 
          group_by(Participant_Id) %>% 
          arrange(desc(`Age (days) [EUPATH_0000579]`)) %>% slice(1) %>% 
          select(Participant_Id, `Age (days) [EUPATH_0000579]`, 
                 `Length-for-age z-score [EUPATH_0000689]`) %>%
          rename(participant = Participant_Id, age_laz = `Age (days) [EUPATH_0000579]`, LAZ = `Length-for-age z-score [EUPATH_0000689]`),
        observations_df %>% 
          filter(`Age (days) [EUPATH_0000579]` <=372, !is.na(`Weight-for-age z-score [EUPATH_0000733]`)) %>% 
          group_by(Participant_Id) %>% 
          arrange(desc(`Age (days) [EUPATH_0000579]`)) %>% slice(1) %>% 
          select(Participant_Id, `Age (days) [EUPATH_0000579]`, 
                 `Weight-for-age z-score [EUPATH_0000733]`) %>%
          rename(participant = Participant_Id, age_waz = `Age (days) [EUPATH_0000579]`, WAZ = `Weight-for-age z-score [EUPATH_0000733]`), 
        
        by = c("participant")), 
      by = "participant")%>% 
    left_join(
      observations %>% 
        filter(`Age (days) [EUPATH_0000579]` < 372) %>% group_by(Participant_Id) %>% 
        summarise(max_days = max(`Cumulative days within diarrheal episodes [EUPATH_0010480]`, na.rm = T)) %>% 
        rename(participant = Participant_Id, n_days_diarrhea_episode = max_days),
      by = "participant"
    )
  return(final)
  
}

add_maled_env <- function(samples_data, samples_f, obs, participants, households){
  #'  Title: Add the maled Data ffor the MAL-ED Dataset. 
  
  df_subsets <- list()
  
  # Myeloperixidase 
  df_subsets[[1]] <- samples_data %>% 
    select(Participant_Id, `Myeloperoxidase (ng/mL) [EUPATH_0011277]`, `Age (days) [EUPATH_0000579]`) %>% 
    filter(!is.na(`Myeloperoxidase (ng/mL) [EUPATH_0011277]`)) %>% 
    mutate(
      age_diff_12  = abs(`Age (days) [EUPATH_0000579]` - (12 * 7)), 
      age_diff_24  = abs(`Age (days) [EUPATH_0000579]` - (24 * 7)), 
      age_diff_40  = abs(`Age (days) [EUPATH_0000579]` - (40 * 7))) %>% 
    group_by(Participant_Id) %>% 
    mutate(
      min_12 = min(age_diff_12), 
      min_24 = min(age_diff_24), 
      min_40 = min(age_diff_40)) %>% 
    mutate(
      myeloperoxidase_12 = ifelse(min_12 == age_diff_12, `Myeloperoxidase (ng/mL) [EUPATH_0011277]`, NA),
      myeloperoxidase_24 = ifelse(min_24 == age_diff_24, `Myeloperoxidase (ng/mL) [EUPATH_0011277]`, NA),
      myeloperoxidase_40 = ifelse(min_40 == age_diff_40, `Myeloperoxidase (ng/mL) [EUPATH_0011277]`, NA),
      myeloperoxidase_age_12 = ifelse(min_12 == age_diff_12, `Age (days) [EUPATH_0000579]`, NA),
      myeloperoxidase_age_24 = ifelse(min_24 == age_diff_24, `Age (days) [EUPATH_0000579]`, NA),
      myeloperoxidase_age_40 = ifelse(min_40 == age_diff_40, `Age (days) [EUPATH_0000579]`, NA)
    ) %>% 
    filter(!is.na(myeloperoxidase_age_12) | !is.na(myeloperoxidase_age_24) | !is.na(myeloperoxidase_age_40)) %>% 
    group_by(Participant_Id) %>% 
    summarise(
      myeloperoxidase_12 = max(myeloperoxidase_12, na.rm = T),
      myeloperoxidase_24 = max(myeloperoxidase_24, na.rm = T),
      myeloperoxidase_40 = max(myeloperoxidase_40, na.rm = T),
      myeloperoxidase_age_12 = max(myeloperoxidase_age_12, na.rm = T),
      myeloperoxidase_age_24 = max(myeloperoxidase_age_24, na.rm = T),
      myeloperoxidase_age_40 = max(myeloperoxidase_age_40, na.rm = T)
    )
  
  # Neopterin 
  df_subsets[[2]] <- samples_data %>% 
    select(Participant_Id, `Neopterin (nmol/L) [EUPATH_0011278]`, `Age (days) [EUPATH_0000579]`) %>% 
    filter(!is.na(`Neopterin (nmol/L) [EUPATH_0011278]`)) %>% 
    mutate(
      age_diff_12  = abs(`Age (days) [EUPATH_0000579]` - (12 * 7)), 
      age_diff_24  = abs(`Age (days) [EUPATH_0000579]` - (24 * 7)), 
      age_diff_40  = abs(`Age (days) [EUPATH_0000579]` - (40 * 7))) %>% 
    group_by(Participant_Id) %>% 
    mutate(
      min_12 = min(age_diff_12), 
      min_24 = min(age_diff_24), 
      min_40 = min(age_diff_40)) %>% 
    mutate(
      neopterin_12 = ifelse(min_12 == age_diff_12, `Neopterin (nmol/L) [EUPATH_0011278]`, NA),
      neopterin_24 = ifelse(min_24 == age_diff_24, `Neopterin (nmol/L) [EUPATH_0011278]`, NA),
      neopterin_40 = ifelse(min_40 == age_diff_40, `Neopterin (nmol/L) [EUPATH_0011278]`, NA),
      neopterin_age_12 = ifelse(min_12 == age_diff_12, `Age (days) [EUPATH_0000579]`, NA),
      neopterin_age_24 = ifelse(min_24 == age_diff_24, `Age (days) [EUPATH_0000579]`, NA),
      neopterin_age_40 = ifelse(min_40 == age_diff_40, `Age (days) [EUPATH_0000579]`, NA)
    ) %>% 
    filter(!is.na(neopterin_age_12) | !is.na(neopterin_age_24) | !is.na(neopterin_age_40)) %>% 
    group_by(Participant_Id) %>% 
    summarise(
      neopterin_12 = max(neopterin_12, na.rm = T),
      neopterin_24 = max(neopterin_24, na.rm = T),
      neopterin_40 = max(neopterin_40, na.rm = T),
      neopterin_age_12 = max(neopterin_age_12, na.rm = T),
      neopterin_age_24 = max(neopterin_age_24, na.rm = T),
      neopterin_age_40 = max(neopterin_age_40, na.rm = T)
    )
  
  # Ferritin 
  df_subsets[[3]] <- samples_data %>% 
    mutate(ferritin = ifelse(is.na(`Mean ferritin (ng/mL) [EUPATH_0011871]`), 
                             ifelse(is.na(`Ferritin (ng/mL) [EUPATH_0011849]`), 
                                    ifelse(is.na(`Ferritin (ng/mL), 2nd [EUPATH_0011850]`), 
                                           NA, `Ferritin (ng/mL), 2nd [EUPATH_0011850]`),
                                    `Ferritin (ng/mL) [EUPATH_0011849]`), 
                             `Mean ferritin (ng/mL) [EUPATH_0011871]`)) %>%
    select(Participant_Id, ferritin, `Age (days) [EUPATH_0000579]`) %>% 
    filter(!is.na(ferritin)) %>%
    mutate(
      age_diff_18  = abs(`Age (days) [EUPATH_0000579]` - (18 * 7))) %>% 
    group_by(Participant_Id) %>% 
    mutate(
      min_18 = min(age_diff_18)) %>% 
    mutate(
      ferritin_18 = ifelse(min_18 == age_diff_18, ferritin, NA),
      ferritin_age_18 = ifelse(min_18 == age_diff_18, `Age (days) [EUPATH_0000579]`, NA)
    ) %>% 
    filter(!is.na(ferritin_age_18)) %>% 
    group_by(Participant_Id) %>% 
    summarise(
      ferritin_18 = max(ferritin_18, na.rm = T),
      ferritin_age_18 = max(ferritin_age_18, na.rm = T)
    )
  
  # Zinc
  df_subsets[[4]] <- samples_data %>% 
    filter(!is.na(`Zinc (umol/L) [EUPATH_0011166]`)) %>% 
    select(Participant_Id, `Zinc (umol/L) [EUPATH_0011166]`, `Age (days) [EUPATH_0000579]`) %>% 
    filter(!is.na(`Zinc (umol/L) [EUPATH_0011166]`)) %>%
    mutate(
      age_diff_18  = abs(`Age (days) [EUPATH_0000579]` - (18 * 7))) %>% 
    group_by(Participant_Id) %>% 
    mutate(
      min_18 = min(age_diff_18)) %>% 
    mutate(
      zinc_18 = ifelse(min_18 == age_diff_18,`Zinc (umol/L) [EUPATH_0011166]`, NA),
      zinc_age_18 = ifelse(min_18 == age_diff_18, `Age (days) [EUPATH_0000579]`, NA)
    ) %>% 
    filter(!is.na(zinc_age_18)) %>% 
    group_by(Participant_Id) %>% 
    summarise(
      zinc_18 = max(zinc_18, na.rm = T),
      zinc_age_18 = max(zinc_age_18, na.rm = T)
    )
  
  # Retinol 
  df_subsets[[5]] <- samples_data %>% 
    filter(!is.na(`Retinol (ug/dL) [EUPATH_0011875]`)) %>% 
    select(Participant_Id, `Retinol (ug/dL) [EUPATH_0011875]`, `Age (days) [EUPATH_0000579]`) %>% 
    filter(!is.na(`Retinol (ug/dL) [EUPATH_0011875]`)) %>%
    mutate(
      age_diff_18  = abs(`Age (days) [EUPATH_0000579]` - (18 * 7))) %>% 
    group_by(Participant_Id) %>% 
    mutate(
      min_18 = min(age_diff_18)) %>% 
    mutate(
      rbp_18 = ifelse(min_18 == age_diff_18,`Retinol (ug/dL) [EUPATH_0011875]`, NA),
      rbp_age_18 = ifelse(min_18 == age_diff_18, `Age (days) [EUPATH_0000579]`, NA)
    ) %>% 
    filter(!is.na(rbp_age_18)) %>% 
    group_by(Participant_Id) %>% 
    summarise(
      rbp_18 = max(rbp_18, na.rm = T),
      rbp_age_18 = max(rbp_age_18, na.rm = T)
    )
  
  # Cumulative Days bre
  df_subsets[[6]] <- obs %>% filter(!is.na(`Cumulative days exclusively breastfed [EUPATH_0011014]`)) %>% 
    group_by(Participant_Id) %>% 
    summarise(exclusive_breastfeeding = max(`Cumulative days exclusively breastfed [EUPATH_0011014]`, na.rm = T))
  
  # Drinking water source 
  df_subsets[[7]] <- households %>% 
    select(Household_Id, `Drinking water source [ENVO_00003064]`, `Household data collection date [EUPATH_0021085]`) %>%
    filter(!is.na(`Drinking water source [ENVO_00003064]`)) %>% 
    mutate(collection_date = as.Date(`Household data collection date [EUPATH_0021085]`)) %>% 
    group_by(Household_Id) %>% 
    mutate(max_date = max(collection_date)) %>% 
    filter(max_date == collection_date) %>% 
    left_join(participants %>% select(Household_Id, Participant_Id), by = "Household_Id") %>% 
    ungroup() %>% 
    select(Participant_Id, `Drinking water source [ENVO_00003064]`)
  
  # Maternal Education score 
  df_subsets[[8]] <- households %>% 
    select(Household_Id, `Maternal education score [EUPATH_0011591]`, `Household data collection date [EUPATH_0021085]`) %>%
    filter(!is.na(`Maternal education score [EUPATH_0011591]`)) %>% 
    mutate(collection_date = as.Date(`Household data collection date [EUPATH_0021085]`)) %>% 
    group_by(Household_Id) %>% 
    mutate(max_date = max(collection_date)) %>% 
    filter(max_date == collection_date) %>% 
    left_join(participants %>% select(Household_Id, Participant_Id), by = "Household_Id")%>% 
    ungroup() %>% 
    select(Participant_Id, `Maternal education score [EUPATH_0011591]`)
  
  
  # Append all the variables together. 
  final <- samples_f %>% rename(Participant_Id = participant)
  for(i in 1:length(df_subsets)){
    print(i)
    final <- final %>% left_join(df_subsets[[i]], by = "Participant_Id")
    
  }
  
  return(final)
} 

determine_designation <- function(path1, path2){
  final <- rep("ERROR", length(path1))
  final[which(path1 > 0 & path2 > 0)] <- "both"
  final[which(path1 > 0 & path2 == 0)] <- "path1"
  final[which(path1 == 0 & path2 > 0)] <- "path2"
  final[which(path1 == 0 & path2 == 0)] <- "neither"
  return(final)
}

condense_participants <- function(labels){
  if("both" %in% labels){
    return("both")
  }else if(("path1" %in% labels) & ("path2" %in% labels)){
    return("not_concurrent") 
  }else if((("path1" %in% labels) & !("path2" %in% labels)) | (!("path1" %in% labels) & ("path2" %in% labels))){
    return("either")
  }else if(!("path1" %in% labels) & !("path2" %in% labels)){
    return("neither")
  }else{
    return("ERROR")
  }
  
  if(length(labels) == 0){
    print("Empty labels")
  }
}


copath_relationship_summary <- function(pairs, original, by_stool = F, fill_missing = T){
  # Get the pathogen pairs 
  
  # Get the target pathogens 
  sig_pairs <- pairs %>%
    select_target_pathogens() %>% 
    # get_top_ranked_pathogens(shared = T) %>% 
    # get_shared_sig_pairs(pairs) %>% 
    ungroup() %>% distinct(path1, path2)
  
  pair_list <- list()
  for(p in 1: nrow(sig_pairs)){
    # Make a new dataframe, for all the significant pairs, find them in the original dataset and give them a
    # 1 or 0 if they are present or not. 
    pair_list[[p]] <- original %>%
      select(participant, study, stool_type, sig_pairs$path1[p],sig_pairs$path2[p]) %>%
      rename_with(~c("participant","study", "stool_type",  "path1", "path2")) %>%
      mutate(designation = determine_designation(path1, path2)) %>%
      rename(path1_present = path1, path2_present = path2) %>%
      mutate(path1 = sig_pairs$path1[p], path2 = sig_pairs$path2[p])
  }
  
  # Final iteration indicates we want to pool across all stools and we won't fill missing 
  # participants with a neither designation. 
  final <- do.call(rbind, pair_list) %>% 
    group_by(participant, study, path1, path2) %>% summarise(custom_tag = condense_participants(designation))
  
  if(by_stool){
    final <- do.call(rbind, pair_list) %>%
      group_by(participant, study,
               stool_type,
               path1, path2) %>%
      summarise(custom_tag = condense_participants(designation)) %>%
      mutate(combined = paste(path1, path2, sep = "+"))



    if(fill_missing){
      final <- final %>% ungroup() %>% group_by(study) %>%
        tidyr::complete(participant, stool_type, combined, fill = list(custom_tag = "neither"))

    }else{
      final <- final %>% ungroup() %>% group_by(study, stool_type) %>%
        tidyr::complete(participant, combined, fill = list(custom_tag = "neither"))
    }

    # We want all participants, we want all participant stool_type combinations.
  }else{
    final <- do.call(rbind, pair_list) %>%
      group_by(participant, study, path1, path2) %>% summarise(custom_tag = condense_participants(designation))

  }
  # final <- final %>% mutate(combined = paste(path1, path2, sep = "+"))
  return(final)
}

copath_relationship_env_vars <- function(summary_df, env_vars){
  
  final <- summary_df %>% 
    left_join(
      env_vars %>%
        select(-contains("+")) %>% ungroup() %>% 
        select(-direction) %>%
        tidyr::pivot_longer(-c("Participant_Id", "study"),
                            names_to = "measurement",
                            values_to = "value", 
                            values_drop_na = TRUE) %>% 
        rename(participant = "Participant_Id"),
      # by = c("participant", "study", "path1", "path2")) %>% distinct()
      by = c("participant", "study")) %>% distinct()
  
  return(final)
}

test_kw_tests <- function(d_f, specs, by_stool = T){
  if(by_stool){
    d_df <- d_f %>% ungroup() %>% select(study, 
                                         stool_type,
                                         path1, path2) %>%  distinct()
    
  }else{
    d_df <- d_f %>% ungroup() %>% select(study, path1, path2) %>%  distinct()
  }
  
  num_combos <- nrow(d_df) * length(names(specs)) *4
  path1 <- vector(mode = "character", length = num_combos)
  path2 <- vector(mode = "character", length = num_combos)
  study_v <- vector(mode = "character", length = num_combos)
  stool <- vector(mode = "character", length = num_combos)
  
  n_obs <- vector(mode = "numeric", length = num_combos)
  ave <- vector(mode = "numeric", length = num_combos)
  st_dev <- vector(mode = "numeric", length = num_combos)
  minim <- vector(mode = "numeric", length = num_combos)
  low_quant <- vector(mode = "numeric", length = num_combos)
  high_quant <- vector(mode = "numeric", length = num_combos)
  medi <- vector(mode = "numeric", length = num_combos)
  maxim <- vector(mode = "numeric", length = num_combos)
  
  degfree <- vector(mode = "numeric", length = num_combos)
  chi_sq <- vector(mode = "numeric", length = num_combos)
  tag <- vector(mode = "numeric", length = num_combos)
  
  pr_f <- vector(mode = "numeric", length = num_combos)
  v_iterator <- 0
  idx <- 1
  if(by_stool){
    cleaned_df <- d_f %>% 
      distinct(participant, study, 
               stool_type,
               path1, path2, custom_tag, ndays_diarrhea) 
  }else{
    cleaned_df <- d_f %>% 
      distinct(participant, study, path1, path2, custom_tag, ndays_diarrhea) 
    
  }
  for(v in names(specs)){
    for(r in 1:nrow(d_df)){
      if(by_stool){
        data_subset <- cleaned_df %>% 
          filter(study == d_df$study[r],
                 stool_type == d_df$stool_type[r],
                 path1 == d_df$path1[r], 
                 path2 == d_df$path2[r])
        
      }else{
        data_subset <- cleaned_df %>% 
          filter(study == d_df$study[r],
                 path1 == d_df$path1[r], 
                 path2 == d_df$path2[r])
      }
      
      sample_regression <- kruskal.test(data_subset[[v]] ~ data_subset[["custom_tag"]], 
                                        data = data_subset)
      
      for(l in unique(d_f$custom_tag)){
        
        sub_subset <- data_subset %>% filter(custom_tag == l)
        n_obs[idx] <- nrow(sub_subset)
        
        ave[idx] <- mean(sub_subset[[v]])
        st_dev[idx] <- sd(sub_subset[[v]])
        minim[idx] <- min(sub_subset[[v]])
        low_quant[idx] <- quantile(sub_subset[[v]])[2][[1]]
        high_quant[idx] <- quantile(sub_subset[[v]])[4][[1]]
        medi[idx] <- median(sub_subset[[v]])
        maxim[idx] <- max(sub_subset[[v]])
        chi_sq[idx] <- sample_regression$statistic[[1]]
        
        pr_f[idx] <- sample_regression$p.value[1]
        degfree[idx] <- sample_regression[["parameter"]][["df"]]
        path1[idx] <- d_df$path1[r]
        path2[idx] <- d_df$path2[r]
        
        study_v[idx] <- d_df$study[r]
        tag[idx] <- l
        
        if(by_stool){
          stool[idx] <- d_df$stool_type[r]
        } 
        idx <- idx + 1
      }
    }
  }
  v_iterator <- v_iterator + 1
  if(!by_stool){
    final <- data.frame(study_v, path1, path2, tag, n_obs, ave, st_dev, minim, 
                        low_quant,medi, high_quant, maxim, degfree, chi_sq, pr_f)
  }else{
    final <- data.frame(study_v, stool, path1, path2, tag, n_obs, ave, 
                        st_dev, minim, low_quant,medi, high_quant, maxim, degfree, chi_sq, pr_f)
  }
  
  return(final)
  
}

label_pathogen_pair <- function(path1, path2){
  pathogen_labels <- list()

  pathogen_labels[["viruses"]] <- c(
    "adenovirus f"                  = "Adenovirus 40/41",
    "norovirus gi"                  = "Norovirus GI",
    "norovirus gii"                 = "Norovirus GII",
    "astrovirus"                    = "Astrovirus",
    "sapovirus"                     = "Sapovirus",
    "rotavirus"                     = "Rotavirus"#,
  )
  pathogen_labels[["bacteria"]] <- c(
    "aeromonas"                     = "Aeromonas",
    "salmonella"                    = "Salmonella",
    "h.pylori"                      = "H. pylori",
    "c.jejuni/coli"                 = "C. jejuni/coli",
    "campy pan"                     = "Campylobacter spp.",
    "b.fragilis"                    = "B. fragilis",
    "c.difficile"                   = "C. difficile",
    "m.tb"                          = "M. tuberculosis",
    "v.cholerae"                    = "V. cholerae",
    "shigella & eiec"               = "Shigella spp.",
    "eaec"                          = "EAEC",
    "atypical epec"                 = "aEPEC",
    "typical epec"                  = "tEPEC",
    "stec"                          = "STEC",
    "lt_etec"                       = "ETEC lt",
    "st_etec"                       = "ETEC st",
    "etec"                          = "ETEC", 
    "epec"                          = "EPEC"
  )
  
  pathogen_labels[["other"]] <- c(
    # "aeromonas"                     = "Aeromonas",
    "ancyclostoma"                  = "Ancyclostoma",
    "ascaris lumbricoides"          = "A. lumbricoides", 
    "trichuris trichiura"           = "Trichuris",
    "e.bieneusi"                    = "E. bieneusi",
    "e.intestinalis"                = "E. intestinalis",
    "cryptosporidium"               = "Cryptosporidium spp.",
    "necator"                       = "Necator",
    "strongyloides"                 = "Strongyloides",
    "cyclospora"                    = "Cyclospora",
    "isospora"                      = "Isospora",
    "e.histolytica"                 = "E. histolytica"#,
  )
  
  path1_temp <- NA
  path2_temp <- NA
  for(l in names(pathogen_labels)){
    # print(l)
    if(path1 %in% names(pathogen_labels[[l]])){
      path1_temp <- l
    }
    if(path2 %in% names(pathogen_labels[[l]])){
      path2_temp <- l
    }
  }
  
  return(paste(sort(c(path1_temp, path2_temp)), collapse = " + "))
}

# The problem here is that there's multiple different observations for a single episode
# for every episode, look and see if any of the observation ids are found within the samples subset
expanded_matching <- function(o, h, original){
  # Get for every episode, find the pool of observation IDs,  then see if there's an pbservation ID that's found 
  # within the sampples of the original dataset. 
  bangladesh_subset <- h %>% filter(`Country [OBI_0001627]` == "Bangladesh")
  
  # Collapse by the participant and the episode number 
  collapsed_o <- o %>% filter(Household_Id %in% c(bangladesh_subset$Household_Id)) %>% 
    filter(!is.na(`Diarrheal episode # [EUPATH_0010472]`), `Age (days) [EUPATH_0000579]` <= 372) %>%
    group_by(Participant_Id, `Diarrheal episode # [EUPATH_0010472]`) %>% 
    summarise(observation_levels = c(unique(Observation_Id))) %>% ungroup()
  
  
  participant_ids <- vector(mode = "character", length = nrow(collapsed_o))
  episodes <- vector(mode = "character", length = nrow(collapsed_o))
  idx <- 1
  for(p in unique(collapsed_o$Participant_Id)){
    o_subset <- collapsed_o %>% filter(Participant_Id == p)
    for(e in unique(as.character(o_subset$`Diarrheal episode # [EUPATH_0010472]`))){
      
      obs_ids <- c(o_subset$observation_levels[which(o_subset$`Diarrheal episode # [EUPATH_0010472]` == e)])
      og_ids <- c(original$observation_id[which(original$participant == p)])
      
      if(length(intersect(og_ids, obs_ids)) == 0){
        
        participant_ids[idx] <- p
        episodes[idx] <- e
        idx <- idx + 1
        
      }else{
        # print(paste("condisition found for", p, "epsisode", e))
      }
    }
    
  }
  
  return(data.frame(participant_ids, episodes) %>% filter(participant_ids != ""))
  
  #TODO: Should check that participants are from banflasesh 
}

combine_like_pathogens <- function(d_f){
  all_etec <- d_f %>% select(one_of(c("st_etec", "etec", "lt_etec"))) %>% rowSums(.)
  
  new_etec <- (all_etec > 0) * 1
  
  final <- cbind(
    d_f %>% select(-one_of(c("st_etec", "lt_etec", "etec"))), 
    etec = new_etec
  ) %>% 
    rename(
      epec = `typical epec`
    ) %>% 
    select(
      - `atypical epec`
    )
  
  return(final)
  
}

filter_out_symmetric <- function(d_f){
  final <- d_f %>% 
    mutate(path1_switch = ifelse(path1 > path2, path1, path2),
           path2_switch = ifelse(path1 > path2, path2, path1)) %>% 
    select(-path1, -path2) %>% 
    rename(path1 = path1_switch, path2 = path2_switch) %>% 
    select(path1, path2, everything()) %>% 
    filter(path1 != path2) %>% 
    distinct()
  return(final)
}

##### END OF Functions #####

two_var_coloring <- c("#1f78b4", "#67D067")

##### DATA IMPORT #####
# Import the needed data, original dataset and simple results
original_df <- import_complete_datasets(parent_dir = "data/") %>% combine_like_pathogens()
all_simple_results <- get_all_simple_results(original_df, parent_dir = "results/pooled_samples/EXP_")
# 
# MAL-ED
ontology <- data.table::fread("data/ISASimple_Gates_MAL-ED_phase3_RSRC_ontologyMetadata.txt")
samples <- data.table::fread("data/ISASimple_Gates_MAL-ED_phase3_RSRC_samples.txt")
observations <- data.table::fread("data/ISASimple_Gates_MAL-ED_phase3_RSRC_observations.txt")
participant <- data.table::fread("data/ISASimple_Gates_MAL-ED_phase3_RSRC_participant.txt")
households <- data.table::fread("data/ISASimple_Gates_MAL-ED_phase3_RSRC_households.txt")


# For each pathogen SID, what is the count of occurences of pathogen pairs, the tally of pathogen pairs that were significant, shared by both sudies versus not sharing both pathogens 
sid_counts <- count_shared_sids(all_simple_results, original_df, method = "sid")

# For each participant, the number of episodes where a shared pathogen pair was found, and the number pf episodes where a non shoread pathogen pair was found. 
sid_by_episode <- count_shared_sids(all_simple_results, original_df, method = "episode")

# For each pathogen pair, is it shared or not shared, the number of distinct sid's account. 
sid_by_pathogen <- count_shared_sids(all_simple_results, original_df, method = "pathogen")

# Same as above, only includes the pathogen pairs that were shared across both studies. 
shared_sid_counts <- count_shared_sids(all_simple_results, original_df, method = "sid", shared = T)
shared_sid_by_episode <- count_shared_sids(all_simple_results, original_df, method = "episode", shared = T)
shared_sid_by_pathogen <- count_shared_sids(all_simple_results, original_df, method = "pathogen", shared = T)

sid_fraction_by_pathogen <- fraction_of_observations(all_simple_results, original_df, method = "pathogen_episodes")

sid_fraction_wide <- pivot_sid_fraction_wide(sid_fraction_by_pathogen, observations, all_simple_results)

sid_fraction_wide_env_vars <- add_maled_env(
  left_join(samples, observations %>% 
              select(Observation_Id, `Observation date [EUPATH_0004991]`, `Age (days) [EUPATH_0000579]`), by = "Observation_Id"),
  sid_fraction_wide, 
  observations, 
  participant, 
  households)


provide_env_variables <- read.csv(file = "data/copath_fractions_maled_provide_env_Oct20.csv")
provide_missing_values <- read.csv("data/provide_missed_specs.csv", stringsAsFactors = F)

copath_fractions_all <- readr::read_csv("data/copath_fractions_maled_provide_env_Oct20.csv") %>% tidyr::pivot_longer(contains("__")) 

pair_bins <- copath_relationship_summary(
  all_simple_results, original_df, by_stool = F) %>% 
  copath_relationship_env_vars(., provide_env_variables)


pair_bins_bystool <- copath_relationship_summary(all_simple_results, 
                                                 original_df, by_stool = T, 
                                                 fill_missing = T) %>%
  tidyr::separate(combined, c("path1", "path2"), sep = "\\+") %>% 
  copath_relationship_env_vars(., provide_env_variables)

missing_diar_ids_maled <- expanded_matching(observations, households, original_df) %>% distinct(participant_ids) 


# TODO: THis needs to change here, there's too many nested functions, we should 
# have it eliminate any participants that are missing any of the stools. 
pair_bins_bystool_dropped <- copath_relationship_summary(
  all_simple_results, original_df,
  by_stool = T, fill_missing = T) %>% 
  tidyr::separate(combined, c("path1", "path2"), sep = "\\+") %>% 
  copath_relationship_env_vars(., provide_env_variables) %>% 
  left_join(
    copath_fractions_all %>%
      rename(ndays_diarrhea = n_days_diarrhea_episode, participant = Participant_Id) %>%
      select(participant, ndays_diarrhea) %>% distinct(), by = "participant") %>% 
  anti_join(
    rbind(
      provide_missing_values %>% select(-N_asympto) %>% tidyr::pivot_longer(c("Missing_asympto", "Missing_diarrheal"),
                                                                            names_to = "stool_type",
                                                                            values_to = "n_missing") %>% 
        mutate(stool_type = stringr::str_replace(stool_type, "Missing_", "")) %>% 
        mutate(stool_type = ifelse(stool_type == "diarrheal", "Diarrhea", "Asymptomatic")) %>% filter(n_missing > 0) %>% 
        mutate(participant = as.character(Participant_id)) %>% select(participant, stool_type),
      missing_diar_ids_maled %>% rename(participant = participant_ids) %>% select(participant) %>% mutate(stool_type = "Diarrhea")),
    by = c("participant",
           "stool_type"
           )) 




pair_bins_bystool_dropped_nonfilled <- copath_relationship_summary(
  all_simple_results, original_df,
  by_stool = T, fill_missing = F) %>% 
  tidyr::separate(combined, c("path1", "path2"), sep = "\\+") %>% 
  copath_relationship_env_vars(., provide_env_variables)


# TODO: Need to fill in these pathogen combinations
pair_bins_wide <- pair_bins %>% tidyr::pivot_wider(names_from = "measurement", values_from = "value")

pair_bins_wide_bystool <- pair_bins_bystool %>% 
  tidyr::pivot_wider(id_cols = c("study", "participant", 
                                 "stool_type",
                                 "path1", "path2", "custom_tag"),  
                     names_from = "measurement", values_from = "value")


pooled_dropped_participants <- pair_bins %>% 
  left_join(
    copath_fractions_all %>%
      rename(ndays_diarrhea = n_days_diarrhea_episode, participant = Participant_Id) %>%
      select(participant, ndays_diarrhea) %>% distinct(), by = "participant") %>% 
  anti_join(
    rbind(
      provide_missing_values %>% select(-N_asympto) %>% 
        tidyr::pivot_longer(c("Missing_asympto", "Missing_diarrheal"), names_to = "stool_type", values_to = "n_missing") %>% 
        mutate(stool_type = stringr::str_replace(stool_type, "Missing_", "")) %>% 
        mutate(stool_type = ifelse(stool_type == "diarrheal", "Diarrhea", "Asymptomatic")) %>% filter(n_missing > 0) %>% 
        mutate(participant = as.character(Participant_id)) %>% select(participant) %>% distinct(participant), 
      missing_diar_ids_maled %>% rename(participant = participant_ids)), 
    by = c("participant")) 


copath_fractions_all <- readr::read_csv("data/copath_fractions_maled_provide_env_Oct20.csv") %>% 
  tidyr::pivot_longer(contains("__"))
# 'Dictionary" for comparisons 
kw_comparisons <-   list(
  "ndays_diarrhea" = list(
    'title' = "Days of Diarrhea in 1st Year of Life", 
    'x' = "Days"
  )
)

# Run the KW test for the original set, were missing values are set as neither 
original_regression_results <- test_kw_tests(
  pair_bins %>% left_join(
    copath_fractions_all %>% 
      rename(ndays_diarrhea = n_days_diarrhea_episode, participant = Participant_Id) %>% 
      select(participant, ndays_diarrhea) %>% distinct(), by = "participant"), 
  kw_comparisons, by_stool = F)

# Seperating out by stool, KW test
kw_bystool <- test_kw_tests(
  pair_bins_bystool %>% left_join(
    copath_fractions_all %>% 
      rename(ndays_diarrhea = n_days_diarrhea_episode, participant = Participant_Id) %>% 
      select(participant, ndays_diarrhea) %>% distinct(), by = "participant"), 
  kw_comparisons, by_stool = T)
# 

# Dropping individuals 
kw_bystool_dropped <- test_kw_tests(
  pair_bins_bystool_dropped,# %>% left_join(
  # copath_fractions_all %>% 
  #   rename(
  #     ndays_diarrhea = n_days_diarrhea_episode,
  #     participant = Participant_Id) %>% 
  # select(participant, ndays_diarrhea) %>% distinct(), by = "participant"), 
  kw_comparisons, by_stool = T)

kw_pooled_dropped <- pooled_dropped_participants %>% test_kw_tests(kw_comparisons, by_stool = F)


my_kw_plots <- function(d_f){
  final <- d_f %>% 
    rename(measurement_ave = ave) %>%
    mutate(sig_label = case_when(pr_f < 0.05 ~ "*", TRUE ~""), 
           study_v = case_when(study_v == "maled" ~ "MAL-ED", study_v == "provide" ~ "PROVIDE")) %>% 
    # mutate(combined = paste0(combined, sig_label)) %>%
    # arrange(ave_rank) %>%
    # case_when(pr_f < 0.05 ~ paste(combined, "*"), TRUE ~ combined) %>% 
    # group_by(combined) %>% 
    # mutate(facet_idx = seq(nrow(.))) %>% View()
    # mutate(combined = factor(combined, levels = unique(reorder(combined, ave_rank)))) %>% 
    ggplot(aes(x = combined, ))+
    geom_bar(
      aes(
        fill = tag, color = tag,y = measurement_ave
        # alpha = factor(sig_label)
        ),
      width = 0.8, 
      color = "black",
      stat = "identity", position = position_dodge(width = 0.8))+
    # geom_label(aes(label = round(measurement_ave, 2)), position = position_dodge2(width = 0.8))+
    geom_text(aes(label = sig_label, y = max(measurement_ave, na.rm = T) + 1, group = combined), 
              # position = position_dodge(width = 0.8), 
              color = "black")+
    coord_flip()+
    scale_fill_manual(
      labels = c("both" = "Both", "either" = "Either", "neither" = "Neither", "not_concurrent" = "Not Concurrent"),
      values = c("#a6cee3", "#1f78b4", "#b2df8a",  "#33a02c"))+
    scale_color_manual(
      labels = c("both" = "Both", "either" = "Either", "neither" = "Neither", "not_concurrent" = "Not Concurrent"),
      values = c("#a6cee3", "#1f78b4", "#b2df8a",  "#33a02c"))+
    scale_alpha_manual(values =c(0.2, 0.99),limits =c("", "*"), labels = c("> 0.05", "< 0.05"))+
    theme(strip.background.y = element_blank(), 
          strip.text.y = element_blank(), 
          panel.background = element_rect(fill = 'gray99'), 
          panel.border = element_rect(fill = NA, color = "black"), 
          panel.grid.major.x = element_line(color = "black"), 
          axis.text.y = element_text(size = rel(1.2)))+
    # facet_grid(facet_idx~.,
    #            scales = "free_y"
    #            )+
    facet_grid(combined~study_v, scales = "free_y")+
    labs(y = "Number of Days of Diarrhea", x = "",  fill = "Pathogen found in:", alpha = "KW P Value")
  return(final)
}

interaction_types <- all_simple_results %>% rowwise() %>% 
  mutate(interaction_type = label_pathogen_pair(path1, path2)) %>% ungroup() %>% 
  select(interaction_type) %>% unique() %>% unlist()


# for(i in interaction_types){
  # kw_pooled_dropped
    temp_source_data <- kw_pooled_dropped %>%
    left_join(
      all_simple_results %>% 
        # rank_pathogen_pairs() %>% 
        select_target_pathogens() %>%
      # get_top_ranked_pathogens(all_simple_results, shared = T) %>% 
        select(path1, path2, 
               stool_type,
               # direction, 
               study, 
               # ave_rank
               ) %>% 
        # arrange(ave_rank) 
      mutate(facet_idx = seq(nrow(.))), 
      by = c("path1", "path2", 
             # "stool" = "stool_type",
             "study_v" = "study")
    ) %>% 
    rowwise() %>% 
    mutate(interaction_type = label_pathogen_pair(path1, path2)) %>% ungroup() %>% 
      # group_by(path1, path2, study_v, interaction_type#, 
      #          # direction
      #          ) %>% mutate(facet_idx = mean(facet_idx, na.rm = T)) %>% ungroup() %>% 
      readable_path_names() %>% 
      mutate(combined = paste(path1, path2, sep = " + "))
    
    
    # for(s in unique(temp_source_data$study_v)){
      # for(d in unique((temp_source_data$direction))){
      #   valid_plot <- temp_source_data %>%
      #           filter(study_v == s, interaction_type == i, direction == d) %>% nrow() > 0
        # if(valid_plot){
          temp_plot<- temp_source_data %>%
            # filter(study_v == s, 
            #        # interaction_type == i#, 
            #        # direction == d
            #        ) %>%
            filter((interaction_type == "bacteria + bacteria" | combined == "EPEC + Cryptosporidium spp.")) %>%
            my_kw_plots() +
            labs(title = "Bacteria + Bacteria Pairs")
          ggsave(path = "figures", filename = "BB_kw_plot.png", device = "png")
                 dev.off()
                 
          temp_plot<- temp_source_data %>%
             # filter(study_v == s#, 
             #        # interaction_type == i#, 
             #        # direction == d
             # ) %>%
             filter(interaction_type == "bacteria + viruses" | combined == "Norovirus GII + Astrovirus") %>%
             my_kw_plots() +
             labs(title = "Bacteria + Virus Pairs")
           ggsave(path = "figures", filename = "BV_kw_plot.png", device = "png")
           dev.off()
        # }

      # }
    # }
    
  
# }
