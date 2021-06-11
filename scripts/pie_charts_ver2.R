#' Specific Pie script
#' 
#' Ross gave me a list of pathogens to use in the pai chart plots. The coding is different so I need 
#' to adjust accordingly 
#' 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(copathogenTools)

source("scripts/load_all_results.R", echo = F)

MINIMUM_CO <- 10

##### Functions #####

pie_plot_all_subsets <- function(d_f, original){
  # Plot all the subsets together. 
  
  bb_plot <- plot_pie_chart(d_f, original, 
                            plot_pair_type = "bacteria + bacteria", 
                            my_plot_title = "Bacteria + bacteria pairs and their burden of diarrhea", 
                            my_caption = "One bacteria + parasite pair included.")
  bv_plot  <- plot_pie_chart(d_f, original, plot_pair_type = "bacteria + viruses", 
                             my_plot_title = "Bacteria + virus pairs and their burden of diarrhea", 
                             my_caption = "One virus + virus pair included.")
  # plot all. 
  pdf('figures/copathogen_pie_charts.pdf', onefile = TRUE, height = 20, 
      width = 20)
  grid.arrange(
    bb_plot,
    bv_plot,
    ncol = 1,
    nrow = 2
  )
  dev.off()
  
  # # # plot all. 
  # pdf('../figures/copathogen_pie_charts_ver2_photo.png', height = 20,
  #     width = 20)
  png_vers <- arrangeGrob(
    bb_plot,
    bv_plot,
    ncol = 1,
    nrow = 2
  )
  
  ggsave(file = 'figures/copathogen_pie_charts_photo.png', height = 20,
         width = 20, plot = png_vers)
  
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
    
    
    
    
    
    
    
    
    
    # "#B2258F" = "Cryptosporidium spp.+tEPEC",
    # # "#C74C28" = "aEPEC+ETEC",
    # # "#7FC97F" = "Adenovirus 40/41+aEPEC",
    # "#7FC97F" = "Norovirus GII+Astrovirus",
    # "#1f78b4" = "Astrovirus+EPEC",
    # "#a65628" = "Astrovirus+ETEC",
    # "#33a02c" = "Adenovirus 40/41+EAEC",
    # # "#E31864" = "Sapovirus+aEPEC",
    # "#E31864" = "Sapovirus+Campylobacter spp.",
    # "#b15928" = "Rotavirus+Campylobacter spp.",
    # "#984ea3" = "Rotavirus+ETEC",
    # "#e41a1c" = "Rotavirus+EPEC",
    # "#EDAB89" = "Rotavirus+Shigella spp.",
    # "#fdbf6f" = "Rotavirus+Norovirus GII",
    # "#C74C28" = "EPEC+Shigella spp.",
    # "#4daf4a" = "Norovirus GI+Campylobacter spp.",
    # "lightblue" = "Aeromonas+V. cholerae",
    # 'pink' = "Norovirus GII+C. difficile",
    # 'yellow' = "Norovirus GII+Astrovirus",
    # '#6a3d9a' = "Rotavirus+C. difficile",
    # '#ff7f00' = 'Astrovirus+EPEC',
    # 'green' = "Astrovirus+Cryptosporidium spp.",
    # 'yellow' = "Rotavirus+Cryptosporidium spp.",
    # "NULL" = "white"
    # "#666666" = "Fraction across both studies",
  )
  # pathogens_subset <- vector(mode = "character", length = length(pathogens))
  # colors_subset <- vector(mode = "character", length = length(pathogens))
  # counter <- 1
  # for(p in 1:length(pathogens)){
  #   for(c in 1:length(pathogens_and_colors)){
  #     if(stringr::str_detect(pathogens[p], stringr::fixed(pathogens_and_colors[c]))){
  #       # print(pathogens[p])
  #       pathogens_subset[counter] <- pathogens[p]
  #       colors_subset[counter] <- names(pathogens_and_colors)[c]
  #       counter <- counter + 1
  #     }#else{
  #     #   print(paste(p, pathogens_and_colors[c]))
  #     # }
  #   }
  # }
  # names(pathogens_subset) <- colors_subset
  # return(pathogens_subset)
  return(pathogens_and_colors)
}


label_pathogen_pair <- function(path1, path2){
  pathogen_labels <- list()
  # print(path1)
  # print(path2)
  # print("Next")
  pathogen_labels[["viruses"]] <- c(
    # "aeromonas"                     = "Aeromonas",
    # "ancyclostoma"                  = "Ancyclostoma",
    # "trichuris trichiura"           = "Trichuris",
    # "e.bieneusi"                    = "E. bieneusi",
    # "e.intestinalis"                = "E. intestinalis",
    # "cryptosporidium"               = "Cryptosporidium spp.",
    # "salmonella"                    = "Salmonella",
    # "h.pylori"                      = "H. pylori",
    # "c.jejuni/coli"                 = "C. jejuni/coli",
    # "campy pan"                     = "Campylobacter spp.",
    # "b.fragilis"                    = "B. fragilis",
    # "c.difficile"                   = "C. difficile",
    "adenovirus f"                  = "Adenovirus 40/41",
    "norovirus gi"                  = "Norovirus GI",
    "norovirus gii"                 = "Norovirus GII",
    "astrovirus"                    = "Astrovirus",
    # "necator"                       = "Necator",
    # "strongyloides"                 = "Strongyloides",
    # "cyclospora"                    = "Cyclospora",
    # "isospora"                      = "Isospora",
    # "e.histolytica"                 = "E. histolytica",
    # "m.tb"                          = "M. tuberculosis",
    # "v.cholerae"                    = "V. cholerae",
    # "shigella & eiec"               = "Shigella spp.",
    "sapovirus"                     = "Sapovirus",
    "rotavirus"                     = "Rotavirus"#,
    # "eaec"                          = "EAEC",
    # "atypical epec"                 = "aEPEC",
    # "typical epec"                  = "tEPEC",
    # "stec"                          = "STEC",
    # "lt_etec"                       = "ETEC lt",
    # "st_etec"                       = "ETEC st",
    # "etec"                          = "ETEC"
  )
  pathogen_labels[["bacteria"]] <- c(
    "aeromonas"                     = "Aeromonas",
    # "ancyclostoma"                  = "Ancyclostoma",
    # "trichuris trichiura"           = "Trichuris",
    # "e.bieneusi"                    = "E. bieneusi",
    # "e.intestinalis"                = "E. intestinalis",
    # "cryptosporidium"               = "Cryptosporidium spp.",
    "salmonella"                    = "Salmonella",
    "h.pylori"                      = "H. pylori",
    "c.jejuni/coli"                 = "C. jejuni/coli",
    "campy pan"                     = "Campylobacter spp.",
    "b.fragilis"                    = "B. fragilis",
    "c.difficile"                   = "C. difficile",
    # "adenovirus f"                  = "Adenovirus 40/41",
    # "norovirus gi"                  = "Norovirus GI",
    # "norovirus gii"                 = "Norovirus GII",
    # "astrovirus"                    = "Astrovirus",
    # "necator"                       = "Necator",
    # "strongyloides"                 = "Strongyloides",
    # "cyclospora"                    = "Cyclospora",
    # "isospora"                      = "Isospora",
    # "e.histolytica"                 = "E. histolytica",
    "m.tb"                          = "M. tuberculosis",
    "v.cholerae"                    = "V. cholerae",
    "shigella & eiec"               = "Shigella spp.",
    # "sapovirus"                     = "Sapovirus",
    # "rotavirus"                     = "Rotavirus"#,
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
    # "salmonella"                    = "Salmonella",
    # "h.pylori"                      = "H. pylori",
    # "c.jejuni/coli"                 = "C. jejuni/coli",
    # "campy pan"                     = "Campylobacter spp.",
    # "b.fragilis"                    = "B. fragilis",
    # "c.difficile"                   = "C. difficile",
    # "adenovirus f"                  = "Adenovirus 40/41",
    # "norovirus gi"                  = "Norovirus GI",
    # "norovirus gii"                 = "Norovirus GII",
    # "astrovirus"                    = "Astrovirus",
    "necator"                       = "Necator",
    "strongyloides"                 = "Strongyloides",
    "cyclospora"                    = "Cyclospora",
    "isospora"                      = "Isospora",
    "e.histolytica"                 = "E. histolytica"#,
    # "m.tb"                          = "M. tuberculosis",
    # "v.cholerae"                    = "V. cholerae",
    # "shigella & eiec"               = "Shigella spp.",
    # "sapovirus"                     = "Sapovirus",
    # "rotavirus"                     = "Rotavirus"#,
    # "eaec"                          = "EAEC",
    # "atypical epec"                 = "aEPEC",
    # "typical epec"                  = "tEPEC",
    # "stec"                          = "STEC",
    # "lt_etec"                       = "ETEC lt",
    # "st_etec"                       = "ETEC st",
    # "etec"                          = "ETEC"
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

plot_pie_chart <- function(d_f, original, 
                           plot_pair_type = "bacteria + bacteria", 
                           my_plot_title = "", 
                           my_caption = ""){
  bar_size <- 0.8

  target_pathogens <- get_target_pairs()

  d_f %>% 
    rowwise() %>% 
    mutate(pair_type = label_pathogen_pair(path1, path2)) %>% ungroup() %>% 
    readable_path_names() %>% 
    rowwise() %>%
    mutate(combined = paste(path1, path2, sep = "+")) %>% 
    filter(
      pair_type == plot_pair_type |
             (plot_pair_type == "bacteria + bacteria" & combined == "EPEC+Cryptosporidium spp.") |
        (plot_pair_type == "bacteria + viruses" & combined == "Norovirus GII+Astrovirus")
           ) %>%
    # filter(combined %in% target_pathogens) %>% 
    View()
  path_combos <- d_f %>% 
    rowwise() %>% 
    mutate(pair_type = label_pathogen_pair(path1, path2)) %>% ungroup() %>% 
    readable_path_names() %>% 
    rowwise() %>%
    mutate(combined = paste(path1, path2, sep = "+")) %>% 
    filter(pair_type == plot_pair_type | 
             (plot_pair_type == "bacteria + bacteria" & combined == "EPEC+Cryptosporidium spp.") |
             (plot_pair_type == "bacteria + viruses" & combined == "Norovirus GII+Astrovirus")) %>%
    filter(combined %in% target_pathogens) %>% 
    distinct(combined) %>% unlist()
  
  
  
  print(path_combos)
  # Build the dataframe that serves as the basis for the whole plot. 
  count_data <- d_f %>% filter(
    actual > MINIMUM_CO
  ) %>% 
    # Watch here, this might remove pairs that shouldn't be removed. 
    readable_path_names() %>% 
    rowwise() %>%
    mutate(combined = paste(path1, path2, sep = "+"))%>% 
    filter(combined %in% path_combos) %>% 
    # Group by pathogen pair, and stool type, not direction
    group_by(combined, stool_type) %>% 
    # Get the sum of all co-occurences, and diarrheal co-occurences
    summarise(diar_co = sum(diar_co), co_all = sum(co_all), .groups = "keep") %>% 
    # It doubles for pathogens pairs that appear in bothDiarrhea and Aymptomatic stools. Take the mean to fix this. 
    ungroup() %>% group_by(combined) %>% summarise(diar_co = mean(diar_co), co_all = mean(co_all), .groups = "keep") %>% 
    mutate(diar_fraction = diar_co/co_all) %>% ungroup() %>% 
    select(combined, diar_co, co_all, diar_fraction) %>% 
    mutate(data_type = bar_size, ordering = diar_co/co_all) %>% ungroup() %>% 
    mutate(combined_label = paste(combined, " (N = ", co_all, ")", sep = "")) 
  
  # Re-arrange the pathogen pairs
  path_combos <- count_data %>% 
    arrange(as.numeric(ordering)) %>% 
    # distinct(combined_label) %>%
    pull(combined_label) 
  
  
  count_data <- count_data %>% select(-diar_fraction)
  
  # Add in the null column to add spacing in the middle chart 
  blank_label <- " "
  
  count_data <- rbind(count_data, 
                      c(blank_label, 0, 1, bar_size, 0, blank_label)
  ) %>% 
    mutate(diar_co = as.numeric(diar_co), co_all = as.numeric(co_all), data_type = as.numeric(data_type), 
           ordering = as.numeric(ordering)) 
  
  count_data$combined_label[is.na(count_data$combined_label)] <- blank_label
  
  title_segment_1 <- "Percent diarrhea where pathogen pairs were found. Pairs were "
  title_segment_2 <- "\nthan expected due to prevalence."
  
  plot_title <- paste(c(title_segment_1,"blah", title_segment_2), collapse = "")
  
  # path_colors_and_combos <- get_pathogens_and_colors(path_combos)

  count_data$combined_label <- factor(reorder(count_data$combined_label, count_data$ordering), ordered = T)
  
  pathogen_colors <- target_pathogens[which(target_pathogens %in% count_data$combined)]
  
  pathogen_colors <- vector(mode = "character", length = nrow(count_data))
  
  View(count_data, title = plot_pair_type)
  # print(target_pathogens)

  count_data <- count_data %>% arrange(ordering)
  
  for(p in 1:nrow(count_data)){
    pathogen_idx <- which(target_pathogens == count_data$combined[p])
    # print(pathogen_idx)
    if(length(pathogen_idx) > 1){
      print(count_data$combined[p])
    }
    # print(as.character(count_data$combined_label[pathogen_idx]))
    pathogen_colors[p] <- as.character(count_data$combined_label[count_data$combined == target_pathogens[pathogen_idx]])
    names(pathogen_colors)[p] <- names(target_pathogens)[pathogen_idx]
  }
  
  final_pie <- count_data %>% ungroup() %>% 
    ggplot(aes(
      # x = factor(reorder(combined_label,  ordering), ordered = T),
      x = combined_label,
      width = data_type, y = diar_co/co_all))+
    geom_bar(aes(
      # fill = factor(reorder(combined_label, ordering), ordered = T)),
      fill = combined_label),
      stat = "identity", 
      position = position_dodge2(preserve = "single"),
    )+
    geom_label(
      aes(
        y = (diar_co/co_all) * 1.01,
        label = ifelse(diar_co > 0, diar_co, ""), 
        size =factor(data_type)),
      label.size = 0,
      hjust = 0.5,
      alpha = 0.8,
      fill = "white",
      fontface = "bold")+
    coord_polar("y", start = 0)+
    geom_label(aes(
      size =factor(data_type),
      label = ifelse(diar_co > 0, paste0(round(diar_co/co_all, 2) * 100, "%"), ""),
      hjust = 0,
      y = 0),
      alpha = 0.8, fill = "white",
      label.size = 0,
      fontface = "bold")+
    scale_size_manual(values = c(5,7), guide = "none")+
    scale_fill_manual(values = rev(names(pathogen_colors)),
                      breaks =  rev(pathogen_colors)
                      # drop = FALSE
                      # na.value = "white"
                      # guide = "none"
    )+
    labs(x = "", y = "", title = my_plot_title, fill = "Pathogen Pair", 
         caption = paste("*", my_caption))+
    ylim(0, 1)+
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size = rel(1.1)),
          legend.title = element_text(size = rel(1.2)),
          plot.title = element_text(size = rel(2)),
          panel.grid.minor.y = element_blank())
  return(final_pie)
}


sort_pathogen_pairs <- function(path1, path2){
  # print(path1)
  # print(path2)
  viruses <- c(
    "aeromonas"                     = "Aeromonas",
    # "ancyclostoma"                  = "Ancyclostoma",
    # "trichuris trichiura"           = "Trichuris",
    # "e.bieneusi"                    = "E. bieneusi",
    # "e.intestinalis"                = "E. intestinalis",
    # "cryptosporidium"               = "Cryptosporidium spp.",
    # "salmonella"                    = "Salmonella",
    # "h.pylori"                      = "H. pylori",
    # "c.jejuni/coli"                 = "C. jejuni/coli",
    # "campy pan"                     = "Campylobacter spp.",
    # "b.fragilis"                    = "B. fragilis",
    # "c.difficile"                   = "C. difficile",
    "adenovirus f"                  = "Adenovirus 40/41",
    "norovirus gi"                  = "Norovirus GI",
    "norovirus gii"                 = "Norovirus GII",
    "astrovirus"                    = "Astrovirus",
    # "necator"                       = "Necator",
    # "strongyloides"                 = "Strongyloides",
    # "cyclospora"                    = "Cyclospora",
    # "isospora"                      = "Isospora",
    # "e.histolytica"                 = "E. histolytica",
    # "m.tb"                          = "M. tuberculosis",
    # "v.cholerae"                    = "V. cholerae",
    # "shigella & eiec"               = "Shigella spp.",
    "sapovirus"                     = "Sapovirus",
    "rotavirus"                     = "Rotavirus"#,
    # "eaec"                          = "EAEC",
    # "atypical epec"                 = "aEPEC",
    # "typical epec"                  = "tEPEC",
    # "stec"                          = "STEC",
    # "lt_etec"                       = "ETEC lt",
    # "st_etec"                       = "ETEC st",
    # "etec"                          = "ETEC"
  )
  
  if(path1 %in% viruses){
    return(c(path2, path1))
  }else if(path2 %in% viruses){
    return(c(path1, path2))
  }else{
    # print("HEre")
    # print(sort(c(path1, path2)))
    return(sort(c(path1, path2)))
  }
  
}

pre_pie_data_prep <- function(d_f){
  final <- d_f %>% 
    # filter_out_nonsymmetric() %>%
    # filter_out_symmetric() %>%
    # filter_out_redundancies() %>%
    filter(actual > MINIMUM_CO) %>%
    # group_by(study, stool_type) %>%
    # mutate(percentile = as.numeric(percentile)) %>% 
    mutate(percentile_rank = rank(percentile, ties.method = "average"), 
           distance_rank = rank(-abs(0.5 - percentile), ties.method = "average"), 
           distance = abs(0.5 - percentile)) %>%
    mutate(percentile_rank_n = percentile_rank/max(percentile_rank), 
           distance_rank_n = distance_rank/max(distance_rank)) %>% 
    mutate(top10_distance = distance_rank < max(distance_rank) * 0.1) %>% 
    ungroup() %>% group_by(path1, path2, stool_type) %>% 
    mutate(top10_both = sum(top10_distance) > 1) %>%  ungroup() %>% 
    # readable_path_names() %>% 
    # filter_out_nonsymmetric() %>% 
    mutate(pathogen_pair = paste(path1, path2, sep = " + ")) %>% 
    # filter(stool_type == s, actual > 10) %>% 
    # select(pathogen_pair, study, stool_type, actual, distance_rank_n) %>% 
    distinct() %>%
    # mutate(direction = ifelse(actual - average > 0, "Higher", "Lower")) %>% # Keep pathogen pairs that both areg higher than the average 
    group_by(pathogen_pair, stool_type) %>%
    mutate(
      ave_rank = mean(distance_rank_n), 
      n_pairs = n_distinct(study)) %>% 
    ungroup() %>% 
    filter(
      n_pairs > 1) %>% 
    # group_by(stool_type) %>% 
    # slice_min(ave_rank, 
    #           n = floor(choose(length(union(D_f$path1, d_f$path2)), 2)* 0.1)
    #           # n = 20
    # ) %>% # 52 is a hard coding 10%, 
    arrange(ave_rank)
  return(final)
}
##### Data Import 
original_df <- import_complete_datasets(parent_dir = "data/") %>% combine_like_pathogens()
all_simple_results <- get_all_simple_results(original_df, parent_dir = "results/pooled_samples/EXP_")

all_simple_results %>% pre_pie_data_prep() %>% 
  pie_plot_all_subsets(original_df)
##### 
