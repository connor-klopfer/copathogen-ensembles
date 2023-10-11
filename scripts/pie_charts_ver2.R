#' Specific Pie script
#' 
#' Ross gave me a list of pathogens to use in the pai chart plots. The coding is different so I need 
#' to adjust accordingly 
#' 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggtext)
library(copathogenTools)

source("scripts/load_all_results.R", echo = F)

MINIMUM_CO <- 10

##### Functions #####

pie_plot_all_subsets <- function(d_f, original){
  # Plot all the subsets together. 
  
  bb_plot <- plot_pathogen_bar_chart(d_f, original, 
                            plot_pair_type = "bacteria + bacteria", 
                            # my_plot_title = "Bacteria + bacteria pairs and their burden of diarrhea"#, 
                            # my_caption = "One bacteria + parasite pair included."
                            )
  bv_plot  <- plot_pathogen_bar_chart(d_f, original, plot_pair_type = "bacteria + viruses", 
                             # my_plot_title = "Bacteria + virus pairs and their burden of diarrhea"#, 
                             # my_caption = "One virus + virus pair included."
                             )
  
  # For curved bar charts: 
  #   height: 10,
  #   width: 12
  
  # plot all. 
  pdf('figures/copathogen_bar_charts_BB_alt2.pdf', onefile = TRUE, 
      height = 6, 
      width = 10)
  print(bb_plot)
  dev.off()
  
  pdf('figures/copathogen_bar_charts_BV_alt2.pdf', onefile = TRUE, 
      height = 6, 
      width = 10)
  print(bv_plot)
  dev.off()
  
}


get_target_pairs <- function(){
  #' For indentifying specific pathogens to use in the manuscript based on what 
  #' Ross ID'd 
  #' 
  pathogens_and_colors <- c(
    "#ddb310" = "*Campylobacter* spp.+Rotavirus",
    "#5eccab" = "EAEC+Norovirus GII",
    "#e41a1c" = "EAEC+Adenovirus 40/41",
    "#1f78b4" = "*B. fragilis*+Norovirus GII",
    "#00beff" = "ETEC+Rotavirus",
    "#33a02c" = "*Shigella* spp.+Rotavirus",
    "#999999" = "EPEC+Rotavirus",
    "#b15928" = "EPEC+Astrovirus",
    "#984ea3" = "ETEC+Astrovirus",
    "#fb49b0" = "Norovirus GII+Astrovirus",
    "#C8B1C7" = "ETEC+EPEC",
    "#f781bf" = "ETEC+*Campylobacter* spp.",
    "navy" = "EPEC+*Campylobacter* spp.",
    "#537eff" = "EAEC+*Campylobacter* spp.",
    "#ff7f00" = "ETEC+EAEC",
    "orange" = "EPEC+*B. fragilis*",
    "#4daf4a" = "ETEC+*B. fragilis*",
    "gray32" = "EPEC+*Cryptosporidium* spp.",
    "#FDD58C" = "*Shigella* spp.+*Campylobacter* spp.",
    "#9BB5A4" = "*Shigella* spp.+ETEC",
    "#9BB5A4" = "*Shigella* spp.+ETEC",
    "#9BB5A4" = "*Shigella* spp.ETEC",
    "white" = " "
    
  )
  return(pathogens_and_colors)
}


label_pathogen_pair <- function(path1, path2){
  pathogen_labels <- list()
  # print(path1)
  # print(path2)
  # print("Next")
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


generate_pi_chart_data <- function(d_f, path_combos){
  return(d_f %>% filter(
    actual > MINIMUM_CO
  ) %>% 
    # Watch here, this might remove pairs that shouldn't be removed. 
    readable_path_names(formatted = T) %>% 
    rowwise() %>%
    mutate(combined = paste(path1, path2, sep = "+"))%>% 
    filter(combined %in% path_combos) %>% 
    # Group by pathogen pair, and stool type, not direction
    group_by(combined, stool_type) %>% 
    # Get the sum of all co-occurences, and diarrheal co-occurences
    summarise(diar_co = sum(diar_co), co_all = sum(co_all), n_obs = sum(n_obs), n_diar = sum(n_obs * diar_fraction),  n_asym = sum(n_obs * (1-diar_fraction)), .groups = "keep") %>% 
    # It doubles for pathogens pairs that appear in bothDiarrhea and Aymptomatic stools. Take the mean to fix this. 
    ungroup() %>% group_by(combined) %>% summarise(diar_co = mean(diar_co), co_all = mean(co_all), n_obs = mean(n_obs), n_diar = mean(n_diar), n_asym = mean(n_asym), .groups = "keep") %>% 
    # mutate(n_diar = n_obs * diar_fraction, n_asym = n_obs * (1-diar_fraction)) %>% 
    mutate(diar_co_proportion = diar_co / n_diar, asym_co_proportion = (co_all-diar_co)/n_asym) %>% 
    mutate(
      diar_CI_lower = Hmisc::binconf(diar_co, n_diar, method ="wilson")[2], 
      diar_CI_higher = Hmisc::binconf(diar_co, n_diar, method = "wilson")[3],
      asym_CI_lower = Hmisc::binconf(co_all-diar_co, n_asym, method ="wilson")[2], 
      asym_CI_higher = Hmisc::binconf(co_all-diar_co, n_asym, method = "wilson")[3],
      low_alt = diar_CI_lower / asym_CI_higher,
      high_alt = diar_CI_higher / asym_CI_lower
      ) %>% # CIS here
    mutate(alt_metric = diar_co_proportion/asym_co_proportion) %>% 
    mutate(diar_fraction = diar_co/co_all) %>% ungroup() %>% 
    select(combined, diar_co, co_all, diar_fraction, alt_metric, diar_co_proportion, asym_co_proportion, diar_CI_lower, diar_CI_higher, asym_CI_lower, asym_CI_higher, low_alt, high_alt) )
  
}

plot_pie_chart <- function(d_f, original, 
                           plot_pair_type = "bacteria + bacteria", 
                           my_plot_title = "", 
                           my_caption = ""){
  bar_size <- 0.8

  target_pathogens <- get_target_pairs()

  path_combos <- d_f %>% 
    rowwise() %>% 
    mutate(pair_type = label_pathogen_pair(path1, path2)) %>% ungroup() %>% 
    readable_path_names(formatted = T) %>% 
    rowwise() %>%
    mutate(combined = paste(path1, path2, sep = "+")) %>% 
    filter(pair_type == plot_pair_type | 
             (plot_pair_type == "bacteria + bacteria" & combined == "EPEC+*Cryptosporidium* spp.") |
             (plot_pair_type == "bacteria + viruses" & combined == "Norovirus GII+Astrovirus")) %>%
    filter(combined %in% target_pathogens) %>% 
    distinct(combined) %>% unlist()
  
  print(path_combos)
  
  # Build the dataframe that serves as the basis for the whole plot. 
  count_data <- d_f %>% generate_pi_chart_data(path_combos)%>% 
    mutate(data_type = bar_size, ordering = diar_co/co_all) %>% ungroup() %>% 
    # mutate(combined_label = paste(combined, " (N = ", co_all, ")", sep = "")) 
    mutate(combined_label = combined) 
  
  # Re-arrange the pathogen pairs
  path_combos <- count_data %>% 
    arrange(as.numeric(ordering)) %>% 
    # distinct(combined_label) %>%
    pull(combined_label) 
  
  
  count_data <- count_data %>% select(-diar_fraction)
  
  # Add in the null column to add spacing in the middle chart 
  blank_label <- " "
  
  count_data <- rbind(count_data, 
                      c(blank_label, 0, 1, 0, bar_size, 0, blank_label)
  ) %>% 
    mutate(diar_co = as.numeric(diar_co), co_all = as.numeric(co_all), data_type = as.numeric(data_type), 
           ordering = as.numeric(ordering)) 
  
  count_data$combined_label[is.na(count_data$combined_label)] <- blank_label
  
  title_segment_1 <- "Percent diarrhea where pathogen pairs were found. Pairs were "
  title_segment_2 <- "\nthan expected due to prevalence."
  
  plot_title <- paste(c(title_segment_1,"blah", title_segment_2), collapse = "")
  plot_title <- 
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

  final_pie <- count_data %>% ungroup() %>%  mutate(alt_metric = as.numeric(alt_metric)) %>%
    ggplot(aes(
      # x = factor(reorder(combined_label,  ordering), ordered = T),
      x = combined_label,
      width = data_type, 
      # y = diar_co/co_all, 
      y = alt_metric
      ))+
    geom_bar(aes(
      # fill = factor(reorder(combined_label, ordering), ordered = T)),
      fill = combined_label),
      stat = "identity", 
      position = position_dodge2(preserve = "single"),
    )+
    geom_label(
      aes(
        y = (alt_metric) * 1.01,
        label = ifelse(diar_co > 0, diar_co, ""), 
        size =factor(data_type)),
      label.size = 0,
      hjust = 0.5,
      alpha = 0.8,
      fill = "white",
      fontface = "bold")+
    # coord_polar("y", start = 0)+
    geom_label(aes(
      size =factor(data_type),
      label = ifelse(diar_co > 0, paste0(round(alt_metric) * 100, "%"), ""),
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
    # ylim(0, 1)+
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          legend.text = element_markdown(size = rel(1.5)),
          legend.title = element_text(size = rel(1.7)),
          legend.key.height = unit(1, 'cm'),
          plot.title = element_text(size = rel(2)),
          panel.grid.minor.y = element_blank())
  return(final_pie)
}


plot_pathogen_bar_chart <- function(d_f, original, 
         plot_pair_type = "bacteria + bacteria", 
         my_plot_title = "", 
         my_caption = ""){
  bar_size <- 0.8
  
  target_pathogens <- get_target_pairs()
  
  path_combos <- d_f %>% 
    rowwise() %>% 
    mutate(pair_type = label_pathogen_pair(path1, path2)) %>% ungroup() %>% 
    readable_path_names(formatted = T) %>% 
    rowwise() %>%
    mutate(combined = paste(path1, path2, sep = "+")) %>% 
    filter(pair_type == plot_pair_type | 
             (plot_pair_type == "bacteria + bacteria" & combined == "EPEC+*Cryptosporidium* spp.") |
             (plot_pair_type == "bacteria + viruses" & combined == "Norovirus GII+Astrovirus")) %>%
    filter(combined %in% target_pathogens) %>% 
    distinct(combined) %>% unlist()
  
  print(path_combos)
  
  # Build the dataframe that serves as the basis for the whole plot. 
  count_data <- d_f %>% generate_pi_chart_data(path_combos)%>% 
    mutate(data_type = bar_size, ordering = diar_co/co_all) %>% ungroup() %>% 
    mutate(combined_label = paste(combined, " (N = ", co_all, ")", sep = ""))
    # mutate(combined_label = combined) 
  
  # Re-arrange the pathogen pairs
  path_combos <- count_data %>% 
    arrange(as.numeric(ordering)) %>% 
    # distinct(combined_label) %>%
    pull(combined_label) 
  
  
  count_data <- count_data %>% select(-diar_fraction)
  
  # Add in the null column to add spacing in the middle chart 
  blank_label <- " "
  
  count_data <- rbind(count_data#,
                      #                     c(blank_label, 0, 1, bar_size, 0, blank_label)
  ) %>% 
    mutate(diar_co = as.numeric(diar_co), co_all = as.numeric(co_all), data_type = as.numeric(data_type), 
           ordering = as.numeric(ordering)) 
  
  count_data$combined_label[is.na(count_data$combined_label)] <- blank_label
  
  title_segment_1 <- "Percent diarrhea where pathogen pairs were found. Pairs were "
  title_segment_2 <- "\nthan expected due to prevalence."
  
  plot_title <- paste(c(title_segment_1,"blah", title_segment_2), collapse = "")
  
  count_data$combined_label <- factor(reorder(count_data$combined_label, count_data$ordering), ordered = T)
  
  pathogen_colors <- target_pathogens[which(target_pathogens %in% count_data$combined)]
  
  pathogen_colors <- vector(mode = "character", length = nrow(count_data))
  
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
  
  # View(count_data, title = paste("count_data", plot_pair_type))
  print(paste("Results for", plot_title))
  count_data %>% ungroup() %>% 
    mutate(
      alt_metric = as.numeric(alt_metric), 
      diar_co_proportion = as.numeric(diar_co_proportion), 
      asym_co_proportion = as.numeric(asym_co_proportion)) %>%
    mutate(
      barLabel = paste(round(diar_co_proportion, 3)*100, "% / ", round(asym_co_proportion, 3)*100, "%", sep = "")) %>% 
    print()
  
  final_pie <- count_data %>% ungroup() %>% mutate(alt_metric = as.numeric(alt_metric), diar_co_proportion = as.numeric(diar_co_proportion), asym_co_proportion = as.numeric(asym_co_proportion)) %>%
    # rowwise() %>% 
    mutate(barLabel = paste(round(diar_co_proportion, 3)*100, "% / ", round(asym_co_proportion, 3)*100, "%", sep = "")) %>% 
    ggplot(aes(
      # x = factor(reorder(combined_label,  ordering), ordered = T),
      x = combined_label,
      width = data_type, y = alt_metric))+
    geom_bar(aes(
      # fill = factor(reorder(combined_label, ordering), ordered = T)),
      fill = combined_label),
      stat = "identity", 
      position = position_dodge2(preserve = "single"),
    )+
    # geom_hline(aes(yintercept = 0.5), linetype = "dashed", alpha = 0.6, gray = "gray30", size = 1.2)+
    geom_label(
      aes(
        y = (alt_metric) * 1.01,
        label = ifelse(diar_co > 0, barLabel, ""), 
        size =factor(data_type)),
      label.size = 0,
      hjust = -.08,
      alpha = 0.8,
      fill = "white",
      fontface = "bold")+
    # coord_polar("y", start = 0)+
    # geom_label(aes(
    #   size = factor(data_type),
    #   label = ifelse(diar_co > 0, paste0(round(diar_co/co_all, 2) * 100, "%"), ""),
    #   hjust = 0,
    #   y = 0),
    #   alpha = 0.8, fill = "white",
    #   label.size = 0,
    #   fontface = "bold")+
    scale_size_manual(values = c(5,7), guide = "none")+
    scale_fill_manual(values = rev(names(pathogen_colors)),
                      breaks =  rev(pathogen_colors),
                      # drop = FALSE
                      # na.value = "white"
                      guide = "none"
    )+
    labs(
      y = "Ratio of co-infection rate in diarrheal stools compared \nto co-infection rate in asymptomatic stools", 
      # y = "Percent of Co-occurences found in Diarrhea", 
      x = "", title = my_plot_title, fill = "Pathogen Pair")+
    ylim(0, 11)+
    theme(
      axis.text.y = element_markdown(size = rel(1.5)),
      axis.ticks.y = element_line(color = "black"),
      axis.text.x = element_text(size = rel(1.2)),
      panel.background = element_rect(fill = NA, color = "black"),
      legend.text = element_markdown(size = rel(1.5)),
      legend.title = element_text(size = rel(1.7)),
      legend.key.height = unit(1, 'cm'),
      panel.grid.major = element_line(color = "gray"),
      plot.title = element_text(size = rel(2)),
      panel.grid.minor.y = element_line(color = "gray"))+
    coord_flip()
  return(final_pie)
}

sort_pathogen_pairs <- function(path1, path2){
  # print(path1)
  # print(path2)
  viruses <- c(
    "aeromonas"                     = "Aeromonas",
    "adenovirus f"                  = "Adenovirus 40/41",
    "norovirus gi"                  = "Norovirus GI",
    "norovirus gii"                 = "Norovirus GII",
    "astrovirus"                    = "Astrovirus",
    "sapovirus"                     = "Sapovirus",
    "rotavirus"                     = "Rotavirus"#,
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
    mutate(pathogen_pair = paste(path1, path2, sep = " + ")) %>% 
    distinct() %>%
    # mutate(direction = ifelse(actual - average > 0, "Higher", "Lower")) %>% # Keep pathogen pairs that both areg higher than the average 
    group_by(pathogen_pair, stool_type) %>%
    mutate(
      ave_rank = mean(distance_rank_n), 
      n_pairs = n_distinct(study)) %>% 
    ungroup() %>% 
    filter(
      n_pairs > 1) %>% 
    arrange(ave_rank)
  return(final)
}
##### Data Import 
original_df <- import_complete_datasets(parent_dir = "data/") %>% combine_like_pathogens()
all_simple_results <- get_all_simple_results(original_df, parent_dir = "results/pooled_samples")

all_simple_results %>% pre_pie_data_prep() %>%
  pie_plot_all_subsets(original_df)
##### 
