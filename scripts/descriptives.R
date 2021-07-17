#' Scripts for descriptive statisitcs and tables. 

library(dplyr)
library(ggplot2)
library(copathogenTools)

##### Functions ##### 
select_target_pathogen_pairs <- function(){
  pathogens_and_colors <- c(
    "campy pan+rotavirus",
    "eaec+norovirus gii",
    "eaec+adenovirus f",
    "b.fragilis+norovirus gii",
    "etec+rotavirus",
    # "#7FC97F" = "B. fragilis+Norovirus GII",
    "shigella & eiec+rotavirus",
    "epec+rotavirus",
    "epec+astrovirus",
    "etec+astrovirus",
    # "#7FC97F" = "EPEC+Rotavirus",
    "norovirus gii+astrovirus",
    "etec+epec",
    "etec+campy pan",
    "epec+campy pan",
    "eaec+campy pan",
    "etec+eaec",
    "epec+b.fragilis",
    "etec+b.fragilis",
    "epec+cryptosporidium",
    "shigella & eiec+campy pan",
    "shigella & eiec+etec"
    # "#9BB5A4" = "Shigella spp.+ETEC",
    # "#9BB5A4" = "Shigella spp.ETEC",
    # "white" = " "
  )
  return(pathogens_and_colors)
}

select_target_pathogens <- function(){
  path_pairs <- select_target_pathogen_pairs()
  split_pairs <- unlist(stringr::str_split(path_pairs, "\\+"))
  
  return(unique(split_pairs))
}

##### 

# Final color choice, light blue and dark green, high saturation. 
two_var_coloring <- c("#1f78b4", "#67D067")

complete_data <- import_complete_datasets(bangladesh_only = T, parent_dir = "data/")

prevalence_plot <- complete_data %>%
  combine_like_pathogens() %>% 
  select(c("participant", "study", "stool_type", select_target_pathogens())) %>%
  group_by(study) %>% mutate(n_obs = n()) %>% ungroup() %>% 
  tidyr::pivot_longer(select_target_pathogens(), names_to = "pathogen", values_to = "present") %>%
  group_by(study, stool_type, pathogen) %>% 
  summarise(p_frac = sum(present, na.rm = T)/mean(n_obs, na.rm = T), n_obs_m = mean(n_obs)) %>%
  # mutate(pathogen = plyr::revalue(pathogen, readable_path_names(.), warn_missing = F)) %>%
  # readable_path_names() %>% 
  mutate(study = ifelse(study == "maled", "MAL-ED", "PROVIDE")) %>%
  # filter_out_symmetric() %>% 
  mutate(path1 = pathogen, path2 = "") %>% 
  readable_path_names() %>% mutate(pathogen = path1) %>% 
  ggplot(aes(x = pathogen, y = p_frac, fill = stool_type))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~study)+
  labs(title = "Proportion of Observations With Pathogen", y = "Fraction of observations where present", x = "", fill = "Stool Type")+
  scale_fill_manual(values = two_var_coloring)+
  coord_flip()+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray"),
        panel.grid = element_line(color = "lightgray"), 
        strip.background = element_rect(fill = "white"), 
        axis.text = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.1)))
png(file.path("figures", "path_prevalence.png"))
print(prevalence_plot)
dev.off()
# ggsave(, print(prevalence_plot))

print("Number of Participants in dataset:")
complete_data %>% 
  group_by(study) %>% 
  summarise(`Number of Participants` = n_distinct(participant))

print("Number of observations")
complete_data %>% 
  group_by(study, stool_type) %>% 
  summarise(`Number of stools` = n())


# Objective: Number of pathogens a participant encountered during the first year of life. 
print("Number of Pathogens Per Participant in the First Year of Life")
complete_data %>% 
  combine_like_pathogens() %>%
  select(-bin_12) %>% 
  group_by(study, stool_type) %>% mutate(total_stools = n()) %>% ungroup() %>% 
  tidyr::pivot_longer(-c("participant", "study", "stool_type", "age", "collection_date", "observation_id", "country", "total_stools"), 
                      names_to = 'pathogen', values_to = 'present') %>% 
  ungroup() %>% 
  filter(present > 0) %>% 
  group_by(study, participant) %>% 
  summarise(n_pathogens = n_distinct(pathogen), total_stools = mean(total_stools)) %>%    # Count the number of pathogens for a single stool
  ungroup() %>% group_by(study) %>% 
  summarise(`AVE Number of Pathogens` = mean(n_pathogens), `SD Number of Pathogens Per Participant` = sd(n_pathogens))

# Percent greater or equal to 2 pathogens in the first year of life. 
print("Percent of Participants with greater or equal to 2 pathogens")
complete_data %>% 
  combine_like_pathogens() %>% 
  select(-bin_12) %>% 
  group_by(study, stool_type) %>% mutate(total_stools = n()) %>% ungroup() %>% 
  tidyr::pivot_longer(-c("participant", "study", "stool_type", "age", "collection_date", "observation_id", "country", "total_stools"), 
                      names_to = 'pathogen', values_to = 'present') %>%
  mutate(appended = paste(observation_id, participant, age, sep = "")) %>% 
  group_by(study, stool_type, appended) %>% 
  summarise(n_pathogens = sum(present, na.rm = T), total_stools = mean(total_stools)) %>%    # Count the number of pathogens for a single stool
  mutate(greater_than_2 = n_pathogens >= 2) %>% 
  ungroup() %>% group_by(study, stool_type) %>%  
  summarise(percent_greater_2 = sum(greater_than_2) / n())


# Mean number of Co-pathogens per stools and the standard deviation of stools . 
print("Mean number of Pathogens per stool and SDEV number of pathogens")
complete_data %>% 
  combine_like_pathogens() %>% 
  tidyr::pivot_longer(-all_of(c("participant", "study", "stool_type", "age", "collection_date", "observation_id", "country", "bin_12")), 
                      names_to = 'pathogen', values_to = 'present') %>% 
  group_by(stool_type, study, observation_id) %>% 
  summarise(n_pathogens = sum(present)) %>% 
  ungroup() %>% 
  group_by(stool_type, study) %>% 
  summarise(`Mean Number of pathogens` = mean(n_pathogens), `STDEV number of pathogens` = sd(n_pathogens)) 



  
  