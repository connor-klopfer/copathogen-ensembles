# We want the to keep all the significant pairs together. 

# Simple-> All observations together 

library(dplyr)
library(copathogenTools)

# Updated 02Dec2020

# Loads the most Up-to date experiment for visualisation. 


#### Original Dataset #### 

# original_df <- import_complete_datasets()


##### Simple Representation ####


combine_specs <- function(specs, original_df, ci = 0.95, parent_dir = "../results/EXP_", diar_fraction = T){
  #' Importing List of specifications, 
  
  required_keys <- c("study", "stool_type", "filename", "data_type")
  
  optional_keys <- c("timepoint") 
  
  
  df_list <- list()
  for(s in 1:length(specs)){
    # A Vector of booleans, checks if any of the required columns are missing. 
    key_check <- !required_keys %in% names(specs[[s]])
    
    # If Any of the required keys are mssing, then trhow a flag. 
    if(sum(key_check) > 0){
      stop(paste("ERROR: Missing keys:", required_keys[!key_check]))
    }
    
    if(!"timepoint" %in% names(specs[[s]])){
      specs[[s]][["timepoint"]] <- NA
      temp_timepoint <- "pooled"
    }else{
      temp_timepoint <- specs[[s]][["timepoint"]]
    }
    
    
    temp_file <- paste0(parent_dir, specs[[s]][["filename"]], ".csv")
    temp_study <- specs[[s]][["study"]]
    temp_stool <- specs[[s]][["stool_type"]]
    temp_data_type <- specs[[s]][["data_type"]]
    
    df_list[[s]] <- readr::read_csv(temp_file) %>% 
      as.data.frame() %>% 
      process_ci_df(confidence_interval = ci) 
    if(diar_fraction){
      df_list[[s]] <- df_list[[s]] %>% 
        get_diar_fraction(original_df, temp_study, specs[[s]][["timepoint"]])
    }
    
    df_list[[s]] <- df_list[[s]] %>% 
        mutate(study = temp_study, stool_type = temp_stool, data_type = temp_data_type)
    
    if('country' %in% names(specs[[s]])){
      df_list[[s]] <- df_list[[s]] %>% mutate(country = specs[[s]][['country']])
    }
    
    
    df_list[[s]] <- df_list[[s]] %>% mutate(time_type = temp_timepoint)

  }
  return(do.call(rbind, df_list))
}
  
  
##### Current Results: no Specific Campy MArkers #####
get_all_simple_results <- function(original, ci = 0.95,  
                                   parent_dir = "../results/pooled_samples/EXP_"){
  # Get the results from the simple experimentation. 
  simple_specs <- list()
  
  simple_specs[[1]] <- list(
    "filename" = "17d848d9-f7e0-4cd5-94b9-830b9b83f2b5", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "provide"
  )
  
  simple_specs[[2]] <- list(
    "filename" = "929e1537-00cb-4576-b2c8-579c8c561e65", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "provide"
  )
  
  simple_specs[[3]] <- list(
    # "filename" = "baab7727-da3b-44f0-969b-a06d121c3b96",
    "filename" = "e1b4b5d7-1dd5-4c85-96e7-1d99dc1c6e54", # The Changed to invclue 3 additionl pathogens
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled"
  )
  
  simple_specs[[4]] <- list(
    "filename" = "5b9a8497-c577-4c21-aca1-1a58b55d7f01", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled"
  )
  
  return(combine_specs(simple_specs, original, ci, parent_dir = parent_dir))
}



##### Country Variables #####
get_all_country_results <- function(original){
  country_results <- list()
  
  country_results[[1]] <- list(
    "filename" = "233b8cf2-57c4-4a5f-b020-76cff87e1cec", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Brazil"
  )
  
  country_results[[2]] <- list(
    "filename" = "7fcc2eca-f963-4309-a410-ef032b28db0e", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Brazil"
  )
  
  country_results[[3]] <- list(
    "filename" = "cfabde97-e80a-49dd-b7ec-f9467fe66abc", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "India"
  )
  
  country_results[[4]] <- list(
    "filename" = "494fb628-ef95-4d88-9794-706d8c0b8d0e", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "India"
  )
  
  country_results[[5]] <- list(
    "filename" = "2ef15310-78b9-4893-a562-362b9c1a8b27", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Nepal"
  )
  
  country_results[[6]] <- list(
    "filename" = "28ed0d0f-5d26-4386-8ee6-91a4ff59c835", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Nepal"
  )
  
  country_results[[7]] <- list(
    "filename" = "15e739c4-65f2-4253-bdbc-c44a18874fdb", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Pakistan"
  )
  
  country_results[[8]] <- list(
    "filename" = "b160ef50-dd66-4940-9b2b-11dd5c8f11d0", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Pakistan"
  )
  
  country_results[[9]] <- list(
    "filename" = "62c49381-f042-4dc4-91be-ae3c4f3e2f18", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "South Africa"
  )
  
  country_results[[10]] <- list(
    "filename" = "b46240a3-a14e-4dac-b82c-4e84ba3ac975", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "South Africa"
  )
  
  country_results[[11]] <- list(
    "filename" = "84e8f1f3-12d0-497e-a90d-706bab032982", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Peru"
  )
  
  country_results[[12]] <- list(
    "filename" = "5152d4b2-fea1-4816-9cf4-fecedaab15cf", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Peru"
  )
  
  country_results[[13]] <- list(
    "filename" = "da812d61-4632-4593-b9b3-f4396d848c7f", 
    "stool_type" = "Diarrhea",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Tanzania"
  )
  
  country_results[[14]] <- list(
    "filename" = "2f5a2db8-eab3-41f9-8182-1010d3d4210a", 
    "stool_type" = "Asymptomatic",
    "data_type" = "complete_data", 
    "study" = "maled", 
    "country" = "Tanzania"
  )
  return(
    combine_specs(
      country_results, 
      original, 
      parent_dir = "../data/vacc_trained/appended_datatables/EXP_", diar_fraction = F))
}

# country_results <- get_all_country_results(original_df)

##### OLd Experiement: TBD #####
# simple_exp_specs <- list(
#   list(
#     "file_name" = "EXP_ac8498c6-e73a-4f72-83f0-37e10c479aee",
#     "study" =  "provide",
#     "stool_type" = "Diarrhea"
#   ),
#   list(
#     "file_name" = "EXP_237fb9ed-f2d3-4fa1-9572-7b2b2855ddd9",
#     "study" =  "provide",
#     "stool_type" = "Asymptomatic"
#   ),
#   list(
#     "file_name" = "EXP_952a7a2c-2c46-49ba-a6cb-cd856d4b175b",
#     "study" =  "maled",
#     "stool_type" = "Diarrhea"
#   ),
#   list(
#     "file_name" = "EXP_1303b544-9078-444f-b9ad-8be4b0ce0ece",
#     "study" =  "maled",
#     "stool_type" = "Asymptomatic"
#   )
# )

# all_simple_results <- get_all_simple_results(original_df)


# all_simple_results_old <- get_all_simple_results(original_df)

##### 12 Month Partitions: By Age #####

month_byage_specs <- list()
month_byage_specs[[1]] <- list(
  filename = "2b5b5bc6-042e-4663-96e6-b65e93b52611", 
  study = "provide",
  stool_type = "Diarrhea",
  timepoint = "bin_12", 
  data_type = "complete_data"
)

month_byage_specs[[2]] <- list(
  filename = "fa8456e5-7a57-41cc-84fe-943abeb9b69d", 
  study = "provide",
  stool_type = "Asymptomatic",
  timepoint = "bin_12", 
  data_type = "complete_data"
)

month_byage_specs[[3]] <- list(
  filename = "7cc6a6d0-d8d8-4b87-be8e-7e0b00628e24", 
  study = "maled",
  stool_type = "Diarrhea",
  timepoint = "bin_12", 
  data_type = "complete_data"
)

month_byage_specs[[4]] <- list(
  filename = "d13bd280-e2f5-41fb-891b-8fc92184afcb", 
  study = "maled",
  stool_type = "Asymptomatic",
  timepoint = "bin_12", 
  data_type = "complete_data"
)

get_timeseries_results <- function(original_dataset, specs, ts, parent_dir = "../data/", ci = 0.95){
  #' Append all the results for the configuration model togeher for timeseries models.
  #'
  #' @description Rather than individual datasets and repitiion, loop through all the filenames,
  #' and append the datasets togehter into one large set.
  #'
  #' @param original_dataset the original dataset of qPCR data in a binary representation.
  #' @param parent_dir string of the parent directory where the results are kept. Options are
  #' "" if called from the command line of a script, and ".." if called from a notebook.
  #' @param specs list of lists, were each sub-list has a key for the filename (without .csv extension)
  #' the study, and the stool type.
  #' @param ts string containing the name of the variable to partition the timeseries by
  temp_list <- list()
  for(x in 1:length(specs)){
    temp_list[[x]] <- readr::read_csv(paste(parent_dir, "vacc_trained/appended_datatables/EXP_", specs[[x]][["filename"]], ".csv", sep = "")) %>%
      as.data.frame() %>%
      process_ci_df(confidence_interval = ci) %>%
      get_diar_fraction(original_dataset, specs[[x]][["study"]], ts) %>%
      mutate(study = specs[[x]][["study"]], stool_type = specs[[x]][["stool_type"]])
  }
  return(do.call(rbind, temp_list))

}


# month_byage_results <- get_timeseries_results(original_df, month_byage_specs, "bin_12")

##### By Seasons #####
#' 
#' season_specs <- list(
#'   list(
#'     "file_name" = "EXP_bcca58c1-c5ec-4614-89be-684e14206d39",
#'     "study" =  "provide",
#'     "stool_type" = "Diarrhea"
#'   ),
#'   list(
#'     "file_name" = "EXP_3ce475d4-5af3-4007-9cc5-cdc928a61edf",
#'     "study" =  "provide",
#'     "stool_type" = "Asymptomatic"
#'   ),
#'   list(
#'     "file_name" = "EXP_5e4c7331-f30d-4772-8619-727e36979910",
#'     "study" =  "maled",
#'     "stool_type" = "Diarrhea"
#'   ),
#'   list(
#'     "file_name" = "EXP_04993ee1-c060-4c1a-a822-238a28053d2d",
#'     "study" =  "maled",
#'     "stool_type" = "Asymptomatic"
#'   )
#' )
#' 
#' season_results <- get_timeseries_results(simple_expanded_reduced, "", 
#'                                                season_specs, "season")
#' 
#' ##### Get By Calender Month #####
#' 
#' calmonth_specs <- list(
#'   list(
#'     "file_name" = "EXP_72bddad7-d5de-4e0b-9ef3-db4220955fae",
#'     "study" =  "provide",
#'     "stool_type" = "Diarrhea"
#'   ),
#'   list(
#'     "file_name" = "EXP_3c9236a4-fd7e-48cc-b569-14b03d66477a",
#'     "study" =  "provide",
#'     "stool_type" = "Asymptomatic"
#'   ),
#'   list(
#'     "file_name" = "EXP_68d5e45c-8051-44f0-a60d-4c3a95687926",
#'     "study" =  "maled",
#'     "stool_type" = "Diarrhea"
#'   ),
#'   list(
#'     "file_name" = "EXP_86be39ea-35dc-4b38-9d47-2bb1258a8e97",
#'     "study" =  "maled",
#'     "stool_type" = "Asymptomatic"
#'   )
#' )
#' 
#' calmonth_results <- get_timeseries_results(simple_expanded_reduced, "", 
#'                                            calmonth_specs, "month")
#' 
##### Delete Excess #####
# rm(combine_specs)
# rm(get_all_simple_results)
# rm(get_all_country_results)
