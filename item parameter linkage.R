# this code is for item parameter linkage

library(tidyverse)
library(mirt)

# set old data path and new data path
old_response_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                               "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data")
old_parameter_path <-file.path("~", "Dropbox", "Research", "Research_Assistance",
                               "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data")
  
new_response_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                               "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data", "anchor_responses")
new_parameter_path <-file.path("~", "Dropbox", "Research", "Research_Assistance",
                               "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data", "adjusted_item_params")
  
# get the student who took both old and new items

domains <- c('571', '572', '573', '574', '577', '578', '579', '580', '581', '582')

domain <- domains[1]

# load the parameters of old items and old anchor studnets response

old_response <- read_csv(file.path(old_response_path, 
                                   paste('tys_1718', domain, 'anchor_student_response.csv', sep = '_')))
old_item_parameter <- read_csv(file.path(old_parameter_path,
                                         paste(domain, 'refined_item_param.csv', sep = '_'))) %>%
  dplyr::select(unique_id, avg_a, avg_d)

new_response <- read_csv(file.path(new_response_path, 
                                   paste(domain, 'new_anchor.csv', sep = '_')))
new_item_parameter <- read_csv(file.path(new_parameter_path,
                                         paste(domain, 'adjusted_item_param.csv', sep = '_'))) %>%
  dplyr::select(item_id, avg_a, avg_d)



