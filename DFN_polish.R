library(tidyverse)
library(mirt)
library(data.table)
library(keras)

data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Data")

save_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Simulated_Data")

domains = c("571", "572", "573", "574", "577", "578", "579", "580", "581", "582")

domain <- domains[1]

response <- read.csv(file.path(save_path, paste(domain, "est_theta_response.csv", sep = '_')))


#
creat_dfn <- function(n_input){
  
  input_layer <- layer_input(shape = c(n_input))
  hidden_1 <- layer_dense(units = 100, activation = 'relu', 
                          activity_regularizer = )
  
  
  
}
