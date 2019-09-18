# this code is to do re-estimate the item parameters using resampling method

library(tidyverse)
library(mirt)


# get the orginal responses and ability
response_data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                           "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data", "anchor_responses")

ability_data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                           "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data", 'mirt_output')

save_item_param <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                             "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data", 'adjusted_item_params')
if(!dir.exists(save_item_param))dir.create(save_item_param)

domain_biases <- c(0.1543, 0.1527, 0.1466, 0.2428, 0.3668, 0.2049, 0.1144, 0.2192, 0.0576, 0.1318)

domains <- c('571', '572', '573', '574', '577', '578', '579', '580', '581', '582')

original_data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                                "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data", 
                                "refined_theta")


for(i in 1:10){
  print(domains[i])
  response <- read_csv(file = file.path(response_data_path, 
                                        paste(domains[i], 'new_anchor.csv', sep = '_')))
  ability <- read_csv(file = file.path(ability_data_path, 
                                       paste(domains[i], 'new_anchor_ability.csv', sep = '_')))
  names(ability) <- c('est_ability', 'userID')
  
  item_param <- read_csv(file = file.path(ability_data_path, 
                                          paste(domains[i], 'new_anchor_item_param.csv', sep = '_')))
  
  
  diff_theta <- domain_biases[i] # the bias
  ability$est_ability <- ability$est_ability + diff_theta
  
  
  old_theta_refined <- read_csv(file = file.path(save_data_path, 
                                                 paste(domains[i], 'refined_theta.csv', 
                                                                       sep = '_')))
  names(old_theta_refined) <- c('userID', 'est_ability')
  
  common_theta <- inner_join(ability, old_theta_refined, by = c("userID"))
  print(nrow(common_theta))
  # diff_theta # difference between the new data and old data
  
  
  theta_est_refined <- ability$est_ability
  # resampling student
    
  prob <- pnorm(theta_est_refined, mean = 0, sd = 1)/pnorm(theta_est_refined, mean = mean(theta_est_refined), sd = sd(theta_est_refined))
  
  B <- 50
  true_diff <- item_param$d
  
  cover_fit2 <- {}
  est_fit2 <- {}
  for(b in 1:B){
    # bootstrap
    print(b)
    sample_new_index <- sample(x = seq_along(theta_est_refined), 
                               size = length(theta_est_refined), 
                               prob = prob, replace = TRUE)
    
    response_sample <- response[sample_new_index, c(-1, -2)]
    
    fit2 <- mirt(response_sample, model = 1, itemtype = "2PL", SE = TRUE,
                 verbose = FALSE)
    
    irt_params_fit2 <- coef(fit2)[-length(coef(fit2))] %>%
      bind_rows()
    irt_params_fit2 <- irt_params_fit2[1:6,] %>%
      t() %>%
      as.data.frame() %>%
      mutate(item = colnames(irt_params_fit2))
    names(irt_params_fit2) <- c("a", "a_lower", "a_upper", "d", "d_lower", "d_upper", 'item_id')
    
    irt_params_fit2 <- irt_params_fit2 %>%
      mutate(a_se = (a-a_lower)/1.96,
             d_se = (d-d_lower)/1.96)
    
    est_fit2 <- est_fit2 %>%
      rbind(dplyr::select(irt_params_fit2, a, d, a_se, d_se, item_id))
    
    # estimation comparison
    #cover_fit2 <- c(cover_fit2, sum(true_diff$Dffclt > irt_params_fit2$d_lower & true_diff$Dffclt < irt_params_fit2$d_upper) + 1)
  }
  
  #cover_fit2 <- cover_fit2/B
  
  est_fit2_summary <- est_fit2 %>%
    group_by(item_id) %>%
    summarise(avg_a = mean(a), avg_a_se = mean(a_se), 
              avg_d = mean(d), avg_d_se = mean(d_se), 
              sd_a = sd(a), sd_d = sd(d), 
              sd_a_se = sd(a_se), sd_d_se = sd(d_se))
  
  write_csv(est_fit2_summary, path = file.path(save_item_param, paste(domains[i], 'adjusted_item_param.csv', sep = '_')))
}
