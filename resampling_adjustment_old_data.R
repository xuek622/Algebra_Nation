# this code is to do re-estimate the item parameters using resampling method

library(tidyverse)
library(mirt)


# get the orginal responses and ability
original_data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                                "UF_Algebra_Nation_2018", "Code", "DFN", "Real_Data")

save_data_path <- file.path(original_data_path, 'refined_theta')


domain_biases <- c(-0.0671, -4.67E-02, -0.06859248, -0.1370402, -0.1943727, -0.201052,
                   -0.09104687, -0.06124629, -0.04811687, -0.04181193)

domains <- c('571', '572', '573', '574', '577', '578', '579', '580', '581', '582')

for(i in 1:10){
  print(domains[i])
 
  response <- read_csv(file = file.path(original_data_path, 
                                        paste('tys_1718', domains[i], 
                                              'anchor_student_response.csv', sep = '_')))
  ability <- read_csv(file = file.path(original_data_path, 
                                       paste('tys_1718', domains[i], 
                                             'ability_mirt.csv', sep = '_')))
  item_param <- read_csv(file = file.path(original_data_path, 
                                          paste('tys_1718', domains[i], 
                                                'item_parameter_estimate_mirt.csv', sep = '_')))
  
  
  diff_theta <- domain_biases[i]
  theta_est_refined <- ability + diff_theta
  
  theta_est_refined <- cbind(response$useraccount_id, theta_est_refined)
  
  write_csv(theta_est_refined, path = file.path(save_data_path, 
                                                paste(domains[i],
                                                      'refined_theta.csv', 
                                                      sep = '_')))
  # resampling student
  
  # prob <- pnorm(theta_est_refined, mean = 0, sd = 1)/pnorm(theta_est_refined, mean = mean(theta_est_refined), sd = sd(theta_est_refined))
  # 
  # B <- 50
  # true_diff <- item_param$d
  # 
  # cover_fit2 <- {}
  # est_fit2 <- {}
  # for(b in 1:B){
  #   # bootstrap
  #   print(b)
  #   sample_new_index <- sample(x = seq_along(theta_est_refined), 
  #                              size = length(theta_est_refined), 
  #                              prob = prob, replace = TRUE)
  #   
  #   response_sample <- response[sample_new_index, c(-1, -2)]
  #   
  #   fit2 <- mirt(response_sample, model = 1, itemtype = "2PL", SE = TRUE,
  #                verbose = FALSE)
  #   
  #   irt_params_fit2 <- coef(fit2)[-length(coef(fit2))] %>%
  #     bind_rows()
  #   irt_params_fit2 <- irt_params_fit2[1:6,] %>%
  #     t() %>%
  #     as.data.frame() %>%
  #     mutate(item = colnames(irt_params_fit2))
  #   names(irt_params_fit2) <- c("a", "a_lower", "a_upper", "d", "d_lower", "d_upper", 'item_id')
  #   
  #   irt_params_fit2 <- irt_params_fit2 %>%
  #     mutate(a_se = (a-a_lower)/1.96,
  #            d_se = (d-d_lower)/1.96)
  #   
  #   est_fit2 <- est_fit2 %>%
  #     rbind(dplyr::select(irt_params_fit2, a, d, a_se, d_se, item_id))
  #   
  #   # estimation comparison
  #   #cover_fit2 <- c(cover_fit2, sum(true_diff$Dffclt > irt_params_fit2$d_lower & true_diff$Dffclt < irt_params_fit2$d_upper) + 1)
  # }
  # 
  # #cover_fit2 <- cover_fit2/B
  # 
  # est_fit2_summary <- est_fit2 %>%
  #   group_by(item_id) %>%
  #   summarise(avg_a = mean(a), avg_a_se = mean(a_se), 
  #             avg_d = mean(d), avg_d_se = mean(d_se), 
  #             sd_a = sd(a), sd_d = sd(d), 
  #             sd_a_se = sd(a_se), sd_d_se = sd(d_se))
  # 
  # write_csv(est_fit2_summary, path = file.path(save_item_param, paste(domains[i], 'adjusted_item_param.csv', sep = '_')))

  }
