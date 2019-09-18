# this code is to estimate the ability and item parameters using anchor student's response


library(tidyverse)
library(mirt)

old_data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                           "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data", "anchor_responses")
new_data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                           "UF_Algebra_Nation_2018", "Code", "DFN", "New_Real_Data", 'mirt_output')

if(!dir.exists(new_data_path))dir.create(new_data_path)
files <- dir(old_data_path)

for(file in files){
  save_name <- str_sub(file, start = 1L, end = -5L)
  
  data <- read_csv(file.path(old_data_path, file))
  response <- data[, c(-1, -2)]
  
  #using mirt package to estimate
  fit <- mirt(response, model = 1, itemtype = "2PL",
              technical = list(removeEmptyRows=TRUE), SE = TRUE)
  
  irt_params <- coef(fit)[-length(coef(fit))] %>%
    bind_rows()
  irt_params <- irt_params[1:6,] %>%
    t() %>%
    as.data.frame()
  names(irt_params) <- c("a", "a_lower", "a_upper", "d", "d_lower", "d_upper")
  irt_theta <- cbind(fscores(fit), data[, 2])
  
  write_csv(irt_params, path = file.path(new_data_path, paste(save_name, 'item_param.csv', sep = '_')))
  write_csv(irt_theta, path = file.path(new_data_path, paste(save_name, 'ability.csv', sep = '_')))
}