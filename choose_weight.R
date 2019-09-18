library(tidyverse)

files <- dir()

files <- files[grepl(pattern = 'weighted_estimate.csv', files)]

for (file in files) {
  tab <- read_csv(file = file) %>%
    mutate(val_loss_total = `# validation_loss_theta` + validation_loss_response,
           train_loss_total = train_loss_theta + training_loss_response,
           loss_total = val_loss_total + train_loss_total) 
  val_loss_total_cum <- c(0, tab$val_loss_total[-20] - tab$val_loss_total[-1])
  #val_loss_theta_cum <- c(0, tab$validation_loss_theta[-20] - tab$validation_loss_theta[-1])
  val_loss_response_cum <- c(0, tab$validation_loss_response[-20] - tab$validation_loss_response[-1])
  
  loss_total_cum <- c(0, tab$loss_total[-20] - tab$loss_total[-1])
  
 # min_weight <- tab$weight[which.min(tab$ability_diff)]
  
  tab %>%
    mutate(val_loss_total_cum = val_loss_total_cum,
           val_loss_response_cum = val_loss_response_cum,
           #val_loss_theta_cum = val_loss_theta_cum,
           loss_total_cum = loss_total_cum) %>%
    ggplot() +
    geom_line(aes(x = weight, y = validation_loss_response), color = 'blue') +
    geom_line(aes(x = weight, y = `# validation_loss_theta`), color = 'orange') +
    geom_line(aes(x = weight, y = val_loss_total), color = 'green') +
    geom_line(aes(x = weight, y = ability_avg), color = 'red') +
    geom_hline(yintercept = mean(val_loss_response_cum)) #+
    #geom_vline(xintercept = min_weight)
  
  ggsave(paste(filename = str_sub(file, 1, 21), '.pdf'), width = 10, height = 5)
}
