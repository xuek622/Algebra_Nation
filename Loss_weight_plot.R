library(tidyverse)

data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Refined_Theta")

files <- dir(data_path)
files <- files[grepl('_weighted_estimate.csv', files)]

est_file <- NULL

for(sub_file in files){
  domain <- stringr::str_split(sub_file, '_')[[1]][1]
  
  domain_estimation <- read.csv(file = file.path(data_path, sub_file), header = TRUE)
  
  #names(domain_estimation) <- c("Validation Loss", "Theta Estimates")
  
 
  domain_estimation <- domain_estimation %>%
    mutate(domain_id = domain)
  
  est_file <- rbind(est_file, domain_estimation)
}

names(est_file)[1] <- 'ability_true'


true_ablity <- c(.069, .159, .203, .152, .178,
                 .228, .178, .218, .241, .312)

est_ablity <- c(.063, .141, .198, .160, .180, 
                .232, .171, .207, .241, .320)

domain <- c('571', '572', '573', '574', '577',
               '578', '579', '580', '581', '582')

est_file_all <- est_file %>% 
  mutate(validation_loss = validation_loss_response + validation_loss_theta) %>%
  gather(key = 'key', value = 'value', -weight, -domain_id)

ggplot(est_file_all, aes(x = weight, y = value, color = key)) +
  geom_line() +
  facet_wrap(~domain_id, ncol = 5)


x_weight <- est_file %>%
  group_by(domain_id) %>%
  filter(`Validation Loss` == min(`Validation Loss`))

abilities <- list(true_ablity, est_ablity, domain) %>%
  as.data.frame()

names(abilities) <- c('True Ability', 'Estimates', 'domain_id')
#abilities$domain_id <- as.character(abilities$domain_id)

cols <- c('Validation Loss Curve' = "#D55E00", 'Estimate Curve' = "#56B4E9", 
          'True Theta' = '#000000', 'MSE of Estimates' = 'blue')
line_types <- c('Validation Loss Curve' = "solid", 'Estimate Curve' = "solid", 
          'True Theta' = 'dashed')
shapes <- c('Optimized Estimate' = 1, 'Minimum Validation Loss' = 4)
est_file_full <- est_file %>%
  right_join(abilities) %>%
  mutate(MSE = (`True Ability`-`Theta Estimates`)^2) %>%
  dplyr::select(domain_id, weights_theta, `Validation Loss`, `Theta Estimates`, MSE) %>%
  gather(key = type, value = value, -domain_id, -weights_theta) 

ggplot() +
  geom_line(aes(x = weights_theta, y = value, colour = 'Validation Loss Curve'),
            data = filter(est_file_full, type == 'Validation Loss')) +
  geom_line(aes(x = weights_theta, y = value, colour = 'Estimate Curve'),
            data = filter(est_file_full, type == 'Theta Estimates')) +
  geom_line(aes(x = weights_theta, y = value, colour = 'MSE of Estimates'), 
             data = filter(est_file_full, type == 'MSE'), 
            linetype = 'dashed') +
  geom_hline(aes(yintercept = `True Ability`, colour = 'True Theta'), 
             data = abilities, linetype = 'dashed') +
  geom_point(aes(x = weights_theta, y = `Theta Estimates`, shape = 'Optimized Estimate'), 
             data = x_weight, 
             colour = 'red') +
  geom_point(aes(x = weights_theta, y = `Validation Loss`, shape = 'Minimum Validation Loss'), data = x_weight, 
             colour = 'red') +
  geom_vline(aes(xintercept = weights_theta), data = x_weight,
             colour = 'red', linetype = 'dotted') +
  facet_wrap(.~domain_id, ncol = 5)+
  scale_color_manual(name = 'Curves', values = cols) +
  scale_shape_manual(name = 'Points', values = shapes) + 
  labs(x = 'Weight of'~MSE(hat(theta), phi(tilde(Theta)))) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))
  
  

save_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "plot")
ggsave(filename = file.path(save_path, 'loss_est.pdf'), height = 10, width = 14)
  
