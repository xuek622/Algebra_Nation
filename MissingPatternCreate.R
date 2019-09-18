# missing response simulation
library(tidyverse)
library(mirt)
library(data.table)

data_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Data")

save_path <- file.path("~", "Dropbox", "Research", "Research_Assistance",
                       "UF_Algebra_Nation_2018", "Code", "DFN", "Simulated_Data")


sub_data <- fread(file = file.path(data_path, "sub_data_removing_items.csv")) %>%
  as.tibble() %>%
  filter(!is.na(dev_scale_score)) %>%
  #dplyr::select(response, dev_scale_score) %>%
  mutate_at(vars("dev_scale_score"), scale) 
  #separate(col = "domain_unique_id", into = c('domain_id', 'unique_item_id'))


#create three sub dataset
sub_data_skip_domain <- sub_data %>%
  filter(!is.na(dev_scale_score)) %>%
  mutate(skip_domain = response == "domain_no_taken") %>%
  dplyr::select(-achievement_level, -response) %>%
  separate(col = domain_unique_id, into = c('domain', "unique_id")) %>%
  distinct(useraccount_id, domain, dev_scale_score, skip_domain) %>%
  mutate_at(vars("domain"), as.numeric)


sub_data_answer_all <- sub_data %>%
  filter(response != "domain_no_taken") %>%
  separate(col = domain_unique_id, into = c('domain', 'unique_id')) %>%
  mutate(answered = response %in% c('0', '1')) %>%
  group_by(domain, useraccount_id, dev_scale_score) %>%
  summarise(n_answered = sum(answered), n_item = n_distinct(unique_id)) %>%
  mutate(answer_all = n_answered == n_item)


sub_data_not_response <- sub_data %>%
  filter(response != "domain_no_taken") %>% # removing students who skip domain
  separate(col = domain_unique_id, into = c('domain', 'unique_id')) %>%
  left_join(sub_data_answer_all[c("useraccount_id", "domain", "answer_all")],
            by = c("useraccount_id", "domain")) %>%
  filter(!answer_all) %>% # removing students who answer all items within a domain
  distinct(useraccount_id, domain, unique_id, response, dev_scale_score) %>%
  mutate(no_response = response %in% c("item_no_shown", "Unfinished"))


# simulate data for each domain
domains <- unique(sub_data_skip_domain$domain) %>%
  as.character()

irt_params_mirt1 <- {}
irt_theta_mirt1 <- {}
irt_param_compare_real <- {}
irt_theta_compare_real <- {}

set.seed(1234)
N_stu <- n_distinct(sub_data$useraccount_id)
theta <- rnorm(N_stu) %>%
  data.frame()
names(theta) <- "dev_scale_score"

for(domain in domains){
  cat(domain, '\n')
  cat('predicting the missing probability... \n')
  
  
  # -1) create logistic model to classify if a student will skip a domain (no response to all items)
  model_skip_domain <- glm(skip_domain ~ as.vector(dev_scale_score),
                           family = binomial(link = 'logit'),
                           data = filter(sub_data_skip_domain,
                                         domain == domain))
  
  # -2) create logistic model to classify if a student will answer all items within a domain
  model_answer_all <- glm(answer_all ~ as.vector(dev_scale_score),
                          family = binomial(link = 'logit'),
                          data = filter(sub_data_answer_all,
                                        domain == domain))
  
  # -3) create logistic model to predict if a student will provide no-response to an item
  #     because of two reasons: 1) item no shown;  2) Unfinished
  model_no_response <- glm(no_response ~ as.vector(dev_scale_score),
                           family = binomial(link = 'logit'),
                           data = filter(sub_data_not_response,
                                         domain == domain))
  
  
  # load irt parameters
  irt_parmas <- fread(file = file.path(save_path, paste(domain, "irt.cvs", sep = ""))) %>%
    as.tibble() %>%
    filter(Dffclt < 5 & Dffclt > -5)

  a <- as.matrix(irt_parmas$Dscrmn, ncol = 1)
  d <- as.matrix(irt_parmas$Dffclt, ncol = 1)
  
  # simulate responses
  cat("simulating response matrix ...\n")
  #set.seed(111111)
  simulated_data <- mirt::simdata(a = a, d = d, N = N_stu,
                                  itemtype = '2PL', Theta = as.matrix(theta))
  
  
  cat("creating missing pattern ...\n")
  # create missing pattern matrix
  missing_pattern = matrix(FALSE, ncol = ncol(simulated_data), nrow = nrow(simulated_data))
  skip_domain = predict( object = model_skip_domain, newdata = theta, type = 'response') >
    runif(N_stu)
  missing_pattern[skip_domain, ] <- TRUE
  
  #set.seed(888888)
  answer_all_item <- predict(object = model_answer_all, newdata = filter(theta, !skip_domain), type = 'response') > 
    runif(sum(!skip_domain))
  
  skip_domain[!skip_domain] <- answer_all_item
  
  no_response <-  predict(object = model_answer_all, newdata = filter(theta, !skip_domain), type = 'response') %>%
    rep(ncol(missing_pattern)) %>%
    matrix(ncol = ncol(missing_pattern))
  
  threhold <- runif(nrow(no_response)*ncol(no_response)) %>%
    matrix(ncol = ncol(missing_pattern))
  
  missing_pattern[!skip_domain] <- no_response > threhold
  
  simulated_data[missing_pattern]  <- NA
  
  idx <- apply(simulated_data, 2, mean, na.rm = T) == 1
  if(any(idx)){
    simulated_data[sample(1:N_stu, 100), idx] <- 0
  }
  
  
  # save simulated responses
  write.csv(x = data.frame(simulated_data), 
            file = file.path(save_path, paste(domain, "simulate_response.csv", sep = "_")))
  
  #using mirt package to estimate
  cat("estimation using mirt ...\n")
  mirt1 <- mirt(simulated_data, model = 1, itemtype = "2PL",
                technical = list(removeEmptyRows=TRUE), SE = TRUE)
  
  irt_params_mirt1 <- coef(mirt1)[-length(coef(mirt1))] %>%
    bind_rows()
  irt_params_mirt1 <- irt_params_mirt1[1:6,] %>%
    t() %>%
    as.data.frame()
  names(irt_params_mirt1) <- c("a", "a_lower", "a_upper", "d", "d_lower", "d_upper")
  irt_params_mirt1 <- cbind(irt_params_mirt1, irt_parmas)
  irt_theta_mirt1 <- cbind(fscores(mirt1), domain)
  
  write.csv(irt_params_mirt1, file = file.path(save_path, paste(domain, "est_simulate_item.csv",sep = "_")))
  write.csv(irt_theta_mirt1, file = file.path(save_path, paste(domain, "est_simulate_theta.csv", sep = '_')))
  
  irt_param_compare_real <- rbind(irt_param_compare_real, irt_params_mirt1)
  irt_theta_compare_real <- rbind(irt_theta_compare_real, irt_theta_mirt1)
}


write.csv(irt_param_compare_real, file = file.path(save_path, "est_simulate_item.csv"))
write.csv(irt_theta_compare_real, file = file.path(save_path, "est_simulate_theta.csv"))


# plotting
irt_param_compare <- fread(file = file.path(save_path, "est_simulate_item.csv")) %>%
  as.tibble()

# plot discrimination
cols = c("estimates" = "red", "truth" = "blue")
irt_param_compare %>%
  separate(item_id, into = c("domain", "item_id"), sep = "_") %>%
  ggplot(aes(y = a, x = item_id, color = "estimates")) +
  geom_errorbar(aes(ymin = a_lower, ymax = a_upper), width = .1) +
  geom_point(size = .1) +
  geom_point(aes(y = Dscrmn, x = item_id,  color = "truth"), size = .1) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~domain, ncol = 2, scales = "free") +
  scale_color_manual(values = cols) +
  labs(title = "Discrimination_real", y = "Discrimination", x= "Item ID") +
  theme(legend.position="bottom", axis.text.x = element_text(size = 4, angle = 60),
        legend.text = element_text(size = 4))

# difficulty plotting
cols = c("estimates" = "red", "truth" = "blue")
irt_param_compare %>%
  separate(item_id, into = c("domain", "item_id"), sep = "_") %>%
  ggplot(aes(y = d, x = item_id, color = "estimates")) +
  geom_errorbar(aes(ymin = d_lower, ymax = d_upper), width = .1) +
  geom_point(size = .1) +
  geom_point(aes(y = Dffclt, x = item_id,  color = "truth"), size = .1) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~domain, ncol = 2, scales = "free") +
  scale_color_manual(values = cols) +
  labs(title = "Difficulty_real", y = "Difficulty", x= "Item ID") +
  theme(legend.position="bottom", axis.text.x = element_text(size = 4, angle = 60),
        legend.text = element_text(size = 4))


# theta ability plotting
all_domain_theta <- fread(file = file.path(save_path, "est_true_theta.csv")) %>%
  as.tibble() %>%
  dplyr::select(F1, domain)

cols <- c("estimation"="blue", "truth"="red")
all_domain_theta %>%
  ggplot(aes(F1)) +
  geom_density(aes(fill = "estimation"), alpha = 0.5) +
  geom_density(data = theta, aes(dev_scale_score, fill = "truth"), alpha = 0.5) +
  facet_wrap(~domain, ncol = 2) + 
  labs(x = "ability") + 
  scale_fill_manual(values = cols,
                    name = "Theta") +
  theme(legend.position="bottom")
