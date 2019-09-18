library(CDM)
library(tidyverse)
library(keras)

# data loading
data_path <- file.path("~", "Dropbox", "Research", "My research Topics",
                       "2019 Attributes estimation using noisy clustering results",
                       "Data")
if(!dir.exists(data_path)){dir.create(data_path)}
files <- dir(data_path)

file <- files[2]
# data loading
error_label <- read_csv(file.path(data_path, file, "error_label.csv"))
q_mat <- read_csv(file.path(data_path, file, "q_mat.csv"))
response <- read_csv(file.path(data_path, file, "response.csv"))
true_label <- read_csv(file.path(data_path, file, "true_label.csv"))

## num of attr, num of item, level of disc, q-matrix accuracy, # examinees
n_attr <- ncol(q_mat)
n_item <- nrow(q_mat)
n_examinee <- nrow(response)


# Claculate all accuracy for each method
cal_acc <- function(est, truth){
  all_accuracy <- est == truth
  
  ### accuracy for each attribute
  accuracy_attr <- all_accuracy %>%
    apply(2, as.numeric) %>%
    apply(2, mean)
  
  ### accuracy for pattern
  accuracy_pattern <- apply(all_accuracy, 1, prod) %>%
    as.numeric() %>%
    mean()
  
  return(c(accuracy_attr, accuracy_pattern))
}


# chosing DINA and DINO model as noisy labels

Y_target <- error_label %>%
  dpyler::select(matches(c("_RRUM|_GDINA"))) %>%
  as.matrix()
Y_target <- (Y_target > .5)*1

X <- response %>%
  as.matrix()

##=======================================================
# conduct neural network model for noisy label correction

#### 1) sub functions
create_model <- function(param){
  # create the deep forwardfeed networks
  
  n_item <- param$n_item
  n_attr <- param$n_attr
  n_hidden_1 <- param$n_hidden_1
  n_hidden_2 <- param$n_hidden_2
  n_out <- param$n_out
    
  input_layer <- layer_input(shape = c(n_item))
  
  attr_layer <- input_layer %>%
    layer_dense(units = n_hidden_1, activation = 'relu', 
                input_shape = c(n_item), 
                activity_regularizer = regularizer_l1(1e-10)) %>% 
    layer_dense(units = n_attr, activation = 'sigmoid') 
  
  test_model <- attr_layer %>%
    layer_dense(units = n_hidden_2, activation = 'relu',
                activity_regularizer = regularizer_l1(1e-30)) %>%
    layer_dense(units = n_out, activation = 'sigmoid')
  
  autoencoder_model <- keras_model(inputs = input_layer,
                                   outputs = test_model)
  
  attr_model <- keras_model(inputs = input_layer,
                            outputs = attr_layer)
  
  autoencoder_model %>% compile(
    optimizer = optimizer_sgd(momentum = .8, 
                              decay = 1e-6, 
                              nesterov = TRUE),
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
  return(list(deepForwardFeedNets_model = autoencoder_model,
              attr_model = attr_model))
}


attr_match <- function(attr_est_col, true_attr){
  ## calibrate estimation attr to the true attr
  
  temp_1 <- attr_est_col > .5
  temp_acc_1 <- temp_1 == true_attr 
  temp_acc_1 <- apply(temp_acc_1, 2, mean) 
  
  temp_2 <- attr_est_col <= .5
  temp_acc_2 <- temp_2 == true_attr 
  temp_acc_2 <- apply(temp_acc_2, 2, mean) 
  
  temp_acc <- c(temp_acc_1, temp_acc_2)
  
  if(which.max(temp_acc) > ncol(true_attr)){
    return_value <- temp_2}
  else{
    return_value <- temp_1
  }
  
  return(list(attr = names(temp_acc)[which.max(temp_acc)],
              value = return_value))
}


Proposed <- {} # each row of proposed indicated 
               # the accuracy for one time training

calibrate_attr_est <- {} # save all estimation of each iteration

n_iter <- 0
#while(n_iter < 10){
  # initialize hyper parameters
  n_hidden_1 <- 160
  n_hidden_2 <- 20
  epochs <- 80
  batch_size = 5
  
  # create keras models
  models <- create_model(param = list(n_item = n_item,
                                      n_attr = n_attr,
                                      n_hidden_1 = n_hidden_1,
                                      n_hidden_2 = n_hidden_2,
                                      n_out = ncol(Y_target)))
  
  deepForwardFeedNets_model <- models$deepForwardFeedNets_model
  attr_model <- models$attr_model

while(n_iter < 10){  
  history <- deepForwardFeedNets_model %>%
    keras::fit(X, Y_target, epochs = epochs, 
               batch_size = batch_size)
  
  #plot(history)
  
  # get the estimation of X
  attr_estmation <- attr_model %>%
    keras::predict_on_batch(X)
  
  ## attribute calibration
  #=============================
  calibrate_attr_est_temp <- apply(attr_estmation, 2,
                                   attr_match, 
                                   true_attr = true_label[1:n_attr])  
  
  calibrate_attr_est <- matrix(NA, nrow = n_examinee, 
                               ncol = n_attr) %>%
    data.frame()
  names(calibrate_attr_est) <- names(true_label[1:n_attr])

  for(i in 1:length(calibrate_attr_est_temp)){
    x <- calibrate_attr_est_temp[[i]]
    calibrate_attr_est[x$attr] <- x$value
  }
  #=============================
  
  ## attribute error and pattern error
  Proposed <- Proposed %>%
    rbind(cal_acc(calibrate_attr_est, true_label[1:n_attr]))

  n_iter <- n_iter+1
  }
#DFN <- Proposed[which.max(Proposed[,ncol(Proposed)]),]
#cat(DFN)


##=======================================================
# result comparison and save
accuracy_matrix <- {}
DCMs <- c("DINA", "DINO", "GDINA", "LCDM", "RRUM")
for (DCM in DCMs) {
  acc <- error_label %>%
    mutate_all('>', 0.5) %>%
    dplyr::select(matches(paste("_",DCM, sep = ""))) %>%
    cal_acc(truth = true_label[1:n_attr])
  accuracy_matrix <- rbind(accuracy_matrix, acc)
}
colnames(accuracy_matrix) <- c(names(true_label[1:n_attr]), "pattern")
rownames(accuracy_matrix) <- DCMs

DFN <- Proposed[which.max(Proposed[,ncol(Proposed)]),]

accuracy_matrix <- rbind(accuracy_matrix, DFN)
accuracy_matrix
