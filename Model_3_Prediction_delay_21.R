# Dependencies
if(!require(rpart)) install.packages("rpart") 
library(rpart)
if(!require(rpart.plot)) install.packages("rpart.plot") 
library(rpart.plot)
if(!require(Metrics)) install.packages("Metrics") 
library(Metrics)
if(!require(openxlsx)) install.packages("openxlsx") 
library(openxlsx)
if(!require(caret)) install.packages("caret") 
library(caret)
if(!require(pROC)) install.packages("pROC") 
library(pROC)
if(!require(pdp)) install.packages("pdp") 
library(pdp)
if(!require(e1071)) install.packages("e1071")
library(e1071)
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(glmnet)) install.packages("glmnet") 
library(glmnet)
if(!require(Metrics)) install.packages("Metrics") 
library(Metrics)
if(!require(vip)) install.packages("vip") 
library(vip)

install.packages("remotes")
remotes::install_github("laresbernardo/lares")
library(lares)

#rm(list = ls()) #clear
#cat(rep("\n",128)) #quick and dirty clear console


############################# Prepare the data #################################
#load data
data_collection <- read.delim("..\\data_collection_prepared_new.txt", sep = ";", dec = ".")
#str(data_collection)


# Convert columns to the correct data type
data_collection <- data_collection %>%
  mutate(due_date = as.Date(due_date, format = "%Y-%m-%d"))
data_collection <- data_collection %>%
  mutate(payment_date = as.Date(payment_date, format = "%Y-%m-%d"))
data_collection <- data_collection %>%
  mutate(product_type = as.factor(product_type))
data_collection <- data_collection %>%
  mutate(contract_status = as.factor(contract_status))
data_collection <- data_collection %>%
  mutate(business_discount = as.factor(business_discount))
data_collection <- data_collection %>%
  mutate(gender = as.factor(gender))
data_collection <- data_collection %>%
  mutate(marital_status = as.factor(marital_status))
data_collection <- data_collection %>%
  mutate(clients_phone = as.factor(clients_phone))
data_collection <- data_collection %>%
  mutate(client_mobile = as.factor(client_mobile))
data_collection <- data_collection %>%
  mutate(client_email = as.factor(client_email))
data_collection <- data_collection %>%
  mutate(total_earnings = factor(total_earnings, labels = c(
    "level1", "level2", "level3", "level4",
    "level5", "level6", "level7", "level8",
    "level9", "level10", "not_declared"
  )))
data_collection <- data_collection %>%
  mutate(different_contact_area = as.factor(different_contact_area))
data_collection <- data_collection %>%
  mutate(kc_flag = as.factor(kc_flag))
data_collection <- data_collection %>%
  mutate(cf_val = as.numeric(cf_val))
data_collection <- data_collection %>%
  mutate(kzmz_flag = as.factor(kzmz_flag))
data_collection <- data_collection %>%
  mutate(due_amount = as.numeric(due_amount))
data_collection <- data_collection %>%
  mutate(delay_21_y = as.factor(delay_21_y))

# High correlation assumptions 
cor.test(data_collection$due_amount, data_collection$paid_amount,
         method = "pearson") #0.9976633 - exclude

#Top 15 the most ranked cross-correlations 
corr_cross(data_collection_delay, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 15 )# display top 15 couples of variables (by correlation coefficient)

#  Exclude irrelevant variables
# high correlation with paid_amount
data_collection_delay <- subset(data_collection, select = -due_amount) 
# due_date and payment_date count delay
data_collection_delay <- subset(data_collection_delay, select = -due_date) 
# excluding contract_id
data_collection_delay <- subset(data_collection_delay, select = -contract_id)


# if NA in mean_delay_1/3/6/12m replace with 0 
data_collection_delay$mean_delay_1m[is.na(data_collection_delay$mean_delay_1m)] <- 0
data_collection_delay$mean_delay_3m[is.na(data_collection_delay$mean_delay_3m)] <- 0
data_collection_delay$mean_delay_6m[is.na(data_collection_delay$mean_delay_6m)] <- 0
data_collection_delay$mean_delay_12m[is.na(data_collection_delay$mean_delay_12m)] <- 0


# Filter rows with delay greater than 21.
data_collection_delay <- subset(data_collection_delay, data_collection_delay$delay_21_y == 1)
#  Exclude delay_21_y, not needed anymore
data_collection_delay <- subset(data_collection_delay, select = -delay_21_y)


#  Split the data
#fix the random number generator
set.seed(500) 
#Total splitting data 60:20:20 
#stratified sampling using the caret package (to avoid missing classes in training data)
n <- nrow(data_collection_delay)
# Randomly shuffle the data
data_collection_delay <- data_collection_delay[sample(n), ]
nr_trval <- round(0.80 * n)
index_trval <- sample(1:n, nr_trval)

dt_trval <- data_collection_delay[index_trval, ]
dt_test <- data_collection_delay[-index_trval, ]

nr_train <- round(0.75 * nr_trval) #splitting between data train and data validation
index_train <- sample(1:nr_trval, nr_train)
dt_train <- dt_trval[index_train, ]
dt_validation <- dt_trval[-index_train, ]

# Adjusting data to glment package format
data_model_matrix_train <- model.matrix(delay ~ ., dt_train) 
data_model_matrix_validation <- model.matrix(delay ~ ., dt_validation)
data_model_matrix_test <- model.matrix(delay ~ ., dt_test)
data_model_matrix_trval <- model.matrix(delay ~ ., dt_trval) 


#################### #### Baseline model - SIMPLE AVERAGE ######################
simple_average <- mean(dt_trval$delay, trim = 0, na.rm = FALSE)
# 128.2179
rmse(dt_test$delay, simple_average)
#  213.3069

########### Model - searching for best hyper parametres with HOLDOUT ########### 
#defining grid to tune the parameters
hyper_grid <- expand.grid(
  alpha = seq(from = 0 , to = 1, by = 0.1),
  lambda = seq(from = 0, to = 100, length.out = 50),
  rmse_ho = NA, #for hold out
  rmse_cv = NA #for cross validation
)

#  change memory limit
memory.size() 
memory.limit()
memory.limit(size=560000) 

# get estimation of the validation error for each couple on the grid
for(i in 1:nrow(hyper_grid)){
  #i = 2
  
  # Fitting elastic net regression
  fit_elnet_reg <- glmnet(
    x = data_model_matrix_train, 
    y = dt_train$delay,
    alpha = hyper_grid[i,"alpha"],
    lambda = hyper_grid[i,"lambda"],
    standardize = TRUE, 
    intercept = TRUE,
    family = "gaussian")
  
  # Calculating predictions
  pred_delay = predict(
    object = fit_elnet_reg,
    newx =  data_model_matrix_validation)
  #calculate rmse 
  hyper_grid[i,"rmse_ho"]<- rmse(dt_validation$delay, pred_delay)
}

opt_row_ho = which.min(hyper_grid[,"rmse_ho"])

# Estimation of rmse on test data using train and validation data for training
fit_elnet_reg_ho <- glmnet(
  x = data_model_matrix_trval, 
  y = dt_trval$delay,
  alpha = hyper_grid[opt_row_ho,"alpha"],
  lambda = hyper_grid[opt_row_ho,"lambda"],
  standardize = TRUE, 
  intercept = TRUE,
  family = "gaussian")

# Evaluation of rmse on testing data
rmse_test_ho = rmse(dt_test$delay,  predict(object = fit_elnet_reg_ho, newx =  data_model_matrix_test))
print(rmse_test_ho)
# 74.51247

##### Model - searching for best hyper parameters with CROSS VALIDATION #######
K = 10 # Creating 10 equally sized folds (data were randomly shuffled)

folds <- cut(seq(1, nrow(dt_trval)), breaks = K, labels = F)
rmse_fold = c() #creating vector for fold results


#Estimation of the validation error for each couple on the grid
for(i in 1:nrow(hyper_grid)){
  for(j in 1:K) {
    #j = 1# 
    validation_row_indexes <- which(folds == j, arr.ind = T) 
    data_validation_fold <- dt_trval[validation_row_indexes, ] #this fold is for validation
    data_train_fold <- dt_trval[-validation_row_indexes, ] #remaining folds are for training
    
    # Creating elastic net model
    # Adjusting data to glment package format
    data_model_matrix_train_fold <- model.matrix(delay ~ ., data_train_fold) 
    data_model_matrix_validation_fold <- model.matrix(delay ~ ., data_validation_fold)
    
    # Fitting model
    fit_elnet_cv_reg <- glmnet(
      x = data_model_matrix_train_fold, 
      y = data_train_fold$delay,
      alpha = hyper_grid[i,"alpha"],
      lambda = hyper_grid[i,"lambda"],
      standardize = TRUE, 
      intercept = TRUE,
      family = "gaussian")
    
    # Calculating predictions
    pred_delay = predict(
      object = fit_elnet_cv_reg,
      newx =  data_model_matrix_validation_fold)
    #calculating rmse for the fold
    rmse_fold[j] = rmse(data_validation_fold$delay, pred_delay)
  } #end fold
  hyper_grid[i,"rmse_cv"]<- mean(rmse_fold)
}#end hyper grid  

opt_row_cv = which.min(hyper_grid[,"rmse_cv"])

opt_par = hyper_grid[c(opt_row_ho, opt_row_cv),] 
print(opt_par)

#       alpha l ambda  rmse_ho    rmse_cv
# 1       0      0    74.78015   73.95688
# 1.1     0      0    74.78015   73.95688

#alpha and lambda = 0

#Estimation of rmse on test data using train and validation data for training
fit_elnet_reg_cv <- glmnet(
  x = data_model_matrix_trval, 
  y = dt_trval$delay,
  alpha = hyper_grid[opt_row_cv,"alpha"],
  lambda = hyper_grid[opt_row_cv,"lambda"],
  standardize = TRUE, 
  intercept = TRUE, 
  family = "gaussian",
  nfolds = 10)

rmse_test_cv = rmse(dt_test$delay,  predict(object = fit_elnet_reg_cv, newx =  data_model_matrix_test))

#rmse on testing data result
rmse_test_cv
# 74.51247

############################ Variables importance ##############################
vip(fit_elnet_reg_cv, bar = FALSE, horizontal = FALSE, size = 1, metric = "rmse",
    aesthetics = list(colour = "black",fill = "darkgreen"))
vi(fit_elnet_reg_cv,)


# Variable           Importance Sign 
# <chr>                   <dbl> <chr>
#   1 delay_140_y            165.   POS  
# 2 product_type5           22.3  NEG  
# 3 contract_status8        18.5  POS  
# 4 product_type4           11.8  NEG  
# 5 contract_status6        10.3  POS  
# 6 client_emailTRUE        10.1  POS  
# 7 business_discount1       8.71 NEG  
# 8 delay_indiv_140          7.13 NEG  
# 9 product_type3            7.08 NEG  
# 10 contract_status7         6.31 POS  
