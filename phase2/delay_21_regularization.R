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
if(!require(randomForest)) install.packages("randomForest") 
library(randomForest)
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


rm(list = ls()) #clear
cat(rep("\n",128)) #quick and dirty clear console

setwd("C:/Users/tung.tran/Desktop/Prednasky-cvika/Data X/Data set - projekt/phase2")



#load data
data_collection <- read.delim("data_collection_prepared.txt", sep = ";", dec = ".")


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
  mutate(living_area = as.factor(living_area))
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



# # Get rid of irrelevant columns
data_collection = data_collection[,-(27:32)] #delay_140y, delay_indiv, mean_delay_1m/3m/6m/12m
data_collection = data_collection[,-25] #delay
data_collection = data_collection[,-8] #payment_date
data_collection = data_collection[,-4] #living_area
data_collection = data_collection[,-(1:2)] #contract_id, payment_order

set.seed(500) #fix the random number generator

#Splitting data to 60:20:20 (holdout)
#stratified sampling using the caret package (to avoid missing classes in training data)

trainval_index <- createDataPartition(data_collection$delay_21_y, p = .8, list = FALSE)
data_trainval <- data_collection[ trainval_index,]
data_test <- data_collection[-trainval_index,]

train_index <- createDataPartition(data_trainval$delay_21_y, p = .75, list = FALSE)
data_train <- data_trainval[ train_index,]
data_val <- data_trainval[ -train_index,]



# Adjust data to glment package format
data_model_matrix_train <- model.matrix(delay_21_y ~ ., data_train)
data_model_matrix_val <- model.matrix(delay_21_y ~ ., data_val)
data_model_matrix_test <- model.matrix(delay_21_y ~ ., data_test)
data_model_matrix_trainval <- model.matrix(delay_21_y ~ ., data_trainval)

#define grid to tune the parameters
hyper_grid <- expand.grid(
  alpha = seq(from = 0 , to = 1, by = 0.1),
  lambda = seq(from = 0, to = 100, length.out = 50),
  acc_ho = NA, #for hold out
  acc_cv = NA #for cross validation
)

#get estimate of the validation error for each couple on the grid
for(i in 1:nrow(hyper_grid)){

  # Fit elastic net regression
  fit_elnet_reg <- glmnet(
    x = data_model_matrix_train,
    y = data_train$delay_21_y,
    alpha = hyper_grid[i,"alpha"],
    lambda = hyper_grid[i,"lambda"],
    standardize = TRUE,
    intercept = TRUE,
    family = "binomial")

  #calcualte predictions
  preds = predict(
    object = fit_elnet_reg,
    newx =  data_model_matrix_val,
    type = "response")
  #set cut off to 50%
  preds[,1] <- ifelse(preds[,1]>0.5, "1", "0")
  #calculate accuracy
  hyper_grid[i,"acc_ho"]<- accuracy(data_val$delay_21_y, preds)

  print(i)
}

opt_row_ho = which.max(hyper_grid[,"acc_ho"])

fit_elnet_reg_hoe <- glmnet(
  x = data_model_matrix_trainval,
  y = data_trainval$delay_21_y,
  alpha = hyper_grid[opt_row_ho,"alpha"],
  lambda = hyper_grid[opt_row_ho,"lambda"],
  standardize = TRUE,
  intercept = TRUE,
  family = "binomial")

# fit_elnet_reg_hoe <- glmnet(
#   x = data_model_matrix_trainval,
#   y = data_trainval$delay_21_y,
#   alpha = 0,
#   lambda = 0,
#   standardize = TRUE,
#   intercept = TRUE,
#   family = "binomial")

#calcualte predictions
preds_prob = predict(
  object = fit_elnet_reg_hoe,
  newx =  data_model_matrix_test,
  type = "response")
#set cut off to 50%
preds1 <- ifelse(preds_prob[,1]>0.5, "1", "0")
#calculate accuracy
accuracy(data_test$delay_21_y, preds1)


conf.mat = confusionMatrix(table(preds1, data_test$delay_21_y), positive = "1")
logist_sensitivity = conf.mat$byClass["Sensitivity"]
logist_specifity = conf.mat$byClass["Specificity"]
s.logist.roc = roc(response = data_test$delay_21_y, predictor=preds_prob, plot = TRUE, print.auc = TRUE)
abline(v = logist_specifity, h = logist_sensitivity, col = 'red', lty = "dotted")



# same accuracy
# set.seed(500)
# cv_5 = trainControl(method = "cv", number = 5)
# 
# def_elnet = train(
#   delay_21_y ~ ., data = data_train,
#   method = "glmnet",
#   trControl = cv_5
# )
# def_elnet #best results alpha = 1, lambda = 0.0003322894
# 
# calc_acc = function(actual, predicted) {
#   mean(actual == predicted)
# }
# 
# calc_acc(actual = data_test$delay_21_y,
#          predicted = predict(def_elnet, newdata = data_test))
