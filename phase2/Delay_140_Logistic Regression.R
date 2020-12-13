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
if(!require(speedglm)) install.packages("speedglm")
library(speedglm)

rm(list = ls()) #clear
cat(rep("\n",128)) #quick and dirty clear console

setwd("C:/Users/tung.tran/Desktop/Prednasky-cvika/Data X/Data set - projekt/phase2")

pred_class = function(preds_probs, cut.off){
  preds = relevel(as.factor(preds_probs[,2]>cut.off),ref = "FALSE")
  levels(preds) = c("0","1")
  return(preds)
}


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
  mutate(delay_140_y = as.factor(delay_140_y))



# # Get rid of irrelevant columns
data_collection = data_collection[,-(28:32)] #delay_indiv, mean_delay_1m/3m/6m/12m
data_collection = data_collection[,-(25:26)] #delay, delay_21_y
data_collection = data_collection[,-8] #payment_date
data_collection = data_collection[,-4] #living_area
data_collection = data_collection[,-(1:2)] #contract_id, payment_order

set.seed(500) #fix the random number generator

# #Splitting data to 60:20:20 (holdout)
# #stratified sampling using the caret package (to avoid missing classes in training data)
# library(caret)
# trainval_index <- createDataPartition(data_collection$delay_21_y, p = .8, list = FALSE)
# data_trainval <- data_collection[ trainval_index,]
# data_test <- data_collection[-trainval_index,]
# 
# train_index <- createDataPartition(data_trainval$delay_21_y, p = .75, list = FALSE)
# data_train <- data_trainval[ train_index,]
# data_val <- data_trainval[ -train_index,]


trainval_index <- createDataPartition(data_collection$delay_140_y, p = .8, list = FALSE)
data_train <- data_collection[ trainval_index,]
data_val <- data_collection[-trainval_index,]


####Logistic regression#####
#set the formula
formula_mod = formula(delay_140_y ~ .) # "." - dot denotes all other variables from data frame

#fit the model using the glm function
logist_fit = glm(formula = formula_mod, data=data_train, family=binomial(link="logit"))



### Model_stepwise -------------------------------------------------------------
# Stepwise
# library(MASS)

# # Stepwise regression model
# fit_step <- stepAIC(logist_fit, direction = "backward", trace = T)
# #summary(fit_step)
# #Stepwise will delete column: kc_flag - does not affect accuracy



#predict probabilities
preds_probs = predict(logist_fit, newdata = data_val, type = "response")
#add probs of class 0
preds_probs = cbind((1-preds_probs), preds_probs)
preds = pred_class(preds_probs = preds_probs, cut.off = 0.5)

#accuracy measures
conf_mat = confusionMatrix(data = preds, reference = data_val$delay_140_y, positive = "1")
print(conf_mat$overall["Accuracy"])
logist_sensitivity = conf_mat$byClass["Sensitivity"]
logist_specifity = conf_mat$byClass["Specificity"]
s_logist_roc = roc(response = data_val$delay_140_y, predictor=preds_probs[,2], plot = TRUE, print.auc = TRUE)
abline(v = logist_specifity, h = logist_sensitivity, col = 'red', lty = "dotted")




