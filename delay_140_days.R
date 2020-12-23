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
if(!require(lift)) install.packages("lift") 
library(lift)
if(!require(MASS)) install.packages("MASS") 
library(MASS)

rm(list = ls()) #clear
cat(rep("\n",128)) #quick and dirty clear console

setwd("C:/Users/tung.tran/Desktop/Prednasky-cvika/Data X/Data set - projekt/phase2")


#load data
data_collection <- read.delim("data_collection_prepared_final.txt", sep = ";", dec = ".")


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
data_collection = data_collection[,-(26)] #delay_21_y - irrelevant
data_collection = data_collection[,-25] #delay - if we leave this in the dataset the accuracy/AUC will be 100%/1
data_collection = data_collection[,-8] #payment_date - if we leave this in the dataset the accuracy/AUC will be 100%/1
data_collection = data_collection[,-4] #living_area - has too many levels - more than 100gb of RAM is needed. Ask Mr. Prochazka

# replace NA values with 0 in mean delay
data_collection[is.na(data_collection)] <- 0

set.seed(500) #fix the random number generator

#Splitting data to 60:20:20 (holdout)
#stratified sampling using the caret package (to avoid missing classes in training data)

trainval_index <- createDataPartition(data_collection$delay_140_y, p = .8, list = FALSE)
data_trainval <- data_collection[ trainval_index,]
data_test <- data_collection[-trainval_index,]

train_index <- createDataPartition(data_trainval$delay_140_y, p = .75, list = FALSE)
data_train <- data_trainval[ train_index,]
data_val <- data_trainval[ -train_index,]



# Adjust data to glment package format
data_model_matrix_train <- model.matrix(delay_140_y ~ ., data_train)
data_model_matrix_val <- model.matrix(delay_140_y ~ ., data_val)
data_model_matrix_test <- model.matrix(delay_140_y ~ ., data_test)
data_model_matrix_trainval <- model.matrix(delay_140_y ~ ., data_trainval)

#define grid to tune the parameters
hyper_grid <- expand.grid(
  alpha = seq(from = 0 , to = 1, by = 0.1),
  lambda = seq(from = 0, to = 100, length.out = 50),
  acc_ho = NA, #for hold out
  acc_cv = NA #for cross validation
)

########### stepwise and var importance
# NOTE!!! Stepwise() and varImp() can be used only with LM or GLM type object
#set the formula
formula_mod = formula(delay_140_y ~ .) 

#fit the model using the glm function
logist_fit = glm(formula = formula_mod, data=data_train, family=binomial(link="logit"))

#Stepwise
#fit_step <- stepAIC(logist_fit, direction = "backward", trace = T)

#variable importance
imp <- as.data.frame(varImp(logist_fit))
imp <- data.frame(overall = imp$Overall,
                  variables   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

########### HOLDOUT ################
#get estimate of the validation error for each couple on the grid
for(i in 1:nrow(hyper_grid)){
  # Fit elastic net regression
  fit_elnet_reg <- glmnet(
    x = data_model_matrix_train,
    y = data_train$delay_140_y,
    alpha = hyper_grid[i,"alpha"],
    lambda = hyper_grid[i,"lambda"],
    standardize = TRUE,
    intercept = TRUE,
    family = "binomial",
    trace = TRUE)
  
  #calcualte predictions
  preds = predict(
    object = fit_elnet_reg,
    newx =  data_model_matrix_val,
    type = "response")
  #set cut off to 50%
  preds[,1] <- ifelse(preds[,1]>0.5, "1", "0")
  #calculate accuracy
  hyper_grid[i,"acc_ho"]<- accuracy(data_val$delay_140_y, preds)
  
  print(i)
}

opt_row_ho = which.max(hyper_grid[,"acc_ho"])

#fitting 
fit_elnet_reg_hoe <- glmnet(
  x = data_model_matrix_trainval,
  y = data_trainval$delay_140_y,
  alpha = hyper_grid[opt_row_ho,"alpha"],
  lambda = hyper_grid[opt_row_ho,"lambda"],
  standardize = TRUE,
  intercept = TRUE,
  family = "binomial",
  trace = TRUE)


#calcualte predictions
preds_prob = predict(
  object = fit_elnet_reg_hoe,
  newx =  data_model_matrix_test,
  type = "response")
#set cut off to 50%
preds1 <- ifelse(preds_prob[,1]>0.50, "1", "0")
#calculate accuracy
accuracy(data_test$delay_140_y, preds1)

# ROC curve, AUC
conf.mat = confusionMatrix(table(preds1, data_test$delay_140_y), positive = "1")
logist_sensitivity = conf.mat$byClass["Sensitivity"]
logist_specifity = conf.mat$byClass["Specificity"]
s.logist.roc = roc(response = data_test$delay_140_y, predictor=preds_prob, plot = TRUE, print.auc = TRUE)
abline(v = logist_specifity, h = logist_sensitivity, col = 'red', lty = "dotted")


########### CROSS VALIDATION ################
#only provides validation for different lambdas and alpha fixed

#using alpha opimized in ho
fit_elnet_cvp <- cv.glmnet(
  x = data_model_matrix_trainval, 
  y = data_trainval$delay_140_y,
  lambda = c(seq(from = 0, to = 500, length.out = 50)),
  alpha = hyper_grid[opt_row_ho,"alpha"], # number from interval (0,1) hyper_grid[opt_row_ho,"alpha"]
  family = "binomial", 
  nfolds = 5,
  trace=TRUE)

#calcualte predictions
preds_prob2 = predict(
  object = fit_elnet_cvp,
  newx =  data_model_matrix_test,
  type = "response")

#set cut off
preds2 <- ifelse(preds_prob2[,1]>0.5, "1", "0")

#calculate accuracy
accuracy(data_test$delay_140_y, preds2)

# ROC curve, AUC
conf.mat = confusionMatrix(table(preds2, data_test$delay_140_y), positive = "1")
logist_sensitivity = conf.mat$byClass["Sensitivity"]
logist_specifity = conf.mat$byClass["Specificity"]
s.logist.roc = roc(response = data_test$delay_140_y, predictor=preds_prob2, plot = TRUE, print.auc = TRUE)
abline(v = logist_specifity, h = logist_sensitivity, col = 'red', lty = "dotted")

#top decile lift
TopDecileLift(preds_prob2, data_test$delay_140_y)
plotLift(preds_prob2, data_test$delay_140_y, cumulative = FALSE, n.buckets = 10)




