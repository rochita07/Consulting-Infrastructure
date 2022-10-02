####### XGBoost for Texas  ########

library(dplyr)
library(xgboost)
library(magrittr)
library(Matrix)
library(ggplot2)

load("F:/Courses/684_Fall_2021/DATA/crispcleanfinal.RData")
crisp = crispcleanfinal
dim(crisp)
colnames(crisp)
# View(crisp)

##### Removing the variables (columns) have missing values more than 5%   ######

crisprf = crisp[,colSums(is.na(crisp)) < dim(crisp)[1]*0.05]  # HarrisCountyWeight is getting removed
colnames(crisprf)
crisprf = crisprf[complete.cases(crisprf),]
colnames(crisprf)
dim(crisprf)


## Removing some columns

remove_var = c('weight1_PID',
               'weight2_PID',
               'HarrisCountyWeight',
               'AllTexasWeight',
               'fold',
               'County',
               'id',
               'xcrisp',
               'testtrain',
               'screenlen',
               'intlen',
               '99thPercentilePrecipDays1Y',
               '99thPercentilePrecipDays3Y',
               '99thPercentilePrecipDays5Y')

remove_id = which(colnames(crisprf) %in% remove_var)
remove_id


##### Test-Train data  ######

crisprf$testtrain = as.factor(crisprf$testtrain)
crisprf_train = subset(crisprf, crisprf$testtrain == 'train')
crisprf_test = subset(crisprf, crisprf$testtrain == 'test')
dim(crisprf_train)
dim(crisprf_test)


####### Optimum XGBoost Model by CV  ##########


max_depth = seq(4, 12, by = 2)
eta = seq(0.01, 1.1, by = 0.2) 
gamma = c(0)

cv.nround = 50
cv.nfold = 5


parameters_df = as.data.frame(expand.grid(max_depth, eta, gamma))
parameters_df
names(parameters_df) = c('max_depth', 'eta' , 'gamma')

xgboost_cv = function(parameters_df, matrix_x, label_y, weight_vec)
{
  best_param <- list()
  # best_seednumber <- 1234
  best_rmse <- Inf
  best_rmse_index <- 0
  
  for(j in 1: dim(parameters_df)[1])
  {
    param = list(
      objective = "reg:linear",
      eval_metric = "rmse",
      max_depth = parameters_df$max_depth[j],
      eta = parameters_df$eta[j],
      gamma = parameters_df$gamma[j]
    )
    print(paste('j:', j, param))
    
    # weight = crisprf_train$AllTexasWeight
    mdcv <- xgb.cv(data = matrix_x, label = label_y, weight = weight_vec,
                   params = param, nthread = 6, 
                   nfold = cv.nfold, nrounds = cv.nround, verbsity = 0, silent = TRUE, early_stopping_rounds = 8)
    #verbose = T)
    
    min_rmse_index  <-  mdcv$best_iteration
    min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
    
    
    if (min_rmse < best_rmse) 
    {
      best_rmse <- min_rmse
      best_rmse_index <- min_rmse_index
      # best_seednumber <- seed.number
      best_param <- param
    }
  } # For loop
  
  nround = best_rmse_index
  nround
  best_param
  
  return(list('nround' = nround, 'best_param' = best_param))
}



############################ PPPSupportUse ##########################################


plot(crisprf_train$PPPSupportUse, main = "PPPSupportUse_Train", ylab = 'PPPSupportUse')
l1 = which(colnames(crisprf_train) == 'PPPSupportUse')
l1


mod_PPPSupportUse_train = sparse.model.matrix(crisprf_train$PPPSupportUse~. , data = crisprf_train[, -c(remove_id, l1)])
head(mod_PPPSupportUse_train)
dim(mod_PPPSupportUse_train)

mod_PPPSupportUse_test = sparse.model.matrix(crisprf_test$PPPSupportUse~. , data = crisprf_test[, -c(remove_id, l1)])
head(mod_PPPSupportUse_test)
dim(mod_PPPSupportUse_test)


info_PPPSupportUse_Texas = xgboost_cv(parameters_df, 
                                      matrix_x = mod_PPPSupportUse_train , 
                                      label_y = crisprf_train$PPPSupportUse,
                                      weight_vec = crisprf_train$AllTexasWeight)
info_PPPSupportUse_Texas

## Train set

bst_PPPSupportUse_train_Texas <- xgboost(data = mod_PPPSupportUse_train, label = crisprf_train$PPPSupportUse, weight = crisprf_train$AllTexasWeight,
                                         params = info_PPPSupportUse_Texas$best_param,  nround = info_PPPSupportUse_Texas$nround, verbosity = 0, silent = TRUE)
bst_PPPSupportUse_train_Texas
imp_PPPSupportUse_train_Texas <- xgb.importance(feature_names = colnames(mod_PPPSupportUse_train), model = bst_PPPSupportUse_train_Texas) 
head(imp_PPPSupportUse_train_Texas, 30)

x11()
(gg_PPPSupportUse_train_Texas <- xgb.ggplot.importance(imp_PPPSupportUse_train_Texas, measure = "Gain", top_n = 30))
gg_PPPSupportUse_train_Texas + ggplot2::ylab("Gain:PPPSupportUse_Texas")


## prediction on test set


bst_PPPSupportUse_test_Texas <- xgboost(data = mod_PPPSupportUse_test, label = crisprf_test$PPPSupportUse, weight = crisprf_test$AllTexasWeight,
                                        params = info_PPPSupportUse_Texas$best_param,  nround = info_PPPSupportUse_Texas$nround, verbose = F)
bst_PPPSupportUse_test_Texas 
pred_PPPSupportUse_test_Texas = predict(bst_PPPSupportUse_test_Texas, newdata = mod_PPPSupportUse_test)
pred_PPPSupportUse_test_Texas


plot(crisprf_test$PPPSupportUse - pred_PPPSupportUse_test_Texas, ylab = 'Error', main = 'PPPSupportUse_Texas')
abline(h = 0, col ="red")

#rmse
sqrt(sum((crisprf_test$PPPSupportUse - pred_PPPSupportUse_test_Texas)^2)/length(crisprf_test$PPPSupportUse))


accuracy_measure = function(y, y_pred)
{
  SSE = sum((y - y_pred)^2)
  TSE = sum((y - mean(y))^2)
  r_sq = 1 - (SSE/TSE)
  avg_abs_resid = mean(abs(y - y_pred))
  return(list('r_sq' = r_sq, 'avg_abs_resid' = avg_abs_resid))
}

accuracy_measure(crisprf_test$PPPSupportUse, pred_PPPSupportUse_test_Texas)





##################################  PPPSupport ##################################################


plot(crisprf_train$PPPSupport, main = "PPPSupport_Train", ylab = 'PPPSupport')
l2 = which(colnames(crisprf_train) == 'PPPSupport')
l2


mod_PPPSupport_train = sparse.model.matrix(crisprf_train$PPPSupport~. , data = crisprf_train[, -c(remove_id, l2)])
head(mod_PPPSupport_train)
dim(mod_PPPSupport_train)

mod_PPPSupport_test = sparse.model.matrix(crisprf_test$PPPSupport~. , data = crisprf_test[, -c(remove_id, l2)])
head(mod_PPPSupport_test)
dim(mod_PPPSupport_test)



info_PPPSupport_Texas = xgboost_cv(parameters_df, 
                                   matrix_x = mod_PPPSupport_train , 
                                   label_y = crisprf_train$PPPSupport,
                                   weight_vec = crisprf_train$AllTexasWeight)
info_PPPSupport_Texas

## Train set

bst_PPPSupport_train_Texas <- xgboost(data = mod_PPPSupport_train, label = crisprf_train$PPPSupport, weight = crisprf_train$AllTexasWeight,
                                      params = info_PPPSupport_Texas$best_param,  nround = info_PPPSupport_Texas$nround, verbosity = 0, silent = TRUE)
bst_PPPSupport_train_Texas
imp_PPPSupport_train_Texas <- xgb.importance(feature_names = colnames(mod_PPPSupport_train), model = bst_PPPSupport_train_Texas) 
head(imp_PPPSupport_train_Texas, 30)

x11()
(gg_PPPSupport_train_Texas <- xgb.ggplot.importance(imp_PPPSupport_train_Texas, measure = "Gain", top_n = 30))
gg_PPPSupport_train_Texas + ggplot2::ylab("Gain:PPPSupport_Texas")


## prediction on test set



bst_PPPSupport_test_Texas <- xgboost(data = mod_PPPSupport_test, label = crisprf_test$PPPSupport, weight = crisprf_test$AllTexasWeight,
                                     params = info_PPPSupport_Texas$best_param,  nround = info_PPPSupport_Texas$nround, verbose = F)
bst_PPPSupport_test_Texas 
pred_PPPSupport_test_Texas = predict(bst_PPPSupport_test_Texas, newdata = mod_PPPSupport_test)
pred_PPPSupport_test_Texas


plot(crisprf_test$PPPSupport - pred_PPPSupport_test_Texas, ylab = 'Error', main = 'PPPSupport_Texas')
abline(h = 0, col ="red")

#rmse
sqrt(sum((crisprf_test$PPPSupport - pred_PPPSupport_test_Texas)^2)/length(crisprf_test$PPPSupport))

accuracy_measure(crisprf_test$PPPSupport, pred_PPPSupport_test_Texas)






##################################  PPP_Policy ##################################################


plot(crisprf_train$PPP_Policy, main = "PPP_Policy_Train", ylab = 'PPP_Policy')
l3 = which(colnames(crisprf_train) == 'PPP_Policy')
l3


mod_PPP_Policy_train = sparse.model.matrix(crisprf_train$PPP_Policy~. , data = crisprf_train[, -c(remove_id, l3)])
head(mod_PPP_Policy_train)
dim(mod_PPP_Policy_train)

mod_PPP_Policy_test = sparse.model.matrix(crisprf_test$PPP_Policy~. , data = crisprf_test[, -c(remove_id, l3)])
head(mod_PPP_Policy_test)
dim(mod_PPP_Policy_test)



info_PPP_Policy_Texas = xgboost_cv(parameters_df, 
                                   matrix_x = mod_PPP_Policy_train , 
                                   label_y = crisprf_train$PPP_Policy,
                                   weight_vec = crisprf_train$AllTexasWeight)
info_PPP_Policy_Texas

## Train set

bst_PPP_Policy_train_Texas <- xgboost(data = mod_PPP_Policy_train, label = crisprf_train$PPP_Policy, weight = crisprf_train$AllTexasWeight,
                                      params = info_PPP_Policy_Texas$best_param,  nround = info_PPP_Policy_Texas$nround, verbosity = 0, silent = TRUE)
bst_PPP_Policy_train_Texas
imp_PPP_Policy_train_Texas <- xgb.importance(feature_names = colnames(mod_PPP_Policy_train), model = bst_PPP_Policy_train_Texas) 
head(imp_PPP_Policy_train_Texas, 30)

x11()
(gg_PPP_Policy_train_Texas <- xgb.ggplot.importance(imp_PPP_Policy_train_Texas, measure = "Gain", top_n = 30))
gg_PPP_Policy_train_Texas + ggplot2::ylab("Gain:PPP_Policy_Texas")


## prediction on test set



bst_PPP_Policy_test_Texas <- xgboost(data = mod_PPP_Policy_test, label = crisprf_test$PPP_Policy, weight = crisprf_test$AllTexasWeight,
                                     params = info_PPP_Policy_Texas$best_param,  nround = info_PPP_Policy_Texas$nround, verbose = F)
bst_PPP_Policy_test_Texas 
pred_PPP_Policy_test_Texas = predict(bst_PPP_Policy_test_Texas, newdata = mod_PPP_Policy_test)
pred_PPP_Policy_test_Texas


plot(crisprf_test$PPP_Policy - pred_PPP_Policy_test_Texas, ylab = 'Error', main = 'PPP_Policy_Texas')
abline(h = 0, col ="red")

#rmse
sqrt(sum((crisprf_test$PPP_Policy - pred_PPP_Policy_test_Texas)^2)/length(crisprf_test$PPP_Policy))

accuracy_measure(crisprf_test$PPP_Policy, pred_PPP_Policy_test_Texas)
