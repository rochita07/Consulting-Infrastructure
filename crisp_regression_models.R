#####################################################################
# This program creates simple regression models for LASSO variable 
# selection.
#####################################################################

rm(list = ls())

library(glmnet)

# Load the data
load("./data/crispcleanfinal.RData")
dim(crispcleanfinal)
colnames(crispcleanfinal)
crisp <- crispcleanfinal

###################################################################################
# Question 15: PPPSupportUse
## How much would you oppose or support the following types of Public
## Private Partnerships for the construction of some infrastructure project in your
## community?
###################################################################################

# Variables to exclude from analysis
## excluded County for now
vars <- c("weight1_PID", "weight2_PID", "id", "AllTexasWeight", "HarrisCountyWeight", 
          "xcrisp", "PPPSupport", "County")

# Create formula for model of Q15
form.q15 <- paste0("PPPSupportUse ~ -1")
for(param in colnames(crisp)[-which(colnames(crisp) %in% vars)]){
  form.q15 <- paste0(form.q15, " + ", param)
}

# Fit intial linear model for PPPSupportUse with all covariates of interest
fit_q15 <- lm(as.formula(form.q15), data = crisp, weights = weight1_PID)
summary(fit_q15)

# Set up x variables and y variable for glmnet
q15mat <- model.matrix(as.formula(form.q15), crisp)
data.q15mat <- merge(crisp, data.frame(q15mat))
x_vars <- q15mat
y_var <- data.q15mat$PPPSupportUse

# Fit glmnet for LASSO variable selection choosing the value of lambda
fit.q15 <- glmnet(x_vars, y_var, alpha = 1, lambda = 0.05, weights = data.q15mat$weight1_PID)

# Print covariates not set to 0
cbind(rownames(coef(fit.q15))[which(coef(fit.q15) != 0)], 
      round(coef(fit.q15)[which(coef(fit.q15) != 0)], 3))

# Fit model with variables chosen from LASSO
fitq15.lasso <- lm(PPPSupportUse ~ -1 + Q1_3 + Q1_4 + Q1_7 + Q1_12 + Q2_2 + Q8 + 
                     Q17_4 + Q18 + IDEO + partyid7 + coastal + income, 
                weights = weight1_PID, data = crisp)
summary(fitq15.lasso)

# Comput CV in glmnet to choose best lambda
cv.output <- cv.glmnet(x_vars, y_var, alpha = 1, weights = data.q15mat$weight1_PID)

# Identify best lambda
best.lam <- cv.output$lambda.min
best.lam

# Rebuild model with best lambda
lasso.best15 <- glmnet(x_vars, y_var, alpha = 1, lambda = best.lam)

# Print top variables chosen from LASSO
cbind(rownames(coef(lasso.best15))[which(coef(lasso.best15) != 0)],
      round(coef(lasso.best15)[which(coef(lasso.best15) != 0)], 3))

# Fit model with variables chosen from LASSO (taking out County)
fitq15.lasso.bestlam <- lm(PPPSupportUse ~ -1 + Q1_8 + Q8 + Q17_4 + income, 
                   weights = weight1_PID, data = crisp)
summary(fitq15.lasso.bestlam)

## TO DO: Likelihood ratio test to determine if all these variables are necessary

###############################################################################
# Question 17: PPPSupport
## To what extent do you disagree or agree with the following statements about
## Public Private Partnerships?
###############################################################################

# Variables to exclude from analysis
## excluded County for now
vars <- c("weight1_PID", "weight2_PID", "id", "AllTexasWeight", "HarrisCountyWeight", 
          "xcrisp", "PPPSupportUse", "County")

# Create formula for model of Q17
form.q17 <- paste0("PPPSupport ~ -1")
for(param in colnames(crisp)[-which(colnames(crisp) %in% vars)]){
  form.q17 <- paste0(form.q17, " + ", param)
}

# Fit intial linear model for PPPSupportUse with all covariates of interest
fit_q17 <- lm(as.formula(form.q17), data = crisp, weights = weight1_PID)
summary(fit_q17)

# Set up x variables and y variable for glmnet
q17mat <- model.matrix(as.formula(form.q17), crisp)
data.q17mat <- merge(crisp, data.frame(q17mat))
x_vars <- q17mat
y_var <- data.q17mat$PPPSupport

# Fit glmnet for LASSO variable selection choosing the value of lambda
fit.q17 <- glmnet(x_vars, y_var, alpha = 1, lambda = 0.05, weights = data.q17mat$weight1_PID)

# Print covariates not set to 0
cbind(rownames(coef(fit.q17))[which(coef(fit.q17) != 0)], 
      round(coef(fit.q17)[which(coef(fit.q17) != 0)], 3))

# Fit model with variables chosen from LASSO
fitq17.lasso <- lm(PPPSupportUse ~ -1 + Q1_3 + Q19_2 + Q19_7 + IDEO + partyid7 + age + race, 
                   weights = weight1_PID, data = crisp)
summary(fitq17.lasso)

# Comput CV in glmnet to choose best lambda
cv.output <- cv.glmnet(x_vars, y_var, alpha = 1, weights = data.q17mat$weight1_PID)

# Identify best lambda
best.lam <- cv.output$lambda.min
best.lam

# Rebuild model with best lambda
lasso.best17 <- glmnet(x_vars, y_var, alpha = 1, lambda = best.lam)

# Print top variables chosen from LASSO
cbind(rownames(coef(lasso.best17))[which(coef(lasso.best17) != 0)],
      round(coef(lasso.best17)[which(coef(lasso.best17) != 0)], 3))
# only chooses intercept


#############################################################################
# Question 22: PPP_Policy
## How much do you oppose or support the following policies to improve
## your community infrastructure?
##############################################################################

# Variables to exclude from analysis
vars <- c("weight1_PID", "weight2_PID", "id", "AllTexasWeight", "HarrisCountyWeight", 
          "xcrisp", "PPP_Policy", "County")

# Create formula for model of Q22
form.q22 <- paste0("PPP_Policy ~ -1")
for(param in colnames(crisp)[-which(colnames(crisp) %in% vars)]){
  form.q22 <- paste0(form.q22, " + ", param)
}

# Fit intial linear model for PPPSupportUse with all covariates of interest
fit_q22 <- lm(as.formula(form.q22), data = crisp, weights = weight1_PID)
summary(fit_q22)

# Set up x variables and y variable for glmnet
q22mat <- model.matrix(as.formula(form.q22), crisp)
data.q22mat <- merge(crisp, data.frame(q22mat))
x_vars <- q22mat
y_var <- data.q22mat$PPP_Policy

# Fit glmnet for LASSO variable selection choosing the value of lambda
fit.q22 <- glmnet(x_vars, y_var, alpha = 1, lambda = 0.05, weights = data.q22mat$weight1_PID)

# Print covariates not set to 0
cbind(rownames(coef(fit.q22))[which(coef(fit.q22) != 0)], 
      round(coef(fit.q22)[which(coef(fit.q22) != 0)], 3))

# Fit model with variables chosen from LASSO
fitq22.lasso <- lm(PPPSupportUse ~ -1 + Q1_12 + Q2_1 + Q2_2 + Q2_3 + Q2_6 + Q8 + 
                     Q19_7 + Q23_8 + Q23_9 + Q5avg + age + race + education + income, 
                   weights = weight1_PID, data = crisp)
summary(fitq22.lasso)

# Comput CV in glmnet to choose best lambda
cv.output <- cv.glmnet(x_vars, y_var, alpha = 1, weights = data.q22mat$weight1_PID)

# Identify best lambda
best.lam <- cv.output$lambda.min
best.lam

# Rebuild model with best lambda
lasso.best22 <- glmnet(x_vars, y_var, alpha = 1, lambda = best.lam)

# Print top variables chosen from LASSO
cbind(rownames(coef(lasso.best22))[which(coef(lasso.best22) != 0)],
      round(coef(lasso.best22)[which(coef(lasso.best22) != 0)], 3))

# Fit model with variables chosen from LASSO (taking out County)
fitq22.lasso.bestlam <- lm(PPPSupportUse ~ -1 + Q1_12 + Q2_3 + Q23_8 + Q23_9 + age, 
                           weights = weight1_PID, data = crisp)
summary(fitq22.lasso.bestlam)

## TO DO: Likelihood ratio test to determine if all these variables are necessary
