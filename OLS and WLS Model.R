#### LIBRARIES ####
library(lmtest)
library(nortest)
library(car)
library(dplyr)
library(performance)
library(readxl)
library(ggplot2)
library(gamlr)

#### EXPLORATORY DATA ANALYSIS ####

##### 1. Read the Data #####
gdp = read_excel(file.choose())
head(gdp)

##### 2. Summarical Analysis #####
summary(gdp$Inflation_Consumer_Prices_Annual)
summary(gdp$Households_Consumption_Expenditure_Annual_Growth)
summary(gdp$Age_Dependency_Ratio)
summary(gdp$Inflation_GDP)

#### LINEAR REGRESSION ####
# Using only suspected significant variable
model1 = lm(Inflation_Consumer_Prices_Annual ~ Households_Consumption_Expenditure_Annual_Growth +
              Age_Dependency_Ratio +
              Inflation_GDP, data = gdp)
summary(model1)

#### ASSUMPTION TEST ####
error1 = resid(model1)

##### 1.  Normality Test #####
lillie.test(error1)
  # p-value > alpha -> residuals are normally distributed

##### 2. Homoscedasticity Test #####
bptest(model1) 
  # p-value < alpha -> residuals are heteroscedastic

##### 3. Non-Autocorrelation Test #####
dwtest(model1)
  # p-value > alpha -> residuals are not autocorrelated

##### 4. Non-Multicolllinearity Test #####
vif(model1)
  # all vif < 10 -> no variables are multicollinear

#### WEIGHTED LEAST SQUARES ####
# Need to be done since the model above fail the homoscedasticity test

##### 1. Define the weight #####
# To handle inequality of residual variances or to give more emphasis to certain data points.
weight1 = 1 / lm(abs(model1$residuals) ~ model1$fitted.values)$fitted.values^2

##### 2. Modelling #####
model_wls1 <- lm(Inflation_Consumer_Prices_Annual ~ Households_Consumption_Expenditure_Annual_Growth +
                   Age_Dependency_Ratio + Inflation_GDP, data = gdp, weights = weight1)
summary(model_wls1)

# since the Households_Consumption_Expenditure_Annual_Growth is not significant with 0.05 alpha, we have to remove the variable by repeating the steps before
model2 = lm(Inflation_Consumer_Prices_Annual ~
              Age_Dependency_Ratio +
              Inflation_GDP, data = gdp)
weight2 = 1 / lm(abs(model2$residuals) ~ model2$fitted.values)$fitted.values^2
model_wls2 <- lm(Inflation_Consumer_Prices_Annual ~
                  Age_Dependency_Ratio + Inflation_GDP, data = gdp, weights = weight2)
summary(model_wls2)

##### 3. Non-Autocorrelation Test #####
bptest(model_wls2)
  # p-value > alpha -> residuals are homoscedastic

#### GOODNESS-OF-FIT ####

##### 1. AIC #####
AIC(model1)
AIC(model_wls1)
AIC(model_wls2)

##### 2. AICc  #####
AICc(model1)
AICc(model_wls1)
AICc(model_wls2)

##### BIC  #####
BIC(model1)
BIC(model_wls1)
BIC(model_wls2)

##### RSE  #####
summary(model1)$sigma
summary(model_wls1)$sigma
summary(model_wls2)$sigma
