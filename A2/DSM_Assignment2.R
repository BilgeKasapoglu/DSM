# DSM -  Assignment 2
# load necessary packages
library(foreign)
library(data.table)
library(dplyr)
library(fastDummies)
library(stringr)
library(readxl)
library(leaps)
library(glmnet)
library(pls)
library(randomForest)

# Question 1
# clean environment
rm(list=ls())

# Load the data
soccer <- read_excel("soccer.xlsx")
summary(soccer)

# Drop the missing and duplicated values 
dta <-  na.omit(soccer)
dta <- dta[!duplicated(dta$Name),] 
dta <- dta  %>% select(c(4:20))

# Work Rate
unique(dta$`Work Rate`)
dta$`Work Rate` <- ordered(dta$`Work Rate`, levels = 
                                c("Low/ Low", "Low/ Medium", "Low/ High", 
                                  "Medium/ Low", "Medium/ Medium", "Medium/ High",
                                  "High/ Low", "High/ Medium","High/ High"))
dta$`Work Rate` <- unclass(dta$`Work Rate`)

# Body Types
unique(dta$`Body Type`)
table(dta$`Body Type`)
dta$`Body Type`[dta$`Body Type` == "PLAYER_BODY_TYPE_25"] <- "Other"
dta <- dummy_cols(dta, select_columns = "Body Type")
dta <- dta  %>% select(-c("Body Type")) 

# Position
unique(dta$Position)
table(dta$Position)
dta <- dummy_cols(dta, select_columns = "Position")
dta <- dta  %>% select(-c("Position")) 

# Preferred Foot
unique(dta$`Preferred Foot`)
table(dta$`Preferred Foot`)
dta <- dummy_cols(dta, select_columns = "Preferred Foot")
dta <- dta  %>% select(-c("Preferred Foot")) 

# Convert Wage into Numeric
sum(!str_detect(dta$Wage, "€"))
sum(!str_detect(dta$Wage, "K"))
dta$Wage <- str_remove(dta$Wage, "€")
dta$Wage <- str_remove(dta$Wage, "K") %>% as.numeric()
dta$Wage <- dta$Wage * 1000

# Convert Value into Numeric
money_convert <- function(x)  {
  value = as.numeric(str_sub(x, 2, -2))
  suffix = str_sub(x, nchar(x), -1)
  if (suffix == "K"){
    value = value*1000
  } else if ((suffix == "M"))  {
    value = value*1000000
  }
}
sum(!str_detect(dta$Value, "€"))
sum(!str_detect(dta$Value, "K"))
sum(!str_detect(dta$Value, "M"))
dta$Value <- sapply(dta$Value, money_convert)

# Nationality 
dta <- dummy_cols(dta, select_columns = "Nationality")
dta <- dta  %>% select(-c("Nationality")) 

# Weight
dta$Weight <- str_remove(dta$Weight, "lbs") %>% as.numeric()

# Height 
dta$Height <- str_replace(dta$Height, "'", ".") %>% as.numeric()
head(dta)

data <- lapply(dta, as.numeric) %>% as.data.frame()

# (a)

# Split data into two parts
set.seed(21) # for reproducibility
sum(is.na(data)) # check whether there are missing values
train.size = dim(data)[1] / 2 # learn how many observations we need to sample
train = sample(1:dim(data)[1], train.size) # get indexes of observations for training set
test = -train # get indexes of observations for test set
data.train = data[train, ] # extract training observations
data.test = data[test, ] # extract test observations

# Linear Regression
lm.fit = lm(Wage~., data=data.train) # apply linear regression
lm.pred = predict(lm.fit, data.test)  %>% as.numeric() # get predicted outcomes
testerror.ls = mean((data.test[, "Wage"] - lm.pred)^2) # find MSE
testerror.ls

# Convert data to matrix format
train.mat = model.matrix(Wage~., data=data.train)
test.mat = model.matrix(Wage~., data=data.test)

# Create a grid of lambdas from a large range
grid = 10^seq(2, -3, length=100)

# Lasso
mod.lasso = cv.glmnet(train.mat, data.train[, "Wage"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best.lasso = mod.lasso$lambda.min
lambda.best.lasso
lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best.lasso)
testerror.lasso=mean((data.test[, "Wage"] - lasso.pred)^2)
testerror.lasso

# Ridge
mod.ridge = cv.glmnet(train.mat, data.train[, "Wage"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best.ridge = mod.ridge$lambda.min
lambda.best.ridge
ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best.ridge)
testerror.ridge = mean((data.test[, "Wage"] - ridge.pred)^2)
testerror.ridge

# Random forest
mod.rf = randomForest(Wage~., data=data, subset=train, mtry=7, importance=TRUE)
yhat.rf = predict(mod.rf, newdata=data[-train,])
rf.test = data[-train, "Wage"]
testerror.rf = mean((yhat.rf-rf.test)^2)
importance(mod.rf)
varImpPlot(mod.rf)

# Overview of test errors
testerror.ls
testerror.lassos
testerror.ridge
testerror.rf

# (b)
reg.lasso = glmnet(data.train[,!(names(data.train) %in% c("Wage"))],
                   data.train$Wage, nlambda=lambda.best.lasso, alpha=0, 
                   family="gaussian", standardize=TRUE)
plot(x = log(reg.lasso$lambda), y = reg.lasso$beta[1,], type="l", 
     ylim=c(-80000,80000))
text(x = 12, y = max(reg.lasso$beta[1,]), labels = names(reg.lasso$beta[,1])[1])
for(i in 2:211){
  lines(x = log(reg.lasso$lambda), y = reg.lasso$beta[i,])
  text(x = 12, y = max(reg.lasso$beta[i,]), labels = names(reg.lasso$beta[,1])[i])
}
choose.lasso <- replicate(221, NA)
for(i in 1:221){
  choose.lasso[i] <- which.min(reg.lasso$beta[i,])
}