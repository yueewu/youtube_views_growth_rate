##################################  Libraries Used #################################
library(caret)
library(glmnet)
library(randomForest)
library(tidyverse)
library(gbm)

#################################### Import Data ###################################
# import the data and remove id column from training dataset
training <- read.csv("/Users/apple/Desktop/STATS 101C/Final Project/training.csv")[-1]
testing <- read.csv("/Users/apple/Desktop/STATS 101C/Final Project/test.csv")


############################## Basic Data Exploration ###############################
dim(training)
str(training)
#determine whether training dataset contains any missing (NA/NULL) values
all(!is.na(training))
all(!is.null(training))


################## Split Data into Training Data and Validation Data #################
set.seed(6930)
trainIndex <- createDataPartition(training$growth_2_6, p = 0.7, list = FALSE)
train_data <- training[trainIndex, ]
validation <- training[-trainIndex, ]


############################## Data Transform and Combine #############################
days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

#######################################################################################
### input: dataset
### output: modified dataset
### purpose: convert publishedDate into two numeric variables: dates and times
###          we convert the month and day into dates, hour and minute into times
#######################################################################################
convert_published_date <- function(dataset) {
  # combine month and day
  # extract the month and day from PublishedDate 
  month <- as.numeric(unlist(strsplit(dataset$PublishedDate, "/"))[seq(1, nrow(dataset) * 3, by = 3)])
  date <- as.numeric(unlist(strsplit(dataset$PublishedDate, "/"))[seq(2, nrow(dataset) * 3, by = 3)])
  # combine the month and day into one numeric variable
  time <- month + date / days[month]
  # update the dataset
  dataset$month <- time
  
  # combine hour and minute
  # extract the time from PublishedDate
  time <- apply(matrix(dataset$PublishedDate), 1, function(x){
    unlist(str_split(x, " "))[2]
  })
  # combine the hour and minute into one numeric variable
  hour <- apply(matrix(time), 1, function(x){
    # extract the hour and minute
    time <- as.numeric(unlist(str_split(x, ":")))
    # combine the hour and minute
    time[1] + time[2]/60
  })
  # remove the PublishedDate variable
  dataset <- dataset[, -1]
  # update the dataset
  dataset <- data.frame(hour = hour, dataset)
  
  dataset
}

# convert publishedDate for training data
train_data <- convert_published_date(train_data)
# convert publishedDate for validation data
validation <- convert_published_date(validation)
# convert publishedDate for testing data
testing <- convert_published_date(testing)


#######################################################################################
### input: dataset, channel features (string)
### output: modified dataset
### purpose: extract the variables describe the given feature
###          combine the variables into 1 variable that contains 4 levels 
###          (low, low-mid, mid-high, high)
#######################################################################################
combine_channel_feature_levels <- function(dataset, feature) {
  # determine which variables describe this feature
  index <- str_which(colnames(dataset), feature)
  # determine which level each observation belongs to
  determined_levels <- apply(dataset[, index], 1, function(x){
    # if any of the observation equal to 1, it belongs to the corresponding level
    # otherwise it belongs to high level
    ifelse(any(x == 1), which(x == 1), 4)
  })
  # modify the dataset, delete original channel feature columns
  dataset <- dataset[, -index]
  # update the dataset with the new variable
  dataset <- cbind(temp_name = as.factor(determined_levels), dataset)
  colnames(dataset)[1] <- feature
  
  dataset
}

# combine levels for Num_Subscribers
train_data <- combine_channel_feature_levels(train_data, "Num_Subscribers")
validation <- combine_channel_feature_levels(validation, "Num_Subscribers")
testing <- combine_channel_feature_levels(testing, "Num_Subscribers")
# combine levels for Num_Views_Base
train_data <- combine_channel_feature_levels(train_data, "Num_Views_Base")
validation <- combine_channel_feature_levels(validation, "Num_Views_Base")
testing <- combine_channel_feature_levels(testing, "Num_Views_Base")
# combine levels for avg_growth
train_data <- combine_channel_feature_levels(train_data, "avg_growth")
validation <- combine_channel_feature_levels(validation, "avg_growth")
testing <- combine_channel_feature_levels(testing, "avg_growth")
# combine levels for count_vids
train_data <- combine_channel_feature_levels(train_data, "count_vids")
validation <- combine_channel_feature_levels(validation, "count_vids")
testing <- combine_channel_feature_levels(testing, "count_vids")


######################## Feature Selection Base on Variance ########################
# delete variables that have low variance
# have less than 100 non-zero values
remove <- names(which(colSums(train_data != 0) < 100))
train_data <- train_data[ , -which(colnames(train_data) %in% remove)]
validation <- validation[, -which(colnames(validation) %in% remove)]
testing <- testing[, -which(colnames(testing) %in% remove)]


######################## Feature Selection Based on lasso #############################
set.seed(0617)
# The set up and define a grid of possible values for lambda
grid <- 10^seq(2, -3, by = -.1)
x <- model.matrix(growth_2_6 ~ ., train_data)[,-1]
y <- train_data$growth_2_6

# Select the best value for lambda using K-fold cross-validation.
cv.output <- cv.glmnet(x, y, family = "gaussian", alpha = 1, 
                       lambda = grid, standardize = TRUE, nfolds = 10)
# Retrieve the actual best value of lambda.
best.lambda.cv <- cv.output$lambda.min
best.lambda.cv
# fit the best lasso model
lasso_reg <- glmnet(x, y, alpha = 1, lambda = best.lambda.cv, standardize = TRUE)
lasso_reg$beta

# delete the variables which suggested to be deleted by lasso
# delete variables
lasso_var <- c(rownames(lasso_reg$beta)[lasso_reg$beta[, 1] == 0]) 
train_data <- train_data[, -which(colnames(train_data) %in% lasso_var)]
validation <- validation[, -which(colnames(validation) %in% lasso_var)]


################### Feature Selection Based on Random Forest #########################
set.seed(6930)
# fit a regression random forest model for feature section
rf.mod <- randomForest(growth_2_6~.,
                       data = train_data,
                       mtry = 37,
                       importance = TRUE,
                       ntree = 750) 

# construct a tibble with variables names, %IncMSE, and IncNodePurity
t <- tibble(names = rownames(rf.mod$importance),
            mse = rf.mod$importance[, 1],
            node = rf.mod$importance[, 2])
# arrange the variables based on decreasing order of %IncMSE and IncNodePurity
arrange(t, desc(mse))
arrange(t, desc(node))

# get the variable names with top 25% based on the decresing order of %IncMSE
var1 <- arrange(t, desc(mse))$names[sort(t$mse, decreasing = TRUE) > quantile(t$mse, 0.75)]
# get the variable names with top 25% based on the decreasing order of IncNodePurity
var2 <- arrange(t, desc(node))$names[sort(t$node, decreasing = TRUE) > quantile(t$node, 0.75)]
# the final selected variables will the the variables both in the top 25% of %IncMSE and IncNodePurity
var <- c("growth_2_6", c(var1, var2)[duplicated(c(var1, var2))])

# variable importance plot of the top 25% variables
varImpPlot(rf.mod, n.var = 27)

# choose the variables for final model
train_data <- train_data[, which(colnames(train_data) %in% var)]
validation <- validation[, which(colnames(validation) %in% var)]

################################## Final Model #######################################
set.seed(18973846)
# Use the out-of-bag estimation to select the optimal parameter values.
# Here, we specify how we will evaluate our models
oob_train_control <- trainControl(method = "oob", savePredictions = TRUE)
tunegrid <- expand.grid(mtry = 1:26)
forestfit <- train(growth_2_6~., 
                   data = train_data, 
                   method = "rf", 
                   importance = FALSE, 
                   trControl = oob_train_control, 
                   tuneGrid = tunegrid)
# The model shows the best m
print(forestfit)

mod <- randomForest(growth_2_6~.,
             data = train_data,
             mtry = 17,
             importance = TRUE,
             ntree = 750)
varImpPlot(mod, scale = F)



#######################################################################################
### input: true values, predicted values
### output: root-mean-square error
### purpose: compute root-mean-square error of predicted values
#######################################################################################
RMSE <- function(data, pred) {
  sqrt((1 / nrow(data)) * sum((data$growth_2_6 - pred)^2))
}

# check the RMSE for the result
forest.pred <- predict(forestfit, validation)
RMSE(validation, forest.pred)
forest.pred.train <- predict(forestfit, train_data)
RMSE(train_data, forest.pred.train)
rf.pred <- predict(forestfit, testing)

################################## Alternative Models #######################################
set.seed(0617)
# lasso
grid <- 10^seq(2, -3, by = -.1)
x <- model.matrix(growth_2_6 ~ ., train_data)[,-1]
y <- train_data$growth_2_6
tx <- model.matrix(growth_2_6 ~ ., validation)[,-1]
# cross validation determine lambda
cv.output <- cv.glmnet(x, y, family = "gaussian", alpha = 1, 
                       lambda = grid, standardize = TRUE, nfolds = 10)
best.lambda.cv <- cv.output$lambda.min
best.lambda.cv
# best model
lasso_reg <- glmnet(x, y, alpha = 1, lambda = best.lambda.cv, standardize = TRUE)
# check the RMSE for the result
pred_lasso <- predict(lasso_reg, s = best.lambda.cv, newx = tx, type = "response")
pred_lasso_train <- predict(lasso_reg, s = best.lambda.cv, newx = x, type = "response")
RMSE(validation, pred_lasso)
RMSE(train_data, pred_lasso_train)

# ridge
set.seed(0904)
grid <- 10^seq(2, -3, by = -.1)
x <- model.matrix(growth_2_6 ~ ., train_data)[,-1]
y <- train_data$growth_2_6
tx <- model.matrix(growth_2_6 ~ ., validation)[,-1]
# cross validation determine lambda
cv.output <- cv.glmnet(x, y, family = "gaussian", alpha = 0, 
                       lambda = grid, standardize = TRUE, nfolds = 10)
best.lambda.cv <- cv.output$lambda.min
best.lambda.cv
# best model
ridge_reg <- glmnet(x, y, alpha = 0, lambda = best.lambda.cv, standardize = TRUE)
# check the RMSE for the result
pred_ridge <- predict(ridge_reg, s = best.lambda.cv, newx = tx, type = "response")
pred_ridge_train <- predict(ridge_reg, s = best.lambda.cv, newx = x, type = "response")
RMSE(validation, pred_ridge)
RMSE(train_data, pred_ridge_train)

# boosting
boost.mod = gbm(growth_2_6 ~., data = train_data, n.trees = 750, 
                   distribution = "gaussian")
# check the RMSE for the result
pred.boost = predict(boost.mod, validation, n.trees =750)
pred.mod.train = predict(boost.mod, train_data, n.trees =750)
RMSE(validation, pred.boost)
RMSE(train_data, pred.mod.train)

# MLR
lm.mod <- lm(growth_2_6 ~., train_data)
# check the RMSE for the result
lm.pred.train <- pred(lm.mod, train_data)
lm.pred <- pred(lm.mod, validation)
RMSE(validation, lm.pred)
RMSE(validation, lm.pred.train)
