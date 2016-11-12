library(car)
library(Hmisc)
library(ROCR)
library(caret)
library(caTools)

german_credit = read.csv("german.csv")
summary(german_credit)

# Checkpoint 1: Data Understanding and Exploration
# Univariate analysis to understand the importance of a few variables
# Variable 1: Status.of.existing.checking.account
ggplot(german_credit, 
       aes(x = Status.of.existing.checking.account, 
           fill = factor(Default_status))) + geom_bar()

# Varible 2:  Duration.in.month
ggplot(german_credit, aes(Duration.in.month,
                          fill = factor(Default_status))) + geom_histogram() 


# Variable 3: Credit.history
ggplot(german_credit, 
       aes(x = Credit.history, 
           fill = factor(Default_status))) + geom_bar()


# Variable 4: Purpose
ggplot(german_credit, 
       aes(x = Purpose, 
           fill = factor(Default_status))) + geom_bar()

# Variable 5: Credit.amount
ggplot(german_credit, aes(Credit.amount, 
                          fill = factor(Default_status))) + geom_histogram() 


# Checkpoint 2: Data Cleaning and Transformation
# Observe missing values, if any
sapply(german_credit, function(x) sum(is.na(x)))

# Outliers
int_vars <- c(2, 5, 8, 11, 13, 16, 18)
probs <- seq(0, 1, 0.01)
sapply(german_credit[, int_vars], function(x) quantile(x, probs))

# Converting into dummy variables(using )
factor_vars <- c(1, 3, 4, 6, 7, 9, 10, 12, 14, 15, 17, 19, 20)
factor_df <- german_credit[, factor_vars]
dummy_df <- data.frame(sapply(factor_df, function(x) model.matrix(~x)))

# Merge dummy variables with the main data frame
german_credit <- german_credit[, -factor_vars]
german_credit <- cbind(german_credit, dummy_df)

# Method 2: Longer but correct
# fact_1 <- model.matrix(~german_credit[, 1], data = german_credit)
# fact_3 <- model.matrix(~german_credit[, 3], data = german_credit)
# fact_4 <- model.matrix(~german_credit[, 4], data = german_credit)
# fact_6 <- model.matrix(~german_credit[, 6], data = german_credit)
# fact_7 <- model.matrix(~german_credit[, 7], data = german_credit)
# fact_9 <- model.matrix(~german_credit[, 9], data = german_credit)
# fact_10 <- model.matrix(~german_credit[, 10], data = german_credit)
# fact_12 <- model.matrix(~german_credit[, 12], data = german_credit)
# fact_14 <- model.matrix(~german_credit[, 14], data = german_credit)
# fact_17 <- model.matrix(~german_credit[, 17], data = german_credit)
# fact_19 <- model.matrix(~german_credit[, 19], data = german_credit)
# fact_20 <- model.matrix(~german_credit[, 20], data = german_credit)

# Checkpoint 3
# Splitting into train and test sets
set.seed(100)
split_data = sample.split(german_credit$Default_status, SplitRatio = 0.7)
train = german_credit[split_data,]
test = german_credit[!split_data,]

# Checkpoint 4
# Building a rough model with all variables
initial_model = glm(Default_status ~ ., data = train, family = "binomial")
summary(initial_model)


# Stepwise selection of variables
best_model = step(initial_model, direction = "both", k = 2)
summary(best_model)


# Defining a customized function for automatically selection of variables based on VIF
vif(best_model)

# assigning model suggested by stepAIC and vif as the final model 
# (as mentioned in the instructions)
final_model = best_model


# getting names of important variables into a vector
final_variables = row.names(vif(best_model))

# Checkpoint 5
# predicting the responses on training data
# type = "response" returns the log odds of default and allows concordance measurement
pred_train = predict(final_model, data = train, type = "response")
rcorr.cens(pred_train, train$Default_status)

#KS statistic and ROC curve 
# generate a prediction object (needed for ROC curve)
pred <- predict(final_model, newdata = test)
roc_pred <- prediction(pred, test$Default_status)
model_perf <- performance(roc_pred, "tpr", "fpr")
plot(model_perf)

# assuming a low threshold, acc to the ROC curve
threshold = 0.35

# KS Statistic
ks_table <- attr(model_perf, "y.values")[[1]] - 
  (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)
ks
which.max(ks_table)

# Checkpoint 6: Threshold Value and confusionMatrix
pred_cf = predict(final_model, newdata = test[,-21], type = "response")
table(pred_cf > threshold, test$Default_status)

# trying different thresholds
# Accuracy, specificity and sensitivity can be calculated from the table
table(pred_cf > 0.4, test$Default_status)
table(pred_cf > 0.5, test$Default_status)  
