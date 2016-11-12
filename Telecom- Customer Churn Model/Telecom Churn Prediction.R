#Telecom Churn Case study:
# Load the given files.

internet <-read.csv("internet_data.csv", stringsAsFactors = F, header = T, strip.white = T)
customer <-read.csv("customer_data (1).csv", stringsAsFactors = F, header = T,  strip.white = T)
churn <-read.csv("churn_data.csv", stringsAsFactors = F, header = T)


# Collate the 3 files in a single file.
telecom <- merge(internet, customer, by = "customerID")
telecom <- merge(telecom, churn, by = "customerID")


# Understand the structure of the collated file.
str(telecom)

#Finding and removing/imputing missing values
sapply(telecom, function(x)sum(is.na(x)))

#delete the rows 11 rows with NA in the dataset
telecom<- telecom[!(is.na(telecom$TotalCharges) ), ]

# TotalCharges vs MonthlyCharges 
ggplot(telecom, aes(x = TotalCharges, y = MonthlyCharges, col = tenure)) + geom_point()

cor(telecom$TotalCharges, telecom$MonthlyCharges)
# moderate to high correlation


# TotalCharges vs tenure 
ggplot(telecom, aes(x = TotalCharges, y = tenure, col = tenure)) + geom_point()

cor(telecom$TotalCharges, telecom$tenure)
# highly correlated


# MonthlyCharges vs tenure
ggplot(telecom, aes(x = MonthlyCharges, y = tenure, col = tenure)) + geom_point()

cor(telecom$tenure, telecom$MonthlyCharges)
# low correlation

# plotting churn vs non-churn customers
ggplot(telecom, aes(x = telecom$Churn)) + geom_bar()

# COnverting target variable to factor
telecom$Churn <- as.factor(telecom$Churn)
# Bar Charts of attributes Vs target variable

# Contract Vs Churn
ggplot(telecom, aes(x = telecom$Contract, fill = Churn)) + geom_bar(position = "dodge")
# PaymentMethod Vs Churn
ggplot(telecom, aes(x = telecom$PaymentMethod, fill = (Churn))) + geom_bar(position = "dodge")
# PhoneService Vs Churn
ggplot(telecom, aes(x = telecom$PhoneService, fill = Churn)) + geom_bar(position = "dodge")
# Dependents Vs Churn
ggplot(telecom, aes(x = telecom$Dependents, fill = factor(Churn))) + geom_bar(position = "dodge")
# Partner Vs Churn
ggplot(telecom, aes(x = telecom$Partner, fill = Churn)) + geom_bar(position = "dodge")
# gender Vs Churn
ggplot(telecom, aes(x = factor(telecom$gender), fill = Churn)) + geom_bar(position = "dodge")
# SeniorCitizen Vs Churn
ggplot(telecom, aes(x = factor(telecom$SeniorCitizen), fill = Churn)) + geom_bar(position = "dodge")
# InternetService Vs Churn
ggplot(telecom, aes(x = telecom$InternetService, fill = Churn)) + geom_bar(position = "dodge")
# OnlineSecurity Vs Churn
ggplot(telecom, aes(x = telecom$OnlineSecurity, fill = Churn)) + geom_bar(position = "dodge")
# OnlineBackup Vs Churn
ggplot(telecom, aes(x = telecom$OnlineBackup, fill = Churn)) + geom_bar(position = "dodge")
# DeviceProtection Vs Churn
ggplot(telecom, aes(x = telecom$DeviceProtection, fill = Churn)) + geom_bar(position = "dodge")
# TechSupport Vs Churn
ggplot(telecom, aes(x = telecom$TechSupport, fill = Churn)) + geom_bar(position = "dodge")
# StreamingTV Vs Churn
ggplot(telecom, aes(x = telecom$StreamingTV, fill = Churn)) + geom_bar(position = "dodge")
# StreamingMovies Vs Churn
ggplot(telecom, aes(x = telecom$StreamingMovies, fill = Churn)) + geom_bar(position = "dodge")
# MultipleLines Vs Churn
ggplot(telecom, aes(x = telecom$MultipleLines, fill = Churn)) + geom_bar(position = "dodge")

# Stacked bar charts can also be plotted
# Contract Vs Churn
ggplot(telecom, aes(x = telecom$Contract, fill = Churn)) + geom_bar(position = "stack")
# PaymentMethod Vs Churn
ggplot(telecom, aes(x = telecom$PaymentMethod, fill = (Churn))) + geom_bar(position = "stack")
# PhoneService Vs Churn
ggplot(telecom, aes(x = telecom$PhoneService, fill = Churn)) + geom_bar(position = "stack")
# Dependents Vs Churn
ggplot(telecom, aes(x = telecom$Dependents, fill = factor(Churn))) + geom_bar(position = "stack")
# Partner Vs Churn
ggplot(telecom, aes(x = telecom$Partner, fill = Churn)) + geom_bar(position = "stack")
# gender Vs Churn
ggplot(telecom, aes(x = factor(telecom$gender), fill = Churn)) + geom_bar(position = "stack")
# SeniorCitizen Vs Churn
ggplot(telecom, aes(x = factor(telecom$SeniorCitizen), fill = Churn)) + geom_bar(position = "stack")
# InternetService Vs Churn
ggplot(telecom, aes(x = telecom$InternetService, fill = Churn)) + geom_bar(position = "stack")
# OnlineSecurity Vs Churn
ggplot(telecom, aes(x = telecom$OnlineSecurity, fill = Churn)) + geom_bar(position = "stack")
# OnlineBackup Vs Churn
ggplot(telecom, aes(x = telecom$OnlineBackup, fill = Churn)) + geom_bar(position = "stack")
# DeviceProtection Vs Churn
ggplot(telecom, aes(x = telecom$DeviceProtection, fill = Churn)) + geom_bar(position = "stack")
# TechSupport Vs Churn
ggplot(telecom, aes(x = telecom$TechSupport, fill = Churn)) + geom_bar(position = "stack")
# StreamingTV Vs Churn
ggplot(telecom, aes(x = telecom$StreamingTV, fill = Churn)) + geom_bar(position = "stack")
# StreamingMovies Vs Churn
ggplot(telecom, aes(x = telecom$StreamingMovies, fill = Churn)) + geom_bar(position = "stack")
# MultipleLines Vs Churn
ggplot(telecom, aes(x = telecom$MultipleLines, fill = Churn)) + geom_bar(position = "stack")


# Checking for outliers in numeric variables using box plots. 
boxplot(telecom$tenure)
boxplot.stats(telecom$tenure)$out
boxplot(telecom$MonthlyCharges)
boxplot.stats(telecom$MonthlyCharges)
boxplot(telecom$TotalCharges)
boxplot.stats(telecom$TotalCharges)
# No outliers

# Checking for Duplicates
sum(duplicated(telecom$customerID))
# No duplicates

# Model 1 : K-NN

#Bring the variables in the correct format wrt KNN model
telecom_knn <- telecom

#function converts all values to 1 and 0 for yes and No respectively
telecom_knn$PaperlessBilling <-as.numeric( ifelse(telecom_knn$PaperlessBilling== "Yes",1,0))

telecom_knn$Partner <- as.numeric(ifelse(telecom_knn$Partner == "Yes",1,0))

telecom_knn$Dependents<- as.numeric(ifelse(telecom_knn$Dependents == "Yes",1,0))

telecom_knn$PhoneService<- as.numeric(ifelse(telecom_knn$PhoneService == "Yes",1,0))

# OR 

chartonum <- function(x)
{
  x[x=="Yes"] <- c(1)
  x[x=="No"] <- c(0)
  return(x)
}

## convert Yes to 1 and No to 0 in all the variables
telecom_knn$Partner <- as.numeric(chartonum(telecom_knn$Partner))
telecom_knn$Dependents <- as.numeric(chartonum(telecom_knn$Dependents))
telecom_knn$PhoneService <- as.numeric(chartonum(telecom_knn$PhoneService))
telecom_knn$PaperlessBilling <- as.numeric(chartonum(telecom_knn$PaperlessBilling))


#factorize columns
telecom_knn$MultipleLines <- as.factor(telecom_knn$MultipleLines)
telecom_knn$InternetService <- as.factor(telecom_knn$InternetService)
telecom_knn$OnlineSecurity <- as.factor(telecom_knn$OnlineSecurity)
telecom_knn$OnlineBackup <- as.factor(telecom_knn$OnlineBackup)
telecom_knn$DeviceProtection <- as.factor(telecom_knn$DeviceProtection)
telecom_knn$TechSupport <- as.factor(telecom_knn$TechSupport)
telecom_knn$StreamingTV <- as.factor(telecom_knn$StreamingTV)
telecom_knn$StreamingMovies <- as.factor(telecom_knn$StreamingMovies)
telecom_knn$PaymentMethod <- as.factor(telecom_knn$PaymentMethod)
telecom_knn$Contract <- as.factor(telecom_knn$Contract)
telecom_knn$gender<- as.numeric(ifelse(telecom_knn$gender == "Male",1,0))


# K-NN Model:
# Creating Dummy Variables.
dummy_1 <- as.data.frame(model.matrix( ~ MultipleLines -1, data = telecom_knn))
dummy_2 <- as.data.frame(model.matrix( ~ InternetService -1, data = telecom_knn))
dummy_3 <- as.data.frame(model.matrix( ~ OnlineSecurity -1, data = telecom_knn))
dummy_4 <- as.data.frame(model.matrix( ~ OnlineBackup -1, data = telecom_knn))
dummy_5 <- as.data.frame(model.matrix( ~ DeviceProtection -1, data = telecom_knn))
dummy_6 <- as.data.frame(model.matrix( ~ TechSupport -1, data = telecom_knn))
dummy_7 <- as.data.frame(model.matrix( ~ StreamingTV -1, data = telecom_knn))
dummy_8 <- as.data.frame(model.matrix( ~ StreamingMovies -1, data = telecom_knn))
dummy_9 <- as.data.frame(model.matrix( ~ Contract -1, data = telecom_knn))
dummy_10 <- as.data.frame(model.matrix( ~ PaymentMethod -1, data = telecom_knn))


#remove the primary key from the new dataset
#Collating the data together. 

telecom_knn <- cbind(telecom_knn[,c(10:15,17,19:21)], dummy_1[, -1], dummy_2[, -1], dummy_3[, -1], 
                             dummy_4[, -1], dummy_5[, -1], dummy_6[, -1], dummy_7[, -1], dummy_8[, -1], dummy_9[, -1], 
                             dummy_10[, -1])
# Storing the telecom_knn data set as another object for further use

telecom_knn1 <- telecom_knn
# Scaling continuous Variables

telecom_knn$tenure <- scale(telecom_knn$tenure)
telecom_knn$MonthlyCharges <- scale(telecom_knn$MonthlyCharges)
telecom_knn$TotalCharges <- scale(telecom_knn$TotalCharges)


#Check structure of the new dataset telecom_dataset_knn
str(telecom_dataset_knn)

# Splitting dataset into training and testing
set.seed(2)
subset_data=sample(1:nrow(telecom_knn),0.7*nrow(telecom_knn))
# training data contains 70% of the data
telecom_knn_train=telecom_knn[subset_data,]
#testing data contains 30% of the data
telecom_knn_test=telecom_knn[-subset_data,]

#checking the structure of train and test dataset
str(telecom_knn_train)
str(telecom_knn_test)

#finding optimal K value for the model
#Using the train() command to find the best K.
knn_model <- train(
  Churn~., 
  data=telecom_knn_train,
  method='knn',
  tuneGrid=expand.grid(.k=1:50),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15))
# Optimal k = 27
knn_model
#Generating the plot of the model
plot(knn_model)

# storing True Churn labels of training data in churn_knn
label_knn <- telecom_knn_train[,10]

# KNN - 27 Nearest neighbours 

library(class)
impknn <- knn(telecom_knn_train[,-10],telecom_knn_test[,-10], label_knn, k = 21, prob = TRUE)
table(impknn,telecom_knn_test[,10])
confusionMatrix(impknn, telecom_knn_test[,10], positive = 'Yes')
#accuracy = 0.7929
#senstivity = 0.5722
#specificty = 0.8688

# ROC curve

attr(impknn,"prob") <- ifelse(impknn=='Yes',attr(impknn,"prob"),1 - attr(impknn,"prob"))
#calculating the values for ROC curve
pred_knn <- prediction(attr(impknn,"prob"), telecom_knn_test[,"Churn"])
perf_knn <- performance(pred_knn,"tpr","fpr")


# plotting the ROC curve
plot(perf_knn)

# calculating AUC
auc <- performance(pred_knn,"auc")
auc

# Scaling all the variables except the target variable

telecom_knn1[, -10] <- scale(telecom_knn1[,-10])

# Splitting dataset into training and testing
set.seed(2)
subset_data1=sample(1:nrow(telecom_knn1),0.7*nrow(telecom_knn1))
# training data contains 70% of the data
telecom_knn_train1=telecom_knn1[subset_data1,]
#testing data contains 30% of the data
telecom_knn_test1=telecom_knn1[-subset_data1,]

#checking the structure of train and test dataset
str(telecom_knn_train1)
str(telecom_knn_test1)

#finding optimal K value for the model
#Using the train() command to find the best K.
knn_model <- train(
  Churn~., 
  data=telecom_knn_train1,
  method='knn',
  tuneGrid=expand.grid(.k=1:50),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15))
#k value identified is 49. But k=21 is also almost same accuracy and will have less complexity.
knn_model
#Generating the plot of the model
plot(knn_model)

# storing True Churn labels of training data in churn_knn
label_knn <- telecom_knn_train1[,10]

#KNN - 21 Nearest neighbours 

library(class)
impknn1 <- knn(telecom_knn_train1[,-10],telecom_knn_test1[,-10], label_knn, k = 21, prob = TRUE)
table(impknn1,telecom_knn_test1[,10])
confusionMatrix(impknn1, telecom_knn_test1[,10], positive = 'Yes')
#accuracy = 0.7886
#senstivity = 0.5722
#specificty = 0.8631

# ROC curve

attr(impknn1,"prob") <- ifelse(impknn1=='Yes',attr(impknn1,"prob"),1 - attr(impknn1,"prob"))
#calculating the values for ROC curve
pred_knn1 <- prediction(attr(impknn1,"prob"), telecom_knn_test1[,"Churn"])
perf_knn1 <- performance(pred_knn1,"tpr","fpr")


# plotting the ROC curve
plot(perf_knn1)

# calculating AUC
aucknn1 <- performance(pred_knn1,"auc")


#####--------------------------NAIVE BAYES-----------------------------####

telecom_naive <- telecom

# Bringing the data in the correct format
# We have to make categorical variables as factors and can leave the continuous
# variables as it is.
telecom_naive$Churn <- as.factor(telecom_naive$Churn)
telecom_naive$MultipleLines <- as.factor(telecom_naive$MultipleLines)
telecom_naive$InternetService <- as.factor(telecom_naive$InternetService)
telecom_naive$OnlineSecurity <- as.factor(telecom_naive$OnlineSecurity)
telecom_naive$OnlineBackup <- as.factor(telecom_naive$OnlineBackup)
telecom_naive$DeviceProtection <- as.factor(telecom_naive$DeviceProtection)
telecom_naive$TechSupport <- as.factor(telecom_naive$TechSupport)
telecom_naive$StreamingTV <- as.factor(telecom_naive$StreamingTV)
telecom_naive$StreamingMovies <- as.factor(telecom_naive$StreamingMovies)
telecom_naive$gender <- as.factor(telecom_naive$gender)
telecom_naive$SeniorCitizen <- as.factor(telecom_naive$SeniorCitizen)
telecom_naive$Partner <- as.factor(telecom_naive$Partner)
telecom_naive$Dependents <- as.factor(telecom_naive$Dependents)
telecom_naive$PhoneService <- as.factor(telecom_naive$PhoneService)
telecom_naive$Contract <- as.factor(telecom_naive$Contract)
telecom_naive$PaperlessBilling <- as.factor(telecom_naive$PaperlessBilling)
telecom_naive$PaymentMethod <- as.factor(telecom_naive$PaymentMethod)


# We have seen during EDA that the variables total charges and tenure are highly correlated
# Variables total charges and monthly charges have a moderately high corrlation
# variables monthly charges and tenure have low correlation.
# Therefore we remove the variable Total charges from the data to make the Naive bayes model.
telecom_naive1 <- telecom_naive[,c(-1,-20)]
str(telecom_dataset_naive_new)


#divide the dataset into 70:30 ratio for train and test respectively.
set.seed(2)
s=sample(1:nrow(telecom_naive1),0.7*nrow(telecom_naive1))
telecom_naive_train=telecom_naive1[s,]
telecom_naive_test=telecom_naive1[-s,]

#run Naive Bayes algorithm on this data
impnb <- naiveBayes(Churn ~. , data = telecom_naive_train)

#predict function on test data
#telecom_dataset_naive_test[,-18] indicates dataset without 18th column which is churn
naive_bayes_pred <- predict(impnb, telecom_naive_test[,-19] )
table(naive_bayes_pred, telecom_naive_test[,19])
confusionMatrix(naive_bayes_pred, telecom_naive_test$Churn, positive = "Yes")
#accuracy -72.23%
#specificity - 69.87%
#senstivity-79.07%

#ROC with probabilities 
predraw_nb <- predict(impnb,telecom_naive_test, type = "raw")
predprob_nb <- predraw_nb[,2]
realvec_nb <- ifelse(telecom_naive_test$Churn=="Yes",1,0)
pr_nb <- prediction(predprob_nb, realvec_nb)
perf_nb <- performance(pr_nb, "tpr", "fpr")
plot(perf_nb)
aucnb <- performance(pr_nb, "auc")
aucnb <- aucnb@y.values[[1]]
#auc - 81.3988

###------------------------------------Logistic Regression----------------###

# We can use the data frame telecom_knn1 here directly rather than using the original data frame 
# and bringing the variables in the correct format once again

# Creating a logistic data frame

telecom_logistic <- telecom_knn1

# You need to change the type of Churn variable in to numeric type
#First change levels of this variable into 0 and 1, So, Replace "No" level with "0" and "Yes" with "1"
levels(telecom_logistic$Churn)[1]<- "0"
levels(telecom_logistic$Churn)[2]<- "1"

#Now convert this variable to numeric type

telecom_logistic$Churn <- as.numeric(levels(telecom_logistic$Churn))[telecom_logistic$Churn]

# Removing the customerID column

telecom_logistic <- telecom_logistic[, -1]


# dividing the dataset into 2 parts log_train.data and log_test.data in 7:3 ratio.
set.seed(2)
sample_logistic = sample(1:nrow(telecom_logistic), 0.7*nrow(telecom_logistic))

telecom_logistic_train= telecom_logistic[sample_logistic, ]
telecom_logistic_test = telecom_logistic[-sample_logistic, ]

#initial model model_1 which includes all the variables
model1 <- glm(Churn~. , data = telecom_logistic_train , family = "binomial")
summary(model1)

# Stepwise selection of the variables
step = stepAIC(model1, direction = "both")
step

model2 <- glm(formula = Churn ~ SeniorCitizen + tenure + PaperlessBilling + 
                MonthlyCharges + TotalCharges + MultipleLinesYes + `InternetServiceFiber optic` + 
                InternetServiceNo + OnlineSecurityYes + TechSupportYes + 
                StreamingTVYes + StreamingMoviesYes + `ContractOne year` + 
                `ContractTwo year` + `PaymentMethodElectronic check`, family = "binomial", 
              data = telecom_logistic_train)
summary(model2)
# Select the variables using VIF criterion. 
# Remove multicollinearity through VIF check
vif(model2)

# Monthlycharges, totalcharges and tenure have maximum VIF. The maximum p-value for these 
# 3 variables is for Totalcharges. So we remove that variable from the model

model3 <-  glm(formula = Churn ~ SeniorCitizen + tenure + PaperlessBilling + 
                          MonthlyCharges + MultipleLinesYes + `InternetServiceFiber optic` + 
                          InternetServiceNo + OnlineSecurityYes + TechSupportYes + 
                          StreamingTVYes + StreamingMoviesYes + `ContractOne year` + 
                          `ContractTwo year` + `PaymentMethodElectronic check`, family = "binomial", 
                        data = telecom_logistic_train)
summary(model3)
# Select the variables using VIF criterion. 
# Remove multicollinearity through VIF check
vif(model3)

# Monthlycharges has the maximum VIF and p-value. SO we remove that from our model

model4 <-  glm(formula = Churn ~ SeniorCitizen + tenure + PaperlessBilling 
                 + MultipleLinesYes + `InternetServiceFiber optic` + 
                 InternetServiceNo + OnlineSecurityYes + TechSupportYes + 
                 StreamingTVYes + StreamingMoviesYes + `ContractOne year` + 
                 `ContractTwo year` + `PaymentMethodElectronic check`, family = "binomial", 
               data = telecom_logistic_train)
summary(model4)
# Select the variables using VIF criterion. 
# Remove multicollinearity through VIF check
vif(model4)

# Now all the variables have a VIF of less than 2. So this becomes our final model

# Null deviance: 5741.7  on 4921  degrees of freedom
# Residual deviance: 4096.2  on 4908  degrees of freedom
# AIC: 4124.2

# Model Evaluation
predictedprob_train = predict(model4,  type = "response")
rcorr.cens(predictedprob_train,telecom_logistic_train$Churn)

#c-index for train dataset = 8.481678e-01
predictedprob_test = predict(model4, newdata = telecom_logistic_test[,-9] ,type = "response")
rcorr.cens(predictedprob_test,telecom_logistic_test$Churn)
#c-index for test dataset = 8.349617e-01

#finding ks statistic value for train dataset
model_score_train <- prediction(predictedprob_train,telecom_logistic_train$Churn)
model_perf_train <- performance(model_score_train, "tpr", "fpr")
#plot of model_perf between false positive rate and true positive rate
plot(model_perf_train)
#difference between true positive rate and false positive rate
ks_table <- attr(model_perf_train, "y.values")[[1]] - (attr(model_perf_train, "x.values")[[1]])
ks = max(ks_table)
#ks = 0.54122

which(ks_table == ks)
#1280/4922

auc_train<- performance(model_score_train,"auc")
auc_train
#area under ROC curve = 0.8481

#finding ks statistic value for test dataset
model_score_test <- prediction(predictedprob_test ,telecom_logistic_test$Churn)
model_perf_test <- performance(model_score_test, "tpr", "fpr")
#plot of model_perf_test between false positive rate and true positive rate
plot(model_perf_test)
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
#difference between true positive rate and false positive rate

ks_test = max(ks_table_test)
#ks.test = 0.5162

which(ks_table_test == ks_test)
#658/2110 

auc_test<- performance(model_score_test,"auc")
auc_test
#area under ROC curve = 0.8349

#Threshold value


confusionMatrix(as.numeric(predictedprob_test > 0.5), telecom_logistic_test$Churn)
confusionMatrix(as.numeric(predictedprob_test > 0.7), telecom_logistic_test$Churn, positive = "1")
confusionMatrix(as.numeric(predictedprob_test > 0.3), telecom_logistic_test$Churn, positive = "1")
