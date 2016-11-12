
#-----------------------------------------------------------------------------------------
#----------------------------Linear Regression------------------------
#-----------------------------------------------------------------------------------------

# Load the dataset in working directory 

carmileage <- read.csv("carMPG.csv")

#-----------------------------------------------------------------------------------------

# See the structure of carmileage

str(carmileage)

#-----------------------------------------------------------------------------------------

# Convert Horsepower into numeric variable 

carmileage$Horsepower <- as.numeric(levels(carmileage$Horsepower))[carmileage$Horsepower]

#-----------------------------------------------------------------------------------------

# Again, See structure of carmileage

str(carmileage)

# Check the missing values in the dataset

sum(is.na(carmileage))

# Check the summary of dataset 

summary(carmileage)

#-----------------------------------------------------------------------------------------

# Treat missing values from Horsepower variable.
# Replace NA values with mean of Horsepower variable

carmileage$Horsepower[which(is.na(carmileage$Horsepower))]<- mean(carmileage$Horsepower,na.rm = T)

# check the missing vaule again. 

sum(is.na(carmileage))

#-----------------------------------------------------------------------------------------

# Check the structure of dataset again.
str(carmileage)

# Convert Cylinder and Origin variables into factor variable

carmileage$Cylinders<-as.factor(carmileage$Cylinders)


carmileage$Origin<-as.factor(carmileage$Origin)

#-----------------------------------------------------------------------------------------

# There are 305 levels of Car_name variable, lets first check how to reduce levels of this variable 
# Check the structure of car_name variable 

str(carmileage$Car_Name)

# You can see that the first letter of each string of this variable is the name of car. 

# So, Extract the first word of each string and store it into the same variable "Car_Name"

carmileage$Car_Name <-gsub("\\ .*", "", carmileage$Car_Name)

# Convert this variable into factor 

carmileage$Car_Name <-as.factor(carmileage$Car_Name)


# Check the levels of Car_name variable

## You can see that the spelling of maxda , vokswagen etc are incorrect, so we need to clean these levels are assign the
## levels to corrected one 

levels(carmileage$Car_Name)[7] <- "chevrolet"

levels(carmileage$Car_Name)[31] <- "toyota"

levels(carmileage$Car_Name)[32] <- "volkswagen" 

levels(carmileage$Car_Name)[34] <-"volkswagen"

levels(carmileage$Car_Name)[14] <- "honda"

#-----------------------------------------------------------------------------------------

# Check the structure of carmileage dataset

str(carmileage)


#-----------------------------------------------------------------------------------------

#  
# Create the dummy variables

dummy_1 <- data.frame(model.matrix( ~Car_Name, data = carmileage))
dummy_1<-dummy_1[,-1]

dummy_2 <- data.frame(model.matrix( ~Origin, data = carmileage))
dummy_2<-dummy_2[,-1]

dummy_3 <- data.frame(model.matrix( ~Cylinders, data = carmileage))
dummy_3<-dummy_3[,-1]

# Combine the dummy variables and the numeric columns of carmileage dataset. 

# Store these variables into a new dataset "carmileage_1"

carmileage_1 <- cbind(carmileage[ , c(1,3,4,5,6,7)], dummy_1,dummy_2,dummy_3)

#-----------------------------------------------------------------------------------------

# Divide you data in 70:30

set.seed(100)
indices= sample(1:nrow(carmileage_1), 0.7*nrow(carmileage_1))

train=carmileage_1[indices,]
test = carmileage_1[-indices,]

#-----------------------------------------------------------------------------------------

# Develop the first model 

model_1 <-lm(MPG~.,data=train)
summary(model_1)

#-----------------------------------------------------------------------------------------

# Apply the stepwise method 
library(MASS)

step <- stepAIC(model_1, direction="both")

# create a new model_2 after stepwise method

model_2 <- lm(formula = MPG ~ Horsepower + Weight + Model_year + Car_Namedatsun + 
                Car_Nameford + Car_Namehonda + Car_Namemazda + Car_Namenissan + 
                Car_Nameplymouth + Car_Namepontiac + Car_Nametoyota + Car_Nametriumph + 
                Car_Namevolkswagen + Cylinders4 + Cylinders5 + Cylinders6 + 
                Cylinders8, data = train)

#-----------------------------------------------------------------------------------------

# Check the summary of model_2

 summary(model_2)
 
# Now calculate the VIF of this model

install.packages("car")

library(car)

vif(model_2)

# Remove the variables from the model whose VIF is largest among others 

# "Cylinders4" is having highest vif, Also check the significance of this variable before removing it from model
summary(model_2)

# The significance of this variable is very high, Thus,We cann't remove this variable from model.

# Let's again check the VIF of this model, 
vif(model_2)

# Next, "Cylinders8" variable is second highest vif 

# check the significance of this variable too

summary(model_2)

# Again, this variable is also most significant, Thus we cann't remove this variable 

# check the next variable's vif i.e 3rd highest vif of the model variables 

vif(model_2)

# Next one is Cylinder6, also check the significance of this variable.
summary(model_2)

# Yes, this variable is not significance as much as above two were, So we can remove this variable from model 

#-----------------------------------------------------------------------------------------

# So make a new model, "Model_3". It should not contain "Cylinders6" variable

model_3 <- lm(formula = MPG ~ Horsepower + Weight + Model_year + Car_Namedatsun + 
                Car_Nameford + Car_Namehonda + Car_Namemazda + Car_Namenissan + 
                Car_Nameplymouth + Car_Namepontiac + Car_Nametoyota + Car_Nametriumph + 
                Car_Namevolkswagen + Cylinders4 + Cylinders5 + Cylinders8, data = train)

# Check the summary and VIF of model_3.

summary(model_3)

# We will do the same steps again and again unless we get all the model variables less than the VIF of 2.

# Check the VIF of model_3

vif(model_3)


# Now, Variable "Weight" is having the highest VIF.
# Check the significance of this variable

summary(model_3)

# This variable is also highly significant, So we will check the  2nd largest vif variable and its significane
# That is, "Horsepower" variable and also this variable is not highly significant, So we can remove 
# this variable from our model.
#-----------------------------------------------------------------------------------------

# Remove "Horsepower"
model_4 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                Car_Nameford + Car_Namehonda + Car_Namemazda + Car_Namenissan + 
                Car_Nameplymouth + Car_Namepontiac + Car_Nametoyota + Car_Nametriumph + 
                Car_Namevolkswagen + Cylinders4 + Cylinders5 + Cylinders8, data = train)

# Check summary of model_4

summary(model_4)

# Check VIF of model_4

vif(model_4)

# From the summary and VIF of model_4, we can see that the weight is highest VIF and the Cylinders4
# is second largest VIF. But both the variables are strongly significant. Thus we cann't remove these 
# variables from the model.

# You can see that the 3rd largest VIF variable is "Cylinders8".It is also Insignificant,So we can go ahead
# remove it from our model.

#-----------------------------------------------------------------------------------------

# Remove "Cylinders8"

model_5 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                Car_Nameford + Car_Namehonda + Car_Namemazda + Car_Namenissan + 
                Car_Nameplymouth + Car_Namepontiac + Car_Nametoyota + Car_Nametriumph + 
                Car_Namevolkswagen + Cylinders4 + Cylinders5, data = train)

# Check the summary of model_5

summary(model_5)

# Check the VIF of model_5

vif(model_5)

# Same as above models, both "weight" and "Cylinders4" variables are coming significant, But you can see 
# that the p-value of weight variable is very small than the Cylinders4 variable. If, we remove this variable
# from our model.It would impact on accuracy of model i.e R2 , but if we remove the Cylinders4  
# variable then we could not get drastic change in R2 value.So we can go ahead and remove "Cylinder4" variable
# from the model.

#-----------------------------------------------------------------------------------------

# Remove "Cylinders4"

model_6 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                Car_Nameford + Car_Namehonda + Car_Namemazda + Car_Namenissan + 
                Car_Nameplymouth + Car_Namepontiac + Car_Nametoyota + Car_Nametriumph + 
                Car_Namevolkswagen + Cylinders5, data = train)

summary(model_6)

vif(model_6)

# VIF of all the variables are less than 2. Now, we have to remove insignificant variables on the basis
# of p-value.

#-----------------------------------------------------------------------------------------

# Based on significance level we can remove the variables from the model 
# Check the summary of model_6

summary(model_6)

#-----------------------------------------------------------------------------------------

# P-value of car_Name nissan variable is largest, Thus remove "Car_Namenissan" from the model.

model_7 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                Car_Nameford + Car_Namehonda + Car_Namemazda +  
                Car_Nameplymouth + Car_Namepontiac + Car_Nametoyota + Car_Nametriumph + 
                Car_Namevolkswagen + Cylinders5, data = train)

summary(model_7)

#-----------------------------------------------------------------------------------------

# Same here,
# Remove "Car_Nameford"

model_8 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                Car_Namehonda + Car_Namemazda +Car_Nameplymouth + Car_Namepontiac + 
                Car_Nametoyota + Car_Nametriumph +Car_Namevolkswagen + Cylinders5, data = train)
summary(model_8)

#-----------------------------------------------------------------------------------------

# Remove "Cylinders5"

model_9 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                Car_Namehonda + Car_Namemazda +Car_Nameplymouth + Car_Namepontiac + 
                Car_Nametoyota + Car_Nametriumph +Car_Namevolkswagen, data = train)

summary(model_9)

#-----------------------------------------------------------------------------------------
#Remove "Car_Namemazda"

model_10 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                 Car_Namehonda  + Car_Nameplymouth + Car_Namepontiac + 
                 Car_Nametoyota + Car_Nametriumph +Car_Namevolkswagen, data = train)

summary(model_10)

# Remove "Car_Namehonda"

model_11 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                  Car_Nameplymouth + Car_Namepontiac + 
                 Car_Nametoyota + Car_Nametriumph +Car_Namevolkswagen, data = train)

summary(model_11)

#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------

# Remove "Car_Nametoyota" 

model_12 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                 Car_Nameplymouth + Car_Namepontiac + 
                 Car_Nametriumph +Car_Namevolkswagen, data = train)

summary(model_12)

#-----------------------------------------------------------------------------------------
# Remove "Car_triumph"

model_13 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                 Car_Nameplymouth + Car_Namepontiac + 
                 Car_Namevolkswagen, data = train)

summary(model_13)

#-----------------------------------------------------------------------------------------
# Remove "Car_Nameplymouth"

model_14 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                  Car_Namepontiac + Car_Namevolkswagen, data = train)

summary(model_14)

#-----------------------------------------------------------------------------------------

# Remove "Car_Namepontiac"
model_15 <- lm(formula = MPG ~ Weight + Model_year + Car_Namedatsun + 
                 Car_Namevolkswagen, data = train)

summary(model_15)

#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------

# Model_15 is the final model for predicting the future outcomes.

# Ideally, model should not contain more than 4 variables. 
# This is very subject call to choose the p-value, ideally we prefer variables in model whosep-value is less than 0.05
# But for the large dataset, we abide this rule and consider the variables which is the most significant amongest them 

#-----------------------------------------------------------------------------------------
#----------------------------Model Validation and Prediction -----------------------------

# Test this model to the test dataset.

Predict1 <- predict(model_15,test[,-1])

#-----------------------------------------------------------------------------------------

# Add a new column "test_predict" into the test dataset


test$test_MPG <- Predict1
#-----------------------------------------------------------------------------------------

# test R2 value.
cor(test$MPG,test$test_MPG)
cor(test$MPG,test$test_MPG)^2

#-----------------------------------------------------------------------------------------

#-----------------------------(Additional information)------------------------------------
# Now, calculate the RMSE value of test and train dataset 

# Before using rmse function, first install "hydroGOF" package 

install.packages("hydroGOF")

# load the package into your current working directory

library(hydroGOF)

#-----------------------------------------------------------------------------------------

# Now calculate the rmse between predicted_MPG and actual_MPG from the test dataset

rmse(test$MPG,test$test_MPG)

#-----------------------------------------------------------------------------------------

# Now, We want to compare rmse of test and train dataset

# First apply model_15 into train dataset and store it into the same dataset 

 train_Predict1 <- predict(model_15,train[,-1])

 train$train_MPG <- train_Predict1
 
# Now, calculate rmse between train_MPG and the actual MPG of train dataset 
 
 rmse(train$MPG,train$train_MPG)
 
#-----------------------------------------------------------------------------------------
 

# Both the values are nearly same, Thus this model is validated can be used for the prediction of future outcomes

#-----------------------------------------------------------------------------------------
 
 
#-----------------------------------------------------------------------------------------------------The End
 