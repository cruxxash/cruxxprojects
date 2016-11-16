############## ---------------- STEP 1 - BUSINESS UNDERSTANDING  ######################
#in this assignment we are finding out the prediction of car mileage based on the datasets given to fit the customer preferences

#===========================================================================================================================================================================#

########### -------------------- STEP 2- DATA CLEANING  ##############################

#setting work directory
setwd("C:/DA/Course 3")

#importing packages
install.packages("MASS")
install.packages("car")
install.packages("ggplot2")
library(MASS)
library(car)
library(ggplot2)

#importing dataset of carmileage and storing it in carmileage variable
carmileage<-read.csv("carMPG.csv",stringsAsFactors = F)

#checking the structure of the dataset
str(carmileage)
View(carmileage)

########################    MISSING VALUE TREATMENT  #######################################

#checking total missing values of the dataset

sum(is.na(carmileage))

#there are no missing values in the data set, now we can procede for the Outlier Treatment

#Checking unique rows in the data set
unique(carmileage)

#All the rows are unique in this data set which means we can procede further for the Outlier Treatment

#---------------------------------------------------------------------------------------------------------------#

########  OUTLIER TREATMENT  ################################################################

##checking outliers for cylinders
quantile(carmileage$Cylinders,seq(0,1,0.01))
table(carmileage$Cylinders)

#converting cylinders to factor type
carmileage$Cylinders<-as.factor(carmileage$Cylinders)


#as there are no outliers for cylinders, we proceed further


##checking outliers for displacement
quantile(carmileage$Displacement,seq(0,1,0.01))
qqPlot(carmileage$Displacement)
table(carmileage$Displacement)
boxplot.stats(carmileage$Displacement)


#binning displacement from 0-100, 100-200, 200-300,300-400, 400 Above
carmileage$Displacement1[which(carmileage$Displacement < 100.00)]<-"Below 100"
carmileage$Displacement1[which(carmileage$Displacement >= 100.00 & carmileage$Displacement <= 200.00)]<-"100-200"
carmileage$Displacement1[which(carmileage$Displacement > 200.00 & carmileage$Displacement <= 300.00)]<-"200-300"
carmileage$Displacement1[which(carmileage$Displacement > 300.00 & carmileage$Displacement <= 400.00)]<-"300-400"
carmileage$Displacement1[which(carmileage$Displacement > 400)]<-"Above 400"
carmileage$Displacement1[which(is.na(carmileage$Displacement1))]<-"Below 100"

sum(is.na(carmileage$Displacement1))

#converting the bins to factors
carmileage$Displacement1<-as.factor(carmileage$Displacement1)


##checking outliers for horsepower
table(carmileage$Horsepower)

#as there are 6 values with a "?", we treat it

#converting horsepower to numeric data set
carmileage$Horsepower<-as.numeric(carmileage$Horsepower)
table(carmileage$Horsepower)

#NA values are introduced
sum(is.na(carmileage$Horsepower)) #6 NA Values to be treated

#now we find the mean of the horsepower and replace the NA values with the mean
mean(carmileage$Horsepower,na.rm = T)

#the mean is 104.4694, we not replace the NA values with the mean
carmileage$Horsepower[which(is.na(carmileage$Horsepower))]<-mean(carmileage$Horsepower,na.rm = T)
table(carmileage$Horsepower)
mean(carmileage$Horsepower)

#now that NA values are removed, we check for outliers
quantile(carmileage$Horsepower,seq(0,1,0.01))
boxplot.stats(carmileage$Horsepower)

#we see that there is a huge jump from 97% to 98%, so capping and flooring all values above 97% to the value of 97%
carmileage$Horsepower[which(carmileage$Horsepower>198)]<-198


##checking for Weight
quantile(carmileage$Weight,seq(0,1,0.01))
boxplot.stats(carmileage$Weight)

#as there are no outliers for weight, we proceed further

##checking for acceleration
#converting it to numeric type
carmileage$Acceleration<-as.numeric(carmileage$Acceleration)

#checking outliers
quantile(carmileage$Acceleration,seq(0,1,0.01))
boxplot.stats(carmileage$Acceleration)

#there are outliers between 0% to 1% and from 99% to 100%, so capping and flooring the outliers to 1% and 99% respectively
carmileage$Acceleration[which(carmileage$Acceleration<9)]<-9
carmileage$Acceleration[which(carmileage$Acceleration>22.239)]<-22.239


##checking model year
quantile(carmileage$Model_year,seq(0,1,0.01))
boxplot.stats(carmileage$Model_year)

#Binning model year into 2 groups , Below 2006, 2006-2012 and Above 2012
carmileage$Model_year1[which(carmileage$Model_year<2006)]<-"Below 2006"
carmileage$Model_year1[which(carmileage$Model_year>=2006&&carmileage$Model_year<=2012)]<-"2006-2012"
carmileage$Model_year1[which(carmileage$Model_year>2012)]<-"Above 2012"
carmileage$Model_year1[which(is.na(carmileage$Model_year1))]<-"2006-2012"

#converting it into a factor variable
carmileage$Model_year1<-as.factor(carmileage$Model_year1)

#origin
quantile(carmileage$Origin,seq(0,1,0.01))
boxplot.stats(carmileage$Origin)

#model year and origin do not have outliers

#car name
str(carmileage$Car_Name)
unique(carmileage$Car_Name)

#now taking only the 1st word from the string of car names as these are the company names

carmileage<-carmileage[order(carmileage$Car_Name),] #ordering the dataset
carmileage$Car_Name<-gsub(" .*","",carmileage$Car_Name)
carmileage$Car_Name<-trimws(carmileage$Car_Name)
unique(carmileage$Car_Name)
table(carmileage$Car_Name)

#now we observe that there are many wrong company names in the car_names
carmileage$Car_Name[which(carmileage$Car_Name=="chevroelt")]<-"chevrolet"
carmileage$Car_Name[which(carmileage$Car_Name=="maxda")]<-"mazda"
carmileage$Car_Name[which(carmileage$Car_Name=="toyouta")]<-"toyota"
carmileage$Car_Name[which(carmileage$Car_Name=="mercedes")]<-"mercedes-benz"
carmileage$Car_Name[which(carmileage$Car_Name=="vokswagen")]<-"volkswagen"
carmileage$Car_Name[which(carmileage$Car_Name=="chevy")]<-"chevrolet"
carmileage$Car_Name[which(carmileage$Car_Name=="capri")]<-"ford"
carmileage$Car_Name[which(carmileage$Car_Name=="chevy")]<-"chevrolet"
carmileage$Car_Name[which(carmileage$Car_Name=="vw")]<-"volkswagen"

#converting car names to factor variable
carmileage$Car_Name<-as.factor(carmileage$Car_Name)
str(carmileage)

#--------------------------------------------------------------------------------------------#
######################### STEP 3- DATA PREPARATION #################################################################

##############     CREATING DUMMY VARIABLES      #################################################

#creating dummy variables for cylinders, displacement1, model_year1, and car_name

#dummy variable for cylinders
dummy1<-data.frame(model.matrix(~Cylinders,data=carmileage))
dummy1<-dummy1[,-1]

#dummy variable for displacement1
dummy2<-data.frame(model.matrix(~Displacement1,data=carmileage))
dummy2<-dummy2[,-1]

#dummy variable for modelyear1
dummy3<-data.frame(model.matrix(~Model_year1,data=carmileage))
dummy3<-dummy3[,-1]  
  
dummy4<-data.frame(model.matrix(~Car_Name,data=carmileage))
dummy4<-dummy4[,-1]



#now combining dummy variables and the carmileage dataset

carmileage1<-cbind(carmileage[,-c(2,3,7,9,10,11)],dummy1,dummy2,dummy3,dummy4)

#now our data set is ready

#-------------------------------------------------------------------------------------------#
#############       BUILDING TRAIN AND TEST DATASETS          ##############################

# WE now seggregate the data set into training and testing datasets in 70:30 ratio
set.seed(100)

indices=sample(1:nrow(carmileage1),0.7*nrow(carmileage1))

train<-carmileage1[indices,]
test<-carmileage1[-indices,]

#now we are ready to build models on the train dataset

#=========================================================================================================================================================================#

############### STEP 4 -  MODEL DEVELOPMENT ############################################

#Model building is done considering factors like VIF, p-value and correlation of the independent variables. I am building the model first based on VIF and correlation and then using the p-value approach

#Target is to achieve more than 80% R Square

#building model1

model_1<-lm(MPG~.,carmileage1)
summary(model_1)

#Removing insignificant variables using Stepwise Reduction of variables

step<-stepAIC(model_1,direction="both")

step

#making model_2 and checking VIF

#####################################   VIF BASED MODEL  ###############################


model_2<-lm(formula = MPG ~ Horsepower + Weight + Acceleration + Cylinders4 + 
              Cylinders5 + Cylinders6 + Cylinders8 + Displacement1Above.400 + 
              Displacement1Below.100 + Model_year1Above.2012 + Model_year1Below.2006 + 
              Car_Namebuick + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda + Car_Namemazda + Car_Namenissan + Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)
vif(model_2)
summary(model_2)



#we observe that cylinder4 has the highest VIF which is exceedingly larger than 2, but is highly significant, so we remove cylinder4


model_3<-lm(formula = MPG ~ Horsepower + Weight + Acceleration  + 
              Cylinders5 + Cylinders6 + Cylinders8 + Displacement1Above.400 + 
              Displacement1Below.100 + Model_year1Above.2012 + Model_year1Below.2006 + 
              Car_Namebuick + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda + Car_Namemazda + Car_Namenissan + Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)
vif(model_3)
summary(model_3)

#now we find that Horsepower has highest VIF and is significant significant, so is Weight, so we check the correlation between 2
cor(carmileage1$Horsepower,carmileage1$Weight)

#we find that the both are highly correlated, so we remove horsepower

model_4<-lm(formula = MPG ~  Weight + Acceleration  + 
              Cylinders5 + Cylinders6 + Cylinders8 + Displacement1Above.400 + 
              Displacement1Below.100 + Model_year1Above.2012 + Model_year1Below.2006 + 
              Car_Namebuick + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda + Car_Namemazda + Car_Namenissan + Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)

vif(model_4)
summary(model_4)

#we find that weight has highest vif and second is cylinder8, so we check the correlation between the two
cor(carmileage1$Weight,carmileage1$Cylinders8)
#since the two are highly correlated, we remove cylinder 8 as if has no significance

model_5<-lm(formula = MPG ~  Weight + Acceleration  + 
              Cylinders5 + Cylinders6  + Displacement1Above.400 + 
              Displacement1Below.100 + Model_year1Above.2012 + Model_year1Below.2006 + 
              Car_Namebuick + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda + Car_Namemazda + Car_Namenissan + Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)

vif(model_5)
summary(model_5)

#next, we find that weight has highest vif and second highest is Displacement1Below.100, checking the correlation between the two
cor(carmileage1$Weight,carmileage1$Displacement1Below.100)
#as these both are negatively correlated, we chose to remove Displacement1Below.100 from the model

model_6<-lm(formula = MPG ~  Weight + Acceleration  + 
              Cylinders5 + Cylinders6  + Displacement1Above.400  + Model_year1Above.2012 + Model_year1Below.2006 + 
              Car_Namebuick + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda + Car_Namemazda + Car_Namenissan + Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)

vif(model_6)
summary(model_6)

############# ---------- P-VALUE BASED MODELS  #################

# mazda company has the highest p-value, so removing them from the model

model_7<-lm(formula = MPG ~  Weight + Acceleration  + 
              Cylinders5 + Cylinders6  + Displacement1Above.400  + Model_year1Above.2012 + Model_year1Below.2006 + 
              Car_Namebuick + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda  + Car_Namenissan + Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)

vif(model_7)
summary(model_7)

# buick company has the highest p-value, so removing them from the model

model_8<-lm(formula = MPG ~  Weight + Acceleration  + 
              Cylinders5 + Cylinders6  + Displacement1Above.400  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda  + Car_Namenissan + Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)

vif(model_8)
summary(model_8)

# now nissan Company has the highest p-value, so removing them from the model

model_9<-lm(formula = MPG ~  Weight + Acceleration  + 
              Cylinders5 + Cylinders6  + Displacement1Above.400  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
              Car_Namehonda  +  Car_Nameoldsmobile + 
              Car_Nameplymouth + Car_Namepontiac + Car_Namerenault + Car_Nametriumph, 
            data = carmileage1)

vif(model_9)
summary(model_9)

# Triumph company has the highest p-value, so removing them from the model

model_10<-lm(formula = MPG ~  Weight + Acceleration  + 
               Cylinders5 + Cylinders6  + Displacement1Above.400  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
               Car_Namehonda  +  Car_Nameoldsmobile + 
               Car_Nameplymouth + Car_Namepontiac + Car_Namerenault , 
             data = carmileage1)

vif(model_10)
summary(model_10)

# oldsmobile company has the highest p-value, so removing them from the model

model_11<-
  model_10<-lm(formula = MPG ~  Weight + Acceleration  + 
                 Cylinders5 + Cylinders6  + Displacement1Above.400  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
                 Car_Namehonda   + 
                 Car_Nameplymouth + Car_Namepontiac + Car_Namerenault , 
               data = carmileage1)

vif(model_11)
summary(model_11)

#cylinder5 has the highest p-value, so removing them from the model

model_12<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Displacement1Above.400  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
               Car_Namehonda   + 
               Car_Nameplymouth + Car_Namepontiac + Car_Namerenault , 
             data = carmileage1)

vif(model_12)
summary(model_12)

# displacement above 400 has the highest p-value, so removing them from the model

model_13<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
               Car_Namehonda   + 
               Car_Nameplymouth + Car_Namepontiac + Car_Namerenault , 
             data = carmileage1)

vif(model_13)
summary(model_13)

# Plymouth company has the highest p-value, so removing them from the model

model_14<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
               Car_Namehonda    + Car_Namepontiac + Car_Namerenault , 
             data = carmileage1)

vif(model_14)
summary(model_14)

# Renault has the highest p-value, so removing them from the model

model_15<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006  + Car_Namecadillac + Car_Namedatsun + Car_Nameford + 
               Car_Namehonda    + Car_Namepontiac  , 
             data = carmileage1)

vif(model_15)
summary(model_15)

# Cadillac has the highest p-value, so removing them from the model

model_16<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006   + Car_Namedatsun + Car_Nameford + 
               Car_Namehonda    + Car_Namepontiac  , 
             data = carmileage1)

vif(model_16)
summary(model_16)

# Honda has the highest p-value, so removing them from the model

model_17<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006   + Car_Namedatsun + Car_Nameford   + Car_Namepontiac  , 
             data = carmileage1)

vif(model_17)
summary(model_17)

# Pontiac company has the highest p-value, so removing them from the model

model_18<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006   + Car_Namedatsun + Car_Nameford    , 
             data = carmileage1)

vif(model_18)
summary(model_18)

# Ford company has the highest p-value, so removing them from the model

model_19<-lm(formula = MPG ~  Weight + Acceleration   + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006   + Car_Namedatsun , 
             data = carmileage1)

vif(model_19)
summary(model_19)

# Acceleration has the highest p-value, so removing them from the model

model_20<-lm(formula = MPG ~  Weight + Cylinders6  + Model_year1Above.2012 + Model_year1Below.2006   + Car_Namedatsun , 
             data = carmileage1)

vif(model_20)
summary(model_20)


############## ----- MODEL SUCCESS #################################

# Adjusted R square = 0.8212
# Model contains 5 variables

#### FINAL MODEL ############################################
#-  model_20 is the model which would be best for prediction of MPG for the business requirement ####

############################# STEP 5 - MODEL DEPLOYMENT ###############################################

##### ---------- PREDICTION  ################

predict_1<-predict(model_20,test)

test$mpgpredicted<-predict_1

######################### STEP 6 - VALIDATING THE BUILT MODEL #############################################
######### --------- PREDICTED MODEL R SQUARE  ######################################

#find out the square of correlation between the test data value and the training data value

cor(test$MPG,test$mpgpredicted)^2

# R square for prediction is 0.84

#so it is safe to say that the prediction is around 84% TRUE


##################### END OF ASSIGNMENT  ##################################################
