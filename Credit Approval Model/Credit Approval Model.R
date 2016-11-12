#-------------------------------------------------------------------------------------------
#---------------------------------Gramener Case Study------------------------------
#-------------------------------------------------------------------------------------------
# install.packages("GGally")
# install.packages("ggplot2")
# install.packages("stats")
# install.packages("MASS")

library(stats)
library(MASS)
library(GGally)
library(ggplot2)

###--------------------------- Checkpoint-1: Data Preparation-------------------------------


## Load the file loan.csv into data frame loan

loan <-read.csv("loan.csv",stringsAsFactors = F)

#-------------------------------------------------------------------------------------------

### Check structure of loan dataset

str(loan)
#(There are many columns which might not contains any values let's check that- 
# - This step is not Required-EXTRA INFO)

## Remove the column which are containing only NA values 

# 1. First, store the Na column name in a vector "Na_column_names" 
Na_column_names <- names(loan)[colSums(is.na(loan))==nrow(loan)]

# 2. Lastly, Remove the column from the main dataset (%in% is used for mapping)
loan <- loan[,!(colnames(loan) %in% Na_column_names)]

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

### Impute the NA values for all driver variables.

# 3. In this case study our objective is to work with only driving variables, Let's create 
# the object "Loan_driving" and store the driving variables as mentioned in the
# problem statement.

## Store the names of driving variables into a vector "drivers"

drivers <- c("loan_amnt", "funded_amnt","annual_inc", "int_rate", 
             "grade", "dti", "emp_length","purpose","home_ownership",
             "loan_status")
#------------------------------------------------------------------------------------------

# Now, Map drivers vector to the load dataset and store it into the same dataset.

loan <-loan[,colnames(loan) %in% drivers]

# Let's check the structure of loan dataset
str(loan)

# Check the "NA" values in loan dataset

sum(is.na(loan))

# Let's check the summary of loan dataset

summary(loan) 
#(In loan_amnt,funded_amnt and the dti are containing 7 NA values.It means that these -
# - NA's might be from same obsevations)

#------------------------------------------------------------------------------------------

#  Let's check the this Na's 

Na_values<- subset(loan,is.na(loan$loan_amnt))

# So, we need to remove these observations from the loan dataset.

loan <- subset(loan,!is.na(loan$loan_amnt))

# Check the NA's again

sum(is.na(loan))
#(Still 4 NA's are present)

# Check summary again 

summary(loan)
# (All these 4 NA's are present in annual_inc variables, Thus we need to replace -
# - these Na's with mean)

loan[which(is.na(loan$annual_inc)),]$annual_inc <- mean(loan$annual_inc,na.rm = T)

# Again, Check the NA's 

sum(is.na(loan))
# (Removed all the NA's from loan dataset)

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

### Remove rows with loan_status  = "Fully Paid"

loan<-subset(loan, loan$loan_status != "Fully Paid")
loan<-subset(loan, loan$loan_status != "Does not meet the credit policy. Status:Fully Paid")

#------------------------------------------------------------------------------------------

# Let's check the structure of dataset once 

str(loan)
(# You can see that most of the variables are not in a proper class and also contains some symbols) 

#------------------------------------------------------------------------------------------

# Remove % symbol from "int_rate" variable 
loan$int_rate <- as.numeric(sub("%","0",loan$int_rate))

# convert "grade" variable to factor
loan$grade <- as.factor(loan$grade)

# First, convert emp_length into numeric. you have to remove all the non-numeric values from-
# - each cells of emp_length variable.
loan$emp_length <- as.numeric(gsub("[^0-9]", "", loan$emp_length))

# Convert "home_ownership" variable to factor
loan$home_ownership <- as.factor(loan$home_ownership)

# Convert "purpose" variable to factor
loan$purpose <- as.factor(loan$purpose)

# Convert "Loan status" variable to factor
loan$loan_status <- as.factor(loan$loan_status)

#------------------------------------------------------------------------------------------

# Let's check the NA's again because there might be some chances of coersion after changing class of variables 

sum(is.na(loan)) 
#(Okay, 288 NA's are coersed)
# So, Let's check the summary of loan dataset

summary(loan)
#(You can see that the emp_length variable contain all the 288 NA's in total 7597 observation
# we cann't replace NA's with mean or mode beacuse all the obsevations of emp_length(industry experience)
# -are independent to each other)
# (In this case we can safely remove this observations from the loan dataset)

loan<-subset(loan, !is.na(loan$emp_length))

# Let's again check NA's value
sum(is.na(loan))
#----------------------------------------------------------------------------------------------------- 

### Create a new column named loan_status_1  with three levels current_new, default_new and late such that
# rows with loan_status as "current" or "in grace period" are tagged as  "current_new"
# rows with loan_status as " default" or "charged off" are tagged as "default_new"
# rows with loan_status as " late 16- 30 days" "late 31-120 days" are tagged as "late"

# First create a New variable "Loan_status_1" in the current loan data and assign each observation to NA.
loan$loan_status_1 <- "NA" 

## rows with loan_status as "current" or "in grace period" are tagged as  "current_new" 
# So tag "In grace Period" and "Current" as "current_new" in "loan_status_1" column
loan$loan_status_1[(which(loan$loan_status == "In Grace Period" | loan$loan_status == "Current"))]<- "current_new"

# rows with loan_status as " default" or "charged off" are tagged as "default_new"
loan$loan_status_1[which(loan$loan_status == "Default" | loan$loan_status == "Charged Off"| loan$loan_status=="Does not meet the credit policy. Status:Charged Off")]<-"default_new"

# rows with loan_status as " late 16- 30 days" "late 31-120 days" are tagged as "late"
loan$loan_status_1[which(loan$loan_status == "Late (31-120 days)" | loan$loan_status == "Late (16-30 days)")]<-"late"

## Convert it to factor 
loan$loan_status_1 <- as.factor(loan$loan_status_1)

#--------------------------------------------------------------------------------------------------------  
# Create new bin variables for int_rate and emp_length respectively as follows:

# Create int_rate_grp such that int_rate < 10 is tagged "Low"; int_rate (10-18) is tagged 
# "Medium"; int_rate (>18) is tagged "High"

# First create "int_rate_grp" variable in loan dataset and assign all the observation to NA.
loan$int_rate_grp <-NA

loan$int_rate_grp[which(loan$int_rate < 10 )]<-"Low"

loan$int_rate_grp[which(loan$int_rate >=10 & loan$int_rate <=18 )]<-"Medium"

loan$int_rate_grp[which(loan$int_rate > 18 )]<-"High"

loan$int_rate_grp <- as.factor(loan$int_rate_grp)

# Create emp_len_grp such that emp_length (0-4) is tagged as "Junior"; emp_length (5-8) is
# tagged as "Mid-level"; emp_length (>8) is tagged as "Senior".

# Create a new column "emp_len_grp" in loan dataset
loan$emp_len_grp <- NA

loan$emp_len_grp[which(loan$emp_length <= 4)]<-"Junior"

loan$emp_len_grp[which(loan$emp_length > 8 )]<-"Senior"

loan$emp_len_grp[which(is.na(loan$emp_len_grp))] <- "Mid-level"

loan$emp_len_grp <- as.factor(loan$emp_len_grp)
#-----------------------------------------------------------------------------------------------     

#----------------------- Checkpoint 2: Exploratory Data Analysis -------------------------------

###Univariate Analysis###

# Summary Statistics
# Distribution plot
# Outlier treatment

#-----------------------------------------------------------------------------------------------     

summary(loan)

#Lets plot the density curve for driving varialbes to know the distribution --------------------

# Outlier Detection 

loan_amount <- ggplot(loan, aes(x=loan_amnt)) + geom_density()+
  ggtitle("Density plot of loan amount")+ xlab("loan_amnt") + ylab("Density") 

loan_amount
#(The density of obsevations are spread over the range from 0 - 35000. Here we could not see any kind of outlier as such)
# (Let's do the boxplot for same)
boxplot_loan_amount <- ggplot(loan, aes(x=1, y=loan_amnt)) + geom_boxplot(outlier.colour = "Blue")  + xlab("Width of boxplot") + ylab("Loan Amount") + ggtitle("Loan Amount plot")

boxplot_loan_amount

# No need to remove(orcapping/flooring)outliers for this variable
# No outlier Treatment
#-----------------------------------------------------------------------------------------------   

# Similarly,Let's check density and boxplot for rest of the continueous variables 

# Outlier Detection

# Funded amount variable
loan_fund_amount <- ggplot(loan, aes(x=funded_amnt)) + geom_density() + xlab("Funded Amnt") + ylab("Density") +  ggtitle("Funded Amnt plot")

# Do the boxplot
boxplot_funded_amount <- ggplot(loan, aes(x=1, y=funded_amnt)) + geom_boxplot(outlier.colour = "Blue") + xlab("Funded Amnt") + ylab("Boxplot") +  ggtitle("Funded Amnt Box plot")

boxplot_funded_amount

#-----------------------------------------------------------------------------------------------   
# Outlier Treatment

# Yes,we need to cap the outlier values with (Q3+ 1.5*IQR) value

Q3 <- quantile(loan$funded_amnt,.75)

# Let's first calulate the value of upper cap i.e (Q3+ 1.5*IQR) 

Uppercap <- Q3[[1]]+1.5*IQR(loan$funded_amnt)

# cap all the values of observation above the uppercap value(33500) in funded amount variable

loan$funded_amnt[which(loan$funded_amnt >Uppercap)] <- Uppercap

# Check the boxplot again 

boxplot_funded_amount <- ggplot(loan, aes(x=1, y=funded_amnt)) + geom_boxplot(outlier.colour = "Blue") + xlab("Funded Amnt") + ylab("Boxplot") +  ggtitle("Funded Amnt Box plot")

boxplot_funded_amount 
#--------------------------------------------------------------------------------------------------------   

# For annual income variable

# Outlier Detection

Plot_income <- ggplot(loan, aes(x=annual_inc)) + geom_density() +
  ggtitle("Density plot for annual income")+ xlab("Annual income") + ylab("Density")

# (Plot looks like postive skewed but let's also detect the outliers too.

BoxPlot_income <- ggplot(loan, aes(x=1, y=annual_inc)) + geom_boxplot(outlier.colour = "Blue")+ ggtitle("Annual Income plot") + xlab("Annual_income") + ylab("Boxplot")

#(Clearly visible outliers in this variables)

#--------------------------------------------------------------------------------------------------------   

# Outlier Treatment 

Q3_income <- quantile(loan$annual_inc,.75)

# Let's first calulate the value of upper cap i.e (Q3+ 1.5*IQR) 

Uppercap_1 <- Q3_income[[1]]+1.5*IQR(loan$annual_inc)

# cap all the values of observation above the uppercap value(Uppercap_1) in  amount variable

loan$annual_inc[which(loan$annual_inc >Uppercap_1)] <- Uppercap_1

# Again do the boxplot

BoxPlot_income <- ggplot(loan, aes(x=1, y=annual_inc)) + geom_boxplot(outlier.colour = "Blue")+ ggtitle("Annual Income plot") + xlab("Annual_income") + ylab("Boxplot")

#--------------------------------------------------------------------------------------------------------   
#--------------------------------------------------------------------------------------------------------

# For debt to income ratio variable (dti)

# Outlier Detection

loan_dti<- ggplot(loan, aes(x=dti)) + geom_density() + 
  xlab("DTI") + ylab("Density") +  ggtitle("Density plot")

loan_dti
#( This variable is distributed evenly Thus we can say there should not be present any outliers)
# Let's look the outlier if present and 

boxplot_dti <- ggplot(loan, aes(x=1, y=dti)) + geom_boxplot(outlier.colour = "Blue") + xlab("DTI") + ylab("Boxplot") +  ggtitle("Box_Plot")

boxplot_dti

# No outliers present
#----------------------------------------------------------------------------------------------- 

#  Let's also explore categorical variables - Univariate Analysis----------------------------- 

## Grade variable

ggplot(loan, aes(x = grade)) + geom_bar(color = "red",fill = "grey") + ggtitle("Garde analysis") +xlab("Grade")+ ylab("Frequency")

# You can see, maximum number of loans are passed under B grade and very few are passed under G grade.

#-----------------------------------------------------------------------------------------------   

## Home ownership variable

ggplot(loan, aes(x = home_ownership)) + geom_bar(color = "Red", fill = "grey") +ggtitle("Home Ownership analysis") +xlab("Home Ownership")+ ylab("Frequency")

# you can see, the maximum number of loan seekers had taken the loan followed by them who mortaged their homes.

#-----------------------------------------------------------------------------------------------  
## Purpose variable

ggplot(loan, aes(x = purpose)) + geom_bar(color = "Red", fill = "grey") + ggtitle("Purpose of loan")+xlab("Purpose")+ ylab("Frequency")

# You can see that the highest number of bank customers have taken loan for debt consolidation. Debt consolidation is taking one loan to payoff all other loans.
# This means servicing only one loan and results in lower rate of interest for the consumer.

#-----------------------------------------------------------------------------------------------  
## Lets see loan_status_l variable  

ggplot(loan, aes(x = loan_status_1)) +geom_bar(color = "Red",fill = "grey") + ggtitle("Loan Status Analysis") +xlab("Loan Status")+ ylab("Frequency")

# In the above plot, you can outline that the most of the loans were in new_defaulter categories.

#-----------------------------------------------------------------------------------------------  
## int_rate_grp Variable

ggplot(loan, aes(x = int_rate_grp)) + geom_bar(color = "Red", fill = "grey") + ggtitle("Interest Rate Analysis") + xlab("Interest Rate")+ ylab("Frequency")

# Maximum loan seekers are taken loan at medium intrest rates.


#-----------------------------------------------------------------------------------------------
## count of years of experience

ggplot(loan, aes(x = emp_len_grp)) + geom_bar(color = "Red", fill = "grey") + ggtitle("Experience Level Analysis") + xlab("experience Level")+ ylab("Frequency")

# Maximum loan given to employees at junior level.

# Under univariate analysis, We had tried to cover most of the analysis. but yes, It's very subjective call 
# to whom you need to explore thoughly. 

# Now, Let's move on to the analysis of more than one variable at a time. 

#-------------------------------------------------------------------------------------------------------

#### -------------------------Checkpoint 2:  Multivariate analysis ---------------------------------------

### Correlation table

#Lets subset the continous variables so that we can calculate correlation of all the pairs at the same time
loan_continous <- loan[,c(1,2,7,10)]

#-Determine the correlation
loan_continous_matrix <- cor(loan_continous)

#-------------------------------------------------------------------------------------------------------

##Let's see the correlation matrix for all the continous variables
library(GGally)

ggpairs(loan_continous)

##From the correlation matrix and the correlation values it is clear that  

# 1.) maximum correlation is observed between the funded amount-loan amount applied
# 2.) Low correlation is observed between dti and the other continuous variables.

#-------------------------------------------------------------------------------------------------------

### Distribution of all the driver variables across different levels of two categorical variables:

## loan_status_1: Make plots to show how the continuous variables vary across the three levels of loan_status_1; 
#for e.g. how annual_inc is distributed across the levels


## loan amount V/s loan_status_1 

ggplot(loan, aes(x = loan_amnt)) + geom_histogram(binwidth = 1000, fill = "black") + facet_wrap(~loan_status_1) +
  xlab(" Applied Loan Amount")+ylab("Frequency") +ggtitle("Distribution of loan amount across levels")
#(Distribution of loan applied are larger in default category), Now, let's check the amount distribution also(EXTRA INFO)

ggplot(loan,aes(x=loan_status_1,y= loan_amnt))+geom_bar(stat = "identity")+ggtitle(" Total loan amount across levels")

#-------------------------------------------------------------------------------------------------------

## funded loan V/s loan_status_1

ggplot(loan, aes(x = funded_amnt)) +geom_histogram(binwidth = 1000, fill = "black") + facet_wrap(~loan_status_1) +
  xlab(" Funded Amount")+ylab("Frequency") +ggtitle("Distribution of Funded amount accross levels")

ggplot(loan,aes(x=loan_status_1,y= funded_amnt))+geom_bar(stat = "identity")+ggtitle(" Total funded amount across levels")

#-------------------------------------------------------------------------------------------------------

## Annual Income v/s loan_status_1 

ggplot(loan, aes(x = annual_inc)) + geom_histogram( binwidth = 10000,fill = "Black") + facet_wrap(~loan_status_1) +
  xlab(" Annual Income")+ylab("Frequency") +ggtitle("Distribution of Annual income accross levels")

ggplot(loan,aes(x=loan_status_1,y= annual_inc))+geom_bar(stat = "identity")+ggtitle(" Total annual income across levels")

# Even you can also check the boxplot to compare the median of each levels among themselves(# EXTRA INFO).

boxplot(annual_inc~loan_status_1, data = loan, xlab = "loan status", ylab = "Annual Income")

# (Insights: You can see (Check the plot) that the Late category doesn't contain any outliers but in default category contains most of the outliers

#-------------------------------------------------------------------------------------------------------

## dti V/s loan_status_1

ggplot(loan, aes(x = dti)) +geom_histogram(binwidth = 5, fill = "black") +facet_wrap(~loan_status_1) +
  xlab(" DTI")+ylab("Frequency") +ggtitle("Distribution of DTI accross levels")

boxplot(dti~loan_status_1, data = loan, xlab = "Loan status", ylab = "DTI")
#(You can see that the median of late "loan" payer is larger than other two categories)

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

# 2. int_rate_grp: Make plots to show how the continuous variables vary across the three levels of 
# int_rate_grp (High, Low and Medium)

## Annual Income V/s int_rate_grp 

ggplot(loan, aes(x = annual_inc)) + geom_histogram(binwidth = 1000, fill = "Black") +facet_wrap(~int_rate_grp) + 
  xlab("Annual Income")+ylab("Frequency") +ggtitle("Distribution of Annual income accross levels")

ggplot(loan,aes(x=int_rate_grp,y= annual_inc))+geom_bar(stat = "identity")+ggtitle(" Total annual income across levels")

#-------------------------------------------------------------------------------------------------------

## loan amount V/s int_rate_grp 

ggplot(loan, aes(x = loan_amnt)) +geom_histogram(binwidth = 5000, fill = "black") +facet_wrap(~int_rate_grp) + 
  xlab(" Loan Amount")+ylab("Frequency") +ggtitle("Distribution of Loan amount accross each levels of int_rate_grp variable")


ggplot(loan,aes(x=int_rate_grp,y= loan_amnt))+geom_bar(stat = "identity")+ggtitle(" Total loan_amnt across levels")


#-------------------------------------------------------------------------------------------------------

## funded loan amount V/s int_rate_grp 

ggplot(loan, aes(x = funded_amnt)) + geom_histogram(binwidth = 4000, fill = "black") + facet_wrap(~int_rate_grp) + 
  xlab(" Funded Amount")+ylab("Frequency") +ggtitle("Distribution of Funded amount accross each levels of int_rate_grp variable")

ggplot(loan,aes(x=int_rate_grp,y= funded_amnt))+geom_bar(stat = "identity")+ggtitle(" Total funded_amnt across levels")


#-------------------------------------------------------------------------------------------------------

## DTI V/s int_rate_grp

ggplot(loan, aes(x = dti)) +geom_histogram(binwidth = 5, fill = "black") + facet_wrap(~int_rate_grp) + 
  xlab(" DTI")+ylab("Frequency") +ggtitle("Distribution of DTI accross each levels of int_rate_grp variable")

boxplot(dti~int_rate_grp, data = loan, xlab = "Int_rate_grp", ylab = "DTI")

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

## -------------------------------Checkpoint 3: Hypthesis Testing---------------------------------------

# Test hypotheses (95 % conf. level) for two levels of loan_status_1 - default_new and current_new


current_new <- loan[which(loan$loan_status_1=="current_new"),]

default_new <- loan[which(loan$loan_status_1=="default_new"),]

#-------------------------------------------------------------------------------------------------------

# First, let's test "loan amount" variable
t_test_loan_amount <- t.test(x = current_new$loan_amnt, y = default_new$loan_amnt, conf.level = 0.95, alternative = "two.sided")

# Next, "Funded amount" variable
t_test_funded_amnt <- t.test(x = current_new$funded_amnt, y =default_new$funded_amnt, conf.level = 0.95, alternative = "two.sided")

# Next, "Annual income" variable
t_test_annual_income <- t.test(x = current_new$annual_inc, y = default_new$annual_inc, conf.level = 0.95, alternative = "two.sided")

# Then, "DTI" variable
t_test_dti<- t.test(x = current_new$dti, y =default_new$dti, conf.level = 0.95, alternative = "two.sided")

#-------------------------------------------------------------------------------------------------------

##  Test hypotheses (95 % conf. level) for two levels of int_rate_grp - high and low

high_rate<- loan[which(loan$int_rate_grp=="High"),]

low_rate <- loan[which(loan$int_rate_grp=="Low"),]

#-------------------------------------------------------------------------------------------------------

# Same as above, let's test the hypotheses for high_rate ad low_rate level for all continuous variables 

# First, let's test "loan amount" variable
t_test_loan_amnt_2 <- t.test(x = current_new$loan_amnt, y = default_new$loan_amnt, conf.level = 0.95, alternative = "two.sided")

# Next, "Funded amount" variable
t_test_funded_amnt_2 <- t.test(x = high_rate$funded_amnt, y=low_rate$funded_amnt, conf.level = 0.95, alternative = "two.sided")

# Next, "Annual income" variable
t_test_annual_income_2 <- t.test(x = high_rate$annual_inc, y = low_rate$annual_inc, conf.level = 0.95, alternative = "two.sided")

# Then, "DTI" variable
t_test_dti_loan_2 <- t.test(x = high_rate$dti, y=low_rate$dti, conf.level = 0.95, alternative = "two.sided")

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
##########################################################################################################----The End

