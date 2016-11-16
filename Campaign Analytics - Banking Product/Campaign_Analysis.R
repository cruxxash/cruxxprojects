#importing dataset
main<-read.csv("Campaign Data1.csv",stringsAsFactors = F,sep = ",",header = T)

install.packages("caTools")
install.packages("car")
install.packages("e1071")
install.packages("MASS")
install.packages("Hmisc")
install.packages("ROCR")
install.packages("ggplot2")


library(ggplot2)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(Hmisc)
library(ROCR)

################################  OUTLIER AND FORMAT TREATMENT  #######################################################
sum(is.na(main))
#as there are 0 missing values, checking for duplicates
unique(main)
options(max.print = 4200000)
#all are unique

str(main)
colnames(main)<-c("Age","Job","Marital Status","Education","Default","Housing","Loan","Contact","Month","DayofWeek","Duration",
                  "Campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m",
                  "nr.employed","Response")
#------------- Age ----------------------#
main$age<-as.numeric(main$age)
quantile(main$age,seq(0,1,0.01))
boxplot.stats(main$age)$out

#Binning the age in a factor of 20
main$age[which(main$age<20)]<- "Below 20years"
main$age[which(main$age<40)]<-"Between 20-40years"
main$age[which(main$age<60)]<-"Between 40-60years"
main$age[which(main$age<98.1)]<-"Above 60years"
table(main$age)
main$age<-as.factor(main$age)

#------------- Job  ---------------------#
table(main$Job)
main$Job<-as.factor(main$Job)
levels(main$Job)<-c("admin." ,  "blue-collar" , "entrepreneur"    , "housemaid" ,   "management"      , "retired" ,
                    "self-employed" ,     "services" ,"student"  ,  "technician"  ,  "unemployed" ,      "unknown")

#------------ Marital Status-------------#
table(main$`Marital Status`)
main$`Marital Status`<-as.factor(main$`Marital Status`)
levels(main$`Marital Status`)<-c("divorced" , "married" ,  "single" , "unknown")

#--------------  Education  --------------#
main$Education<-as.factor(main$Education)
table(main$Education)
levels(main$Education)<-c("basic.4y"        ,    "basic.6y"     ,       "basic.9y"  ,       "high.school"     ,     "illiterate" ,
                          "professional.course","university.degree"    ,         "unknown")
#-----------------  Default  --------------#
main$Default<-as.factor(main$Default)
table(main$Default)
levels(main$Default)<-c("no" ,"unknown"     ,"yes")

#-----------------  Housing  -----------#
main$Housing<-as.factor(main$Housing)
table(main$Housing)
levels(main$Housing)<-c("no" ,"unknown" ,    "yes")

#----------------  Loan  ----------------#
main$Loan<-as.factor(main$Loan)
table(main$Loan)
levels(main$Loan)<-c("no", "unknown"  ,   "yes")

#--------------  Contact  ---------------#
main$Contact<-as.factor(main$Contact)
table(main$Contact)
levels(main$Contact)<-c("cellular" ,"telephone")
main$Contact<-as.numeric(main$Contact) #1 is cellular, 2 is telephone

#-------------  Month  ------------------#
main$Month<-as.factor(main$Month)
table(main$Month)
levels(main$Month)<-c("apr" ,"aug", "dec" ,"jul" ,"jun", "mar" ,"may", "nov" ,"oct" ,"sep")

#--------------  Day of Week  -------------#
main$DayofWeek<-as.factor(main$DayofWeek)
table(main$DayofWeek)
levels(main$DayofWeek)<-c("fri", "mon" ,"thu" ,"tue" ,"wed")

#-------------  Duration  ----------------#
main$Duration<-as.numeric(main$Duration)
boxplot.stats(main$Duration)$out
boxplot(main$Duration)
quantile(main$Duration,seq(0,1,0.01))

#Outlier Treatment
#As there is a sudden jump from 99 percentile to 100 percentile, using cap and floor method to change value to 1271.13
main$Duration[which(main$Duration>1271.13)]<-1271.13

#--------------  Campaign  ---------------#
table(main$Campaign)
main$Campaign<-as.numeric(main$Campaign)
quantile(main$Campaign,seq(0,1,0.01))
boxplot(main$Campaign)

#Outlier Treatment via cap and flooring to 99 percentile
main$Campaign[which(main$Campaign>14)]<-14

#--------------  Pdays  ------------------#
summary(main$pdays)
main$pdays<-as.numeric(main$pdays)
quantile(main$pdays,seq(0,1,0.01))
boxplot(main$pdays)
table(main$pdays)

#Binning Pdays
main$pdays[which(main$pdays<11)]<-"Within 10 days"
main$pdays[which(main$pdays<30)]<-"Within 1 month"
main$pdays[which(main$pdays==999)]<-"Not Contacted"

main$pdays<-as.factor(main$pdays)

#----------------  Previous  --------------#
quantile(main$previous,seq(0,1,0.01))
table(main$previous)

#Binning
main$previous[which(main$previous==0)]<-"No Previous Contact"
main$previous[which(main$previous==1)]<-"one Contact"
main$previous[which(main$previous==2)]<-"Two Contacts"
main$previous[which(main$previous==3)]<-"Three Contacts"
main$previous[which(main$previous==4)]<-"Four Contacts"
main$previous[which(main$previous==5)]<-"Five contacts"
main$previous[which(main$previous==6)]<-"Six Contacts"
main$previous[which(main$previous==7)]<-"Seven Contacts"

main$previous<-as.factor(main$previous)

#-----------------  Poutcome  --------------#
main$poutcome<-as.factor(main$poutcome)
table(main$poutcome)
levels(main$poutcome)<-c("failure", "nonexistent" ,    "success")

#----------------  Emp Var Rate  -------------#
quantile(main$emp.var.rate,seq(0,1,0.01))
main$emp.var.rate<-as.numeric(main$emp.var.rate)

#--------------- Cons Price Idx  -------------#
quantile(main$cons.price.idx,seq(0,1,0.01))

#---------------  Cons conf idx  --------------#
quantile(main$cons.conf.idx,seq(0,1,0.01))

#---------------  Euribor 3 month  ------------#
quantile(main$euribor3m,seq(0,1,0.01))

#---------------  NR Employed  -----------------#
quantile(main$nr.employed,seq(0,1,0.01))

#-----------------  Response  ------------------#
main$Response<-as.factor(main$Response)
table(main$Response)
main$Response<-as.numeric(main$Response) #1 is No, 2 is Yes

##################################  CREATING DUMMY VARIABLES  #######################################################

#Since all Outlier treatment and converting to appropriate format is done, proceeding to creation of dummy variables
dummy_age<-data.frame(model.matrix(~Age,main))[,-1]
dummy_job<-data.frame(model.matrix(~Job,main))[,-1]
dummy_marstat<-data.frame(model.matrix(~`Marital Status`,main))[,-1]
dummy_edu<-data.frame(model.matrix(~Education,main))[,-1]
dummy_default<-data.frame(model.matrix(~Default,main))[,-1]
dummy_house<-data.frame(model.matrix(~Housing,main))[,-1]
dummy_loan<-data.frame(model.matrix(~Loan,main))[,-1]
dummy_month<-data.frame(model.matrix(~Month,main))[,-1]
dummy_dayofweek<-data.frame(model.matrix(~DayofWeek,main))[,-1]
dummy_pdays<-data.frame(model.matrix(~pdays,main))[,-1]
dummy_prev<-data.frame(model.matrix(~previous,main))[,-1]
dummy_pout<-data.frame(model.matrix(~poutcome,main))[,-1]

#Combining the dataset with all dummy variables
main_final<-cbind(main[,-c(1,2,3,4,5,6,7,9,10,13,14,15)],dummy_age,dummy_job,dummy_marstat,dummy_edu,
                  dummy_default,dummy_house,dummy_loan,dummy_month,dummy_dayofweek,dummy_pdays,dummy_prev,dummy_pout)

#Scaling the Response variable
main_final$Response<-main_final$Response - 1
main_final$Contact<-main_final$Contact-1
main_final$Duration<-scale(main_final$Duration)
main_final$Campaign<-scale(main_final$Campaign)
main_final$emp.var.rate<-scale(main_final$emp.var.rate)
main_final$cons.price.idx<-scale(main_final$cons.price.idx)
main_final$cons.conf.idx<-scale(main_final$cons.conf.idx)
main_final$euribor3m<-scale(main_final$euribor3m)
main_final$nr.employed<-scale(main_final$nr.employed)

#Creating test and train datasets
set.seed(2)
sample<-sample.split(main_final$Response,SplitRatio = 0.7)

train<-main_final[sample,]
test<-main_final[!sample,]

#################################  LOGISTIC MODEL BUILDING  #################################################

#now we have to predict the Response of a customer based on the features provided
#building initial Logistic Regression model



model_1<-glm(Response~. , data = train,family = "binomial")

#using stepwise reduction of variables
step<-step(model_1,direction = "both",k=2)
summary(step)
vif(step,family="binomial")

#Removing NR Employed due to high VIF and low significance
model_2<-glm(formula = Response ~ Contact + Duration + Campaign + emp.var.rate + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + 
               Jobmanagement + Jobself.employed + Jobservices + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct + Monthsep + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + previousSeven.Contacts + 
               previousThree.Contacts + previousTwo.Contacts + poutcomesuccess, 
             family = "binomial", data = train)
summary(model_2)
vif(model_2,family="binomial")

#removing emp var rate due to extreme high vif
model_3<-glm(formula = Response ~ Contact + Duration + Campaign  + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + 
               Jobmanagement + Jobself.employed + Jobservices + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct + Monthsep + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + previousSeven.Contacts + 
               previousThree.Contacts + previousTwo.Contacts + poutcomesuccess, 
             family = "binomial", data = train)
summary(model_3)
vif(model_3,family="binomial")

#Removing poutcome Success due to high VIF
model_4<-glm(formula = Response ~ Contact + Duration + Campaign  + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + 
               Jobmanagement + Jobself.employed + Jobservices + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct + Monthsep + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + previousSeven.Contacts + 
               previousThree.Contacts + previousTwo.Contacts , 
             family = "binomial", data = train)
summary(model_4)
vif(model_4,family="binomial")

#Removing previous 7 contacts due to low significane
model_5<-glm(formula = Response ~ Contact + Duration + Campaign  + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + 
               Jobmanagement + Jobself.employed + Jobservices + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct + Monthsep + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + 
               previousThree.Contacts + previousTwo.Contacts , 
             family = "binomial", data = train)
summary(model_5)
vif(model_5,family="binomial")

#Removing September month due to insignificance
model_6<-glm(formula = Response ~ Contact + Duration + Campaign  + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + 
               Jobmanagement + Jobself.employed + Jobservices + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct  + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + 
               previousThree.Contacts + previousTwo.Contacts , 
             family = "binomial", data = train)
summary(model_6)
vif(model_6,family="binomial")

#Removing Job Self Employed due to insignificance
model_7<-glm(formula = Response ~ Contact + Duration + Campaign  + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + 
               Jobmanagement  + Jobservices + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct  + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + 
               previousThree.Contacts + previousTwo.Contacts , 
             family = "binomial", data = train)
summary(model_7)
vif(model_7,family="binomial")

#Removing Job Services
model_8<-glm(formula = Response ~ Contact + Duration + Campaign  + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + 
               Jobmanagement   + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct  + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + 
               previousThree.Contacts + previousTwo.Contacts , 
             family = "binomial", data = train)
summary(model_8)
vif(model_8,family="binomial")

#Removing Job Management
model_9<-glm(formula = Response ~ Contact + Duration + Campaign  + 
               cons.price.idx + cons.conf.idx + euribor3m + 
               AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + Jobstudent + 
               Educationprofessional.course + Educationuniversity.degree + 
               Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
               Monthmay + Monthnov + Monthoct  + DayofWeekmon + 
               DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
               previousFour.Contacts + previousone.Contact + 
               previousThree.Contacts + previousTwo.Contacts , 
             family = "binomial", data = train)
summary(model_9)
vif(model_9,family="binomial")

#Removing November Month
model_10<-glm(formula = Response ~ Contact + Duration + Campaign  + 
                cons.price.idx + cons.conf.idx + euribor3m + 
                AgeBetween.20.40years + AgeBetween.40.60years + Jobblue.collar + Jobstudent + 
                Educationprofessional.course + Educationuniversity.degree + 
                Defaultunknown + Monthaug + Monthjul + Monthjun + Monthmar + 
                Monthmay + Monthoct  + DayofWeekmon + 
                DayofWeekwed + pdaysWithin.1.month + pdaysWithin.10.days + 
                previousFour.Contacts + previousone.Contact + 
                previousThree.Contacts + previousTwo.Contacts , 
              family = "binomial", data = train)
summary(model_10)
vif(model_10,family="binomial")

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.4723  -0.3003  -0.1792  -0.1198   3.1613  
# 
# Coefficients:
#                                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                  -2.54144    0.11471 -22.154  < 2e-16 ***
#   Contact                      -0.30536    0.08244  -3.704 0.000212 *** 
#   Duration                      1.21749    0.02161  56.338  < 2e-16 ***
#   Campaign                     -0.12002    0.03389  -3.542 0.000398 ***
#   cons.price.idx                0.23978    0.03237   7.408 1.28e-13 ***
#   cons.conf.idx                 0.19549    0.02509   7.792 6.60e-15 ***
#   euribor3m                    -1.21621    0.03800 -32.004  < 2e-16 ***
#   AgeBetween.20.40years        -0.37161    0.09882  -3.761 0.000169 ***
#   AgeBetween.40.60years        -0.53550    0.10210  -5.245 1.56e-07 ***
#   Jobblue.collar               -0.22248    0.07444  -2.989 0.002803 ** 
#   Jobstudent                    0.39838    0.12016   3.316 0.000915 ***
#   Educationprofessional.course  0.15951    0.07738   2.061 0.039271 *  
#   Educationuniversity.degree    0.20205    0.05965   3.387 0.000706 ***
#   Defaultunknown               -0.34936    0.08117  -4.304 1.68e-05 ***
#   Monthaug                      0.25085    0.09056   2.770 0.005604 ** 
#   Monthjul                      0.29066    0.09067   3.206 0.001347 ** 
#   Monthjun                      0.35353    0.09235   3.828 0.000129 ***
#   Monthmar                      1.44554    0.13105  11.030  < 2e-16 ***
#   Monthmay                     -0.75933    0.07431 -10.218  < 2e-16 ***
#   Monthoct                      0.44869    0.12570   3.570 0.000357 ***
#   DayofWeekmon                 -0.16731    0.06362  -2.630 0.008539 ** 
#   DayofWeekwed                  0.14013    0.06293   2.227 0.025968 *  
#   pdaysWithin.1.month           1.23064    0.22511   5.467 4.58e-08 ***
#   pdaysWithin.10.days           2.05745    0.10899  18.877  < 2e-16 ***
#   previousFour.Contacts        -1.34226    0.33851  -3.965 7.33e-05 ***
#   previousone.Contact          -0.57881    0.08009  -7.227 4.95e-13 ***
#   previousThree.Contacts       -0.73648    0.21710  -3.392 0.000693 ***
#   previousTwo.Contacts         -0.56040    0.13802  -4.060 4.90e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 20299  on 28831  degrees of freedom
# Residual deviance: 11739  on 28804  degrees of freedom
# AIC: 11795

plot(model_10)

##################################  MODEL EVALUATION  #####################################################

#C-Stats
pred_train<-predict(model_10,data=train,type = "response")
rcorr.cens(pred_train,train$Response)
#C Index = 9.331773e-01

#KS Stat and ROC curve
pred<-predict(model_10,newdata = test)
roc_pred<-prediction(pred,test$Response)
model_perf<-performance(roc_pred,"tpr","fpr")
plot(model_perf)

threshold = 0.4

#KS Stat
ks_table<-attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)
ks #0.75
which.max(ks_table) #3150

# Checkpoint 6: Threshold Value and confusionMatrix
pred_cf = predict(model_10, newdata = test[,-9], type = "response")
table(pred_cf > threshold, test$Response)

# trying different thresholds
# Accuracy, specificity and sensitivity can be calculated from the table

confusionMatrix(as.numeric(pred>0),test$Response)

#As we want to ensure that we do not spend on marketing on those customers who would have a high probability of saying no,
#we make sure we have a minimum threshold for the above confusion matrix, so we have max sensitivity and also a good amount of specificity
#Accuracy  = 90.94%
#Sensitivity = 97.15%
#Specificity = 41.95%

write.csv(main,file = "completed.csv")
