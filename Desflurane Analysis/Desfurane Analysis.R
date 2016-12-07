setwd("C:/DA/Jayanth")

install.packages("readxl")
install.packages("ggplot2")
library(ggplot2)
library(readxl)

################################### HEART RATE  ################################################
heart_rate<-read_excel("combined.xlsx",sheet = 1)
sum(is.na(heart_rate))
str(heart_rate)

#Subsetting the two study groups
heart_l<-subset(heart_rate,heart_rate$`STUDY GROUP`=="L")
heart_m<-subset(heart_rate,heart_rate$`STUDY GROUP`=="M")

ggplot(heart_rate,aes(heart_rate$Type,heart_rate$Rate,fill=heart_rate$Type))+geom_point()

#===============================  PREOP  ===============================#

mean(heart_l$Rate[which(heart_l$Type=="PRE OP")],na.rm = T) #82.03
mean(heart_m$Rate[which(heart_m$Type=="PRE OP")],na.rm = T) #82.06

sd(heart_l$Rate[which(heart_l$Type=="PRE OP")],na.rm = T) #7.88
sd(heart_m$Rate[which(heart_m$Type=="PRE OP")],na.rm = T) #10.03

median(heart_l$Rate[which(heart_l$Type=="PRE OP")],na.rm = T) #82
median(heart_m$Rate[which(heart_m$Type=="PRE OP")],na.rm = T) #84

# Intergroup
t.test(heart_l$Rate[which(heart_l$Type=="PRE OP")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t-value = -0.014313
#p-value = 0.9886 , hence insignificant difference

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="PRE OP")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
# t = 0, df = 58, p-value = 1
t.test(heart_m$Rate[which(heart_m$Type=="PRE OP")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
# t = 0, df = 58, p-value = 1

#===============================  INDUCTION  ===========================#

mean(heart_l$Rate[which(heart_l$Type=="INDUCTION")],na.rm = T) #81.83
mean(heart_m$Rate[which(heart_m$Type=="INDUCTION")],na.rm = T) #81.06

sd(heart_l$Rate[which(heart_l$Type=="INDUCTION")],na.rm = T) #11.19
sd(heart_m$Rate[which(heart_m$Type=="INDUCTION")],na.rm = T) #12.16

median(heart_l$Rate[which(heart_l$Type=="INDUCTION")],na.rm = T) #82
median(heart_m$Rate[which(heart_m$Type=="INDUCTION")],na.rm = T) #82

t.test(heart_l$Rate[which(heart_l$Type=="INDUCTION")],heart_m$Rate[which(heart_m$Type=="INDUCTION")],conf.level = 0.95)
#t-value = 0.25403
#p-value = 0.8004 , hence insignificant difference

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="INDUCTION")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.080017, df = 52.099, p-value = 0.9365
t.test(heart_m$Rate[which(heart_m$Type=="INDUCTION")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
# t = -0.34742, df = 55.961, p-value = 0.7296

#=========================  INTUBATION  ==============================#

mean(heart_l$Rate[which(heart_l$Type=="INTUBATION")],na.rm = T) #95.13
mean(heart_m$Rate[which(heart_m$Type=="INTUBATION")],na.rm = T) #93.06

sd(heart_l$Rate[which(heart_l$Type=="INTUBATION")],na.rm = T) #11.47
sd(heart_m$Rate[which(heart_m$Type=="INTUBATION")],na.rm = T) #10.65

median(heart_l$Rate[which(heart_l$Type=="INTUBATION")],na.rm = T) #94.5
median(heart_m$Rate[which(heart_m$Type=="INTUBATION")],na.rm = T) #95

t.test(heart_l$Rate[which(heart_l$Type=="INTUBATION")],heart_m$Rate[which(heart_m$Type=="INTUBATION")],conf.level = 0.95)
#t-value = 0.72286
#p-value = 0.4727 , hence insignificant difference

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="INTUBATION")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.1531, df = 51.39, p-value = 4.123e-06
t.test(heart_m$Rate[which(heart_m$Type=="INTUBATION")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 4.118, df = 57.788, p-value = 0.0001231
#=================    5 mins ==============================#

mean(heart_l$Rate[which(heart_l$Type=="5 MINS")],na.rm = T) #90.06
mean(heart_m$Rate[which(heart_m$Type=="5 MINS")],na.rm = T) #85.96

sd(heart_l$Rate[which(heart_l$Type=="5 MINS")],na.rm = T) #9.59
sd(heart_m$Rate[which(heart_m$Type=="5 MINS")],na.rm = T) #13.49

median(heart_l$Rate[which(heart_l$Type=="5 MINS")],na.rm = T) #91
median(heart_m$Rate[which(heart_m$Type=="5 MINS")],na.rm = T) #89

t.test(heart_l$Rate[which(heart_l$Type=="5 MINS")],heart_m$Rate[which(heart_m$Type=="5 MINS")],conf.level = 0.95)


# t = 1.3561, df = 52.348, p-value = 0.1809

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="5 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.5429, df = 55.9, p-value = 0.0008071
t.test(heart_m$Rate[which(heart_m$Type=="5 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.2704, df = 53.537, p-value = 0.2094
#============================  10 MINS  ==================================#

mean(heart_l$Rate[which(heart_l$Type=="10 MINS")],na.rm = T) #81.93
mean(heart_m$Rate[which(heart_m$Type=="10 MINS")],na.rm = T) #79.233

sd(heart_l$Rate[which(heart_l$Type=="10 MINS")],na.rm = T) #9.08
sd(heart_m$Rate[which(heart_m$Type=="10 MINS")],na.rm = T) #12.10

median(heart_l$Rate[which(heart_l$Type=="10 MINS")],na.rm = T) #83
median(heart_m$Rate[which(heart_m$Type=="10 MINS")],na.rm = T) #81

t.test(heart_l$Rate[which(heart_l$Type=="10 MINS")],heart_m$Rate[which(heart_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.97679, df = 53.807, p-value = 0.333

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="10 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.045519, df = 56.866, p-value = 0.9639
t.test(heart_m$Rate[which(heart_m$Type=="10 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.98715, df = 56.054, p-value = 0.3278

#=============================  15 MINS  ==================================#

mean(heart_l$Rate[which(heart_l$Type=="15 MINS")],na.rm = T) #78.7
mean(heart_m$Rate[which(heart_m$Type=="15 MINS")],na.rm = T) #75.73

sd(heart_l$Rate[which(heart_l$Type=="15 MINS")],na.rm = T) #11.24
sd(heart_m$Rate[which(heart_m$Type=="15 MINS")],na.rm = T) #12.60

median(heart_l$Rate[which(heart_l$Type=="15 MINS")],na.rm = T) #77.5
median(heart_m$Rate[which(heart_m$Type=="15 MINS")],na.rm = T) #75.5

t.test(heart_l$Rate[which(heart_l$Type=="15 MINS")],heart_m$Rate[which(heart_m$Type=="15 MINS")],conf.level = 0.95)
#t = 0.9621, df = 57.261, p-value = 0.34 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="15 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.3295, df = 51.968, p-value = 0.1895,  not significant
t.test(heart_m$Rate[which(heart_m$Type=="15 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.1539, df = 55.213, p-value = 0.03563, significant

#==========================  30 MINS  ======================================#

mean(heart_l$Rate[which(heart_l$Type=="30 MINS")],na.rm = T) #78.7
mean(heart_m$Rate[which(heart_m$Type=="30 MINS")],na.rm = T) #75.73

sd(heart_l$Rate[which(heart_l$Type=="30 MINS")],na.rm = T) #11.24
sd(heart_m$Rate[which(heart_m$Type=="30 MINS")],na.rm = T) #12.60

median(heart_l$Rate[which(heart_l$Type=="30 MINS")],na.rm = T) #77.5
median(heart_m$Rate[which(heart_m$Type=="30 MINS")],na.rm = T) #75.5

t.test(heart_l$Rate[which(heart_l$Type=="30 MINS")],heart_m$Rate[which(heart_m$Type=="30 MINS")],conf.level = 0.95)
#t = -0.11988, df = 57.987, p-value = 0.905 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="30 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.67, df = 53.35, p-value = 0.0005614,   significant
t.test(heart_m$Rate[which(heart_m$Type=="30 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.1884, df = 57.644, p-value = 0.002313, significant

#=============================  45 MINS  ===================================#

mean(heart_l$Rate[which(heart_l$Type=="45 MINS")],na.rm = T) #72.63
mean(heart_m$Rate[which(heart_m$Type=="45 MINS")],na.rm = T) #71.63

sd(heart_l$Rate[which(heart_l$Type=="45 MINS")],na.rm = T) #11.29
sd(heart_m$Rate[which(heart_m$Type=="45 MINS")],na.rm = T) #11.66

median(heart_l$Rate[which(heart_l$Type=="45 MINS")],na.rm = T) #73
median(heart_m$Rate[which(heart_m$Type=="45 MINS")],na.rm = T) #73.5

t.test(heart_l$Rate[which(heart_l$Type=="45 MINS")],heart_m$Rate[which(heart_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0.32318, df = 57.844, p-value = 0.7477 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="45 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.5258, df = 49.412, p-value = 0.0009216,   significant
t.test(heart_m$Rate[which(heart_m$Type=="45 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.7142, df = 56.716, p-value = 0.0004673, significant

#===============================  60 MINS  ================================#

mean(heart_l$Rate[which(heart_l$Type=="60 MINS")],na.rm = T) #71.3
mean(heart_m$Rate[which(heart_m$Type=="60 MINS")],na.rm = T) #72.63

sd(heart_l$Rate[which(heart_l$Type=="60 MINS")],na.rm = T) #10.01
sd(heart_m$Rate[which(heart_m$Type=="60 MINS")],na.rm = T) #10.18

median(heart_l$Rate[which(heart_l$Type=="60 MINS")],na.rm = T) #72
median(heart_m$Rate[which(heart_m$Type=="60 MINS")],na.rm = T) #72.5

t.test(heart_l$Rate[which(heart_l$Type=="60 MINS")],heart_m$Rate[which(heart_m$Type=="60 MINS")],conf.level = 0.95)
#t = -0.51108, df = 57.983, p-value = 0.6112 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="60 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.6114, df = 54.966, p-value = 2.43e-05,   significant
t.test(heart_m$Rate[which(heart_m$Type=="60 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.6142, df = 57.985, p-value = 0.0006314, significant

#=============================  75 MINS ===================================#

mean(heart_l$Rate[which(heart_l$Type=="75 MINS")],na.rm = T) #76.03
mean(heart_m$Rate[which(heart_m$Type=="75 MINS")],na.rm = T) #75.06

sd(heart_l$Rate[which(heart_l$Type=="75 MINS")],na.rm = T) #11.59
sd(heart_m$Rate[which(heart_m$Type=="75 MINS")],na.rm = T) #11.81

median(heart_l$Rate[which(heart_l$Type=="75 MINS")],na.rm = T) #74
median(heart_m$Rate[which(heart_m$Type=="75 MINS")],na.rm = T) #77

t.test(heart_l$Rate[which(heart_l$Type=="75 MINS")],heart_m$Rate[which(heart_m$Type=="75 MINS")],conf.level = 0.95)
#t = 0.31984, df = 57.979, p-value = 0.7502 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="75 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.3441, df = 51.103, p-value = 0.023,   significant
t.test(heart_m$Rate[which(heart_m$Type=="75 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.4739, df = 56.503, p-value = 0.0164, significant

#============================== 90 MINS  ==================================#

mean(heart_l$Rate[which(heart_l$Type=="90 MINS")],na.rm = T) #73.93
mean(heart_m$Rate[which(heart_m$Type=="90 MINS")],na.rm = T) #74.13

sd(heart_l$Rate[which(heart_l$Type=="90 MINS")],na.rm = T) #9.92
sd(heart_m$Rate[which(heart_m$Type=="90 MINS")],na.rm = T) #10.74

median(heart_l$Rate[which(heart_l$Type=="90 MINS")],na.rm = T) #73.5
median(heart_m$Rate[which(heart_m$Type=="90 MINS")],na.rm = T) #74

t.test(heart_l$Rate[which(heart_l$Type=="90 MINS")],heart_m$Rate[which(heart_m$Type=="90 MINS")],conf.level = 0.95)
#t = -0.074887, df = 57.641, p-value = 0.9406 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="75 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.3441, df = 51.103, p-value = 0.023,   significant
t.test(heart_m$Rate[which(heart_m$Type=="75 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.4739, df = 56.503, p-value = 0.0164, significant

#===================================  105 MINS  ==================================#

mean(heart_l$Rate[which(heart_l$Type=="105 MINS")],na.rm = T) #70.9
mean(heart_m$Rate[which(heart_m$Type=="105 MINS")],na.rm = T) #72.13

sd(heart_l$Rate[which(heart_l$Type=="105 MINS")],na.rm = T) #8.47
sd(heart_m$Rate[which(heart_m$Type=="105 MINS")],na.rm = T) #8.12

median(heart_l$Rate[which(heart_l$Type=="105 MINS")],na.rm = T) #74.5
median(heart_m$Rate[which(heart_m$Type=="105 MINS")],na.rm = T) #75

t.test(heart_l$Rate[which(heart_l$Type=="105 MINS")],heart_m$Rate[which(heart_m$Type=="105 MINS")],conf.level = 0.95)
#t = -0.57546, df = 57.901, p-value = 0.5672 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="105 MINS")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.2694, df = 57.705, p-value = 2.124e-06,   significant
t.test(heart_m$Rate[which(heart_m$Type=="105 MINS")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.2152, df = 55.615, p-value = 9.25e-05, significant

#=====================================  120 MINS  ===============================#

mean(heart_l$Rate[which(heart_l$Type=="120 MIN")],na.rm = T) #72.24
mean(heart_m$Rate[which(heart_m$Type=="120 MIN")],na.rm = T) #72.83

sd(heart_l$Rate[which(heart_l$Type=="120 MIN")],na.rm = T) #8.29
sd(heart_m$Rate[which(heart_m$Type=="120 MIN")],na.rm = T) #7.87

median(heart_l$Rate[which(heart_l$Type=="120 MIN")],na.rm = T) #73
median(heart_m$Rate[which(heart_m$Type=="120 MIN")],na.rm = T) #74

t.test(heart_l$Rate[which(heart_l$Type=="120 MIN")],heart_m$Rate[which(heart_m$Type=="120 MIN")],conf.level = 0.95)
#t = -0.28092, df = 56.586, p-value = 0.7798 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="120 MIN")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.6453, df = 56.593, p-value = 2.07e-05,   significant
t.test(heart_m$Rate[which(heart_m$Type=="120 MIN")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.9658, df = 54.926, p-value = 0.0002136, significant

#====================================  135 MIN  =======================================#

mean(heart_l$Rate[which(heart_l$Type=="135 MIN")],na.rm = T) #73.93
mean(heart_m$Rate[which(heart_m$Type=="135 MIN")],na.rm = T) #71.86

sd(heart_l$Rate[which(heart_l$Type=="135 MIN")],na.rm = T) #7.86
sd(heart_m$Rate[which(heart_m$Type=="135 MIN")],na.rm = T) #14.43

median(heart_l$Rate[which(heart_l$Type=="135 MIN")],na.rm = T) #75
median(heart_m$Rate[which(heart_m$Type=="135 MIN")],na.rm = T) #74

t.test(heart_l$Rate[which(heart_l$Type=="135 MIN")],heart_m$Rate[which(heart_m$Type=="135 MIN")],conf.level = 0.95)
#t = 0.68512, df = 45.116, p-value = 0.4968 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="135 MIN")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.9522, df = 56.944, p-value = 0.0002162,   significant
t.test(heart_m$Rate[which(heart_m$Type=="135 MIN")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.1781, df = 51.695, p-value = 0.002503, significant

#======================================  150 MIN  ====================================#

mean(heart_l$Rate[which(heart_l$Type=="150 MIN")],na.rm = T) #74
mean(heart_m$Rate[which(heart_m$Type=="150 MIN")],na.rm = T) #74.8

sd(heart_l$Rate[which(heart_l$Type=="150 MIN")],na.rm = T) #9.32
sd(heart_m$Rate[which(heart_m$Type=="150 MIN")],na.rm = T) #8.91

median(heart_l$Rate[which(heart_l$Type=="150 MIN")],na.rm = T) #72
median(heart_m$Rate[which(heart_m$Type=="150 MIN")],na.rm = T) #72

t.test(heart_l$Rate[which(heart_l$Type=="150 MIN")],heart_m$Rate[which(heart_m$Type=="150 MIN")],conf.level = 0.95)
#t = -0.33652, df = 56.644, p-value = 0.7377 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="150 MIN")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.5669, df = 54.798, p-value = 0.0007591,   significant
t.test(heart_m$Rate[which(heart_m$Type=="150 MIN")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.9658, df = 57.222, p-value = 0.004397, significant

#=======================================  165 MIN  ==================================#

mean(heart_l$Rate[which(heart_l$Type=="165 MIN")],na.rm = T) #74.72
mean(heart_m$Rate[which(heart_m$Type=="165 MIN")],na.rm = T) #76.5

sd(heart_l$Rate[which(heart_l$Type=="165 MIN")],na.rm = T) #11.90
sd(heart_m$Rate[which(heart_m$Type=="165 MIN")],na.rm = T) #8.97

median(heart_l$Rate[which(heart_l$Type=="165 MIN")],na.rm = T) #79
median(heart_m$Rate[which(heart_m$Type=="165 MIN")],na.rm = T) #77

t.test(heart_l$Rate[which(heart_l$Type=="165 MIN")],heart_m$Rate[which(heart_m$Type=="165 MIN")],conf.level = 0.95)
#t = -0.6453, df = 52.042, p-value = 0.5216 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="165 MIN")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.7703, df = 48.376, p-value = 0.007922,   significant
t.test(heart_m$Rate[which(heart_m$Type=="165 MIN")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.2658, df = 57.3, p-value = 0.02726, significant

#=========================================  180 MIN  ================================#

mean(heart_l$Rate[which(heart_l$Type=="180 MIN")],na.rm = T) #76.89
mean(heart_m$Rate[which(heart_m$Type=="180 MIN")],na.rm = T) #77.65

sd(heart_l$Rate[which(heart_l$Type=="180 MIN")],na.rm = T) #10.81
sd(heart_m$Rate[which(heart_m$Type=="180 MIN")],na.rm = T) #8.27

median(heart_l$Rate[which(heart_l$Type=="180 MIN")],na.rm = T) #78.5
median(heart_m$Rate[which(heart_m$Type=="180 MIN")],na.rm = T) #80

t.test(heart_l$Rate[which(heart_l$Type=="180 MIN")],heart_m$Rate[which(heart_m$Type=="180 MIN")],conf.level = 0.95)
#t = -0.29813, df = 50.584, p-value = 0.7668 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="180 MIN")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.0565, df = 49.174, p-value = 0.04506,   significant
t.test(heart_m$Rate[which(heart_m$Type=="180 MIN")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.8455, df = 55.654, p-value = 0.07029, significant

#========================================  0 MIN_Emergence  ==========================#

mean(heart_l$Rate[which(heart_l$Type=="0 MIN_Emergence")],na.rm = T) #83.23
mean(heart_m$Rate[which(heart_m$Type=="0 MIN_Emergence")],na.rm = T) #82.2

sd(heart_l$Rate[which(heart_l$Type=="0 MIN_Emergence")],na.rm = T) #12.18
sd(heart_m$Rate[which(heart_m$Type=="0 MIN_Emergence")],na.rm = T) #9.18

median(heart_l$Rate[which(heart_l$Type=="0 MIN_Emergence")],na.rm = T) #81.5
median(heart_m$Rate[which(heart_m$Type=="0 MIN_Emergence")],na.rm = T) #81.5

t.test(heart_l$Rate[which(heart_l$Type=="0 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 0.37103, df = 53.911, p-value = 0.7121 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="0 MIN_Emergence")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0.45295, df = 49.672, p-value = 0.6526,   significant
t.test(heart_m$Rate[which(heart_m$Type=="0 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.053714, df = 57.556, p-value = 0.9573, significant

#========================================  1 MIN_Emergence  ============================#

mean(heart_l$Rate[which(heart_l$Type=="1 MIN_Emergence")],na.rm = T) #84.8
mean(heart_m$Rate[which(heart_m$Type=="1 MIN_Emergence")],na.rm = T) #83.23

sd(heart_l$Rate[which(heart_l$Type=="1 MIN_Emergence")],na.rm = T) #11.53
sd(heart_m$Rate[which(heart_m$Type=="1 MIN_Emergence")],na.rm = T) #8.91

median(heart_l$Rate[which(heart_l$Type=="1 MIN_Emergence")],na.rm = T) #82.5
median(heart_m$Rate[which(heart_m$Type=="1 MIN_Emergence")],na.rm = T) #80

t.test(heart_l$Rate[which(heart_l$Type=="1 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 0.58876, df = 54.532, p-value = 0.5585 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="1 MIN_Emergence")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.0847, df = 51.251, p-value = 0.2831, significant
t.test(heart_m$Rate[which(heart_m$Type=="1 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.47632, df = 57.212, p-value = 0.6357, significant

#=========================================  2 MIN_Emergence  ===============================#

mean(heart_l$Rate[which(heart_l$Type=="2 MIN_Emergence")],na.rm = T) #88.16
mean(heart_m$Rate[which(heart_m$Type=="2 MIN_Emergence")],na.rm = T) #87.4

sd(heart_l$Rate[which(heart_l$Type=="2 MIN_Emergence")],na.rm = T) #13.14
sd(heart_m$Rate[which(heart_m$Type=="2 MIN_Emergence")],na.rm = T) #9.46

median(heart_l$Rate[which(heart_l$Type=="2 MIN_Emergence")],na.rm = T) #88
median(heart_m$Rate[which(heart_m$Type=="2 MIN_Emergence")],na.rm = T) #84

t.test(heart_l$Rate[which(heart_l$Type=="2 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 0.2592, df = 52.687, p-value = 0.7965 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="2 MIN_Emergence")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.1911, df = 47.468, p-value = 0.03339, significant
t.test(heart_m$Rate[which(heart_m$Type=="2 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 2.1187, df = 57.807, p-value = 0.03843, significant

#==========================================  3 MIN_Emergence  ================================#

mean(heart_l$Rate[which(heart_l$Type=="3 MIN_Emergence")],na.rm = T) #90.06
mean(heart_m$Rate[which(heart_m$Type=="3 MIN_Emergence")],na.rm = T) #88.9

sd(heart_l$Rate[which(heart_l$Type=="3 MIN_Emergence")],na.rm = T) #14.8
sd(heart_m$Rate[which(heart_m$Type=="3 MIN_Emergence")],na.rm = T) #9.86

median(heart_l$Rate[which(heart_l$Type=="3 MIN_Emergence")],na.rm = T) #90
median(heart_m$Rate[which(heart_m$Type=="3 MIN_Emergence")],na.rm = T) #84.5

t.test(heart_l$Rate[which(heart_l$Type=="3 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 0.35926, df = 50.509, p-value = 0.7209 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="3 MIN_Emergence")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.6236, df = 44.232, p-value = 0.0119, significant
t.test(heart_m$Rate[which(heart_m$Type=="3 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 2.6611, df = 57.984, p-value = 0.01006, significant

#===========================================  4 MIN_Emergence  =================================#

mean(heart_l$Rate[which(heart_l$Type=="4 MIN_Emergence")],na.rm = T) #95.06
mean(heart_m$Rate[which(heart_m$Type=="4 MIN_Emergence")],na.rm = T) #92.9

sd(heart_l$Rate[which(heart_l$Type=="4 MIN_Emergence")],na.rm = T) #10.12
sd(heart_m$Rate[which(heart_m$Type=="4 MIN_Emergence")],na.rm = T) #8.07

median(heart_l$Rate[which(heart_l$Type=="4 MIN_Emergence")],na.rm = T) #94.5
median(heart_m$Rate[which(heart_m$Type=="4 MIN_Emergence")],na.rm = T) #90

t.test(heart_l$Rate[which(heart_l$Type=="4 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 0.91628, df = 55.279, p-value = 0.3635 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="4 MIN_Emergence")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.5633, df = 54.721, p-value = 8.178e-07, significant
t.test(heart_m$Rate[which(heart_m$Type=="4 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 4.608, df = 55.488, p-value = 2.425e-05, significant

#============================================  5 MIN_Emergence  ===================================#

mean(heart_l$Rate[which(heart_l$Type=="5 MIN_Emergence")],na.rm = T) #93.63
mean(heart_m$Rate[which(heart_m$Type=="5 MIN_Emergence")],na.rm = T) #91.36

sd(heart_l$Rate[which(heart_l$Type=="5 MIN_Emergence")],na.rm = T) #8.03
sd(heart_m$Rate[which(heart_m$Type=="5 MIN_Emergence")],na.rm = T) #6.58

median(heart_l$Rate[which(heart_l$Type=="5 MIN_Emergence")],na.rm = T) #92.5
median(heart_m$Rate[which(heart_m$Type=="5 MIN_Emergence")],na.rm = T) #90.5

t.test(heart_l$Rate[which(heart_l$Type=="5 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = 1.195, df = 55.837, p-value = 0.2371 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="5 MIN_Emergence")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.6433, df = 57.979, p-value = 5.242e-07, significant
t.test(heart_m$Rate[which(heart_m$Type=="5 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 4.2465, df = 50.084, p-value = 9.405e-05, significant

#=============================================  6 MIN_Emergence  =====================================#

mean(heart_l$Rate[which(heart_l$Type=="6 MIN_Emergence")],na.rm = T) #96.5
mean(heart_m$Rate[which(heart_m$Type=="6 MIN_Emergence")],na.rm = T) #93.57

sd(heart_l$Rate[which(heart_l$Type=="6 MIN_Emergence")],na.rm = T) #6.34
sd(heart_m$Rate[which(heart_m$Type=="6 MIN_Emergence")],na.rm = T) #6.32

median(heart_l$Rate[which(heart_l$Type=="6 MIN_Emergence")],na.rm = T) #97
median(heart_m$Rate[which(heart_m$Type=="6 MIN_Emergence")],na.rm = T) #96

t.test(heart_l$Rate[which(heart_l$Type=="6 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="6 MIN_Emergence")],conf.level = 0.95)
#t = 1.2234, df = 26, p-value = 0.2322 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="6 MIN_Emergence")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 6.5031, df = 31.217, p-value = 2.862e-07, significant
t.test(heart_m$Rate[which(heart_m$Type=="6 MIN_Emergence")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 4.6183, df = 37.981, p-value = 4.344e-05, significant

#============================================  0 MIN_Recovery  ========================================#

mean(heart_l$Rate[which(heart_l$Type=="0 MIN_Recovery")],na.rm = T) #97.2
mean(heart_m$Rate[which(heart_m$Type=="0 MIN_Recovery")],na.rm = T) #94.06

sd(heart_l$Rate[which(heart_l$Type=="0 MIN_Recovery")],na.rm = T) #8.41
sd(heart_m$Rate[which(heart_m$Type=="0 MIN_Recovery")],na.rm = T) #8.90

median(heart_l$Rate[which(heart_l$Type=="0 MIN_Recovery")],na.rm = T) #99
median(heart_m$Rate[which(heart_m$Type=="0 MIN_Recovery")],na.rm = T) #96

t.test(heart_l$Rate[which(heart_l$Type=="0 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="0 MIN_Recovery")],conf.level = 0.95)
#t = 1.4005, df = 57.817, p-value = 0.1667 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="0 MIN_Recovery")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 7.2025, df = 57.754, p-value = 1.368e-09, significant
t.test(heart_m$Rate[which(heart_m$Type=="0 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 4.901, df = 57.202, p-value = 8.223e-06, significant

#============================================  15 MIN_Recovery  =========================================#

mean(heart_l$Rate[which(heart_l$Type=="15 MIN_Recovery")],na.rm = T) #86.6
mean(heart_m$Rate[which(heart_m$Type=="15 MIN_Recovery")],na.rm = T) #83.1

sd(heart_l$Rate[which(heart_l$Type=="15 MIN_Recovery")],na.rm = T) #7.98
sd(heart_m$Rate[which(heart_m$Type=="15 MIN_Recovery")],na.rm = T) #8.56

median(heart_l$Rate[which(heart_l$Type=="15 MIN_Recovery")],na.rm = T) #90
median(heart_m$Rate[which(heart_m$Type=="15 MIN_Recovery")],na.rm = T) #82

t.test(heart_l$Rate[which(heart_l$Type=="15 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="15 MIN_Recovery")],conf.level = 0.95)
#t = 1.6364, df = 57.719, p-value = 0.1072 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="15 MIN_Recovery")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.2283, df = 57.99, p-value = 0.02976, significant
t.test(heart_m$Rate[which(heart_m$Type=="15 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.42913, df = 56.621, p-value = 0.6695, significant

#============================================  30 MIN_Recovery  ========================================#

mean(heart_l$Rate[which(heart_l$Type=="30 MIN_Recovery")],na.rm = T) #82.43
mean(heart_m$Rate[which(heart_m$Type=="30 MIN_Recovery")],na.rm = T) #81.1

sd(heart_l$Rate[which(heart_l$Type=="30 MIN_Recovery")],na.rm = T) #6.19
sd(heart_m$Rate[which(heart_m$Type=="30 MIN_Recovery")],na.rm = T) #6.04

median(heart_l$Rate[which(heart_l$Type=="30 MIN_Recovery")],na.rm = T) #82
median(heart_m$Rate[which(heart_m$Type=="30 MIN_Recovery")],na.rm = T) #80

t.test(heart_l$Rate[which(heart_l$Type=="30 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="30 MIN_Recovery")],conf.level = 0.95)
#t = 0.84384, df = 57.968, p-value = 0.4022 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="30 MIN_Recovery")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0.21855, df = 54.908, p-value = 0.8278, significant
t.test(heart_m$Rate[which(heart_m$Type=="30 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.45215, df = 47.633, p-value = 0.6532, significant

#==============================================  45 MIN_Recovery  ======================================#

mean(heart_l$Rate[which(heart_l$Type=="45 MIN_Recovery")],na.rm = T) #79.26
mean(heart_m$Rate[which(heart_m$Type=="45 MIN_Recovery")],na.rm = T) #78.83

sd(heart_l$Rate[which(heart_l$Type=="45 MIN_Recovery")],na.rm = T) #7.05
sd(heart_m$Rate[which(heart_m$Type=="45 MIN_Recovery")],na.rm = T) #6.21

median(heart_l$Rate[which(heart_l$Type=="45 MIN_Recovery")],na.rm = T) #79.5
median(heart_m$Rate[which(heart_m$Type=="45 MIN_Recovery")],na.rm = T) #78

t.test(heart_l$Rate[which(heart_l$Type=="45 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="45 MIN_Recovery")],conf.level = 0.95)
#t = 0.25242, df = 57.09, p-value = 0.8016 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="45 MIN_Recovery")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4321, df = 57.299, p-value = 0.1575, significant
t.test(heart_m$Rate[which(heart_m$Type=="45 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.5012, df = 48.416, p-value = 0.1398, significant

#==============================================  60 MIN_Recovery  =======================================#

mean(heart_l$Rate[which(heart_l$Type=="60 MIN_Recovery")],na.rm = T) #77.96
mean(heart_m$Rate[which(heart_m$Type=="60 MIN_Recovery")],na.rm = T) #79.06

sd(heart_l$Rate[which(heart_l$Type=="60 MIN_Recovery")],na.rm = T) #6.77
sd(heart_m$Rate[which(heart_m$Type=="60 MIN_Recovery")],na.rm = T) #5.76

median(heart_l$Rate[which(heart_l$Type=="60 MIN_Recovery")],na.rm = T) #77
median(heart_m$Rate[which(heart_m$Type=="60 MIN_Recovery")],na.rm = T) #80

t.test(heart_l$Rate[which(heart_l$Type=="60 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="60 MIN_Recovery")],conf.level = 0.95)
#t = -0.67729, df = 56.553, p-value = 0.501 not significant

#Intragroup
t.test(heart_l$Rate[which(heart_l$Type=="60 MIN_Recovery")],heart_l$Rate[which(heart_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.1427, df = 56.714, p-value = 0.03645, significant
t.test(heart_m$Rate[which(heart_m$Type=="60 MIN_Recovery")],heart_m$Rate[which(heart_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4206, df = 46.284, p-value = 0.1621, significant

#-----------------------------------------------------------------------------------------------------------------------------------#

####################################################  SBP  ####################################################################

sbp_rate<-read_excel("combined.xlsx",sheet = 2)
sum(is.na(sbp_rate))
str(sbp_rate)

#Subsetting the two study groups
sbp_l<-subset(sbp_rate,sbp_rate$`STUDY GROUP`=="L")
sbp_m<-subset(sbp_rate,sbp_rate$`STUDY GROUP`=="M")

#===============================  PREOP  ===============================#

mean(sbp_l$Rate[which(sbp_l$Type=="PRE OP")],na.rm = T) #121.66
mean(sbp_m$Rate[which(sbp_m$Type=="PRE OP")],na.rm = T) #121.133

sd(sbp_l$Rate[which(sbp_l$Type=="PRE OP")],na.rm = T) #7.77
sd(sbp_m$Rate[which(sbp_m$Type=="PRE OP")],na.rm = T) #6.81

median(sbp_l$Rate[which(sbp_l$Type=="PRE OP")],na.rm = T) #122.5
median(sbp_m$Rate[which(sbp_m$Type=="PRE OP")],na.rm = T) #123.5

t.test(sbp_l$Rate[which(sbp_l$Type=="PRE OP")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.28244, df = 57.018, p-value = 0.7786 hence insignificant difference

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="PRE OP")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="PRE OP")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1, significant

#===============================  INDUCTION  ===========================#

mean(sbp_l$Rate[which(sbp_l$Type=="INDUCTION")],na.rm = T) #112.53
mean(sbp_m$Rate[which(sbp_m$Type=="INDUCTION")],na.rm = T) #111.06

sd(sbp_l$Rate[which(sbp_l$Type=="INDUCTION")],na.rm = T) #10.84
sd(sbp_m$Rate[which(sbp_m$Type=="INDUCTION")],na.rm = T) #10.78

median(sbp_l$Rate[which(sbp_l$Type=="INDUCTION")],na.rm = T) #114
median(sbp_m$Rate[which(sbp_m$Type=="INDUCTION")],na.rm = T) #112

t.test(sbp_l$Rate[which(sbp_l$Type=="INDUCTION")],sbp_m$Rate[which(sbp_m$Type=="INDUCTION")],conf.level = 0.95)
#t = 0.52502, df = 57.998, p-value = 0.6016 , hence insignificant difference

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="INDUCTION")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.7473, df = 52.583, p-value = 0.0004456, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="INDUCTION")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.3204, df = 48.969, p-value = 7.592e-05, significant

#=========================  INTUBATION  ==============================#

mean(sbp_l$Rate[which(sbp_l$Type=="INTUBATION")],na.rm = T) #134.8
mean(sbp_m$Rate[which(sbp_m$Type=="INTUBATION")],na.rm = T) #131.43

sd(sbp_l$Rate[which(sbp_l$Type=="INTUBATION")],na.rm = T) #11.88
sd(sbp_m$Rate[which(sbp_m$Type=="INTUBATION")],na.rm = T) #8.30

median(sbp_l$Rate[which(sbp_l$Type=="INTUBATION")],na.rm = T) #133
median(sbp_m$Rate[which(sbp_m$Type=="INTUBATION")],na.rm = T) #132

t.test(sbp_l$Rate[which(sbp_l$Type=="INTUBATION")],sbp_m$Rate[which(sbp_m$Type=="INTUBATION")],conf.level = 0.95)
#t = 1.2718, df = 51.882, p-value = 0.2091 , hence insignificant difference

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="INTUBATION")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.065, df = 49.998, p-value = 5.938e-06, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="INTUBATION")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 5.2499, df = 55.87, p-value = 2.443e-06, significant

#=================    5 mins ==============================#

mean(sbp_l$Rate[which(sbp_l$Type=="5 MINS")],na.rm = T) #119.06
mean(sbp_m$Rate[which(sbp_m$Type=="5 MINS")],na.rm = T) #115.73

sd(sbp_l$Rate[which(sbp_l$Type=="5 MINS")],na.rm = T) #9.95
sd(sbp_m$Rate[which(sbp_m$Type=="5 MINS")],na.rm = T) #8.53

median(sbp_l$Rate[which(sbp_l$Type=="5 MINS")],na.rm = T) #122
median(sbp_m$Rate[which(sbp_m$Type=="5 MINS")],na.rm = T) #113.5

t.test(sbp_l$Rate[which(sbp_l$Type=="5 MINS")],sbp_m$Rate[which(sbp_m$Type=="5 MINS")],conf.level = 0.95)
# t = 1.3927, df = 56.682, p-value = 0.1691 , Insignificant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="5 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.1275, df = 54.804, p-value = 0.2645, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="5 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.7081, df = 55.3, p-value = 0.008989, significant

#============================  10 MINS  ==================================#

mean(sbp_l$Rate[which(sbp_l$Type=="10 MINS")],na.rm = T) #108.73
mean(sbp_m$Rate[which(sbp_m$Type=="10 MINS")],na.rm = T) #108.26

sd(sbp_l$Rate[which(sbp_l$Type=="10 MINS")],na.rm = T) #9.91
sd(sbp_m$Rate[which(sbp_m$Type=="10 MINS")],na.rm = T) #8.51

median(sbp_l$Rate[which(sbp_l$Type=="10 MINS")],na.rm = T) #105
median(sbp_m$Rate[which(sbp_m$Type=="10 MINS")],na.rm = T) #108

t.test(sbp_l$Rate[which(sbp_l$Type=="10 MINS")],sbp_m$Rate[which(sbp_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.19557, df = 56.714, p-value = 0.8456 , Insignificant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="10 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.6218, df = 54.894, p-value = 6.547e-07, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="10 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -6.46, df = 55.342, p-value = 2.813e-08, significant

#=============================  15 MINS  ==================================#

mean(sbp_l$Rate[which(sbp_l$Type=="15 MINS")],na.rm = T) #108
mean(sbp_m$Rate[which(sbp_m$Type=="15 MINS")],na.rm = T) #103.43

sd(sbp_l$Rate[which(sbp_l$Type=="15 MINS")],na.rm = T) #11.49
sd(sbp_m$Rate[which(sbp_m$Type=="15 MINS")],na.rm = T) #8.99

median(sbp_l$Rate[which(sbp_l$Type=="15 MINS")],na.rm = T) #105.5
median(sbp_m$Rate[which(sbp_m$Type=="15 MINS")],na.rm = T) #104

t.test(sbp_l$Rate[which(sbp_l$Type=="15 MINS")],sbp_m$Rate[which(sbp_m$Type=="15 MINS")],conf.level = 0.95)
#t = 1.7135, df = 54.819, p-value = 0.09227 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="15 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.3922, df = 50.947, p-value = 1.809e-06, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="15 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -8.5912, df = 54.053, p-value = 1.101e-11, significant

#==========================  30 MINS  ======================================#

mean(sbp_l$Rate[which(sbp_l$Type=="30 MINS")],na.rm = T) #105.96
mean(sbp_m$Rate[which(sbp_m$Type=="30 MINS")],na.rm = T) #101.9

sd(sbp_l$Rate[which(sbp_l$Type=="30 MINS")],na.rm = T) #13.7
sd(sbp_m$Rate[which(sbp_m$Type=="30 MINS")],na.rm = T) #10.1

median(sbp_l$Rate[which(sbp_l$Type=="30 MINS")],na.rm = T) #104
median(sbp_m$Rate[which(sbp_m$Type=="30 MINS")],na.rm = T) #100

t.test(sbp_l$Rate[which(sbp_l$Type=="30 MINS")],sbp_m$Rate[which(sbp_m$Type=="30 MINS")],conf.level = 0.95)
#t = 1.3078, df = 53.34, p-value = 0.1965 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="30 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.4561, df = 45.922, p-value = 1.884e-06, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="30 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -8.6411, df = 50.858, p-value = 1.501e-11, significant

#=============================  45 MINS  ===================================#

mean(sbp_l$Rate[which(sbp_l$Type=="45 MINS")],na.rm = T) #106.46
mean(sbp_m$Rate[which(sbp_m$Type=="45 MINS")],na.rm = T) #103.5

sd(sbp_l$Rate[which(sbp_l$Type=="45 MINS")],na.rm = T) #14.6
sd(sbp_m$Rate[which(sbp_m$Type=="45 MINS")],na.rm = T) #12.75

median(sbp_l$Rate[which(sbp_l$Type=="45 MINS")],na.rm = T) #100.5
median(sbp_m$Rate[which(sbp_m$Type=="45 MINS")],na.rm = T) #97.5

t.test(sbp_l$Rate[which(sbp_l$Type=="45 MINS")],sbp_m$Rate[which(sbp_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0.83818, df = 56.967, p-value = 0.4054 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="30 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.4561, df = 45.922, p-value = 1.884e-06, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="30 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -8.6411, df = 50.858, p-value = 1.501e-11, significant

#===============================  60 MINS  ================================#

mean(sbp_l$Rate[which(sbp_l$Type=="60 MINS")],na.rm = T) #103.1
mean(sbp_m$Rate[which(sbp_m$Type=="60 MINS")],na.rm = T) #102.43

sd(sbp_l$Rate[which(sbp_l$Type=="60 MINS")],na.rm = T) #13.3
sd(sbp_m$Rate[which(sbp_m$Type=="60 MINS")],na.rm = T) #12.3

median(sbp_l$Rate[which(sbp_l$Type=="60 MINS")],na.rm = T) #99
median(sbp_m$Rate[which(sbp_m$Type=="60 MINS")],na.rm = T) #99

t.test(sbp_l$Rate[which(sbp_l$Type=="60 MINS")],sbp_m$Rate[which(sbp_m$Type=="60 MINS")],conf.level = 0.95)
#t = 0.20151, df = 57.649, p-value = 0.841 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="60 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -6.5988, df = 46.753, p-value = 3.399e-08, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="60 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -7.2822, df = 45.272, p-value = 3.765e-09, significant

#=============================  75 MINS ===================================#

mean(sbp_l$Rate[which(sbp_l$Type=="75 MINS")],na.rm = T) #108.56
mean(sbp_m$Rate[which(sbp_m$Type=="75 MINS")],na.rm = T) #105.73

sd(sbp_l$Rate[which(sbp_l$Type=="75 MINS")],na.rm = T) #11.02
sd(sbp_m$Rate[which(sbp_m$Type=="75 MINS")],na.rm = T) #9.09

median(sbp_l$Rate[which(sbp_l$Type=="75 MINS")],na.rm = T) #112.5
median(sbp_m$Rate[which(sbp_m$Type=="75 MINS")],na.rm = T) #108

t.test(sbp_l$Rate[which(sbp_l$Type=="75 MINS")],sbp_m$Rate[which(sbp_m$Type=="75 MINS")],conf.level = 0.95)
#t = 1.0855, df = 55.975, p-value = 0.2824 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="75 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.3166, df = 52.129, p-value = 2.242e-06, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="75 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -7.4203, df = 53.76, p-value = 8.776e-10, significant

#============================== 90 MINS  ==================================#

mean(sbp_l$Rate[which(sbp_l$Type=="90 MINS")],na.rm = T) #110.93
mean(sbp_m$Rate[which(sbp_m$Type=="90 MINS")],na.rm = T) #109.53

sd(sbp_l$Rate[which(sbp_l$Type=="90 MINS")],na.rm = T) #13.92
sd(sbp_m$Rate[which(sbp_m$Type=="90 MINS")],na.rm = T) #13.47

median(sbp_l$Rate[which(sbp_l$Type=="90 MINS")],na.rm = T) #109
median(sbp_m$Rate[which(sbp_m$Type=="90 MINS")],na.rm = T) #109

t.test(sbp_l$Rate[which(sbp_l$Type=="90 MINS")],sbp_m$Rate[which(sbp_m$Type=="90 MINS")],conf.level = 0.95)
#t = 0.39561, df = 57.938, p-value = 0.6938 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="90 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.6851, df = 45.486, p-value = 0.0006064, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="90 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.2062, df = 42.921, p-value = 0.0001296, significant

#===================================  105 MINS  ==================================#

mean(sbp_l$Rate[which(sbp_l$Type=="105 MINS")],na.rm = T) #111.96
mean(sbp_m$Rate[which(sbp_m$Type=="105 MINS")],na.rm = T) #105.96

sd(sbp_l$Rate[which(sbp_l$Type=="105 MINS")],na.rm = T) #14.09
sd(sbp_m$Rate[which(sbp_m$Type=="105 MINS")],na.rm = T) #13

median(sbp_l$Rate[which(sbp_l$Type=="105 MINS")],na.rm = T) #115
median(sbp_m$Rate[which(sbp_m$Type=="105 MINS")],na.rm = T) #102

t.test(sbp_l$Rate[which(sbp_l$Type=="105 MINS")],sbp_m$Rate[which(sbp_m$Type=="105 MINS")],conf.level = 0.95)
#t = 1.7134, df = 57.624, p-value = 0.09201 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="105 MINS")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.2994, df = 45.157, p-value = 0.001896, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="105 MINS")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.6584, df = 43.821, p-value = 1.083e-06, significant

#=====================================  120 MINS  ===============================#

mean(sbp_l$Rate[which(sbp_l$Type=="120 MIN")],na.rm = T) #112.27
mean(sbp_m$Rate[which(sbp_m$Type=="120 MIN")],na.rm = T) #105.43

sd(sbp_l$Rate[which(sbp_l$Type=="120 MIN")],na.rm = T) #16.7
sd(sbp_m$Rate[which(sbp_m$Type=="120 MIN")],na.rm = T) #13.72

median(sbp_l$Rate[which(sbp_l$Type=="120 MIN")],na.rm = T) #108
median(sbp_m$Rate[which(sbp_m$Type=="120 MIN")],na.rm = T) #100

t.test(sbp_l$Rate[which(sbp_l$Type=="120 MIN")],sbp_m$Rate[which(sbp_m$Type=="120 MIN")],conf.level = 0.95)
#t = 1.7162, df = 54.183, p-value = 0.09183 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="120 MIN")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.7528, df = 39.303, p-value = 0.008898, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="120 MIN")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.6129, df = 42.493, p-value = 1.38e-06, significant

#====================================  135 MIN  =======================================#

mean(sbp_l$Rate[which(sbp_l$Type=="135 MIN")],na.rm = T) #112.37
mean(sbp_m$Rate[which(sbp_m$Type=="135 MIN")],na.rm = T) #104.66

sd(sbp_l$Rate[which(sbp_l$Type=="135 MIN")],na.rm = T) #18.95
sd(sbp_m$Rate[which(sbp_m$Type=="135 MIN")],na.rm = T) #13.6

median(sbp_l$Rate[which(sbp_l$Type=="135 MIN")],na.rm = T) #108
median(sbp_m$Rate[which(sbp_m$Type=="135 MIN")],na.rm = T) #100

t.test(sbp_l$Rate[which(sbp_l$Type=="135 MIN")],sbp_m$Rate[which(sbp_m$Type=="135 MIN")],conf.level = 0.95)
#t = 1.7903, df = 50.706, p-value = 0.07937 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="135 MIN")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.4473, df = 36.917, p-value = 0.01927, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="135 MIN")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.9257, df = 42.69, p-value = 4.805e-07, significant

#======================================  150 MIN  ====================================#

mean(sbp_l$Rate[which(sbp_l$Type=="150 MIN")],na.rm = T) #109.5
mean(sbp_m$Rate[which(sbp_m$Type=="150 MIN")],na.rm = T) #102.73

sd(sbp_l$Rate[which(sbp_l$Type=="150 MIN")],na.rm = T) #15.33
sd(sbp_m$Rate[which(sbp_m$Type=="150 MIN")],na.rm = T) #11.57

median(sbp_l$Rate[which(sbp_l$Type=="150 MIN")],na.rm = T) #104
median(sbp_m$Rate[which(sbp_m$Type=="150 MIN")],na.rm = T) #102

t.test(sbp_l$Rate[which(sbp_l$Type=="150 MIN")],sbp_m$Rate[which(sbp_m$Type=="150 MIN")],conf.level = 0.95)
#t = 1.9133, df = 52.09, p-value = 0.06121 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="150 MIN")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.8188, df = 41.205, p-value = 0.0004437, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="150 MIN")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -7.5022, df = 46.953, p-value = 1.431e-09, significant

#=======================================  165 MIN  ==================================#

mean(sbp_l$Rate[which(sbp_l$Type=="165 MIN")],na.rm = T) #110.37
mean(sbp_m$Rate[which(sbp_m$Type=="165 MIN")],na.rm = T) #104.26

sd(sbp_l$Rate[which(sbp_l$Type=="165 MIN")],na.rm = T) #13.25
sd(sbp_m$Rate[which(sbp_m$Type=="165 MIN")],na.rm = T) #11.09

median(sbp_l$Rate[which(sbp_l$Type=="165 MIN")],na.rm = T) #110
median(sbp_m$Rate[which(sbp_m$Type=="165 MIN")],na.rm = T) #100

t.test(sbp_l$Rate[which(sbp_l$Type=="165 MIN")],sbp_m$Rate[which(sbp_m$Type=="165 MIN")],conf.level = 0.95)
#t = 1.9181, df = 54.593, p-value = 0.06034 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="165 MIN")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.9728, df = 44.944, p-value = 0.0002541, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="165 MIN")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -7.096, df = 48.17, p-value = 5.146e-09, significant

#=========================================  180 MIN  ================================#

mean(sbp_l$Rate[which(sbp_l$Type=="180 MIN")],na.rm = T) #114
mean(sbp_m$Rate[which(sbp_m$Type=="180 MIN")],na.rm = T) #110.41

sd(sbp_l$Rate[which(sbp_l$Type=="180 MIN")],na.rm = T) #15.63
sd(sbp_m$Rate[which(sbp_m$Type=="180 MIN")],na.rm = T) #15.57

median(sbp_l$Rate[which(sbp_l$Type=="180 MIN")],na.rm = T) #114
median(sbp_m$Rate[which(sbp_m$Type=="180 MIN")],na.rm = T) #112

t.test(sbp_l$Rate[which(sbp_l$Type=="180 MIN")],sbp_m$Rate[which(sbp_m$Type=="180 MIN")],conf.level = 0.95)
#t = 0.85931, df = 53.689, p-value = 0.394 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="180 MIN")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.3048, df = 37.224, p-value = 0.02686, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="180 MIN")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.404, df = 38.065, p-value = 0.001577, significant

#========================================  0 MIN_Emergence  ==========================#

mean(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Emergence")],na.rm = T) #116.63
mean(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Emergence")],na.rm = T) #112.7

sd(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Emergence")],na.rm = T) #15.62
sd(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Emergence")],na.rm = T) #15.81

median(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Emergence")],na.rm = T) #118
median(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Emergence")],na.rm = T) #112

t.test(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 0.88695, df = 57.992, p-value = 0.3788 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Emergence")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.6839, df = 42.539, p-value = 0.09953, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.6827, df = 39.419, p-value = 0.01062, significant

#========================================  1 MIN_Emergence  ============================#

mean(sbp_l$Rate[which(sbp_l$Type=="1 MIN_Emergence")],na.rm = T) #120.93
mean(sbp_m$Rate[which(sbp_m$Type=="1 MIN_Emergence")],na.rm = T) #116.3

sd(sbp_l$Rate[which(sbp_l$Type=="1 MIN_Emergence")],na.rm = T) #16.27
sd(sbp_m$Rate[which(sbp_m$Type=="1 MIN_Emergence")],na.rm = T) #16.07

median(sbp_l$Rate[which(sbp_l$Type=="1 MIN_Emergence")],na.rm = T) #120
median(sbp_m$Rate[which(sbp_m$Type=="1 MIN_Emergence")],na.rm = T) #114

t.test(sbp_l$Rate[which(sbp_l$Type=="1 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 1.1093, df = 57.991, p-value = 0.2719 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="1 MIN_Emergence")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.22264, df = 41.588, p-value = 0.8249, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="1 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.5161, df = 39.102, p-value = 0.1375, significant

#=========================================  2 MIN_Emergence  ===============================#

mean(sbp_l$Rate[which(sbp_l$Type=="2 MIN_Emergence")],na.rm = T) #127.83
mean(sbp_m$Rate[which(sbp_m$Type=="2 MIN_Emergence")],na.rm = T) #122.23

sd(sbp_l$Rate[which(sbp_l$Type=="2 MIN_Emergence")],na.rm = T) #19.29
sd(sbp_m$Rate[which(sbp_m$Type=="2 MIN_Emergence")],na.rm = T) #17.26

median(sbp_l$Rate[which(sbp_l$Type=="2 MIN_Emergence")],na.rm = T) #128
median(sbp_m$Rate[which(sbp_m$Type=="2 MIN_Emergence")],na.rm = T) #120.5

t.test(sbp_l$Rate[which(sbp_l$Type=="2 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 1.1846, df = 57.299, p-value = 0.2411 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="2 MIN_Emergence")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.6236, df = 38.184, p-value = 0.1127, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="2 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.32457, df = 37.825, p-value = 0.7473, significant

#==========================================  3 MIN_Emergence  ================================#

mean(sbp_l$Rate[which(sbp_l$Type=="3 MIN_Emergence")],na.rm = T) #132.63
mean(sbp_m$Rate[which(sbp_m$Type=="3 MIN_Emergence")],na.rm = T) #128.76

sd(sbp_l$Rate[which(sbp_l$Type=="3 MIN_Emergence")],na.rm = T) #16.73
sd(sbp_m$Rate[which(sbp_m$Type=="3 MIN_Emergence")],na.rm = T) #14.17

median(sbp_l$Rate[which(sbp_l$Type=="3 MIN_Emergence")],na.rm = T) #131
median(sbp_m$Rate[which(sbp_m$Type=="3 MIN_Emergence")],na.rm = T) #125

t.test(sbp_l$Rate[which(sbp_l$Type=="3 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 0.96548, df = 56.472, p-value = 0.3384 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="3 MIN_Emergence")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.2543, df = 40.968, p-value = 0.002282, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="3 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 2.6577, df = 41.727, p-value = 0.0111, significant

#===========================================  4 MIN_Emergence  =================================#

mean(sbp_l$Rate[which(sbp_l$Type=="4 MIN_Emergence")],na.rm = T) #133.23
mean(sbp_m$Rate[which(sbp_m$Type=="4 MIN_Emergence")],na.rm = T) #131.36

sd(sbp_l$Rate[which(sbp_l$Type=="4 MIN_Emergence")],na.rm = T) #35.93
sd(sbp_m$Rate[which(sbp_m$Type=="4 MIN_Emergence")],na.rm = T) #24.81

median(sbp_l$Rate[which(sbp_l$Type=="4 MIN_Emergence")],na.rm = T) #136
median(sbp_m$Rate[which(sbp_m$Type=="4 MIN_Emergence")],na.rm = T) #131

t.test(sbp_l$Rate[which(sbp_l$Type=="4 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 0.23411, df = 51.527, p-value = 0.8158 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="4 MIN_Emergence")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7229, df = 31.711, p-value = 0.09465, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="4 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 2.1783, df = 33.353, p-value = 0.03655, significant

#============================================  5 MIN_Emergence  ===================================#

mean(sbp_l$Rate[which(sbp_l$Type=="5 MIN_Emergence")],na.rm = T) #140.4
mean(sbp_m$Rate[which(sbp_m$Type=="5 MIN_Emergence")],na.rm = T) #136.14

sd(sbp_l$Rate[which(sbp_l$Type=="5 MIN_Emergence")],na.rm = T) #14.85
sd(sbp_m$Rate[which(sbp_m$Type=="5 MIN_Emergence")],na.rm = T) #11.59

median(sbp_l$Rate[which(sbp_l$Type=="5 MIN_Emergence")],na.rm = T) #142
median(sbp_m$Rate[which(sbp_m$Type=="5 MIN_Emergence")],na.rm = T) #132

t.test(sbp_l$Rate[which(sbp_l$Type=="5 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = 1.1842, df = 49.183, p-value = 0.242 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="5 MIN_Emergence")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.8721, df = 38.329, p-value = 8.278e-07, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="5 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 5.9565, df = 43.059, p-value = 4.21e-07, significant

#=============================================  6 MIN_Emergence  =====================================#

mean(sbp_l$Rate[which(sbp_l$Type=="6 MIN_Emergence")],na.rm = T) #142.09
mean(sbp_m$Rate[which(sbp_m$Type=="6 MIN_Emergence")],na.rm = T) #138.25

sd(sbp_l$Rate[which(sbp_l$Type=="6 MIN_Emergence")],na.rm = T) #11.96
sd(sbp_m$Rate[which(sbp_m$Type=="6 MIN_Emergence")],na.rm = T) #8.24

median(sbp_l$Rate[which(sbp_l$Type=="6 MIN_Emergence")],na.rm = T) #146
median(sbp_m$Rate[which(sbp_m$Type=="6 MIN_Emergence")],na.rm = T) #139

t.test(sbp_l$Rate[which(sbp_l$Type=="6 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="6 MIN_Emergence")],conf.level = 0.95)
#t = 0.88876, df = 17.58, p-value = 0.3861 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="6 MIN_Emergence")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.2691, df = 13.232, p-value = 0.0001431, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="6 MIN_Emergence")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 6.3713, df = 17.341, p-value = 6.339e-06, significant

#============================================  0 MIN_Recovery  ========================================#

mean(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Recovery")],na.rm = T) #142.46
mean(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Recovery")],na.rm = T) #137.76

sd(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Recovery")],na.rm = T) #15.42
sd(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Recovery")],na.rm = T) #12.69

median(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Recovery")],na.rm = T) #140
median(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Recovery")],na.rm = T) #132

t.test(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="0 MIN_Recovery")],conf.level = 0.95)
#t = 1.2883, df = 55.929, p-value = 0.2029 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="0 MIN_Recovery")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 6.5934, df = 42.847, p-value = 5.087e-08, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="0 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 6.3218, df = 44.434, p-value = 1.091e-07, significant

#============================================  15 MIN_Recovery  =========================================#

mean(sbp_l$Rate[which(sbp_l$Type=="15 MIN_Recovery")],na.rm = T) #130.1
mean(sbp_m$Rate[which(sbp_m$Type=="15 MIN_Recovery")],na.rm = T) #126.53

sd(sbp_l$Rate[which(sbp_l$Type=="15 MIN_Recovery")],na.rm = T) #8.52
sd(sbp_m$Rate[which(sbp_m$Type=="15 MIN_Recovery")],na.rm = T) #6.95

median(sbp_l$Rate[which(sbp_l$Type=="15 MIN_Recovery")],na.rm = T) #132
median(sbp_m$Rate[which(sbp_m$Type=="15 MIN_Recovery")],na.rm = T) #130

t.test(sbp_l$Rate[which(sbp_l$Type=="15 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="15 MIN_Recovery")],conf.level = 0.95)
#t = 1.7757, df = 55.762, p-value = 0.08125 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="15 MIN_Recovery")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 4.003, df = 57.522, p-value = 0.0001813, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="15 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.0368, df = 57.976, p-value = 0.00358, significant

#============================================  30 MIN_Recovery  ========================================#

mean(sbp_l$Rate[which(sbp_l$Type=="30 MIN_Recovery")],na.rm = T) #126.13
mean(sbp_m$Rate[which(sbp_m$Type=="30 MIN_Recovery")],na.rm = T) #125.43

sd(sbp_l$Rate[which(sbp_l$Type=="30 MIN_Recovery")],na.rm = T) #5.21
sd(sbp_m$Rate[which(sbp_m$Type=="30 MIN_Recovery")],na.rm = T) #4.65

median(sbp_l$Rate[which(sbp_l$Type=="30 MIN_Recovery")],na.rm = T) #127
median(sbp_m$Rate[which(sbp_m$Type=="30 MIN_Recovery")],na.rm = T) #126.5

t.test(sbp_l$Rate[which(sbp_l$Type=="30 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="30 MIN_Recovery")],conf.level = 0.95)
#t = 0.54853, df = 57.287, p-value = 0.5855 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="30 MIN_Recovery")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.613, df = 50.666, p-value = 0.01178, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="30 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 2.8526, df = 51.239, p-value = 0.006239, significant

#==============================================  45 MIN_Recovery  ======================================#

mean(sbp_l$Rate[which(sbp_l$Type=="45 MIN_Recovery")],na.rm = T) #121.2
mean(sbp_m$Rate[which(sbp_m$Type=="45 MIN_Recovery")],na.rm = T) #119.73

sd(sbp_l$Rate[which(sbp_l$Type=="45 MIN_Recovery")],na.rm = T) #6.2
sd(sbp_m$Rate[which(sbp_m$Type=="45 MIN_Recovery")],na.rm = T) #5.98

median(sbp_l$Rate[which(sbp_l$Type=="45 MIN_Recovery")],na.rm = T) #122
median(sbp_m$Rate[which(sbp_m$Type=="45 MIN_Recovery")],na.rm = T) #121

t.test(sbp_l$Rate[which(sbp_l$Type=="45 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="45 MIN_Recovery")],conf.level = 0.95)
#t = 0.9324, df = 57.926, p-value = 0.355 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="45 MIN_Recovery")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.25696, df = 55.253, p-value = 0.7982, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="45 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.8455, df = 57.039, p-value = 0.4014, significant

#==============================================  60 MIN_Recovery  =======================================#

mean(sbp_l$Rate[which(sbp_l$Type=="60 MIN_Recovery")],na.rm = T) #119.56
mean(sbp_m$Rate[which(sbp_m$Type=="60 MIN_Recovery")],na.rm = T) #119

sd(sbp_l$Rate[which(sbp_l$Type=="60 MIN_Recovery")],na.rm = T) #7.61
sd(sbp_m$Rate[which(sbp_m$Type=="60 MIN_Recovery")],na.rm = T) #7.15

median(sbp_l$Rate[which(sbp_l$Type=="60 MIN_Recovery")],na.rm = T) #122.5
median(sbp_m$Rate[which(sbp_m$Type=="60 MIN_Recovery")],na.rm = T) #120

t.test(sbp_l$Rate[which(sbp_l$Type=="60 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="60 MIN_Recovery")],conf.level = 0.95)
#t = 0.29699, df = 57.771, p-value = 0.7675 not significant

#Intragroup
t.test(sbp_l$Rate[which(sbp_l$Type=="60 MIN_Recovery")],sbp_l$Rate[which(sbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.0564, df = 57.975, p-value = 0.2952, significant
t.test(sbp_m$Rate[which(sbp_m$Type=="60 MIN_Recovery")],sbp_m$Rate[which(sbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.1825, df = 57.865, p-value = 0.2418, significant

#############################################   DBP  #########################################################

dbp_rate<-read_excel("combined.xlsx",sheet = 3)
sum(is.na(dbp_rate))
str(dbp_rate)

#Subsetting the two study groups
dbp_l<-subset(dbp_rate,dbp_rate$`STUDY GROUP`=="L")
dbp_m<-subset(dbp_rate,dbp_rate$`STUDY GROUP`=="M")

#===============================  PREOP  ===============================#

mean(dbp_l$Rate[which(dbp_l$Type=="PRE OP")],na.rm = T) #75.7
mean(dbp_m$Rate[which(dbp_m$Type=="PRE OP")],na.rm = T) #75.4

sd(dbp_l$Rate[which(dbp_l$Type=="PRE OP")],na.rm = T) #8.21
sd(dbp_m$Rate[which(dbp_m$Type=="PRE OP")],na.rm = T) #7.81

median(dbp_l$Rate[which(dbp_l$Type=="PRE OP")],na.rm = T) #78.5
median(dbp_m$Rate[which(dbp_m$Type=="PRE OP")],na.rm = T) #76

t.test(dbp_l$Rate[which(dbp_l$Type=="PRE OP")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.14497, df = 57.854, p-value = 0.8852 hence insignificant difference

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="PRE OP")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="PRE OP")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1, significant

#===============================  INDUCTION  ===========================#

mean(dbp_l$Rate[which(dbp_l$Type=="INDUCTION")],na.rm = T) #72.43
mean(dbp_m$Rate[which(dbp_m$Type=="INDUCTION")],na.rm = T) #71.5

sd(dbp_l$Rate[which(dbp_l$Type=="INDUCTION")],na.rm = T) #10.94
sd(dbp_m$Rate[which(dbp_m$Type=="INDUCTION")],na.rm = T) #8.06

median(dbp_l$Rate[which(dbp_l$Type=="INDUCTION")],na.rm = T) #70
median(dbp_m$Rate[which(dbp_m$Type=="INDUCTION")],na.rm = T) #70

t.test(dbp_l$Rate[which(dbp_l$Type=="INDUCTION")],dbp_m$Rate[which(dbp_m$Type=="INDUCTION")],conf.level = 0.95)
#t = 0.37606, df = 53.319, p-value = 0.7084 , hence insignificant difference

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="INDUCTION")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.3076, df = 53.8, p-value = 0.1966, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="INDUCTION")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.9028, df = 57.941, p-value = 0.06204, significant

#=========================  INTUBATION  ==============================#

mean(dbp_l$Rate[which(dbp_l$Type=="INTUBATION")],na.rm = T) #82.86
mean(dbp_m$Rate[which(dbp_m$Type=="INTUBATION")],na.rm = T) #82.8

sd(dbp_l$Rate[which(dbp_l$Type=="INTUBATION")],na.rm = T) #15.05
sd(dbp_m$Rate[which(dbp_m$Type=="INTUBATION")],na.rm = T) #11.54

median(dbp_l$Rate[which(dbp_l$Type=="INTUBATION")],na.rm = T) #84
median(dbp_m$Rate[which(dbp_m$Type=="INTUBATION")],na.rm = T) #84

t.test(dbp_l$Rate[which(dbp_l$Type=="INTUBATION")],dbp_m$Rate[which(dbp_m$Type=="INTUBATION")],conf.level = 0.95)
#t = 0.019248, df = 54.334, p-value = 0.9847 , hence insignificant difference

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="INTUBATION")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.2888, df = 44.857, p-value = 0.02685, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="INTUBATION")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 2.9084, df = 50.958, p-value = 0.005369, significant

#=================    5 mins ==============================#

mean(dbp_l$Rate[which(dbp_l$Type=="5 MINS")],na.rm = T) #70.83
mean(dbp_m$Rate[which(dbp_m$Type=="5 MINS")],na.rm = T) #68.7

sd(dbp_l$Rate[which(dbp_l$Type=="5 MINS")],na.rm = T) #10.14
sd(dbp_m$Rate[which(dbp_m$Type=="5 MINS")],na.rm = T) #9.05

median(dbp_l$Rate[which(dbp_l$Type=="5 MINS")],na.rm = T) #72
median(dbp_m$Rate[which(dbp_m$Type=="5 MINS")],na.rm = T) #67

t.test(dbp_l$Rate[which(dbp_l$Type=="5 MINS")],dbp_m$Rate[which(dbp_m$Type=="5 MINS")],conf.level = 0.95)
# t = 0.85944, df = 57.264, p-value = 0.3937 , Insignificant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="5 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.0422, df = 55.594, p-value = 0.04588, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="5 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.0693, df = 56.782, p-value = 0.003287, significant

#============================  10 MINS  ==================================#

mean(dbp_l$Rate[which(dbp_l$Type=="10 MINS")],na.rm = T) #67.63
mean(dbp_m$Rate[which(dbp_m$Type=="10 MINS")],na.rm = T) #67.9

sd(dbp_l$Rate[which(dbp_l$Type=="10 MINS")],na.rm = T) #8.41
sd(dbp_m$Rate[which(dbp_m$Type=="10 MINS")],na.rm = T) #7.25

median(dbp_l$Rate[which(dbp_l$Type=="10 MINS")],na.rm = T) #68.5
median(dbp_m$Rate[which(dbp_m$Type=="10 MINS")],na.rm = T) #68.5

t.test(dbp_l$Rate[which(dbp_l$Type=="10 MINS")],dbp_m$Rate[which(dbp_m$Type=="10 MINS")],conf.level = 0.95)
#t = -0.13146, df = 56.755, p-value = 0.8959 , Insignificant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="10 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.7568, df = 57.965, p-value = 0.0004016, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="10 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.8545, df = 57.682, p-value = 0.0002941, significant

#=============================  15 MINS  ==================================#

mean(dbp_l$Rate[which(dbp_l$Type=="15 MINS")],na.rm = T) #65.93
mean(dbp_m$Rate[which(dbp_m$Type=="15 MINS")],na.rm = T) #63.73

sd(dbp_l$Rate[which(dbp_l$Type=="15 MINS")],na.rm = T) #10.55
sd(dbp_m$Rate[which(dbp_m$Type=="15 MINS")],na.rm = T) #8.52

median(dbp_l$Rate[which(dbp_l$Type=="15 MINS")],na.rm = T) #64
median(dbp_m$Rate[which(dbp_m$Type=="15 MINS")],na.rm = T) #62

t.test(dbp_l$Rate[which(dbp_l$Type=="15 MINS")],dbp_m$Rate[which(dbp_m$Type=="15 MINS")],conf.level = 0.95)
#t = 0.88802, df = 55.556, p-value = 0.3784 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="15 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.0002, df = 54.701, p-value = 0.0001916, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="15 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.5251, df = 57.556, p-value = 8.299e-07, significant

#==========================  30 MINS  ======================================#

mean(dbp_l$Rate[which(dbp_l$Type=="30 MINS")],na.rm = T) #65.6
mean(dbp_m$Rate[which(dbp_m$Type=="30 MINS")],na.rm = T) #64.63

sd(dbp_l$Rate[which(dbp_l$Type=="30 MINS")],na.rm = T) #10.38
sd(dbp_m$Rate[which(dbp_m$Type=="30 MINS")],na.rm = T) #8.41

median(dbp_l$Rate[which(dbp_l$Type=="30 MINS")],na.rm = T) #64
median(dbp_m$Rate[which(dbp_m$Type=="30 MINS")],na.rm = T) #63.5

t.test(dbp_l$Rate[which(dbp_l$Type=="30 MINS")],dbp_m$Rate[which(dbp_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0.39623, df = 55.615, p-value = 0.6935 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="30 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.1791, df = 55.085, p-value = 0.0001055, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="30 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.1367, df = 57.682, p-value = 3.454e-06, significant

#=============================  45 MINS  ===================================#

mean(dbp_l$Rate[which(dbp_l$Type=="45 MINS")],na.rm = T) #67.36
mean(dbp_m$Rate[which(dbp_m$Type=="45 MINS")],na.rm = T) #66.4

sd(dbp_l$Rate[which(dbp_l$Type=="45 MINS")],na.rm = T) #10.03
sd(dbp_m$Rate[which(dbp_m$Type=="45 MINS")],na.rm = T) #7.83

median(dbp_l$Rate[which(dbp_l$Type=="45 MINS")],na.rm = T) #66
median(dbp_m$Rate[which(dbp_m$Type=="45 MINS")],na.rm = T) #66

t.test(dbp_l$Rate[which(dbp_l$Type=="45 MINS")],dbp_m$Rate[which(dbp_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0.41598, df = 54.776, p-value = 0.6791 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="45 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.5203, df = 55.823, p-value = 0.0008663, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="45 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.4564, df = 58, p-value = 3.866e-05, significant

#===============================  60 MINS  ================================#

mean(dbp_l$Rate[which(dbp_l$Type=="60 MINS")],na.rm = T) #65.1
mean(dbp_m$Rate[which(dbp_m$Type=="60 MINS")],na.rm = T) #65.36

sd(dbp_l$Rate[which(dbp_l$Type=="60 MINS")],na.rm = T) #11.87
sd(dbp_m$Rate[which(dbp_m$Type=="60 MINS")],na.rm = T) #8.69

median(dbp_l$Rate[which(dbp_l$Type=="60 MINS")],na.rm = T) #65
median(dbp_m$Rate[which(dbp_m$Type=="60 MINS")],na.rm = T) #64

t.test(dbp_l$Rate[which(dbp_l$Type=="60 MINS")],dbp_m$Rate[which(dbp_m$Type=="60 MINS")],conf.level = 0.95)
#t = -0.099209, df = 53.145, p-value = 0.9213 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="60 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.02, df = 51.568, p-value = 0.0001903, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="60 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.7015, df = 57.344, p-value = 1.665e-05, significant

#=============================  75 MINS ===================================#

mean(dbp_l$Rate[which(dbp_l$Type=="75 MINS")],na.rm = T) #69.166
mean(dbp_m$Rate[which(dbp_m$Type=="75 MINS")],na.rm = T) #67.33

sd(dbp_l$Rate[which(dbp_l$Type=="75 MINS")],na.rm = T) #8.39
sd(dbp_m$Rate[which(dbp_m$Type=="75 MINS")],na.rm = T) #6.53

median(dbp_l$Rate[which(dbp_l$Type=="75 MINS")],na.rm = T) #66.5
median(dbp_m$Rate[which(dbp_m$Type=="75 MINS")],na.rm = T) #66.5

t.test(dbp_l$Rate[which(dbp_l$Type=="75 MINS")],dbp_m$Rate[which(dbp_m$Type=="75 MINS")],conf.level = 0.95)
#t = 0.94379, df = 54.706, p-value = 0.3494 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="75 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.0468, df = 57.972, p-value = 0.003479, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="75 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.3383, df = 56.25, p-value = 6.023e-05, significant

#============================== 90 MINS  ==================================#

mean(dbp_l$Rate[which(dbp_l$Type=="90 MINS")],na.rm = T) #67.1
mean(dbp_m$Rate[which(dbp_m$Type=="90 MINS")],na.rm = T) #67.233

sd(dbp_l$Rate[which(dbp_l$Type=="90 MINS")],na.rm = T) #7.66
sd(dbp_m$Rate[which(dbp_m$Type=="90 MINS")],na.rm = T) #6.01

median(dbp_l$Rate[which(dbp_l$Type=="90 MINS")],na.rm = T) #66
median(dbp_m$Rate[which(dbp_m$Type=="90 MINS")],na.rm = T) #67

t.test(dbp_l$Rate[which(dbp_l$Type=="90 MINS")],dbp_m$Rate[which(dbp_m$Type=="90 MINS")],conf.level = 0.95)
#t = -0.07494, df = 54.892, p-value = 0.9405 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="90 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.1924, df = 57.727, p-value = 9.599e-05, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="90 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.5372, df = 54.449, p-value = 3.181e-05, significant

#===================================  105 MINS  ==================================#

mean(dbp_l$Rate[which(dbp_l$Type=="105 MINS")],na.rm = T) #70.36
mean(dbp_m$Rate[which(dbp_m$Type=="105 MINS")],na.rm = T) #68.96

sd(dbp_l$Rate[which(dbp_l$Type=="105 MINS")],na.rm = T) #9.79
sd(dbp_m$Rate[which(dbp_m$Type=="105 MINS")],na.rm = T) #8.26

median(dbp_l$Rate[which(dbp_l$Type=="105 MINS")],na.rm = T) #70.5
median(dbp_m$Rate[which(dbp_m$Type=="105 MINS")],na.rm = T) #68.5

t.test(dbp_l$Rate[which(dbp_l$Type=="105 MINS")],dbp_m$Rate[which(dbp_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0.59828, df = 56.402, p-value = 0.552 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="105 MINS")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.2851, df = 56.287, p-value = 0.0261, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="105 MINS")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.0986, df = 57.816, p-value = 0.003, significant

#=====================================  120 MINS  ===============================#

mean(dbp_l$Rate[which(dbp_l$Type=="120 MIN")],na.rm = T) #69.06
mean(dbp_m$Rate[which(dbp_m$Type=="120 MIN")],na.rm = T) #67.13

sd(dbp_l$Rate[which(dbp_l$Type=="120 MIN")],na.rm = T) #11.71
sd(dbp_m$Rate[which(dbp_m$Type=="120 MIN")],na.rm = T) #9.64

median(dbp_l$Rate[which(dbp_l$Type=="120 MIN")],na.rm = T) #68
median(dbp_m$Rate[which(dbp_m$Type=="120 MIN")],na.rm = T) #68

t.test(dbp_l$Rate[which(dbp_l$Type=="120 MIN")],dbp_m$Rate[which(dbp_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0.69155, df = 54.229, p-value = 0.4922 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="120 MIN")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.5094, df = 50.021, p-value = 0.01538, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="120 MIN")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.6484, df = 55.599, p-value = 0.0005834, significant

#====================================  135 MIN  =======================================#

mean(dbp_l$Rate[which(dbp_l$Type=="135 MIN")],na.rm = T) #65.68
mean(dbp_m$Rate[which(dbp_m$Type=="135 MIN")],na.rm = T) #66.96

sd(dbp_l$Rate[which(dbp_l$Type=="135 MIN")],na.rm = T) #9.33
sd(dbp_m$Rate[which(dbp_m$Type=="135 MIN")],na.rm = T) #7.98

median(dbp_l$Rate[which(dbp_l$Type=="135 MIN")],na.rm = T) #65
median(dbp_m$Rate[which(dbp_m$Type=="135 MIN")],na.rm = T) #68

t.test(dbp_l$Rate[which(dbp_l$Type=="135 MIN")],dbp_m$Rate[which(dbp_m$Type=="135 MIN")],conf.level = 0.95)
#t = -0.5638, df = 55.051, p-value = 0.5752 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="135 MIN")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.3683, df = 55.557, p-value = 5.521e-05, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="135 MIN")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.1342, df = 57.97, p-value = 0.0001162, significant

#======================================  150 MIN  ====================================#

mean(dbp_l$Rate[which(dbp_l$Type=="150 MIN")],na.rm = T) #67.31
mean(dbp_m$Rate[which(dbp_m$Type=="150 MIN")],na.rm = T) #67.43

sd(dbp_l$Rate[which(dbp_l$Type=="150 MIN")],na.rm = T) #8.72
sd(dbp_m$Rate[which(dbp_m$Type=="150 MIN")],na.rm = T) #7.19

median(dbp_l$Rate[which(dbp_l$Type=="150 MIN")],na.rm = T) #68
median(dbp_m$Rate[which(dbp_m$Type=="150 MIN")],na.rm = T) #69.5

t.test(dbp_l$Rate[which(dbp_l$Type=="150 MIN")],dbp_m$Rate[which(dbp_m$Type=="150 MIN")],conf.level = 0.95)
#t = -0.05899, df = 54.262, p-value = 0.9532 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="150 MIN")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.8009, df = 56.496, p-value = 0.0003554, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="150 MIN")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.1101, df = 57.607, p-value = 0.0001268, significant

#=======================================  165 MIN  ==================================#

mean(dbp_l$Rate[which(dbp_l$Type=="165 MIN")],na.rm = T) #69.34
mean(dbp_m$Rate[which(dbp_m$Type=="165 MIN")],na.rm = T) #68.53

sd(dbp_l$Rate[which(dbp_l$Type=="165 MIN")],na.rm = T) #9.25
sd(dbp_m$Rate[which(dbp_m$Type=="165 MIN")],na.rm = T) #7.66

median(dbp_l$Rate[which(dbp_l$Type=="165 MIN")],na.rm = T) #70
median(dbp_m$Rate[which(dbp_m$Type=="165 MIN")],na.rm = T) #70

t.test(dbp_l$Rate[which(dbp_l$Type=="165 MIN")],dbp_m$Rate[which(dbp_m$Type=="165 MIN")],conf.level = 0.95)
#t = 0.36633, df = 54.357, p-value = 0.7155 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="165 MIN")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.7869, df = 55.701, p-value = 0.007263, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="165 MIN")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.4379, df = 57.978, p-value = 0.001091, significant

#=========================================  180 MIN  ================================#

mean(dbp_l$Rate[which(dbp_l$Type=="180 MIN")],na.rm = T) #70.44
mean(dbp_m$Rate[which(dbp_m$Type=="180 MIN")],na.rm = T) #73.27

sd(dbp_l$Rate[which(dbp_l$Type=="180 MIN")],na.rm = T) #9.53
sd(dbp_m$Rate[which(dbp_m$Type=="180 MIN")],na.rm = T) #10.74

median(dbp_l$Rate[which(dbp_l$Type=="180 MIN")],na.rm = T) #73
median(dbp_m$Rate[which(dbp_m$Type=="180 MIN")],na.rm = T) #74

t.test(dbp_l$Rate[which(dbp_l$Type=="180 MIN")],dbp_m$Rate[which(dbp_m$Type=="180 MIN")],conf.level = 0.95)
#t = -1.0445, df = 53.881, p-value = 0.3009 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="180 MIN")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.2181, df = 51.67, p-value = 0.03097, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="180 MIN")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.86602, df = 51.045, p-value = 0.3905, significant

#========================================  0 MIN_Emergence  ==========================#

mean(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Emergence")],na.rm = T) #73.93
mean(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Emergence")],na.rm = T) #74.7

sd(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Emergence")],na.rm = T) #9.95
sd(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Emergence")],na.rm = T) #9.39

median(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Emergence")],na.rm = T) #74
median(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Emergence")],na.rm = T) #76

t.test(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = -0.30676, df = 57.812, p-value = 0.7601 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Emergence")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.74993, df = 55.987, p-value = 0.4564, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.31372, df = 56.119, p-value = 0.7549, significant

#========================================  1 MIN_Emergence  ============================#

mean(dbp_l$Rate[which(dbp_l$Type=="1 MIN_Emergence")],na.rm = T) #75.5
mean(dbp_m$Rate[which(dbp_m$Type=="1 MIN_Emergence")],na.rm = T) #74.93

sd(dbp_l$Rate[which(dbp_l$Type=="1 MIN_Emergence")],na.rm = T) #9.93
sd(dbp_m$Rate[which(dbp_m$Type=="1 MIN_Emergence")],na.rm = T) #9.39

median(dbp_l$Rate[which(dbp_l$Type=="1 MIN_Emergence")],na.rm = T) #70
median(dbp_m$Rate[which(dbp_m$Type=="1 MIN_Emergence")],na.rm = T) #70

t.test(dbp_l$Rate[which(dbp_l$Type=="1 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 0.227, df = 57.816, p-value = 0.8212 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="1 MIN_Emergence")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.084972, df = 56.015, p-value = 0.9326, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="1 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.20925, df = 56.135, p-value = 0.835, significant

#=========================================  2 MIN_Emergence  ===============================#

mean(dbp_l$Rate[which(dbp_l$Type=="2 MIN_Emergence")],na.rm = T) #80.4
mean(dbp_m$Rate[which(dbp_m$Type=="2 MIN_Emergence")],na.rm = T) #79.16

sd(dbp_l$Rate[which(dbp_l$Type=="2 MIN_Emergence")],na.rm = T) #14.27
sd(dbp_m$Rate[which(dbp_m$Type=="2 MIN_Emergence")],na.rm = T) #11.65

median(dbp_l$Rate[which(dbp_l$Type=="2 MIN_Emergence")],na.rm = T) #79
median(dbp_m$Rate[which(dbp_m$Type=="2 MIN_Emergence")],na.rm = T) #78

t.test(dbp_l$Rate[which(dbp_l$Type=="2 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 0.36663, df = 55.769, p-value = 0.7153 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="2 MIN_Emergence")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.5633, df = 46.31, p-value = 0.1248, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="2 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.4706, df = 50.681, p-value = 0.1476, significant

#==========================================  3 MIN_Emergence  ================================#

mean(dbp_l$Rate[which(dbp_l$Type=="3 MIN_Emergence")],na.rm = T) #79.4
mean(dbp_m$Rate[which(dbp_m$Type=="3 MIN_Emergence")],na.rm = T) #79.66

sd(dbp_l$Rate[which(dbp_l$Type=="3 MIN_Emergence")],na.rm = T) #12.78
sd(dbp_m$Rate[which(dbp_m$Type=="3 MIN_Emergence")],na.rm = T) #10.07

median(dbp_l$Rate[which(dbp_l$Type=="3 MIN_Emergence")],na.rm = T) #80
median(dbp_m$Rate[which(dbp_m$Type=="3 MIN_Emergence")],na.rm = T) #80

t.test(dbp_l$Rate[which(dbp_l$Type=="3 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = -0.089742, df = 55.005, p-value = 0.9288 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="3 MIN_Emergence")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.3339, df = 49.462, p-value = 0.1883, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="3 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.833, df = 54.605, p-value = 0.07226, significant

#===========================================  4 MIN_Emergence  =================================#

mean(dbp_l$Rate[which(dbp_l$Type=="4 MIN_Emergence")],na.rm = T) #84.06
mean(dbp_m$Rate[which(dbp_m$Type=="4 MIN_Emergence")],na.rm = T) #83.3

sd(dbp_l$Rate[which(dbp_l$Type=="4 MIN_Emergence")],na.rm = T) #10.21
sd(dbp_m$Rate[which(dbp_m$Type=="4 MIN_Emergence")],na.rm = T) #8.37

median(dbp_l$Rate[which(dbp_l$Type=="4 MIN_Emergence")],na.rm = T) #84
median(dbp_m$Rate[which(dbp_m$Type=="4 MIN_Emergence")],na.rm = T) #84

t.test(dbp_l$Rate[which(dbp_l$Type=="4 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 0.31777, df = 55.859, p-value = 0.7518 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="4 MIN_Emergence")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.4956, df = 55.438, p-value = 0.0009386, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="4 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.7773, df = 57.716, p-value = 0.0003772, significant

#============================================  5 MIN_Emergence  ===================================#

mean(dbp_l$Rate[which(dbp_l$Type=="5 MIN_Emergence")],na.rm = T) #86.62
mean(dbp_m$Rate[which(dbp_m$Type=="5 MIN_Emergence")],na.rm = T) #86.10

sd(dbp_l$Rate[which(dbp_l$Type=="5 MIN_Emergence")],na.rm = T) #8.49
sd(dbp_m$Rate[which(dbp_m$Type=="5 MIN_Emergence")],na.rm = T) #7.74

median(dbp_l$Rate[which(dbp_l$Type=="5 MIN_Emergence")],na.rm = T) #88
median(dbp_m$Rate[which(dbp_m$Type=="5 MIN_Emergence")],na.rm = T) #88

t.test(dbp_l$Rate[which(dbp_l$Type=="5 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = 0.23807, df = 52.121, p-value = 0.8128 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="5 MIN_Emergence")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 4.9253, df = 53.921, p-value = 8.367e-06, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="5 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 5.2404, df = 55.789, p-value = 2.537e-06, significant

#=============================================  6 MIN_Emergence  =====================================#

mean(dbp_l$Rate[which(dbp_l$Type=="6 MIN_Emergence")],na.rm = T) #84
mean(dbp_m$Rate[which(dbp_m$Type=="6 MIN_Emergence")],na.rm = T) #87.33

sd(dbp_l$Rate[which(dbp_l$Type=="6 MIN_Emergence")],na.rm = T) #9.03
sd(dbp_m$Rate[which(dbp_m$Type=="6 MIN_Emergence")],na.rm = T) #5.06

median(dbp_l$Rate[which(dbp_l$Type=="6 MIN_Emergence")],na.rm = T) #86
median(dbp_m$Rate[which(dbp_m$Type=="6 MIN_Emergence")],na.rm = T) #89

t.test(dbp_l$Rate[which(dbp_l$Type=="6 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="6 MIN_Emergence")],conf.level = 0.95)
#t = -1.0781, df = 15.437, p-value = 0.2975 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="6 MIN_Emergence")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.6695, df = 16.46, p-value = 0.01649, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="6 MIN_Emergence")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 5.8403, df = 31.155, p-value = 1.9e-06, significant

#============================================  0 MIN_Recovery  ========================================#

mean(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Recovery")],na.rm = T) #86.23
mean(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Recovery")],na.rm = T) #86.33

sd(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Recovery")],na.rm = T) #9.27
sd(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Recovery")],na.rm = T) #9

median(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Recovery")],na.rm = T) #84
median(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Recovery")],na.rm = T) #84

t.test(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="0 MIN_Recovery")],conf.level = 0.95)
#t = -0.042362, df = 57.95, p-value = 0.9664 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="0 MIN_Recovery")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 4.6565, df = 57.162, p-value = 1.96e-05, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="0 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 5.023, df = 56.861, p-value = 5.361e-06, significant

#============================================  15 MIN_Recovery  =========================================#

mean(dbp_l$Rate[which(dbp_l$Type=="15 MIN_Recovery")],na.rm = T) #78.46
mean(dbp_m$Rate[which(dbp_m$Type=="15 MIN_Recovery")],na.rm = T) #77.13

sd(dbp_l$Rate[which(dbp_l$Type=="15 MIN_Recovery")],na.rm = T) #6.34
sd(dbp_m$Rate[which(dbp_m$Type=="15 MIN_Recovery")],na.rm = T) #6.34

median(dbp_l$Rate[which(dbp_l$Type=="15 MIN_Recovery")],na.rm = T) #80
median(dbp_m$Rate[which(dbp_m$Type=="15 MIN_Recovery")],na.rm = T) #79.5

t.test(dbp_l$Rate[which(dbp_l$Type=="15 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="15 MIN_Recovery")],conf.level = 0.95)
#t = 0.82746, df = 57.934, p-value = 0.4114 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="15 MIN_Recovery")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.4601, df = 54.523, p-value = 0.15, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="15 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.95589, df = 54.918, p-value = 0.3433, significant

#============================================  30 MIN_Recovery  ========================================#

mean(dbp_l$Rate[which(dbp_l$Type=="30 MIN_Recovery")],na.rm = T) #74.93
mean(dbp_m$Rate[which(dbp_m$Type=="30 MIN_Recovery")],na.rm = T) #75.73

sd(dbp_l$Rate[which(dbp_l$Type=="30 MIN_Recovery")],na.rm = T) #4.98
sd(dbp_m$Rate[which(dbp_m$Type=="30 MIN_Recovery")],na.rm = T) #4.92

median(dbp_l$Rate[which(dbp_l$Type=="30 MIN_Recovery")],na.rm = T) #72
median(dbp_m$Rate[which(dbp_m$Type=="30 MIN_Recovery")],na.rm = T) #75

t.test(dbp_l$Rate[which(dbp_l$Type=="30 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="30 MIN_Recovery")],conf.level = 0.95)
#t = -0.62537, df = 57.993, p-value = 0.5342 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="30 MIN_Recovery")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.43713, df = 47.797, p-value = 0.6645, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="30 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.19771, df = 48.922, p-value = 0.8441, significant

#==============================================  45 MIN_Recovery  ======================================#

mean(dbp_l$Rate[which(dbp_l$Type=="45 MIN_Recovery")],na.rm = T) #73.96
mean(dbp_m$Rate[which(dbp_m$Type=="45 MIN_Recovery")],na.rm = T) #74.76

sd(dbp_l$Rate[which(dbp_l$Type=="45 MIN_Recovery")],na.rm = T) #5.67
sd(dbp_m$Rate[which(dbp_m$Type=="45 MIN_Recovery")],na.rm = T) #5.2

median(dbp_l$Rate[which(dbp_l$Type=="45 MIN_Recovery")],na.rm = T) #73.5
median(dbp_m$Rate[which(dbp_m$Type=="45 MIN_Recovery")],na.rm = T) #75

t.test(dbp_l$Rate[which(dbp_l$Type=="45 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="45 MIN_Recovery")],conf.level = 0.95)
#t = -0.56891, df = 57.565, p-value = 0.5716 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="45 MIN_Recovery")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.95082, df = 51.565, p-value = 0.3461, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="45 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.3696, df = 50.508, p-value = 0.7132, significant

#==============================================  60 MIN_Recovery  =======================================#

mean(dbp_l$Rate[which(dbp_l$Type=="60 MIN_Recovery")],na.rm = T) #72.43
mean(dbp_m$Rate[which(dbp_m$Type=="60 MIN_Recovery")],na.rm = T) #74.03

sd(dbp_l$Rate[which(dbp_l$Type=="60 MIN_Recovery")],na.rm = T) #6
sd(dbp_m$Rate[which(dbp_m$Type=="60 MIN_Recovery")],na.rm = T) #5.84

median(dbp_l$Rate[which(dbp_l$Type=="60 MIN_Recovery")],na.rm = T) #71
median(dbp_m$Rate[which(dbp_m$Type=="60 MIN_Recovery")],na.rm = T) #75

t.test(dbp_l$Rate[which(dbp_l$Type=="60 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="60 MIN_Recovery")],conf.level = 0.95)
#t = -1.0453, df = 57.956, p-value = 0.3002 not significant

#Intragroup
t.test(dbp_l$Rate[which(dbp_l$Type=="60 MIN_Recovery")],dbp_l$Rate[which(dbp_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.7581, df = 53.134, p-value = 0.08449, significant
t.test(dbp_m$Rate[which(dbp_m$Type=="60 MIN_Recovery")],dbp_m$Rate[which(dbp_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.76728, df = 53.729, p-value = 0.4463, significant

##################################################  MAP  ####################################################################

map_rate<-read_excel("combined.xlsx",sheet = 4)
sum(is.na(map_rate))
str(map_rate)

#Subsetting the two study groups
map_l<-subset(map_rate,map_rate$`STUDY GROUP`=="L")
map_m<-subset(map_rate,map_rate$`STUDY GROUP`=="M")

#===============================  PREOP  ===============================#

mean(map_l$Rate[which(map_l$Type=="PRE OP")],na.rm = T) #91.83
mean(map_m$Rate[which(map_m$Type=="PRE OP")],na.rm = T) #90.76

sd(map_l$Rate[which(map_l$Type=="PRE OP")],na.rm = T) #7.01
sd(map_m$Rate[which(map_m$Type=="PRE OP")],na.rm = T) #6.19

median(map_l$Rate[which(map_l$Type=="PRE OP")],na.rm = T) #92.5
median(map_m$Rate[which(map_m$Type=="PRE OP")],na.rm = T) #92

t.test(map_l$Rate[which(map_l$Type=="PRE OP")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.62433, df = 57.133, p-value = 0.5349 hence insignificant difference

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="PRE OP")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1, significant
t.test(map_m$Rate[which(map_m$Type=="PRE OP")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1, significant

#===============================  INDUCTION  ===========================#

mean(map_l$Rate[which(map_l$Type=="INDUCTION")],na.rm = T) #87.63
mean(map_m$Rate[which(map_m$Type=="INDUCTION")],na.rm = T) #86.1

sd(map_l$Rate[which(map_l$Type=="INDUCTION")],na.rm = T) #10.26
sd(map_m$Rate[which(map_m$Type=="INDUCTION")],na.rm = T) #7.63

median(map_l$Rate[which(map_l$Type=="INDUCTION")],na.rm = T) #85
median(map_m$Rate[which(map_m$Type=="INDUCTION")],na.rm = T) #85

t.test(map_l$Rate[which(map_l$Type=="INDUCTION")],map_m$Rate[which(map_m$Type=="INDUCTION")],conf.level = 0.95)
#t = 0.65638, df = 53.588, p-value = 0.5144 , hence insignificant difference

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="INDUCTION")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.8506, df = 51.231, p-value = 0.07, significant
t.test(map_m$Rate[which(map_m$Type=="INDUCTION")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.5985, df = 55.629, p-value = 0.01196, significant

#=========================  INTUBATION  ==============================#

mean(map_l$Rate[which(map_l$Type=="INTUBATION")],na.rm = T) #101.9
mean(map_m$Rate[which(map_m$Type=="INTUBATION")],na.rm = T) #100

sd(map_l$Rate[which(map_l$Type=="INTUBATION")],na.rm = T) #11.9
sd(map_m$Rate[which(map_m$Type=="INTUBATION")],na.rm = T) #8.21

median(map_l$Rate[which(map_l$Type=="INTUBATION")],na.rm = T) #99
median(map_m$Rate[which(map_m$Type=="INTUBATION")],na.rm = T) #99

t.test(map_l$Rate[which(map_l$Type=="INTUBATION")],map_m$Rate[which(map_m$Type=="INTUBATION")],conf.level = 0.95)
#t = 0.71951, df = 51.525, p-value = 0.4751 , hence insignificant difference

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="INTUBATION")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.9911, df = 46.968, p-value = 0.0002293, significant
t.test(map_m$Rate[which(map_m$Type=="INTUBATION")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 4.9142, df = 53.922, p-value = 8.701e-06, significant

#=================    5 mins ==============================#

mean(map_l$Rate[which(map_l$Type=="5 MINS")],na.rm = T) #88.9
mean(map_m$Rate[which(map_m$Type=="5 MINS")],na.rm = T) #85.46

sd(map_l$Rate[which(map_l$Type=="5 MINS")],na.rm = T) #7.88
sd(map_m$Rate[which(map_m$Type=="5 MINS")],na.rm = T) #6.52

median(map_l$Rate[which(map_l$Type=="5 MINS")],na.rm = T) #88.5
median(map_m$Rate[which(map_m$Type=="5 MINS")],na.rm = T) #85

t.test(map_l$Rate[which(map_l$Type=="5 MINS")],map_m$Rate[which(map_m$Type=="5 MINS")],conf.level = 0.95)
# t = 1.8379, df = 56.031, p-value = 0.07139 , Insignificant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="5 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.5226, df = 57.222, p-value = 0.1334, significant
t.test(map_m$Rate[which(map_m$Type=="5 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.227, df = 57.848, p-value = 0.002061, significant

#============================  10 MINS  ==================================#

mean(map_l$Rate[which(map_l$Type=="10 MINS")],na.rm = T) #83.2
mean(map_m$Rate[which(map_m$Type=="10 MINS")],na.rm = T) #83

sd(map_l$Rate[which(map_l$Type=="10 MINS")],na.rm = T) #5.9
sd(map_m$Rate[which(map_m$Type=="10 MINS")],na.rm = T) #4.73

median(map_l$Rate[which(map_l$Type=="10 MINS")],na.rm = T) #82
median(map_m$Rate[which(map_m$Type=="10 MINS")],na.rm = T) #82

t.test(map_l$Rate[which(map_l$Type=="10 MINS")],map_m$Rate[which(map_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.14467, df = 55.364, p-value = 0.8855 , Insignificant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="10 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.1563, df = 56.38, p-value = 3.367e-06, significant
t.test(map_m$Rate[which(map_m$Type=="10 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.4554, df = 54.254, p-value = 1.238e-06, significant

#=============================  15 MINS  ==================================#

mean(map_l$Rate[which(map_l$Type=="15 MINS")],na.rm = T) #81.66
mean(map_m$Rate[which(map_m$Type=="15 MINS")],na.rm = T) #78.9

sd(map_l$Rate[which(map_l$Type=="15 MINS")],na.rm = T) #9.1
sd(map_m$Rate[which(map_m$Type=="15 MINS")],na.rm = T) #7.34

median(map_l$Rate[which(map_l$Type=="15 MINS")],na.rm = T) #81.5
median(map_m$Rate[which(map_m$Type=="15 MINS")],na.rm = T) #80

t.test(map_l$Rate[which(map_l$Type=="15 MINS")],map_m$Rate[which(map_m$Type=="15 MINS")],conf.level = 0.95)
#t = 1.2956, df = 55.523, p-value = 0.2005 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="15 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.8462, df = 54.458, p-value = 1.086e-05, significant
t.test(map_m$Rate[which(map_m$Type=="15 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -6.7636, df = 56.398, p-value = 8.244e-09, significant

#==========================  30 MINS  ======================================#

mean(map_l$Rate[which(map_l$Type=="30 MINS")],na.rm = T) #79.06
mean(map_m$Rate[which(map_m$Type=="30 MINS")],na.rm = T) #78.03

sd(map_l$Rate[which(map_l$Type=="30 MINS")],na.rm = T) #10.46
sd(map_m$Rate[which(map_m$Type=="30 MINS")],na.rm = T) #8.36

median(map_l$Rate[which(map_l$Type=="30 MINS")],na.rm = T) #75
median(map_m$Rate[which(map_m$Type=="30 MINS")],na.rm = T) #75

t.test(map_l$Rate[which(map_l$Type=="30 MINS")],map_m$Rate[which(map_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0.42255, df = 55.303, p-value = 0.6743 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="30 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -5.5509, df = 50.675, p-value = 1.044e-06, significant
t.test(map_m$Rate[which(map_m$Type=="30 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -6.7021, df = 53.474, p-value = 1.308e-08, significant

#=============================  45 MINS  ===================================#

mean(map_l$Rate[which(map_l$Type=="45 MINS")],na.rm = T) #81.8
mean(map_m$Rate[which(map_m$Type=="45 MINS")],na.rm = T) #80.1

sd(map_l$Rate[which(map_l$Type=="45 MINS")],na.rm = T) #11.25
sd(map_m$Rate[which(map_m$Type=="45 MINS")],na.rm = T) #9.24

median(map_l$Rate[which(map_l$Type=="45 MINS")],na.rm = T) #77
median(map_m$Rate[which(map_m$Type=="45 MINS")],na.rm = T) #78.5

t.test(map_l$Rate[which(map_l$Type=="45 MINS")],map_m$Rate[which(map_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0.63922, df = 55.875, p-value = 0.5253 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="45 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.1428, df = 48.555, p-value = 0.0001365, significant
t.test(map_m$Rate[which(map_m$Type=="45 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.2509, df = 50.69, p-value = 3.013e-06, significant

#===============================  60 MINS  ================================#

mean(map_l$Rate[which(map_l$Type=="60 MINS")],na.rm = T) #79.93
mean(map_m$Rate[which(map_m$Type=="60 MINS")],na.rm = T) #79.53

sd(map_l$Rate[which(map_l$Type=="60 MINS")],na.rm = T) #12.91
sd(map_m$Rate[which(map_m$Type=="60 MINS")],na.rm = T) #10.44

median(map_l$Rate[which(map_l$Type=="60 MINS")],na.rm = T) #75.5
median(map_m$Rate[which(map_m$Type=="60 MINS")],na.rm = T) #78

t.test(map_l$Rate[which(map_l$Type=="60 MINS")],map_m$Rate[which(map_m$Type=="60 MINS")],conf.level = 0.95)
#t = 0.1319, df = 55.578, p-value = 0.8955 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="60 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -4.4356, df = 44.736, p-value = 5.913e-05, significant
t.test(map_m$Rate[which(map_m$Type=="60 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.0653, df = 47.154, p-value = 6.697e-06, significant

#=============================  75 MINS ===================================#

mean(map_l$Rate[which(map_l$Type=="75 MINS")],na.rm = T) #84.43
mean(map_m$Rate[which(map_m$Type=="75 MINS")],na.rm = T) #81.33

sd(map_l$Rate[which(map_l$Type=="75 MINS")],na.rm = T) #9.36
sd(map_m$Rate[which(map_m$Type=="75 MINS")],na.rm = T) #7.32

median(map_l$Rate[which(map_l$Type=="75 MINS")],na.rm = T) #83
median(map_m$Rate[which(map_m$Type=="75 MINS")],na.rm = T) #81

t.test(map_l$Rate[which(map_l$Type=="75 MINS")],map_m$Rate[which(map_m$Type=="75 MINS")],conf.level = 0.95)
#t = 1.428, df = 54.8, p-value = 0.159 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="75 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.4635, df = 53.733, p-value = 0.001055, significant
t.test(map_m$Rate[which(map_m$Type=="75 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.3868, df = 56.455, p-value = 1.448e-06, significant

#============================== 90 MINS  ==================================#

mean(map_l$Rate[which(map_l$Type=="90 MINS")],na.rm = T) #83.76
mean(map_m$Rate[which(map_m$Type=="90 MINS")],na.rm = T) #83.23

sd(map_l$Rate[which(map_l$Type=="90 MINS")],na.rm = T) #10.79
sd(map_m$Rate[which(map_m$Type=="90 MINS")],na.rm = T) #10.09

median(map_l$Rate[which(map_l$Type=="90 MINS")],na.rm = T) #81
median(map_m$Rate[which(map_m$Type=="90 MINS")],na.rm = T) #81

t.test(map_l$Rate[which(map_l$Type=="90 MINS")],map_m$Rate[which(map_m$Type=="90 MINS")],conf.level = 0.95)
#t = 0.19771, df = 57.741, p-value = 0.844 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="90 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.433, df = 49.785, p-value = 0.001211, significant
t.test(map_m$Rate[which(map_m$Type=="90 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.4844, df = 48.144, p-value = 0.001062, significant

#===================================  105 MINS  ==================================#

mean(map_l$Rate[which(map_l$Type=="105 MINS")],na.rm = T) #85.66
mean(map_m$Rate[which(map_m$Type=="105 MINS")],na.rm = T) #83.23

sd(map_l$Rate[which(map_l$Type=="105 MINS")],na.rm = T) #11.03
sd(map_m$Rate[which(map_m$Type=="105 MINS")],na.rm = T) #9.46

median(map_l$Rate[which(map_l$Type=="105 MINS")],na.rm = T) #81
median(map_m$Rate[which(map_m$Type=="105 MINS")],na.rm = T) #80.5

t.test(map_l$Rate[which(map_l$Type=="105 MINS")],map_m$Rate[which(map_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0.91671, df = 56.68, p-value = 0.3632 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="105 MINS")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.583, df = 49.134, p-value = 0.01282, significant
t.test(map_m$Rate[which(map_m$Type=="105 MINS")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.6477, df = 50.003, p-value = 0.0006309, significant

#=====================================  120 MINS  ===============================#

mean(map_l$Rate[which(map_l$Type=="120 MIN")],na.rm = T) #85.24
mean(map_m$Rate[which(map_m$Type=="120 MIN")],na.rm = T) #82.5

sd(map_l$Rate[which(map_l$Type=="120 MIN")],na.rm = T) #13.09
sd(map_m$Rate[which(map_m$Type=="120 MIN")],na.rm = T) #11.03

median(map_l$Rate[which(map_l$Type=="120 MIN")],na.rm = T) #82
median(map_m$Rate[which(map_m$Type=="120 MIN")],na.rm = T) #82

t.test(map_l$Rate[which(map_l$Type=="120 MIN")],map_m$Rate[which(map_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0.86822, df = 54.738, p-value = 0.3891 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="120 MIN")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.3991, df = 42.527, p-value = 0.02089, significant
t.test(map_m$Rate[which(map_m$Type=="120 MIN")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.5777, df = 45.632, p-value = 0.0008348, significant

#====================================  135 MIN  =======================================#

mean(map_l$Rate[which(map_l$Type=="135 MIN")],na.rm = T) #84.96
mean(map_m$Rate[which(map_m$Type=="135 MIN")],na.rm = T) #82.06

sd(map_l$Rate[which(map_l$Type=="135 MIN")],na.rm = T) #13.27
sd(map_m$Rate[which(map_m$Type=="135 MIN")],na.rm = T) #10.65

median(map_l$Rate[which(map_l$Type=="135 MIN")],na.rm = T) #84
median(map_m$Rate[which(map_m$Type=="135 MIN")],na.rm = T) #83

t.test(map_l$Rate[which(map_l$Type=="135 MIN")],map_m$Rate[which(map_m$Type=="135 MIN")],conf.level = 0.95)
#t = 0.92324, df = 53.647, p-value = 0.36 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="135 MIN")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.4727, df = 42.184, p-value = 0.01752, significant
t.test(map_m$Rate[which(map_m$Type=="135 MIN")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -3.8663, df = 46.603, p-value = 0.0003411, significant

#======================================  150 MIN  ====================================#

mean(map_l$Rate[which(map_l$Type=="150 MIN")],na.rm = T) #83.82
mean(map_m$Rate[which(map_m$Type=="150 MIN")],na.rm = T) #81.23

sd(map_l$Rate[which(map_l$Type=="150 MIN")],na.rm = T) #8.95
sd(map_m$Rate[which(map_m$Type=="150 MIN")],na.rm = T) #7.2

median(map_l$Rate[which(map_l$Type=="150 MIN")],na.rm = T) #80
median(map_m$Rate[which(map_m$Type=="150 MIN")],na.rm = T) #80

t.test(map_l$Rate[which(map_l$Type=="150 MIN")],map_m$Rate[which(map_m$Type=="150 MIN")],conf.level = 0.95)
#t = 1.2238, df = 53.709, p-value = 0.2264 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="150 MIN")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.8153, df = 53.047, p-value = 0.000357, significant
t.test(map_m$Rate[which(map_m$Type=="150 MIN")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -5.4949, df = 56.729, p-value = 9.617e-07, significant

#=======================================  165 MIN  ==================================#

mean(map_l$Rate[which(map_l$Type=="165 MIN")],na.rm = T) #84.34
mean(map_m$Rate[which(map_m$Type=="165 MIN")],na.rm = T) #80.96

sd(map_l$Rate[which(map_l$Type=="165 MIN")],na.rm = T) #9.54
sd(map_m$Rate[which(map_m$Type=="165 MIN")],na.rm = T) #9.42

median(map_l$Rate[which(map_l$Type=="165 MIN")],na.rm = T) #86
median(map_m$Rate[which(map_m$Type=="165 MIN")],na.rm = T) #80.5

t.test(map_l$Rate[which(map_l$Type=="165 MIN")],map_m$Rate[which(map_m$Type=="165 MIN")],conf.level = 0.95)
#t = 1.3674, df = 56.875, p-value = 0.1769 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="165 MIN")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -3.425, df = 51.348, p-value = 0.001217, significant
t.test(map_m$Rate[which(map_m$Type=="165 MIN")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -4.7583, df = 50.117, p-value = 1.696e-05, significant

#=========================================  180 MIN  ================================#

mean(map_l$Rate[which(map_l$Type=="180 MIN")],na.rm = T) #85.07
mean(map_m$Rate[which(map_m$Type=="180 MIN")],na.rm = T) #84.51

sd(map_l$Rate[which(map_l$Type=="180 MIN")],na.rm = T) #10.44
sd(map_m$Rate[which(map_m$Type=="180 MIN")],na.rm = T) #10.1

median(map_l$Rate[which(map_l$Type=="180 MIN")],na.rm = T) #86
median(map_m$Rate[which(map_m$Type=="180 MIN")],na.rm = T) #84

t.test(map_l$Rate[which(map_l$Type=="180 MIN")],map_m$Rate[which(map_m$Type=="180 MIN")],conf.level = 0.95)
#t = 0.20258, df = 53.404, p-value = 0.8402 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="180 MIN")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.8371, df = 44.783, p-value = 0.006815, significant
t.test(map_m$Rate[which(map_m$Type=="180 MIN")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.8529, df = 46.173, p-value = 0.006462, significant

#========================================  0 MIN_Emergence  ==========================#

mean(map_l$Rate[which(map_l$Type=="0 MIN_Emergence")],na.rm = T) #89.03
mean(map_m$Rate[which(map_m$Type=="0 MIN_Emergence")],na.rm = T) #87.46

sd(map_l$Rate[which(map_l$Type=="0 MIN_Emergence")],na.rm = T) #10.15
sd(map_m$Rate[which(map_m$Type=="0 MIN_Emergence")],na.rm = T) #9.78

median(map_l$Rate[which(map_l$Type=="0 MIN_Emergence")],na.rm = T) #88.5
median(map_m$Rate[which(map_m$Type=="0 MIN_Emergence")],na.rm = T) #85

t.test(map_l$Rate[which(map_l$Type=="0 MIN_Emergence")],map_m$Rate[which(map_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 0.60843, df = 57.921, p-value = 0.5453 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="0 MIN_Emergence")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.2426, df = 51.533, p-value = 0.2196, significant
t.test(map_m$Rate[which(map_m$Type=="0 MIN_Emergence")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.5605, df = 49.031, p-value = 0.1251, significant

#========================================  1 MIN_Emergence  ============================#

mean(map_l$Rate[which(map_l$Type=="1 MIN_Emergence")],na.rm = T) #91.9
mean(map_m$Rate[which(map_m$Type=="1 MIN_Emergence")],na.rm = T) #89.93

sd(map_l$Rate[which(map_l$Type=="1 MIN_Emergence")],na.rm = T) #10.02
sd(map_m$Rate[which(map_m$Type=="1 MIN_Emergence")],na.rm = T) #10.34

median(map_l$Rate[which(map_l$Type=="1 MIN_Emergence")],na.rm = T) #92
median(map_m$Rate[which(map_m$Type=="1 MIN_Emergence")],na.rm = T) #89

t.test(map_l$Rate[which(map_l$Type=="1 MIN_Emergence")],map_m$Rate[which(map_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 0.7476, df = 57.944, p-value = 0.4577 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="1 MIN_Emergence")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0.029839, df = 51.888, p-value = 0.9763, significant
t.test(map_m$Rate[which(map_m$Type=="1 MIN_Emergence")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.3785, df = 47.433, p-value = 0.7067, significant

#=========================================  2 MIN_Emergence  ===============================#

mean(map_l$Rate[which(map_l$Type=="2 MIN_Emergence")],na.rm = T) #98.03
mean(map_m$Rate[which(map_m$Type=="2 MIN_Emergence")],na.rm = T) #96.3

sd(map_l$Rate[which(map_l$Type=="2 MIN_Emergence")],na.rm = T) #15.25
sd(map_m$Rate[which(map_m$Type=="2 MIN_Emergence")],na.rm = T) #13.86

median(map_l$Rate[which(map_l$Type=="2 MIN_Emergence")],na.rm = T) #99
median(map_m$Rate[which(map_m$Type=="2 MIN_Emergence")],na.rm = T) #94.5

t.test(map_l$Rate[which(map_l$Type=="2 MIN_Emergence")],map_m$Rate[which(map_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 0.46022, df = 57.495, p-value = 0.6471 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="2 MIN_Emergence")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.0226, df = 40.732, p-value = 0.04972, significant
t.test(map_m$Rate[which(map_m$Type=="2 MIN_Emergence")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.9931, df = 40.107, p-value = 0.05309, significant

#==========================================  3 MIN_Emergence  ================================#

mean(map_l$Rate[which(map_l$Type=="3 MIN_Emergence")],na.rm = T) #99.1
mean(map_m$Rate[which(map_m$Type=="3 MIN_Emergence")],na.rm = T) #98.23

sd(map_l$Rate[which(map_l$Type=="3 MIN_Emergence")],na.rm = T) #13.55
sd(map_m$Rate[which(map_m$Type=="3 MIN_Emergence")],na.rm = T) #11.4

median(map_l$Rate[which(map_l$Type=="3 MIN_Emergence")],na.rm = T) #97
median(map_m$Rate[which(map_m$Type=="3 MIN_Emergence")],na.rm = T) #96.5

t.test(map_l$Rate[which(map_l$Type=="3 MIN_Emergence")],map_m$Rate[which(map_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 0.26794, df = 56.355, p-value = 0.7897 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="3 MIN_Emergence")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.6079, df = 43.485, p-value = 0.01244, significant
t.test(map_m$Rate[which(map_m$Type=="3 MIN_Emergence")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.1505, df = 44.743, p-value = 0.002905 significant

#===========================================  4 MIN_Emergence  =================================#

mean(map_l$Rate[which(map_l$Type=="4 MIN_Emergence")],na.rm = T) #102.86
mean(map_m$Rate[which(map_m$Type=="4 MIN_Emergence")],na.rm = T) #100.76

sd(map_l$Rate[which(map_l$Type=="4 MIN_Emergence")],na.rm = T) #11.92
sd(map_m$Rate[which(map_m$Type=="4 MIN_Emergence")],na.rm = T) #11.16

median(map_l$Rate[which(map_l$Type=="4 MIN_Emergence")],na.rm = T) #99
median(map_m$Rate[which(map_m$Type=="4 MIN_Emergence")],na.rm = T) #98

t.test(map_l$Rate[which(map_l$Type=="4 MIN_Emergence")],map_m$Rate[which(map_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 0.70402, df = 57.749, p-value = 0.4842 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="4 MIN_Emergence")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 4.3677, df = 46.91, p-value = 6.885e-05, significant
t.test(map_m$Rate[which(map_m$Type=="4 MIN_Emergence")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 4.2893, df = 45.314, p-value = 9.286e-05 significant

#============================================  5 MIN_Emergence  ===================================#

mean(map_l$Rate[which(map_l$Type=="5 MIN_Emergence")],na.rm = T) #103.66
mean(map_m$Rate[which(map_m$Type=="5 MIN_Emergence")],na.rm = T) #102.6

sd(map_l$Rate[which(map_l$Type=="5 MIN_Emergence")],na.rm = T) #8.96
sd(map_m$Rate[which(map_m$Type=="5 MIN_Emergence")],na.rm = T) #7.28

median(map_l$Rate[which(map_l$Type=="5 MIN_Emergence")],na.rm = T) #103
median(map_m$Rate[which(map_m$Type=="5 MIN_Emergence")],na.rm = T) #102

t.test(map_l$Rate[which(map_l$Type=="5 MIN_Emergence")],map_m$Rate[which(map_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = 0.47987, df = 50.106, p-value = 0.6334 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="5 MIN_Emergence")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.5075, df = 49.154, p-value = 1.319e-06, significant
t.test(map_m$Rate[which(map_m$Type=="5 MIN_Emergence")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 6.6425, df = 53.178, p-value = 1.672e-08 significant

#=============================================  6 MIN_Emergence  =====================================#

mean(map_l$Rate[which(map_l$Type=="6 MIN_Emergence")],na.rm = T) #106.72
mean(map_m$Rate[which(map_m$Type=="6 MIN_Emergence")],na.rm = T) #106.33

sd(map_l$Rate[which(map_l$Type=="6 MIN_Emergence")],na.rm = T) #6.52
sd(map_m$Rate[which(map_m$Type=="6 MIN_Emergence")],na.rm = T) #6.77

median(map_l$Rate[which(map_l$Type=="6 MIN_Emergence")],na.rm = T) #107
median(map_m$Rate[which(map_m$Type=="6 MIN_Emergence")],na.rm = T) #109.5

t.test(map_l$Rate[which(map_l$Type=="6 MIN_Emergence")],map_m$Rate[which(map_m$Type=="6 MIN_Emergence")],conf.level = 0.95)
#t = 0.14199, df = 20.938, p-value = 0.8884 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="6 MIN_Emergence")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 6.3429, df = 19.075, p-value = 4.289e-06, significant
t.test(map_m$Rate[which(map_m$Type=="6 MIN_Emergence")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 6.891, df = 18.798, p-value = 1.512e-06 significant

#============================================  0 MIN_Recovery  ========================================#

mean(map_l$Rate[which(map_l$Type=="0 MIN_Recovery")],na.rm = T) #105.9
mean(map_m$Rate[which(map_m$Type=="0 MIN_Recovery")],na.rm = T) #103.2

sd(map_l$Rate[which(map_l$Type=="0 MIN_Recovery")],na.rm = T) #11.54
sd(map_m$Rate[which(map_m$Type=="0 MIN_Recovery")],na.rm = T) #9.9

median(map_l$Rate[which(map_l$Type=="0 MIN_Recovery")],na.rm = T) #107
median(map_m$Rate[which(map_m$Type=="0 MIN_Recovery")],na.rm = T) #100

t.test(map_l$Rate[which(map_l$Type=="0 MIN_Recovery")],map_m$Rate[which(map_m$Type=="0 MIN_Recovery")],conf.level = 0.95)
#t = 0.9724, df = 56.684, p-value = 0.335 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="0 MIN_Recovery")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 5.7041, df = 47.838, p-value = 7.146e-07, significant
t.test(map_m$Rate[which(map_m$Type=="0 MIN_Recovery")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 5.8306, df = 48.695, p-value = 4.348e-07 significant

#============================================  15 MIN_Recovery  =========================================#

mean(map_l$Rate[which(map_l$Type=="15 MIN_Recovery")],na.rm = T) #95.7
mean(map_m$Rate[which(map_m$Type=="15 MIN_Recovery")],na.rm = T) #92.96

sd(map_l$Rate[which(map_l$Type=="15 MIN_Recovery")],na.rm = T) #7.42
sd(map_m$Rate[which(map_m$Type=="15 MIN_Recovery")],na.rm = T) #6.8

median(map_l$Rate[which(map_l$Type=="15 MIN_Recovery")],na.rm = T) #96
median(map_m$Rate[which(map_m$Type=="15 MIN_Recovery")],na.rm = T) #96

t.test(map_l$Rate[which(map_l$Type=="15 MIN_Recovery")],map_m$Rate[which(map_m$Type=="15 MIN_Recovery")],conf.level = 0.95)
#t = 1.4861, df = 57.56, p-value = 0.1427 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="15 MIN_Recovery")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.0731, df = 57.809, p-value = 0.04263, significant
t.test(map_m$Rate[which(map_m$Type=="15 MIN_Recovery")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.3093, df = 57.498, p-value = 0.1956 significant

#============================================  30 MIN_Recovery  ========================================#

mean(map_l$Rate[which(map_l$Type=="30 MIN_Recovery")],na.rm = T) #91.43
mean(map_m$Rate[which(map_m$Type=="30 MIN_Recovery")],na.rm = T) #91.33

sd(map_l$Rate[which(map_l$Type=="30 MIN_Recovery")],na.rm = T) #5.3
sd(map_m$Rate[which(map_m$Type=="30 MIN_Recovery")],na.rm = T) #5.14

median(map_l$Rate[which(map_l$Type=="30 MIN_Recovery")],na.rm = T) #91
median(map_m$Rate[which(map_m$Type=="30 MIN_Recovery")],na.rm = T) #90

t.test(map_l$Rate[which(map_l$Type=="30 MIN_Recovery")],map_m$Rate[which(map_m$Type=="30 MIN_Recovery")],conf.level = 0.95)
#t = 0.07411, df = 57.941, p-value = 0.9412 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="30 MIN_Recovery")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -0.24909, df = 54.022, p-value = 0.8042, significant
t.test(map_m$Rate[which(map_m$Type=="30 MIN_Recovery")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0.38548, df = 56.092, p-value = 0.7013 significant

#==============================================  45 MIN_Recovery  ======================================#

mean(map_l$Rate[which(map_l$Type=="45 MIN_Recovery")],na.rm = T) #89.23
mean(map_m$Rate[which(map_m$Type=="45 MIN_Recovery")],na.rm = T) #89.33

sd(map_l$Rate[which(map_l$Type=="45 MIN_Recovery")],na.rm = T) #4.49
sd(map_m$Rate[which(map_m$Type=="45 MIN_Recovery")],na.rm = T) #5.26

median(map_l$Rate[which(map_l$Type=="45 MIN_Recovery")],na.rm = T) #90
median(map_m$Rate[which(map_m$Type=="45 MIN_Recovery")],na.rm = T) #91

t.test(map_l$Rate[which(map_l$Type=="45 MIN_Recovery")],map_m$Rate[which(map_m$Type=="45 MIN_Recovery")],conf.level = 0.95)
#t = -0.07917, df = 56.613, p-value = 0.9372 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="45 MIN_Recovery")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.7099, df = 49.374, p-value = 0.09356, significant
t.test(map_m$Rate[which(map_m$Type=="45 MIN_Recovery")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.96584, df = 56.514, p-value = 0.3382 significant

#==============================================  60 MIN_Recovery  =======================================#

mean(map_l$Rate[which(map_l$Type=="60 MIN_Recovery")],na.rm = T) #88.36
mean(map_m$Rate[which(map_m$Type=="60 MIN_Recovery")],na.rm = T) #89.3

sd(map_l$Rate[which(map_l$Type=="60 MIN_Recovery")],na.rm = T) #5.01
sd(map_m$Rate[which(map_m$Type=="60 MIN_Recovery")],na.rm = T) #5.28

median(map_l$Rate[which(map_l$Type=="60 MIN_Recovery")],na.rm = T) #89
median(map_m$Rate[which(map_m$Type=="60 MIN_Recovery")],na.rm = T) #92

t.test(map_l$Rate[which(map_l$Type=="60 MIN_Recovery")],map_m$Rate[which(map_m$Type=="60 MIN_Recovery")],conf.level = 0.95)
#t = -0.70168, df = 57.839, p-value = 0.4857 not significant

#Intragroup
t.test(map_l$Rate[which(map_l$Type=="60 MIN_Recovery")],map_l$Rate[which(map_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.2026, df = 52.505, p-value = 0.03204, significant
t.test(map_m$Rate[which(map_m$Type=="60 MIN_Recovery")],map_m$Rate[which(map_m$Type=="PRE OP")],conf.level = 0.95)
#t = -0.98634, df = 56.595, p-value = 0.3282 significant

#####################################################  SPO2  #######################################################################

spo2_rate<-read_excel("combined.xlsx",sheet = 5)
sum(is.na(spo2_rate))
str(spo2_rate)

#Subsetting the two study groups
spo2_l<-subset(spo2_rate,spo2_rate$`STUDY GROUP`=="L")
spo2_m<-subset(spo2_rate,spo2_rate$`STUDY GROUP`=="M")

#===============================  PREOP  ===============================#

mean(spo2_l$Rate[which(spo2_l$Type=="PRE OP")],na.rm = T) #99.33
mean(spo2_m$Rate[which(spo2_m$Type=="PRE OP")],na.rm = T) #99.33

sd(spo2_l$Rate[which(spo2_l$Type=="PRE OP")],na.rm = T) #0.95
sd(spo2_m$Rate[which(spo2_m$Type=="PRE OP")],na.rm = T) #0.95

median(spo2_l$Rate[which(spo2_l$Type=="PRE OP")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="PRE OP")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="PRE OP")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 hence insignificant difference

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="PRE OP")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="PRE OP")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#===============================  INDUCTION  ===========================#

mean(spo2_l$Rate[which(spo2_l$Type=="INDUCTION")],na.rm = T) #99.66
mean(spo2_m$Rate[which(spo2_m$Type=="INDUCTION")],na.rm = T) #99.66

sd(spo2_l$Rate[which(spo2_l$Type=="INDUCTION")],na.rm = T) #0.47
sd(spo2_m$Rate[which(spo2_m$Type=="INDUCTION")],na.rm = T) #0.47

median(spo2_l$Rate[which(spo2_l$Type=="INDUCTION")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="INDUCTION")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="INDUCTION")],spo2_m$Rate[which(spo2_m$Type=="INDUCTION")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 , hence insignificant difference

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="INDUCTION")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="INDUCTION")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant

#=========================  INTUBATION  ==============================#

mean(spo2_l$Rate[which(spo2_l$Type=="INTUBATION")],na.rm = T) #99
mean(spo2_m$Rate[which(spo2_m$Type=="INTUBATION")],na.rm = T) #99

sd(spo2_l$Rate[which(spo2_l$Type=="INTUBATION")],na.rm = T) #1.43
sd(spo2_m$Rate[which(spo2_m$Type=="INTUBATION")],na.rm = T) #1.43

median(spo2_l$Rate[which(spo2_l$Type=="INTUBATION")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="INTUBATION")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="INTUBATION")],spo2_m$Rate[which(spo2_m$Type=="INTUBATION")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 , hence insignificant difference

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="INTUBATION")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.0561, df = 50.526, p-value = 0.2959, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="INTUBATION")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.0561, df = 50.526, p-value = 0.2959 significant

#=================    5 mins ==============================#

mean(spo2_l$Rate[which(spo2_l$Type=="5 MINS")],na.rm = T) #100
mean(spo2_m$Rate[which(spo2_m$Type=="5 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="5 MINS")],na.rm = T) #0
sd(spo2_m$Rate[which(spo2_m$Type=="5 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="5 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="5 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="5 MINS")],spo2_m$Rate[which(spo2_m$Type=="5 MINS")],conf.level = 0.95)
# t = 0, df = 58, p-value = 1 , Insignificant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="5 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="5 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#============================  10 MINS  ==================================#

mean(spo2_l$Rate[which(spo2_l$Type=="10 MINS")],na.rm = T) #100
mean(spo2_m$Rate[which(spo2_m$Type=="10 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="10 MINS")],na.rm = T) #0
sd(spo2_m$Rate[which(spo2_m$Type=="10 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="10 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="10 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="10 MINS")],spo2_m$Rate[which(spo2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 , Insignificant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="10 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="10 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#=============================  15 MINS  ==================================#

mean(spo2_l$Rate[which(spo2_l$Type=="15 MINS")],na.rm = T) #99.66
mean(spo2_m$Rate[which(spo2_m$Type=="15 MINS")],na.rm = T) #99.66

sd(spo2_l$Rate[which(spo2_l$Type=="15 MINS")],na.rm = T) #0.47
sd(spo2_m$Rate[which(spo2_m$Type=="15 MINS")],na.rm = T) #0.47

median(spo2_l$Rate[which(spo2_l$Type=="15 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="15 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="15 MINS")],spo2_m$Rate[which(spo2_m$Type=="15 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="15 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="15 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant

#==========================  30 MINS  ======================================#

mean(spo2_l$Rate[which(spo2_l$Type=="30 MINS")],na.rm = T) #100
mean(spo2_m$Rate[which(spo2_m$Type=="30 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="30 MINS")],na.rm = T) #0
sd(spo2_m$Rate[which(spo2_m$Type=="30 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="30 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="30 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="30 MINS")],spo2_m$Rate[which(spo2_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="30 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="30 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#=============================  45 MINS  ===================================#

mean(spo2_l$Rate[which(spo2_l$Type=="45 MINS")],na.rm = T) #99.76
mean(spo2_m$Rate[which(spo2_m$Type=="45 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="45 MINS")],na.rm = T) #0.5
sd(spo2_m$Rate[which(spo2_m$Type=="45 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="45 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="45 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="45 MINS")],spo2_m$Rate[which(spo2_m$Type=="45 MINS")],conf.level = 0.95)
#t = -2.5357, df = 29, p-value = 0.01687  significant
####################################################################################################################

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="45 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.1909, df = 43.886, p-value = 0.03381, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="45 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#===============================  60 MINS  ================================#

mean(spo2_l$Rate[which(spo2_l$Type=="60 MINS")],na.rm = T) #100
mean(spo2_m$Rate[which(spo2_m$Type=="60 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="60 MINS")],na.rm = T) #0
sd(spo2_m$Rate[which(spo2_m$Type=="60 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="60 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="60 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="60 MINS")],spo2_m$Rate[which(spo2_m$Type=="60 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="60 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="60 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#=============================  75 MINS ===================================#

mean(spo2_l$Rate[which(spo2_l$Type=="75 MINS")],na.rm = T) #99.96
mean(spo2_m$Rate[which(spo2_m$Type=="75 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="75 MINS")],na.rm = T) #0.18
sd(spo2_m$Rate[which(spo2_m$Type=="75 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="75 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="75 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="75 MINS")],spo2_m$Rate[which(spo2_m$Type=="75 MINS")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="75 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.5537, df = 31.1, p-value = 0.001237, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="75 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#============================== 90 MINS  ==================================#

mean(spo2_l$Rate[which(spo2_l$Type=="90 MINS")],na.rm = T) #99.93
mean(spo2_m$Rate[which(spo2_m$Type=="90 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="90 MINS")],na.rm = T) #0.36
sd(spo2_m$Rate[which(spo2_m$Type=="90 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="90 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="90 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="90 MINS")],spo2_m$Rate[which(spo2_m$Type=="90 MINS")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="90 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.2028, df = 37.237, p-value = 0.002787, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="90 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#===================================  105 MINS  ==================================#

mean(spo2_l$Rate[which(spo2_l$Type=="105 MINS")],na.rm = T) #100
mean(spo2_m$Rate[which(spo2_m$Type=="105 MINS")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="105 MINS")],na.rm = T) #0
sd(spo2_m$Rate[which(spo2_m$Type=="105 MINS")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="105 MINS")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="105 MINS")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="105 MINS")],spo2_m$Rate[which(spo2_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="105 MINS")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="105 MINS")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#=====================================  120 MINS  ===============================#

mean(spo2_l$Rate[which(spo2_l$Type=="120 MIN")],na.rm = T) #99.9
mean(spo2_m$Rate[which(spo2_m$Type=="120 MIN")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="120 MIN")],na.rm = T) #0.4
sd(spo2_m$Rate[which(spo2_m$Type=="120 MIN")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="120 MIN")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="120 MIN")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="120 MIN")],spo2_m$Rate[which(spo2_m$Type=="120 MIN")],conf.level = 0.95)
#t = -1.3605, df = 29, p-value = 0.1841 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="120 MIN")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 2.9844, df = 38.915, p-value = 0.00489, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="120 MIN")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#====================================  135 MIN  =======================================#

mean(spo2_l$Rate[which(spo2_l$Type=="135 MIN")],na.rm = T) #99.96
mean(spo2_m$Rate[which(spo2_m$Type=="135 MIN")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="135 MIN")],na.rm = T) #0.18
sd(spo2_m$Rate[which(spo2_m$Type=="135 MIN")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="135 MIN")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="135 MIN")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="135 MIN")],spo2_m$Rate[which(spo2_m$Type=="135 MIN")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="135 MIN")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.5537, df = 31.1, p-value = 0.001237, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="135 MIN")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#======================================  150 MIN  ====================================#

mean(spo2_l$Rate[which(spo2_l$Type=="150 MIN")],na.rm = T) #99.96
mean(spo2_m$Rate[which(spo2_m$Type=="150 MIN")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="150 MIN")],na.rm = T) #0.18
sd(spo2_m$Rate[which(spo2_m$Type=="150 MIN")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="150 MIN")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="150 MIN")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="150 MIN")],spo2_m$Rate[which(spo2_m$Type=="150 MIN")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="150 MIN")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.5537, df = 31.1, p-value = 0.001237, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="150 MIN")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#=======================================  165 MIN  ==================================#

mean(spo2_l$Rate[which(spo2_l$Type=="165 MIN")],na.rm = T) #100
mean(spo2_m$Rate[which(spo2_m$Type=="165 MIN")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="165 MIN")],na.rm = T) #0
sd(spo2_m$Rate[which(spo2_m$Type=="165 MIN")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="165 MIN")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="165 MIN")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="165 MIN")],spo2_m$Rate[which(spo2_m$Type=="165 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="165 MIN")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723, significant
t.test(spo2_m$Rate[which(spo2_m$Type=="165 MIN")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#=========================================  180 MIN  ================================#

mean(spo2_l$Rate[which(spo2_l$Type=="180 MIN")],na.rm = T) #99.66
mean(spo2_m$Rate[which(spo2_m$Type=="180 MIN")],na.rm = T) #99.66

sd(spo2_l$Rate[which(spo2_l$Type=="180 MIN")],na.rm = T) #0.47
sd(spo2_m$Rate[which(spo2_m$Type=="180 MIN")],na.rm = T) #0.47

median(spo2_l$Rate[which(spo2_l$Type=="180 MIN")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="180 MIN")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="180 MIN")],spo2_m$Rate[which(spo2_m$Type=="180 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="180 MIN")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="180 MIN")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant

#========================================  0 MIN_Emergence  ==========================#

mean(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Emergence")],na.rm = T) #99.33
mean(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Emergence")],na.rm = T) #99.33

sd(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Emergence")],na.rm = T) #0.95
sd(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Emergence")],na.rm = T) #0.95

median(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Emergence")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Emergence")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Emergence")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#========================================  1 MIN_Emergence  ============================#

mean(spo2_l$Rate[which(spo2_l$Type=="1 MIN_Emergence")],na.rm = T) #99.33
mean(spo2_m$Rate[which(spo2_m$Type=="1 MIN_Emergence")],na.rm = T) #99.33

sd(spo2_l$Rate[which(spo2_l$Type=="1 MIN_Emergence")],na.rm = T) #0.95
sd(spo2_m$Rate[which(spo2_m$Type=="1 MIN_Emergence")],na.rm = T) #0.95

median(spo2_l$Rate[which(spo2_l$Type=="1 MIN_Emergence")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="1 MIN_Emergence")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="1 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="1 MIN_Emergence")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="1 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(spo2_l$Rate[which(spo2_l$Type=="2 MIN_Emergence")],na.rm = T) #99.66
mean(spo2_m$Rate[which(spo2_m$Type=="2 MIN_Emergence")],na.rm = T) #99.66

sd(spo2_l$Rate[which(spo2_l$Type=="2 MIN_Emergence")],na.rm = T) #0.47
sd(spo2_m$Rate[which(spo2_m$Type=="2 MIN_Emergence")],na.rm = T) #0.47

median(spo2_l$Rate[which(spo2_l$Type=="2 MIN_Emergence")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="2 MIN_Emergence")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="2 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="2 MIN_Emergence")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="2 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant

#==========================================  3 MIN_Emergence  ================================#

mean(spo2_l$Rate[which(spo2_l$Type=="3 MIN_Emergence")],na.rm = T) #99
mean(spo2_m$Rate[which(spo2_m$Type=="3 MIN_Emergence")],na.rm = T) #99

sd(spo2_l$Rate[which(spo2_l$Type=="3 MIN_Emergence")],na.rm = T) #1.43
sd(spo2_m$Rate[which(spo2_m$Type=="3 MIN_Emergence")],na.rm = T) #1.43

median(spo2_l$Rate[which(spo2_l$Type=="3 MIN_Emergence")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="3 MIN_Emergence")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="3 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="3 MIN_Emergence")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.0561, df = 50.526, p-value = 0.2959 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="3 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.0561, df = 50.526, p-value = 0.2959 significant

#===========================================  4 MIN_Emergence  =================================#

mean(spo2_l$Rate[which(spo2_l$Type=="4 MIN_Emergence")],na.rm = T) #99
mean(spo2_m$Rate[which(spo2_m$Type=="4 MIN_Emergence")],na.rm = T) #99

sd(spo2_l$Rate[which(spo2_l$Type=="4 MIN_Emergence")],na.rm = T) #0.83
sd(spo2_m$Rate[which(spo2_m$Type=="4 MIN_Emergence")],na.rm = T) #0.83

median(spo2_l$Rate[which(spo2_l$Type=="4 MIN_Emergence")],na.rm = T) #99
median(spo2_m$Rate[which(spo2_m$Type=="4 MIN_Emergence")],na.rm = T) #99

t.test(spo2_l$Rate[which(spo2_l$Type=="4 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="4 MIN_Emergence")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="4 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant

#============================================  5 MIN_Emergence  ===================================#

mean(spo2_l$Rate[which(spo2_l$Type=="5 MIN_Emergence")],na.rm = T) #98.66
mean(spo2_m$Rate[which(spo2_m$Type=="5 MIN_Emergence")],na.rm = T) #98.66

sd(spo2_l$Rate[which(spo2_l$Type=="5 MIN_Emergence")],na.rm = T) #0.95
sd(spo2_m$Rate[which(spo2_m$Type=="5 MIN_Emergence")],na.rm = T) #0.95

median(spo2_l$Rate[which(spo2_l$Type=="5 MIN_Emergence")],na.rm = T) #98
median(spo2_m$Rate[which(spo2_m$Type=="5 MIN_Emergence")],na.rm = T) #98

t.test(spo2_l$Rate[which(spo2_l$Type=="5 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="5 MIN_Emergence")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = -2.6926, df = 58, p-value = 0.009253 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="5 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = -2.6926, df = 58, p-value = 0.009253 significant

#=============================================  6 MIN_Emergence  =====================================#

mean(spo2_l$Rate[which(spo2_l$Type=="6 MIN_Emergence")],na.rm = T) #99
mean(spo2_m$Rate[which(spo2_m$Type=="6 MIN_Emergence")],na.rm = T) #99

sd(spo2_l$Rate[which(spo2_l$Type=="6 MIN_Emergence")],na.rm = T) #0.83
sd(spo2_m$Rate[which(spo2_m$Type=="6 MIN_Emergence")],na.rm = T) #0.83

median(spo2_l$Rate[which(spo2_l$Type=="6 MIN_Emergence")],na.rm = T) #99
median(spo2_m$Rate[which(spo2_m$Type=="6 MIN_Emergence")],na.rm = T) #99

t.test(spo2_l$Rate[which(spo2_l$Type=="6 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="6 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="6 MIN_Emergence")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="6 MIN_Emergence")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant

#============================================  0 MIN_Recovery  ========================================#

mean(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Recovery")],na.rm = T) #100
mean(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Recovery")],na.rm = T) #100

sd(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Recovery")],na.rm = T) #0
sd(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Recovery")],na.rm = T) #0

median(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Recovery")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Recovery")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="0 MIN_Recovery")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="0 MIN_Recovery")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="0 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant

#============================================  15 MIN_Recovery  =========================================#

mean(spo2_l$Rate[which(spo2_l$Type=="15 MIN_Recovery")],na.rm = T) #99.66
mean(spo2_m$Rate[which(spo2_m$Type=="15 MIN_Recovery")],na.rm = T) #999.66

sd(spo2_l$Rate[which(spo2_l$Type=="15 MIN_Recovery")],na.rm = T) #0.47
sd(spo2_m$Rate[which(spo2_m$Type=="15 MIN_Recovery")],na.rm = T) #0.47

median(spo2_l$Rate[which(spo2_l$Type=="15 MIN_Recovery")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="15 MIN_Recovery")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="15 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="15 MIN_Recovery")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="15 MIN_Recovery")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="15 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant

#============================================  30 MIN_Recovery  ========================================#

mean(spo2_l$Rate[which(spo2_l$Type=="30 MIN_Recovery")],na.rm = T) #99.67
mean(spo2_m$Rate[which(spo2_m$Type=="30 MIN_Recovery")],na.rm = T) #99.67

sd(spo2_l$Rate[which(spo2_l$Type=="30 MIN_Recovery")],na.rm = T) #0.47
sd(spo2_m$Rate[which(spo2_m$Type=="30 MIN_Recovery")],na.rm = T) #0.47

median(spo2_l$Rate[which(spo2_l$Type=="30 MIN_Recovery")],na.rm = T) #100
median(spo2_m$Rate[which(spo2_m$Type=="30 MIN_Recovery")],na.rm = T) #100

t.test(spo2_l$Rate[which(spo2_l$Type=="30 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="30 MIN_Recovery")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="30 MIN_Recovery")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="30 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = 1.7029, df = 42.647, p-value = 0.09586 significant

#==============================================  45 MIN_Recovery  ======================================#

mean(spo2_l$Rate[which(spo2_l$Type=="45 MIN_Recovery")],na.rm = T) #99
mean(spo2_m$Rate[which(spo2_m$Type=="45 MIN_Recovery")],na.rm = T) #99

sd(spo2_l$Rate[which(spo2_l$Type=="45 MIN_Recovery")],na.rm = T) #0.83
sd(spo2_m$Rate[which(spo2_m$Type=="45 MIN_Recovery")],na.rm = T) #0.83

median(spo2_l$Rate[which(spo2_l$Type=="45 MIN_Recovery")],na.rm = T) #99
median(spo2_m$Rate[which(spo2_m$Type=="45 MIN_Recovery")],na.rm = T) #99

t.test(spo2_l$Rate[which(spo2_l$Type=="45 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="45 MIN_Recovery")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="45 MIN_Recovery")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="45 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant

#==============================================  60 MIN_Recovery  =======================================#

mean(spo2_l$Rate[which(spo2_l$Type=="60 MIN_Recovery")],na.rm = T) #99
mean(spo2_m$Rate[which(spo2_m$Type=="60 MIN_Recovery")],na.rm = T) #99

sd(spo2_l$Rate[which(spo2_l$Type=="60 MIN_Recovery")],na.rm = T) #0.83
sd(spo2_m$Rate[which(spo2_m$Type=="60 MIN_Recovery")],na.rm = T) #0.83

median(spo2_l$Rate[which(spo2_l$Type=="60 MIN_Recovery")],na.rm = T) #99
median(spo2_m$Rate[which(spo2_m$Type=="60 MIN_Recovery")],na.rm = T) #99

t.test(spo2_l$Rate[which(spo2_l$Type=="60 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="60 MIN_Recovery")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(spo2_l$Rate[which(spo2_l$Type=="60 MIN_Recovery")],spo2_l$Rate[which(spo2_l$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant
t.test(spo2_m$Rate[which(spo2_m$Type=="60 MIN_Recovery")],spo2_m$Rate[which(spo2_m$Type=="PRE OP")],conf.level = 0.95)
#t = -1.4392, df = 56.84, p-value = 0.1556 significant

###########################################################  ETCO2  #################################################################

etco2_rate<-read_excel("combined.xlsx",sheet = 6)
sum(is.na(etco2_rate))
str(etco2_rate)

#Subsetting the two study groups
etco2_l<-subset(etco2_rate,etco2_rate$`STUDY GROUP`=="L")
etco2_m<-subset(etco2_rate,etco2_rate$`STUDY GROUP`=="M")

#===============================  INDUCTION  ===========================#

mean(etco2_l$Rate[which(etco2_l$Type=="INDUCTION")],na.rm = T) #34.7
mean(etco2_m$Rate[which(etco2_m$Type=="INDUCTION")],na.rm = T) #34.8

sd(etco2_l$Rate[which(etco2_l$Type=="INDUCTION")],na.rm = T) #3.91
sd(etco2_m$Rate[which(etco2_m$Type=="INDUCTION")],na.rm = T) #3.88

median(etco2_l$Rate[which(etco2_l$Type=="INDUCTION")],na.rm = T) #35
median(etco2_m$Rate[which(etco2_m$Type=="INDUCTION")],na.rm = T) #35

t.test(etco2_l$Rate[which(etco2_l$Type=="INDUCTION")],etco2_m$Rate[which(etco2_m$Type=="INDUCTION")],conf.level = 0.95)
#t = -0.099368, df = 57.996, p-value = 0.9212 , hence insignificant difference

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="INDUCTION")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 2.9861, df = 46.519, p-value = 0.004496 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="INDUCTION")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.1335, df = 45.047, p-value = 0.003036 significant

#=========================  INTUBATION  ==============================#

mean(etco2_l$Rate[which(etco2_l$Type=="INTUBATION")],na.rm = T) #37.06
mean(etco2_m$Rate[which(etco2_m$Type=="INTUBATION")],na.rm = T) #36.93

sd(etco2_l$Rate[which(etco2_l$Type=="INTUBATION")],na.rm = T) #3.21
sd(etco2_m$Rate[which(etco2_m$Type=="INTUBATION")],na.rm = T) #3.05

median(etco2_l$Rate[which(etco2_l$Type=="INTUBATION")],na.rm = T) #36
median(etco2_m$Rate[which(etco2_m$Type=="INTUBATION")],na.rm = T) #36

t.test(etco2_l$Rate[which(etco2_l$Type=="INTUBATION")],etco2_m$Rate[which(etco2_m$Type=="INTUBATION")],conf.level = 0.95)
#t = 0.16476, df = 57.84, p-value = 0.8697 , hence insignificant difference

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="INTUBATION")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.7262, df = 52.147, p-value = 1.336e-08 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="INTUBATION")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.8675, df = 51.878, p-value = 8.139e-09 significant

#=================    5 mins ==============================#

mean(etco2_l$Rate[which(etco2_l$Type=="5 MINS")],na.rm = T) #36
mean(etco2_m$Rate[which(etco2_m$Type=="5 MINS")],na.rm = T) #36

sd(etco2_l$Rate[which(etco2_l$Type=="5 MINS")],na.rm = T) #3.05
sd(etco2_m$Rate[which(etco2_m$Type=="5 MINS")],na.rm = T) #2.87

median(etco2_l$Rate[which(etco2_l$Type=="5 MINS")],na.rm = T) #36
median(etco2_m$Rate[which(etco2_m$Type=="5 MINS")],na.rm = T) #36

t.test(etco2_l$Rate[which(etco2_l$Type=="5 MINS")],etco2_m$Rate[which(etco2_m$Type=="5 MINS")],conf.level = 0.95)
# t = 0, df = 57.8, p-value = 1 , Insignificant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="5 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 5.4253, df = 53.568, p-value = 1.424e-06 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="5 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.7104, df = 53.478, p-value = 5.083e-07 significant

#============================  10 MINS  ==================================#

mean(etco2_l$Rate[which(etco2_l$Type=="10 MINS")],na.rm = T) #32.23
mean(etco2_m$Rate[which(etco2_m$Type=="10 MINS")],na.rm = T) #32.26

sd(etco2_l$Rate[which(etco2_l$Type=="10 MINS")],na.rm = T) #2.26
sd(etco2_m$Rate[which(etco2_m$Type=="10 MINS")],na.rm = T) #2.13

median(etco2_l$Rate[which(etco2_l$Type=="10 MINS")],na.rm = T) #32.5
median(etco2_m$Rate[which(etco2_m$Type=="10 MINS")],na.rm = T) #32.5

t.test(etco2_l$Rate[which(etco2_l$Type=="10 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -0.058628, df = 57.776, p-value = 0.9535 , Insignificant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="10 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="10 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=============================  15 MINS  ==================================#

mean(etco2_l$Rate[which(etco2_l$Type=="15 MINS")],na.rm = T) #30.73
mean(etco2_m$Rate[which(etco2_m$Type=="15 MINS")],na.rm = T) #30.76

sd(etco2_l$Rate[which(etco2_l$Type=="15 MINS")],na.rm = T) #1.87
sd(etco2_m$Rate[which(etco2_m$Type=="15 MINS")],na.rm = T) #1.75

median(etco2_l$Rate[which(etco2_l$Type=="15 MINS")],na.rm = T) #31
median(etco2_m$Rate[which(etco2_m$Type=="15 MINS")],na.rm = T) #31

t.test(etco2_l$Rate[which(etco2_l$Type=="15 MINS")],etco2_m$Rate[which(etco2_m$Type=="15 MINS")],conf.level = 0.95)
#t = -0.071098, df = 57.753, p-value = 0.9436 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="15 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -2.7913, df = 55.998, p-value = 0.007166 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="15 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -2.9746, df = 55.936, p-value = 0.004325 significant

#==========================  30 MINS  ======================================#

mean(etco2_l$Rate[which(etco2_l$Type=="30 MINS")],na.rm = T) #29.3
mean(etco2_m$Rate[which(etco2_m$Type=="30 MINS")],na.rm = T) #29.2

sd(etco2_l$Rate[which(etco2_l$Type=="30 MINS")],na.rm = T) #2.38
sd(etco2_m$Rate[which(etco2_m$Type=="30 MINS")],na.rm = T) #2.24

median(etco2_l$Rate[which(etco2_l$Type=="30 MINS")],na.rm = T) #28.5
median(etco2_m$Rate[which(etco2_m$Type=="30 MINS")],na.rm = T) #28.5

t.test(etco2_l$Rate[which(etco2_l$Type=="30 MINS")],etco2_m$Rate[which(etco2_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0.16723, df = 57.817, p-value = 0.8678 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="30 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -4.8853, df = 57.869, p-value = 8.531e-06 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="30 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -5.4186, df = 57.834, p-value = 1.219e-06 significant

#=============================  45 MINS  ===================================#

mean(etco2_l$Rate[which(etco2_l$Type=="45 MINS")],na.rm = T) #28.03
mean(etco2_m$Rate[which(etco2_m$Type=="45 MINS")],na.rm = T) #27.96

sd(etco2_l$Rate[which(etco2_l$Type=="45 MINS")],na.rm = T) #2.84
sd(etco2_m$Rate[which(etco2_m$Type=="45 MINS")],na.rm = T) #2.72

median(etco2_l$Rate[which(etco2_l$Type=="45 MINS")],na.rm = T) #27.5
median(etco2_m$Rate[which(etco2_m$Type=="45 MINS")],na.rm = T) #27.5

t.test(etco2_l$Rate[which(etco2_l$Type=="45 MINS")],etco2_m$Rate[which(etco2_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0.092703, df = 57.886, p-value = 0.9265  significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="45 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -6.3191, df = 55.259, p-value = 4.805e-08 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="45 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -6.8103, df = 54.851, p-value = 7.806e-09 significant

#===============================  60 MINS  ================================#

mean(etco2_l$Rate[which(etco2_l$Type=="60 MINS")],na.rm = T) #27.5
mean(etco2_m$Rate[which(etco2_m$Type=="60 MINS")],na.rm = T) #27.5

sd(etco2_l$Rate[which(etco2_l$Type=="60 MINS")],na.rm = T) #1.16
sd(etco2_m$Rate[which(etco2_m$Type=="60 MINS")],na.rm = T) #1.1

median(etco2_l$Rate[which(etco2_l$Type=="60 MINS")],na.rm = T) #27.5
median(etco2_m$Rate[which(etco2_m$Type=="60 MINS")],na.rm = T) #27.5

t.test(etco2_l$Rate[which(etco2_l$Type=="60 MINS")],etco2_m$Rate[which(etco2_m$Type=="60 MINS")],conf.level = 0.95)
#t = 0, df = 57.835, p-value = 1 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="60 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -10.159, df = 43.336, p-value = 4.881e-13 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="60 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -10.868, df = 43.559, p-value = 5.498e-14 significant

#=============================  75 MINS ===================================#

mean(etco2_l$Rate[which(etco2_l$Type=="75 MINS")],na.rm = T) #27.5
mean(etco2_m$Rate[which(etco2_m$Type=="75 MINS")],na.rm = T) #27.5

sd(etco2_l$Rate[which(etco2_l$Type=="75 MINS")],na.rm = T) #1.16
sd(etco2_m$Rate[which(etco2_m$Type=="75 MINS")],na.rm = T) #1.10

median(etco2_l$Rate[which(etco2_l$Type=="75 MINS")],na.rm = T) #27.5
median(etco2_m$Rate[which(etco2_m$Type=="75 MINS")],na.rm = T) #27.5

t.test(etco2_l$Rate[which(etco2_l$Type=="75 MINS")],etco2_m$Rate[which(etco2_m$Type=="75 MINS")],conf.level = 0.95)
#t = 0, df = 57.835, p-value = 1 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="75 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -10.159, df = 43.336, p-value = 4.881e-13 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="75 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -10.868, df = 43.559, p-value = 5.498e-14 significant

#============================== 90 MINS  ==================================#

mean(etco2_l$Rate[which(etco2_l$Type=="90 MINS")],na.rm = T) #27.26
mean(etco2_m$Rate[which(etco2_m$Type=="90 MINS")],na.rm = T) #27.23

sd(etco2_l$Rate[which(etco2_l$Type=="90 MINS")],na.rm = T) #2.70
sd(etco2_m$Rate[which(etco2_m$Type=="90 MINS")],na.rm = T) #2.55

median(etco2_l$Rate[which(etco2_l$Type=="90 MINS")],na.rm = T) #27
median(etco2_m$Rate[which(etco2_m$Type=="90 MINS")],na.rm = T) #27

t.test(etco2_l$Rate[which(etco2_l$Type=="90 MINS")],etco2_m$Rate[which(etco2_m$Type=="90 MINS")],conf.level = 0.95)
#t = 0.049084, df = 57.818, p-value = 0.961 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="90 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -7.7078, df = 56.314, p-value = 2.264e-10 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="90 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -8.2833, df = 56.199, p-value = 2.581e-11 significant

#===================================  105 MINS  ==================================#

mean(etco2_l$Rate[which(etco2_l$Type=="105 MINS")],na.rm = T) #27.1
mean(etco2_m$Rate[which(etco2_m$Type=="105 MINS")],na.rm = T) #26.9

sd(etco2_l$Rate[which(etco2_l$Type=="105 MINS")],na.rm = T) #4.48
sd(etco2_m$Rate[which(etco2_m$Type=="105 MINS")],na.rm = T) #4.26

median(etco2_l$Rate[which(etco2_l$Type=="105 MINS")],na.rm = T) #25.5
median(etco2_m$Rate[which(etco2_m$Type=="105 MINS")],na.rm = T) #25.5

t.test(etco2_l$Rate[which(etco2_l$Type=="105 MINS")],etco2_m$Rate[which(etco2_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0.17711, df = 57.853, p-value = 0.86 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="105 MINS")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -5.5961, df = 42.951, p-value = 1.415e-06 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="105 MINS")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -6.1683, df = 42.665, p-value = 2.143e-07 significant

#=====================================  120 MINS  ===============================#

mean(etco2_l$Rate[which(etco2_l$Type=="120 MIN")],na.rm = T) #27.5
mean(etco2_m$Rate[which(etco2_m$Type=="120 MIN")],na.rm = T) #27.5

sd(etco2_l$Rate[which(etco2_l$Type=="120 MIN")],na.rm = T) #2.62
sd(etco2_m$Rate[which(etco2_m$Type=="120 MIN")],na.rm = T) #2.46

median(etco2_l$Rate[which(etco2_l$Type=="120 MIN")],na.rm = T) #27.5
median(etco2_m$Rate[which(etco2_m$Type=="120 MIN")],na.rm = T) #27.5

t.test(etco2_l$Rate[which(etco2_l$Type=="120 MIN")],etco2_m$Rate[which(etco2_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0, df = 57.763, p-value = 1 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="120 MIN")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -7.4748, df = 56.826, p-value = 5.233e-10 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="120 MIN")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -8.0195, df = 56.854, p-value = 6.489e-11 significant

#====================================  135 MIN  =======================================#

mean(etco2_l$Rate[which(etco2_l$Type=="135 MIN")],na.rm = T) #28
mean(etco2_m$Rate[which(etco2_m$Type=="135 MIN")],na.rm = T) #28

sd(etco2_l$Rate[which(etco2_l$Type=="135 MIN")],na.rm = T) #3.77
sd(etco2_m$Rate[which(etco2_m$Type=="135 MIN")],na.rm = T) #3.55

median(etco2_l$Rate[which(etco2_l$Type=="135 MIN")],na.rm = T) #28
median(etco2_m$Rate[which(etco2_m$Type=="135 MIN")],na.rm = T) #28

t.test(etco2_l$Rate[which(etco2_l$Type=="135 MIN")],etco2_m$Rate[which(etco2_m$Type=="135 MIN")],conf.level = 0.95)
#t = 0, df = 57.781, p-value = 1 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="135 MIN")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -5.2607, df = 47.516, p-value = 3.378e-06 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="135 MIN")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -5.6402, df = 47.496, p-value = 9.115e-07 significant

#======================================  150 MIN  ====================================#

mean(etco2_l$Rate[which(etco2_l$Type=="150 MIN")],na.rm = T) #28.93
mean(etco2_m$Rate[which(etco2_m$Type=="150 MIN")],na.rm = T) #29.03

sd(etco2_l$Rate[which(etco2_l$Type=="150 MIN")],na.rm = T) #4.09
sd(etco2_m$Rate[which(etco2_m$Type=="150 MIN")],na.rm = T) #3.91

median(etco2_l$Rate[which(etco2_l$Type=="150 MIN")],na.rm = T) #30
median(etco2_m$Rate[which(etco2_m$Type=="150 MIN")],na.rm = T) #30

t.test(etco2_l$Rate[which(etco2_l$Type=="150 MIN")],etco2_m$Rate[which(etco2_m$Type=="150 MIN")],conf.level = 0.95)
#t = -0.12898, df = 57.882, p-value = 0.8978 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="150 MIN")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -3.862, df = 45.291, p-value = 0.0003552significant
t.test(etco2_m$Rate[which(etco2_m$Type=="150 MIN")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -3.9337, df = 44.834, p-value = 0.0002876 significant

#=======================================  165 MIN  ==================================#

mean(etco2_l$Rate[which(etco2_l$Type=="165 MIN")],na.rm = T) #28.63
mean(etco2_m$Rate[which(etco2_m$Type=="165 MIN")],na.rm = T) #28.86

sd(etco2_l$Rate[which(etco2_l$Type=="165 MIN")],na.rm = T) #3.74
sd(etco2_m$Rate[which(etco2_m$Type=="165 MIN")],na.rm = T) #3.77

median(etco2_l$Rate[which(etco2_l$Type=="165 MIN")],na.rm = T) #28.5
median(etco2_m$Rate[which(etco2_m$Type=="165 MIN")],na.rm = T) #28.5

t.test(etco2_l$Rate[which(etco2_l$Type=="165 MIN")],etco2_m$Rate[which(etco2_m$Type=="165 MIN")],conf.level = 0.95)
#t = -0.24028, df = 57.996, p-value = 0.811 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="165 MIN")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -4.5019, df = 47.76, p-value = 4.326e-05 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="165 MIN")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -4.2944, df = 45.79, p-value = 9.01e-05 significant

#=========================================  180 MIN  ================================#

mean(etco2_l$Rate[which(etco2_l$Type=="180 MIN")],na.rm = T) #29.1
mean(etco2_m$Rate[which(etco2_m$Type=="180 MIN")],na.rm = T) #29.4

sd(etco2_l$Rate[which(etco2_l$Type=="180 MIN")],na.rm = T) #4.01
sd(etco2_m$Rate[which(etco2_m$Type=="180 MIN")],na.rm = T) #4.03

median(etco2_l$Rate[which(etco2_l$Type=="180 MIN")],na.rm = T) #29
median(etco2_m$Rate[which(etco2_m$Type=="180 MIN")],na.rm = T) #29

t.test(etco2_l$Rate[which(etco2_l$Type=="180 MIN")],etco2_m$Rate[which(etco2_m$Type=="180 MIN")],conf.level = 0.95)
#t = -0.28863, df = 57.997, p-value = 0.7739 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="180 MIN")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -3.7235, df = 45.838, p-value = 0.0005364 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="180 MIN")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -3.4374, df = 43.998, p-value = 0.001294 significant

#========================================  0 MIN_Emergence  ==========================#

mean(etco2_l$Rate[which(etco2_l$Type=="0 MIN_Emergence")],na.rm = T) #29.06
mean(etco2_m$Rate[which(etco2_m$Type=="0 MIN_Emergence")],na.rm = T) #29.43

sd(etco2_l$Rate[which(etco2_l$Type=="0 MIN_Emergence")],na.rm = T) #3.62
sd(etco2_m$Rate[which(etco2_m$Type=="0 MIN_Emergence")],na.rm = T) #3.6

median(etco2_l$Rate[which(etco2_l$Type=="0 MIN_Emergence")],na.rm = T) #29
median(etco2_m$Rate[which(etco2_m$Type=="0 MIN_Emergence")],na.rm = T) #30

t.test(etco2_l$Rate[which(etco2_l$Type=="0 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = -0.3925, df = 57.998, p-value = 0.6961 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="0 MIN_Emergence")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -4.0525, df = 48.676, p-value = 0.0001819 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="0 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -3.7034, df = 47.062, p-value = 0.0005579 significant

#========================================  1 MIN_Emergence  ============================#

mean(etco2_l$Rate[which(etco2_l$Type=="1 MIN_Emergence")],na.rm = T) #29.23
mean(etco2_m$Rate[which(etco2_m$Type=="1 MIN_Emergence")],na.rm = T) #29.26

sd(etco2_l$Rate[which(etco2_l$Type=="1 MIN_Emergence")],na.rm = T) #2.83
sd(etco2_m$Rate[which(etco2_m$Type=="1 MIN_Emergence")],na.rm = T) #2.8

median(etco2_l$Rate[which(etco2_l$Type=="1 MIN_Emergence")],na.rm = T) #29.5
median(etco2_m$Rate[which(etco2_m$Type=="1 MIN_Emergence")],na.rm = T) #29.5

t.test(etco2_l$Rate[which(etco2_l$Type=="1 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = -0.04578, df = 57.992, p-value = 0.9636 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="1 MIN_Emergence")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -4.5231, df = 55.335, p-value = 3.267e-05 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="1 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -4.6654, df = 54.144, p-value = 2.061e-05 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(etco2_l$Rate[which(etco2_l$Type=="2 MIN_Emergence")],na.rm = T) #28.76
mean(etco2_m$Rate[which(etco2_m$Type=="2 MIN_Emergence")],na.rm = T) #28.73

sd(etco2_l$Rate[which(etco2_l$Type=="2 MIN_Emergence")],na.rm = T) #2.29
sd(etco2_m$Rate[which(etco2_m$Type=="2 MIN_Emergence")],na.rm = T) #2.33

median(etco2_l$Rate[which(etco2_l$Type=="2 MIN_Emergence")],na.rm = T) #29
median(etco2_m$Rate[which(etco2_m$Type=="2 MIN_Emergence")],na.rm = T) #29

t.test(etco2_l$Rate[which(etco2_l$Type=="2 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 0.055731, df = 57.988, p-value = 0.9557 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="2 MIN_Emergence")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -5.8768, df = 57.99, p-value = 2.175e-07 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="2 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -6.1227, df = 57.537, p-value = 8.773e-08 significant

#==========================================  3 MIN_Emergence  ================================#

mean(etco2_l$Rate[which(etco2_l$Type=="3 MIN_Emergence")],na.rm = T) #28.1
mean(etco2_m$Rate[which(etco2_m$Type=="3 MIN_Emergence")],na.rm = T) #27.9

sd(etco2_l$Rate[which(etco2_l$Type=="3 MIN_Emergence")],na.rm = T) #2.89
sd(etco2_m$Rate[which(etco2_m$Type=="3 MIN_Emergence")],na.rm = T) #3.03

median(etco2_l$Rate[which(etco2_l$Type=="3 MIN_Emergence")],na.rm = T) #29.5
median(etco2_m$Rate[which(etco2_m$Type=="3 MIN_Emergence")],na.rm = T) #29.5

t.test(etco2_l$Rate[which(etco2_l$Type=="3 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 0.26137, df = 57.872, p-value = 0.7947 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="3 MIN_Emergence")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -6.1572, df = 54.889, p-value = 9.019e-08 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="3 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -6.4515, df = 52.044, p-value = 3.692e-08 significant

#===========================================  4 MIN_Emergence  =================================#

mean(etco2_l$Rate[which(etco2_l$Type=="4 MIN_Emergence")],na.rm = T) #29.66
mean(etco2_m$Rate[which(etco2_m$Type=="4 MIN_Emergence")],na.rm = T) #99

sd(etco2_l$Rate[which(etco2_l$Type=="4 MIN_Emergence")],na.rm = T) #0.83
sd(etco2_m$Rate[which(etco2_m$Type=="4 MIN_Emergence")],na.rm = T) #0.83

median(etco2_l$Rate[which(etco2_l$Type=="4 MIN_Emergence")],na.rm = T) #99
median(etco2_m$Rate[which(etco2_m$Type=="4 MIN_Emergence")],na.rm = T) #99

t.test(etco2_l$Rate[which(etco2_l$Type=="4 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="4 MIN_Emergence")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -2.8252, df = 43.251, p-value = 0.007121 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="4 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -3.3021, df = 42.051, p-value = 0.001964 significant

#============================================  5 MIN_Emergence  ===================================#

mean(etco2_l$Rate[which(etco2_l$Type=="5 MIN_Emergence")],na.rm = T) #31.03
mean(etco2_m$Rate[which(etco2_m$Type=="5 MIN_Emergence")],na.rm = T) #30.96

sd(etco2_l$Rate[which(etco2_l$Type=="5 MIN_Emergence")],na.rm = T) #2.84
sd(etco2_m$Rate[which(etco2_m$Type=="5 MIN_Emergence")],na.rm = T) #2.72

median(etco2_l$Rate[which(etco2_l$Type=="5 MIN_Emergence")],na.rm = T) #30.5
median(etco2_m$Rate[which(etco2_m$Type=="5 MIN_Emergence")],na.rm = T) #30.5

t.test(etco2_l$Rate[which(etco2_l$Type=="5 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = 0.092703, df = 57.886, p-value = 0.9265 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="5 MIN_Emergence")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -1.8055, df = 55.259, p-value = 0.07645 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="5 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -2.0589, df = 54.851, p-value = 0.04426 significant

#=============================================  6 MIN_Emergence  =====================================#

mean(etco2_l$Rate[which(etco2_l$Type=="6 MIN_Emergence")],na.rm = T) #31.76
mean(etco2_m$Rate[which(etco2_m$Type=="6 MIN_Emergence")],na.rm = T) #31.73

sd(etco2_l$Rate[which(etco2_l$Type=="6 MIN_Emergence")],na.rm = T) #2.83
sd(etco2_m$Rate[which(etco2_m$Type=="6 MIN_Emergence")],na.rm = T) #2.80

median(etco2_l$Rate[which(etco2_l$Type=="6 MIN_Emergence")],na.rm = T) #31.5
median(etco2_m$Rate[which(etco2_m$Type=="6 MIN_Emergence")],na.rm = T) #31.5

t.test(etco2_l$Rate[which(etco2_l$Type=="6 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="6 MIN_Emergence")],conf.level = 0.95)
#t = 0.04578, df = 57.992, p-value = 0.9636 not significant

#Intragroup
t.test(etco2_l$Rate[which(etco2_l$Type=="6 MIN_Emergence")],etco2_l$Rate[which(etco2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -0.70359, df = 55.335, p-value = 0.4846 significant
t.test(etco2_m$Rate[which(etco2_m$Type=="6 MIN_Emergence")],etco2_m$Rate[which(etco2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -0.82941, df = 54.144, p-value = 0.4105 significant

###########################################################  FICO2  #################################################################

fico2_rate<-read_excel("combined.xlsx",sheet = 7)
sum(is.na(fico2_rate))
str(fico2_rate)

#Subsetting the two study groups
fico2_l<-subset(fico2_rate,fico2_rate$`STUDY GROUP`=="L")
fico2_m<-subset(fico2_rate,fico2_rate$`STUDY GROUP`=="M")


#=================    5 mins ==============================#

mean(fico2_l$Rate[which(fico2_l$Type=="5 MINS")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="5 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="5 MINS")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="5 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="5 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="5 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="5 MINS")],fico2_m$Rate[which(fico2_m$Type=="5 MINS")],conf.level = 0.95)
# t = NaN, df = NaN, p-value = NA , Insignificant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="5 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="5 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#============================  10 MINS  ==================================#

mean(fico2_l$Rate[which(fico2_l$Type=="10 MINS")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="10 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="10 MINS")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="10 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="10 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="10 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="10 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA , Insignificant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="10 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="10 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#=============================  15 MINS  ==================================#

mean(fico2_l$Rate[which(fico2_l$Type=="15 MINS")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="15 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="15 MINS")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="15 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="15 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="15 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="15 MINS")],fico2_m$Rate[which(fico2_m$Type=="15 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="15 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="15 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#==========================  30 MINS  ======================================#

mean(fico2_l$Rate[which(fico2_l$Type=="30 MINS")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="30 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="30 MINS")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="30 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="30 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="30 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="30 MINS")],fico2_m$Rate[which(fico2_m$Type=="30 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="30 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="30 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#=============================  45 MINS  ===================================#

mean(fico2_l$Rate[which(fico2_l$Type=="45 MINS")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="45 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="45 MINS")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="45 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="45 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="45 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="45 MINS")],fico2_m$Rate[which(fico2_m$Type=="45 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA  significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="45 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="45 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#===============================  60 MINS  ================================#

mean(fico2_l$Rate[which(fico2_l$Type=="60 MINS")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="60 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="60 MINS")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="60 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="60 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="60 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="60 MINS")],fico2_m$Rate[which(fico2_m$Type=="60 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="60 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="60 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#=============================  75 MINS ===================================#

mean(fico2_l$Rate[which(fico2_l$Type=="75 MINS")],na.rm = T) #0.03
mean(fico2_m$Rate[which(fico2_m$Type=="75 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="75 MINS")],na.rm = T) #0.18
sd(fico2_m$Rate[which(fico2_m$Type=="75 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="75 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="75 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="75 MINS")],fico2_m$Rate[which(fico2_m$Type=="75 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="75 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256
t.test(fico2_m$Rate[which(fico2_m$Type=="75 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#============================== 90 MINS  ==================================#

mean(fico2_l$Rate[which(fico2_l$Type=="90 MINS")],na.rm = T) #0.03
mean(fico2_m$Rate[which(fico2_m$Type=="90 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="90 MINS")],na.rm = T) #0.18
sd(fico2_m$Rate[which(fico2_m$Type=="90 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="90 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="90 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="90 MINS")],fico2_m$Rate[which(fico2_m$Type=="90 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="90 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 significant
t.test(fico2_m$Rate[which(fico2_m$Type=="90 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#===================================  105 MINS  ==================================#

mean(fico2_l$Rate[which(fico2_l$Type=="105 MINS")],na.rm = T) #0.33
mean(fico2_m$Rate[which(fico2_m$Type=="105 MINS")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="105 MINS")],na.rm = T) #0.47
sd(fico2_m$Rate[which(fico2_m$Type=="105 MINS")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="105 MINS")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="105 MINS")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="105 MINS")],fico2_m$Rate[which(fico2_m$Type=="105 MINS")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 not significant
###########################################################################################################################################

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="105 MINS")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant
t.test(fico2_m$Rate[which(fico2_m$Type=="105 MINS")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#=====================================  120 MINS  ===============================#

mean(fico2_l$Rate[which(fico2_l$Type=="120 MIN")],na.rm = T) #0.33
mean(fico2_m$Rate[which(fico2_m$Type=="120 MIN")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="120 MIN")],na.rm = T) #0.47
sd(fico2_m$Rate[which(fico2_m$Type=="120 MIN")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="120 MIN")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="120 MIN")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="120 MIN")],fico2_m$Rate[which(fico2_m$Type=="120 MIN")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 not significant
#########################################################################################################################################

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="120 MIN")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant
t.test(fico2_m$Rate[which(fico2_m$Type=="120 MIN")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#====================================  135 MIN  =======================================#

mean(fico2_l$Rate[which(fico2_l$Type=="135 MIN")],na.rm = T) #0.33
mean(fico2_m$Rate[which(fico2_m$Type=="135 MIN")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="135 MIN")],na.rm = T) #0.47
sd(fico2_m$Rate[which(fico2_m$Type=="135 MIN")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="135 MIN")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="135 MIN")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="135 MIN")],fico2_m$Rate[which(fico2_m$Type=="135 MIN")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 not significant
###################################################################################################################################

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="135 MIN")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 29, p-value = 0.0006723 significant
t.test(fico2_m$Rate[which(fico2_m$Type=="135 MIN")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#======================================  150 MIN  ====================================#

mean(fico2_l$Rate[which(fico2_l$Type=="150 MIN")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="150 MIN")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="150 MIN")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="150 MIN")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="150 MIN")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="150 MIN")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="150 MIN")],fico2_m$Rate[which(fico2_m$Type=="150 MIN")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="150 MIN")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="150 MIN")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#=======================================  165 MIN  ==================================#

mean(fico2_l$Rate[which(fico2_l$Type=="165 MIN")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="165 MIN")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="165 MIN")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="165 MIN")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="165 MIN")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="165 MIN")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="165 MIN")],fico2_m$Rate[which(fico2_m$Type=="165 MIN")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="165 MIN")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="165 MIN")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#=========================================  180 MIN  ================================#

mean(fico2_l$Rate[which(fico2_l$Type=="180 MIN")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="180 MIN")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="180 MIN")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="180 MIN")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="180 MIN")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="180 MIN")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="180 MIN")],fico2_m$Rate[which(fico2_m$Type=="180 MIN")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="180 MIN")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="180 MIN")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#========================================  0 MIN_Emergence  ==========================#

mean(fico2_l$Rate[which(fico2_l$Type=="0 MIN_Emergence")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="0 MIN_Emergence")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="0 MIN_Emergence")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="0 MIN_Emergence")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="0 MIN_Emergence")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="0 MIN_Emergence")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="0 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="0 MIN_Emergence")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="0 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#========================================  1 MIN_Emergence  ============================#

mean(fico2_l$Rate[which(fico2_l$Type=="1 MIN_Emergence")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="1 MIN_Emergence")],na.rm = T) #0.03

sd(fico2_l$Rate[which(fico2_l$Type=="1 MIN_Emergence")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="1 MIN_Emergence")],na.rm = T) #0.18

median(fico2_l$Rate[which(fico2_l$Type=="1 MIN_Emergence")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="1 MIN_Emergence")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="1 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="1 MIN_Emergence")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="1 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(fico2_l$Rate[which(fico2_l$Type=="2 MIN_Emergence")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="2 MIN_Emergence")],na.rm = T) #0.03

sd(fico2_l$Rate[which(fico2_l$Type=="2 MIN_Emergence")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="2 MIN_Emergence")],na.rm = T) #0.18

median(fico2_l$Rate[which(fico2_l$Type=="2 MIN_Emergence")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="2 MIN_Emergence")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="2 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="2 MIN_Emergence")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="2 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 significant

#==========================================  3 MIN_Emergence  ================================#

mean(fico2_l$Rate[which(fico2_l$Type=="3 MIN_Emergence")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="3 MIN_Emergence")],na.rm = T) #0.03

sd(fico2_l$Rate[which(fico2_l$Type=="3 MIN_Emergence")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="3 MIN_Emergence")],na.rm = T) #0.18

median(fico2_l$Rate[which(fico2_l$Type=="3 MIN_Emergence")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="3 MIN_Emergence")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="3 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="3 MIN_Emergence")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="3 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 significant

#===========================================  4 MIN_Emergence  =================================#

mean(fico2_l$Rate[which(fico2_l$Type=="4 MIN_Emergence")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="4 MIN_Emergence")],na.rm = T) #0.03

sd(fico2_l$Rate[which(fico2_l$Type=="4 MIN_Emergence")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="4 MIN_Emergence")],na.rm = T) #0.18

median(fico2_l$Rate[which(fico2_l$Type=="4 MIN_Emergence")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="4 MIN_Emergence")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="4 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = -1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="4 MIN_Emergence")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="4 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 significant

#============================================  5 MIN_Emergence  ===================================#

mean(fico2_l$Rate[which(fico2_l$Type=="5 MIN_Emergence")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="5 MIN_Emergence")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="5 MIN_Emergence")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="5 MIN_Emergence")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="5 MIN_Emergence")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="5 MIN_Emergence")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="5 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="5 MIN_Emergence")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="5 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#=============================================  6 MIN_Emergence  =====================================#

mean(fico2_l$Rate[which(fico2_l$Type=="6 MIN_Emergence")],na.rm = T) #0
mean(fico2_m$Rate[which(fico2_m$Type=="6 MIN_Emergence")],na.rm = T) #0

sd(fico2_l$Rate[which(fico2_l$Type=="6 MIN_Emergence")],na.rm = T) #0
sd(fico2_m$Rate[which(fico2_m$Type=="6 MIN_Emergence")],na.rm = T) #0

median(fico2_l$Rate[which(fico2_l$Type=="6 MIN_Emergence")],na.rm = T) #0
median(fico2_m$Rate[which(fico2_m$Type=="6 MIN_Emergence")],na.rm = T) #0

t.test(fico2_l$Rate[which(fico2_l$Type=="6 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="6 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fico2_l$Rate[which(fico2_l$Type=="6 MIN_Emergence")],fico2_l$Rate[which(fico2_l$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant
t.test(fico2_m$Rate[which(fico2_m$Type=="6 MIN_Emergence")],fico2_m$Rate[which(fico2_m$Type=="10 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA significant

#################################################################  ETN2O  ############################################################

etno2_rate<-read_excel("combined.xlsx",sheet = 8)
sum(is.na(etno2_rate))
str(etno2_rate)

#Subsetting the two study groups
etno2_l<-subset(etno2_rate,etno2_rate$`STUDY GROUP`=="L")
etno2_m<-subset(etno2_rate,etno2_rate$`STUDY GROUP`=="M")


#=================    5 mins ==============================#

mean(etno2_l$Rate[which(etno2_l$Type=="5 MINS")],na.rm = T) #42.83
mean(etno2_m$Rate[which(etno2_m$Type=="5 MINS")],na.rm = T) #44.56

sd(etno2_l$Rate[which(etno2_l$Type=="5 MINS")],na.rm = T) #3.06
sd(etno2_m$Rate[which(etno2_m$Type=="5 MINS")],na.rm = T) #2.37

median(etno2_l$Rate[which(etno2_l$Type=="5 MINS")],na.rm = T) #43.5
median(etno2_m$Rate[which(etno2_m$Type=="5 MINS")],na.rm = T) #45

t.test(etno2_l$Rate[which(etno2_l$Type=="5 MINS")],etno2_m$Rate[which(etno2_m$Type=="5 MINS")],conf.level = 0.95)
# t = -2.4498, df = 54.593, p-value = 0.01753 , Insignificant
############################################################################################################################

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="5 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -2.618, df = 57.323, p-value = 0.01129 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="5 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -1.7293, df = 57.702, p-value = 0.08911 significant

#============================  10 MINS  ==================================#

mean(etno2_l$Rate[which(etno2_l$Type=="10 MINS")],na.rm = T) #44.8
mean(etno2_m$Rate[which(etno2_m$Type=="10 MINS")],na.rm = T) #45.66

sd(etno2_l$Rate[which(etno2_l$Type=="10 MINS")],na.rm = T) #2.74
sd(etno2_m$Rate[which(etno2_m$Type=="10 MINS")],na.rm = T) #2.55

median(etno2_l$Rate[which(etno2_l$Type=="10 MINS")],na.rm = T) #46
median(etno2_m$Rate[which(etno2_m$Type=="10 MINS")],na.rm = T) #46

t.test(etno2_l$Rate[which(etno2_l$Type=="10 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -1.2664, df = 57.685, p-value = 0.2105 , Insignificant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="10 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="10 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=============================  15 MINS  ==================================#

mean(etno2_l$Rate[which(etno2_l$Type=="15 MINS")],na.rm = T) #45.26
mean(etno2_m$Rate[which(etno2_m$Type=="15 MINS")],na.rm = T) #45.96

sd(etno2_l$Rate[which(etno2_l$Type=="15 MINS")],na.rm = T) #2.76
sd(etno2_m$Rate[which(etno2_m$Type=="15 MINS")],na.rm = T) #2.82

median(etno2_l$Rate[which(etno2_l$Type=="15 MINS")],na.rm = T) #45
median(etno2_m$Rate[which(etno2_m$Type=="15 MINS")],na.rm = T) #45

t.test(etno2_l$Rate[which(etno2_l$Type=="15 MINS")],etno2_m$Rate[which(etno2_m$Type=="15 MINS")],conf.level = 0.95)
#t = -0.97026, df = 57.977, p-value = 0.3359 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="15 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0.65571, df = 57.997, p-value = 0.5146 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="15 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.43196, df = 57.417, p-value = 0.6674 significant

#==========================  30 MINS  ======================================#

mean(etno2_l$Rate[which(etno2_l$Type=="30 MINS")],na.rm = T) #45
mean(etno2_m$Rate[which(etno2_m$Type=="30 MINS")],na.rm = T) #45.3

sd(etno2_l$Rate[which(etno2_l$Type=="30 MINS")],na.rm = T) #2.87
sd(etno2_m$Rate[which(etno2_m$Type=="30 MINS")],na.rm = T) #2.91

median(etno2_l$Rate[which(etno2_l$Type=="30 MINS")],na.rm = T) #47
median(etno2_m$Rate[which(etno2_m$Type=="30 MINS")],na.rm = T) #47

t.test(etno2_l$Rate[which(etno2_l$Type=="30 MINS")],etno2_m$Rate[which(etno2_m$Type=="30 MINS")],conf.level = 0.95)
#t = -0.40126, df = 57.99, p-value = 0.6897 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="30 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0.27541, df = 57.876, p-value = 0.784 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="30 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -0.51856, df = 56.999, p-value = 0.6061 significant

#=============================  45 MINS  ===================================#

mean(etno2_l$Rate[which(etno2_l$Type=="45 MINS")],na.rm = T) #47.73
mean(etno2_m$Rate[which(etno2_m$Type=="45 MINS")],na.rm = T) #47.73

sd(etno2_l$Rate[which(etno2_l$Type=="45 MINS")],na.rm = T) #1.85
sd(etno2_m$Rate[which(etno2_m$Type=="45 MINS")],na.rm = T) #1.85

median(etno2_l$Rate[which(etno2_l$Type=="45 MINS")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="45 MINS")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="45 MINS")],etno2_m$Rate[which(etno2_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1  significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="45 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.8468, df = 50.909, p-value = 1.219e-05 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="45 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.5887, df = 52.982, p-value = 0.0007259 significant

#===============================  60 MINS  ================================#

mean(etno2_l$Rate[which(etno2_l$Type=="60 MINS")],na.rm = T) #48
mean(etno2_m$Rate[which(etno2_m$Type=="60 MINS")],na.rm = T) #48

sd(etno2_l$Rate[which(etno2_l$Type=="60 MINS")],na.rm = T) #0
sd(etno2_m$Rate[which(etno2_m$Type=="60 MINS")],na.rm = T) #0

median(etno2_l$Rate[which(etno2_l$Type=="60 MINS")],na.rm = T) #48
median(etno2_m$Rate[which(etno2_m$Type=="60 MINS")],na.rm = T) #48

t.test(etno2_l$Rate[which(etno2_l$Type=="60 MINS")],etno2_m$Rate[which(etno2_m$Type=="60 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="60 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.381, df = 29, p-value = 5.632e-07 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="60 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0106, df = 29, p-value = 2.463e-05 significant

#=============================  75 MINS ===================================#

mean(etno2_l$Rate[which(etno2_l$Type=="75 MINS")],na.rm = T) #48.63
mean(etno2_m$Rate[which(etno2_m$Type=="75 MINS")],na.rm = T) #48.66

sd(etno2_l$Rate[which(etno2_l$Type=="75 MINS")],na.rm = T) #0.55
sd(etno2_m$Rate[which(etno2_m$Type=="75 MINS")],na.rm = T) #0.47

median(etno2_l$Rate[which(etno2_l$Type=="75 MINS")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="75 MINS")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="75 MINS")],etno2_m$Rate[which(etno2_m$Type=="75 MINS")],conf.level = 0.95)
#t = -0.24866, df = 56.771, p-value = 0.8045 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="75 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 7.4919, df = 31.373, p-value = 1.786e-08 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="75 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.3313, df = 31.047, p-value = 4.766e-07 significant

#============================== 90 MINS  ==================================#

mean(etno2_l$Rate[which(etno2_l$Type=="90 MINS")],na.rm = T) #48.93
mean(etno2_m$Rate[which(etno2_m$Type=="90 MINS")],na.rm = T) #49

sd(etno2_l$Rate[which(etno2_l$Type=="90 MINS")],na.rm = T) #0.58
sd(etno2_m$Rate[which(etno2_m$Type=="90 MINS")],na.rm = T) #0

median(etno2_l$Rate[which(etno2_l$Type=="90 MINS")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="90 MINS")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="90 MINS")],etno2_m$Rate[which(etno2_m$Type=="90 MINS")],conf.level = 0.95)
#t = -0.62601, df = 29, p-value = 0.5362 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="90 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 8.0623, df = 31.61, p-value = 3.63e-09 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="90 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 7.158, df = 29, p-value = 7.049e-08 significant

#===================================  105 MINS  ==================================#

mean(etno2_l$Rate[which(etno2_l$Type=="105 MINS")],na.rm = T) #48.7
mean(etno2_m$Rate[which(etno2_m$Type=="105 MINS")],na.rm = T) #48.66

sd(etno2_l$Rate[which(etno2_l$Type=="105 MINS")],na.rm = T) #0.46
sd(etno2_m$Rate[which(etno2_m$Type=="105 MINS")],na.rm = T) #0.47

median(etno2_l$Rate[which(etno2_l$Type=="105 MINS")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="105 MINS")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="105 MINS")],etno2_m$Rate[which(etno2_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0.27304, df = 57.954, p-value = 0.7858 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="105 MINS")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 7.6672, df = 30.669, p-value = 1.29e-08 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="105 MINS")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.3313, df = 31.047, p-value = 4.766e-07 significant

#=====================================  120 MINS  ===============================#

mean(etno2_l$Rate[which(etno2_l$Type=="120 MIN")],na.rm = T) #48.33
mean(etno2_m$Rate[which(etno2_m$Type=="120 MIN")],na.rm = T) #48.33

sd(etno2_l$Rate[which(etno2_l$Type=="120 MIN")],na.rm = T) #0.958
sd(etno2_m$Rate[which(etno2_m$Type=="120 MIN")],na.rm = T) #0.95

median(etno2_l$Rate[which(etno2_l$Type=="120 MIN")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="120 MIN")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="120 MIN")],etno2_m$Rate[which(etno2_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="120 MIN")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.6519, df = 35.965, p-value = 9.479e-08 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="120 MIN")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.3601, df = 37.037, p-value = 4.603e-06 significant

#====================================  135 MIN  =======================================#

mean(etno2_l$Rate[which(etno2_l$Type=="135 MIN")],na.rm = T) #48.3
mean(etno2_m$Rate[which(etno2_m$Type=="135 MIN")],na.rm = T) #48.33

sd(etno2_l$Rate[which(etno2_l$Type=="135 MIN")],na.rm = T) #0.53
sd(etno2_m$Rate[which(etno2_m$Type=="135 MIN")],na.rm = T) #0.47

median(etno2_l$Rate[which(etno2_l$Type=="135 MIN")],na.rm = T) #48
median(etno2_m$Rate[which(etno2_m$Type=="135 MIN")],na.rm = T) #48

t.test(etno2_l$Rate[which(etno2_l$Type=="135 MIN")],etno2_m$Rate[which(etno2_m$Type=="135 MIN")],conf.level = 0.95)
#t = -0.25414, df = 57.317, p-value = 0.8003 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="135 MIN")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.8505, df = 31.197, p-value = 1.084e-07 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="135 MIN")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.6278, df = 31.047, p-value = 3.533e-06 significant

#======================================  150 MIN  ====================================#

mean(etno2_l$Rate[which(etno2_l$Type=="150 MIN")],na.rm = T) #49.03
mean(etno2_m$Rate[which(etno2_m$Type=="150 MIN")],na.rm = T) #49

sd(etno2_l$Rate[which(etno2_l$Type=="150 MIN")],na.rm = T) #0.18
sd(etno2_m$Rate[which(etno2_m$Type=="150 MIN")],na.rm = T) #0

median(etno2_l$Rate[which(etno2_l$Type=="150 MIN")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="150 MIN")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="150 MIN")],etno2_m$Rate[which(etno2_m$Type=="150 MIN")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="150 MIN")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 8.4229, df = 29.256, p-value = 2.591e-09 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="150 MIN")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 7.158, df = 29, p-value = 7.049e-08 significant

#=======================================  165 MIN  ==================================#

mean(etno2_l$Rate[which(etno2_l$Type=="165 MIN")],na.rm = T) #48.63
mean(etno2_m$Rate[which(etno2_m$Type=="165 MIN")],na.rm = T) #48.66

sd(etno2_l$Rate[which(etno2_l$Type=="165 MIN")],na.rm = T) #0.55
sd(etno2_m$Rate[which(etno2_m$Type=="165 MIN")],na.rm = T) #0.47

median(etno2_l$Rate[which(etno2_l$Type=="165 MIN")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="165 MIN")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="165 MIN")],etno2_m$Rate[which(etno2_m$Type=="165 MIN")],conf.level = 0.95)
#t = -0.24866, df = 56.771, p-value = 0.8045 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="165 MIN")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 7.4919, df = 31.373, p-value = 1.786e-08 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="165 MIN")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.3313, df = 31.047, p-value = 4.766e-07 significant

#=========================================  180 MIN  ================================#

mean(etno2_l$Rate[which(etno2_l$Type=="180 MIN")],na.rm = T) #48.66
mean(etno2_m$Rate[which(etno2_m$Type=="180 MIN")],na.rm = T) #48.66

sd(etno2_l$Rate[which(etno2_l$Type=="180 MIN")],na.rm = T) #0.47
sd(etno2_m$Rate[which(etno2_m$Type=="180 MIN")],na.rm = T) #0.47

median(etno2_l$Rate[which(etno2_l$Type=="180 MIN")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="180 MIN")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="180 MIN")],etno2_m$Rate[which(etno2_m$Type=="180 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="180 MIN")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 7.5955, df = 30.766, p-value = 1.53e-08 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="180 MIN")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.3313, df = 31.047, p-value = 4.766e-07 significant

#========================================  0 MIN_Emergence  ==========================#

mean(etno2_l$Rate[which(etno2_l$Type=="0 MIN_Emergence")],na.rm = T) #49.03
mean(etno2_m$Rate[which(etno2_m$Type=="0 MIN_Emergence")],na.rm = T) #49

sd(etno2_l$Rate[which(etno2_l$Type=="0 MIN_Emergence")],na.rm = T) #0.18
sd(etno2_m$Rate[which(etno2_m$Type=="0 MIN_Emergence")],na.rm = T) #0

median(etno2_l$Rate[which(etno2_l$Type=="0 MIN_Emergence")],na.rm = T) #49
median(etno2_m$Rate[which(etno2_m$Type=="0 MIN_Emergence")],na.rm = T) #49

t.test(etno2_l$Rate[which(etno2_l$Type=="0 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="0 MIN_Emergence")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 8.4229, df = 29.256, p-value = 2.591e-09 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="0 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 7.158, df = 29, p-value = 7.049e-08 significant

#========================================  1 MIN_Emergence  ============================#

mean(etno2_l$Rate[which(etno2_l$Type=="1 MIN_Emergence")],na.rm = T) #19.96
mean(etno2_m$Rate[which(etno2_m$Type=="1 MIN_Emergence")],na.rm = T) #15.6

sd(etno2_l$Rate[which(etno2_l$Type=="1 MIN_Emergence")],na.rm = T) #2.07
sd(etno2_m$Rate[which(etno2_m$Type=="1 MIN_Emergence")],na.rm = T) #2.78

median(etno2_l$Rate[which(etno2_l$Type=="1 MIN_Emergence")],na.rm = T) #20
median(etno2_m$Rate[which(etno2_m$Type=="1 MIN_Emergence")],na.rm = T) #16.5

t.test(etno2_l$Rate[which(etno2_l$Type=="1 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 6.8829, df = 53.608, p-value = 6.602e-09 not significant
###############################################################################################################################

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="1 MIN_Emergence")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -39.506, df = 53.978, p-value < 2.2e-16 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="1 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -43.593, df = 57.552, p-value < 2.2e-16 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(etno2_l$Rate[which(etno2_l$Type=="2 MIN_Emergence")],na.rm = T) #13.66
mean(etno2_m$Rate[which(etno2_m$Type=="2 MIN_Emergence")],na.rm = T) #8

sd(etno2_l$Rate[which(etno2_l$Type=="2 MIN_Emergence")],na.rm = T) #2.73
sd(etno2_m$Rate[which(etno2_m$Type=="2 MIN_Emergence")],na.rm = T) #2.81

median(etno2_l$Rate[which(etno2_l$Type=="2 MIN_Emergence")],na.rm = T) #13
median(etno2_m$Rate[which(etno2_m$Type=="2 MIN_Emergence")],na.rm = T) #9

t.test(etno2_l$Rate[which(etno2_l$Type=="2 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 7.9085, df = 57.948, p-value = 8.769e-11 not significant
###########################################################################################################################

#Intragroup
t.test(etno2_l$Rate[which(fico2_l$Type=="2 MIN_Emergence")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -44.006, df = 57.999, p-value < 2.2e-16 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="2 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -54.298, df = 57.44, p-value < 2.2e-16 significant

#==========================================  3 MIN_Emergence  ================================#

mean(etno2_l$Rate[which(etno2_l$Type=="3 MIN_Emergence")],na.rm = T) #5.33
mean(etno2_m$Rate[which(etno2_m$Type=="3 MIN_Emergence")],na.rm = T) #1.76

sd(etno2_l$Rate[which(etno2_l$Type=="3 MIN_Emergence")],na.rm = T) #1.935
sd(etno2_m$Rate[which(etno2_m$Type=="3 MIN_Emergence")],na.rm = T) #1.63

median(etno2_l$Rate[which(etno2_l$Type=="3 MIN_Emergence")],na.rm = T) #5
median(etno2_m$Rate[which(etno2_m$Type=="3 MIN_Emergence")],na.rm = T) #1

t.test(etno2_l$Rate[which(etno2_l$Type=="3 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 7.7131, df = 56.403, p-value = 2.198e-10 not significant
#############################################################################################################################

#Intragroup
t.test(etno2_l$Rate[which(etno2_l$Type=="3 MIN_Emergence")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -64.329, df = 52.106, p-value < 2.2e-16 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="3 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -79.388, df = 49.36, p-value < 2.2e-16 significant

#===========================================  4 MIN_Emergence  =================================#

mean(etno2_l$Rate[which(etno2_l$Type=="4 MIN_Emergence")],na.rm = T) #0.4
mean(etno2_m$Rate[which(etno2_m$Type=="4 MIN_Emergence")],na.rm = T) #0

sd(etno2_l$Rate[which(etno2_l$Type=="4 MIN_Emergence")],na.rm = T) #1.13
sd(etno2_m$Rate[which(etno2_m$Type=="4 MIN_Emergence")],na.rm = T) #0

median(etno2_l$Rate[which(etno2_l$Type=="4 MIN_Emergence")],na.rm = T) #0
median(etno2_m$Rate[which(etno2_m$Type=="4 MIN_Emergence")],na.rm = T) #0

t.test(etno2_l$Rate[which(etno2_l$Type=="4 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 1.9344, df = 29, p-value = 0.06287 not significant


#Intragroup
t.test(etno2_l$Rate[which(etno2_l$Type=="4 MIN_Emergence")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -81.851, df = 38.584, p-value < 2.2e-16 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="4 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -98.064, df = 29, p-value < 2.2e-16 significant

#============================================  5 MIN_Emergence  ===================================#

mean(etno2_l$Rate[which(etno2_l$Type=="5 MIN_Emergence")],na.rm = T) #0
mean(etno2_m$Rate[which(etno2_m$Type=="5 MIN_Emergence")],na.rm = T) #0

sd(etno2_l$Rate[which(etno2_l$Type=="5 MIN_Emergence")],na.rm = T) #0
sd(etno2_m$Rate[which(etno2_m$Type=="5 MIN_Emergence")],na.rm = T) #0

median(etno2_l$Rate[which(etno2_l$Type=="5 MIN_Emergence")],na.rm = T) #0
median(etno2_m$Rate[which(etno2_m$Type=="5 MIN_Emergence")],na.rm = T) #0

t.test(etno2_l$Rate[which(etno2_l$Type=="5 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(etno2_l$Rate[which(etno2_l$Type=="5 MIN_Emergence")],etno2_l$Rate[which(etno2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -89.333, df = 29, p-value < 2.2e-16 significant
t.test(etno2_m$Rate[which(etno2_m$Type=="5 MIN_Emergence")],etno2_m$Rate[which(etno2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -98.064, df = 29, p-value < 2.2e-16 significant

#########################################################  FIN2O  ##################################################################

fin2o_rate<-read_excel("combined.xlsx",sheet = 9)
sum(is.na(fin2o_rate))
str(fin2o_rate)

#Subsetting the two study groups
fin2o_l<-subset(fin2o_rate,fin2o_rate$`STUDY GROUP`=="L")
fin2o_m<-subset(fin2o_rate,fin2o_rate$`STUDY GROUP`=="M")


#=================    5 mins ==============================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="5 MINS")],na.rm = T) #39.83
mean(fin2o_m$Rate[which(fin2o_m$Type=="5 MINS")],na.rm = T) #44.73

sd(fin2o_l$Rate[which(fin2o_l$Type=="5 MINS")],na.rm = T) #3.77
sd(fin2o_m$Rate[which(fin2o_m$Type=="5 MINS")],na.rm = T) #2.36

median(fin2o_l$Rate[which(fin2o_l$Type=="5 MINS")],na.rm = T) #40
median(fin2o_m$Rate[which(fin2o_m$Type=="5 MINS")],na.rm = T) #45

t.test(fin2o_l$Rate[which(fin2o_l$Type=="5 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="5 MINS")],conf.level = 0.95)
# t = -6.0219, df = 48.664, p-value = 2.216e-07 , Insignificant
############################################################################################################################

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="5 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = -3.4676, df = 49.087, p-value = 0.001102 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="5 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -2.0122, df = 57.995, p-value = 0.04885 significant

#============================  10 MINS  ==================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],na.rm = T) #42.66
mean(fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],na.rm = T) #45.96

sd(fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],na.rm = T) #2.39
sd(fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],na.rm = T) #2.55

median(fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],na.rm = T) #46
median(fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],na.rm = T) #46

t.test(fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -1.2664, df = 57.685, p-value = 0.2105 , Insignificant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=============================  15 MINS  ==================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="15 MINS")],na.rm = T) #45.26
mean(fin2o_m$Rate[which(fin2o_m$Type=="15 MINS")],na.rm = T) #45.96

sd(fin2o_l$Rate[which(fin2o_l$Type=="15 MINS")],na.rm = T) #2.76
sd(fin2o_m$Rate[which(fin2o_m$Type=="15 MINS")],na.rm = T) #2.82

median(fin2o_l$Rate[which(fin2o_l$Type=="15 MINS")],na.rm = T) #45
median(fin2o_m$Rate[which(fin2o_m$Type=="15 MINS")],na.rm = T) #45

t.test(fin2o_l$Rate[which(fin2o_l$Type=="15 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="15 MINS")],conf.level = 0.95)
#t = -0.97026, df = 57.977, p-value = 0.3359 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="15 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.9847, df = 35.024, p-value = 0.0003261 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="15 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.30138, df = 56.903, p-value = 0.7642 significant

#==========================  30 MINS  ======================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="30 MINS")],na.rm = T) #45
mean(fin2o_m$Rate[which(fin2o_m$Type=="30 MINS")],na.rm = T) #45.3

sd(fin2o_l$Rate[which(fin2o_l$Type=="30 MINS")],na.rm = T) #2.87
sd(fin2o_m$Rate[which(fin2o_m$Type=="30 MINS")],na.rm = T) #2.91

median(fin2o_l$Rate[which(fin2o_l$Type=="30 MINS")],na.rm = T) #47
median(fin2o_m$Rate[which(fin2o_m$Type=="30 MINS")],na.rm = T) #47

t.test(fin2o_l$Rate[which(fin2o_l$Type=="30 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="30 MINS")],conf.level = 0.95)
#t = -0.40126, df = 57.99, p-value = 0.6897 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="30 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 7.5518, df = 46.882, p-value = 1.216e-09 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="30 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -0.64033, df = 56.335, p-value = 0.5246 significant

#=============================  45 MINS  ===================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="45 MINS")],na.rm = T) #47.73
mean(fin2o_m$Rate[which(fin2o_m$Type=="45 MINS")],na.rm = T) #47.73

sd(fin2o_l$Rate[which(fin2o_l$Type=="45 MINS")],na.rm = T) #1.85
sd(fin2o_m$Rate[which(fin2o_m$Type=="45 MINS")],na.rm = T) #1.85

median(fin2o_l$Rate[which(fin2o_l$Type=="45 MINS")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="45 MINS")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="45 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1  significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="45 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 10.607, df = 38.048, p-value = 6.378e-13 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="45 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.6222, df = 38.137, p-value = 0.0008481 significant

#===============================  60 MINS  ================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="60 MINS")],na.rm = T) #48
mean(fin2o_m$Rate[which(fin2o_m$Type=="60 MINS")],na.rm = T) #48

sd(fin2o_l$Rate[which(fin2o_l$Type=="60 MINS")],na.rm = T) #0
sd(fin2o_m$Rate[which(fin2o_m$Type=="60 MINS")],na.rm = T) #0

median(fin2o_l$Rate[which(fin2o_l$Type=="60 MINS")],na.rm = T) #48
median(fin2o_m$Rate[which(fin2o_m$Type=="60 MINS")],na.rm = T) #48

t.test(fin2o_l$Rate[which(fin2o_l$Type=="60 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="60 MINS")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="60 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 13.442, df = 31.316, p-value = 1.505e-14 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="60 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.0789, df = 31.34, p-value = 9.388e-07 significant

#=============================  75 MINS ===================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="75 MINS")],na.rm = T) #48.63
mean(fin2o_m$Rate[which(fin2o_m$Type=="75 MINS")],na.rm = T) #48.66

sd(fin2o_l$Rate[which(fin2o_l$Type=="75 MINS")],na.rm = T) #0.55
sd(fin2o_m$Rate[which(fin2o_m$Type=="75 MINS")],na.rm = T) #0.47

median(fin2o_l$Rate[which(fin2o_l$Type=="75 MINS")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="75 MINS")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="75 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="75 MINS")],conf.level = 0.95)
#t = -0.24866, df = 56.771, p-value = 0.8045 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="75 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 14.142, df = 38.048, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="75 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 7.1734, df = 38.137, p-value = 1.406e-08 significant

#============================== 90 MINS  ==================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="90 MINS")],na.rm = T) #48.93
mean(fin2o_m$Rate[which(fin2o_m$Type=="90 MINS")],na.rm = T) #49

sd(fin2o_l$Rate[which(fin2o_l$Type=="90 MINS")],na.rm = T) #0.58
sd(fin2o_m$Rate[which(fin2o_m$Type=="90 MINS")],na.rm = T) #0

median(fin2o_l$Rate[which(fin2o_l$Type=="90 MINS")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="90 MINS")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="90 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="90 MINS")],conf.level = 0.95)
#t = -0.62601, df = 29, p-value = 0.5362 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="90 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 14.47, df = 29, p-value = 8.465e-15 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="90 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.966, df = 29, p-value = 1.171e-07 significant

#===================================  105 MINS  ==================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="105 MINS")],na.rm = T) #48.7
mean(fin2o_m$Rate[which(fin2o_m$Type=="105 MINS")],na.rm = T) #48.66

sd(fin2o_l$Rate[which(fin2o_l$Type=="105 MINS")],na.rm = T) #0.46
sd(fin2o_m$Rate[which(fin2o_m$Type=="105 MINS")],na.rm = T) #0.47

median(fin2o_l$Rate[which(fin2o_l$Type=="105 MINS")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="105 MINS")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="105 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0.27304, df = 57.954, p-value = 0.7858 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="105 MINS")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 12.408, df = 47.484, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="105 MINS")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.9651, df = 47.631, p-value = 2.908e-07 significant

#=====================================  120 MINS  ===============================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="120 MIN")],na.rm = T) #48.33
mean(fin2o_m$Rate[which(fin2o_m$Type=="120 MIN")],na.rm = T) #48.33

sd(fin2o_l$Rate[which(fin2o_l$Type=="120 MIN")],na.rm = T) #0.958
sd(fin2o_m$Rate[which(fin2o_m$Type=="120 MIN")],na.rm = T) #0.95

median(fin2o_l$Rate[which(fin2o_l$Type=="120 MIN")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="120 MIN")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="120 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="120 MIN")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 14.142, df = 38.048, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="120 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 7.1734, df = 38.137, p-value = 1.406e-08 significant

#====================================  135 MIN  =======================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="135 MIN")],na.rm = T) #48.3
mean(fin2o_m$Rate[which(fin2o_m$Type=="135 MIN")],na.rm = T) #48.33

sd(fin2o_l$Rate[which(fin2o_l$Type=="135 MIN")],na.rm = T) #0.53
sd(fin2o_m$Rate[which(fin2o_m$Type=="135 MIN")],na.rm = T) #0.47

median(fin2o_l$Rate[which(fin2o_l$Type=="135 MIN")],na.rm = T) #48
median(fin2o_m$Rate[which(fin2o_m$Type=="135 MIN")],na.rm = T) #48

t.test(fin2o_l$Rate[which(fin2o_l$Type=="135 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="135 MIN")],conf.level = 0.95)
#t = -0.25414, df = 57.317, p-value = 0.8003 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="135 MIN")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 14.936, df = 31.316, p-value = 8.561e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="135 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 7.5798, df = 31.34, p-value = 1.415e-08 significant

#======================================  150 MIN  ====================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="150 MIN")],na.rm = T) #49.03
mean(fin2o_m$Rate[which(fin2o_m$Type=="150 MIN")],na.rm = T) #49

sd(fin2o_l$Rate[which(fin2o_l$Type=="150 MIN")],na.rm = T) #0.18
sd(fin2o_m$Rate[which(fin2o_m$Type=="150 MIN")],na.rm = T) #0

median(fin2o_l$Rate[which(fin2o_l$Type=="150 MIN")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="150 MIN")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="150 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="150 MIN")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="150 MIN")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 14.142, df = 38.048, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="150 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 7.1734, df = 38.137, p-value = 1.406e-08 significant

#=======================================  165 MIN  ==================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="165 MIN")],na.rm = T) #48.63
mean(fin2o_m$Rate[which(fin2o_m$Type=="165 MIN")],na.rm = T) #48.66

sd(fin2o_l$Rate[which(fin2o_l$Type=="165 MIN")],na.rm = T) #0.55
sd(fin2o_m$Rate[which(fin2o_m$Type=="165 MIN")],na.rm = T) #0.47

median(fin2o_l$Rate[which(fin2o_l$Type=="165 MIN")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="165 MIN")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="165 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="165 MIN")],conf.level = 0.95)
#t = -0.24866, df = 56.771, p-value = 0.8045 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="165 MIN")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 12.117, df = 44.059, p-value = 1.274e-15 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="165 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.4743, df = 44.192, p-value = 1.963e-06 significant

#=========================================  180 MIN  ================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="180 MIN")],na.rm = T) #48.66
mean(fin2o_m$Rate[which(fin2o_m$Type=="180 MIN")],na.rm = T) #48.66

sd(fin2o_l$Rate[which(fin2o_l$Type=="180 MIN")],na.rm = T) #0.47
sd(fin2o_m$Rate[which(fin2o_m$Type=="180 MIN")],na.rm = T) #0.47

median(fin2o_l$Rate[which(fin2o_l$Type=="180 MIN")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="180 MIN")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="180 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="180 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="180 MIN")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 13.673, df = 35.861, p-value = 8.572e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="180 MIN")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.5786, df = 35.93, p-value = 1.191e-07 significant

#========================================  0 MIN_Emergence  ==========================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="0 MIN_Emergence")],na.rm = T) #49.03
mean(fin2o_m$Rate[which(fin2o_m$Type=="0 MIN_Emergence")],na.rm = T) #49

sd(fin2o_l$Rate[which(fin2o_l$Type=="0 MIN_Emergence")],na.rm = T) #0.18
sd(fin2o_m$Rate[which(fin2o_m$Type=="0 MIN_Emergence")],na.rm = T) #0

median(fin2o_l$Rate[which(fin2o_l$Type=="0 MIN_Emergence")],na.rm = T) #49
median(fin2o_m$Rate[which(fin2o_m$Type=="0 MIN_Emergence")],na.rm = T) #49

t.test(fin2o_l$Rate[which(fin2o_l$Type=="0 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 1, df = 29, p-value = 0.3256 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="0 MIN_Emergence")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = 13.673, df = 35.861, p-value = 8.572e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="0 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.5786, df = 35.93, p-value = 1.191e-07 significant

#========================================  1 MIN_Emergence  ============================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="1 MIN_Emergence")],na.rm = T) #19.96
mean(fin2o_m$Rate[which(fin2o_m$Type=="1 MIN_Emergence")],na.rm = T) #15.6

sd(fin2o_l$Rate[which(fin2o_l$Type=="1 MIN_Emergence")],na.rm = T) #2.07
sd(fin2o_m$Rate[which(fin2o_m$Type=="1 MIN_Emergence")],na.rm = T) #2.78

median(fin2o_l$Rate[which(fin2o_l$Type=="1 MIN_Emergence")],na.rm = T) #20
median(fin2o_m$Rate[which(fin2o_m$Type=="1 MIN_Emergence")],na.rm = T) #16.5

t.test(fin2o_l$Rate[which(fin2o_l$Type=="1 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 6.8829, df = 53.608, p-value = 6.602e-09 not significant
###############################################################################################################################

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="1 MIN_Emergence")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = -42.866, df = 51.532, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="1 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -42.586, df = 54.861, p-value < 2.2e-16 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="2 MIN_Emergence")],na.rm = T) #13.66
mean(fin2o_m$Rate[which(fin2o_m$Type=="2 MIN_Emergence")],na.rm = T) #8

sd(fin2o_l$Rate[which(fin2o_l$Type=="2 MIN_Emergence")],na.rm = T) #2.73
sd(fin2o_m$Rate[which(fin2o_m$Type=="2 MIN_Emergence")],na.rm = T) #2.81

median(fin2o_l$Rate[which(fin2o_l$Type=="2 MIN_Emergence")],na.rm = T) #13
median(fin2o_m$Rate[which(fin2o_m$Type=="2 MIN_Emergence")],na.rm = T) #9

t.test(fin2o_l$Rate[which(fin2o_l$Type=="2 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 7.9085, df = 57.948, p-value = 8.769e-11 not significant
###########################################################################################################################

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="2 MIN_Emergence")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = -43.659, df = 57.042, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="2 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -57.901, df = 57.362, p-value < 2.2e-16 significant

#==========================================  3 MIN_Emergence  ================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="3 MIN_Emergence")],na.rm = T) #5.33
mean(fin2o_m$Rate[which(fin2o_m$Type=="3 MIN_Emergence")],na.rm = T) #1.76

sd(fin2o_l$Rate[which(fin2o_l$Type=="3 MIN_Emergence")],na.rm = T) #1.935
sd(fin2o_m$Rate[which(fin2o_m$Type=="3 MIN_Emergence")],na.rm = T) #1.63

median(fin2o_l$Rate[which(fin2o_l$Type=="3 MIN_Emergence")],na.rm = T) #5
median(fin2o_m$Rate[which(fin2o_m$Type=="3 MIN_Emergence")],na.rm = T) #1

t.test(fin2o_l$Rate[which(fin2o_l$Type=="3 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 7.7131, df = 56.403, p-value = 2.198e-10 not significant
#############################################################################################################################

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="3 MIN_Emergence")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = -65.207, df = 56.151, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="3 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.754, df = 49.828, p-value < 2.2e-16 significant

#===========================================  4 MIN_Emergence  =================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="4 MIN_Emergence")],na.rm = T) #0.4
mean(fin2o_m$Rate[which(fin2o_m$Type=="4 MIN_Emergence")],na.rm = T) #0

sd(fin2o_l$Rate[which(fin2o_l$Type=="4 MIN_Emergence")],na.rm = T) #1.13
sd(fin2o_m$Rate[which(fin2o_m$Type=="4 MIN_Emergence")],na.rm = T) #0

median(fin2o_l$Rate[which(fin2o_l$Type=="4 MIN_Emergence")],na.rm = T) #0
median(fin2o_m$Rate[which(fin2o_m$Type=="4 MIN_Emergence")],na.rm = T) #0

t.test(fin2o_l$Rate[which(fin2o_l$Type=="4 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 1.9344, df = 29, p-value = 0.06287 not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="4 MIN_Emergence")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = -91.976, df = 35.46, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="4 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -105.56, df = 29, p-value < 2.2e-16 significant

#============================================  5 MIN_Emergence  ===================================#

mean(fin2o_l$Rate[which(fin2o_l$Type=="5 MIN_Emergence")],na.rm = T) #0
mean(fin2o_m$Rate[which(fin2o_m$Type=="5 MIN_Emergence")],na.rm = T) #0

sd(fin2o_l$Rate[which(fin2o_l$Type=="5 MIN_Emergence")],na.rm = T) #0
sd(fin2o_m$Rate[which(fin2o_m$Type=="5 MIN_Emergence")],na.rm = T) #0

median(fin2o_l$Rate[which(fin2o_l$Type=="5 MIN_Emergence")],na.rm = T) #0
median(fin2o_m$Rate[which(fin2o_m$Type=="5 MIN_Emergence")],na.rm = T) #0

t.test(fin2o_l$Rate[which(fin2o_l$Type=="5 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fin2o_l$Rate[which(fin2o_l$Type=="5 MIN_Emergence")],fin2o_l$Rate[which(fin2o_l$Type=="10 MINS")],conf.level = 0.95)
#t = -97.482, df = 29, p-value < 2.2e-16 significant
t.test(fin2o_m$Rate[which(fin2o_m$Type=="5 MIN_Emergence")],fin2o_m$Rate[which(fin2o_m$Type=="10 MINS")],conf.level = 0.95)
#t = -105.56, df = 29, p-value < 2.2e-16 significant

########################################################   ETDES  ########################################################################

etdes_rate<-read_excel("combined.xlsx",sheet = 10)
sum(is.na(etdes_rate))
str(etdes_rate)

#Subsetting the two study groups
etdes_l<-subset(etdes_rate,etdes_rate$`STUDY GROUP`=="L")
etdes_m<-subset(etdes_rate,etdes_rate$`STUDY GROUP`=="M")


#=================    5 mins ==============================#

mean(etdes_l$Rate[which(etdes_l$Type=="5 MINS")],na.rm = T) #2.65
mean(etdes_m$Rate[which(etdes_m$Type=="5 MINS")],na.rm = T) #2.8

sd(etdes_l$Rate[which(etdes_l$Type=="5 MINS")],na.rm = T) #0.18
sd(etdes_m$Rate[which(etdes_m$Type=="5 MINS")],na.rm = T) #0.08

median(etdes_l$Rate[which(etdes_l$Type=="5 MINS")],na.rm = T) #2.7
median(etdes_m$Rate[which(etdes_m$Type=="5 MINS")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="5 MINS")],etdes_m$Rate[which(etdes_m$Type=="5 MINS")],conf.level = 0.95)
# t = -4.0816, df = 40.417, p-value = 0.0002052 , Insignificant
############################################################################################################################

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="5 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = -0.36225, df = 57.8, p-value = 0.7185 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="5 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant

#============================  10 MINS  ==================================#

mean(etdes_l$Rate[which(etdes_l$Type=="10 MINS")],na.rm = T) #2.66
mean(etdes_m$Rate[which(etdes_m$Type=="10 MINS")],na.rm = T) #2.66

sd(etdes_l$Rate[which(etdes_l$Type=="10 MINS")],na.rm = T) #0.17
sd(etdes_m$Rate[which(etdes_m$Type=="10 MINS")],na.rm = T) #0.17

median(etdes_l$Rate[which(etdes_l$Type=="10 MINS")],na.rm = T) #2.6
median(etdes_m$Rate[which(etdes_m$Type=="10 MINS")],na.rm = T) #2.6

t.test(etdes_l$Rate[which(etdes_l$Type=="10 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 , Insignificant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="10 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="10 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=============================  15 MINS  ==================================#

mean(etdes_l$Rate[which(etdes_l$Type=="15 MINS")],na.rm = T) #2.8
mean(etdes_m$Rate[which(etdes_m$Type=="15 MINS")],na.rm = T) #2.8

sd(etdes_l$Rate[which(etdes_l$Type=="15 MINS")],na.rm = T) #0
sd(etdes_m$Rate[which(etdes_m$Type=="15 MINS")],na.rm = T) #0

median(etdes_l$Rate[which(etdes_l$Type=="15 MINS")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="15 MINS")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="15 MINS")],etdes_m$Rate[which(etdes_m$Type=="15 MINS")],conf.level = 0.95)
#t = -0.97026, df = 57.977, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="15 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.2245, df = 29, p-value = 0.0002168 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="15 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 4.2245, df = 29, p-value = 0.0002168 significant

#==========================  30 MINS  ======================================#

mean(etdes_l$Rate[which(etdes_l$Type=="30 MINS")],na.rm = T) #2.83
mean(etdes_m$Rate[which(etdes_m$Type=="30 MINS")],na.rm = T) #2.83

sd(etdes_l$Rate[which(etdes_l$Type=="30 MINS")],na.rm = T) #0.09
sd(etdes_m$Rate[which(etdes_m$Type=="30 MINS")],na.rm = T) #0.09

median(etdes_l$Rate[which(etdes_l$Type=="30 MINS")],na.rm = T) #2.9
median(etdes_m$Rate[which(etdes_m$Type=="30 MINS")],na.rm = T) #2.9

t.test(etdes_l$Rate[which(etdes_l$Type=="30 MINS")],etdes_m$Rate[which(etdes_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="30 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="30 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant

#=============================  45 MINS  ===================================#

mean(etdes_l$Rate[which(etdes_l$Type=="45 MINS")],na.rm = T) #2.73
mean(etdes_m$Rate[which(etdes_m$Type=="45 MINS")],na.rm = T) #2.73

sd(etdes_l$Rate[which(etdes_l$Type=="45 MINS")],na.rm = T) #0.09
sd(etdes_m$Rate[which(etdes_m$Type=="45 MINS")],na.rm = T) #0.09

median(etdes_l$Rate[which(etdes_l$Type=="45 MINS")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="45 MINS")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="45 MINS")],etdes_m$Rate[which(etdes_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1  significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="45 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 1.8471, df = 45.303, p-value = 0.07127 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="45 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.8471, df = 45.303, p-value = 0.07127 significant

#===============================  60 MINS  ================================#

mean(etdes_l$Rate[which(etdes_l$Type=="60 MINS")],na.rm = T) #2.76
mean(etdes_m$Rate[which(etdes_m$Type=="60 MINS")],na.rm = T) #2.76

sd(etdes_l$Rate[which(etdes_l$Type=="60 MINS")],na.rm = T) #0.04
sd(etdes_m$Rate[which(etdes_m$Type=="60 MINS")],na.rm = T) #0.04

median(etdes_l$Rate[which(etdes_l$Type=="60 MINS")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="60 MINS")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="60 MINS")],etdes_m$Rate[which(etdes_m$Type=="60 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="60 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442significant
t.test(etdes_m$Rate[which(etdes_m$Type=="60 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant

#=============================  75 MINS ===================================#

mean(etdes_l$Rate[which(etdes_l$Type=="75 MINS")],na.rm = T) #2.8
mean(etdes_m$Rate[which(etdes_m$Type=="75 MINS")],na.rm = T) #2.8

sd(etdes_l$Rate[which(etdes_l$Type=="75 MINS")],na.rm = T) #0.08
sd(etdes_m$Rate[which(etdes_m$Type=="75 MINS")],na.rm = T) #0.08

median(etdes_l$Rate[which(etdes_l$Type=="75 MINS")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="75 MINS")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="75 MINS")],etdes_m$Rate[which(etdes_m$Type=="75 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="75 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="75 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant

#============================== 90 MINS  ==================================#

mean(etdes_l$Rate[which(etdes_l$Type=="90 MINS")],na.rm = T) #2.86
mean(etdes_m$Rate[which(etdes_m$Type=="90 MINS")],na.rm = T) #2.86

sd(etdes_l$Rate[which(etdes_l$Type=="90 MINS")],na.rm = T) #0.04
sd(etdes_m$Rate[which(etdes_m$Type=="90 MINS")],na.rm = T) #0.04

median(etdes_l$Rate[which(etdes_l$Type=="90 MINS")],na.rm = T) #2.9
median(etdes_m$Rate[which(etdes_m$Type=="90 MINS")],na.rm = T) #2.9

t.test(etdes_l$Rate[which(etdes_l$Type=="90 MINS")],etdes_m$Rate[which(etdes_m$Type=="90 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="90 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="90 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant

#===================================  105 MINS  ==================================#

mean(etdes_l$Rate[which(etdes_l$Type=="105 MINS")],na.rm = T) #2.86
mean(etdes_m$Rate[which(etdes_m$Type=="105 MINS")],na.rm = T) #2.86

sd(etdes_l$Rate[which(etdes_l$Type=="105 MINS")],na.rm = T) #0.04
sd(etdes_m$Rate[which(etdes_m$Type=="105 MINS")],na.rm = T) #0.04

median(etdes_l$Rate[which(etdes_l$Type=="105 MINS")],na.rm = T) #2.9
median(etdes_m$Rate[which(etdes_m$Type=="105 MINS")],na.rm = T) #2.9

t.test(etdes_l$Rate[which(etdes_l$Type=="105 MINS")],etdes_m$Rate[which(etdes_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="105 MINS")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="105 MINS")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant

#=====================================  120 MINS  ===============================#

mean(etdes_l$Rate[which(etdes_l$Type=="120 MIN")],na.rm = T) #2.83
mean(etdes_m$Rate[which(etdes_m$Type=="120 MIN")],na.rm = T) #2.83

sd(etdes_l$Rate[which(etdes_l$Type=="120 MIN")],na.rm = T) #0.09
sd(etdes_m$Rate[which(etdes_m$Type=="120 MIN")],na.rm = T) #0.09

median(etdes_l$Rate[which(etdes_l$Type=="120 MIN")],na.rm = T) #2.9
median(etdes_m$Rate[which(etdes_m$Type=="120 MIN")],na.rm = T) #2.9

t.test(etdes_l$Rate[which(etdes_l$Type=="120 MIN")],etdes_m$Rate[which(etdes_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="120 MIN")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="120 MIN")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant

#====================================  135 MIN  =======================================#

mean(etdes_l$Rate[which(etdes_l$Type=="135 MIN")],na.rm = T) #2.83
mean(etdes_m$Rate[which(etdes_m$Type=="135 MIN")],na.rm = T) #2.83

sd(etdes_l$Rate[which(etdes_l$Type=="135 MIN")],na.rm = T) #0.04
sd(etdes_m$Rate[which(etdes_m$Type=="135 MIN")],na.rm = T) #0.04

median(etdes_l$Rate[which(etdes_l$Type=="135 MIN")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="135 MIN")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="135 MIN")],etdes_m$Rate[which(etdes_m$Type=="135 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="135 MIN")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="135 MIN")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant

#======================================  150 MIN  ====================================#

mean(etdes_l$Rate[which(etdes_l$Type=="150 MIN")],na.rm = T) #2.76
mean(etdes_m$Rate[which(etdes_m$Type=="150 MIN")],na.rm = T) #2.76

sd(etdes_l$Rate[which(etdes_l$Type=="150 MIN")],na.rm = T) #0.04
sd(etdes_m$Rate[which(etdes_m$Type=="150 MIN")],na.rm = T) #0.04

median(etdes_l$Rate[which(etdes_l$Type=="150 MIN")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="150 MIN")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="150 MIN")],etdes_m$Rate[which(etdes_m$Type=="150 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="150 MIN")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="150 MIN")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant

#=======================================  165 MIN  ==================================#

mean(etdes_l$Rate[which(etdes_l$Type=="165 MIN")],na.rm = T) #2.8
mean(etdes_m$Rate[which(etdes_m$Type=="165 MIN")],na.rm = T) #2.8

sd(etdes_l$Rate[which(etdes_l$Type=="165 MIN")],na.rm = T) #0.08
sd(etdes_m$Rate[which(etdes_m$Type=="165 MIN")],na.rm = T) #0.08

median(etdes_l$Rate[which(etdes_l$Type=="165 MIN")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="165 MIN")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="165 MIN")],etdes_m$Rate[which(etdes_m$Type=="165 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="165 MIN")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="165 MIN")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
# = 3.8079, df = 41.708, p-value = 0.000453 significant

#=========================================  180 MIN  ================================#

mean(etdes_l$Rate[which(etdes_l$Type=="180 MIN")],na.rm = T) #2.76
mean(etdes_m$Rate[which(etdes_m$Type=="180 MIN")],na.rm = T) #2.76

sd(etdes_l$Rate[which(etdes_l$Type=="180 MIN")],na.rm = T) #0.047
sd(etdes_m$Rate[which(etdes_m$Type=="180 MIN")],na.rm = T) #0.047

median(etdes_l$Rate[which(etdes_l$Type=="180 MIN")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="180 MIN")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="180 MIN")],etdes_m$Rate[which(etdes_m$Type=="180 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="180 MIN")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="180 MIN")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant

#========================================  0 MIN_Emergence  ==========================#

mean(etdes_l$Rate[which(etdes_l$Type=="0 MIN_Emergence")],na.rm = T) #2.83
mean(etdes_m$Rate[which(etdes_m$Type=="0 MIN_Emergence")],na.rm = T) #2.83

sd(etdes_l$Rate[which(etdes_l$Type=="0 MIN_Emergence")],na.rm = T) #0.04
sd(etdes_m$Rate[which(etdes_m$Type=="0 MIN_Emergence")],na.rm = T) #0.04

median(etdes_l$Rate[which(etdes_l$Type=="0 MIN_Emergence")],na.rm = T) #2.8
median(etdes_m$Rate[which(etdes_m$Type=="0 MIN_Emergence")],na.rm = T) #2.8

t.test(etdes_l$Rate[which(etdes_l$Type=="0 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="0 MIN_Emergence")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="0 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant

#========================================  1 MIN_Emergence  ============================#

mean(etdes_l$Rate[which(etdes_l$Type=="1 MIN_Emergence")],na.rm = T) #0.75
mean(etdes_m$Rate[which(etdes_m$Type=="1 MIN_Emergence")],na.rm = T) #0.57

sd(etdes_l$Rate[which(etdes_l$Type=="1 MIN_Emergence")],na.rm = T) #0.11
sd(etdes_m$Rate[which(etdes_m$Type=="1 MIN_Emergence")],na.rm = T) #0.09

median(etdes_l$Rate[which(etdes_l$Type=="1 MIN_Emergence")],na.rm = T) #0.8
median(etdes_m$Rate[which(etdes_m$Type=="1 MIN_Emergence")],na.rm = T) #0.6

t.test(etdes_l$Rate[which(etdes_l$Type=="1 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#tt = 6.6469, df = 57.271, p-value = 1.206e-08 not significant
###############################################################################################################################

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="1 MIN_Emergence")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = -51.148, df = 49.344, p-value < 2.2e-16 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="1 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = -57.677, df = 46.114, p-value < 2.2e-16 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(etdes_l$Rate[which(etdes_l$Type=="2 MIN_Emergence")],na.rm = T) #0.35
mean(etdes_m$Rate[which(etdes_m$Type=="2 MIN_Emergence")],na.rm = T) #0.21

sd(etdes_l$Rate[which(etdes_l$Type=="2 MIN_Emergence")],na.rm = T) #0.12
sd(etdes_m$Rate[which(etdes_m$Type=="2 MIN_Emergence")],na.rm = T) #0.075

median(etdes_l$Rate[which(etdes_l$Type=="2 MIN_Emergence")],na.rm = T) #0.4
median(etdes_m$Rate[which(etdes_m$Type=="2 MIN_Emergence")],na.rm = T) #0.2

t.test(etdes_l$Rate[which(etdes_l$Type=="2 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 5.5818, df = 48.451, p-value = 1.057e-06 not significant
###########################################################################################################################

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="2 MIN_Emergence")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = -59.751, df = 52.211, p-value < 2.2e-16 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="2 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = -71.271, df = 39.776, p-value < 2.2e-16 significant

#==========================================  3 MIN_Emergence  ================================#

mean(etdes_l$Rate[which(etdes_l$Type=="3 MIN_Emergence")],na.rm = T) #0.06
mean(etdes_m$Rate[which(etdes_m$Type=="3 MIN_Emergence")],na.rm = T) #0

sd(etdes_l$Rate[which(etdes_l$Type=="3 MIN_Emergence")],na.rm = T) #0.06
sd(etdes_m$Rate[which(etdes_m$Type=="3 MIN_Emergence")],na.rm = T) #0

median(etdes_l$Rate[which(etdes_l$Type=="3 MIN_Emergence")],na.rm = T) #0.1
median(etdes_m$Rate[which(etdes_m$Type=="3 MIN_Emergence")],na.rm = T) #0

t.test(etdes_l$Rate[which(etdes_l$Type=="3 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 5.1877, df = 29, p-value = 1.505e-05 not significant
#############################################################################################################################

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="3 MIN_Emergence")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = -76.928, df = 37.488, p-value < 2.2e-16 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="3 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#===========================================  4 MIN_Emergence  =================================#

mean(etdes_l$Rate[which(etdes_l$Type=="4 MIN_Emergence")],na.rm = T) #0.01
mean(etdes_m$Rate[which(etdes_m$Type=="4 MIN_Emergence")],na.rm = T) #0

sd(etdes_l$Rate[which(etdes_l$Type=="4 MIN_Emergence")],na.rm = T) #0.02
sd(etdes_m$Rate[which(etdes_m$Type=="4 MIN_Emergence")],na.rm = T) #0

median(etdes_l$Rate[which(etdes_l$Type=="4 MIN_Emergence")],na.rm = T) #0
median(etdes_m$Rate[which(etdes_m$Type=="4 MIN_Emergence")],na.rm = T) #0

t.test(etdes_l$Rate[which(etdes_l$Type=="4 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 1.4329, df = 29, p-value = 0.1626 not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="4 MIN_Emergence")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = -83.171, df = 30.524, p-value < 2.2e-16 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="4 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#============================================  5 MIN_Emergence  ===================================#

mean(etdes_l$Rate[which(etdes_l$Type=="5 MIN_Emergence")],na.rm = T) #0
mean(etdes_m$Rate[which(etdes_m$Type=="5 MIN_Emergence")],na.rm = T) #0

sd(etdes_l$Rate[which(etdes_l$Type=="5 MIN_Emergence")],na.rm = T) #0
sd(etdes_m$Rate[which(etdes_m$Type=="5 MIN_Emergence")],na.rm = T) #0

median(etdes_l$Rate[which(etdes_l$Type=="5 MIN_Emergence")],na.rm = T) #0
median(etdes_m$Rate[which(etdes_m$Type=="5 MIN_Emergence")],na.rm = T) #0

t.test(etdes_l$Rate[which(etdes_l$Type=="5 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(etdes_l$Rate[which(etdes_l$Type=="5 MIN_Emergence")],etdes_l$Rate[which(etdes_l$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant
t.test(etdes_m$Rate[which(etdes_m$Type=="5 MIN_Emergence")],etdes_m$Rate[which(etdes_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#########################################################  FIDES  ################################################################

fides_rate<-read_excel("combined.xlsx",sheet = 11)
sum(is.na(fides_rate))
str(fides_rate)

#Subsetting the two study groups
fides_l<-subset(fides_rate,fides_rate$`STUDY GROUP`=="L")
fides_m<-subset(fides_rate,fides_rate$`STUDY GROUP`=="M")


#=================    5 mins ==============================#

mean(fides_l$Rate[which(fides_l$Type=="5 MINS")],na.rm = T) #2.8
mean(fides_m$Rate[which(fides_m$Type=="5 MINS")],na.rm = T) #2.8

sd(fides_l$Rate[which(fides_l$Type=="5 MINS")],na.rm = T) #0.08
sd(fides_m$Rate[which(fides_m$Type=="5 MINS")],na.rm = T) #0.08

median(fides_l$Rate[which(fides_l$Type=="5 MINS")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="5 MINS")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="5 MINS")],fides_m$Rate[which(fides_m$Type=="5 MINS")],conf.level = 0.95)
# t = 0, df = 58, p-value = 1 , Insignificant
############################################################################################################################

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="5 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant
t.test(fides_m$Rate[which(fides_m$Type=="5 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant

#============================  10 MINS  ==================================#

mean(fides_l$Rate[which(fides_l$Type=="10 MINS")],na.rm = T) #2.66
mean(fides_m$Rate[which(fides_m$Type=="10 MINS")],na.rm = T) #2.66

sd(fides_l$Rate[which(fides_l$Type=="10 MINS")],na.rm = T) #0.17
sd(fides_m$Rate[which(fides_m$Type=="10 MINS")],na.rm = T) #0.17

median(fides_l$Rate[which(fides_l$Type=="10 MINS")],na.rm = T) #2.6
median(fides_m$Rate[which(fides_m$Type=="10 MINS")],na.rm = T) #2.6

t.test(fides_l$Rate[which(fides_l$Type=="10 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 , Insignificant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="10 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(fides_m$Rate[which(fides_m$Type=="10 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=============================  15 MINS  ==================================#

mean(fides_l$Rate[which(fides_l$Type=="15 MINS")],na.rm = T) #2.8
mean(fides_m$Rate[which(fides_m$Type=="15 MINS")],na.rm = T) #2.8

sd(fides_l$Rate[which(fides_l$Type=="15 MINS")],na.rm = T) #0
sd(fides_m$Rate[which(fides_m$Type=="15 MINS")],na.rm = T) #0

median(fides_l$Rate[which(fides_l$Type=="15 MINS")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="15 MINS")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="15 MINS")],fides_m$Rate[which(fides_m$Type=="15 MINS")],conf.level = 0.95)
#t = -0.97026, df = 57.977, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="15 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.2245, df = 29, p-value = 0.0002168 significant
t.test(fides_m$Rate[which(fides_m$Type=="15 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 4.2245, df = 29, p-value = 0.0002168 significant

#==========================  30 MINS  ======================================#

mean(fides_l$Rate[which(fides_l$Type=="30 MINS")],na.rm = T) #2.83
mean(fides_m$Rate[which(fides_m$Type=="30 MINS")],na.rm = T) #2.83

sd(fides_l$Rate[which(fides_l$Type=="30 MINS")],na.rm = T) #0.09
sd(fides_m$Rate[which(fides_m$Type=="30 MINS")],na.rm = T) #0.09

median(fides_l$Rate[which(fides_l$Type=="30 MINS")],na.rm = T) #2.9
median(fides_m$Rate[which(fides_m$Type=="30 MINS")],na.rm = T) #2.9

t.test(fides_l$Rate[which(fides_l$Type=="30 MINS")],fides_m$Rate[which(fides_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="30 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant
t.test(fides_m$Rate[which(fides_m$Type=="30 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant

#=============================  45 MINS  ===================================#

mean(fides_l$Rate[which(fides_l$Type=="45 MINS")],na.rm = T) #2.73
mean(fides_m$Rate[which(fides_m$Type=="45 MINS")],na.rm = T) #2.73

sd(fides_l$Rate[which(fides_l$Type=="45 MINS")],na.rm = T) #0.09
sd(fides_m$Rate[which(fides_m$Type=="45 MINS")],na.rm = T) #0.09

median(fides_l$Rate[which(fides_l$Type=="45 MINS")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="45 MINS")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="45 MINS")],fides_m$Rate[which(fides_m$Type=="45 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1  significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="45 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 1.8471, df = 45.303, p-value = 0.07127 significant
t.test(fides_m$Rate[which(fides_m$Type=="45 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.8471, df = 45.303, p-value = 0.07127 significant

#===============================  60 MINS  ================================#

mean(fides_l$Rate[which(fides_l$Type=="60 MINS")],na.rm = T) #2.76
mean(fides_m$Rate[which(fides_m$Type=="60 MINS")],na.rm = T) #2.76

sd(fides_l$Rate[which(fides_l$Type=="60 MINS")],na.rm = T) #0.04
sd(fides_m$Rate[which(fides_m$Type=="60 MINS")],na.rm = T) #0.04

median(fides_l$Rate[which(fides_l$Type=="60 MINS")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="60 MINS")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="60 MINS")],fides_m$Rate[which(fides_m$Type=="60 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="60 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant
t.test(fides_m$Rate[which(fides_m$Type=="60 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant

#=============================  75 MINS ===================================#

mean(fides_l$Rate[which(fides_l$Type=="75 MINS")],na.rm = T) #2.8
mean(fides_m$Rate[which(fides_m$Type=="75 MINS")],na.rm = T) #2.8

sd(fides_l$Rate[which(fides_l$Type=="75 MINS")],na.rm = T) #0.08
sd(fides_m$Rate[which(fides_m$Type=="75 MINS")],na.rm = T) #0.08

median(fides_l$Rate[which(fides_l$Type=="75 MINS")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="75 MINS")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="75 MINS")],fides_m$Rate[which(fides_m$Type=="75 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="75 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant
t.test(fides_m$Rate[which(fides_m$Type=="75 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant

#============================== 90 MINS  ==================================#

mean(fides_l$Rate[which(fides_l$Type=="90 MINS")],na.rm = T) #2.86
mean(fides_m$Rate[which(fides_m$Type=="90 MINS")],na.rm = T) #2.86

sd(fides_l$Rate[which(fides_l$Type=="90 MINS")],na.rm = T) #0.04
sd(fides_m$Rate[which(fides_m$Type=="90 MINS")],na.rm = T) #0.04

median(fides_l$Rate[which(fides_l$Type=="90 MINS")],na.rm = T) #2.9
median(fides_m$Rate[which(fides_m$Type=="90 MINS")],na.rm = T) #2.9

t.test(fides_l$Rate[which(fides_l$Type=="90 MINS")],fides_m$Rate[which(fides_m$Type=="90 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="90 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant
t.test(fides_m$Rate[which(fides_m$Type=="90 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant

#===================================  105 MINS  ==================================#

mean(fides_l$Rate[which(fides_l$Type=="105 MINS")],na.rm = T) #2.86
mean(fides_m$Rate[which(fides_m$Type=="105 MINS")],na.rm = T) #2.86

sd(fides_l$Rate[which(fides_l$Type=="105 MINS")],na.rm = T) #0.04
sd(fides_m$Rate[which(fides_m$Type=="105 MINS")],na.rm = T) #0.04

median(fides_l$Rate[which(fides_l$Type=="105 MINS")],na.rm = T) #2.9
median(fides_m$Rate[which(fides_m$Type=="105 MINS")],na.rm = T) #2.9

t.test(fides_l$Rate[which(fides_l$Type=="105 MINS")],fides_m$Rate[which(fides_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="105 MINS")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant
t.test(fides_m$Rate[which(fides_m$Type=="105 MINS")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.1062, df = 33.435, p-value = 6.702e-07 significant

#=====================================  120 MINS  ===============================#

mean(fides_l$Rate[which(fides_l$Type=="120 MIN")],na.rm = T) #2.83
mean(fides_m$Rate[which(fides_m$Type=="120 MIN")],na.rm = T) #2.83

sd(fides_l$Rate[which(fides_l$Type=="120 MIN")],na.rm = T) #0.09
sd(fides_m$Rate[which(fides_m$Type=="120 MIN")],na.rm = T) #0.09

median(fides_l$Rate[which(fides_l$Type=="120 MIN")],na.rm = T) #2.9
median(fides_m$Rate[which(fides_m$Type=="120 MIN")],na.rm = T) #2.9

t.test(fides_l$Rate[which(fides_l$Type=="120 MIN")],fides_m$Rate[which(fides_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="120 MIN")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant
t.test(fides_m$Rate[which(fides_m$Type=="120 MIN")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 4.6177, df = 45.303, p-value = 3.21e-05 significant

#====================================  135 MIN  =======================================#

mean(fides_l$Rate[which(fides_l$Type=="135 MIN")],na.rm = T) #2.83
mean(fides_m$Rate[which(fides_m$Type=="135 MIN")],na.rm = T) #2.83

sd(fides_l$Rate[which(fides_l$Type=="135 MIN")],na.rm = T) #0.04
sd(fides_m$Rate[which(fides_m$Type=="135 MIN")],na.rm = T) #0.04

median(fides_l$Rate[which(fides_l$Type=="135 MIN")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="135 MIN")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="135 MIN")],fides_m$Rate[which(fides_m$Type=="135 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="135 MIN")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant
t.test(fides_m$Rate[which(fides_m$Type=="135 MIN")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant

#======================================  150 MIN  ====================================#

mean(fides_l$Rate[which(fides_l$Type=="150 MIN")],na.rm = T) #2.76
mean(fides_m$Rate[which(fides_m$Type=="150 MIN")],na.rm = T) #2.76

sd(fides_l$Rate[which(fides_l$Type=="150 MIN")],na.rm = T) #0.04
sd(fides_m$Rate[which(fides_m$Type=="150 MIN")],na.rm = T) #0.04

median(fides_l$Rate[which(fides_l$Type=="150 MIN")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="150 MIN")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="150 MIN")],fides_m$Rate[which(fides_m$Type=="150 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="150 MIN")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant
t.test(fides_m$Rate[which(fides_m$Type=="150 MIN")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant

#=======================================  165 MIN  ==================================#

mean(fides_l$Rate[which(fides_l$Type=="165 MIN")],na.rm = T) #2.8
mean(fides_m$Rate[which(fides_m$Type=="165 MIN")],na.rm = T) #2.8

sd(fides_l$Rate[which(fides_l$Type=="165 MIN")],na.rm = T) #0.08
sd(fides_m$Rate[which(fides_m$Type=="165 MIN")],na.rm = T) #0.08

median(fides_l$Rate[which(fides_l$Type=="165 MIN")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="165 MIN")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="165 MIN")],fides_m$Rate[which(fides_m$Type=="165 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="165 MIN")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.8079, df = 41.708, p-value = 0.000453 significant
t.test(fides_m$Rate[which(fides_m$Type=="165 MIN")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
# = 3.8079, df = 41.708, p-value = 0.000453 significant

#=========================================  180 MIN  ================================#

mean(fides_l$Rate[which(fides_l$Type=="180 MIN")],na.rm = T) #2.76
mean(fides_m$Rate[which(fides_m$Type=="180 MIN")],na.rm = T) #2.76

sd(fides_l$Rate[which(fides_l$Type=="180 MIN")],na.rm = T) #0.047
sd(fides_m$Rate[which(fides_m$Type=="180 MIN")],na.rm = T) #0.047

median(fides_l$Rate[which(fides_l$Type=="180 MIN")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="180 MIN")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="180 MIN")],fides_m$Rate[which(fides_m$Type=="180 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="180 MIN")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant
t.test(fides_m$Rate[which(fides_m$Type=="180 MIN")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 3.0531, df = 33.435, p-value = 0.00442 significant

#========================================  0 MIN_Emergence  ==========================#

mean(fides_l$Rate[which(fides_l$Type=="0 MIN_Emergence")],na.rm = T) #2.83
mean(fides_m$Rate[which(fides_m$Type=="0 MIN_Emergence")],na.rm = T) #2.83

sd(fides_l$Rate[which(fides_l$Type=="0 MIN_Emergence")],na.rm = T) #0.04
sd(fides_m$Rate[which(fides_m$Type=="0 MIN_Emergence")],na.rm = T) #0.04

median(fides_l$Rate[which(fides_l$Type=="0 MIN_Emergence")],na.rm = T) #2.8
median(fides_m$Rate[which(fides_m$Type=="0 MIN_Emergence")],na.rm = T) #2.8

t.test(fides_l$Rate[which(fides_l$Type=="0 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="0 MIN_Emergence")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant
t.test(fides_m$Rate[which(fides_m$Type=="0 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.0885, df = 33.435, p-value = 1.375e-05 significant

#========================================  1 MIN_Emergence  ============================#

mean(fides_l$Rate[which(fides_l$Type=="1 MIN_Emergence")],na.rm = T) #0.72
mean(fides_m$Rate[which(fides_m$Type=="1 MIN_Emergence")],na.rm = T) #0.57

sd(fides_l$Rate[which(fides_l$Type=="1 MIN_Emergence")],na.rm = T) #0.14
sd(fides_m$Rate[which(fides_m$Type=="1 MIN_Emergence")],na.rm = T) #0.10

median(fides_l$Rate[which(fides_l$Type=="1 MIN_Emergence")],na.rm = T) #0.7
median(fides_m$Rate[which(fides_m$Type=="1 MIN_Emergence")],na.rm = T) #0.6

t.test(fides_l$Rate[which(fides_l$Type=="1 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 4.3112, df = 53.032, p-value = 7.086e-05 not significant
###############################################################################################################################

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="1 MIN_Emergence")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = -46.966, df = 56.558, p-value < 2.2e-16 significant
t.test(fides_m$Rate[which(fides_m$Type=="1 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = -56.267, df = 48.447, p-value < 2.2e-16 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(fides_l$Rate[which(fides_l$Type=="2 MIN_Emergence")],na.rm = T) #0.34
mean(fides_m$Rate[which(fides_m$Type=="2 MIN_Emergence")],na.rm = T) #0.21

sd(fides_l$Rate[which(fides_l$Type=="2 MIN_Emergence")],na.rm = T) #0.13
sd(fides_m$Rate[which(fides_m$Type=="2 MIN_Emergence")],na.rm = T) #0.075

median(fides_l$Rate[which(fides_l$Type=="2 MIN_Emergence")],na.rm = T) #0.4
median(fides_m$Rate[which(fides_m$Type=="2 MIN_Emergence")],na.rm = T) #0.2

t.test(fides_l$Rate[which(fides_l$Type=="2 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 4.6534, df = 46.099, p-value = 2.775e-05 not significant
###########################################################################################################################

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="2 MIN_Emergence")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = -58.447, df = 54.4, p-value < 2.2e-16 significant
t.test(fides_m$Rate[which(fides_m$Type=="2 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = -71.271, df = 39.776, p-value < 2.2e-16 significant

#==========================================  3 MIN_Emergence  ================================#

mean(fides_l$Rate[which(fides_l$Type=="3 MIN_Emergence")],na.rm = T) #0.06
mean(fides_m$Rate[which(fides_m$Type=="3 MIN_Emergence")],na.rm = T) #0

sd(fides_l$Rate[which(fides_l$Type=="3 MIN_Emergence")],na.rm = T) #0.06
sd(fides_m$Rate[which(fides_m$Type=="3 MIN_Emergence")],na.rm = T) #0

median(fides_l$Rate[which(fides_l$Type=="3 MIN_Emergence")],na.rm = T) #0.1
median(fides_m$Rate[which(fides_m$Type=="3 MIN_Emergence")],na.rm = T) #0

t.test(fides_l$Rate[which(fides_l$Type=="3 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 5.1877, df = 29, p-value = 1.505e-05 not significant
#############################################################################################################################

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="3 MIN_Emergence")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = -76.928, df = 37.488, p-value < 2.2e-16 significant
t.test(fides_m$Rate[which(fides_m$Type=="3 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#===========================================  4 MIN_Emergence  =================================#

mean(fides_l$Rate[which(fides_l$Type=="4 MIN_Emergence")],na.rm = T) #0.01
mean(fides_m$Rate[which(fides_m$Type=="4 MIN_Emergence")],na.rm = T) #0

sd(fides_l$Rate[which(fides_l$Type=="4 MIN_Emergence")],na.rm = T) #0.02
sd(fides_m$Rate[which(fides_m$Type=="4 MIN_Emergence")],na.rm = T) #0

median(fides_l$Rate[which(fides_l$Type=="4 MIN_Emergence")],na.rm = T) #0
median(fides_m$Rate[which(fides_m$Type=="4 MIN_Emergence")],na.rm = T) #0

t.test(fides_l$Rate[which(fides_l$Type=="4 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 1.4329, df = 29, p-value = 0.1626 not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="4 MIN_Emergence")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = -83.171, df = 30.524, p-value < 2.2e-16 significant
t.test(fides_m$Rate[which(fides_m$Type=="4 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#============================================  5 MIN_Emergence  ===================================#

mean(fides_l$Rate[which(fides_l$Type=="5 MIN_Emergence")],na.rm = T) #0
mean(fides_m$Rate[which(fides_m$Type=="5 MIN_Emergence")],na.rm = T) #0

sd(fides_l$Rate[which(fides_l$Type=="5 MIN_Emergence")],na.rm = T) #0
sd(fides_m$Rate[which(fides_m$Type=="5 MIN_Emergence")],na.rm = T) #0

median(fides_l$Rate[which(fides_l$Type=="5 MIN_Emergence")],na.rm = T) #0
median(fides_m$Rate[which(fides_m$Type=="5 MIN_Emergence")],na.rm = T) #0

t.test(fides_l$Rate[which(fides_l$Type=="5 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(fides_l$Rate[which(fides_l$Type=="5 MIN_Emergence")],fides_l$Rate[which(fides_l$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant
t.test(fides_m$Rate[which(fides_m$Type=="5 MIN_Emergence")],fides_m$Rate[which(fides_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#########################################################  MAC  #################################################################

mac_rate<-read_excel("combined.xlsx",sheet = 12)
sum(is.na(mac_rate))
str(mac_rate)

#Subsetting the two study groups
mac_l<-subset(mac_rate,mac_rate$`STUDY GROUP`=="L")
mac_m<-subset(mac_rate,mac_rate$`STUDY GROUP`=="M")


#=================    5 mins ==============================#

mean(mac_l$Rate[which(mac_l$Type=="5 MINS")],na.rm = T) #0.9
mean(mac_m$Rate[which(mac_m$Type=="5 MINS")],na.rm = T) #0.92

sd(mac_l$Rate[which(mac_l$Type=="5 MINS")],na.rm = T) #0.07
sd(mac_m$Rate[which(mac_m$Type=="5 MINS")],na.rm = T) #0.06

median(mac_l$Rate[which(mac_l$Type=="5 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="5 MINS")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="5 MINS")],mac_m$Rate[which(mac_m$Type=="5 MINS")],conf.level = 0.95)
# t = -1.2289, df = 56.761, p-value = 0.2242 , Insignificant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="5 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 2.0095, df = 54.771, p-value = 0.04942 significant
t.test(mac_m$Rate[which(mac_m$Type=="5 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 4.7352, df = 53.751, p-value = 1.637e-05 significant

#============================  10 MINS  ==================================#

mean(mac_l$Rate[which(mac_l$Type=="10 MINS")],na.rm = T) #0.86
mean(mac_m$Rate[which(mac_m$Type=="10 MINS")],na.rm = T) #0.85

sd(mac_l$Rate[which(mac_l$Type=="10 MINS")],na.rm = T) #0.06
sd(mac_m$Rate[which(mac_m$Type=="10 MINS")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="10 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="10 MINS")],na.rm = T) #0.85

t.test(mac_l$Rate[which(mac_l$Type=="10 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.91517, df = 56.026, p-value = 0.364 , Insignificant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="10 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(mac_m$Rate[which(mac_m$Type=="10 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=============================  15 MINS  ==================================#

mean(mac_l$Rate[which(mac_l$Type=="15 MINS")],na.rm = T) #0.83
mean(mac_m$Rate[which(mac_m$Type=="15 MINS")],na.rm = T) #0.82

sd(mac_l$Rate[which(mac_l$Type=="15 MINS")],na.rm = T) #0.06
sd(mac_m$Rate[which(mac_m$Type=="15 MINS")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="15 MINS")],na.rm = T) #0.8
median(mac_m$Rate[which(mac_m$Type=="15 MINS")],na.rm = T) #0.8

t.test(mac_l$Rate[which(mac_l$Type=="15 MINS")],mac_m$Rate[which(mac_m$Type=="15 MINS")],conf.level = 0.95)
#t = 0.87216, df = 53.911, p-value = 0.387 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="15 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -1.6078, df = 57.598, p-value = 0.1134 significant
t.test(mac_m$Rate[which(mac_m$Type=="15 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = -2.04, df = 57.995, p-value = 0.04592 significant

#==========================  30 MINS  ======================================#

mean(mac_l$Rate[which(mac_l$Type=="30 MINS")],na.rm = T) #0.88
mean(mac_m$Rate[which(mac_m$Type=="30 MINS")],na.rm = T) #0.87

sd(mac_l$Rate[which(mac_l$Type=="30 MINS")],na.rm = T) #0.06
sd(mac_m$Rate[which(mac_m$Type=="30 MINS")],na.rm = T) #0.06

median(mac_l$Rate[which(mac_l$Type=="30 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="30 MINS")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="30 MINS")],mac_m$Rate[which(mac_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0.59189, df = 57.588, p-value = 0.5562 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="30 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 1.3923, df = 57.399, p-value = 0.1692 significant
t.test(mac_m$Rate[which(mac_m$Type=="30 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.8108, df = 55.662, p-value = 0.07556 significant

#=============================  45 MINS  ===================================#

mean(mac_l$Rate[which(mac_l$Type=="45 MINS")],na.rm = T) #0.86
mean(mac_m$Rate[which(mac_m$Type=="45 MINS")],na.rm = T) #0.87

sd(mac_l$Rate[which(mac_l$Type=="45 MINS")],na.rm = T) #0.08
sd(mac_m$Rate[which(mac_m$Type=="45 MINS")],na.rm = T) #0.06

median(mac_l$Rate[which(mac_l$Type=="45 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="45 MINS")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="45 MINS")],mac_m$Rate[which(mac_m$Type=="45 MINS")],conf.level = 0.95)
#t = -0.54421, df = 56.21, p-value = 0.5885  significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="45 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0.12429, df = 53.653, p-value = 0.9015 significant
t.test(mac_m$Rate[which(mac_m$Type=="45 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.6858, df = 53.402, p-value = 0.09768 significant

#===============================  60 MINS  ================================#

mean(mac_l$Rate[which(mac_l$Type=="60 MINS")],na.rm = T) #0.87
mean(mac_m$Rate[which(mac_m$Type=="60 MINS")],na.rm = T) #0.85

sd(mac_l$Rate[which(mac_l$Type=="60 MINS")],na.rm = T) #0.06
sd(mac_m$Rate[which(mac_m$Type=="60 MINS")],na.rm = T) #0.06

median(mac_l$Rate[which(mac_l$Type=="60 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="60 MINS")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="60 MINS")],mac_m$Rate[which(mac_m$Type=="60 MINS")],conf.level = 0.95)
#t = 1.187, df = 57.745, p-value = 0.2401 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="60 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0.98062, df = 57.521, p-value = 0.3309 significant
t.test(mac_m$Rate[which(mac_m$Type=="60 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.65361, df = 55.511, p-value = 0.5161 significant

#=============================  75 MINS ===================================#

mean(mac_l$Rate[which(mac_l$Type=="75 MINS")],na.rm = T) #0.88
mean(mac_m$Rate[which(mac_m$Type=="75 MINS")],na.rm = T) #0.87

sd(mac_l$Rate[which(mac_l$Type=="75 MINS")],na.rm = T) #0.06
sd(mac_m$Rate[which(mac_m$Type=="75 MINS")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="75 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="75 MINS")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="75 MINS")],mac_m$Rate[which(mac_m$Type=="75 MINS")],conf.level = 0.95)
#t = 0.43255, df = 57.882, p-value = 0.667 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="75 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 1.0537, df = 57.997, p-value = 0.2964 significant
t.test(mac_m$Rate[which(mac_m$Type=="75 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.6515, df = 56.943, p-value = 0.1041 significant

#============================== 90 MINS  ==================================#

mean(mac_l$Rate[which(mac_l$Type=="90 MINS")],na.rm = T) #0.87
mean(mac_m$Rate[which(mac_m$Type=="90 MINS")],na.rm = T) #0.87

sd(mac_l$Rate[which(mac_l$Type=="90 MINS")],na.rm = T) #0.06
sd(mac_m$Rate[which(mac_m$Type=="90 MINS")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="90 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="90 MINS")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="90 MINS")],mac_m$Rate[which(mac_m$Type=="90 MINS")],conf.level = 0.95)
#t = 0.21337, df = 57.712, p-value = 0.8318 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="90 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0.83219, df = 57.981, p-value = 0.4087 significant
t.test(mac_m$Rate[which(mac_m$Type=="90 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.6515, df = 56.943, p-value = 0.1041 significant

#===================================  105 MINS  ==================================#

mean(mac_l$Rate[which(mac_l$Type=="105 MINS")],na.rm = T) #0.86
mean(mac_m$Rate[which(mac_m$Type=="105 MINS")],na.rm = T) #0.86

sd(mac_l$Rate[which(mac_l$Type=="105 MINS")],na.rm = T) #0.04
sd(mac_m$Rate[which(mac_m$Type=="105 MINS")],na.rm = T) #0.04

median(mac_l$Rate[which(mac_l$Type=="105 MINS")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="105 MINS")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="105 MINS")],mac_m$Rate[which(mac_m$Type=="105 MINS")],conf.level = 0.95)
#t = 0.26122, df = 57.984, p-value = 0.7948 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="105 MINS")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 55.251, p-value = 1 significant
t.test(mac_m$Rate[which(mac_m$Type=="105 MINS")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.76931, df = 57.976, p-value = 0.4448 significant

#=====================================  120 MINS  ===============================#

mean(mac_l$Rate[which(mac_l$Type=="120 MIN")],na.rm = T) #0.86
mean(mac_m$Rate[which(mac_m$Type=="120 MIN")],na.rm = T) #0.86

sd(mac_l$Rate[which(mac_l$Type=="120 MIN")],na.rm = T) #0.04
sd(mac_m$Rate[which(mac_m$Type=="120 MIN")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="120 MIN")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="120 MIN")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="120 MIN")],mac_m$Rate[which(mac_m$Type=="120 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="120 MIN")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0.23414, df = 54.745, p-value = 0.8158 significant
t.test(mac_m$Rate[which(mac_m$Type=="120 MIN")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.2226, df = 57.7, p-value = 0.2264 significant

#====================================  135 MIN  =======================================#

mean(mac_l$Rate[which(mac_l$Type=="135 MIN")],na.rm = T) #0.85
mean(mac_m$Rate[which(mac_m$Type=="135 MIN")],na.rm = T) #0.86

sd(mac_l$Rate[which(mac_l$Type=="135 MIN")],na.rm = T) #0.05
sd(mac_m$Rate[which(mac_m$Type=="135 MIN")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="135 MIN")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="135 MIN")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="135 MIN")],mac_m$Rate[which(mac_m$Type=="135 MIN")],conf.level = 0.95)
#t = -0.48655, df = 57.449, p-value = 0.6284 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="135 MIN")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -0.45925, df = 55.847, p-value = 0.6478 significant
t.test(mac_m$Rate[which(mac_m$Type=="135 MIN")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.96916, df = 57.543, p-value = 0.3365 significant

#======================================  150 MIN  ====================================#

mean(mac_l$Rate[which(mac_l$Type=="150 MIN")],na.rm = T) #0.8
mean(mac_m$Rate[which(mac_m$Type=="150 MIN")],na.rm = T) #0.81

sd(mac_l$Rate[which(mac_l$Type=="150 MIN")],na.rm = T) #0.01
sd(mac_m$Rate[which(mac_m$Type=="150 MIN")],na.rm = T) #0.04

median(mac_l$Rate[which(mac_l$Type=="150 MIN")],na.rm = T) #0.8
median(mac_m$Rate[which(mac_m$Type=="150 MIN")],na.rm = T) #0.8

t.test(mac_l$Rate[which(mac_l$Type=="150 MIN")],mac_m$Rate[which(mac_m$Type=="150 MIN")],conf.level = 0.95)
#t = -0.82605, df = 40.445, p-value = 0.4136 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="150 MIN")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -5.1231, df = 34.073, p-value = 1.183e-05 significant
t.test(mac_m$Rate[which(mac_m$Type=="150 MIN")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = -3.3778, df = 55.098, p-value = 0.001347 significant

#=======================================  165 MIN  ==================================#

mean(mac_l$Rate[which(mac_l$Type=="165 MIN")],na.rm = T) #0.85
mean(mac_m$Rate[which(mac_m$Type=="165 MIN")],na.rm = T) #0.85

sd(mac_l$Rate[which(mac_l$Type=="165 MIN")],na.rm = T) #0.05
sd(mac_m$Rate[which(mac_m$Type=="165 MIN")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="165 MIN")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="165 MIN")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="165 MIN")],mac_m$Rate[which(mac_m$Type=="165 MIN")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="165 MIN")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -0.45925, df = 55.847, p-value = 0.6478 significant
t.test(mac_m$Rate[which(mac_m$Type=="165 MIN")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.50999, df = 57.995, p-value = 0.612 significant

#=========================================  180 MIN  ================================#

mean(mac_l$Rate[which(mac_l$Type=="180 MIN")],na.rm = T) #0.85
mean(mac_m$Rate[which(mac_m$Type=="180 MIN")],na.rm = T) #0.86

sd(mac_l$Rate[which(mac_l$Type=="180 MIN")],na.rm = T) #0.05
sd(mac_m$Rate[which(mac_m$Type=="180 MIN")],na.rm = T) #0.04

median(mac_l$Rate[which(mac_l$Type=="180 MIN")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="180 MIN")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="180 MIN")],mac_m$Rate[which(mac_m$Type=="180 MIN")],conf.level = 0.95)
#t = -0.51939, df = 57.955, p-value = 0.6055 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="180 MIN")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -0.45925, df = 55.847, p-value = 0.6478 significant
t.test(mac_m$Rate[which(mac_m$Type=="180 MIN")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.034, df = 57.921, p-value = 0.3054 significant

#========================================  0 MIN_Emergence  ==========================#

mean(mac_l$Rate[which(mac_l$Type=="0 MIN_Emergence")],na.rm = T) #0.85
mean(mac_m$Rate[which(mac_m$Type=="0 MIN_Emergence")],na.rm = T) #0.85

sd(mac_l$Rate[which(mac_l$Type=="0 MIN_Emergence")],na.rm = T) #0.05
sd(mac_m$Rate[which(mac_m$Type=="0 MIN_Emergence")],na.rm = T) #0.05

median(mac_l$Rate[which(mac_l$Type=="0 MIN_Emergence")],na.rm = T) #0.9
median(mac_m$Rate[which(mac_m$Type=="0 MIN_Emergence")],na.rm = T) #0.9

t.test(mac_l$Rate[which(mac_l$Type=="0 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = -0.23964, df = 57.27, p-value = 0.8115 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="0 MIN_Emergence")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -0.687, df = 55.982, p-value = 0.4949 significant
t.test(mac_m$Rate[which(mac_m$Type=="0 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.4788, df = 57.298, p-value = 0.6339 significant

#========================================  1 MIN_Emergence  ============================#

mean(mac_l$Rate[which(mac_l$Type=="1 MIN_Emergence")],na.rm = T) #0.43
mean(mac_m$Rate[which(mac_m$Type=="1 MIN_Emergence")],na.rm = T) #0.44

sd(mac_l$Rate[which(mac_l$Type=="1 MIN_Emergence")],na.rm = T) #0.15
sd(mac_m$Rate[which(mac_m$Type=="1 MIN_Emergence")],na.rm = T) #0.15

median(mac_l$Rate[which(mac_l$Type=="1 MIN_Emergence")],na.rm = T) #0.4
median(mac_m$Rate[which(mac_m$Type=="1 MIN_Emergence")],na.rm = T) #0.4

t.test(mac_l$Rate[which(mac_l$Type=="1 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = -0.42826, df = 57.998, p-value = 0.67 not significant


#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="1 MIN_Emergence")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -14.541, df = 38.338, p-value < 2.2e-16 significant
t.test(mac_m$Rate[which(mac_m$Type=="1 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = -13.927, df = 35.559, p-value = 5.738e-16 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(mac_l$Rate[which(mac_l$Type=="2 MIN_Emergence")],na.rm = T) #0.23
mean(mac_m$Rate[which(mac_m$Type=="2 MIN_Emergence")],na.rm = T) #0.24

sd(mac_l$Rate[which(mac_l$Type=="2 MIN_Emergence")],na.rm = T) #0.10
sd(mac_m$Rate[which(mac_m$Type=="2 MIN_Emergence")],na.rm = T) #0.10

median(mac_l$Rate[which(mac_l$Type=="2 MIN_Emergence")],na.rm = T) #0.2
median(mac_m$Rate[which(mac_m$Type=="2 MIN_Emergence")],na.rm = T) #0.2

t.test(mac_l$Rate[which(mac_l$Type=="2 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = -0.61562, df = 57.857, p-value = 0.5406 not significant


#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="2 MIN_Emergence")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -58.447, df = 54.4, p-value < 2.2e-16 significant
t.test(mac_m$Rate[which(mac_m$Type=="2 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = -71.271, df = 39.776, p-value < 2.2e-16 significant

#==========================================  3 MIN_Emergence  ================================#

mean(mac_l$Rate[which(mac_l$Type=="3 MIN_Emergence")],na.rm = T) #0.09
mean(mac_m$Rate[which(mac_m$Type=="3 MIN_Emergence")],na.rm = T) #0.10

sd(mac_l$Rate[which(mac_l$Type=="3 MIN_Emergence")],na.rm = T) #0.09
sd(mac_m$Rate[which(mac_m$Type=="3 MIN_Emergence")],na.rm = T) #0.10

median(mac_l$Rate[which(mac_l$Type=="3 MIN_Emergence")],na.rm = T) #0.1
median(mac_m$Rate[which(mac_m$Type=="3 MIN_Emergence")],na.rm = T) #0.1

t.test(mac_l$Rate[which(mac_l$Type=="3 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = -0.50081, df = 57.724, p-value = 0.6184 not significant


#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="3 MIN_Emergence")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -76.928, df = 37.488, p-value < 2.2e-16 significant
t.test(mac_m$Rate[which(mac_m$Type=="3 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#===========================================  4 MIN_Emergence  =================================#

mean(mac_l$Rate[which(mac_l$Type=="4 MIN_Emergence")],na.rm = T) #0.02
mean(mac_m$Rate[which(mac_m$Type=="4 MIN_Emergence")],na.rm = T) #0.03

sd(mac_l$Rate[which(mac_l$Type=="4 MIN_Emergence")],na.rm = T) #0.04
sd(mac_m$Rate[which(mac_m$Type=="4 MIN_Emergence")],na.rm = T) #0.06

median(mac_l$Rate[which(mac_l$Type=="4 MIN_Emergence")],na.rm = T) #0
median(mac_m$Rate[which(mac_m$Type=="4 MIN_Emergence")],na.rm = T) #0

t.test(mac_l$Rate[which(mac_l$Type=="4 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = -0.69458, df = 49.834, p-value = 0.4905 not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="4 MIN_Emergence")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -83.171, df = 30.524, p-value < 2.2e-16 significant
t.test(mac_m$Rate[which(mac_m$Type=="4 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant

#============================================  5 MIN_Emergence  ===================================#

mean(mac_l$Rate[which(mac_l$Type=="5 MIN_Emergence")],na.rm = T) #0
mean(mac_m$Rate[which(mac_m$Type=="5 MIN_Emergence")],na.rm = T) #0

sd(mac_l$Rate[which(mac_l$Type=="5 MIN_Emergence")],na.rm = T) #0
sd(mac_m$Rate[which(mac_m$Type=="5 MIN_Emergence")],na.rm = T) #0

median(mac_l$Rate[which(mac_l$Type=="5 MIN_Emergence")],na.rm = T) #0
median(mac_m$Rate[which(mac_m$Type=="5 MIN_Emergence")],na.rm = T) #0

t.test(mac_l$Rate[which(mac_l$Type=="5 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = NaN, df = NaN, p-value = NA not significant

#Intragroup
t.test(mac_l$Rate[which(mac_l$Type=="5 MIN_Emergence")],mac_l$Rate[which(mac_l$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant
t.test(mac_m$Rate[which(mac_m$Type=="5 MIN_Emergence")],mac_m$Rate[which(mac_m$Type=="10 MINS")],conf.level = 0.95)
#t = -84.489, df = 29, p-value < 2.2e-16 significant


#########################################################  ETO2  ################################################################

eto2_rate<-read_excel("combined.xlsx",sheet = 13)
sum(is.na(eto2_rate))
str(eto2_rate)

#Subsetting the two study groups
eto2_l<-subset(eto2_rate,eto2_rate$`STUDY GROUP`=="L")
eto2_m<-subset(eto2_rate,eto2_rate$`STUDY GROUP`=="M")

#===============================  INDUCTION  ===========================#

mean(eto2_l$Rate[which(eto2_l$Type=="INDUCTION")],na.rm = T) #96
mean(eto2_m$Rate[which(eto2_m$Type=="INDUCTION")],na.rm = T) #95.25

sd(eto2_l$Rate[which(eto2_l$Type=="INDUCTION")],na.rm = T) #1.11
sd(eto2_m$Rate[which(eto2_m$Type=="INDUCTION")],na.rm = T) #1.11

median(eto2_l$Rate[which(eto2_l$Type=="INDUCTION")],na.rm = T) #96
median(eto2_m$Rate[which(eto2_m$Type=="INDUCTION")],na.rm = T) #96

t.test(eto2_l$Rate[which(eto2_l$Type=="INDUCTION")],eto2_m$Rate[which(eto2_m$Type=="INDUCTION")],conf.level = 0.95)
#t = 0.86882, df = 25.714, p-value = 0.393 , hence insignificant difference

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="INDUCTION")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 85.255, df = 37.674, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="INDUCTION")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 50.303, df = 33.947, p-value < 2.2e-16 significant

#=========================  INTUBATION  ==============================#

mean(eto2_l$Rate[which(eto2_l$Type=="INTUBATION")],na.rm = T) #95.4
mean(eto2_m$Rate[which(eto2_m$Type=="INTUBATION")],na.rm = T) #96.25

sd(eto2_l$Rate[which(eto2_l$Type=="INTUBATION")],na.rm = T) #1.77
sd(eto2_m$Rate[which(eto2_m$Type=="INTUBATION")],na.rm = T) #2.43

median(eto2_l$Rate[which(eto2_l$Type=="INTUBATION")],na.rm = T) #94
median(eto2_m$Rate[which(eto2_m$Type=="INTUBATION")],na.rm = T) #96.5

t.test(eto2_l$Rate[which(eto2_l$Type=="INTUBATION")],eto2_m$Rate[which(eto2_m$Type=="INTUBATION")],conf.level = 0.95)
#t = -1.4325, df = 40.816, p-value = 0.1596 , hence insignificant difference

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="INTUBATION")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 76.733, df = 48.55, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="INTUBATION")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 74.292, df = 47.715, p-value < 2.2e-16 significant

#=================    5 mins ==============================#

mean(eto2_l$Rate[which(eto2_l$Type=="5 MINS")],na.rm = T) #54.2
mean(eto2_m$Rate[which(eto2_m$Type=="5 MINS")],na.rm = T) #56

sd(eto2_l$Rate[which(eto2_l$Type=="5 MINS")],na.rm = T) #5.9
sd(eto2_m$Rate[which(eto2_m$Type=="5 MINS")],na.rm = T) #5.75

median(eto2_l$Rate[which(eto2_l$Type=="5 MINS")],na.rm = T) #54
median(eto2_m$Rate[which(eto2_m$Type=="5 MINS")],na.rm = T) #57

t.test(eto2_l$Rate[which(eto2_l$Type=="5 MINS")],eto2_m$Rate[which(eto2_m$Type=="5 MINS")],conf.level = 0.95)
#t = -1.1953, df = 57.958, p-value = 0.2368 , Insignificant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="5 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 4.8426, df = 41.78, p-value = 1.79e-05 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="5 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 6.9092, df = 37.779, p-value = 3.364e-08 significant

#============================  10 MINS  ==================================#

mean(eto2_l$Rate[which(eto2_l$Type=="10 MINS")],na.rm = T) #48.4
mean(eto2_m$Rate[which(eto2_m$Type=="10 MINS")],na.rm = T) #48.2

sd(eto2_l$Rate[which(eto2_l$Type=="10 MINS")],na.rm = T) #2.84
sd(eto2_m$Rate[which(eto2_m$Type=="10 MINS")],na.rm = T) #2.26

median(eto2_l$Rate[which(eto2_l$Type=="10 MINS")],na.rm = T) #49
median(eto2_m$Rate[which(eto2_m$Type=="10 MINS")],na.rm = T) #50

t.test(eto2_l$Rate[which(eto2_l$Type=="10 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0.30104, df = 55.205, p-value = 0.7645 , Insignificant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="10 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="10 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 0, df = 58, p-value = 1 significant

#=============================  15 MINS  ==================================#

mean(eto2_l$Rate[which(eto2_l$Type=="15 MINS")],na.rm = T) #47
mean(eto2_m$Rate[which(eto2_m$Type=="15 MINS")],na.rm = T) #45.8

sd(eto2_l$Rate[which(eto2_l$Type=="15 MINS")],na.rm = T) #3.08
sd(eto2_m$Rate[which(eto2_m$Type=="15 MINS")],na.rm = T) #2.52

median(eto2_l$Rate[which(eto2_l$Type=="15 MINS")],na.rm = T) #47
median(eto2_m$Rate[which(eto2_m$Type=="15 MINS")],na.rm = T) #45

t.test(eto2_l$Rate[which(eto2_l$Type=="15 MINS")],eto2_m$Rate[which(eto2_m$Type=="15 MINS")],conf.level = 0.95)
#t = 1.6489, df = 55.814, p-value = 0.1048 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="15 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -1.8264, df = 57.633, p-value = 0.07297 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="15 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -3.8758, df = 57.332, p-value = 0.0002758 significant

#==========================  30 MINS  ======================================#

mean(eto2_l$Rate[which(eto2_l$Type=="30 MINS")],na.rm = T) #44.6
mean(eto2_m$Rate[which(eto2_m$Type=="30 MINS")],na.rm = T) #44

sd(eto2_l$Rate[which(eto2_l$Type=="30 MINS")],na.rm = T) #3.12
sd(eto2_m$Rate[which(eto2_m$Type=="30 MINS")],na.rm = T) #2.13

median(eto2_l$Rate[which(eto2_l$Type=="30 MINS")],na.rm = T) #43
median(eto2_m$Rate[which(eto2_m$Type=="30 MINS")],na.rm = T) #43

t.test(eto2_l$Rate[which(eto2_l$Type=="30 MINS")],eto2_m$Rate[which(eto2_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0.86852, df = 51.209, p-value = 0.3892 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="30 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -4.9228, df = 57.507, p-value = 7.536e-06 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="30 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -7.3928, df = 57.793, p-value = 6.535e-10 significant

#=============================  45 MINS  ===================================#

mean(eto2_l$Rate[which(eto2_l$Type=="45 MINS")],na.rm = T) #43
mean(eto2_m$Rate[which(eto2_m$Type=="45 MINS")],na.rm = T) #42.4

sd(eto2_l$Rate[which(eto2_l$Type=="45 MINS")],na.rm = T) #2.31
sd(eto2_m$Rate[which(eto2_m$Type=="45 MINS")],na.rm = T) #1.88

median(eto2_l$Rate[which(eto2_l$Type=="45 MINS")],na.rm = T) #42
median(eto2_m$Rate[which(eto2_m$Type=="45 MINS")],na.rm = T) #42

t.test(eto2_l$Rate[which(eto2_l$Type=="45 MINS")],eto2_m$Rate[which(eto2_m$Type=="45 MINS")],conf.level = 0.95)
#t = 1.0992, df = 55.689, p-value = 0.2764  significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="45 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -8.0529, df = 55.716, p-value = 6.549e-11 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="45 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -10.777, df = 56.161, p-value = 2.732e-15 significant

#===============================  60 MINS  ================================#

mean(eto2_l$Rate[which(eto2_l$Type=="60 MINS")],na.rm = T) #41.2
mean(eto2_m$Rate[which(eto2_m$Type=="60 MINS")],na.rm = T) #41.2

sd(eto2_l$Rate[which(eto2_l$Type=="60 MINS")],na.rm = T) #2.07
sd(eto2_m$Rate[which(eto2_m$Type=="60 MINS")],na.rm = T) #1.97

median(eto2_l$Rate[which(eto2_l$Type=="60 MINS")],na.rm = T) #41
median(eto2_m$Rate[which(eto2_m$Type=="60 MINS")],na.rm = T) #40

t.test(eto2_l$Rate[which(eto2_l$Type=="60 MINS")],eto2_m$Rate[which(eto2_m$Type=="60 MINS")],conf.level = 0.95)
#t = 0, df = 57.835, p-value = 1 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="60 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -11.193, df = 53.014, p-value = 1.408e-15 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="60 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -12.766, df = 56.922, p-value < 2.2e-16 significant

#=============================  75 MINS ===================================#

mean(eto2_l$Rate[which(eto2_l$Type=="75 MINS")],na.rm = T) #40.2
mean(eto2_m$Rate[which(eto2_m$Type=="75 MINS")],na.rm = T) #40.2

sd(eto2_l$Rate[which(eto2_l$Type=="75 MINS")],na.rm = T) #1.62
sd(eto2_m$Rate[which(eto2_m$Type=="75 MINS")],na.rm = T) #1.49

median(eto2_l$Rate[which(eto2_l$Type=="75 MINS")],na.rm = T) #40
median(eto2_m$Rate[which(eto2_m$Type=="75 MINS")],na.rm = T) #40

t.test(eto2_l$Rate[which(eto2_l$Type=="75 MINS")],eto2_m$Rate[which(eto2_m$Type=="75 MINS")],conf.level = 0.95)
#t = 0, df = 57.835, p-value = 1 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="75 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -13.693, df = 46.114, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="75 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -16.145, df = 50.232, p-value < 2.2e-16 significant

#============================== 90 MINS  ==================================#

mean(eto2_l$Rate[which(eto2_l$Type=="90 MINS")],na.rm = T) #39.4
mean(eto2_m$Rate[which(eto2_m$Type=="90 MINS")],na.rm = T) #40

sd(eto2_l$Rate[which(eto2_l$Type=="90 MINS")],na.rm = T) #2.19
sd(eto2_m$Rate[which(eto2_m$Type=="90 MINS")],na.rm = T) #1.43

median(eto2_l$Rate[which(eto2_l$Type=="90 MINS")],na.rm = T) #40
median(eto2_m$Rate[which(eto2_m$Type=="90 MINS")],na.rm = T) #40

t.test(eto2_l$Rate[which(eto2_l$Type=="90 MINS")],eto2_m$Rate[which(eto2_m$Type=="90 MINS")],conf.level = 0.95)
#t = -1.2539, df = 50.083, p-value = 0.2157 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="90 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -13.719, df = 54.422, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="90 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -16.738, df = 49.116, p-value < 2.2e-16 significant

#===================================  105 MINS  ==================================#

mean(eto2_l$Rate[which(eto2_l$Type=="105 MINS")],na.rm = T) #39
mean(eto2_m$Rate[which(eto2_m$Type=="105 MINS")],na.rm = T) #39.4

sd(eto2_l$Rate[which(eto2_l$Type=="105 MINS")],na.rm = T) #1.92
sd(eto2_m$Rate[which(eto2_m$Type=="105 MINS")],na.rm = T) #1.65

median(eto2_l$Rate[which(eto2_l$Type=="105 MINS")],na.rm = T) #39
median(eto2_m$Rate[which(eto2_m$Type=="105 MINS")],na.rm = T) #40

t.test(eto2_l$Rate[which(eto2_l$Type=="105 MINS")],eto2_m$Rate[which(eto2_m$Type=="105 MINS")],conf.level = 0.95)
#t = -0.86232, df = 56.659, p-value = 0.3921 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="105 MINS")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -14.966, df = 50.995, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="105 MINS")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -17.19, df = 53.056, p-value < 2.2e-16 significant

#=====================================  120 MINS  ===============================#

mean(eto2_l$Rate[which(eto2_l$Type=="120 MIN")],na.rm = T) #38.4
mean(eto2_m$Rate[which(eto2_m$Type=="120 MIN")],na.rm = T) #39

sd(eto2_l$Rate[which(eto2_l$Type=="120 MIN")],na.rm = T) #1.88
sd(eto2_m$Rate[which(eto2_m$Type=="120 MIN")],na.rm = T) #1.28

median(eto2_l$Rate[which(eto2_l$Type=="120 MIN")],na.rm = T) #38
median(eto2_m$Rate[which(eto2_m$Type=="120 MIN")],na.rm = T) #40

t.test(eto2_l$Rate[which(eto2_l$Type=="120 MIN")],eto2_m$Rate[which(eto2_m$Type=="120 MIN")],conf.level = 0.95)
#t = -1.4392, df = 51.179, p-value = 0.1562 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="120 MIN")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -16.034, df = 50.34, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="120 MIN")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -19.343, df = 45.946, p-value < 2.2e-16 significant

#====================================  135 MIN  =======================================#

mean(eto2_l$Rate[which(eto2_l$Type=="135 MIN")],na.rm = T) #38
mean(eto2_m$Rate[which(eto2_m$Type=="135 MIN")],na.rm = T) #39

sd(eto2_l$Rate[which(eto2_l$Type=="135 MIN")],na.rm = T) #1.92
sd(eto2_m$Rate[which(eto2_m$Type=="135 MIN")],na.rm = T) #1.28

median(eto2_l$Rate[which(eto2_l$Type=="135 MIN")],na.rm = T) #38
median(eto2_m$Rate[which(eto2_m$Type=="135 MIN")],na.rm = T) #40

t.test(eto2_l$Rate[which(eto2_l$Type=="135 MIN")],eto2_m$Rate[which(eto2_m$Type=="135 MIN")],conf.level = 0.95)
#t = -2.3616, df = 50.526, p-value = 0.02209 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="135 MIN")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -16.558, df = 50.995, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="135 MIN")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -19.343, df = 45.946, p-value < 2.2e-16 significant

#======================================  150 MIN  ====================================#

mean(eto2_l$Rate[which(eto2_l$Type=="150 MIN")],na.rm = T) #38.2
mean(eto2_m$Rate[which(eto2_m$Type=="150 MIN")],na.rm = T) #38.4

sd(eto2_l$Rate[which(eto2_l$Type=="150 MIN")],na.rm = T) #2.17
sd(eto2_m$Rate[which(eto2_m$Type=="150 MIN")],na.rm = T) #1.22

median(eto2_l$Rate[which(eto2_l$Type=="150 MIN")],na.rm = T) #38
median(eto2_m$Rate[which(eto2_m$Type=="150 MIN")],na.rm = T) #39

t.test(eto2_l$Rate[which(eto2_l$Type=="150 MIN")],eto2_m$Rate[which(eto2_m$Type=="150 MIN")],conf.level = 0.95)
#t = -0.4397, df = 45.655, p-value = 0.6622 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="150 MIN")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -15.599, df = 54.207, p-value < 2.2e-16  significant
t.test(eto2_m$Rate[which(eto2_m$Type=="150 MIN")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -20.861, df = 44.53, p-value < 2.2e-16 significant

#=======================================  165 MIN  ==================================#

mean(eto2_l$Rate[which(eto2_l$Type=="165 MIN")],na.rm = T) #38
mean(eto2_m$Rate[which(eto2_m$Type=="165 MIN")],na.rm = T) #38.5

sd(eto2_l$Rate[which(eto2_l$Type=="165 MIN")],na.rm = T) #1.43
sd(eto2_m$Rate[which(eto2_m$Type=="165 MIN")],na.rm = T) #0.88

median(eto2_l$Rate[which(eto2_l$Type=="165 MIN")],na.rm = T) #38
median(eto2_m$Rate[which(eto2_m$Type=="165 MIN")],na.rm = T) #39

t.test(eto2_l$Rate[which(eto2_l$Type=="165 MIN")],eto2_m$Rate[which(eto2_m$Type=="165 MIN")],conf.level = 0.95)
#t = -1.5688, df = 49.074, p-value = 0.1231 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="165 MIN")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -17.854, df = 42.892, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="165 MIN")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -21.495, df = 39.31, p-value < 2.2e-16 significant

#=========================================  180 MIN  ================================#

mean(eto2_l$Rate[which(eto2_l$Type=="180 MIN")],na.rm = T) #37.8
mean(eto2_m$Rate[which(eto2_m$Type=="180 MIN")],na.rm = T) #38

sd(eto2_l$Rate[which(eto2_l$Type=="180 MIN")],na.rm = T) #1.74
sd(eto2_m$Rate[which(eto2_m$Type=="180 MIN")],na.rm = T) #1.25

median(eto2_l$Rate[which(eto2_l$Type=="180 MIN")],na.rm = T) #38
median(eto2_m$Rate[which(eto2_m$Type=="180 MIN")],na.rm = T) #38.5

t.test(eto2_l$Rate[which(eto2_l$Type=="180 MIN")],eto2_m$Rate[which(eto2_m$Type=="180 MIN")],conf.level = 0.95)
#t = -0.48899, df = 51.426, p-value = 0.6269 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="180 MIN")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -17.37, df = 48.166, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="180 MIN")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -20.985, df = 46.76, p-value < 2.2e-16 significant

#========================================  0 MIN_Emergence  ==========================#

mean(eto2_l$Rate[which(eto2_l$Type=="0 MIN_Emergence")],na.rm = T) #36.5
mean(eto2_m$Rate[which(eto2_m$Type=="0 MIN_Emergence")],na.rm = T) #37.25

sd(eto2_l$Rate[which(eto2_l$Type=="0 MIN_Emergence")],na.rm = T) #0.52
sd(eto2_m$Rate[which(eto2_m$Type=="0 MIN_Emergence")],na.rm = T) #0.84

median(eto2_l$Rate[which(eto2_l$Type=="0 MIN_Emergence")],na.rm = T) #36.5
median(eto2_m$Rate[which(eto2_m$Type=="0 MIN_Emergence")],na.rm = T) #37.5

t.test(eto2_l$Rate[which(eto2_l$Type=="0 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = -3.2696, df = 32.268, p-value = 0.002563 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="0 MIN_Emergence")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -21.982, df = 33.457, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="0 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -24.428, df = 38.538, p-value < 2.2e-16 significant

#========================================  1 MIN_Emergence  ============================#

mean(eto2_l$Rate[which(eto2_l$Type=="1 MIN_Emergence")],na.rm = T) #75
mean(eto2_m$Rate[which(eto2_m$Type=="1 MIN_Emergence")],na.rm = T) #57.75

sd(eto2_l$Rate[which(eto2_l$Type=="1 MIN_Emergence")],na.rm = T) #1.04
sd(eto2_m$Rate[which(eto2_m$Type=="1 MIN_Emergence")],na.rm = T) #8.82

median(eto2_l$Rate[which(eto2_l$Type=="1 MIN_Emergence")],na.rm = T) #75
median(eto2_m$Rate[which(eto2_m$Type=="1 MIN_Emergence")],na.rm = T) #59

t.test(eto2_l$Rate[which(eto2_l$Type=="1 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 9.4413, df = 24.266, p-value = 1.343e-09 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="1 MIN_Emergence")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 44.256, df = 39.891, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="1 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.1653, df = 25.431, p-value = 2.315e-05 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(eto2_l$Rate[which(eto2_l$Type=="2 MIN_Emergence")],na.rm = T) #88.5
mean(eto2_m$Rate[which(eto2_m$Type=="2 MIN_Emergence")],na.rm = T) #80.25

sd(eto2_l$Rate[which(eto2_l$Type=="2 MIN_Emergence")],na.rm = T) #1.55
sd(eto2_m$Rate[which(eto2_m$Type=="2 MIN_Emergence")],na.rm = T) #9.64

median(eto2_l$Rate[which(eto2_l$Type=="2 MIN_Emergence")],na.rm = T) #88.5
median(eto2_m$Rate[which(eto2_m$Type=="2 MIN_Emergence")],na.rm = T) #77

t.test(eto2_l$Rate[which(eto2_l$Type=="2 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = 4.0832, df = 25.343, p-value = 0.0003915 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="2 MIN_Emergence")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 58.19, df = 35.663, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="2 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 15.928, df = 25.035, p-value = 1.305e-14 significant

#==========================================  3 MIN_Emergence  ================================#

mean(eto2_l$Rate[which(eto2_l$Type=="3 MIN_Emergence")],na.rm = T) #93
mean(eto2_m$Rate[which(eto2_m$Type=="3 MIN_Emergence")],na.rm = T) #85

sd(eto2_l$Rate[which(eto2_l$Type=="3 MIN_Emergence")],na.rm = T) #1.04
sd(eto2_m$Rate[which(eto2_m$Type=="3 MIN_Emergence")],na.rm = T) #7.33

median(eto2_l$Rate[which(eto2_l$Type=="3 MIN_Emergence")],na.rm = T) #93
median(eto2_m$Rate[which(eto2_m$Type=="3 MIN_Emergence")],na.rm = T) #82.5

t.test(eto2_l$Rate[which(eto2_l$Type=="3 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = 5.2409, df = 24.82, p-value = 2.044e-05 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="3 MIN_Emergence")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 74.204, df = 39.891, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="3 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 23.704, df = 26.525, p-value < 2.2e-16 significant

#===========================================  4 MIN_Emergence  =================================#

mean(eto2_l$Rate[which(eto2_l$Type=="4 MIN_Emergence")],na.rm = T) #93.5
mean(eto2_m$Rate[which(eto2_m$Type=="4 MIN_Emergence")],na.rm = T) #87.75

sd(eto2_l$Rate[which(eto2_l$Type=="4 MIN_Emergence")],na.rm = T) #1.56
sd(eto2_m$Rate[which(eto2_m$Type=="4 MIN_Emergence")],na.rm = T) #5.56

median(eto2_l$Rate[which(eto2_l$Type=="4 MIN_Emergence")],na.rm = T) #93.5
median(eto2_m$Rate[which(eto2_m$Type=="4 MIN_Emergence")],na.rm = T) #85.5

t.test(eto2_l$Rate[which(eto2_l$Type=="4 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 4.7022, df = 29.327, p-value = 5.662e-05 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="4 MIN_Emergence")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 65.446, df = 35.663, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="4 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 32.712, df = 29.094, p-value < 2.2e-16 significant

#============================================  5 MIN_Emergence  ===================================#

mean(eto2_l$Rate[which(eto2_l$Type=="5 MIN_Emergence")],na.rm = T) #95.5
mean(eto2_m$Rate[which(eto2_m$Type=="5 MIN_Emergence")],na.rm = T) #88.75

sd(eto2_l$Rate[which(eto2_l$Type=="5 MIN_Emergence")],na.rm = T) #0.52
sd(eto2_m$Rate[which(eto2_m$Type=="5 MIN_Emergence")],na.rm = T) #6.51

median(eto2_l$Rate[which(eto2_l$Type=="5 MIN_Emergence")],na.rm = T) #95.5
median(eto2_m$Rate[which(eto2_m$Type=="5 MIN_Emergence")],na.rm = T) #89

t.test(eto2_l$Rate[which(eto2_l$Type=="5 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = 5.0427, df = 23.587, p-value = 3.913e-05 not significant

#Intragroup
t.test(eto2_l$Rate[which(eto2_l$Type=="5 MIN_Emergence")],eto2_l$Rate[which(eto2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 87.003, df = 33.457, p-value < 2.2e-16 significant
t.test(eto2_m$Rate[which(eto2_m$Type=="5 MIN_Emergence")],eto2_m$Rate[which(eto2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 29.113, df = 27.459, p-value < 2.2e-16 significant


########################################################  FIO2  ##############################################################


fio2_rate<-read_excel("combined.xlsx",sheet = 14)
sum(is.na(fio2_rate))
str(fio2_rate)

#Subsetting the two study groups
fio2_l<-subset(fio2_rate,fio2_rate$`STUDY GROUP`=="L")
fio2_m<-subset(fio2_rate,fio2_rate$`STUDY GROUP`=="M")

#===============================  INDUCTION  ===========================#

mean(fio2_l$Rate[which(fio2_l$Type=="INDUCTION")],na.rm = T) #99.2
mean(fio2_m$Rate[which(fio2_m$Type=="INDUCTION")],na.rm = T) #99

sd(fio2_l$Rate[which(fio2_l$Type=="INDUCTION")],na.rm = T) #0.76
sd(fio2_m$Rate[which(fio2_m$Type=="INDUCTION")],na.rm = T) #0.72

median(fio2_l$Rate[which(fio2_l$Type=="INDUCTION")],na.rm = T) #99
median(fio2_m$Rate[which(fio2_m$Type=="INDUCTION")],na.rm = T) #99

t.test(fio2_l$Rate[which(fio2_l$Type=="INDUCTION")],fio2_m$Rate[which(fio2_m$Type=="INDUCTION")],conf.level = 0.95)
#t = 0.98713, df = 50.442, p-value = 0.3283 , hence insignificant difference


#=========================  INTUBATION  ==============================#

mean(fio2_l$Rate[which(fio2_l$Type=="INTUBATION")],na.rm = T) #99
mean(fio2_m$Rate[which(fio2_m$Type=="INTUBATION")],na.rm = T) #99.25

sd(fio2_l$Rate[which(fio2_l$Type=="INTUBATION")],na.rm = T) #0.9
sd(fio2_m$Rate[which(fio2_m$Type=="INTUBATION")],na.rm = T) #1.32

median(fio2_l$Rate[which(fio2_l$Type=="INTUBATION")],na.rm = T) #99
median(fio2_m$Rate[which(fio2_m$Type=="INTUBATION")],na.rm = T) #99

t.test(fio2_l$Rate[which(fio2_l$Type=="INTUBATION")],fio2_m$Rate[which(fio2_m$Type=="INTUBATION")],conf.level = 0.95)
#t = -0.78682, df = 39.157, p-value = 0.4361 , hence insignificant difference



#=================    5 mins ==============================#

mean(fio2_l$Rate[which(fio2_l$Type=="5 MINS")],na.rm = T) #54.2
mean(fio2_m$Rate[which(fio2_m$Type=="5 MINS")],na.rm = T) #54.8

sd(fio2_l$Rate[which(fio2_l$Type=="5 MINS")],na.rm = T) #7.78
sd(fio2_m$Rate[which(fio2_m$Type=="5 MINS")],na.rm = T) #6.14

median(fio2_l$Rate[which(fio2_l$Type=="5 MINS")],na.rm = T) #50
median(fio2_m$Rate[which(fio2_m$Type=="5 MINS")],na.rm = T) #55

t.test(fio2_l$Rate[which(fio2_l$Type=="5 MINS")],fio2_m$Rate[which(fio2_m$Type=="5 MINS")],conf.level = 0.95)
#t = -0.33129, df = 55.055, p-value = 0.7417 , Insignificant

#============================  10 MINS  ==================================#

mean(fio2_l$Rate[which(fio2_l$Type=="10 MINS")],na.rm = T) #50.98
mean(fio2_m$Rate[which(fio2_m$Type=="10 MINS")],na.rm = T) #49.8

sd(fio2_l$Rate[which(fio2_l$Type=="10 MINS")],na.rm = T) #3.19
sd(fio2_m$Rate[which(fio2_m$Type=="10 MINS")],na.rm = T) #1.97

median(fio2_l$Rate[which(fio2_l$Type=="10 MINS")],na.rm = T) #51
median(fio2_m$Rate[which(fio2_m$Type=="10 MINS")],na.rm = T) #51

t.test(fio2_l$Rate[which(fio2_l$Type=="10 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 1.723, df = 48.337, p-value = 0.09128 , Insignificant



#=============================  15 MINS  ==================================#

mean(fio2_l$Rate[which(fio2_l$Type=="15 MINS")],na.rm = T) #48.6
mean(fio2_m$Rate[which(fio2_m$Type=="15 MINS")],na.rm = T) #47.6

sd(fio2_l$Rate[which(fio2_l$Type=="15 MINS")],na.rm = T) #2.09
sd(fio2_m$Rate[which(fio2_m$Type=="15 MINS")],na.rm = T) #1.65

median(fio2_l$Rate[which(fio2_l$Type=="15 MINS")],na.rm = T) #49
median(fio2_m$Rate[which(fio2_m$Type=="15 MINS")],na.rm = T) #47

t.test(fio2_l$Rate[which(fio2_l$Type=="15 MINS")],fio2_m$Rate[which(fio2_m$Type=="15 MINS")],conf.level = 0.95)
#t = 2.0531, df = 55.024, p-value = 0.04483 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="15 MINS")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -3.4155, df = 50.076, p-value = 0.001271 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="15 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -4.6831, df = 56.277, p-value = 1.828e-05 significant

#==========================  30 MINS  ======================================#

mean(fio2_l$Rate[which(fio2_l$Type=="30 MINS")],na.rm = T) #46.8
mean(fio2_m$Rate[which(fio2_m$Type=="30 MINS")],na.rm = T) #46.6

sd(fio2_l$Rate[which(fio2_l$Type=="30 MINS")],na.rm = T) #2.52
sd(fio2_m$Rate[which(fio2_m$Type=="30 MINS")],na.rm = T) #1.37

median(fio2_l$Rate[which(fio2_l$Type=="30 MINS")],na.rm = T) #46
median(fio2_m$Rate[which(fio2_m$Type=="30 MINS")],na.rm = T) #46

t.test(fio2_l$Rate[which(fio2_l$Type=="30 MINS")],fio2_m$Rate[which(fio2_m$Type=="30 MINS")],conf.level = 0.95)
#t = 0.38079, df = 44.906, p-value = 0.7052 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="30 MINS")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -5.6272, df = 55.084, p-value = 6.36e-07 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="30 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -7.2821, df = 51.899, p-value = 1.776e-09 significant

#=============================  45 MINS  ===================================#

mean(fio2_l$Rate[which(fio2_l$Type=="45 MINS")],na.rm = T) #45.4
mean(fio2_m$Rate[which(fio2_m$Type=="45 MINS")],na.rm = T) #45.6

sd(fio2_l$Rate[which(fio2_l$Type=="45 MINS")],na.rm = T) #1.77
sd(fio2_m$Rate[which(fio2_m$Type=="45 MINS")],na.rm = T) #1.77

median(fio2_l$Rate[which(fio2_l$Type=="45 MINS")],na.rm = T) #44
median(fio2_m$Rate[which(fio2_m$Type=="45 MINS")],na.rm = T) #45

t.test(fio2_l$Rate[which(fio2_l$Type=="45 MINS")],fio2_m$Rate[which(fio2_m$Type=="45 MINS")],conf.level = 0.95)
#t = -0.43679, df = 58, p-value = 0.6639  significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="45 MINS")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -8.3724, df = 45.355, p-value = 9.373e-11 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="45 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -8.6735, df = 57.357, p-value = 5.034e-12 significant

#===============================  60 MINS  ================================#

mean(fio2_l$Rate[which(fio2_l$Type=="60 MINS")],na.rm = T) #44.2
mean(fio2_m$Rate[which(fio2_m$Type=="60 MINS")],na.rm = T) #44.4

sd(fio2_l$Rate[which(fio2_l$Type=="60 MINS")],na.rm = T) #1.62
sd(fio2_m$Rate[which(fio2_m$Type=="60 MINS")],na.rm = T) #1.37

median(fio2_l$Rate[which(fio2_l$Type=="60 MINS")],na.rm = T) #44
median(fio2_m$Rate[which(fio2_m$Type=="60 MINS")],na.rm = T) #44

t.test(fio2_l$Rate[which(fio2_l$Type=="60 MINS")],fio2_m$Rate[which(fio2_m$Type=="60 MINS")],conf.level = 0.95)
#t = -0.51346, df = 56.487, p-value = 0.6096 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="60 MINS")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -10.368, df = 43.131, p-value = 2.749e-13 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="60 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -12.766, df = 56.922, p-value < 2.2e-16 significant

#=============================  75 MINS ===================================#

mean(fio2_l$Rate[which(fio2_l$Type=="75 MINS")],na.rm = T) #43.4
mean(fio2_m$Rate[which(fio2_m$Type=="75 MINS")],na.rm = T) #43.4

sd(fio2_l$Rate[which(fio2_l$Type=="75 MINS")],na.rm = T) #1.88
sd(fio2_m$Rate[which(fio2_m$Type=="75 MINS")],na.rm = T) #0.81

median(fio2_l$Rate[which(fio2_l$Type=="75 MINS")],na.rm = T) #43
median(fio2_m$Rate[which(fio2_m$Type=="75 MINS")],na.rm = T) #44

t.test(fio2_l$Rate[which(fio2_l$Type=="75 MINS")],fio2_m$Rate[which(fio2_m$Type=="75 MINS")],conf.level = 0.95)
 #t = 0, df = 57.835, p-value = 1 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="75 MINS")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -11.201, df = 47.066, p-value = 7.136e-15 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="75 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -16.145, df = 50.232, p-value < 2.2e-16 significant

#============================== 90 MINS  ==================================#

mean(fio2_l$Rate[which(fio2_l$Type=="90 MINS")],na.rm = T) #42.6
mean(fio2_m$Rate[which(fio2_m$Type=="90 MINS")],na.rm = T) #43.4

sd(fio2_l$Rate[which(fio2_l$Type=="90 MINS")],na.rm = T) #1.88
sd(fio2_m$Rate[which(fio2_m$Type=="90 MINS")],na.rm = T) #1.22

median(fio2_l$Rate[which(fio2_l$Type=="90 MINS")],na.rm = T) #43
median(fio2_m$Rate[which(fio2_m$Type=="90 MINS")],na.rm = T) #44

t.test(fio2_l$Rate[which(fio2_l$Type=="90 MINS")],fio2_m$Rate[which(fio2_m$Type=="90 MINS")],conf.level = 0.95)
#t = -1.9502, df = 49.659, p-value = 0.05681 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="90 MINS")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -13.719, df = 54.422, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="90 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -16.738, df = 49.116, p-value < 2.2e-16 significant

#===================================  105 MINS  ==================================#

mean(fio2_l$Rate[which(fio2_l$Type=="105 MINS")],na.rm = T) #42.2
mean(fio2_m$Rate[which(fio2_m$Type=="105 MINS")],na.rm = T) #42.8

sd(fio2_l$Rate[which(fio2_l$Type=="105 MINS")],na.rm = T) #2.17
sd(fio2_m$Rate[which(fio2_m$Type=="105 MINS")],na.rm = T) #1.18

median(fio2_l$Rate[which(fio2_l$Type=="105 MINS")],na.rm = T) #42
median(fio2_m$Rate[which(fio2_m$Type=="105 MINS")],na.rm = T) #43

t.test(fio2_l$Rate[which(fio2_l$Type=="105 MINS")],fio2_m$Rate[which(fio2_m$Type=="105 MINS")],conf.level = 0.95)
#t = -1.328, df = 44.885, p-value = 0.1909 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="105 MINS")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -14.966, df = 50.995, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="105 MINS")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -17.19, df = 53.056, p-value < 2.2e-16 significant

#=====================================  120 MINS  ===============================#

mean(fio2_l$Rate[which(fio2_l$Type=="120 MIN")],na.rm = T) #42
mean(fio2_m$Rate[which(fio2_m$Type=="120 MIN")],na.rm = T) #42.6

sd(fio2_l$Rate[which(fio2_l$Type=="120 MIN")],na.rm = T) #1.92
sd(fio2_m$Rate[which(fio2_m$Type=="120 MIN")],na.rm = T) #1.22

median(fio2_l$Rate[which(fio2_l$Type=="120 MIN")],na.rm = T) #42
median(fio2_m$Rate[which(fio2_m$Type=="120 MIN")],na.rm = T) #42

t.test(fio2_l$Rate[which(fio2_l$Type=="120 MIN")],fio2_m$Rate[which(fio2_m$Type=="120 MIN")],conf.level = 0.95)
#t = -1.4392, df = 49, p-value = 0.1564 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="120 MIN")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -16.034, df = 50.34, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="120 MIN")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -19.343, df = 45.946, p-value < 2.2e-16 significant

#====================================  135 MIN  =======================================#

mean(fio2_l$Rate[which(fio2_l$Type=="135 MIN")],na.rm = T) #42
mean(fio2_m$Rate[which(fio2_m$Type=="135 MIN")],na.rm = T) #42.2

sd(fio2_l$Rate[which(fio2_l$Type=="135 MIN")],na.rm = T) #1.92
sd(fio2_m$Rate[which(fio2_m$Type=="135 MIN")],na.rm = T) #0.76

median(fio2_l$Rate[which(fio2_l$Type=="135 MIN")],na.rm = T) #42
median(fio2_m$Rate[which(fio2_m$Type=="135 MIN")],na.rm = T) #42

t.test(fio2_l$Rate[which(fio2_l$Type=="135 MIN")],fio2_m$Rate[which(fio2_m$Type=="135 MIN")],conf.level = 0.95)
#t = -0.52806, df = 37.809, p-value = 0.6005 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="135 MIN")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -16.558, df = 50.995, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="135 MIN")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -19.343, df = 45.946, p-value < 2.2e-16 significant

#======================================  150 MIN  ====================================#

mean(fio2_l$Rate[which(fio2_l$Type=="150 MIN")],na.rm = T) #41.8
mean(fio2_m$Rate[which(fio2_m$Type=="150 MIN")],na.rm = T) #42.2

sd(fio2_l$Rate[which(fio2_l$Type=="150 MIN")],na.rm = T) #2.26
sd(fio2_m$Rate[which(fio2_m$Type=="150 MIN")],na.rm = T) #0.76

median(fio2_l$Rate[which(fio2_l$Type=="150 MIN")],na.rm = T) #42
median(fio2_m$Rate[which(fio2_m$Type=="150 MIN")],na.rm = T) #42

t.test(fio2_l$Rate[which(fio2_l$Type=="150 MIN")],fio2_m$Rate[which(fio2_m$Type=="150 MIN")],conf.level = 0.95)
#t = -0.91683, df = 35.466, p-value = 0.3654 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="150 MIN")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -15.599, df = 54.207, p-value < 2.2e-16  significant
t.test(fio2_m$Rate[which(fio2_m$Type=="150 MIN")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -20.861, df = 44.53, p-value < 2.2e-16 significant

#=======================================  165 MIN  ==================================#

mean(fio2_l$Rate[which(fio2_l$Type=="165 MIN")],na.rm = T) #41.8
mean(fio2_m$Rate[which(fio2_m$Type=="165 MIN")],na.rm = T) #42

sd(fio2_l$Rate[which(fio2_l$Type=="165 MIN")],na.rm = T) #2.07
sd(fio2_m$Rate[which(fio2_m$Type=="165 MIN")],na.rm = T) #1.02

median(fio2_l$Rate[which(fio2_l$Type=="165 MIN")],na.rm = T) #42
median(fio2_m$Rate[which(fio2_m$Type=="165 MIN")],na.rm = T) #42

t.test(fio2_l$Rate[which(fio2_l$Type=="165 MIN")],fio2_m$Rate[which(fio2_m$Type=="165 MIN")],conf.level = 0.95)
#t = -0.46259, df = 44.132, p-value = 0.6459 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="165 MIN")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -17.854, df = 42.892, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="165 MIN")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -21.495, df = 39.31, p-value < 2.2e-16 significant

#=========================================  180 MIN  ================================#

mean(fio2_l$Rate[which(fio2_l$Type=="180 MIN")],na.rm = T) #41.4
mean(fio2_m$Rate[which(fio2_m$Type=="180 MIN")],na.rm = T) #41.75

sd(fio2_l$Rate[which(fio2_l$Type=="180 MIN")],na.rm = T) #2.19
sd(fio2_m$Rate[which(fio2_m$Type=="180 MIN")],na.rm = T) #0.84

median(fio2_l$Rate[which(fio2_l$Type=="180 MIN")],na.rm = T) #42
median(fio2_m$Rate[which(fio2_m$Type=="180 MIN")],na.rm = T) #41.5

t.test(fio2_l$Rate[which(fio2_l$Type=="180 MIN")],fio2_m$Rate[which(fio2_m$Type=="180 MIN")],conf.level = 0.95)
#t = -0.80318, df = 39.126, p-value = 0.4267 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="180 MIN")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -17.37, df = 48.166, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="180 MIN")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -20.985, df = 46.76, p-value < 2.2e-16 significant

#========================================  0 MIN_Emergence  ==========================#

mean(fio2_l$Rate[which(fio2_l$Type=="0 MIN_Emergence")],na.rm = T) #40
mean(fio2_m$Rate[which(fio2_m$Type=="0 MIN_Emergence")],na.rm = T) #41.75

sd(fio2_l$Rate[which(fio2_l$Type=="0 MIN_Emergence")],na.rm = T) #1.04
sd(fio2_m$Rate[which(fio2_m$Type=="0 MIN_Emergence")],na.rm = T) #1.11

median(fio2_l$Rate[which(fio2_l$Type=="0 MIN_Emergence")],na.rm = T) #40
median(fio2_m$Rate[which(fio2_m$Type=="0 MIN_Emergence")],na.rm = T) #42

t.test(fio2_l$Rate[which(fio2_l$Type=="0 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="0 MIN_Emergence")],conf.level = 0.95)
#t = -4.6352, df = 23.428, p-value = 0.0001108 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="0 MIN_Emergence")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = -21.982, df = 33.457, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="0 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = -24.428, df = 38.538, p-value < 2.2e-16 significant

#========================================  1 MIN_Emergence  ============================#

mean(fio2_l$Rate[which(fio2_l$Type=="1 MIN_Emergence")],na.rm = T) #73.5
mean(fio2_m$Rate[which(fio2_m$Type=="1 MIN_Emergence")],na.rm = T) #71

sd(fio2_l$Rate[which(fio2_l$Type=="1 MIN_Emergence")],na.rm = T) #4.7
sd(fio2_m$Rate[which(fio2_m$Type=="1 MIN_Emergence")],na.rm = T) #7.81

median(fio2_l$Rate[which(fio2_l$Type=="1 MIN_Emergence")],na.rm = T) #73.5
median(fio2_m$Rate[which(fio2_m$Type=="1 MIN_Emergence")],na.rm = T) #69.5

t.test(fio2_l$Rate[which(fio2_l$Type=="1 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="1 MIN_Emergence")],conf.level = 0.95)
#t = 1.1939, df = 32.617, p-value = 0.2411 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="1 MIN_Emergence")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 44.256, df = 39.891, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="1 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 5.1653, df = 25.431, p-value = 2.315e-05 significant

#=========================================  2 MIN_Emergence  ===============================#

mean(fio2_l$Rate[which(fio2_l$Type=="2 MIN_Emergence")],na.rm = T) #92
mean(fio2_m$Rate[which(fio2_m$Type=="2 MIN_Emergence")],na.rm = T) #95.5

sd(fio2_l$Rate[which(fio2_l$Type=="2 MIN_Emergence")],na.rm = T) #0
sd(fio2_m$Rate[which(fio2_m$Type=="2 MIN_Emergence")],na.rm = T) #3.02

median(fio2_l$Rate[which(fio2_l$Type=="2 MIN_Emergence")],na.rm = T) #92
median(fio2_m$Rate[which(fio2_m$Type=="2 MIN_Emergence")],na.rm = T) #96

t.test(fio2_l$Rate[which(fio2_l$Type=="2 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="2 MIN_Emergence")],conf.level = 0.95)
#t = -5.6745, df = 23, p-value = 8.898e-06 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="2 MIN_Emergence")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 58.19, df = 35.663, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="2 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 15.928, df = 25.035, p-value = 1.305e-14 significant

#==========================================  3 MIN_Emergence  ================================#

mean(fio2_l$Rate[which(fio2_l$Type=="3 MIN_Emergence")],na.rm = T) #96.5
mean(fio2_m$Rate[which(fio2_m$Type=="3 MIN_Emergence")],na.rm = T) #97

sd(fio2_l$Rate[which(fio2_l$Type=="3 MIN_Emergence")],na.rm = T) #0.52
sd(fio2_m$Rate[which(fio2_m$Type=="3 MIN_Emergence")],na.rm = T) #1.44

median(fio2_l$Rate[which(fio2_l$Type=="3 MIN_Emergence")],na.rm = T) #96.5
median(fio2_m$Rate[which(fio2_m$Type=="3 MIN_Emergence")],na.rm = T) #97

t.test(fio2_l$Rate[which(fio2_l$Type=="3 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="3 MIN_Emergence")],conf.level = 0.95)
#t = -1.5097, df = 32.02, p-value = 0.1409 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="3 MIN_Emergence")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 74.204, df = 39.891, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="3 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 23.704, df = 26.525, p-value < 2.2e-16 significant

#===========================================  4 MIN_Emergence  =================================#

mean(fio2_l$Rate[which(fio2_l$Type=="4 MIN_Emergence")],na.rm = T) #98
mean(fio2_m$Rate[which(fio2_m$Type=="4 MIN_Emergence")],na.rm = T) #97.5

sd(fio2_l$Rate[which(fio2_l$Type=="4 MIN_Emergence")],na.rm = T) #1.04
sd(fio2_m$Rate[which(fio2_m$Type=="4 MIN_Emergence")],na.rm = T) #2.22

median(fio2_l$Rate[which(fio2_l$Type=="4 MIN_Emergence")],na.rm = T) #98
median(fio2_m$Rate[which(fio2_m$Type=="4 MIN_Emergence")],na.rm = T) #98

t.test(fio2_l$Rate[which(fio2_l$Type=="4 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="4 MIN_Emergence")],conf.level = 0.95)
#t = 0.91681, df = 33.95, p-value = 0.3657 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="4 MIN_Emergence")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 65.446, df = 35.663, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="4 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 32.712, df = 29.094, p-value < 2.2e-16 significant

#============================================  5 MIN_Emergence  ===================================#

mean(fio2_l$Rate[which(fio2_l$Type=="5 MIN_Emergence")],na.rm = T) #98
mean(fio2_m$Rate[which(fio2_m$Type=="5 MIN_Emergence")],na.rm = T) #98.25

sd(fio2_l$Rate[which(fio2_l$Type=="5 MIN_Emergence")],na.rm = T) #1.04
sd(fio2_m$Rate[which(fio2_m$Type=="5 MIN_Emergence")],na.rm = T) #1.11

median(fio2_l$Rate[which(fio2_l$Type=="5 MIN_Emergence")],na.rm = T) #98
median(fio2_m$Rate[which(fio2_m$Type=="5 MIN_Emergence")],na.rm = T) #98

t.test(fio2_l$Rate[which(fio2_l$Type=="5 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="5 MIN_Emergence")],conf.level = 0.95)
#t = -0.66217, df = 23.428, p-value = 0.5143 not significant

#Intragroup
t.test(fio2_l$Rate[which(fio2_l$Type=="5 MIN_Emergence")],fio2_l$Rate[which(fio2_l$Type=="10 MINS")],conf.level = 0.95)
#t = 87.003, df = 33.457, p-value < 2.2e-16 significant
t.test(fio2_m$Rate[which(fio2_m$Type=="5 MIN_Emergence")],fio2_m$Rate[which(fio2_m$Type=="10 MINS")],conf.level = 0.95)
#t = 29.113, df = 27.459, p-value < 2.2e-16 significant

