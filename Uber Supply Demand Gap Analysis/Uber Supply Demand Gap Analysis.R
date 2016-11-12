library(ggplot2)

#importing the dataset
dataset<- Uber.request.data <- read.csv("D:/R folder/Uber request data.csv")

#aggregating the data by hour
dataset$Hour<- as.numeric(substr(dataset$Request.time, 1,2))

#plotting the number of trips by hour
ggplot(dataset, aes(x = as.factor(Hour),fill = Pickup.point))+geom_bar(position = "dodge")

#making slots by number of trips
dataset$time_slot = ifelse(dataset$Hour < 4, "Pre_Morning", ifelse(dataset$Hour < 10,"Morning_rush",ifelse(dataset$Hour < 15,"day_time",ifelse(dataset$Hour < 22,"Evening_rush","late_night"))))

#finding the number of trips made in each slot
nrow(subset(dataset, dataset$time_slot == "Pre_Morning"))
nrow(subset(dataset, dataset$time_slot == "Morning_rush"))
nrow(subset(dataset, dataset$time_slot == "day_time"))
nrow(subset(dataset, dataset$time_slot == "Evening_rush"))
nrow(subset(dataset, dataset$time_slot == "late_night"))

#plotting and identifying the most critical problems before Uber
ggplot(dataset, aes(x = as.factor(time_slot), fill= as.factor(dataset$Status))) + geom_bar()

#problem 1
dataset1 <- subset(dataset,time_slot=="Morning_rush")
ggplot(dataset1, aes(x = as.factor(Pickup.point), fill= as.factor(dataset1$Status))) + geom_bar()

#severity of problem by location
nrow(subset(dataset1, dataset1$Pickup.point == "Airport" & dataset1$Status == "Cancelled"))
nrow(subset(dataset1, dataset1$Pickup.point == "City" & dataset1$Status == "Cancelled"))

#problem 2
dataset2 <- subset(dataset,time_slot=="Evening_rush")
ggplot(dataset2, aes(x = as.factor(Pickup.point), fill= as.factor(dataset2$Status))) + geom_bar()

#severity of problem by location
nrow(subset(dataset2, dataset2$Pickup.point == "Airport" & dataset2$Status == "No Cars Available"))
nrow(subset(dataset2, dataset2$Pickup.point == "City" & dataset2$Status == "No Cars Available"))