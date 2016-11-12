As an analyst, you have come to know that the Uber customers are facing the problem of driver cancellation and non-availability of the cars. Thus, you decide to dig further into the problem, and hence, pull out the request level data from the warehouse for past 5 days.
 

In the data, as you can see, there are 6 attributes associated with each request id. These are:-
Date of request: The date on which the customer made the trip request
Time of request: The time at which the customer made the trip request
Drop-off time: The drop-off time, in case the trip was completed 
Pick-up point: The point from which the request was made
Driver id: The unique identification number of the driver
Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available
 
Note that for this case study, only the trips to and from the airport are being considered.

Problem Statement

Uber has identified that there is an issue with regard to availability of cars especially at the airport in one specific city where the airport is far away from the city. They want to identify why this issue exists. You are the analyst at Uber and have been given the task to identify the cause behind this issue. Following are the stages of analysis that you will be performing to get to the bottom of this problem. 
 
All components of this case study have to be executed in R. Also note that this is NOT a group case study but an individual assignment.
 
DATA PREPARATION:
Make a grouped bar chart depicting the hour-wise trip request made at city and airport respectively. You can aggregate the data for all 5 days on the same axis of 24 hours. Each bar should correspond to an hour and pick-up point (city / airport) should be displayed in two colours?(Hint: you can either use the as.Date & as.Time format to convert the Request_time to the required format or can extract the first 2 digits of the string using substring function)
In the bar chart (question 1), you’ll be able to see 5 major time blocks based on the frequency of requests made at the city and airport. You have to now divide the request-time into 5 time-slots described below. Make an additional column “Time_Slot” which takes these 5 categorical values depending on the request time: (Hint: you can either use "elseif" function or a simple conditional for loop to achieve this.) 
Pre_Morning
Morning_Rush
Day_Time
Evening_Rush
Late_Night
Note: The division of time-slots may not have one right answer.
 
PROBLEM IDENTIFICATION:
Make a stacked bar chart where each bar represents a time slot and the y-axis shows the frequency of requests. Different proportions of bars should represent the completed, cancelled and no cars available out of the total customer requests. (Hint: ggplot)
Visually identify the 2 most pressing problems for Uber, out of the 15 possible scenarios (5 slots * 3 trip status).
 
 Problem 1:
For the time slot when problem 1 exists, plot a stacked bar chart to find out if the problem is more severe for pick-up requests made at the airport or the city. As a next step, you have to determine the number of times this issue exists in that time slot. Also find the percentage breakup for the total number of issues in this time slot based on the pick-up point?
Now let’s find out the gap between supply and demand. For this case, the demand is the number of trip requests made at the city, whereas the supply is the number of trips completed from city to the airport?
What do you think is the reason for this issue for the supply-demand gap? (Write the answer in less than 100 words).?
What is your recommendation to Uber (Not more than 50 words)?
 
Problem 2:
For the time slot when problem 2 exists, plot the stacked bar chart to find out if the issue is for pick-up request made at the airport or the city. Just like problem 1, find the percentage breakup for issue based on the pick-up point for the time slot in which problem 2 exists.
Now let’s find out the gap between supply and demand. For this case, the demand is the number of trip requests made at the airport, whereas the supply is the number of trips completed from airport to the city.
What do you think is the reason for this issue for this supply-demand gap. (Not more than 100 words)?
What is your recommendation to Uber (Not more than 50 words)?