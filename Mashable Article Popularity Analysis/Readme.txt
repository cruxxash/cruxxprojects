Introducing the Problem

Problem Statement
You had explored the Mashable news article popularity dataset in EDA module. You had observed two critical insights-
Articles published on Social Media saw, on an average, a larger number of shares than other channels
The average number of shares of articles published over the weekend was more than the articles published over weekdays.
Let's say you are consulting the CEO of Mashable to help him build a strategy to maximize shares of news articles. Based on the validity of the two hypotheses above, he will consider the following changes:
Whether he should set up a team to work on weekends

If option (1) above is recommended, should the new 'weekend team' publish majority of social media news articles
 For employees who work on weekdays only: Should they publish more social media articles than those of other channels
For all social media articles they publish: Should they publish more on weekends than the weekdays  
The recommendations will be implemented only if there is a significant statistical difference between the options.
 

 
Important Note: This data set is different from the one you had used in EDA; it is compulsory to use this for the assignment. 
Article Popularity Datasetfile_download	Download
 
Data preparation and cleaning
Perform outlier treatment over the “shares” attribute using the techniques learnt in EDA module.
Figure out a way to extract Day of publishing for each article from the URL 
 
Initial Insight
You can begin the analysis by finding the average number of shares for these buckets:
 
Table 1.1
 		Weekdays	Weekends	Total
Social Media	X11		X12		Y1
Others		X21		X22		Y2
Total		Z1		Z2	 
 
Questions
Confirm at 1% significance level if the average number of shares for each article differ significantly for articles published over weekdays and the weekend (Z1 versus Z2).
Confirm at 1% significance level if the average number of shares for each article published over the weekend differ significantly for articles on Social media channel and other channels (X12 versus X22).
Confirm at 1% significance level if the average number of shares for each article published over weekdays differ significantly for articles on Social media channel and other channels (X11 versus X21).
Confirm at 5% significance level if the average number of shares for Social Media articles published over weekdays and weekends differ significantly from each other (X11 versus X12).