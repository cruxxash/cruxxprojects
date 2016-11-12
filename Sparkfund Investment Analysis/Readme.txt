Objectives

Project Brief:
You are working for Spark Funds, an asset management company. Spark Funds wants to make investments in a few companies. The CEO of Spark Funds wants to understand the global trends in investments so that she can take the investment decisions effectively.
Business and Data Understanding:
Spark Funds have two minor constraints for investments:
They want to invest between 5 to 15 million USD per round of investment.
They want to invest only in English-speaking countries because of the ease of communication with the companies they’d invest in.
For your analysis, consider a country to be English speaking only if English is one of the official languages in that country.
You may use this list: Click here to know the name of countries where English is an official language
 
These conditions will give you sufficient information for your initial analysis. Before getting to specific questions, let’s understand the data first.
 
1. What is the Strategy?
Spark Funds wants to invest where most other investors are investing. This pattern is often observed among early stage start-up investors.
 
2. Where did we get the data from? 
We have taken real investment data from crunchbase.com, so the insights you get may be incredibly useful. For this group project, we have divided the data into the following files:
You have to use 3 data files for the entire analysis:
1. companies.txt: A text file with basic data of companies.
1. Attributes description of companies.txt file
Attributes	Description
 Permalink	Unique ID of company
 name		Company name
 homepage_url	Website URL
 category_list	Category/categories to which a company belongs
 status		Operational status
 country_code	Country
 state_code	State
 
2. rounds2.csv: A csv file with data about investments. The most important parameters are explained below:
2. Attribute description of rounds2.csv file
 Attributes			Description
 company_permalink 		Unique ID of company
 funding_round_permalink	Unique ID of funding round
 funding_round_type		Type of funding – venture, angel, private equity  etc.
 funding_round_code		Round of venture funding (round A, B etc.)
 funded_at			Date of funding
 raised_amount_usd		Money raised in funding (USD)
 
3. mapping_file.csv: This file maps the numerous sector names (like 3D printing, aerospace, agriculture etc.) to 8 main sector names. The purpose of having 8 main sectors is to simplify the analysis into 8 sector buckets, rather than trying to analyse hundreds of them.
 
3. What is Spark Funds’ business objective?
The business objectives and goals of data analysis are pretty straightforward.
Business objective: The objective is to identify the best sectors, countries and a suitable investment type for making investments. The overall strategy is to invest where others are investing, implying that the best sectors and countries are the ones where most investments are happening.
Goals of data analysis: Your goals are divided into 3 main sub-goals:
Investment type analysis: Understanding investments in venture, seed/angel, private equity categories etc. so Spark Funds can decide which type is best suited for their strategy.
Country analysis: Understanding which countries have had the most investments in the past. These will be Spark Funds’ favourites as well.

Sector analysis: Understanding the distribution of investments across the 8 main sectors (note that we are interested in the 8 main sectors provided in the mapping file. The 2 files, companies and rounds2, have numerous sub-sector names; hence you will need to map each sub-sector to its main sector).



Results Expected: Table 1.1 : 
 Table 1.1 : Understand the data set 
How many unique companies are present in rounds2?	
 
How many unique companies are present in companies ?	                       
In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column.	 
 Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.	 
Merge the two data frames so that all  variables (columns)  in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ?

Results Expected: Table 2.1
Table 2.1 : NA Values Treatment
How many NA values are present in the column raised_amount_usd ?	                                                                                   
What do you replace NA values of raised_amount_usd  with? Enter a numeric value.

Table 3.1 :Average values of investments for each of these funding types 
 Average funding amount of venture type	                              
 Average funding amount of angel type	 
 Average funding amount of seed type	 
 Average funding amount of private equity type	 
 Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, which investment type is the most suitable for them?

Table 4.1: Analysing top 3 English speaking countries 
 1. Top English speaking country name	              
 2. Second English speaking country name	 
 3. Third English speaking country name	 

Table 6.1 : Sector-wise Investment Analysis
   
Country1
Country2
Country3
 1. Total number of investments (count)
 
 
 
 2. Total amount of investment (USD)
 
 
 
 3. Top sector name (no. of  investment-wise)
 
 
 
 4. Second sector name (no. of  investment-wise)
 
 
 
 5. Third sector name (no. of  investment-wise)
 
 
 
 6. Number of investments in top  sector (3)
 
 
 
 7. Number of investments in second   sector (4)
 
 
 
 8. Number of investments in third   sector (5)
 
 
 
 9. For point 3 (top sector count-wise),  which company received the  highest investment?
 
 
 
 10. For point 4 (second best sector  count-wise), which company  received the highest investment?
 
 











