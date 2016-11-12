Business Objectives

Project Brief
You are working for a consumer finance company. When the company receives a loan application, the company has to make a decision for loan approval based on the applicant’s profile. Two types of risks are associated with the bank’s decision -
If the applicant is likely to repay the loan, then not approving the loan to the person results in a loss of business to the company.
If the applicant is not likely to repay the loan i.e. default, then approving the loan to the person results in a financial loss to the company.
In this case study, we consider only consumers whose loan application is approved.  Here, our aim is to understand how consumer attributes and loan attributes influencing the tendency of defaulting.


Business and Data Understanding
1. Business Understanding?
This company is the largest online credit marketplace, facilitating personal loans, business loans, and financing for elective medical procedures. Borrowers can easily access lower interest rate loans through a fast online interface. Investors provide the capital to enable many of the loans in exchange for earning interest. 
 
2. Where did we get the data from? 
Let's assume that the company has provided you the data set for analysis. It can be downloaded from the link below. It contains complete loan data for all loans issued through the time period 2007 t0 2011.

The company has come across some important attributes in order to understand behaviour of their approved loan customers w.r.t. loan default. Thus, the lending company has decided to work only on these variables to mitigate the future risk. The driver variables you need to consider for this case study are:
Attributes 	Definition 
annual_inc	Annual Income of applicant
loan_amnt	The listed amount of the loan applied for by the borrower
funded_amnt	The total amount committed to that loan at that point in time
int_rate	Interest Rate on the loan
grade
LC 		assigned loan grade
dti		Debt to income ratio
emp_length	Employment length in years
Purpose	A category provided by the borrower for the loan request. 

home_ownership	The home ownership status provided by the borrower during registration
loan_status	Current status of the loan
 
You can access the data dictionary which describes the meaning of these variables from the provided link below:

 
3. What is company’s business objective?
The business objectives and goals of data analysis are pretty simple. The company wants to understand the driving factors behind loan default (loan_status_1).  The company can utilise this knowledge for its portfolio and risk assessment. Specifically, the company wants to determine which driver variables are having the most influence on the tendency of loan default.
 
Your goal is divided into 3 main parts:
Data Preparation
Exploratory Data Analysis
Hypothesis testing