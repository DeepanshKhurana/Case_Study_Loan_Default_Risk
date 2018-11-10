library(readxl)
library(tidyverse)
library(lubridate)
library(gmodels)
library(corrplot)
library(lplyr)

# Read Data

loan <- read.csv("loan.csv", stringsAsFactors = F, header = T)
str(loan)
summary(loan)
# Analyzing Individual Variables 
variables <- names(loan)

colwisenavalues <- sapply(loan[,1:111], function(x) sum(is.na(x)))
colwisenavalues <- colwisenavalues[colwisenavalues != 0]

# There are 54 columns in data set which are completely NA ie all 39717
# Values of these variables are NA
# These columns are removed from analysis as there is no data in these columns and 
# No method to determine the missing data

nodatacolumns <- names(colwisenavalues[colwisenavalues == 39717])
completeNAValues <- colwisenavalues[] == 39717
noofallNAColumns <- sum(completeNAValues)

loan <- loan[, colSums(is.na(loan)) != nrow(loan)]

# After removing 54 columns with all NA Values lets look at remaining 57 columns

colwisenavalues <- sapply(loan[,1:57], function(x) sum(is.na(x)))
colwisenavalues

colwisenavalues1 <- sapply(loan[,1:57], function(x) sum(is.na(x)))
colwisenavalues1 - colwisenavalues
percentColWiseNAValues <- sapply(loan[,1:57], 
                                 function(x) 
                                   (sum(is.na(x))/length(x))*100)
percentColWiseNAValues <- percentColWiseNAValues[!near(percentColWiseNAValues, 0)]

#     title       mths_since_last_delinq        mths_since_last_record 
#   0.002517814               64.662487096               92.985371503 
#   collections_12_mths_ex_med   chargeoff_within_12_mths       pub_rec_bankruptcies 
#   0.140997558                0.140997558                1.754916031 
#     tax_liens 
#   0.098194728 

# There are still two columns viz.  "mths_since_last_delinq" 25682 NA values
# and "mths_since_last_record" 36931 NA Values
# will check later if these are significant 

# Before proceeding further the data formatting in lots of columns needs to be
# correctted

# First column to be cleaned up is "term" to a numeric value
# Extracting Term Value and converting to numeric values

loan$term <- sapply(strsplit(loan$term, " "), "[", 2)
loan$term <- as.numeric(loan$term)

# Next column to be cleaned up is "int_rate" to a numeric value
# Extracting Term Value and converting to numeric values

loan$int_rate <- sapply(strsplit(loan$int_rate, "%"), "[", 1)
loan$int_rate <- as.numeric(loan$int_rate)

# Grade and Subgrade shall be treated as Factors

loan$grade <- as.factor(loan$grade)
loan$sub_grade <- as.factor(loan$sub_grade)

summofgrade <- CrossTable(loan$grade)

# 7 levels of Grades No NA Values
# 25.4% of all loans fall in Grade A 
# 30.3% of all loans fall in Grade B
# 20.4% of all loans fall in Grade C
# 13.4% of all loans fall in Grade D

#|         A |         B |         C |         D |         E | 
#  |-----------|-----------|-----------|-----------|-----------|
#  |     10085 |     12020 |      8098 |      5307 |      2842 | 
#  |     0.254 |     0.303 |     0.204 |     0.134 |     0.072 | 
#  |-----------|-----------|-----------|-----------|-----------|

#  |         F |         G | 
#  |-----------|-----------|
#  |      1049 |       316 | 
#  |     0.026 |     0.008 | 
#  |-----------|-----------|
summofSubGrade <- CrossTable(loan$sub_grade)

# converting emp_length (length of employment into factors)

loan$emp_length[loan$emp_length == "n/a"] <- NA
loan$emp_length <- as.factor(loan$emp_length)

summary(factor(loan$emp_length))

# As 11 possible Values as per summary below
# 1 year 10+ years   2 years   3 years   4 years   5 years   6 years   7 years 
#   3240      8879      4388      4095      3436      3282      2229      1773 
# 8 years   9 years  < 1 year       NA's 
#   1479      1258      4583      1075 

# There are 1075 NA Values
# Distribution as 1075 is a very small % of 39717 Values

# Coverting home ownership to factor
# 3 cases have home_ownership as "NONE" which can be considered as missing 
# Unlikely that 3 cases are Homeless therefore needs to be changed to 
# mode of Home Ownership

# Define a function to find Mode of a Vector x as this may be useful in other
# Cases as well
# 

Mode <- function(x) {
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}

loan$home_ownership[loan$home_ownership == "NONE"] <- Mode(loan$home_ownership)
loan$home_ownership <- as.factor(loan$home_ownership)
CrossTable(loan$home_ownership)

#  |  MORTGAGE |     OTHER |       OWN |      RENT | 
#  |-----------|-----------|-----------|-----------|
#  |     17659 |        98 |      3058 |     18902 | 
#  |     0.445 |     0.002 |     0.077 |     0.476 | 
#  |-----------|-----------|-----------|-----------|

# 44.5% of obeservations have Home on Mortgage
# 47.6% of observations have Home on Rent
# No NA and no Blanks

# "verification_status" to be converted factor

loan$verification_status <- as.factor(loan$verification_status)
CrossTable(loan$verification_status)

#   |    Not Verified | Source Verified |        Verified | 
#   |-----------------|-----------------|-----------------|
#   |           16921 |            9987 |           12809 | 
#   |           0.426 |           0.251 |           0.323 | 
#   |-----------------|-----------------|-----------------|
# In 42.6% Cases Income Not verified to be checked against loan_status
# In 54.1% cases Source Verified and only
# in 32.3 cases Income Verified 
# No NA and no Blanks

# issue_d (date the loan was issued) is a date comprising of month and year

loan$issue_d <- paste(loan$issue_d, "01", sep = "-")
loan$issue_d <- myd(loan$issue_d)
unique(loan$issue_d)

# Data for 55 Months starting June 2007 to Dec 2011

# loan_status: Current Status of loans
# Charged_Off is considered as "Defaulted"
# Current is considered as "Ongoing"
# Fully Paid is considered as "OK ie not defaulted"

loan$loan_status <- as.factor(loan$loan_status)
CrossTable(loan$loan_status)

#Total Observations in Table:  39717 


#   Charged Off |     Current |  Fully Paid | 
#  |-------------|-------------|-------------|
#  |        5627 |        1140 |       32950 | 
#  |       0.142 |       0.029 |       0.830 | 
#  |-------------|-------------|-------------|
# 14.2% Defaults
# 83% OK Loans (No Default)
# 2.9% Current Loans (Not useful for predicting defaults)

# pymnt_plan: Indicates whether payment plan put in place
summary(factor(loan$pymnt_plan))

# All Values in have defualt Values of n for all 39717 observations
# No NA or missing values
# a variable with a single value: Could be a error in Data collection 
# Or due to a defualt mode of business
# no use in terms of predicting loan default or no default
# hence dropped

loan <- loan[-18]

# url: URL for the LC page with listing data.
# the only data here is the loan ID which appears after "="

loan$url <- sapply(strsplit(loan$url, "="), "[", 2)
# As only remaining data is loan ID rename the column name to loan_id
colnames(loan)[18] <- "loan_id"

# Let's extract the data from desc

loan$desc <- sapply(strsplit(loan$desc, ">"), "[", 2)
# This data is captured efficiently in the following two columns purpose and title
# Therefore drop the column

loan <- loan[-19]

# purpose (purpose of loan) can be converted to factor

loan$purpose <- as.factor(loan$purpose)
summary(loan$purpose)

# All 39717 data can be divided into only 14 purposes of which 18641 are
# for debt consolidation

# title: is a deeper explanation for purpose leaving it as character variable

# zip_code is like PIN and self explanatory

# addr_state: State in which the applicant is located
# coverting to factor

summary(factor(loan$addr_state))
loan$addr_state <- as.factor(loan$addr_state)
CrossTable(loan$addr_state)

# Also trying to add State Names from State codes

loan$state_names <- state.name[loan$addr_state]

# dti: A ratio calculated using the borrower total monthly debt payments 
# on the total debt obligations, excluding mortgage and the requested LC loan,
# divided by the borrower's self-reported monthly income. A kind of
# debt payments to income ratio self reported

# delinq_2yrs: no. of delinquency in past two years 

# earliest_cr_line: The month the borrower's earliest reported 
# credit line was opened 
# converting to date format

loan$earliest_cr_line <- paste(loan$earliest_cr_line, "01", sep = "-")
loan$earliest_cr_line <- myd(loan$earliest_cr_line)
unique(loan$earliest_cr_line)
# Only 526 uniques values of earliest credit line
# As there are no age of applicant related data
# Credit history (length of credit history is probably critical)
# Creating variable Length of credit history in years from issue_d
# Credit_hist_years

loan$credit_hist_years <- round(as.numeric((loan$issue_d - 
                                              loan$earliest_cr_line)/365.25),0)

# inq_last_6mths: The number of inquiries in past 6 months 
# (excluding auto and mortgage inquiries)
# No Missing Values, Only 8 Unique Values of the variable
# 48.6 % had 0 other enquiries in last six month prior to this loan
# Converting to factor

CrossTable(loan$inq_last_6mths)

#  |         0 |         1 |         2 |         3 |         4 | 
#  |-----------|-----------|-----------|-----------|-----------|
#  |     19300 |     10971 |      5812 |      3048 |       326 | 
#  |     0.486 |     0.276 |     0.146 |     0.077 |     0.008 | 
#  |-----------|-----------|-----------|-----------|-----------|

#  |         5 |         6 |         7 |         8 | 
#  |-----------|-----------|-----------|-----------|
#  |       146 |        64 |        35 |        15 | 
#  |     0.004 |     0.002 |     0.001 |     0.000 | 
#  |-----------|-----------|-----------|-----------|

loan$inq_last_6mths <- as.factor(loan$inq_last_6mths)

# Months Since Last Delinquency
# Convert to years since last delinquency (divide by 12 and round)

summary(factor(round(loan$mths_since_last_delinq/12,0)))

#    0     1     2     3     4     5     6     7     8     9    10  NA's 
# 1215  2144  2936  2353  2213  1344  1471   350     3     4     2 25682 

# the data can be coverted to factors
# NA Values are 25682 out of 39717 
# NA could represent no delinquencies (At least No Known delinquencies)
# Therefore converting to years and replacing NA values with "None" 

loan$mths_since_last_delinq <- round(loan$mths_since_last_delinq/12, 0)
loan$mths_since_last_delinq[is.na(loan$mths_since_last_delinq)] <- "None"
loan$mths_since_last_delinq <- as.factor(loan$mths_since_last_delinq)

# mths_last_record: The number of months since the last public record
# Public record of delinquency? 
# Variable SImilar to previous variable 
# Converting to years since last public record 
# NA Values imply "No Known Public Record" therefore converting to "None"

summary(factor(round(loan$mths_since_last_record/12,0)))

#   0     1     2     3     4     5     6     7     8     9    10    11  NA's 
#  672    10    49    51    78    98   107   305   626   566   223     1 36931 

loan$mths_since_last_record <- round(loan$mths_since_last_record/12, 0)
loan$mths_since_last_record[is.na(loan$mths_since_last_record)] <- "None"
loan$mths_since_last_record <- as.factor(loan$mths_since_last_record)

# open_acc: The number of open credit lines in the borrower's credit file
# No NA or blanks the variable is a left skewed distribution with outliers


summary(factor(loan$open_acc))
ggplot(loan, aes(x = factor(loan$open_acc))) +
  geom_histogram(stat = "count")

# pub_rec: Number of derogatory public records
# No NA or Blanks

summary(factor(loan$pub_rec))
#     0     1     2     3     4 
# 37601  2056    51     7     2 

# Only four levels converting to Factor

loan$pub_rec <- as.factor(loan$pub_rec)

# revol_balance: Total credit revolving balance (monthly)

# revol_util: Revolving line utilization rate, or the 
# amount of credit the borrower is using relative to 
# all available revolving credit.
# removing % sign and conevrting to numeric data

loan$revol_util <- as.numeric((gsub(pattern = "%", replacement = "", 
                                    loan$revol_util)))

# total_acc: The total number of credit lines currently in the borrower's credit file

summary(factor(loan$total_acc))
ggplot(loan, aes(x = factor(loan$total_acc))) +
  geom_histogram(stat = "count")

# initial_list_status: The initial listing status of the loan. 
# Possible values are W, F W implies loan offered as whole ?
# f implies loans offered as partial ?
summary(factor(loan$initial_list_status))
# f 
# 39717

# All observations have a single value = "f" 
# Variables with a single value for all observations have no predictive value
# Therefore dropping this column

loan <- loan[-34]

# out_prncp: Remaining outstanding principal for total amount funded

# out_prncp_inv: Remaining outstanding principal for portion of 
# total amount funded by investors

# total_pymnt: Payments received to date for total amount funded

# total_pymnt: Payments received to date for portion of 
# total amount funded by investors

# total_rec_prncp: Principal received to date

# total_rec_int: Interest received to date

# total_rec_late_fee: Late fees received to date

# recoveries: post charge off gross recovery

# collection_recovery_fee: post charge off collection fee

# Creating new Variable loss_on_prncp = out_prncp - recoveries + collection_recovery_fee
# Only for charged of accounts

loan <- mutate_which(loan, loan_status == "Charged Off", 
             loss_on_prncp = out_prncp - recoveries + collection_recovery_fee)


Total_loss_onChargedOff <- mean(loan$loss_on_prncp, na.rm = T)

# last_pymnt_d: Last total payment amount received
# Converting to date format
# Last payment dates range from May 2005 to Jan 2015
# 71 NA data

loan$last_pymnt_d <- paste(loan$last_pymnt_d, "01", sep = "-")
loan$last_pymnt_d <- myd(loan$last_pymnt_d)
unique(loan$last_pymnt_d)
summary(factor(loan$last_pymnt_d))

# Last payment dates range from Jan 2008 to May 2016
# 71 NA data

# last_pymnt_amnt: Last total payment amount received

# next_pymnt_d: Last total payment amount received
# Converting to date format

loan$next_pymnt_d <- paste(loan$next_pymnt_d, "01", sep = "-")
loan$next_pymnt_d <- myd(loan$next_pymnt_d)
unique(loan$next_pymnt_d)
summary(factor(loan$next_pymnt_d))

# last_credit_pull_d: The most recent month LC pulled credit for this loan
# Converting to Date format

loan$last_credit_pull_d <- paste(loan$last_credit_pull_d, "01", sep = "-")
loan$last_credit_pull_d <- myd(loan$last_credit_pull_d)
unique(loan$last_credit_pull_d)
summary(factor(loan$last_credit_pull_d))

# collections_12_mths_ex_med: Number of collections in 
# 12 months excluding medical collections

summary(factor(loan$collections_12_mths_ex_med))
#   0  NA's 
# 39661    56 

# Given that the data in collections_12_mths_ex_med
# consists of 39661 "0s" and 56 NA Values 
# this column is dropped as it has no data 

loan <- loan[-47]

# policy_code: publicly available policy_code=1
# new products not publicly available policy_code=2

summary(factor(loan$policy_code))
# 1 
# 39717 

# All policy are basis publicly available products
# Hence as all observations are same for this variable it is dropped

loan <- loan[-47]

# appplication_type: All application types are individual

summary(factor(loan$application_type))
# INDIVIDUAL 
# 39717 

# as all observations of variable are of type "INDIVIDUAL"
# This is dropped

loan <- loan[-47]

# acc_now_delinq: The number of accounts on which the borrower is now delinquent
# All Values are 0 see below

summary(factor(loan$acc_now_delinq))
# 0 
# 39717 

# Dropping variable with all values 0 as no predictive value 
# and no way to predict any values

loan <- loan[-47]

# chargeoff_within_12_mths:Number of charge-offs within 12 months 
# No data in this variable 

summary(factor(loan$chargeoff_within_12_mths))
#   0     NA's 
# 39661    56 

# Dropping variable with all values 0 as no predictive value 
# and no way to predict any values

loan <- loan[-47]

# delinq_amnt: The past-due amount owed for 
# the accounts on which the borrower is now delinquent.

summary(factor(loan$delinq_amnt))
# 0 
# 39717 

# Dropping variable with all values 0 as no predictive value 
# and no way to predict any values

loan <- loan[-47]

# pub_rec_bankruptcies: Number of public record bankruptcies
# 1674 cases of 1 Bankruptcies
# 7 cases of 2 Bankruptcies

summary(factor(loan$pub_rec_bankruptcies))
#     0     1       2   NA's 
#   37339  1674     7   697 

# NAs may be interesting in this case as they need to be correlated with 
# loan Defaults to check if significant
# Converting to factor variable

loan$pub_rec_bankruptcies <- as.factor(loan$pub_rec_bankruptcies)

# tax_liens: Number of tax liens 

summary(factor(loan$tax_liens))
# 0       NA's 
# 39678    39 

# Dropping variable with 39678 values 0 and 39 NA as no predictive value 
# and no way to predict any values

loan <- loan[-48]

# Let us look at each column name and see the meaning

label <- read_xlsx("/Users/aga/Data_Dictionary.xlsx", sheet = "LoanStats",
                   col_names = T, trim_ws = T)

relevantNames <- names(loan)

labelwithdata <- label[is.element(label$LoanStatNew, relevantNames),]
labelnodata <- label[!is.element(label$LoanStatNew, relevantNames),]

# Write Variable Names with Meanings
# These files will also be used to note down changes made to data within these 
# columns

write.csv(labelwithdata, "labelwithdata.csv", row.names = F)
write.csv(labelnodata, "labelnodata.csv", row.names = F)

# Missing Values in emp_title

# emp_title :: The job title supplied by the Borrower when applying for the loan.*
# * Employer Title replaces Employer Name for all loans listed after 9/23/2013
# it is a categorical variable and probably has no significant predictive value
# Therefore replacing Missing values with NA

loan$emp_title[loan$emp_title == ""] <- NA
loan$emp_title[loan$emp_title == " "] <- NA


############################ UNIVARIATE ANALYSIS #############################


summary(loan$loan_amnt)

my_data <- loan[, c(3:5,7,8,14)]
cor(my_data)
res <- cor(my_data)
round(res, 2)

# corr plot clearly shows there is almost 100% correleation between loan amount desired 
# and loan amount given
# low correlation between loan amount and interest rate indicating inconclusive

# now finding key statistics of numerical variables

summary(my_data)
#mean loan amount given is 11219 with a mean interest rate of 12 %

#doing segmented univariate analysis using plots as plots will clearly indicate the relationship


#loan amount versus interest rate

ggplot(loan, aes(x = loan_amnt,y=int_rate))+geom_point()
#nothing conclusive can be said here as at say 20000 amount all interest rates can be seen


#putting loan in bins of 5000

loan$loan_bin[loan$loan_amnt<=5000]<-1
loan$loan_bin[(loan$loan_amnt>5000) & (loan$loan_amnt<=10000) ]<-2
loan$loan_bin[(loan$loan_amnt>10000) & (loan$loan_amnt<=15000) ]<-3
loan$loan_bin[(loan$loan_amnt>15000) & (loan$loan_amnt<=20000) ]<-4
loan$loan_bin[(loan$loan_amnt>20000) & (loan$loan_amnt<=25000) ]<-5
loan$loan_bin[(loan$loan_amnt>25000) & (loan$loan_amnt<=30000) ]<-6
loan$loan_bin[(loan$loan_amnt>30000)]<-7

loan$loan_bin=as.factor(loan$loan_bin)
loan$term=as.factor(loan$term)
loan$grade=as.factor(loan$grade)

amt_int<-aggregate(int_rate~loan_bin, loan, mean)
ggplot(amt_int, aes(x = factor(loan_bin),y=int_rate))+geom_bar(stat="identity",fill="green",color="red")
#its clearly seen as amount rises, mean interest rate also rises

amt_int_term<-aggregate(int_rate~loan_bin+term, loan, mean)
#loan amount versus interest rate versus term
ggplot(amt_int_term, aes(x = factor(loan_bin),y=int_rate))+
  geom_bar(stat="identity",fill="blue",color="red")+
  facet_wrap(~term)
#it is clearly seen that interest rate is low for 36 month period as compared to 60 month


#loan amount vs int rate versus grade
amt_int_grade<-aggregate(int_rate~loan_bin+grade, loan, mean)

ggplot(amt_int_grade, aes(x = factor(loan_bin),y=int_rate))+
  geom_bar(stat="identity",fill="blue",color="red")+
  facet_wrap(~grade)
# so grade A is provided to lowest interest rate loans and grade G to highest

#loan amount vs grade
amt_grade<-aggregate(loan_amnt~grade, loan, mean)

ggplot(amt_grade, aes(x = grade,y=loan_amnt))+
  geom_bar(stat="identity",fill="blue",color="red")
#loan grade is deteriorating as loan amount is rising as risk increses with amount

#employ length vs loan amt

loan$emp_length=as.factor(loan$emp_length)

amt_length<-aggregate(loan_amnt~emp_length, loan, mean)

ggplot(amt_length, aes(x = emp_length,y=loan_amnt))+
  geom_bar(stat="identity",fill="blue",color="red")
#as employ length increases, mean loan amount taken increases

#loan status versus loan amount

loan$loan_status=as.factor(loan$loan_status)

amt_status<-aggregate(loan_amnt~loan_status, loan, mean)

ggplot(amt_status, aes(x = loan_status,y=loan_amnt))+
  geom_bar(stat="identity",fill="green",color="red")
# small loan amountsare easily fully paid while current loans are taken on emi 

#loan status versus loan amount vs home ownership

loan$home_ownership=as.factor(loan$home_ownership)

amt_status_ownership_home<-aggregate(loan_amnt~loan_status+home_ownership, loan, mean)

ggplot(amt_status_ownership_home, aes(x = loan_status,y=loan_amnt))+
  geom_bar(stat="identity",fill="red",color="red")+facet_wrap(~home_ownership)
#in mortgage category loan amount in current status is highest as people go for the emi options more 
# and charged off in mortgage is also high as they already have put collateral of house for loans

#loan status versus loan amount vs verification status
loan$verification_status=as.factor(loan$verification_status)

amt_status_ownership_verify<-aggregate(loan_amnt~loan_status+verification_status, loan, mean)

ggplot(amt_status_ownership_verify, aes(x = loan_status,y=loan_amnt))+
  geom_bar(stat="identity",fill="green",color="red")+facet_wrap(~verification_status)
#it can be seen that verified sources only are given higher loan amounts

#plotting the above in different style

ggplot(amt_status_ownership_verify, aes(x = verification_status,y=loan_amnt))+
  geom_bar(stat="identity",fill="green",color="red")+facet_wrap(~loan_status)
# it can be seen charged off loans amounts are highest in case of verified as verified only get high loan amounts
#yet another plotting of same variables, now we see total sum of loan amount
loan$verification_status=as.factor(loan$verification_status)

amt_status_ownership_verify_sum<-aggregate(loan_amnt~loan_status+verification_status, loan, sum)
ggplot(amt_status_ownership_verify_sum, aes(x = verification_status,y=loan_amnt))+
  geom_bar(stat="identity",fill="green",color="red")+facet_wrap(~loan_status)
# in terms of total loan amount too, charge off is higher in case of verified accounts


############################end of UNIVARIATE ANALYSIS #############################






#### Bivariate Analysis

# As current loans have no bearing on default status dropping from analysis
# creating new data with current loans dropped

loans2 <- filter(loan, loan_status != "Current")
loans2$loan_status <- as.factor(loans2$loan_status)

### Business Explanations for choosing the variables:

## Categorical:
# grade - the grade assigned to the loan by the LC is based on various factors and could impact loan_status.
# purpose - purpose can impact loan_status and could help us understand which loans are likely to be charged off.
# verification_status - people with LC verified income sources should (logically) be able to pay loans so this could be a factor.
# revol_util - if a borrower is using most of the available credit, they will have trouble repaying it. So, we could check for correlation.
# home_ownership - borrowers with fixed residences are in better positions to repay loans. We could test this assumption.

## Numerical :
# int_rate - borrowers with higher interest rates will have difficulties paying the loans back. int_rate could impact loan_status
# emp_length - borrowers who have been employed for longer periods should be in a better position to repay loans.
# loan_amnt - the higher the loan_amnt, the difficult it is to repay. We could see if loan_amnts impact the loan_status.
# installment - similar assumption as the previous one.
# dti - the dti ratio tells the borrowers monthly debt payments to monthly income. This could impact loan_status if the buyer has defaulted on other debt payments as well.
# open_acc - the number of open credit lines to a borrower can impact their ability to repay a loan.
# pub_rec_bankruptcies - borrowers with more publicly recorded bankruptcies are more risky than those with fewer or none.



### Checking Categorical Variables for 
# Null Hypothesis: Variables are independent
# Alternate Hypothesis: Variable are correlated
# https://stattrek.com/chi-square-test/independence.aspx

## Applying Chi Square Test between grade and loan Status (both categorical)

chisq.test(table(factor(loans2$loan_status),loans2$grade),correct=F)
ggplot(loans2, aes(x = grade, fill = loan_status)) +
  geom_bar(position = "fill")

# Pearson's Chi-squared test
# data:  table(factor(loans2$loan_status), loans2$grade)
# X-squared = 1581.2, df = 6, p-value < 2.2e-16
# As p Value is very low the Null Hypothesis is rejected
# Grade has a very high predictive value for loan_default


## Checking loan_status against purpose

chisq.test(table(factor(loans2$loan_status),loans2$purpose),correct=F)
ggplot(loans2, aes(x = purpose, fill = loan_status)) +
  geom_bar(position = "fill")

#Pearson's Chi-squared test
#data:  table(factor(loans2$loan_status), loans2$purpose)
#X-squared = 378.94, df = 13, p-value < 2.2e-16

## Checking Loan Status against Verification Status

chisq.test(table(factor(loans2$loan_status), loans2$verification_status), correct=F)

#	Pearson's Chi-squared test
# data:  table(factor(loans2$loan_status), loans2$verification_status)
# X-squared = 89.856, df = 2, p-value < 2.2e-16
# This means that the loan_status and verification_status are correlated.

ggplot(loans2, aes(x = verification_status, fill = loan_status)) +
  geom_bar(position = "fill")

## Checking loan_status against revol_util

# Converting revol_util to category

loans2$revol_util_category <- cut(loans2$revol_util, breaks = c(0,10,20,30,40,50,60,70,80,90,100), include.lowest = TRUE)

# Pearson's Chi-squared test
# data:  table(factor(loans2$loan_status), loans2$revol_util_category)
# X-squared = 394.45, df = 9, p-value < 2.2e-16
# This means that the loan_status and revol_util are correlated.

chisq.test(table(factor(loans2$loan_status), loans2$revol_util_category), correct=F)

ggplot(loans2, aes(x = revol_util_category, fill = loan_status)) +
  geom_bar(position = "fill")

# Some values are missing in revol_util so they have produced NA values. However, these are just 50 and won't impact our dataset.

sum(is.na(loans2$revol_util_category))

sum(is.na(loans2$revol_util))

## Checking home_ownership against loan_status

chisq.test(table(factor(loans2$loan_status), loans2$home_ownership), correct=F)

# 	Pearson's Chi-squared test
# data:  table(factor(loans2$loan_status), loans2$home_ownership)
# X-squared = 21.665, df = 3, p-value = 7.659e-05
# Here, the value is slightly larger than other p-values. There is a correlation since it is still
# less than alpha = 0.05 but the correlation is slightly less pronounced than say, that of revol_util

ggplot(loans2, aes(x = home_ownership, fill = loan_status)) +
  geom_bar(position = "fill")


### Checking Numerical Variables against Loan_status

# Cleaning emp_length

loans2$emp_length <- gsub("years", "", loans2$emp_length)
loans2$emp_length <- gsub("year", "", loans2$emp_length)
loans2$emp_length <- gsub("< 1", "0", loans2$emp_length)
loans2$emp_length <- gsub("[+]", "", loans2$emp_length)
loans2$emp_length <- as.integer(loans2$emp_length)

#Converting pub_rec_bankruptcies to numeric
loans2$pub_rec_bankruptcies <- as.integer(loans2$pub_rec_bankruptcies)

## Checking int_rate against loan_status

ggplot(loans2, aes(x = loan_status, y = int_rate, fill = loan_status)) +
  geom_boxplot()

# Charged off loans had a higher median value than Fully paid loans

chargedoff <- filter(loans2, loans2$loan_status == "Charged Off")
fullypaid <- filter(loans2, loans2$loan_status == "Fully Paid")

mean_int_rate <- c(mean(chargedoff$int_rate),mean(fullypaid$int_rate))
median_int_rate <- c(median(chargedoff$int_rate),median(fullypaid$int_rate))

mean_int_rate[1] - mean_int_rate[2] #The mean interest rate for charged off loans is 2.21% higher
median_int_rate[1] - median_int_rate[2] #The median interest rate for charged off loans is 2.12% higher.

ggplot(loans2, aes(x = int_rate, col = loan_status)) +
  geom_histogram(binwidth = 2 ,fill = "white")

## Checking emp_length against loan_status

# We're marking < 1 as 0 years and considering all 10+ as 10 for making it easier to analyse.
# There are no 10 values in the data, there are 1033 NA values however.

# sum(is.na(loans2$emp_length))

ggplot(loans2, aes(x = loan_status, y = emp_length, fill = loan_status)) + geom_boxplot()

mean_emp_length <- c(mean(chargedoff$emp_length, na.rm = TRUE), mean(fullypaid$emp_length, na.rm = T))
median_emp_length <- c(median(chargedoff$emp_length, na.rm = TRUE), mean(fullypaid$emp_length, na.rm = T))

mean_emp_length[1] - mean_emp_length[2] #0.16 higher for charged off loans
median_emp_length[1] - median_emp_length[2] #0.082 higher for charged off loans

# The mean employment time for charged off loans is larger than the mean employment length of fully paid loans.
# The median follows the same but the difference is half that of charged loans.

## Checking loan_amnt against loan_status

ggplot(loans2, aes(x = loan_status, y = loan_amnt, fill = loan_status)) + geom_boxplot()

mean_loan_amnt <- c(mean(chargedoff$loan_amnt, na.rm = TRUE), mean(fullypaid$loan_amnt, na.rm = T))
median_loan_amnt <- c(median(chargedoff$loan_amnt, na.rm = TRUE), median(fullypaid$loan_amnt, na.rm = T))

mean_loan_amnt[1] - mean_loan_amnt[2] #The mean for charged off loans is 1200 higher.
median_loan_amnt[1] - median_loan_amnt[2] #The median for fully paid loans is 400 higher.

# It is interesting to note that the fully paid loans are slightly less than charged off loans in terms of loan amount.

## Checking installment against loan_status

ggplot(loans2, aes(x = loan_status, y = installment, fill = loan_status)) + geom_boxplot()

mean_installment <- c(mean(chargedoff$installment, na.rm = TRUE), mean(fullypaid$installment, na.rm = T))
median_installment <- c(median(chargedoff$installment, na.rm = TRUE), median(fullypaid$installment, na.rm = T))

mean_installment[1] - mean_installment[2] #The mean installment for charged off loans is higher by 16.
median_installment[1] - median_installment[2] #The median installment for charged off loans is higher by 18.

## Checking dti against loan_status

ggplot(loans2, aes(x = loan_status, y = dti, fill = loan_status)) + geom_boxplot()

mean_dti <- c(mean(chargedoff$dti, na.rm = TRUE), mean(fullypaid$dti, na.rm = TRUE))
median_dti <- c(median(chargedoff$dti, na.rm = TRUE), median(fullypaid$dti, na.rm = T))

mean_dti[1] - mean_dti[2] #The mean dti for charged off loans is 0.85 higher.
median_dti[1] - median_dti[2] #The median dti for fully paid loans is 1.09 higher.

## Checking open_acc against loan_status

ggplot(loans2, aes(x = open_acc, fill = loan_status)) + geom_bar(position = "fill")
ggplot(loans2, aes(x = loan_status, y = open_acc, fill = loan_status)) + geom_boxplot()

mean_open_acc <- c(mean(chargedoff$open_acc, na.rm = TRUE), mean(fullypaid$open_acc, na.rm = TRUE))
median_open_acc <- c(median(chargedoff$open_acc, na.rm = TRUE), median(fullypaid$open_acc, na.rm = TRUE))

mean_open_acc[1] - mean_open_acc[2] #The mean open credit lines for charged off loans is less than that of fully paid ones.
median_open_acc[1] - median_open_acc[2] #The median credit lines for charged off loans is less than that of fully paid ones by 1. 

## Checking pub_rec_bankruptcies against loan_status

ggplot(loans2, aes(x = factor(pub_rec_bankruptcies), fill = loan_status)) + geom_bar(position = "fill")

mean_bankruptices <- c(mean(chargedoff$pub_rec_bankruptcies, na.rm = TRUE), mean(fullypaid$pub_rec_bankruptcies, na.rm = TRUE))
median_bankruptcies <- c(median(chargedoff$pub_rec_bankruptcies, na.rm = TRUE), median(fullypaid$pub_rec_bankruptcies, na.rm = TRUE))

mean_bankruptices[1] - mean_bankruptices[2] #The mean bankruptcies for charged off loans is slightly higher than that off fully paid loans.
median_bankruptcies[1] - median_bankruptcies[2] #The median bankruptcies for both charged off and fully paid loans is 1.

# However, as the visualisation shows, people with 3 bankruptcies have more charged off loans than those with 1 and 2.
# There is a visible correlation obviously but perhaps, it is not as pronounced.

## Conclusion:

# Categorical:
# grade, purpose, verification status, revol_util seem to have strong correlation with loan_status.
# home_ownership doesn't seem to have a pronounced correlation.

# Numeric:
# int_rate seems to have a clear impact on the charged off loans.
# Since median and mean both indicate lowering the interest rates could help in decreasing charged off loans.
# loan_amt, dti, installment - these features could have a correlation or impact on loan_status but their impact couldn't be called as pronounced.
# pub_rec_bankruptcies - This is an odd case, while the correlation isn't numerically visible.
# Visually, it is quite clear that there is an effect on the loan_status with increasing public bankruptcies.
# It is also logically sound as a conclusion.

# The top 5 factors to focus on then are as follows:
# grade, purpose, verification_status, revol_util, int_rate
# Then, we could look at pub_rec_bankruptcies

# Let us now consider the data further

# ID and Member ID are random variables assigned to the loan account
# Dropping them

loans2 <- loans2[-1]
loans2 <- loans2[-1]

m <- cor(loans2[,c(1:3, 5:6, 12, 21, 29, 34:37, 42)])
corrplot(m, method = "number", type = "upper")

# Following are highly correlated viz.
# loan_amnt, funded_amnt, funded_amnt_inv, installment, total_pymnt, total_pymnt etc



pl <- ggplot(loans2[,c(1:3, 5:6, 12, 21, 29, 34:37, 42)])
pl + geom_boxplot(aes(x = factor(loans2$loan_bin), 
                      y = loans2$funded_amnt, fill = loans2$loan_status))

ggplot(loans2, aes(x = funded_amnt, fill = loan_status)) +
  geom_histogram() +
  facet_wrap(~loan_bin, ncol = 1)

# There is not much difference in loan_status due to mean funded amount 
# alomost same in each loan category 

pl +geom_boxplot(aes(x = loans2$loan_bin, 
                     y = loans2$emp_length, fill = loans2$loan_status))

# There is no clear trend except maybe higher employment length with loan amounts

ggplot(loans2, aes(x = loan_bin, 
                   y = int_rate, fill = loan_status)) +
  geom_boxplot()

## Interest rates in each loan category for Charged of case are higher
## High Interest rates are causing defaults ? Tentatively

ggplot(loans2, aes(x = grade, 
                   y = int_rate, fill = loan_status)) +
  geom_boxplot()

## Interest rates in lower grades are higher which is understandable
## But Interest rates in the case of defaults were higher still in D, E, F, G
## Therefore higher interest rates in grades D E F G play a higher role 
## In defaults

ggplot(loans2, aes(x = sub_grade, 
                   y = int_rate, fill = loan_status)) +
  geom_boxplot()

## Let's look at annual income, grade, loan_bin and loan_status next

ggplot(loans2, aes(x = loan_bin, y = log10(annual_inc), col = loan_status)) +
  geom_boxplot()

ggplot(loans2, aes(x = grade, y = log10(annual_inc), col = loan_status)) +
  geom_boxplot()

## There is a clear relationship lower annual income in each loan_bin 
## and within each grade the grading system may need to be revised 
## "Charged Off" cases

## Let's look at dti as a parameter within each loan_bin

ggplot(loans2, aes(x = loan_bin, y = dti, fill = loan_status)) +
  geom_boxplot()

meandticoff <- chargedoff %>% group_by(loan_bin) %>% summarise(mean(dti, na.rm = T))
meandtifpoff <- fullypaid %>% group_by(loan_bin) %>% summarise(mean(dti, na.rm = T))

meandti <- cbind(meandticoff, meandtifpoff[,2])
names(meandti)[2] <- "dti Charged Off"
names(meandti)[3] <- "dti Fully Paid"    
meandti$diffindti <- meandti$`dti Charged Off` - meandti$`dti Fully Paid`

t.test(meandti$`dti Charged Off`, meandti$`dti Fully Paid`)

# Welch Two Sample t-test

# data:  meandti$`dti Charged Off` and meandti$`dti Fully Paid`
# t = 2.4624, df = 11.017, p-value = 0.03151
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.09737552 1.73404085
# sample estimates:
#  mean of x mean of y 
# 14.32404  13.40833 

# The mean dti for charged off accounts is higher

