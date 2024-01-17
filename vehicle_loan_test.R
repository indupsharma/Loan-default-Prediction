#' ---
#' authors: Hill, Jarrouje, Le, Sharma & Teh 
#' ---

# OR 568 Spring 2023
# Project Title: Vehicle Loan Default Prediction
# Due Date: 05/4/2023 

# Prints the installed version of R by executing the following R code:

cat("R version=", paste(R.version$major, R.version$minor,sep = "."), "\n")

# Loading and/or installing needed packages

if( !require('AppliedPredictiveModeling') ){
  install.packages('AppliedPredictiveModeling', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(AppliedPredictiveModeling)


if( !require('PerformanceAnalytics') ){
  install.packages('PerformanceAnalytics', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(PerformanceAnalytics)

if( !require('caret') ){ # To get BoxCoxTrans
  install.packages('caret', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(caret)

if( !require('corrplot') ){ # A visualization of a correlation matrix.
  install.packages('corrplot', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(corrplot)

if( !require('e1071') ){ # To get skewness & other functions
  install.packages('e1071', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(e1071)

if( !require('tidyverse') ){
  install.packages('tidyverse', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(tidyverse)

if( !require('tibble') ){
  install.packages('tibble', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(tibble)

if( !require('RColorBrewer') ){
  install.packages('RColorBrewer', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(RColorBrewer)

if( !require('GGally') ){
  install.packages('GGally', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(GGally)

if( !require('gridExtra') ){
  install.packages('gridExtra', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(gridExtra)

if( !require('ggplot2') ){
  install.packages('ggplot2', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(ggplot2)

if( !require('dplyr') ){
  install.packages('dplyr', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(dplyr)

if( !require('forecast') ){
  install.packages('forecast', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(forecast)

if( !require('Ecdat') ){
  install.packages('Ecdat', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(Ecdat)

if( !require('sandwich') ){
  install.packages('sandwich', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(sandwich)

if( !require('fracdiff') ){
  install.packages('fracdiff', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(fracdiff)

if( !require('lattice') ){
  install.packages('lattice', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(lattice)

if( !require('psych') ){
  install.packages('psych', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(psych)

if( !require('lubridate') ){
  install.packages('lubridate', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(lubridate)

if( !require('eeptools') ){
  install.packages('eeptools', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(eeptools)

if( !require('kernlab') ){
  install.packages('kernlab', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(kernlab)


if( !require('pls') ){
  install.packages('pls', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(pls)

if( !require('glmnet') ){
  install.packages('glmnet', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(glmnet)

if( !require('randomForest') ){
  install.packages('randomForest', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(randomForest)

if( !require('hrbrthemes') ){
  install.packages('hrbrthemes', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(hrbrthemes)

if( !require('tidyr') ){
  install.packages('tidyr', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(tidyr)

if( !require('DataExplorer') ){
  install.packages('DataExplorer', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(DataExplorer)

if( !require('readr') ){
  install.packages('readr', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(readr)

if( !require('party') ){
  install.packages('party', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(party)

# Raw Data Set & Source
#
# Data Source: Kaggle (Avik Paul)
# Vehicle Load Data Set

# Loading Vehicle Loan Data set in its original or non-formatted state
df2 <- read.csv(file="VehicleLoan.csv",
  header=TRUE, as.is=TRUE)

# Identify missing values
missing_values2 <- data.frame(
  variable = names(df2),
  num_na = sapply(df2, function(x) sum(is.na(x))),
  num_null = sapply(df2, function(x) sum(is.null(x))),
  num_nan = sapply(df2, function(x) sum(is.nan(x))),
  num_empty = sapply(df2, function(x) sum(x == ""))
)

# Print the missing values
# print(missing_values2)

# # Overview of the df data frame #
# High-level Summary (Raw Data Sets)
# Display data summary and statistics
#str(df2) # Displays vehicle loans data set internal structure
#summary(df2)
#head(df2, 5) # Displays the first 5 rows of the data set
#dim(df2)
#View(df2)

# Display of "DATE_OF_BIRTH" and # # "DISBURSAL_DATE" data type using class fn 
# class(df$DATE_OF_BIRTH) 
# class(df$DISBURSAL_DATE) 

# Format "DATE_OF_BIRTH" & "DISBURSAL_DATE" date columns

# Code for two or multiple date columns
# Load the lubridate package
library(lubridate)

# Assignment of df_no_format data frame to dates_df data frame
dates_df2 <- df2

# Add empty columns for the formatted dates
dates_df2$formatted_DATE_OF_BIRTH <- NA
dates_df2$formatted_DISBURSAL_DATE <- NA

# Define the list of date formats to try
date_formats2 <- c("%m/%d/%Y", "%d-%m-%Y", "%Y-%m-%d", "%Y-%d-%m")

# Loop over the rows of the data frame
for (i in seq_len(nrow(dates_df2))) {
  # Loop over the two date columns
  for (j in c("DATE_OF_BIRTH", "DISBURSAL_DATE")) {
    # Try to parse the date string using the defined date formats using 
    # lubridate::parse_date_time()
    parsed_date2 <- tryCatch(parse_date_time(dates_df2[[j]][i], 
                                            orders = date_formats2), 
                            error = function(e) NA)
    # If the parsing was successful, format the date object using the format()
    # function and add it to the data frame
    if (!is.na(parsed_date)) {
      formatted_date_string2 <- format(parsed_date2, "%m/%d/%Y")
      dates_df2[[paste0("formatted_", j)]][i] <- formatted_date_string2
    }
  }
}
# Write the updated data frame to a new CSV file
# write.csv(dates_df, "dates_formatted_VehicleLoan.csv", row.names = FALSE)

#str(dates_df2)

# Convert the character columns to date columns using ymd() or mdy() or dmy() 
# depending on the format for both formatted_DATE_OF_BIRTH & 
# formatted_DISBURSAL_DATE

# Convert the character columns to date columns using ymd() or mdy() or dmy() 
# depending on the format
# month-day-year format
dates_df2$formatted_DATE_OF_BIRTH <- mdy(dates_df2$formatted_DATE_OF_BIRTH) 
# month-day-year format
dates_df2$formatted_DISBURSAL_DATE <- mdy(dates_df2$formatted_DISBURSAL_DATE) 

# Displays internal structure of the data frame with formatted date columns 
#str(dates_df2)


# Format and add a new AGE column to the data frame 


# Read the new formatted Vehicle Loan CSV file into a data frame

# Assignment of dates_df data frame to age_df data frame
age_df2 <- dates_df2

# Load the lubridate package
# library(lubridate)

# Convert the "formatted_DATE_OF_BIRTH" column to a date object using the 
# ymd() function
age_df2$formatted_DATE_OF_BIRTH <- ymd(age_df2$formatted_DATE_OF_BIRTH)

# Calculate the current year
current_year2 <- year(Sys.Date())

# Create a new column "Age" containing the age in years
age_df2$AGE <- current_year2 - year(age_df2$formatted_DATE_OF_BIRTH)

#str(age_df2)
#
# View(age_df)

# Remove/exclude both "DATE_OF_BIRTH", "DISBURSAL_DATE" columns to avoid 
# duplication of columns and/or data points

# Exclude columns 'DATE_OF_BIRTH' and 'DISBURSAL_DATE'
age_df2 <- age_df2[, -c(9, 11)]

# Display data summary and statistics
# str(age_df2) # Displays vehicle loans data set internal structure
#summary(age_df2)

# Here, the code ONLY identify the Near-Zero Variance Predictors and provides
# useful metrics
nzv_vals2 <- nearZeroVar(age_df2, saveMetrics = TRUE)
#dim(nzv_vals2)
## [1] 42  4
#nzv_vals2
#View(nzv_vals2)

# Sort descending order and by putting NA top (If available)
nzv_vals2 <- nzv_vals[order(nzv_vals2$nzv, nzv_vals2$zeroVar),]
#dim(nzv_vals2)
#nzv_vals2

# Here, the code actually removes near-zero variance predictors
df_nzv2 <- age_df2[,-nearZeroVar(age_df2)] #remove near-zero variance predictors
#dim(df_nzv2)
# [1] 233154     30
# View(df_nzv)

#str(df_nzv2) # Displays new_df_nzv_VehicleLoan.csv data set internal structure
#
#View(df_nzv2)

# Check for missing values in numeric data type
 sum(is.na(df_nzv))

# Check for missing values specifically for EMPLOYMENT_TYPE column
# Filter the data set to show only rows with blank spaces in the 
# EMPLOYMENT_TYPE column
missing_employment2 <- df2 %>% filter(EMPLOYMENT_TYPE == "")

# Count the number of rows with blank spaces in the EMPLOYMENT_TYPE column
# nrow(missing_employment2)

# # Categorical Data: Impute Missing Values with mode
#
# Dynamic imputation of missing values with mode depending on the data type
getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (cols in colnames(df_nzv2)) {
  if (cols %in% names(df_nzv2[,sapply(df_nzv2, is.numeric)])) {
    df_nzv2 <- df_nzv2%>%mutate(!!cols := replace(!!rlang::sym(cols), 
                                                is.na(!!rlang::sym(cols)), 
                                                mean(!!rlang::sym(cols), 
                                                     na.rm=TRUE)))
    
  }
  else {
    
    df_nzv2<-df_nzv2%>%mutate(!!cols := replace(!!rlang::sym(cols), 
                                              !!rlang::sym(cols) == "", 
                                              getmode(!!rlang::sym(cols))))
    
  }
}

df_nzv2 
#
# Assignment of new_df data frame to df_Imp data frame
df_Imp2 <- df_nzv2

# Validating imputation of missing values for the categorical variable, 
# EMPLOYMENT_TYPE. 
# Filter the data set to show only rows with blank spaces in the 
# EMPLOYMENT_TYPE column
missing_employment2 <- df_Imp2 %>% filter(EMPLOYMENT_TYPE == "")

# Count the number of rows with blank spaces in the EMPLOYMENT_TYPE column
nrow(missing_employment2)
#
#
# Use the subset() function to select multiple rows to view based on multiple 
# values in the UNIQUEID column to confirm the imputation of missing
# values in the EMPLOYMENT_TYPE column.
#
selected_df_Imp2 <- subset(df_Imp2, UNIQUEID %in% 
                            c("52534", "637252","584433","515149","547112",
                              "497986","535877","562770","623921","635397",
                              "505026","432582","629054", "609242", "485779",
                              "438496"))
#View(selected_df_Imp2)

# Getting predictors using the DataExplorer package
# The create_report() function in DataExplorer generates a detailed report that
# includes summary statistics, histograms, density plots, and scatter plots 
# for each variable in the data set. The report also includes information on the
# number of missing values, the number of distinct values, and the data type 
# for each variable.
#
library(DataExplorer)

# generate data profiling report of descriptive statistics using create_report()
# dev.new()
# create_report(df_Imp2)

# Get numeric columns using dplyr() function            
df_Imp_nzv_num2 <- (select_if(df_Imp2, is.numeric))
#
# str(df_Imp_nzv_num2)

set.seed(123)

# A look at all the correlation between all numeric predictors:
# Run lines 411 to 431 to capture correlation plot/chart
cor_df_Imp_nzv_num2 <- cor(df_Imp_nzv_num2)

# Correlation Plot: If no stars, the variable is NOT statistically significant, 
# while one, two and three stars mean that the corresponding variable is 
# significant at 10%, 5% and 1% levels, respectively)
# Correlation Plot: If no stars, the variable is NOT statistically significant, 
# while one, two and three stars mean that the corresponding variable is 
#significant at 10%, 5% and 1% levels, respectively)

# dev.new()
# chart.Correlation( cor_df_Imp_nzv_num2, histogram = TRUE, method = "pearson")
# cor_df_Imp_nzv_num2

# Use the boxplot() function to visualize the data and identify any outliers.
#   boxplot(df_Imp_nzv_num2, main = "Boxplot of Selected Predictors")

# dev.new() 
# Boxplot to zoom in on selected outliers from the entire data frame
# par(mfrow=c(4,3))
# boxplot(df_Imp_nzv_num2$PRI_DISBURSED_AMOUNT, main = "PRI_DISBURSED_AMOUNT",
#         horizontal = TRUE )
# boxplot(df_Imp_nzv_num2$PRI_CURRENT_BALANCE,  main = "PRI_CURRENT_BALANCE",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$PRI_SANCTIONED_AMOUNT, main = "PRI_SANCTIONED_AMOUNT",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$NO_OF_INQUIRIES,  main = "NO_OF_INQUIRIES",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$PRI_OVERDUE_ACCTS,  main = "PRI_OVERDUE_ACCTS",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$PERFORM_CNS_SCORE,  main = "PERFORM_CNS_SCORE",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$DISBURSED_AMOUNT,  main = "DISBURSED_AMOUNT",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$PRI_ACTIVE_ACCTS,  main = "PRI_ACTIVE_ACCTS",
#         horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$ASSET_COST,  main = "ASSET_COST",horizontal = TRUE)
# boxplot(df_Imp_nzv_num2$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,  
#        main = "DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS",horizontal = TRUE)
#

# Histogram, Another visual aid for outlier
# To understand the distribution of the predictor variables, plot of the 
# Histograms is needed as shown below:
#
# dev.new()
# par(mfrow=c(4,3)) 
# hist(df_Imp_nzv_num2$PRI_DISBURSED_AMOUNT, main = "PRI_DISBURSED_AMOUNT")
# hist(df_Imp_nzv_num2$PRI_CURRENT_BALANCE,  main = "PRI_CURRENT_BALANCE")
# hist(df_Imp_nzv_num2$PRI_SANCTIONED_AMOUNT, main = "PRI_SANCTIONED_AMOUNT")
# hist(df_Imp_nzv_num2$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT")
# hist(df_Imp_nzv_num2$NO_OF_INQUIRIES,  main = "NO_OF_INQUIRIES")
# hist(df_Imp_nzv_num2$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT")
# hist(df_Imp_nzv_num2$PRI_OVERDUE_ACCTS,  main = "PRI_OVERDUE_ACCTS")
# hist(df_Imp_nzv_num2$PERFORM_CNS_SCORE,  main = "PERFORM_CNS_SCORE")
# hist(df_Imp_nzv_num2$DISBURSED_AMOUNT,  main = "DISBURSED_AMOUNT")
# hist(df_Imp_nzv_num2$PRI_ACTIVE_ACCTS,  main = "PRI_ACTIVE_ACCTS")
# hist(df_Imp_nzv_num2$ASSET_COST,  main = "ASSET_COST")
# hist(df_Imp_nzv_num2$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,  
#      main = "DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS")
#
# No duplicates in the data frame.

## Split the data into training (80%) and test sets (20%)
set.seed(100) #make this example reproducible

#Split data set into train and test. And use 80% of data set as training set 
# and 20% as test set

inTrain <- sample(c(TRUE, FALSE), nrow(df_Imp), replace=TRUE, prob=c(0.8,0.2))
df_ImpTrain  <- df_Imp[inTrain, ]
df_ImpTest   <- df_Imp[!inTrain, ]
# str(df_ImpTrain)
# str(df_ImpTest)

# Data Pre-processing ( Raw Data Date Set)

# Administration of a series of transformation (i.e. trans) to the data set

## Use caret's preProcess function to transform for skewness
# preProcess estimates the transformation (centering, scaling etc.) 
# function from the training data and can be applied to any data set with 
# the same variables.
df_Imp_nzv_PP2 <- preProcess(df_ImpTest, c("BoxCox", "center", "scale"))
# df_Imp_nzv_PP2

## Apply the transformations
# predict is a generic function for predictions from the results of various 
# model fitting functions. The function invokes particular methods which 
# depend on the class of the first argument.

df_Imp_nzv_PPTestTrans <- predict(df_Imp_nzv_PP2, df_ImpTest)
# str(df_Imp_nzv_PPTestTrans)

## Results for a single predictor
# df_Imp_nzv_PP2$bc$ASSET_COST   # estimated lambda = -1.3

# par(mfrow=c(1,2))
# histogram(df_ImpTest$ASSET_COST, xlab = "Natural Units", type = "count")   
# skewness(df_ImpTest$ASSET_COST)
#dev.new()
# histogram(log(df_ImpTest$ASSET_COST), xlab = "Log Units", type = "count")
# skewness(log(df_ImpTest$ASSET_COST))
# skewness(df_Imp_nzv_PPTestTrans$ASSET_COST)
#
## Results for a single predictor
# df_Imp_nzv_PP2$bc$PRI_DISBURSED_AMOUNT   
#

# par(mfrow=c(1,2))
# histogram(df_ImpTest$PRI_DISBURSED_AMOUNT, xlab = "Natural Units", 
#          type = "count")   
# skewness(df_ImpTest$PRI_DISBURSED_AMOUNT)
#dev.new()
# histogram(log(df_ImpTest$PRI_DISBURSED_AMOUNT), xlab = "Log Units", 
#           type = "count")
# skewness(log(df_ImpTest$PRI_DISBURSED_AMOUNT))
# skewness(df_Imp_nzv_PPTestTrans$PRI_DISBURSED_AMOUNT)

## Apply PCA to the entire set of predictors.

## There maybe a few predictors with only a single value, 
# so we remove these first
## (since PCA uses variances, which would be zero)
isZV <- apply(df_ImpTest, 2, function(x) length(unique(x)) == 1)
df_ImpTest <- df_ImpTest[, !isZV]

# Get numeric columns using dplyr() function            
#df_Imp_nzv_PP_num <- (select_if(df_Imp_nzv_PPTrainTrans, is.numeric))
df_Imp_nzv_PPTestTrans_num <- (select_if(df_Imp_nzv_PPTestTrans, is.numeric))

#dim(df_Imp_nzv_PPTrainTrans_num)
#str(df_Imp_nzv_PPTrainTrans_num)
#View(df_Imp_nzv_PPTrainTrans_num)

## Applying PCA 
df_Imp_nzv_PCA2 <- prcomp(df_Imp_nzv_PPTestTrans_num, center = TRUE, 
                          scale. = TRUE)

#dim(df_Imp_nzv_PPTestTrans_num)
#View(df_Imp_nzv_PPTestTrans_num)

# View the summary of the PCA results
# summary(df_Imp_nzv_PCA2)

# View the loadings (correlation coefficients) of each variable with each 
# principal component
# df_Imp_nzv_PCA2$rotation

## Plot a scatterplot matrix of the first three components
transparentTheme(pchSize = .8, trans = .3)

#
panelRange <- extendrange(df_Imp_nzv_PCA2$x[, 1:3])
splom(as.data.frame(df_Imp_nzv_PCA2$x[, 1:3]), type = c("p", "g"), 
      as.table = TRUE, auto.key = list(columns = 2), 
      prepanel.limits = function(x) panelRange)

## compute the percentage of variance for each component
percentVariancePCA2 = df_Imp_nzv_PCA2$sd^2/sum(df_Imp_nzv_PCA2$sd^2)*100

## show the transformed values  
# head(df_Imp_nzv_PCA2$x[,1:3])

# percentVariancePCA2[1:3]   # first 3 components account for 31% of variance
# plot(percentVariancePCA2, xlab="Component", 
#      ylab="Percentage of Total Variance", type="l", 
#      main="Principle Component Analysis (PCA)")

# Box plots & Histogram Pre & post-transformation

# Get numeric columns using dplyr() function            
df_Imp_nzv_num2 <- (select_if(df_Imp2, is.numeric))
# Sample
# par(mfrow=c(2,2))
# boxplot(df_Imp_nzv_num2$ASSET_COST,  main = "ASSET_COST",horizontal = TRUE)
# boxplot(df_Imp_nzv_PPTestTrans_num$ASSET_COST,  main = "ASSET_COST",
#         horizontal = TRUE)
# hist(df_Imp_nzv_num2$ASSET_COST,  main = "ASSET_COST")
# hist(df_Imp_nzv_PPTestTrans_num$ASSET_COST,  main = "ASSET_COST")

# Load required libraries
library(dplyr)
library(readr)
library(caret)
library(PerformanceAnalytics)
library(corrplot)

# Create a correlation matrix
cor_matrix2 <- cor(df_Imp_nzv_PPTestTrans_num)

# str(df_Imp_nzv_PPTrainTrans_num) 

# Identify highly correlated variables
high_cor2 <- findCorrelation(cor_matrix2, cutoff = 0.75, verbose = TRUE)

# Remove highly correlated variables
clean_vl_TESTData <- df_Imp_nzv_PPTestTrans[, -high_cor2]

# Final cleaned-up vehicle loan (VL) data set
# Get numeric columns using dplyr() function            
clean_vl_TESTData <- (select_if(clean_vl_TESTData, is.numeric))
# str(clean_vl_TESTData)

# Apply the ceiling function to every numeric variable in the data frame
##vehicle_loan_TrainData <- sapply(vehicle_loan_TrainData, function(x) 
# if(is.numeric(x)) ceiling(x) else x)
##View(vehicle_loan_TrainData)

# Predictive models that you have used. 
# Describe the tuning procedure if the model has tunable parameters.

# Present the performance of the predictive model. Include results from 
# resampling (i.e., 10-fold crossvalidation with 5 repeats) and/or a testing 
# data set (e.g., Kaggle test data). For regression problems, report RMSE and 
# R^2. Do not forget about the SDs for these two metrics from resampling 
# procedures. For classification problems, show ROC curves and other metrics 
# that you believe are important for the specific predictive exercise. 

# Discussion of the predictors that are found to be important and whether these 
# predictors agree with what a human expert would believe as important 
# (if this is possible to discuss).

# 1.  Develop a classification tree using the cleaned-up Vehicle loan data frame

library(tidyverse)
library(GGally)
library(corrplot)
#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('DMwR2')
library(DMwR2)   # Contains rt.prune
library(ISLR)    # contains Carseats data set
library(MASS)    # Contains Boston data set
library(party)

# 1.1.  Add categorical variable High to cleaned-up Vehicle loan data frame

DEFAULT=ifelse(clean_vl_TESTData$LOAN_DEFAULT >=1,"Y","N")
vl_Testdf=data.frame(clean_vl_TESTData,DEFAULT)
# str(vl_Testdf)
