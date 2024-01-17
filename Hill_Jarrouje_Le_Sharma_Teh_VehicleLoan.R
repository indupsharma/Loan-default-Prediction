#' ---
#' authors: Hill, Jarrouje, Le, Sharma & Teh 
#' ---

# OR 568 Spring 2023
# Project Title: Vehicle Loan Default Prediction
# Due Date: 05/4/2023 

# 1.0.0
# Prints the installed version of R by executing the following R code:
cat("R version=", paste(R.version$major, R.version$minor,sep = "."), "\n")

# 2.0.0
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

if( !require('ROCR') ){
  install.packages('ROCR', dependencies=TRUE, 
                   repos='http://cran.rstudio.com/')
}
library(ROCR)

# This file contains transformation of vehicle 
# loan TEST Data set (i.e. vl_Testdf). The source file is located in the 
# working directory
# source('vehicle_loan_test.R') 
# Raw Data Set & Source
#
# Data Source: Kaggle (Avik Paul)
# Vehicle Load Data Set

# 3.0.0
# Loads Vehicle Loan Data set in its original or non-formatted state
df <- read.csv(file="VehicleLoan.csv",
               header=TRUE, as.is=TRUE)

#
# 3.1.0
# Identify missing values
missing_values <- data.frame(
  variable = names(df),
  num_na = sapply(df, function(x) sum(is.na(x))),
  num_null = sapply(df, function(x) sum(is.null(x))),
  num_nan = sapply(df, function(x) sum(is.nan(x))),
  num_empty = sapply(df, function(x) sum(x == ""))
)

# Print the missing values
print(missing_values)


# # Overview of the df data frame 
# High-level Summary (Raw Data Sets)

# 3.2.0
# Display data summary and statistics
str(df) # Displays vehicle loans data set internal structure
summary(df)
head(df, 5) # Displays the first 5 rows of the data set
dim(df)
# View(df)

# Display of "DATE_OF_BIRTH" and # # "DISBURSAL_DATE" data type using class fn 
class(df$DATE_OF_BIRTH) 
class(df$DISBURSAL_DATE) 

# 3.3.0
# Format "DATE_OF_BIRTH" & "DISBURSAL_DATE" date columns using lubridate package
# Code for two or multiple date columns
# Load the lubridate package
library(lubridate)

# Assign df_no_format data frame to dates_df data frame
dates_df <- df

# Add empty columns for the formatted dates
dates_df$formatted_DATE_OF_BIRTH <- NA
dates_df$formatted_DISBURSAL_DATE <- NA

# Define the list of date formats to try
date_formats <- c("%m/%d/%Y", "%d-%m-%Y", "%Y-%m-%d", "%Y-%d-%m")

# Loop over the rows of the data frame
for (i in seq_len(nrow(dates_df))) {
  # Loop over the two date columns
  for (j in c("DATE_OF_BIRTH", "DISBURSAL_DATE")) {
    # Try to parse the date string using the defined date formats using 
    # lubridate::parse_date_time()
    parsed_date <- tryCatch(parse_date_time(dates_df[[j]][i], 
                                            orders = date_formats), 
                            error = function(e) NA)
    # If the parsing was successful, format the date object using the format()
    # function and add it to the data frame
    if (!is.na(parsed_date)) {
      formatted_date_string <- format(parsed_date, "%m/%d/%Y")
      dates_df[[paste0("formatted_", j)]][i] <- formatted_date_string
    }
  }
}
# Write the updated data frame to a new CSV file
# write.csv(dates_df, "dates_formatted_VehicleLoan.csv", row.names = FALSE)

str(dates_df)
#
# 3.4.0
# Convert the character columns to date columns using ymd() or mdy() or dmy() 
# depending on the format for both formatted_DATE_OF_BIRTH 
# & formatted_DISBURSAL_DATE

# Convert the character columns to date columns using ymd() or mdy() or dmy() 
# depending on the format
# month-day-year format
dates_df$formatted_DATE_OF_BIRTH <- mdy(dates_df$formatted_DATE_OF_BIRTH) 
# month-day-year format
dates_df$formatted_DISBURSAL_DATE <- mdy(dates_df$formatted_DISBURSAL_DATE) 

# Displays internal structure of the data frame with formatted date columns 
str(dates_df)
#
# 3.5.0
# Format and add a new AGE column to the data frame 
# Read the new formatted Vehicle Loan CSV file into a data frame

# Assign dates_df data frame to age_df data frame
age_df <- dates_df

# Load the lubridate package
# library(lubridate)

# Convert the "formatted_DATE_OF_BIRTH" column to a date object using the 
# ymd() function
age_df$formatted_DATE_OF_BIRTH <- ymd(age_df$formatted_DATE_OF_BIRTH)

# Calculate the current year
current_year <- year(Sys.Date())

# Create a new column "Age" containing the age in years
age_df$AGE <- current_year - year(age_df$formatted_DATE_OF_BIRTH)

str(age_df)
# View(age_df)

# 3.6.0 
# Remove/exclude both "DATE_OF_BIRTH", "DISBURSAL_DATE" columns to avoid 
# duplication of columns and/or data points

# Exclude columns 'DATE_OF_BIRTH' and 'DISBURSAL_DATE'
age_df <- age_df[, -c(9, 11)]

# Display data summary and statistics
str(age_df) # Displays vehicle loans data set internal structure
summary(age_df)
#
 
# 4.0.0
# Here, the code ONLY identify the Near-Zero Variance Predictors and provides
# useful metrics
nzv_vals <- nearZeroVar(age_df, saveMetrics = TRUE)
dim(nzv_vals)
## [1] 42  4
nzv_vals
# View(nzv_vals)

# Sort descending order and by putting NA top (If available)
nzv_vals <- nzv_vals[order(nzv_vals$nzv, nzv_vals$zeroVar),]
dim(nzv_vals)
nzv_vals

# Here, the code actually removes near-zero variance predictors
df_nzv <- age_df[, -nearZeroVar(age_df)] # remove near-zero variance predictors
dim(df_nzv)
# [1] 233154     30
# View(df_nzv)

str(df_nzv) # Displays new_df_nzv_VehicleLoan.csv data set internal structure
# View(df_nzv)
 
# 5.0.0
# Check for missing values in numeric data type
sum(is.na(df_nzv))

# Check for missing values specifically for EMPLOYMENT_TYPE column
# Filter the data set to show only rows with blank spaces in the 
# EMPLOYMENT_TYPE column
missing_employment <- df %>% filter(EMPLOYMENT_TYPE == "")
#
# Count the number of rows with blank spaces in the EMPLOYMENT_TYPE column
nrow(missing_employment)

# 5.1.0
# # Categorical Data: Impute Missing Values with mode
#
# Dynamic imputation of missing values with mode depending on the data type
getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (cols in colnames(df_nzv)) {
  if (cols %in% names(df_nzv[,sapply(df_nzv, is.numeric)])) {
    df_nzv <- df_nzv%>%mutate(!!cols := replace(!!rlang::sym(cols), 
                                                is.na(!!rlang::sym(cols)), 
                                                mean(!!rlang::sym(cols), 
                                                     na.rm=TRUE)))
    
  }
  else {
    
    df_nzv<-df_nzv%>%mutate(!!cols := replace(!!rlang::sym(cols), 
                                              !!rlang::sym(cols) == "", 
                                              getmode(!!rlang::sym(cols))))
    
  }
}

df_nzv 
#
# Assign new_df data frame to df_Imp data frame
df_Imp <- df_nzv

# 5.2.0
# Validating imputation of missing values for the categorical variable, 
# EMPLOYMENT_TYPE. 
# Filter the data set to show only rows with blank spaces in the 
# EMPLOYMENT_TYPE column
missing_employment <- df_Imp %>% filter(EMPLOYMENT_TYPE == "")

# Count the number of rows with blank spaces in the EMPLOYMENT_TYPE column
nrow(missing_employment)
#
#

# Use the subset() function to select multiple rows to view based on multiple 
# values in the UNIQUEID column to confirm the imputation of missing
# values in the EMPLOYMENT_TYPE column.
#
selected_df_Imp <- subset(df_Imp, UNIQUEID %in% 
                            c("52534", "637252","584433","515149","547112",
                              "497986","535877","562770","623921","635397",
                              "505026","432582","629054", "609242", "485779",
                              "438496"))
# View(selected_df_Imp)


# 5.3.0
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
create_report(df_Imp)

# 5.4.0
## A look at all the correlation between all numeric predictors:
# Get numeric columns using dplyr() function            
df_Imp_nzv_num <- (select_if(df_Imp, is.numeric))
#
# str(df_Imp_nzv_num)

set.seed(123)

# A look at all the correlation between all numeric predictors:
# Run lines 411 to 431 to capture correlation plot/chart
cor_df_Imp_nzv_num <- cor(df_Imp_nzv_num)

# Correlation Plot: If no stars, the variable is NOT statistically significant, 
# while one, two and three stars mean that the corresponding variable is 
# significant at 10%, 5% and 1% levels, respectively)
# Correlation Plot: If no stars, the variable is NOT statistically significant, 
# while one, two and three stars mean that the corresponding variable is 
#significant at 10%, 5% and 1% levels, respectively)
#
# dev.new()
chart.Correlation( cor_df_Imp_nzv_num, histogram = TRUE, method = "pearson")
cor_df_Imp_nzv_num

 
# 6.0.0
# Use the boxplot() function to visualize the data and identify any outliers.
boxplot(df_Imp_nzv_num, main = "Boxplot of Selected Predictors")


# 6.1.0
# dev.new() 
# Boxplot to zoom in on selected outliers from the entire data frame
par(mfrow=c(4,3))
boxplot(df_Imp_nzv_num$PRI_DISBURSED_AMOUNT, main = "PRI_DISBURSED_AMOUNT",
        horizontal = TRUE )
boxplot(df_Imp_nzv_num$PRI_CURRENT_BALANCE,  main = "PRI_CURRENT_BALANCE",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$PRI_SANCTIONED_AMOUNT, main = "PRI_SANCTIONED_AMOUNT",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$NO_OF_INQUIRIES,  main = "NO_OF_INQUIRIES",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$PRI_OVERDUE_ACCTS,  main = "PRI_OVERDUE_ACCTS",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$PERFORM_CNS_SCORE,  main = "PERFORM_CNS_SCORE",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$DISBURSED_AMOUNT,  main = "DISBURSED_AMOUNT",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$PRI_ACTIVE_ACCTS,  main = "PRI_ACTIVE_ACCTS",
        horizontal = TRUE)
boxplot(df_Imp_nzv_num$ASSET_COST,  main = "ASSET_COST",horizontal = TRUE)
boxplot(df_Imp_nzv_num$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,  
        main = "DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS",horizontal = TRUE)
#
 
# 6.2.0
# Histogram, Another visual aid for outlier
# To understand the distribution of the predictor variables, plot of the 
# Histograms is needed as shown below:
#
# dev.new()
par(mfrow=c(4,3)) 
hist(df_Imp_nzv_num$PRI_DISBURSED_AMOUNT, main = "PRI_DISBURSED_AMOUNT")
hist(df_Imp_nzv_num$PRI_CURRENT_BALANCE,  main = "PRI_CURRENT_BALANCE")
hist(df_Imp_nzv_num$PRI_SANCTIONED_AMOUNT, main = "PRI_SANCTIONED_AMOUNT")
hist(df_Imp_nzv_num$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT")
hist(df_Imp_nzv_num$NO_OF_INQUIRIES,  main = "NO_OF_INQUIRIES")
hist(df_Imp_nzv_num$PRIMARY_INSTAL_AMT,  main = "PRIMARY_INSTAL_AMT")
hist(df_Imp_nzv_num$PRI_OVERDUE_ACCTS,  main = "PRI_OVERDUE_ACCTS")
hist(df_Imp_nzv_num$PERFORM_CNS_SCORE,  main = "PERFORM_CNS_SCORE")
hist(df_Imp_nzv_num$DISBURSED_AMOUNT,  main = "DISBURSED_AMOUNT")
hist(df_Imp_nzv_num$PRI_ACTIVE_ACCTS,  main = "PRI_ACTIVE_ACCTS")
hist(df_Imp_nzv_num$ASSET_COST,  main = "ASSET_COST")
hist(df_Imp_nzv_num$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS,  
     main = "DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS")


# 7.0.0
## Split the data into training (80%) and test sets (20%)
#Split data set into train and test. And use 80% of data set as training set 
# and 20% as test set

set.seed(100) # make the values reproducible

inTrain <- sample(c(TRUE, FALSE), nrow(df_Imp), replace=TRUE, prob=c(0.8,0.2))
df_ImpTrain  <- df_Imp[inTrain, ]
df_ImpTest   <- df_Imp[!inTrain, ]
# str(df_ImpTrain)
# str(df_ImpTest)

# 7.1.0
# Data Pre-processing ( Raw Data Date Set)
# Administration of a series of transformation (i.e. trans) to the data set

## Use caret's preProcess function to transform for skewness
# preProcess estimates the transformation (centering, scaling etc.) 
# function from the training data and can be applied to any data set with 
# the same variables.
df_Imp_nzv_PP <- preProcess(df_ImpTrain, c("BoxCox", "center", "scale"))
df_Imp_nzv_PP
#str(df_Imp_nzv_PP)


# 7.2.0
## Apply the transformations
# predict is a generic function for predictions from the results of various 
# model fitting functions. The function invokes particular methods which 
# depend on the class of the first argument.

df_Imp_nzv_PPTrainTrans <- predict(df_Imp_nzv_PP, df_ImpTrain)
# str(df_Imp_nzv_PPTrainTrans)

## Results for a single predictor
df_Imp_nzv_PP$bc$ASSET_COST   # estimated lambda = -1.3

par(mfrow=c(1,2))
histogram(df_ImpTrain$ASSET_COST, xlab = "Natural Units", type = "count")   
skewness(df_ImpTrain$ASSET_COST)
#dev.new()
histogram(log(df_ImpTrain$ASSET_COST), xlab = "Log Units", type = "count")
skewness(log(df_ImpTrain$ASSET_COST))
skewness(df_Imp_nzv_PPTrainTrans$ASSET_COST)
#
## Results for a single predictor
# df_Imp_nzv_PP$bc$PRI_DISBURSED_AMOUNT   
#

# par(mfrow=c(1,2))
# histogram(df_ImpTrain$PRI_DISBURSED_AMOUNT, xlab = "Natural Units", 
#           type = "count")   
# skewness(df_ImpTrain$PRI_DISBURSED_AMOUNT)
#dev.new()
# histogram(log(df_ImpTrain$PRI_DISBURSED_AMOUNT), xlab = "Log Units", 
#          type = "count")
# skewness(log(df_ImpTrain$PRI_DISBURSED_AMOUNT))
# skewness(df_Imp_nzv_PPTrainTrans$PRI_DISBURSED_AMOUNT)

# 7.3.0
## Apply PCA to the entire set of predictors.

## There maybe a few predictors with only a single value, so we 
# remove these first
## (since PCA uses variances, which would be zero)
isZV <- apply(df_ImpTrain, 2, function(x) length(unique(x)) == 1)
df_ImpTrain <- df_ImpTrain[, !isZV]

# Get numeric columns using dplyr() function            
#df_Imp_nzv_PP_num <- (select_if(df_Imp_nzv_PPTrainTrans, is.numeric))
df_Imp_nzv_PPTrainTrans_num <- (select_if(df_Imp_nzv_PPTrainTrans, is.numeric))

dim(df_Imp_nzv_PPTrainTrans_num)
# str(df_Imp_nzv_PPTrainTrans_num)
# View(df_Imp_nzv_PPTrainTrans_num)

## Applying PCA 
df_Imp_nzv_PCA <- prcomp(df_Imp_nzv_PPTrainTrans_num, center = TRUE, 
                         scale. = TRUE)

#dim(df_Imp_nzv_PPTrainTrans_num)
#View(df_Imp_nzv_PPTrainTrans_num)

# View the summary of the PCA results
summary(df_Imp_nzv_PCA)

# View the loadings (correlation coefficients) of each variable with each 
# principal component
df_Imp_nzv_PCA$rotation
 
# 7.4.0
## Plot a scatterplot matrix of the first three components
transparentTheme(pchSize = .8, trans = .3) 

#
panelRange <- extendrange(df_Imp_nzv_PCA$x[, 1:3])
splom(as.data.frame(df_Imp_nzv_PCA$x[, 1:3]), type = c("p", "g"), 
      as.table = TRUE, auto.key = list(columns = 2), 
      prepanel.limits = function(x) panelRange)

## compute the percentage of variance for each component
percentVariancePCA = df_Imp_nzv_PCA$sd^2/sum(df_Imp_nzv_PCA$sd^2)*100

## show the transformed values  
head(df_Imp_nzv_PCA$x[,1:3])

percentVariancePCA[1:3]   # first 3 components account for 31% of variance
plot(percentVariancePCA, xlab="Component", ylab="Percentage of Total Variance", 
     type="l", 
     main="Principle Component Analysis (PCA)")

# 7.5.0
# Box plots & Histogram Pre & post-transformation

# Get numeric columns using dplyr() function            
df_Imp_nzv_num <- (select_if(df_Imp, is.numeric))

# View(df_Imp_nzv_num)
# dim(df_Imp_nzv_num)
# str(df_Imp_nzv_num)
# Sample
par(mfrow=c(2,2))
boxplot(df_Imp_nzv_num$ASSET_COST,  main = "ASSET_COST",horizontal = TRUE)
boxplot(df_Imp_nzv_PPTrainTrans_num$ASSET_COST,  main = "ASSET_COST",
        horizontal = TRUE)
hist(df_Imp_nzv_num$ASSET_COST,  main = "ASSET_COST") 
hist(df_Imp_nzv_PPTrainTrans_num$ASSET_COST,  main = "ASSET_COST")


# 7.6.0
# Identify highly correlated variables and remove them

# Load required libraries
library(dplyr)
library(readr)
library(caret)
library(PerformanceAnalytics)
library(corrplot)

# Create a correlation matrix
cor_matrix <- cor(df_Imp_nzv_PPTrainTrans_num)
#View(df_Imp_nzv_PPTrainTrans_num)

# Identify highly correlated variables
high_cor <- findCorrelation(cor_matrix, cutoff = 0.75, verbose = TRUE)

# Remove highly correlated variables
clean_vl_data <- df_Imp_nzv_PPTrainTrans[, -high_cor]
 
# 7.7.0
# Final cleaned-up vehicle loan (VL) data set
# Get numeric columns using dplyr() function            
vehicle_loan_TrainData <- (select_if(clean_vl_data, is.numeric))
# str(vehicle_loan_TrainData)

# Apply the ceiling function to every numeric variable in the data frame
##vehicle_loan_TrainData <- sapply(vehicle_loan_TrainData, 
# function(x) if(is.numeric(x)) ceiling(x) else x)
##View(vehicle_loan_TrainData)


# 8.0.0
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
 
# 9.0.0
# CART/Tree based Model

# 9.1  Develop a classification tree using the cleaned-up Vehicle loan d.frame
# Load selected libraries
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
# Load packages
library(ROCR)
library(readr)
library(pROC)

# 9.2.0
# This file contains transformation of vehicle 
# loan TEST Data set (i.e. vl_Testdf). The source file is located in the 
# working directory
source('vehicle_loan_test.R') 


# 9.3.0
# Exclude column "PRI_OVERDUE_ACCTS",
 vehicle_loan_TrainData <- subset(vehicle_loan_TrainData, 
                                  select = -PRI_OVERDUE_ACCTS)
# str(vehicle_loan_TrainData)


# 9.4.0
# Add categorical variable High to cleaned-up Vehicle loan data frame
# The ifelse() function in R is used to create a new vector based on a condition
# that is evaluated on an existing vector. Here we added a new categorical 
# variable (i.e. DEFUALT) with "Y" and "N".
DEFAULT=ifelse(vehicle_loan_TrainData$LOAN_DEFAULT >=1,"Y","N")
vl_Traindf=data.frame(vehicle_loan_TrainData,DEFAULT)
# str(vl_Traindf)

# 9.5
# Fitting a classification trees using rpart.

# Note: cp() is a function in the rpart package in R that calculates the 
# complexity parameter (cp) values for a decision tree model built using the 
# rpart() function.The cp parameter controls the complexity of the decision tree
# by specifying the minimum decrease in the cross-validation error needed to 
# split a node. A larger cp value will result in a smaller tree with fewer 
# splits, while a smaller cp value will result in a larger tree with more splits.

# We use the cp() function to calculate the complexity parameter values for the 
# model. The resulting output shows a table of cp values and the corresponding 
# number of nodes and cross-validation error rates for each value.
# The cp() function is useful for exploring the range of cp values and selecting
# an appropriate value for pruning the decision tree. The plotcp() function 
# can also be used to visualize the cp values and their corresponding 
# cross-validation error rates.

# Initially, use a very small value of cp.
# A larger cp value will result in a smaller tree with less splits.
# A smaller cp value will result in a larger tree with more splits.
SEED <- 1458
set.seed(SEED)
rpart.vl_Traindf = rpart(DEFAULT~.-LOAN_DEFAULT ,vl_Traindf, cp=0.0000000000001)

# 9.6.0
# Use cross-validation to prune the tree
# dev.new()
# The plotcp() function in R is used to plot the complexity parameter (cp) 
# values and their corresponding cross-validation error rates for a decision 
# tree model built using the rpart() function in the rpart package.

# The plotcp() function is useful for selecting the appropriate value of cp 
# to prune the tree and avoid overfitting. It can also help in selecting the 
# optimal number of nodes for the tree.
par(mfrow=c(2,2))
# Display cross-validation-error plot
plotcp(rpart.vl_Traindf)
printcp(rpart.vl_Traindf)

# 9.7.0
# The cross-validation results indicates that a tree of size 30 has
# the smallest cross validated relative error and that to prune to this can use 
# a cp=0.0002. A larger cp value will result in a smaller tree with 
# less splits.# A smaller cp value (example: cp=0.0000000000001) will result in
# a larger tree with more splits.

rpart.vl_Traindf.min <- prune(rpart.vl_Traindf, cp=0.0002)
printcp(rpart.vl_Traindf.min)

# Importance of each predictor variable in the decision tree
summary(rpart.vl_Traindf.min)

## i) CART Tree Model 
# Display in the plot window the pruned tree (that is, the
# minimum-error tree).
# dev.new()
rpart.plot(rpart.vl_Traindf.min, extra=2,
           main="Vehicle Loan min-error classification tree")

## ii) CART Tree Model (The use of prp() function)
# Display in the plot window the pruned tree (that is, the
# minimum-error tree).
prp(rpart.vl_Traindf.min, type =2, extra=2,
    main="Vehicle Loan min-error classification tree")

# 9.8.0
# train the model using the train function
set.seed(123)
model <- train(DEFAULT~.-LOAN_DEFAULT ,vl_Traindf, method = "rpart", 
             tuneLength = 10)

# 9.9.0
# Make predictions on the test set
predicted <- factor(predict(model, vl_Testdf))
real <- factor(vl_Testdf$DEFAULT)
my_data1 <- data.frame(data = predicted, type = "prediction")
my_data2 <- data.frame(data = real, type = "real")
my_data3 <- rbind(my_data1,my_data2)

# Note: The factor() function is used to create categorical variables, which are 
# variables that have a fixed set of possible values, often referred to as 
# levels. When you create a factor in R, you specify the levels of the factor 
# as a vector of strings or numbers. R then assigns each observation in the 
# dataset to a level of the factor based on its value.

# 9.9.1
# Check if the levels are identical
identical(levels(my_data3[my_data3$type == "prediction",1]) , 
          levels(my_data3[my_data3$type == "real",1]))

# 9.9.2
# Evaluate the model's accuracy using a confusion matrix
conf_mat <- confusionMatrix(my_data3[my_data3$type == "prediction",1], 
                            my_data3[my_data3$type == "real",1],  
                            dnn = c("Prediction", "Reference"))
conf_mat

##
# Get the model's performance statistics
perf <- list(
  Accuracy = conf_mat$overall['Accuracy'],
  Precision = conf_mat$byClass['Precision'],
  Recall = conf_mat$byClass['Recall'],
  F1 = conf_mat$byClass['F1']
)

# Print the performance statistics
print(perf)

# 9.9.3
# Evaluate model's performance using ROC curve and AUC 
# (Area Under the ROC Curve) 
# Load packages
library(rpart)
library(ROCR)
library(readr)
library(pROC)

# Predict class probabilities on test data
pred_prob <- predict(model, newdata = vl_Testdf, type = "prob")

# Check format of predicted probabilities
class(pred_prob) # Should be a matrix
dim(pred_prob) # Should be n_test x 2

# Create ROC curve object
roc_obj <- prediction(pred_prob[, 2], vl_Testdf$DEFAULT)

# Calculate AUC
auc_val <- performance(roc_obj, "auc")@y.values[[1]]
auc_val
# [1] 0.5

# Plot ROC curve
plot(performance(roc_obj, "tpr", "fpr"), main = "ROC Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate", col = "blue")
lines(c(0, 1), c(0, 1), lty = 2, col = "gray")
legend("bottomright", legend = paste("AUC = ", round(auc_val, 3)), 
       col = "black", lty = 1, cex = 0.8)

# 10.0.0
## Logistic Regression
df_Imp_Model1 <- clean_vl_data

df_Imp_Model1 <- df_Imp_Model1 %>% relocate(LOAN_DEFAULT)

# train logistic regression model
modelLog <- glm(LOAN_DEFAULT ~ ., data = df_ImpTrain, family = "binomial")
#
# 10.1.0
# predict on testing data
predictionsLog <- predict(modelLog, df_ImpTest, type = "response")
predicted_classesLog <- ifelse(predictionsLog > 0.5, 1, 0)
#
# calculate accuracy
accuracyLog <- mean(predicted_classesLog == df_ImpTest$LOAN_DEFAULT)
#
# calculate precision and recall
confusion_matrixLog <- confusionMatrix(factor(predicted_classesLog), 
                                       factor(df_ImpTest$LOAN_DEFAULT))
precisionLog <- confusion_matrixLog$byClass["Pos Pred Value"]
recallLog <- confusion_matrixLog$byClass["Sensitivity"]
#
# print confusion matrix
print(confusion_matrixLog$table)
#
# calculate RMSE and R-squared
rmseLog <- sqrt(mean((predictionsLog - df_ImpTest$LOAN_DEFAULT)^2))
rsquaredLog <- cor(predictionsLog, df_ImpTest$LOAN_DEFAULT)^2

# print results
print(paste0("Accuracy: ", round(accuracyLog, 4)))
print(paste0("Precision: ", round(precisionLog, 4)))
print(paste0("Recall: ", round(recallLog, 4)))
print(paste0("RMSE: ", round(rmseLog, 4)))
print(paste0("R-squared: ", round(rsquaredLog, 4)))

# 11.0.0
# Random Forest
library(randomForest)

modelrf <- randomForest(LOAN_DEFAULT ~ ., data = df_ImpTrain, ntree = 5, 
                        importance = TRUE)
# 11.1.0
# predict on testing data
predictionsrf <- predict(modelrf, df_ImpTest,type="response")
predicted_classesrf <- ifelse(predictionsrf > 0.5, 1, 0)
#
# calculate accuracy
accuracyrf <- mean(predicted_classesrf == df_ImpTest$LOAN_DEFAULT)
#
# calculate precision and recall
confusion_matrixrf <- confusionMatrix(factor(predicted_classesrf), 
                                      factor(df_ImpTest$LOAN_DEFAULT))
precisionrf <- confusion_matrixrf$byClass["Pos Pred Value"]
recallrf <- confusion_matrixrf$byClass["Sensitivity"]
#
# print confusion matrix
print(confusion_matrixrf$table)
#
# calculate RMSE and R-squared
rmserf <- sqrt(mean((predicted_classesrf - df_ImpTest$LOAN_DEFAULT)^2))
rsquaredrf <- cor(predicted_classesrf, df_ImpTest$LOAN_DEFAULT)^2

# print evaluation metrics
cat("Accuracy: ", round(accuracyrf, 2), "\n")
cat("Precision: ", round(precisionrf, 2), "\n")
cat("Recall: ", round(recallrf, 2), "\n")
cat("RMSE: ", round(rmserf, 2), "\n")
cat("R-squared: ", round(rsquaredrf, 2), "\n")