setwd("C:/Users/suvid/Desktop/ECON F215 Assignment 1/")

library(foreign)  # For reading .dta files
library(dplyr)  # For data manipulation

# Load Expenditure data
expenditure_data <- read.dta("NSSO Dataset/NSSO_68/Household characteristics - Block 3 - Level 3.dta")

# Load Occupation data
land_data <- read.dta("NSSO Dataset/NSSO_68/Household Characteristics - Block 3 -  Level 2 -  68.dta")

# Load Education data
clothing_bedding_data <- read.dta("NSSO Dataset/NSSO_68/Consumption of clothing, bedding and footwear during last 30 and 365 days - Block 7 and 8  - Level 6 -  68.dta")

# Load Family Size data
education_medical_data <- read.dta("NSSO Dataset/NSSO_68/Expenditure on Education and Medical (institutional) goods and services -  Block 9 - Level 7 -  68.dta")

# Merge datasets on common Household ID
merged_data <- full_join(expenditure_data, full_join(land_data, full_join(clothing_bedding_data, education_medical_data, by = "HHID"), by = "HHID"), by = "HHID")

# Inspect merged data
head(merged_data)

# Select relevant variables
# MPCE_MRP -> Expenditure
# Land_owned -> Land held in acres
# Last_30days_Value -> Consumption of clothing, bedding etc. during last 30 days value (Rs.)
# Expenditure_in_Rs_last_30_days -> Expenditure on education and medical goods and services during last 30 days

final_data <- merged_data %>%
  dplyr::select(MPCE_MRP, Land_owned, Last_30days_Value, Expenditure_in_Rs_last_30_days) %>%
  na.omit()  # Remove missing values

# Run Linear Regression
model <- lm(MPCE_MRP ~ Land_owned + Last_30days_Value + Expenditure_in_Rs_last_30_days, data = final_data)

# Display Results
summary(model)


# Visualizing Data

# Set up plotting layout (2 rows, 2 columns)
par(mfrow = c(2, 2))

# Scatter plot: MPCE_MRP vs. Land_owned
plot(final_data$Land_owned, final_data$MPCE_MRP, 
     main = "Land Owned vs. MPCE_MRP", 
     xlab = "Land Owned (acres)", 
     ylab = "MPCE_MRP", 
     pch = 16, col = "blue")
abline(lm(MPCE_MRP ~ Land_owned, data = final_data), col = "red")

# Scatter plot: MPCE_MRP vs. Last_30days_Value
plot(final_data$Last_30days_Value, final_data$MPCE_MRP, 
     main = "Last 30 Days Expenditure vs. MPCE_MRP", 
     xlab = "Last 30 Days Value (Rs.)", 
     ylab = "MPCE_MRP", 
     pch = 16, col = "blue")
abline(lm(MPCE_MRP ~ Last_30days_Value, data = final_data), col = "red")

# Scatter plot: MPCE_MRP vs. Expenditure_in_Rs_last_30_days
plot(final_data$Expenditure_in_Rs_last_30_days, final_data$MPCE_MRP, 
     main = "Education & Medical Exp. vs. MPCE_MRP", 
     xlab = "Education & Medical Exp. (Rs.)", 
     ylab = "MPCE_MRP", 
     pch = 16, col = "blue")
abline(lm(MPCE_MRP ~ Expenditure_in_Rs_last_30_days, data = final_data), col = "red")



# Adjusted R^2 was very less so took log transformation
# It improved adjusted R^2 to 0.16!

# Fit the log-linear model
model <- lm(log(MPCE_MRP) ~ log(Land_owned + 1) + log(Last_30days_Value + 1) + log(Expenditure_in_Rs_last_30_days + 1), data = final_data)
summary(model)

# Visualizing Data (Log-Transformed)

# Set up plotting layout (2 rows, 2 columns)
par(mfrow = c(2, 2))

# Scatter plot: log(MPCE_MRP) vs. log(Land_owned + 1)
plot(log(final_data$Land_owned + 1), log(final_data$MPCE_MRP), 
     main = "Log(Land Owned) vs. Log(MPCE_MRP)", 
     xlab = "Log(Land Owned + 1)", 
     ylab = "Log(MPCE_MRP)", 
     pch = 16, col = "blue")
abline(lm(log(MPCE_MRP) ~ log(Land_owned + 1), data = final_data), col = "red")

# Scatter plot: log(MPCE_MRP) vs. log(Last_30days_Value + 1)
plot(log(final_data$Last_30days_Value + 1), log(final_data$MPCE_MRP), 
     main = "Log(Last 30 Days Expenditure) vs. Log(MPCE_MRP)", 
     xlab = "Log(Last 30 Days Value + 1)", 
     ylab = "Log(MPCE_MRP)", 
     pch = 16, col = "blue")
abline(lm(log(MPCE_MRP) ~ log(Last_30days_Value + 1), data = final_data), col = "red")

# Scatter plot: log(MPCE_MRP) vs. log(Expenditure_in_Rs_last_30_days + 1)
plot(log(final_data$Expenditure_in_Rs_last_30_days + 1), log(final_data$MPCE_MRP), 
     main = "Log(Education & Medical Exp.) vs. Log(MPCE_MRP)", 
     xlab = "Log(Education & Medical Exp. + 1)", 
     ylab = "Log(MPCE_MRP)", 
     pch = 16, col = "blue")
abline(lm(log(MPCE_MRP) ~ log(Expenditure_in_Rs_last_30_days + 1), data = final_data), col = "red")
