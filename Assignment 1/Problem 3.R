setwd("D:/Personal/College Stuff/2nd Year/ECON F215 Assignment 1/")

library(dplyr)
library(foreign)
library(MASS)  # For LDA & QDA
library(pROC)  # For ROC curves
library(caTools)  # For train-test split

# Load Household Data
household_data <- read.dta("NSSO Dataset/NSSO_68/Household characteristics - Block 3 - Level 3.dta")

# Select relevant columns & remove missing values
data <- household_data %>%
  dplyr::select(Cooking_Code, Lighting_Code, MPCE_MRP) %>%
  na.omit()

# Convert categorical variables to factors
data$Lighting_Code <- as.factor(data$Lighting_Code)
data$Cooking_Code <- as.factor(data$Cooking_Code)

# Remove problematic category from Cooking_Code
data <- data %>% filter(Cooking_Code != "08")
data$Cooking_Code <- droplevels(data$Cooking_Code)

# Remove empty category from Cooking_Code and Lighting_Code
data <- data %>% filter(Cooking_Code != "" & !is.na(Cooking_Code))
data <- data %>% filter(Lighting_Code != "" & !is.na(Lighting_Code))
data$Cooking_Code <- droplevels(data$Cooking_Code)
data$Lighting_Code <- droplevels(data$Lighting_Code)

# Merge small categories to Others in Lighting_Code and Cooking_Code
data$Lighting_Code <- as.character(data$Lighting_Code)
data$Lighting_Code[data$Lighting_Code %in% c("2", "3", "6", "9", "4", "5")] <- "Others"
data$Lighting_Code <- as.factor(data$Lighting_Code)  
data$Lighting_Code <- as.character(data$Lighting_Code)
data$Cooking_Code <- as.character(data$Cooking_Code)
data$Cooking_Code[data$Cooking_Code %in% c("01", "04", "05", "06", "07")] <- "Other"
data$Cooking_Code <- as.factor(data$Cooking_Code)


# Ensure Cooking_Code has more than 2 categories
print(table(data$Cooking_Code)) 

# Split data: 90% training, 10% testing
set.seed(123)
split <- sample.split(data$Cooking_Code, SplitRatio = 0.9)
train_data <- subset(data, split == TRUE)
test_data  <- subset(data, split == FALSE)

# Train LDA Model
lda_model <- lda(Cooking_Code ~ Lighting_Code + MPCE_MRP, data = train_data)

# Train QDA Model
qda_model <- qda(Cooking_Code ~ Lighting_Code + MPCE_MRP, data = train_data)

# Predict probabilities for test data
lda_probs <- predict(lda_model, test_data)$posterior
qda_probs <- predict(qda_model, test_data)$posterior

# Function for threshold-based classification with NA class
classify_with_na <- function(probs, threshold) {
  max_probs <- apply(probs, 1, max)  # Get max probability per row
  pred_class <- apply(probs, 1, function(row) which.max(row))  # Assign highest probability class
  
  pred_class[max_probs < threshold] <- "NA"  # Assign NA when no class exceeds threshold
  return(factor(pred_class, levels = c(levels(test_data$Cooking_Code), "NA")))
}


# Vary classification threshold from 0.5 to 1
thresholds <- seq(0.5, 1, by = 0.1)
results <- data.frame(Threshold = numeric(), LDA_FPR = numeric(), LDA_FNR = numeric(), LDA_Error = numeric(), QDA_FPR = numeric(), QDA_FNR = numeric(), QDA_Error = numeric())

for (t in thresholds) {
  lda_pred <- classify_with_na(lda_probs, t)
  qda_pred <- classify_with_na(qda_probs, t)
  
  # Convert actual classes to numeric
  actual <- as.numeric(as.character(test_data$Cooking_Code))
  
  # Compute confusion matrices with NA class
  lda_conf_matrix <- table(Predicted = factor(lda_pred, levels = c(levels(test_data$Cooking_Code), "NA")),
                           Actual = factor(actual, levels = c(levels(test_data$Cooking_Code), "NA")),
                           useNA = "ifany")
  
  qda_conf_matrix <- table(Predicted = factor(qda_pred, levels = c(levels(test_data$Cooking_Code), "NA")),
                           Actual = factor(actual, levels = c(levels(test_data$Cooking_Code), "NA")),
                           useNA = "ifany")
  
  # Fix NaN for FNR and FPR when there are no predictions above the threshold
  fnr_lda <- ifelse(sum(lda_conf_matrix[-nrow(lda_conf_matrix), ncol(lda_conf_matrix)], na.rm = TRUE) == 0,
                    0, 
                    sum(lda_conf_matrix[-nrow(lda_conf_matrix), ncol(lda_conf_matrix)], na.rm = TRUE) / sum(lda_conf_matrix[-nrow(lda_conf_matrix), ], na.rm = TRUE))
  
  fnr_qda <- ifelse(sum(qda_conf_matrix[-nrow(qda_conf_matrix), ncol(qda_conf_matrix)], na.rm = TRUE) == 0,
                    0, 
                    sum(qda_conf_matrix[-nrow(qda_conf_matrix), ncol(qda_conf_matrix)], na.rm = TRUE) / sum(qda_conf_matrix[-nrow(qda_conf_matrix), ], na.rm = TRUE))
  
  # Handle FPR similarly
  fpr_lda <- ifelse(sum(lda_conf_matrix["NA", ], na.rm = TRUE) == 0,
                    0, 
                    sum(lda_conf_matrix["NA", ], na.rm = TRUE) / sum(lda_conf_matrix, na.rm = TRUE))
  
  fpr_qda <- ifelse(sum(qda_conf_matrix["NA", ], na.rm = TRUE) == 0,
                    0, 
                    sum(qda_conf_matrix["NA", ], na.rm = TRUE) / sum(qda_conf_matrix, na.rm = TRUE))
  print(lda_conf_matrix)
  print(qda_conf_matrix)
  # Compute error rate
  error_lda <- 1 - sum(diag(lda_conf_matrix), na.rm = TRUE) / sum(lda_conf_matrix, na.rm = TRUE)
  error_qda <- 1 - sum(diag(qda_conf_matrix), na.rm = TRUE) / sum(qda_conf_matrix, na.rm = TRUE)
  
  # Store results
  results <- rbind(results, data.frame(
    Threshold = t,
    LDA_FPR = fpr_lda,
    LDA_FNR = fnr_lda,
    LDA_Error = error_lda,
    QDA_FPR = fpr_qda,
    QDA_FNR = fnr_qda,
    QDA_Error = error_qda
  ))
}

# Print results table
print(results)

# Compute Training Error Rate
lda_train_preds <- predict(lda_model, train_data)$class
qda_train_preds <- predict(qda_model, train_data)$class

lda_train_error <- mean(lda_train_preds != train_data$Cooking_Code, na.rm = TRUE)
qda_train_error <- mean(qda_train_preds != train_data$Cooking_Code, na.rm = TRUE)

# Compute Testing Error Rate
lda_test_preds <- predict(lda_model, test_data)$class
qda_test_preds <- predict(qda_model, test_data)$class

lda_test_error <- mean(lda_test_preds != test_data$Cooking_Code, na.rm = TRUE)
qda_test_error <- mean(qda_test_preds != test_data$Cooking_Code, na.rm = TRUE)

print(paste("LDA Testing Error:", round(lda_test_error, 3)))
print(paste("QDA Testing Error:", round(qda_test_error, 3)))


# Data Visualization

# Define thresholds
thresholds <- seq(0.5, 1, by = 0.05)

# Initialize vectors for storing results
lda_accuracy_values <- c()
lda_error_values <- c()
lda_na_counts <- c()

qda_accuracy_values <- c()
qda_error_values <- c()
qda_na_counts <- c()

# Initialize data frame to store results
error_results <- data.frame(Threshold = numeric(), LDA_Error = numeric(), QDA_Error = numeric())

# Loop through thresholds
for (thresh in thresholds) {
  
  # LDA Predictions
  lda_predicted_class <- apply(lda_probs, 1, function(probs) {
    max_prob <- max(probs)
    if (max_prob < thresh) {
      return("NA")  # Assign NA class if below threshold
    } else {
      return(names(probs)[which.max(probs)])
    }
  })
  
  # QDA Predictions
  qda_predicted_class <- apply(qda_probs, 1, function(probs) {
    max_prob <- max(probs)
    if (max_prob < thresh) {
      return("NA")
    } else {
      return(names(probs)[which.max(probs)])
    }
  })
  
  # Convert test_data$Cooking_Code to factor
  test_data$Cooking_Code <- factor(test_data$Cooking_Code)
  
  # Ensure factor levels match, including "NA"
  all_levels <- c(levels(test_data$Cooking_Code), "NA")
  lda_predicted_class <- factor(lda_predicted_class, levels = all_levels)
  qda_predicted_class <- factor(qda_predicted_class, levels = all_levels)
  test_data$Cooking_Code <- factor(test_data$Cooking_Code, levels = all_levels)
  
  # Compute accuracy & error for LDA (ignoring NA)
  lda_correct <- sum(lda_predicted_class == test_data$Cooking_Code, na.rm = TRUE)
  lda_total <- sum(!is.na(lda_predicted_class))
  lda_accuracy <- lda_correct / lda_total
  lda_error <- 1 - lda_accuracy
  
  # Compute accuracy & error for QDA (ignoring NA)
  qda_correct <- sum(qda_predicted_class == test_data$Cooking_Code, na.rm = TRUE)
  qda_total <- sum(!is.na(qda_predicted_class))
  qda_accuracy <- qda_correct / qda_total
  qda_error <- 1 - qda_accuracy
  
  # Store values
  lda_accuracy_values <- c(lda_accuracy_values, lda_accuracy)
  lda_error_values <- c(lda_error_values, lda_error)
  lda_na_counts <- c(lda_na_counts, sum(is.na(lda_predicted_class)))
  
  qda_accuracy_values <- c(qda_accuracy_values, qda_accuracy)
  qda_error_values <- c(qda_error_values, qda_error)
  qda_na_counts <- c(qda_na_counts, sum(is.na(qda_predicted_class)))
  
  # Store error rates in dataframe
  error_results <- rbind(error_results, data.frame(Threshold = thresh, LDA_Error = lda_error, QDA_Error = qda_error))
}

# Print error rates for each threshold
print("Error Rates for Different Thresholds:")
print(error_results)

# PLOT ACCURACY
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2) + 0.1)
plot(thresholds, lda_accuracy_values, type = "o", col = "blue", pch = 16, ylim = c(0, 1),
     xlab = "Threshold", ylab = "Accuracy", main = "LDA & QDA Accuracy",
     lwd = 3, cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 1.5)
lines(thresholds, qda_accuracy_values, type = "o", col = "red", pch = 16, lwd = 3)
legend("topright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 3, cex = 1.5)

# PLOT ERROR
plot(thresholds, lda_error_values, type = "o", col = "blue", pch = 16, ylim = c(0, 1),
     xlab = "Threshold", ylab = "Error Rate", main = "LDA & QDA Error Rate",
     lwd = 3, cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 1.5)
lines(thresholds, qda_error_values, type = "o", col = "red", pch = 16, lwd = 3)
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 3, cex = 1.5)
