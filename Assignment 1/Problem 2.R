setwd("C:/Users/suvid/Desktop/ECON F215 Assignment 1/")

library(foreign)  # For reading .dta files
library(dplyr)  # For data manipulation

# Load Household data
household_data <- read.dta("NSSO Dataset/NSSO_68/Household Characteristics - Block 3 -  Level 2 -  68.dta")

data <- household_data %>%
  dplyr::select(whether_Land_owned, HH_Size, HH_Type, NCO_2004) %>%
  na.omit()  # Remove missing values

# Make new column to account for land ownership
data$land_ownership <- ifelse(data$whether_Land_owned == 2, 0, 1)

# Convert categorical variables to factors
data$land_ownership <- as.factor(data$land_ownership)  
data$HH_Type <- as.factor(data$HH_Type)
data$NCO_2004 <- as.factor(data$NCO_2004)

# Convert HH_Size to numeric
data$HH_Size <- as.numeric(data$HH_Size)

# Part 1

print("Part 1")

# Logistic Regression Model
logit_model <- glm(land_ownership ~ HH_Size + HH_Type + NCO_2004,
                   data = data, family = binomial)

summary(logit_model)

# Predict probabilities for all observations
data$predicted_prob <- predict(logit_model, data, type = "response")

# Convert probabilities to binary classification (threshold = 0.5)
data$predicted_class <- ifelse(data$predicted_prob > 0.5, 1, 0)

# Compute Confusion Matrix
conf_matrix <- table(Actual = data$land_ownership, Predicted = data$predicted_class)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
mismatch_rate <- 1 - accuracy

# Print results
print(paste("Model Accuracy:", round(accuracy, 3)))
print(paste("Error Rate:", round(mismatch_rate, 3)))


# Part 2

print("Part 2")

library(caTools)  # For data splitting

# Split data: 90% training, 10% testing
set.seed(123)
split <- sample.split(data$land_ownership, SplitRatio = 0.9)
train_data <- subset(data, split == TRUE)
test_data  <- subset(data, split == FALSE)

# Logistic Regression Model on Training Data
logit_model <- glm(land_ownership ~ HH_Size + HH_Type + NCO_2004,
                   data = train_data, family = binomial)

summary(logit_model)

# Predict probabilities on test data
test_data$predicted_prob <- predict(logit_model, test_data, type = "response")

# Convert probabilities to binary classification (threshold = 0.5)
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

# Compute mismatch (Error Rate)
conf_matrix_test <- table(Actual = test_data$land_ownership, Predicted = test_data$predicted_class)
conf_matrix_test

# Calculate accuracy
accuracy_test <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)
error_rate_test <- 1 - accuracy_test

# Print results
print(paste("Test Model Accuracy:", round(accuracy_test, 3)))
print(paste("Test Error Rate:", round(error_rate_test, 3)))


# Visualizing Data

# Probability Distribution of Predicted Values

hist(data$predicted_prob, 
     main = "Distribution of Predicted Probabilities", 
     xlab = "Predicted Probability of Land Ownership", 
     col = "lightblue", border = "black", breaks = 20)


# ROC Curve & AUC (Performance Evaluation)

library(pROC)

# Compute ROC Curve
roc_curve <- roc(data$land_ownership, data$predicted_prob)

# Plot ROC Curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "red")  # Random classifier line

# Compute AUC
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))


# Confusion Matrix Heatmap
conf_matrix_numeric <- as.matrix(conf_matrix)

# Define row and column names
rownames(conf_matrix_numeric) <- c("Actual: \nNo Land", "Actual: \n Land")
colnames(conf_matrix_numeric) <- c("Predicted: No Land", "Predicted: Land")

# Plot heatmap
image(1:2, 1:2, t(conf_matrix_numeric), 
      col = terrain.colors(10), axes = FALSE, main = "Confusion Matrix: Land Ownership")

# Add axis labels
axis(1, at = c(1, 2), labels = colnames(conf_matrix_numeric), tick = FALSE)  # X-axis (Predicted)
axis(2, at = c(1, 2), labels = rownames(conf_matrix_numeric), las = 2, tick = FALSE)  # Y-axis (Actual)

# Generate grid for text positioning
x_positions <- rep(1:2, each = 2)  # X-coordinates for text
y_positions <- rep(2:1, times = 2)  # Y-coordinates for text (flipped to match heatmap)

# Add text labels inside cells
text(x_positions, y_positions, labels = as.vector(conf_matrix_numeric), col = "black", cex = 1.5, font = 2)
