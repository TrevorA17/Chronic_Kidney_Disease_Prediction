# Load kidney disease dataset
kidney_data <- read.csv("data/new_model.csv", colClasses = c(
  Bp     = "numeric",
  Sg     = "numeric",
  Al     = "numeric",
  Su     = "numeric",
  Rbc    = "factor",   # Red blood cells: 0/1 or “normal”/“abnormal”
  Bu     = "numeric",  # Blood urea
  Sc     = "numeric",  # Serum creatinine
  Sod    = "numeric",  # Sodium
  Pot    = "numeric",  # Potassium
  Hemo   = "numeric",  # Hemoglobin
  Wbcc   = "numeric",  # White blood cell count
  Rbcc   = "numeric",  # Red blood cell count
  Htn    = "factor",   # Hypertension: 0/1
  Class  = "factor"    # Disease class: 0=no disease, 1=disease
), header = TRUE)

# Display the structure of the dataset
str(kidney_data)

# Load required libraries
library(caret)
library(boot)

set.seed(123)

# 1. Data Splitting (80% train, 20% test)
train_index <- createDataPartition(kidney_data$Class, p = 0.8, list = FALSE)
train_data  <- kidney_data[train_index, ]
test_data   <- kidney_data[-train_index, ]

dim(test_data)
dim(train_data)

# 2. Bootstrapping on the training data
#    Define a statistic function, e.g., mean of Blood Urea (Bu)
boot_mean_bu <- function(data, indices) {
  sampled <- data[indices, ]
  return(mean(sampled$Bu, na.rm = TRUE))
}

#    Perform bootstrapping: 1000 resamples
boot_results <- boot(data = train_data, statistic = boot_mean_bu, R = 1000)

# 3. View Bootstrapping Results
print(boot_results)

# 4. Bootstrap Confidence Interval for the mean Bu
boot_ci <- boot.ci(boot_results, type = c("norm", "basic", "perc"))
print(boot_ci)

# 5. (Optional) Plot the bootstrap distribution
plot(boot_results)

# Load necessary libraries
library(caret)
library(randomForest)
library(e1071)  # For other classifiers if needed

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(kidney_data$Class, p = 0.8, list = FALSE)
train_data <- kidney_data[trainIndex, ]
test_data <- kidney_data[-trainIndex, ]

# Set up cross-validation (repeated k-fold)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10,  # 10-fold cross-validation
                     repeats = 3,  # Repeat the process 3 times
                     classProbs = TRUE,  # For classification, need class probabilities
                     summaryFunction = twoClassSummary)  # To track ROC, Sensitivity, etc.

print(ctrl)

# Inspect the levels of the 'Class' factor
levels(kidney_data$Class)

# Train a logistic regression model
logistic_model <- train(Class ~ ., 
                        data = train_data, 
                        method = "glm", 
                        family = "binomial", 
                        trControl = ctrl, 
                        metric = "ROC")  # Use ROC as the evaluation metric

# Print the results of logistic regression model
print(logistic_model)

# Train a random forest model
rf_model <- train(Class ~ ., 
                  data = train_data, 
                  method = "rf", 
                  trControl = ctrl, 
                  metric = "ROC")  # Use ROC for performance evaluation

# Print the results of the random forest model
print(rf_model)
