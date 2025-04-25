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

# 1. Total missing values in the entire data frame
total_missing <- sum(is.na(kidney_data))
print(paste("Total missing values:", total_missing))

# 2. Missing values per column
missing_by_column <- colSums(is.na(kidney_data))
print("Missing values by column:")
print(missing_by_column)

# 3. Percentage of missing values per column
missing_pct <- round(100 * missing_by_column / nrow(kidney_data), 2)
print("Percentage of missing values by column:")
print(missing_pct)

# 4. Visual check with naniar (optional)
if (!requireNamespace("naniar", quietly = TRUE)) {
  install.packages("naniar")
}
library(naniar)
# Plot a missingness map
vis_miss(kidney_data)

# Recode the factor levels of the target variable
levels(kidney_data$Class) <- c("Negative", "Positive")