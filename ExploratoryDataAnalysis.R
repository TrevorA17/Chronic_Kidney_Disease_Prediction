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

# View the first few rows of the dataset
head(kidney_data)

# Open the dataset in a viewer window (in RStudio)
View(kidney_data)

# Load necessary library
library(dplyr)

# 1. Measures of Frequency
#    Count occurrences for each level of each factor column
freq_Rbc   <- kidney_data %>% count(Rbc, name = "Freq")
freq_Htn   <- kidney_data %>% count(Htn, name = "Freq")
freq_Class <- kidney_data %>% count(Class, name = "Freq")

# 2. Measures of Central Tendency
#    Compute mean, median, and mode for numeric columns
numeric_cols <- kidney_data %>% select(where(is.numeric))
central_tendency <- numeric_cols %>% summarize_all(list(
  Mean   = ~ mean(. , na.rm = TRUE),
  Median = ~ median(. , na.rm = TRUE),
  Mode   = ~ {
    ux <- unique(.,   na.rm = TRUE)
    ux[which.max(tabulate(match(., ux)))]
  }
))

# 3. Measures of Distribution
#    Min, Max, Range, Standard Deviation, and Quantiles 
distribution_measures <- numeric_cols %>% summarize_all(list(
  Min    = ~ min(. , na.rm = TRUE),
  Max    = ~ max(. , na.rm = TRUE),
  Range  = ~ diff(range(. , na.rm = TRUE)),
  SD     = ~ sd(. , na.rm = TRUE),
  Q1     = ~ quantile(. , 0.25, na.rm = TRUE),
  Q3     = ~ quantile(. , 0.75, na.rm = TRUE)
))

# 4. Measures of Relationship
#    Correlation matrix for numeric variables
correlation_matrix <- cor(numeric_cols, use = "pairwise.complete.obs")

# Print outputs
freq_Rbc; freq_Htn; freq_Class
central_tendency
distribution_measures
correlation_matrix

# Load necessary package
library(dplyr)
library(broom)  # for tidy()

# 1. Identify numeric columns
numeric_vars <- kidney_data %>%
  select(where(is.numeric)) %>%
  names()

# 2. Function to run ANOVA for one variable
run_anova <- function(var_name) {
  formula <- as.formula(paste(var_name, "~ Class"))
  aov_res  <- aov(formula, data = kidney_data)
  tidy_res <- tidy(aov_res)
  # Extract the term row for 'Class'
  class_row <- tidy_res %>% filter(term == "Class")
  # Append variable name
  class_row$variable <- var_name
  class_row
}

# 3. Apply across all numeric variables
anova_results <- lapply(numeric_vars, run_anova) %>%
  bind_rows() %>%
  select(variable, df, statistic, p.value)

# 4. View results
print(anova_results)

# Load libraries
library(ggplot2)
library(GGally)

# Univariate Plots

# 1. Histogram of Blood Pressure (Bp)
ggplot(kidney_data, aes(x = Bp)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Blood Pressure", x = "Blood Pressure", y = "Count") +
  theme_minimal()

# 2. Density plot of Serum Creatinine (Sc)
ggplot(kidney_data, aes(x = Sc)) +
  geom_density(fill = "tomato", alpha = 0.5) +
  labs(title = "Density of Serum Creatinine", x = "Serum Creatinine", y = "Density") +
  theme_minimal()

# 3. Bar chart of Red Blood Cell count category (Rbc)
ggplot(kidney_data, aes(x = Rbc)) +
  geom_bar(fill = "gold", color = "black") +
  labs(title = "Red Blood Cell Count Categories", x = "Rbc", y = "Frequency") +
  theme_minimal()


# Multivariate Plots

# 1. Scatter plot of Urea (Bu) vs. Creatinine (Sc) colored by Class
ggplot(kidney_data, aes(x = Bu, y = Sc, color = Class)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Urea vs. Creatinine by Disease Class", x = "Blood Urea", y = "Serum Creatinine") +
  theme_minimal()

# 2. Boxplots of Hemoglobin (Hemo) by Disease Class
ggplot(kidney_data, aes(x = Class, y = Hemo, fill = Class)) +
  geom_boxplot() +
  labs(title = "Hemoglobin by Disease Class", x = "Class", y = "Hemoglobin") +
  theme_minimal()


# 4. Heatmap of correlation matrix
corr_mat <- cor(kidney_data %>% select(where(is.numeric)), use = "pairwise.complete.obs")
library(reshape2)
melted_corr <- reshape2::melt(corr_mat)
ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

