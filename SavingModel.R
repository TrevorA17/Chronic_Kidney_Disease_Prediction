# Save the best performing model
saveRDS(svm_model, "./models/saved_svm_model.rds")

# Load the saved model
loaded_svm_model <- readRDS("./models/saved_svm_model.rds")

# Create a new sample input (example)
new_patient <- data.frame(
  Bp = 80,
  Sg = 1.02,
  Al = 1,
  Su = 0,
  Rbc = factor(1, levels = levels(kidney_data$Rbc)),
  Bu = 36,
  Sc = 1.2,
  Sod = 137.53,
  Pot = 4.63,
  Hemo = 15.4,
  Wbcc = 7800,
  Rbcc = 5.2,
  Htn = factor(1, levels = levels(kidney_data$Htn))
)

# Use the loaded model to predict
prediction <- predict(loaded_svm_model, newdata = new_patient)

# Print the prediction
print(prediction)
