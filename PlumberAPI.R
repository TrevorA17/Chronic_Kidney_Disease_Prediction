# plumber.R

# Load required library
library(plumber)

# Load the saved SVM model
loaded_svm_model <- readRDS("./models/saved_svm_model.rds")

#* @apiTitle Kidney Disease Prediction API

#* @apiDescription Predicts the likelihood of kidney disease based on patient parameters.

#* @param Bp Blood pressure
#* @param Sg Specific gravity
#* @param Al Albumin
#* @param Su Sugar
#* @param Rbc Red blood cells (0 = abnormal, 1 = normal)
#* @param Bu Blood urea
#* @param Sc Serum creatinine
#* @param Sod Sodium
#* @param Pot Potassium
#* @param Hemo Hemoglobin
#* @param Wbcc White blood cell count
#* @param Rbcc Red blood cell count
#* @param Htn Hypertension (0 = no, 1 = yes)

#* @get /predict_kidney

predict_kidney <- function(Bp, Sg, Al, Su, Rbc, Bu, Sc, Sod, Pot,
                           Hemo, Wbcc, Rbcc, Htn) {
  
  # Create a data frame for prediction
  new_data <- data.frame(
    Bp = as.numeric(Bp),
    Sg = as.numeric(Sg),
    Al = as.numeric(Al),
    Su = as.numeric(Su),
    Rbc = factor(Rbc, levels = c(0, 1)),
    Bu = as.numeric(Bu),
    Sc = as.numeric(Sc),
    Sod = as.numeric(Sod),
    Pot = as.numeric(Pot),
    Hemo = as.numeric(Hemo),
    Wbcc = as.numeric(Wbcc),
    Rbcc = as.numeric(Rbcc),
    Htn = factor(Htn, levels = c(0, 1))
  )
  
  # Predict using the loaded SVM model
  prediction <- predict(loaded_svm_model, newdata = new_data)
  
  # Return prediction result
  list(prediction = as.character(prediction))
}
