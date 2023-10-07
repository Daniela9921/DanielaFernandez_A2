# Load necessary libraries
library(tidyverse)
library(caret)
library(lattice)

# Load the dataset
# Replace with the correct file path to your dataset
diabetes_012_health_indicators_BRFSS2015 <- read_csv("~/GUERRA/diabetes_012_health_indicators_BRFSS2015.csv")

# Sample 1% of the dataset for each class variable
set.seed(123)  # For reproducibility
sampled_data_diabetes <- diabetes_012_health_indicators_BRFSS2015 %>%
  filter(Diabetes_012 == 0) %>%
  sample_frac(0.01) %>%
  bind_rows(diabetes_012_health_indicators_BRFSS2015 %>%
              filter(Diabetes_012 == 1))

sampled_data_heart_disease <- diabetes_012_health_indicators_BRFSS2015 %>%
  filter(HeartDiseaseorAttack == 0) %>%
  sample_frac(0.01) %>%
  bind_rows(diabetes_012_health_indicators_BRFSS2015 %>%
              filter(HeartDiseaseorAttack == 1))

sampled_data_sex <- diabetes_012_health_indicators_BRFSS2015 %>%
  filter(Sex == 0) %>%
  sample_frac(0.01) %>%
  bind_rows(diabetes_012_health_indicators_BRFSS2015 %>%
              filter(Sex == 1))

# Feature engineering (modify as needed)
# You can scale numerical features, one-hot encode categorical variables, etc.

# Function to train and evaluate KNN model
train_evaluate_knn <- function(data, class_var_name) {
  # Separate predictors and class variable
  predictors <- select(data, -c(class_var_name))
  class_column <- data[[class_var_name]]
  
  # Create a control set for K search
  ctrl <- trainControl(method = "cv", number = 10)
  
  # Train a KNN model to find the optimal K
  knn_model <- train(x = predictors,
                     y = class_column,
                     method = "knn",
                     trControl = ctrl)
  
  # Print results
  cat("KNN Model for", class_var_name, "\n")
  print(knn_model)
}

# Function to train and evaluate linear/multilinear regression models
train_evaluate_linear_regression <- function(data, target_var_name) {
  # Create a linear regression model with all variables
  lm_model <- lm(reformulate(setdiff(names(data), target_var_name), response = target_var_name), data = data)
  
  # Print results
  cat("Linear Regression Model for", target_var_name, "\n")
  print(summary(lm_model))
  
  # Create a multilinear regression model with all variables
  mlm_model <- lm(reformulate(names(data)[names(data) != target_var_name], response = target_var_name), data = data)
  
  # Print results
  cat("Multilinear Regression Model for", target_var_name, "\n")
  print(summary(mlm_model))
}

# Train and evaluate KNN models for each class variable
train_evaluate_knn(sampled_data_diabetes, "Diabetes_012")
train_evaluate_knn(sampled_data_heart_disease, "HeartDiseaseorAttack")
train_evaluate_knn(sampled_data_sex, "Sex")

# Train and evaluate linear/multilinear regression models for each target variable
train_evaluate_linear_regression(subset_bmi, "BMI")
train_evaluate_linear_regression(subset_menthlth, "MentHlth")
train_evaluate_linear_regression(subset_phys_hlth, "PhysHlth")
