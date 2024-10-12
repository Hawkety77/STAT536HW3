library(tidyverse)

schools <- read_table("SchoolResults.txt")

# EDA
corrplot::corrplot(cor(schools))
formula <- formula(Score ~ . - Income + poly(Income, degree = 2) - STratio)
lm_model <- lm(formula, data = schools)
summary(lm_model)
car::vif(lm_model)
car::avPlots(lm_model)
library(ggfortify)
forecast::autoplot(lm_model)

# CV
k <- 50  # Number of folds
folds <- cut(1:nrow(schools), breaks = k, labels = FALSE)

results <- c()
union_df <- data.frame()

for (i in 1:k) {
  # Split the data
  test_indices <- which(folds == i, arr.ind = TRUE)
  test_data <- schools[test_indices, ]
  train_data <- schools[-test_indices, ]
  
  # Train the model
  model <- lm(formula, data = train_data)
  
  # Predict on test data
  predictions <- predict(model, newdata = test_data)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((predictions - test_data$Score) ^ 2))
  results <- c(results, rmse)
  prediction_df <- data.frame(predictions = predictions, actuals = test_data$Score)
  union_df <- rbind(union_df, prediction_df)
}
mean_rmse <- mean(results)
print(mean_rmse)

# 1. “Income” is generally a measure of how much money a school has to spend on extracurricular activities 
# (as opposed to expenditures which i s how much spent per student in the class room). Is there evidence
# of diminishing returns on extracurricular activities in terms of student learning?

summary(lm_model)
lm_model_no_poly <- lm(Score ~ . - STratio - Expenditure, data = schools)
car::avPlot(lm_model_no_poly, variable = "Income", main = "Added Variable Plot for Income", lwd = 0)
car::avPlot(lm_model, variable = "poly(Income, degree = 2)2", main = "Added Variable Plot for Income^2", lwd = 2)

#   2. Is English as a second language a barrier to student learning?
summary(lm_model)

#   3. In your opinion and based on the data, what can be done to increase student learning?
summary(lm_model)

#   4. (How well does the model predict compared to alternatives?)
gam_model <- mgcv::gam(Score ~ s(Lunch) + s(Computer) + s(Income) + s(Expenditure) + s(English) + s(STratio), data = schools)
summary(gam_model)

k <- 50  # Number of folds
folds <- cut(1:nrow(schools), breaks = k, labels = FALSE)

results <- c()
union_df <- data.frame()

for (i in 1:k) {
  # Split the data
  test_indices <- which(folds == i, arr.ind = TRUE)
  test_data <- schools[test_indices, ]
  train_data <- schools[-test_indices, ]
  
  # Train the model
  model <- mgcv::gam(Score ~ s(Lunch) + s(Computer) + s(Income) + s(Expenditure) + s(English) + s(STratio), data = train_data)
  
  # Predict on test data
  predictions <- predict(model, newdata = test_data)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((predictions - test_data$Score) ^ 2))
  results <- c(results, rmse)
  prediction_df <- data.frame(predictions = predictions, actuals = test_data$Score)
  union_df <- rbind(union_df, prediction_df)
}
mean_rmse <- mean(results)
print(mean_rmse)

k <- 50  # Number of folds
folds <- cut(1:nrow(schools), breaks = k, labels = FALSE)

results <- c()
union_df <- data.frame()

for (i in 1:k) {
  # Split the data
  test_indices <- which(folds == i, arr.ind = TRUE)
  test_data <- schools[test_indices, ]
  train_data <- schools[-test_indices, ]
  
  # Train the model
  model <- loess(Score ~ Income + Lunch + Computer + English, degree = 1, span = .5, data = train_data)
  
  # Predict on test data
  predictions <- predict(model, test_data)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((predictions - test_data$Score) ^ 2, na.rm = FALSE))
  results <- c(results, rmse)
  prediction_df <- data.frame(predictions = predictions, actuals = test_data$Score)
  union_df <- rbind(union_df, prediction_df)
}
mean_rmse <- mean(na.omit(results))
print(mean_rmse)

