# ----------------------------------------
# STEP 1: Load Libraries and Data
# ----------------------------------------
packages <- c("tidyverse", "car", "lmtest", "pROC", "randomForest", "Metrics", "caret", "forcats")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

setwd("D:\\Masters\\VCU\\Classes\\SCMA\\R\\A3")
data <- read.csv("../A1/Ref/NSSO68.csv")

# ----------------------------------------
# STEP 2: Prepare Data
# ----------------------------------------
reg_data <- data[, c("MPCE_MRP", "Age", "Education", "HH_type", "No_of_Meals_per_day",
                     "ricetotal_v", "wheattotal_v", "Milktotal_v", "chicken_v",
                     "vegtt_v", "fruitstt_v", "pulsestot_v", "nonvegtotal_v")]

reg_data <- na.omit(reg_data)

reg_data <- reg_data %>%
  filter(!is.na(Education), !is.na(HH_type)) %>%
  droplevels()

numeric_cols <- sapply(reg_data, is.numeric)
zero_var_cols <- names(which(apply(reg_data[, numeric_cols], 2, var, na.rm = TRUE) == 0))
if(length(zero_var_cols) > 0){
  reg_data <- reg_data[, !(names(reg_data) %in% zero_var_cols)]
}

reg_data$Education <- factor(reg_data$Education)
reg_data$HH_type <- factor(reg_data$HH_type)

# ----------------------------------------
# STEP 3: Initial Model and Diagnostics
# ----------------------------------------
model1 <- lm(MPCE_MRP ~ ., data = reg_data)
summary(model1)

# Check multicollinearity
vif(model1)  

# Breusch-Pagan test
bptest(model1)

# Plot residual diagnostics
par(mfrow = c(2, 2))
plot(model1)

# Kolmogorov-Smirnov test for residual normality
ks.test(scale(residuals(model1)), "pnorm")

# ----------------------------------------
# STEP 4: Outlier Removal & Model Refit
# ----------------------------------------
outliers <- c(62147, 84953, 28787, 20329, 25692, 633180)
reg_data_clean <- reg_data[-outliers, ]
reg_data_clean$log_MPCE <- log(reg_data_clean$MPCE_MRP)

model_clean <- lm(log_MPCE ~ ., data = reg_data_clean)
summary(model_clean)

# Residual diagnostic plots for the clean model
par(mfrow = c(2, 2))
plot(model_clean)

# ----------------------------------------
# STEP 5: Refit Model with Collapsed Factors
# ----------------------------------------
# Combine insignificant education levels
reg_data_clean$Education <- fct_collapse(reg_data_clean$Education, "Other" = c("2", "4"))
reg_data_clean$Education <- droplevels(reg_data_clean$Education)

# Refit
model2 <- lm(log_MPCE ~ Age + Education + HH_type + No_of_Meals_per_day +
               ricetotal_v + wheattotal_v + Milktotal_v + chicken_v +
               vegtt_v + pulsestot_v, data = reg_data_clean)
summary(model2)

# ----------------------------------------
# STEP 6: Random Forest Regression
# ----------------------------------------
set.seed(123)
train_index <- createDataPartition(reg_data_clean$log_MPCE, p = 0.8, list = FALSE)
train_rf <- reg_data_clean[train_index, ]
test_rf  <- reg_data_clean[-train_index, ]

rf_model <- randomForest(log_MPCE ~ Age + Education + HH_type + No_of_Meals_per_day +
                           ricetotal_v + wheattotal_v + Milktotal_v + chicken_v +
                           vegtt_v + pulsestot_v,
                         data = train_rf, ntree = 50, importance = TRUE)

rf_preds <- predict(rf_model, newdata = test_rf)
r2_val <- R2(rf_preds, test_rf$log_MPCE)
rmse_val <- rmse(test_rf$log_MPCE, rf_preds)

cat("Random Forest R²:", round(r2_val, 4), "\n")
cat("Random Forest RMSE:", round(rmse_val, 4), "\n")

# Plot variable importance
varImpPlot(rf_model, main = "Random Forest - Variable Importance")

# ----------------------------------------
# STEP 7: Final Comparison
# ----------------------------------------
cat("Adjusted R² - Model 1 (Raw MPCE): ", summary(model1)$adj.r.squared, "\n")
cat("Adjusted R² - Model Clean (Log MPCE with outliers removed): ", summary(model_clean)$adj.r.squared, "\n")
cat("Adjusted R² - Model 2 (Collapsed factors): ", summary(model2)$adj.r.squared, "\n")
