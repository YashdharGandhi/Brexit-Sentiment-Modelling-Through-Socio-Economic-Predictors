knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning=FALSE)
knitr::opts_chunk$set(
  fig.width = 12,
  fig.height = 8,
  fig.margin = c(1, 1, 1, 1)
)



set.seed(10)
# Load the necessary libraries

library(cluster)

# Load the dataset
data <- read.csv("brexit.csv")

# Data Cleaning: Remove rows with NULL values or 0
clean_data <- data[complete.cases(data) & apply(data[, c("abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")] != 0, 1, all), ]

# Extract input variables for clustering
input_data <- clean_data[, c("abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")]

# Define the range of K values to consider
k_range <- 2:10

# Initialize vectors to store gap statistic values
gap_stat_values <- numeric(length(k_range))

# Perform K-means clustering for each value of K and calculate gap statistic
for (i in 1:length(k_range)) {
  k <- k_range[i]
  kmeans_result <- kmeans(input_data, centers = k, nstart = 25)
  gap_stat_values[i] <- clusGap(input_data, FUN = kmeans, K.max = k, B = 50)$Tab[,"gap"]
}

# Find the optimal number of clusters where gap statistic is maximized
optimal_k <- k_range[which.max(gap_stat_values)]

# Perform K-means clustering with optimal K
kmeans_result <- kmeans(input_data, centers = optimal_k, nstart = 25)

# View the cluster centers
kmeans_centers <- kmeans_result$centers

# Merge Clusters with Voting Outcomes
# Assuming the column with Brexit voting outcomes is named "voteBrexit"
# Since there's no "ElectoralWard" column, we'll use row numbers as a unique identifier
cluster_data <- data.frame(clean_data, Cluster = kmeans_result$cluster)
merged_data <- merge(cluster_data, clean_data[, c("voteBrexit")], by.x = 0, by.y = 0)

# Analysis: Assess the association between clusters and voting outcomes
# For example, using a contingency table and chi-square test
table <- table(merged_data$Cluster, merged_data$voteBrexit)
chi_sq <- chisq.test(table)

# Print contingency table and chi-square test results
print("Contingency Table:")
print(table)
print("Chi-Square Test Results:")
print(chi_sq)




cluster_data_1 <- cbind(input_data, Cluster = kmeans_result$cluster)

medians_by_cluster <- aggregate(. ~ Cluster, data = cluster_data_1, FUN = median)

# Display the median values by cluster
print("Median Values by Cluster:")
print(medians_by_cluster)



# Visualization: Visualize the clusters using scatter plot
plot(input_data[,1], input_data[,2], col = kmeans_result$cluster, 
     main = "K-means Clustering with Optimal Number of Clusters",
     xlab = "abc1", ylab = "medianIncome")
points(kmeans_centers[,1], kmeans_centers[,2], col = 1:optimal_k, pch = 8, cex = 2)
legend("topright", legend = paste("Cluster", 1:optimal_k), col = 1:optimal_k, pch = 5)



# Filter data for Cluster 1
cluster_1_data <- subset(merged_data, Cluster == 1)

# Calculate the count of wards voting for Brexit (TRUE) and against Brexit (FALSE) in Cluster 1
cluster_1_counts <- table(cluster_1_data$voteBrexit)

cluster_2_data <- subset(merged_data, Cluster == 2)

# Calculate the count of wards voting for Brexit (TRUE) and against Brexit (FALSE) in Cluster 2
cluster_2_counts <- table(cluster_2_data$voteBrexit)

cluster_3_data <- subset(merged_data, Cluster == 3)

# Calculate the count of wards voting for Brexit (TRUE) and against Brexit (FALSE) in Cluster 3
cluster_3_counts <- table(cluster_3_data$voteBrexit)

ylim <- c(0, 150)
# Create a new plotting area with 1 row and 3 columns
par(mfrow = c(1, 3))

# Plot for Cluster 1
barplot(cluster_1_counts,
        ylim = ylim,
        main = "Cluster 1",
        xlab = "Vote for Brexit",
        ylab = "Count",
        col = c("blue", "red"),
        legend.text = c("TRUE", "FALSE"))

# Plot for Cluster 2
barplot(cluster_2_counts, 
        ylim = ylim,
        main = "Cluster 2",
        xlab = "Vote for Brexit",
        ylab = "", # Suppress y-axis label for clarity
        col = c("blue", "red"),
        legend.text = c("TRUE", "FALSE"))

# Plot for Cluster 3
barplot(cluster_3_counts, 
        ylim = ylim,
        main = "Cluster 3",
        xlab = "Vote for Brexit",
        ylab = "", # Suppress y-axis label for clarity
        col = c("blue", "red"),
        legend.text = c("TRUE", "FALSE"))





# Fit Logistic Regression Model
logit_model <- glm(voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd + notBornUK + as.factor(kmeans_result$cluster), 
                   data = clean_data, family = binomial)

# Print Model Summary
summary(logit_model)

# Interpret Coefficients
print("Interpreting Coefficients:")
print(coef(logit_model))

# Assess Significance of Coefficients
print("Assessing Significance of Coefficients:")
print(summary(logit_model)$coefficients[, c("Estimate", "Pr(>|z|)")])

# Identify Variables with Strong Effects
significant_vars <- summary(logit_model)$coefficients[summary(logit_model)$coefficients[, "Pr(>|z|)"] < 0.05, ]
print("Variables with Significant Effects:")
print(significant_vars)

# Comparison with Guardian Plots
# Compare with Guardian Demographic Characteristics

# Define variable names corresponding to coefficients
variable_names <- c("Intercept", "abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK", "Cluster2", "Cluster3")

# Extract coefficients and standard errors
coefficients <- coef(summary(logit_model))[, "Estimate"]
standard_errors <- coef(summary(logit_model))[, "Std. Error"]

# Print coefficients and standard errors
print("Coefficients:")
print(coefficients)

print("Standard Errors:")
print(standard_errors)

# Print comparison with Guardian Demographic Characteristics
print("Comparison with Guardian Demographic Characteristics:")

# % Residents with Higher Education
higher_ed_result <- coefficients[variable_names == "withHigherEd"]
print(paste("% Residents with Higher Education:", higher_ed_result))

# % Residents with No Formal Qualifications
# Assuming noFormalQual is represented by the inverse of withHigherEd
no_qualifications_result <- coefficients[variable_names == "withHigherEd"] * -1
print(paste("% Residents with No Formal Qualifications:", no_qualifications_result))

# Median Annual Income of Residents
income_result <- coefficients[variable_names == "medianIncome"]
print(paste("Median Annual Income of Residents:", income_result))

# % Residents of ABC1 Social Grade
abc1_result <- coefficients[variable_names == "abc1"]
print(paste("% Residents of ABC1 Social Grade:", abc1_result))

# Median Age of Residents
age_result <- coefficients[variable_names == "medianAge"]
print(paste("Median Age of Residents:", age_result))

# % Residents Not Born in the UK
not_born_uk_result <- coefficients[variable_names == "notBornUK"]
print(paste("% Residents Not Born in the UK:", not_born_uk_result))

# Interpretation of Cluster Effects
# Cluster 2
cluster2_effect <- coefficients[variable_names == "Cluster2"]
print(paste("Effect of Cluster 2:", cluster2_effect))

# Cluster 3
cluster3_effect <- coefficients[variable_names == "Cluster3"]
print(paste("Effect of Cluster 3:", cluster3_effect))



knitr::opts_chunk$set(message=FALSE)

# Extract variable names from coefficients
variable_names <- names(coefficients)

# Create data frame with variable names and coefficients
coefficients_df <- data.frame(variable = variable_names, coefficient = coefficients)

# Add standard errors to the data frame
coefficients_df$std_error <- standard_errors[variable_names]

# Print the data frame
print(coefficients_df)
# Merge data frames


# Combine coefficients and standard errors into a data frame
coefficients_df <- data.frame(
  variable = names(coefficients),
  coefficient = coefficients,
  std_error = standard_errors
)

# Create the bar plot
library(ggplot2)
coefficients_plot <- ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                width = 0.2, color = "black") +
  labs(x = "Variable", y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Coefficients and Standard Errors") +
  coord_flip()

# Print the plot
print(coefficients_plot)




library(ggplot2)
library(cowplot)
library(gridExtra)


# Create a function to add a median line to the plot
add_median_line <- function(plot, data, x, y) {
  median_value <- median(data[[x]])
  plot + 
    geom_vline(xintercept = median_value, linetype = "dotted", color = "black") +
    geom_text(aes(x = median_value, y = 0.5, label = round(median_value, 2)), vjust = -1)
}

# Plot 1: Percentage of residents with higher education
plot1 <- ggplot(clean_data, aes(x = withHigherEd, y = withHigherEd, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents with higher education", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot1_with_median <- add_median_line(plot1, clean_data, "withHigherEd", "withHigherEd")

# Plot 2: Percentage of residents with no formal qualifications
plot2 <- ggplot(clean_data, aes(x = notBornUK, y = notBornUK, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents with no formal qualifications", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot2_with_median <- add_median_line(plot2, clean_data, "notBornUK", "notBornUK")

# Plot 3: Median annual income of residents
plot3 <- ggplot(clean_data, aes(x = medianIncome, y = medianIncome, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Median annual income of residents", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot3_with_median <- add_median_line(plot3, clean_data, "medianIncome", "medianIncome")

# Plot 4: Percentage of residents of ABC1 social grade
plot4 <- ggplot(clean_data, aes(x = abc1, y = abc1, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents of ABC1 social grade", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot4_with_median <- add_median_line(plot4, clean_data, "abc1", "abc1")

# Plot 5: Median age of residents
plot5 <- ggplot(clean_data, aes(x = medianAge, y = medianAge, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Median age of residents", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot5_with_median <- add_median_line(plot5, clean_data, "medianAge", "medianAge")

# Plot 6: Percentage of residents not born in the UK
plot6 <- ggplot(clean_data, aes(x = notBornUK, y = notBornUK, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents not born in the UK", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot6_with_median <- add_median_line(plot6, clean_data, "notBornUK", "notBornUK")

# Arrange plots using grid.arrange
grid.arrange(plot1_with_median, plot2_with_median, plot3_with_median, plot4_with_median, plot5_with_median, plot6_with_median,
             nrow = 2, ncol = 3)



# Compute Variance Inflation Factors (VIFs)
vif_values <- car::vif(logit_model)

# Print VIF values
print("Variance Inflation Factors:")
print(vif_values)



# Create residual plots to assess linearity
par(mfrow=c(2,2))
plot(logit_model)


# Compute Cook's distance
cooks_distance <- cooks.distance(logit_model)

# Identify influential points based on Cook's distance
influential_points <- which(cooks_distance > 4/nrow(clean_data))

# Print influential points based on Cook's distance
print("Influential Points Based on Cook's Distance:")
print(influential_points)




# Step 1: Generate Bootstrap Samples
n_bootstraps <- 100  # Number of bootstrap samples
bootstrap_samples <- lapply(1:n_bootstraps, function(i) sample(1:nrow(clean_data), replace = TRUE))

# Step 2: Train Regression Models on Bootstrap Samples
models <- lapply(bootstrap_samples, function(indices) {
  sample_data <- clean_data[indices, ]
  model <- lm(voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd + notBornUK, data = sample_data)  # Assuming voteBrexit is your outcome variable
  return(model)
})

# Step 3: Aggregate Coefficient Estimates and Standard Errors
coefficients <- sapply(models, coef)
coefficients_mean <- colMeans(coefficients)
coefficients_sd <- apply(coefficients, 1, sd)

# Step 4: Compute Confidence Intervals and Hypothesis Tests
confidence_intervals <- cbind(coefficients_mean - 1.96 * coefficients_sd, coefficients_mean + 1.96 * coefficients_sd)

confidence_intervals
p_values <- 2 * pnorm(-abs(coefficients_mean / coefficients_sd))  # Two-sided hypothesis tests


# Step 1: Generate Bootstrap Samples (Already done in the previous step)

# Step 2: Train Regression Models on Bootstrap Samples (Already done in the previous step)

# Step 3: Aggregate Coefficients
coefficients <- sapply(models, coef)

# Step 4: Compute Average Coefficients
average_coefficients <- colMeans(coefficients)
average_coefficients
# Step 5: Assess Effect Size
effect_sizes <- abs(average_coefficients)
effect_sizes


# Review coefficients
coefficients <- coef(logit_model)

# Consider theoretical significance
# For example, if 'abc1' represents a demographic characteristic that is known to influence the outcome based on theory or previous research, its coefficient may be considered theoretically significant.

# Examine practical significance
# Calculate the odds ratios for each variable to understand the practical impact on the outcome
odds_ratios <- exp(coefficients)

# Compare with existing knowledge
# You can conduct a literature review or consult with domain experts to validate your findings.

# Print coefficients and odds ratios
print("Coefficients:")
print(coefficients)
print("Odds Ratios:")
print(odds_ratios)



knitr::opts_chunk$set(message=FALSE)

# Load necessary library for AUC calculation
library(pROC)

# Calculate AIC and BIC
AIC_value <- AIC(logit_model)
BIC_value <- BIC(logit_model)

# Predict probabilities for the training data
predicted_probs <- predict(logit_model, type = "response")

# Calculate AUC
AUC_value <- roc(clean_data$voteBrexit, predicted_probs)$auc

# Print model performance metrics
print("Model Performance Metrics:")
print(paste("AIC:", AIC_value))
print(paste("BIC:", BIC_value))
print(paste("AUC:", AUC_value))



library(caret)

library(boot)
fit_logit <- function(data, indices) {
  sample <- data[indices, ]
  model <- glm(voteBrexit ~ ., data = sample, family = binomial)
  coefficients <- coef(model)
  return(coefficients)  # Return coefficients
}





# Set the number of bootstrap replicates
R <- 1000

# Perform bootstrapping
boot_results <- boot(data = clean_data, statistic = fit_logit, R = R)
# Initialize an empty matrix to store predictions
boot_pred_manual <- matrix(NA, nrow = nrow(clean_data), ncol = R)

# Loop over each bootstrapped model
for (i in 1:R) {
  # Extract coefficients from the ith bootstrapped model
  coefficients_i <- boot_results$t[i, ]
  
  # Calculate log odds for each observation using the coefficients
  log_odds_i <- as.matrix(clean_data) %*% coefficients_i
  
  # Apply inverse logit function to obtain predicted probabilities
  pred_probs_i <- 1 / (1 + exp(-log_odds_i))
  
  # Store predicted probabilities in the ith column of boot_pred_manual
  boot_pred_manual[, i] <- pred_probs_i
}

# Aggregate predictions across all bootstrap samples
final_pred_manual <- apply(boot_pred_manual, 1, mean)

boot_coefs <- boot_results$t
coef_variability <- apply(boot_coefs, 2, sd)
print(coef_variability)
# Evaluate model performance
# For example, calculate AUC
library(pROC)
auc <- roc(clean_data$voteBrexit, final_pred_manual)$auc
print(paste("AUC is", auc))

#

set.seed(10)  # for reproducibility
train_index <- sample(1:nrow(clean_data), 0.7 * nrow(clean_data))  # 70% train, 30% test
train_data <- clean_data[train_index, ]
test_data <- clean_data[-train_index, ]

# Scale the input variables
# Exclude the "voteBrexit" column from the dataset
train_data_scaled <- scale(train_data[, !colnames(train_data) %in% "voteBrexit"])
test_data_scaled <- scale(test_data[, !colnames(test_data) %in% "voteBrexit"])


library(glmnet)

# Fit Lasso logistic regression model
lasso_model <- cv.glmnet(x = train_data_scaled, y = train_data$voteBrexit, alpha = 1, family = "binomial")
optimal_lambda_lasso <- lasso_model$lambda.min
lasso_coefficients <- coef(lasso_model, s = optimal_lambda_lasso)[-1]  # Exclude intercept


# Optimal lambda for Lasso
lambda_lasso <- lasso_model$lambda.min
print(paste("Optimal lambda for Lasso:", lambda_lasso))


# Coefficients for Lasso model
coef_lasso <- coef(lasso_model, s = lambda_lasso)



lasso_coefficients <- as.matrix(coef(lasso_model)[-1])  # Exclude intercept

# Get variable names
variable_names <- colnames(train_data_scaled)



# Order variables based on magnitude of coefficients for Lasso
ordered_indices_lasso <- order(abs(lasso_coefficients), decreasing = TRUE)
ordered_variables_lasso <- variable_names[ordered_indices_lasso]


print("Ordered variables for Lasso:")
print(ordered_variables_lasso)




# Evaluate Lasso model
lasso_pred <- predict(lasso_model, test_data_scaled, type = "response")
lasso_auc <- roc(test_data$voteBrexit, lasso_pred)$auc
lasso_accuracy <- sum((lasso_pred > 0.5) == test_data$voteBrexit) / length(test_data$voteBrexit)
lasso_cv_deviance <- cv.glmnet(train_data_scaled, train_data$voteBrexit, alpha = 1, family = "binomial")

# Calculate average cross-validated deviance
avg_cv_deviance <- mean(lasso_cv_deviance$cvm)

# Extract coefficients from the lasso model
lasso_coef <- coef(lasso_model, s = lasso_cv_deviance$lambda.min)

# Calculate the number of parameters (including intercept)
n_params <- sum(lasso_coef != 0) + 1  # Add 1 for intercept

# Calculate the log likelihood
log_likelihood <- sum(log(lasso_pred) * test_data$voteBrexit + log(1 - lasso_pred) * (1 - test_data$voteBrexit))

# Calculate AIC
lasso_aic <- -2 * log_likelihood + 2 * n_params

# Calculate BIC
n_obs <- nrow(test_data_scaled)
lasso_bic <- -2 * log_likelihood + log(n_obs) * n_params

cat("Lasso Model Metrics:\n")
cat("AUC:", lasso_auc, "\n")
cat("Accuracy:", lasso_accuracy, "\n")
cat("Average Cross-validated Deviance:", avg_cv_deviance, "\n")
cat("AIC:", lasso_aic, "\n")
cat("BIC:", lasso_bic, "\n")




# Load necessary libraries
library(ggplot2)
library(cowplot)

# Create a function to add a median line to the plot
add_median_line <- function(plot, data, x, y) {
  median_value <- median(data[[x]])
  plot + 
    geom_vline(xintercept = median_value, linetype = "dotted", color = "black") +
    geom_text(aes(x = median_value, y = 0.5, label = round(median_value, 2)), vjust = -1)
}

# Plot 1: Percentage of residents with higher education
plot_edu <- ggplot(test_data, aes(x = withHigherEd, y = withHigherEd, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents with higher education", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot_edu_with_median <- add_median_line(plot_edu, test_data, "withHigherEd", "withHigherEd")

# Plot 2: Percentage of residents with no formal qualifications
plot_qual <- ggplot(test_data, aes(x = notBornUK, y = notBornUK, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents with no formal qualifications", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot_qual_with_median <- add_median_line(plot_qual, test_data, "notBornUK", "notBornUK")

# Plot 3: Median annual income of residents
plot_income <- ggplot(test_data, aes(x = medianIncome, y = medianIncome, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Median annual income of residents", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot_income_with_median <- add_median_line(plot_income, test_data, "medianIncome", "medianIncome")

# Plot 4: Percentage of residents of ABC1 social grade
plot_abc1 <- ggplot(test_data, aes(x = abc1, y = abc1, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents of ABC1 social grade", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot_abc1_with_median <- add_median_line(plot_abc1, test_data, "abc1", "abc1")

# Plot 5: Median age of residents
plot_age <- ggplot(test_data, aes(x = medianAge, y = medianAge, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Median age of residents", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot_age_with_median <- add_median_line(plot_age, test_data, "medianAge", "medianAge")

# Plot 6: Percentage of residents not born in the UK
plot_born <- ggplot(test_data, aes(x = notBornUK, y = notBornUK, color = voteBrexit)) +
  geom_point(alpha = 0.5) +
  labs(x = "Percentage of residents not born in the UK", y = "") +
  scale_color_manual(values = c("yellow", "dodgerblue2"), labels = c("remain", "leave")) +
  theme_minimal()
plot_born_with_median <- add_median_line(plot_born, test_data, "notBornUK", "notBornUK")

# Arrange plots using cowplot package
plot_grid(plot_edu_with_median, plot_qual_with_median, plot_income_with_median, 
          plot_abc1_with_median, plot_age_with_median, plot_born_with_median,
          nrow = 2, ncol = 3)



