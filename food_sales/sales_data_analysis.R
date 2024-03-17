library(dplyr)
library(readr)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(tidyr)
library(lubridate)
library(tidyverse)
library(lmtest)
library(car)
library(plm)
library(correlation)
library(ppcor)

# Read the dataset from the current working directory
data <- read_csv("C:/Users/shubh/Desktop/R-basic/food_sales/dataset.csv")

# List basic statistics about the data
summary(data)

# List five-number summary (minimum, lower-hinge, median, upper-hinge, maximum) for each numeric column
numeric_columns <- sapply(data, is.numeric)
numeric_data <- data[, numeric_columns]

# Ensure the fivenum_results variable is correctly created and assigned
fivenum_results <- apply(numeric_data, 2, fivenum)

# Print the fivenum_results
print(fivenum_results)

# Detailed descriptive statistics using Hmisc package
describe(data)

str(data)

column_names <- colnames(data)
print(column_names)


# Select relevant columns for visualization
selected_data <- data %>%
  dplyr::select(Income, Kidhome, Teenhome, Recency, MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds)

# Reshape data to long format
long_data <- selected_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")


# Create box plots for selected columns
ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(title = "Box Plots for Selected Columns",
       x = "Variable",
       y = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Create new features
data <- data %>%
  mutate(
    Minorhome = Kidhome + Teenhome,
    Total_Mnt = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds,
    Total_num_purchase = NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumStorePurchases + NumWebVisitsMonth,
    Total_accept = AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + AcceptedCmp1 + AcceptedCmp2 + Response,
    AOV = Total_Mnt / Total_num_purchase
  )

# Check the first few rows of the updated dataset
head(data)

# Histogram for Minorhome
ggplot(data, aes(x = Minorhome)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of Minorhome", x = "Number of Minors in Household", y = "Frequency") +
  theme_minimal()

# Box Plot for Total_Mnt
ggplot(data, aes(x = "", y = Total_Mnt)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  labs(title = "Box Plot of Total_Mnt", x = "", y = "Total Amount Spent") +
  theme_minimal()

# Histogram for Total_Mnt
ggplot(data, aes(x = Total_Mnt)) +
  geom_histogram(binwidth = 100, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of Total Amount Spent", x = "Total Amount Spent", y = "Frequency") +
  theme_minimal()

# Box Plot for Total_num_purchase
ggplot(data, aes(x = "", y = Total_num_purchase)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  labs(title = "Box Plot of Total Number of Purchases", x = "", y = "Total Number of Purchases") +
  theme_minimal()

# Histogram for Total_accept
ggplot(data, aes(x = Total_accept)) +
  geom_histogram(binwidth = 1, fill = "green", alpha = 0.5) +
  labs(title = "Histogram of Total Acceptance", x = "Total Acceptance", y = "Frequency") +
  theme_minimal()

# Box Plot for Average Order Volume (AOV)
ggplot(data, aes(x = "", y = AOV)) +
  geom_boxplot(fill = "purple", outlier.color = "red") +
  labs(title = "Box Plot of Average Order Volume", x = "", y = "Average Order Volume") +
  theme_minimal()

# Bar Plot for Total Purchases by Income
ggplot(data, aes(x = Income, y = Total_num_purchase)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Purchases by Income",
       x = "Income",
       y = "Total Purchases")

# Line Chart for Total Purchases by Age Group
ggplot(data, aes(x = Age, y = Total_num_purchase, group = Age)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Purchases by Age Group",
       x = "Age Group",
       y = "Total Purchases")

# Bar Plot for Total Purchases by Marketing Campaigns Acceptance
ggplot(data, aes(x = Total_accept, y = Total_num_purchase, fill = Total_accept)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Purchases by Marketing Campaigns Acceptance",
       x = "Marketing Acceptance",
       y = "Total Purchases")

# Bar Plot for Total Purchases by Education Level
data$education_level <- factor(data$education_Basic + data$education_Graduation + data$education_Master + data$education_PhD,
                               levels = c(0, 1),
                               labels = c("No Higher Education", "Higher Education"))

ggplot(data, aes(x = education_level, y = Total_num_purchase, fill = education_level)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Total Purchases by Education Level",
       x = "Education Level",
       y = "Total Purchases",
       fill = "Education Level") +
  theme_minimal()

# Bar Plot for Total Purchases by Number of Web Visits
ggplot(data, aes(x = NumWebVisitsMonth, y = Total_num_purchase)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Purchases by Number of Web Visits",
       x = "Number of Web Visits",
       y = "Total Purchases")

# Bar Plot for Total Purchases by Complain
ggplot(data, aes(x = Complain, y = Total_num_purchase, fill = Complain)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Purchases by Complain",
       x = "Complain",
       y = "Total Purchases",
       fill = "Complain") +
  theme_minimal()

# Bar Plot for Total Purchases by Recency
ggplot(data, aes(x = Recency, y = Total_num_purchase, fill = Recency)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Purchases by Recency",
       x = "Recency",
       y = "Total Purchases",
       fill = "Recency") +
  theme_minimal()

# What factors are significantly related to the number of store purchases?

# OLS regression
model <- lm(NumStorePurchases ~ Income + Age + marital_Married + education_PhD, data = data)

# Summary of the model
summary(model)

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(model)

# Extended OLS regression model with more variables
model_extended <- lm(NumStorePurchases ~ Income + Age + marital_Married + education_PhD + 
                       Minorhome + Recency + Total_Mnt + Total_num_purchase + Total_accept + AOV, 
                     data = data)

# Summary of the extended model
summary(model_extended)

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(model_extended)

# Fixed effects regression model

# Create a simple ID column based on the row number
data$ID <- row.names(data)

# Create a simple time index column based on the row number
data$Time <- row.names(data)

# Convert the data frame to a pdata.frame
data_pdata <- pdata.frame(data, index = c("ID", "Time"))

# Perform the fixed effects regression model
model_fixed <- plm(NumStorePurchases ~ Income + Age + marital_Married + education_PhD + 
  Minorhome + Recency + Total_Mnt + Total_num_purchase + Total_accept + AOV, 
                    data = data_pdata, model = "within")

# Replace spaces and other invalid characters in column names with underscores
names(data) <- make.names(names(data))

# Perform a conditional correlation test
cor_test(data, "Income", "NumStorePurchases", partial = TRUE, 
         partial_vars = c("Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between NumStorePurchases and MntTotal
cor_test(data, "NumStorePurchases", "MntTotal", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between NumStorePurchases and AOV
cor_test(data, "NumStorePurchases", "AOV", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between Minorhome and Total_Mnt
cor_test(data, "Minorhome", "Total_Mnt", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between Total_num_purchase and Total_accept
cor_test(data, "Total_num_purchase", "Total_accept", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between Total_Mnt and AOV
cor_test(data, "Total_Mnt", "AOV", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Apply find_mode function to each column in the dataset
modes <- sapply(data, find_mode)

# Print the modes
print(modes)

# Calculate pairwise partial correlations
pcor_results <- pcor(data)

# Print the results
print(pcor_results)

# Calculate correlation matrix
cor_matrix <- cor(data)

# Melt the correlation matrix to long format for ggplot
cor_melted <- melt(cor_matrix)

# Create a heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Variable", fill = "Correlation")

# Performing Chi-square Tests

# Categorize Total_num_purchase into groups
data$Total_num_purchase_group <- cut(data$Total_num_purchase, breaks = quantile(data$Total_num_purchase, probs = c(0, 0.33, 0.66, 1)), labels = c("Low", "Medium", "High"), include.lowest = TRUE)

# Create a contingency table for Income and Total_num_purchase_group
contingency_table <- table(data$Income, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_income <- chisq.test(contingency_table)

# Print the results
print(chisq_test_income)

# Create a contingency table for Age and Total_num_purchase_group
contingency_table <- table(data$Age, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_Age <- chisq.test(contingency_table)

# Print the results
print(chisq_test_Age)

# Create a contingency table for marital_Married and Total_num_purchase_group
contingency_table <- table(data$marital_Married, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_marital <- chisq.test(contingency_table)

# Print the results
print(chisq_test_marital)

# Create a contingency table for Minorhome and Total_num_purchase_group
contingency_table <- table(data$Minorhome, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_minor <- chisq.test(contingency_table)

# Print the results
print(chisq_test_minor)

# Create a contingency table for Recency and Total_num_purchase_group
contingency_table <- table(data$Recency, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_recency <- chisq.test(contingency_table)

# Print the results
print(chisq_test_recency)

# Create a contingency table for Total_Mnt and Total_num_purchase_group
contingency_table <- table(data$Total_Mnt, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_mnt <- chisq.test(contingency_table)

# Print the results
print(chisq_test_mnt)

# Create a contingency table for Total_accept and Total_num_purchase_group
contingency_table <- table(data$Total_accept, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_accept <- chisq.test(contingency_table)

# Print the results
print(chisq_test_accept)

# Create a contingency table for AOV and Total_num_purchase_group
contingency_table <- table(data$AOV, data$Total_num_purchase_group)

# Perform Chi-square test of independence
chisq_test_AOV <- chisq.test(contingency_table)

# Print the results
print(chisq_test_AOV)
