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
    Total_num_purchase = NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumStorePurchases,
    Total_accept = AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 + AcceptedCmp1 + AcceptedCmp2 + Response
  )

# Check the first few rows of the updated dataset
head(data)

# Assuming your data is in a data frame named 'data'
# Create a new data frame for the comparison
comparison_data <- data.frame(
  PurchaseType = c("NumWebPurchases", "NumStorePurchases"),
  Total_num_purchase = c(sum(data$NumWebPurchases), sum(data$NumStorePurchases))
)

# Create the bar graph
ggplot(comparison_data, aes(x = PurchaseType, y = Total_num_purchase, fill = PurchaseType)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of Purchases with Total", x = "Purchase Type", y = "Total Purchases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data <- data %>%
  mutate(IncomeCategory = case_when(
    Income <= 30000 ~ "Low Earners",
    Income <= 70000 ~ "Mid Earners",
    TRUE ~ "High Earners"
  ))

# Create the grouped bar plot
ggplot(data, aes(x = IncomeCategory, y = NumWebPurchases, fill = "Web Purchases")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = NumStorePurchases, fill = "Store Purchases"), stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of Web and Store Purchases by Income Category", x = "Income Category", y = "Number of Purchases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Web Purchases" = "blue", "Store Purchases" = "red"))

# Function to create horizontal bar plots for each income category
create_horizontal_bar_plot <- function(category) {
  data %>%
    filter(IncomeCategory == category) %>%
    pivot_longer(cols = c(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds),
                 names_to = "Product",
                 values_to = "Amount") %>%
    ggplot(aes(x = Amount, y = Product, fill = Product)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Spending on Products for", category), x = "Amount Spent", y = "Product") +
    theme(axis.text.y = element_text(angle = 45, hjust = 1))
}

# Display the horizontal bar plot for low earners
create_horizontal_bar_plot("Low Earners")

# Display the horizontal bar plot for low earners
create_horizontal_bar_plot("Mid Earners")

# Display the horizontal bar plot for low earners
create_horizontal_bar_plot("High Earners")

# Calculate the total amount spent on each product category for each Minorhome class
product_data <- data %>%
  group_by(Minorhome) %>%
  summarise(
    MntWines = sum(MntWines),
    MntFruits = sum(MntFruits),
    MntMeatProducts = sum(MntMeatProducts),
    MntFishProducts = sum(MntFishProducts),
    MntSweetProducts = sum(MntSweetProducts),
    MntGoldProds = sum(MntGoldProds)
  ) %>%
  pivot_longer(cols = -Minorhome, names_to = "Product", values_to = "Amount") %>%
  group_by(Minorhome, Product) %>%
  summarise(Total_Amount = sum(Amount)) %>%
  mutate(Ratio = Total_Amount / sum(Total_Amount))

# Create the bar plot
ggplot(product_data, aes(x = Minorhome, y = Ratio, fill = Product)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Product Purchases by Minorhome", x = "Minorhome", y = "Ratio of Product Purchases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prepare the data for plotting
plot_data <- data %>%
  mutate(PurchaseType = case_when(
    NumCatalogPurchases > 0 ~ "Catalog Purchases",
    NumDealsPurchases > 0 ~ "Deals Purchases",
    TRUE ~ "Other"
  )) %>%
  group_by(PurchaseType) %>%
  summarise(Total_num_purchase = sum(Total_num_purchase))

# Create the grouped bar plot
ggplot(plot_data, aes(x = PurchaseType, y = Total_num_purchase, fill = PurchaseType)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Total Number of Purchases by Purchase Type", x = "Purchase Type", y = "Total Number of Purchases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram for Total_Mnt
ggplot(data, aes(x = Total_Mnt)) +
  geom_histogram(binwidth = 100, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of Total Amount Spent", x = "Total Amount Spent", y = "Frequency") +
  theme_minimal()

# Calculate the ratio of web purchases to store purchases for each education category
plot_data <- data %>%
  mutate(EducationCategory = case_when(
    `education_2n Cycle` == 1 ~ "2n Cycle",
    education_Basic == 1 ~ "Basic",
    education_Graduation == 1 ~ "Graduation",
    education_Master == 1 ~ "Master",
    education_PhD == 1 ~ "PhD",
    TRUE ~ "Other"
  )) %>%
  group_by(EducationCategory) %>%
  summarise(
    Total_num_purchase = sum(Total_num_purchase),
    Web_to_Store_Ratio = sum(NumWebPurchases) / sum(NumStorePurchases)
  )

# Create the bar plot
ggplot(plot_data, aes(x = EducationCategory, y = Total_num_purchase, fill = Web_to_Store_Ratio)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Total Number of Purchases by Education Category and Web vs Store Ratio", x = "Education Category", y = "Total Number of Purchases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "blue", high = "red", name = "Web to Store Ratio")

# Function to create a line chart for a specific type of purchase
create_purchase_chart <- function(data, purchase_type) {
  # Filter data for the specific purchase type
  filtered_data <- data %>%
    filter(!!sym(purchase_type) > 0) %>%
    group_by(Age) %>%
    summarise(Total_Purchases = sum(!!sym(purchase_type)))
  
  # Create the line chart
  ggplot(filtered_data, aes(x = Age, y = Total_Purchases)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Total Purchases by Age for", purchase_type), x = "Age", y = "Total Purchases") +
    theme_minimal()
}

# Call the function to create line charts for each type of purchase
web_purchases_chart <- create_purchase_chart(data, "NumWebPurchases")
store_purchases_chart <- create_purchase_chart(data, "NumStorePurchases")
catalogue_purchases_chart <- create_purchase_chart(data, "NumCatalogPurchases")
deals_purchases_chart <- create_purchase_chart(data, "NumDealsPurchases")

# Display the charts
print(web_purchases_chart)
print(store_purchases_chart)
print(catalogue_purchases_chart)
print(deals_purchases_chart)

# What factors are significantly related to the number of store purchases?

# OLS regression

# OLS regression for store purchases
model_store <- lm(NumStorePurchases ~ Income + Age + marital_Married + education_PhD, data = data)

# Summary of the store purchases model
summary(model_store)

# Check for multicollinearity using Variance Inflation Factor (VIF) for store purchases
vif(model_store)

# Assuming you have variables for web, catalogue, and deals purchases similar to store purchases
# OLS regression for web purchases
model_web <- lm(NumWebPurchases ~ Income + Age + marital_Married + education_PhD, data = data)

# Summary of the web purchases model
summary(model_web)

# Check for multicollinearity using VIF for web purchases
vif(model_web)

# Repeat the process for catalogue and deals purchases
# OLS regression for catalogue purchases
model_catalogue <- lm(NumCatalogPurchases ~ Income + Age + marital_Married + education_PhD, data = data)

# Summary of the catalogue purchases model
summary(model_catalogue)

# Check for multicollinearity using VIF for catalogue purchases
vif(model_catalogue)

# OLS regression for deals purchases
model_deals <- lm(NumDealsPurchases ~ Income + Age + marital_Married + education_PhD, data = data)

# Summary of the deals purchases model
summary(model_deals)

# Check for multicollinearity using VIF for deals purchases
vif(model_deals)


# Fixed effects regression model

# Create a simple ID column based on the row number
data$ID <- row.names(data)

# Create a simple time index column based on the row number
data$Time <- row.names(data)

# Convert the data frame to a pdata.frame
data_pdata <- pdata.frame(data, index = c("ID", "Time"))

# Perform the fixed effects regression model for store purchases
model_fixed_store <- plm(NumStorePurchases ~ Income + Age + marital_Married + education_PhD + 
                           Minorhome + Recency + Total_Mnt + Total_num_purchase + Total_accept + AOV, 
                         data = data_pdata, model = "within")

# Summary of the store purchases fixed effects model
summary(model_fixed_store)

# Assuming you have variables for web, catalogue, and deals purchases similar to store purchases
# Perform the fixed effects regression model for web purchases
model_fixed_web <- plm(NumWebPurchases ~ Income + Age + marital_Married + education_PhD + 
                         Minorhome + Recency + Total_Mnt + Total_num_purchase + Total_accept + AOV, 
                       data = data_pdata, model = "within")

# Summary of the web purchases fixed effects model
summary(model_fixed_web)

# Repeat the process for catalogue and deals purchases
# Perform the fixed effects regression model for catalogue purchases
model_fixed_catalogue <- plm(NumCatalogPurchases ~ Income + Age + marital_Married + education_PhD + 
                               Minorhome + Recency + Total_Mnt + Total_num_purchase + Total_accept + AOV, 
                             data = data_pdata, model = "within")

# Summary of the catalogue purchases fixed effects model
summary(model_fixed_catalogue)

# Perform the fixed effects regression model for deals purchases
model_fixed_deals <- plm(NumDealsPurchases ~ Income + Age + marital_Married + education_PhD + 
                           Minorhome + Recency + Total_Mnt + Total_num_purchase + Total_accept + AOV, 
                         data = data_pdata, model = "within")

# Summary of the deals purchases fixed effects model
summary(model_fixed_deals)




# Replace spaces and other invalid characters in column names with underscores
names(data) <- make.names(names(data))

# Perform a conditional correlation test for store purchases
cor_test(data, "NumStorePurchases", "Income", partial = TRUE, 
         partial_vars = c("Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between NumStorePurchases and MntTotal
cor_test(data, "NumStorePurchases", "MntTotal", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between Minorhome and Total_Mnt
cor_test(data, "Minorhome", "Total_Mnt", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test between Total_num_purchase and Total_accept
cor_test(data, "Total_num_purchase", "Total_accept", partial = TRUE, 
         partial_vars = c("Income", "Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test for web purchases
cor_test(data, "NumWebPurchases", "Income", partial = TRUE, 
         partial_vars = c("Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test for catalogue purchases
cor_test(data, "NumCatalogPurchases", "Income", partial = TRUE, 
         partial_vars = c("Age", "marital_Married", "education_PhD"))

# Perform a conditional correlation test for deals purchases
cor_test(data, "NumDealsPurchases", "Income", partial = TRUE, 
         partial_vars = c("Age", "marital_Married", "education_PhD"))


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

# Create a contingency table for NumStorePurchases and Total_num_purchase_group
contingency_table_store <- table(data$NumStorePurchases, data$Total_num_purchase_group)

# Perform Chi-square test of independence for store purchases
chisq_test_store <- chisq.test(contingency_table_store)

# Print the results
print(chisq_test_store)

# Create a contingency table for NumWebPurchases and Total_num_purchase_group
contingency_table_web <- table(data$NumWebPurchases, data$Total_num_purchase_group)

# Perform Chi-square test of independence for web purchases
chisq_test_web <- chisq.test(contingency_table_web)

# Print the results
print(chisq_test_web)

# Assuming you have variables for other variables similar to store and web purchases
# Create a contingency table for Income and Total_num_purchase_group
contingency_table_income <- table(data$Income, data$Total_num_purchase_group)

# Perform Chi-square test of independence for Income
chisq_test_income <- chisq.test(contingency_table_income)

# Print the results
print(chisq_test_income)

# Create a contingency table for Age and Total_num_purchase_group
contingency_table_age <- table(data$Age, data$Total_num_purchase_group)

# Perform Chi-square test of independence for Age
chisq_test_age <- chisq.test(contingency_table_age)

# Print the results
print(chisq_test_age)

# Create a contingency table for marital_Married and Total_num_purchase_group
contingency_table_marital <- table(data$marital_Married, data$Total_num_purchase_group)

# Perform Chi-square test of independence for marital_Married
chisq_test_marital <- chisq.test(contingency_table_marital)

# Print the results
print(chisq_test_marital)

# Create a contingency table for Minorhome and Total_num_purchase_group
contingency_table_minor <- table(data$Minorhome, data$Total_num_purchase_group)

# Perform Chi-square test of independence for Minorhome
chisq_test_minor <- chisq.test(contingency_table_minor)

# Print the results
print(chisq_test_minor)

# Create a contingency table for Recency and Total_num_purchase_group
contingency_table_recency <- table(data$Recency, data$Total_num_purchase_group)

# Perform Chi-square test of independence for Recency
chisq_test_recency <- chisq.test(contingency_table_recency)

# Print the results
print(chisq_test_recency)

# Create a contingency table for Total_Mnt and Total_num_purchase_group
contingency_table_mnt <- table(data$Total_Mnt, data$Total_num_purchase_group)

# Perform Chi-square test of independence for Total_Mnt
chisq_test_mnt <- chisq.test(contingency_table_mnt)

# Print the results
print(chisq_test_mnt)