# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
loan_data <- read.csv("loan50.csv")

# Part a: Histogram of total credit limit
ggplot(loan_data, aes(x = total_credit_limit)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Histogram of Total Credit Limit", x = "Total Credit Limit", y = "Frequency")

# Part b: Boxplot of total credit limit
ggplot(loan_data, aes(y = total_credit_limit)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Total Credit Limit", x = "", y = "Total Credit Limit")

# Part d: Calculate the mean of the total credit limit
mean_credit_limit <- mean(loan_data$total_credit_limit)
print(paste("Mean of total credit limit:", mean_credit_limit))

# Part e: Calculate the median of the total credit limit
median_credit_limit <- median(loan_data$total_credit_limit)
print(paste("Median of total credit limit:", median_credit_limit))

# Part g: Calculate the standard deviation of the total credit limit
sd_credit_limit <- sd(loan_data$total_credit_limit)
print(paste("Standard deviation of total credit limit:", sd_credit_limit))

# Part h: Calculate the interquartile range of the total credit limit
iqr_credit_limit <- IQR(loan_data$total_credit_limit)
print(paste("Interquartile range of total credit limit:", iqr_credit_limit))

# Part i: Side-by-side boxplot for total credit limit by homeownership
ggplot(loan_data, aes(x = homeownership, y = total_credit_limit, fill = homeownership)) +
  geom_boxplot() +
  labs(title = "Total Credit Limit by Homeownership", x = "Homeownership", y = "Total Credit Limit")

# Part k: Table of counts for the loan purpose variable
loan_purpose_counts <- table(loan_data$loan_purpose)
print("Counts for each loan purpose:")
print(loan_purpose_counts)

# Part l: Table of proportions for the loan purpose variable
loan_purpose_proportions <- prop.table(loan_purpose_counts)
print("Proportions for each loan purpose:")
print(loan_purpose_proportions)

# Part m: Barplot for the distribution of loan purpose types
barplot(loan_purpose_counts, las = 2, col = rainbow(length(loan_purpose_counts)),
        main = "Distribution of Loan Purpose Types",
        xlab = "Loan Purpose", ylab = "Frequency")
