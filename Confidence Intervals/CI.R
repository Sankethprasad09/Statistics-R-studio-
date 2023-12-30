# Part 1: Confidence Intervals for a Proportion

# Load necessary packages
library(tidyverse)
library(infer)

# Create the population data frame
population <- tibble(
  climate_change_affects = c(rep("Yes", 310000), rep("No", 190000))
)

# Visualize the distribution of responses
ggplot(population, aes(x = climate_change_affects)) +
  geom_bar() +
  labs(
    x = "", y = "",
    title = "Do you think climate change is affecting your local community?"
  )

# Obtain summary statistics for the population
population_summary <- population %>%
  count(climate_change_affects) %>%
  mutate(p = n / sum(n))
print(population_summary)

# Take a simple random sample of size 180 from the population
n <- 180
samp <- population %>%
  sample_n(size = n)

# Calculate the proportion of the sample that thinks climate change affects their community
samp_summary <- samp %>%
  count(climate_change_affects) %>%
  mutate(p_hat = n / sum(n))
print(samp_summary)

# Question 3: Check the sample size conditions for the Central Limit Theorem
p_hat <- samp_summary$p_hat[which(samp_summary$climate_change_affects == "Yes")]
se <- sqrt(p_hat * (1 - p_hat) / n)

# Conditions: np_hat and n(1-p_hat) must be greater than 10
np_hat <- n * p_hat
n_one_minus_p_hat <- n * (1 - p_hat)
condition_met <- np_hat > 10 & n_one_minus_p_hat > 10

# Critical value for a 90% confidence interval
qnorm(0.95, 0, 1)

# Critical values for other confidence intervals
critical_value_95 <- qnorm(0.975, 0, 1)
critical_value_85 <- qnorm(0.925, 0, 1)
critical_value_99 <- qnorm(0.995, 0, 1)

# Constructing the 95% confidence interval
critical_value <- critical_value_95
lower_95 <- p_hat - (critical_value * se)
upper_95 <- p_hat + (critical_value * se)
confidence_interval_95 <- c(lower_95, upper_95)

# Question 7: Expected number of 95% confidence intervals to capture the true proportion
expected_capture <- 280 * 0.95

# Simulate 10,000 different random samples and construct 95% confidence intervals
critical_value <- qnorm(0.975, 0, 1)

sample_props180 <- population %>%
  rep_sample_n(size = n, reps = 10000, replace = TRUE) %>%
  count(climate_change_affects) %>%
  filter(climate_change_affects == "Yes") %>%
  mutate(p_hat = n / n()) %>%
  mutate(lower = p_hat - (critical_value * sqrt((p_hat * (1 - p_hat)) / n)), 
         upper = p_hat + (critical_value * sqrt((p_hat * (1 - p_hat)) / n))) %>%
  mutate(capture_parameter = (lower <= 0.62 & upper >= 0.62))

# Proportion of intervals that captured the true parameter
proportion_captured <- mean(sample_props180$capture_parameter)

# Part 2: Confidence Interval for a Mean

# Load salinity data
# salinity <- read_csv(file.choose()) # Uncomment this line to run in RStudio

# Assuming the dataset is loaded, continue with the analysis
# Calculate the point estimate for the mean salinity
samp_mean <- mean(salinity$salinity_ppt)

# Calculate the standard error for the mean
samp_sd <- sd(salinity$salinity_ppt)
n <- length(salinity$salinity_ppt)
se_mean <- samp_sd / sqrt(n)

# Determine the critical value for a 90% confidence interval
critical_value_90 <- qt(0.95, df = n - 1)

# Construct the 90% confidence interval for the mean salinity
lower_90 <- samp_mean - (critical_value_90 * se_mean)
upper_90 <- samp_mean + (critical_value_90 * se_mean)
confidence_interval_90 <- c(lower_90, upper_90)

# Interpret the 90% confidence interval
# The interval constructed from the sample suggests that with 90% confidence,
# the true average salinity of the Bimini Lagoon is between `lower_90` and `upper_90` parts per thousand.
