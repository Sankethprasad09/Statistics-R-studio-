# Load necessary packages
library(tidyverse)
install.packages("infer")
library(infer)

# Create a population data frame
population <- tibble(
  hip = c(rep("Squeaking", 17500), rep("No squeaking", 232500))
)

# Visualize the population distribution
ggplot(population, aes(x = hip)) + 
  geom_bar() + 
  labs(x = "", y = "Count", title = "Ceramic hip patients that develop squeaking")

# Obtain summary statistics for the population
population_summary <- population %>%
  count(hip) %>%
  mutate(p = n/sum(n))
print(population_summary)

# Question 1: Describe the distribution of squeaking vs. no squeaking in a sample
samp1 <- population %>%
  sample_n(200)

# Visualize and summarize the sample data
samp1_summary <- samp1 %>% 
  count(hip) %>%
  mutate(p_hat = n/sum(n))
print(samp1_summary)

# Question 3: Take a second sample and compare it to the first
samp2 <- population %>%
  sample_n(200)

# Summarize the second sample data
samp2_summary <- samp2 %>% 
  count(hip) %>%
  mutate(p_hat = n/sum(n))
print(samp2_summary)

# Create a sampling distribution of sample proportions
sample_props200 <- population %>%
  rep_sample_n(size = 200, reps = 10000, replace = TRUE) %>%
  count(hip) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(hip == "Squeaking")

# Visualize the sampling distribution
ggplot(sample_props200, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.01) +
  labs(x = "p_hat (Develops squeaking)", title = "Sampling Distribution of p_hat", subtitle = "Sample size = 200, Number of samples = 10,000")

# Question 5: Create a sampling distribution of 10 sample proportions from samples of size 100
sample_props_small <- population %>%
  rep_sample_n(size = 100, reps = 10, replace = TRUE) %>%
  count(hip) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(hip == "Squeaking")
print(sample_props_small)

# Question 9: Calculate probabilities using the theoretical sampling distribution
p <- 0.07  # Population proportion
n <- 200   # Sample size
sd <- sqrt(p * (1 - p) / n)  # Standard deviation of the sampling distribution

# Less than 4%
p_less_4 <- pnorm(0.04, mean = p, sd = sd)
# More than 10%
p_more_10 <- 1 - pnorm(0.10, mean = p, sd = sd)
# Between 5% and 10%
p_between_5_10 <- pnorm(0.10, mean = p, sd = sd) - pnorm(0.05, mean = p, sd = sd)

# Output the probabilities
probabilities <- tibble(
  less_than_4_percent = p_less_4,
  more_than_10_percent = p_more_10,
  between_5_and_10_percent = p_between_5_10
)
print(probabilities)
