# R code for Discrete Distributions

# Calculate the PMF
values <- c(0, 150, 200, 250, 300, 350, 400, 450, 500, 700, 750, 800, 900, 1000)
counts <- c(2, 2, 4, 2, 2, 1, 3, 1, 2, 1, 1, 1, 1, 1)
probabilities <- counts / sum(counts)

# Expected Value Calculation
expected_value <- sum(values * probabilities)

# Probability of landing on $400 three times
p_400 <- probabilities[which(values == 400)]
prob_400_three_times <- p_400^3

# Probability of landing on $400 at least once in three spins
prob_not_400 <- 1 - p_400
prob_at_least_one_400 <- 1 - prob_not_400^3

# Simulate 1000 spins and calculate the average
set.seed(0)  # For reproducibility
simulated_spins <- sample(values, size = 1000, replace = TRUE, prob = probabilities)
average_simulated_value <- mean(simulated_spins)

# Simulated Probabilities
simulated_counts <- table(simulated_spins)
simulated_probabilities <- simulated_counts / sum(simulated_counts)

# Output the data
data.frame(values, probabilities, simulated_probabilities)

# The action for Question 7 would be to increase the number of simulations.

