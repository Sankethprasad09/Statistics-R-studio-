# Load the openintro package
library(openintro)

# Load the present dataset
data(present)

# Display the range of years in the dataset
years_range <- range(present$year)
print(paste("Years range:", years_range[1], "to", years_range[2]))

# Display the dimensions of the dataset
dimensions <- dim(present)
print(paste("Dimensions: Rows =", dimensions[1], "Columns =", dimensions[2]))

# Display the variable names
column_names <- names(present)
print("Column names:")
print(column_names)

# Calculate the proportion of boys and plot it
present$prop_boys <- present$boys / (present$boys + present$girls)
plot(present$year, present$prop_boys, type = "l", 
     xlab = "Year", ylab = "Proportion of Boys", 
     main = "Proportion of Boys Born Over Time")

# Load the dplyr package for data manipulation
library(dplyr)

# Calculate total births and arrange in descending order
sorted_present <- present %>%
  mutate(total = boys + girls) %>%
  arrange(desc(total))

# Display the year with the most total number of births
max_birth_year <- sorted_present$year[1]
max_births <- sorted_present$total[1]
print(paste("Year with most births:", max_birth_year, "Total births:", max_births))
