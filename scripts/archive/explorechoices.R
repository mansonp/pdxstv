# This script lets you dig into choices outside the `vote` package to understand dynamics

# Load the tidyverse package
library(tidyverse)


# Analysis using tidyverse
rank_matrix_tbl <- as_tibble(rank_matrix)

result <- rank_matrix_tbl %>% 
  # Filter rows where Michael_Thompson is the first pick
  filter(Linda_Morales == 1) %>%
  # Gather all columns into key-value pairs
  pivot_longer(cols = -Linda_Morales) %>%
  # Filter down to only those instances where 'value' == 2
  filter(value == 2) %>%
  # Count the frequency of each candidate being selected as second choice
  count(name) %>%
  # Calculate the percentages
  mutate(percentage = n / sum(n) * 100)

# Print the result
print(result)


