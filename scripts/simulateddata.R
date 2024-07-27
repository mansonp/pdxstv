# This simulated data has two simulated dimensions. The candidates have manually set.
# The voters are randomly distributed on these dimensions


set.seed(123)  # Ensure reproducibility

# Define number of simulated voters
num_voters <- 1000

# Generate continuous voter ideologies within the range [0, 1]
voters <- data.frame(
  pro_environment = runif(num_voters, -1, 1),
  pro_social_spending = runif(num_voters, -1, 1)
)

# Define candidate weights with decimal values for ideological alignment, 2 (traits) x 8 (candidates)
weights <- matrix(
  c(-.99, -0.80, -0.3, 0.6, -0.6, 0.5, -0.6, 0.99,    # weights for pro_environment 1= environment -1 = development
    0.99, 0.80, 0.8, -0.6, -0.2, 0.45, 0.5, -0.99),    # weights for pro_social_spending 1= Social Spending -1 = Antitax
  nrow = 2, byrow = TRUE
)
colnames(weights) <- c("Emily_Nguyen", "John_Carter", "Linda_Morales", "Michael_Thompson",
                       "Sarah_Goldstein", "Sam_Mayers", "Roger_Lin", "Anna_Schwartz")

# Compute candidate alignment scores
candidate_scores <- as.matrix(voters) %*% weights

# Scale scores between 0 and 1 by normalizing with the maximum possible score
# Compute max possible score by summing absolute values of weights for each trait
max_score <- rowSums(abs(weights))
normalized_scores <- candidate_scores / max_score

# Introduce noise to simulate evaluation uncertainty
noise_std_dev <- 0.1  # Standard deviation of noise
noisy_scores <- normalized_scores + matrix(rnorm(num_voters * ncol(normalized_scores), mean = 0, sd = noise_std_dev),
                                           nrow = num_voters, ncol = ncol(normalized_scores))


# Clipping scores to keep them within [0, 1] while maintaining matrix structure
noisy_scores <- apply(noisy_scores, c(1, 2), function(x) pmin(1, pmax(0, x)))

# Apply rankings across each row
rankings <- t(apply(noisy_scores, 1, function(x) rank(x, ties.method = "random")))

# Provide proper column names based on the candidates
colnames(rankings) <- colnames(weights)

# Create final data frame for analysis or output
rankings_df <- data.frame(id = 1:num_voters, rankings)

# Displaying a snippet of the final dataset
head(rankings_df)

# Writing to CSV, if needed
write.csv(rankings_df, "voters_continuous_preference_rankings.csv", row.names = FALSE)
