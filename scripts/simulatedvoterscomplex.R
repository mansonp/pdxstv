# Bimodal Policy Dimensions
# with coorelation between the dimensions

set.seed(123)  # Ensure reproducibility

# Define number of simulated voters
num_voters <- 1000

# Bimodal distribution parameters for both dimensions
# For pro_environment
mean_env <- c(0.2, 0.8)  # Means for the two peaks
sd_env <- 0.1            # Standard deviation for both peaks

# For pro_social_spending
mean_spending <- c(0.3, 0.7)  # Means for the two peaks
sd_spending <- 0.1            # Standard deviation for both peaks

# Define correlation between the two dimensions
correlation <- 0.5  # Correlation coefficient (e.g., 0.5 for positive correlation)

# Covariance matrix based on specified standard deviations and correlation
cov_matrix <- matrix(c(sd_env^2, correlation*sd_env*sd_spending,
                       correlation*sd_env*sd_spending, sd_spending^2), nrow=2)

# Cholesky decomposition of the covariance matrix
L <- chol(cov_matrix)

# Generate bimodal normal distributions uncorrelated initially
group1 <- mapply(rnorm, n=num_voters/2, mean=mean_env, sd=sd_env)
group2 <- mapply(rnorm, n=num_voters/2, mean=mean_spending, sd=sd_spending)
mix <- rbind(group1, group2)

# Introduce correlation using Cholesky decomposition
correlated_variables <- t(L %*% t(mix))

# Combine the results
voters <- as.data.frame(correlated_variables)
colnames(voters) <- c("pro_environment", "pro_social_spending")

# Shuffle the rows to randomize voter order
voters <- voters[sample(1:nrow(voters)), ]

# Output diagnostics to examine the variables
head(voters)
plot(voters$pro_environment, voters$pro_social_spending, main="Correlation Plot",
     xlab="Pro-Environment", ylab="Pro-Social Spending", pch=19, col=rgb(0.1, 0.3, 0.7, 0.5))

# Re-scale the Data (The correlation decomposition reduced the range)
# Define the scaling function

scale_to_range <- function(vector, new_min, new_max) {
  old_min <- min(vector)
  old_max <- max(vector)
  # Normalize to [0, 1], then scale to [new_min, new_max]
  scaled_vector <- new_min + (new_max - new_min) * ((vector - old_min) / (old_max - old_min))
  return(scaled_vector)
}


# Apply the scaling function to each column
voters$pro_environment <- scale_to_range(voters$pro_environment, -1, 1)
voters$pro_social_spending <- scale_to_range(voters$pro_social_spending, -1, 1)

# Optional diagnostic output to check the results
head(voters)

hist(voters$pro_environment, main="Histogram of Scaled Pro-Development Scores", xlab="Score", breaks=30)
hist(voters$pro_social_spending, main="Histogram of Scaled Pro-Social Spending Scores", xlab="Score", breaks=30)

plot(voters$pro_environment, voters$pro_social_spending, main="Correlation Plot",
     xlab="Pro-Environment", ylab="Pro-Social Spending", pch=19, col=rgb(0.1, 0.3, 0.7, 0.5))

# If the Candidates are available add them to the plot

candidates <- t(weights)
colnames(candidates) <- c("Environment", "Social Spending")

candidates <- as_tibble(candidates)

plot(candidates$Environment, candidates$`Social Spending`)
