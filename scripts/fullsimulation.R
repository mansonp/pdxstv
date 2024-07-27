######
#
# STV and Sythetic Voter Data
#
# Goal for this script is to create a sythetic voter set of preferences to test in STV.
# The voters are created on two policy dimensions from pro-environment to pro-development, and
# pro-social spending to anti-tax. These dimensions are correlated moderately to simulate the 
# interaction between these two policy positions.
#
# The STV tabulation is done with the `vote` package and allows for tabulationa and work with 
# synthetic CVR.
#
#
# Created July 2024 by Paul Manson mansonp@pdx.edu
#
#####



# Install Packages

library(pacman) 
p_load(vote, dplyr, ggplot2) # Note to other users, p_load will install if not installed any package listed


# SET UP VOTERS

# Bimodal Policy Dimensions
# with correlation between the dimensions

set.seed(123)  # Ensure reproducibility

# Define number of simulated voters
num_voters <- 100000

# Bimodal distribution parameters for both dimensions
# For pro_environment

# The idea here is that there is a general pro-environment prespecive (.7) but some skeptics (.4)
mean_env <- c(0.4, 0.7)  # Means for the two peaks
sd_env <- 0.05            # Standard deviation for both peaks

# For pro_social_spending
# Here the division is larger, with a strong pro-camp and a diffuse but real anti camp

mean_spending <- c(0.35, 0.9)  # Means for the two peaks
sd_spending <- 0.04            # Standard deviation for both peaks

# Define correlation between the two dimensions
# This is done with the idea that pro-social spending and pro-enviromnent move in line, but not totally

correlation <- 0.5  # Correlation coefficient (e.g., 0.5 for positive correlation)

# Covariance matrix based on specified standard deviations and correlation
cov_matrix <- matrix(c(sd_env^2, correlation*sd_env*sd_spending,
                       correlation*sd_env*sd_spending, sd_spending^2), nrow=2)

# Cholesky decomposition of the covariance matrix - this allows us to pull out the elements need to simulate
L <- chol(cov_matrix)

# Generate bimodal normal distributions uncorrelated initially
group1 <- mapply(rnorm, n=num_voters/2, mean=mean_env, sd=sd_env)
group2 <- mapply(rnorm, n=num_voters/2, mean=mean_spending, sd=sd_spending)
mix <- rbind(group1, group2)

# Introduce correlation using the Cholesky decomposition from above (L)
# Creates the voter preferences on the two dimensions
correlated_variables <- t(L %*% t(mix))

# Combine the results
voters <- as.data.frame(correlated_variables)
colnames(voters) <- c("pro_environment", "pro_social_spending")

# Re-scale the data (The correlation decomposition reduced the range between min-max)
# Define the scaling function

scale_to_range <- function(vector, new_min, new_max) {
  old_min <- min(vector)
  old_max <- max(vector)
  # Normalize to [0, 1], then scale to [new_min, new_max]
  scaled_vector <- new_min + (new_max - new_min) * ((vector - old_min) / (old_max - old_min))
  return(scaled_vector)
}


# Apply the scaling function to each column from -1 to 1 on the two dimensions
voters$pro_environment <- scale_to_range(voters$pro_environment, -1, 1)
voters$pro_social_spending <- scale_to_range(voters$pro_social_spending, -1, 1)

# Optional diagnostic output to check the results
head(voters)

hist(voters$pro_environment, main="Histogram of Scaled Pro-Environment Scores", xlab="Score", breaks=30)
hist(voters$pro_social_spending, main="Histogram of Scaled Pro-Social Spending Scores", xlab="Score", breaks=30)

plot(voters$pro_environment, voters$pro_social_spending, main="Correlation Plot",
     xlab="Pro-Environment", ylab="Pro-Social Spending", pch=19, col=rgb(0.1, 0.3, 0.7, 0.5))

## SETUP CANDIDATES

# Define candidate weights with decimal values for ideological alignment, 2 (dimensions) x 8 (candidates)
weights <- matrix(
  c(0.95, 0.80, -0.3, 0.6, -0.6, 0.5, -0.6, -0.95,    # weights for pro_environment 1= environment -1 = development
    0.95, 0.70, 0.2, -0.2, -0.2, 0.45, 0.5, -0.95),    # weights for pro_social_spending 1= Social Spending -1 = Antitax
  nrow = 2, byrow = TRUE
)
colnames(weights) <- c("Emily_Nguyen", "John_Carter", "Linda_Morales", "Michael_Thompson",
                       "Sarah_Goldstein", "Sam_Mayers", "Roger_Lin", "Anna_Schwartz")


# CREATE CANDIDATE RANKINGS

# Compute candidate alignment scores
candidate_names <- colnames(weights)
candidates <- as.data.frame(t(weights))
colnames(candidates) <- c("pro_environment", "pro_social_spending")

# Function to calculate Euclidean distance for a given voter row against all candidates
calculate_distances <- function(voter_row, candidate_df) {
  # Computing Euclidean distances
  apply(candidate_df, 1, function(candidate_row) {
    sqrt(sum((candidate_row - voter_row) ^ 2))
  })
}

# Apply the Euclidiean distance function to each voter for each candidate
distance_matrix <- t(apply(voters, 1, calculate_distances, candidates))

# Transform distances into rankings
# Here we inverse the standard ranking to make small distances have the smallest rank number
rank_matrix <- apply(distance_matrix, 1, function(x) rank(x, ties.method = "first"))

rank_matrix <- t(rank_matrix)

# Assign names to rows and columns for clarity
rownames(rank_matrix) <- rownames(voters)
colnames(rank_matrix) <- candidate_names

# Print the rank matrix
head(rank_matrix)


# Output data if needed

# Writing to CSV, if needed
# write.csv(rank_matrix, "voters_continuous_preference_rankings.csv", row.names = FALSE)


# TABULATE WINNERS

# Plurality Contest (FPT)
rankings_df_plural <- rank_matrix
rankings_df_plural <- 1 * (rankings_df_plural == 1 & !is.na(rankings_df_plural))
plurality(rankings_df_plural)

# STV with 3 Member Districts
rankings_df_processed <- rank_matrix

# NOTE on STV Settings:
# seats = number per race
# eps= value added to Droop quota, 
# tie = forward or backward tie breakrule
# constant.quota = the quota can decrease as candidates are eliminated, here we set to constant

# More on tie breaking here:
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=886203 

stvresults <- stv(rankings_df_processed, 
                  nseats=3, 
                  eps = 1, 
                  ties= "f",
                  digits = 0, 
                  constant.quota = TRUE)

#VISUALIZE
candidates_display <- tibble::rownames_to_column(candidates, "Name")
candidates_win <- candidates_display %>% filter(Name %in% stvresults$elected)

ggplot(voters, aes(x=pro_environment, y=pro_social_spending)) + 
  geom_point(alpha=.4) + 
  geom_point(data=candidates,  aes(x=pro_environment, y=pro_social_spending), color='darkblue', size=5) +
  geom_point(data=candidates_win, aes(x=pro_environment, y=pro_social_spending), color='red', size=3) +
  xlab("Pro-Development to Pro-Environment Dimension") +
  ylab("Anti-Tax to Increased Social Spending Dimension")
  
plot(stvresults)

