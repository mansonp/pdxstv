set.seed(123)  # for reproducibility

# Number of simulated voters
num_voters <- 1000

# Voter ideologies
voters <- data.frame(
  id = 1:num_voters,
  pro_development = sample(c(TRUE, FALSE), num_voters, replace = TRUE),
  pro_social_spending = sample(c(TRUE, FALSE), num_voters, replace = TRUE)
)

# Candidate profiles as preference scores for each voter group
candidate_scores <- data.frame(
  Emily_Nguyen = with(voters, 2*pro_development + 2*pro_social_spending),
  John_Carter = with(voters, 2*(!pro_development) + 2*(!pro_social_spending)),
  Linda_Morales = with(voters, 2*(!pro_development) + 2*pro_social_spending),
  Michael_Thompson = with(voters, 2*pro_development + 2*(!pro_social_spending)),
  Sarah_Goldstein = with(voters, 2*(!pro_development) + 2*(!pro_social_spending)),
  Sam_Mayers = with(voters, 2*pro_development + 2*pro_social_spending),
  Roger_Lin = with(voters, 2*(!pro_development) + 2*pro_social_spending),
  Anna_Schwartz = with(voters, 2*pro_development + 2*(!pro_social_spending))
)

# Create a ranking for each voter based on candidate scores
rankings <- t(apply(candidate_scores, 1, rank))
colnames(rankings) <- colnames(candidate_scores)

# Convert the rankings to a data.frame
rankings_df <- data.frame(id = voters$id, rankings)

# Output the first few rows
head(rankings_df)

# Optionally, write to CSV
write.csv(rankings_df, "voters_preference_rankings.csv", row.names = FALSE)
