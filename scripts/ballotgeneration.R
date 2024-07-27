# Ballot Generation
library(pacman)
p_load(here, vote)

# Fully Random Ballots


set.seed(123)  # Set seed for reproducibility

# Create function to generate random preferences for one row
generate_preferences <- function(candidates) {
  selected_candidates <- sample(candidates, 6)
  preferences <- integer(length(candidates))
  names(preferences) <- candidates
  
  # Random preferences for the selected candidates
  prefs <- sample(1:6)
  preferences[selected_candidates] <- prefs
  
  return(preferences)
}

# Define the candidates and initialize data frame
candidates <- c("Chinook", "DouglasFir", "ThunderEgg", "Filbert", "Chanterelle", "Dungeness", "Pear", "Potato")
randomballot <- data.frame(matrix(ncol = length(candidates), nrow = 100))
names(randomballot) <- candidates

# Populate data frame with randomly generated preferences
for (i in 1:100) {
  randomballot[i, ] <- generate_preferences(candidates)
}

# Add BallotID as a unique identifier
randomballot$BallotID <- sprintf("BLT_%03d", 1:nrow(randomballot))

# Assign precincts; roughly a quarter each
randomballot$Precinct <- sample(rep(c(110, 111, 112, 113), each = 25), 100)

# Write to CSV
write.csv(randomballot, here("data", "preferences_simulation.csv"), row.names = FALSE)



# Weighted Random (First Pick Only)

# Here I have set a preference in the overall outcomes to push Chinook past the threshold in round 1, but then Filbert and Dungeness close behind.

set.seed(123)  # For consistent results

# Helper function to generate preferences where 6 out of 8 candidates are ranked
generate_preferences <- function(candidates, weights, num_rows) {
  total_prefs <- length(candidates)
  # Initialize the preferences matrix filled with zeros
  prefs <- matrix(0, nrow = num_rows, ncol = total_prefs)
  colnames(prefs) <- candidates
  
  for (i in 1:num_rows) {
    # Randomly pick 6 candidates to assign rankings
    ranked_candidates <- sample(candidates, 6)
    
    # Calculate probabilities for 6 ranked candidates based on weights
    probs <- weights[match(ranked_candidates, candidates)]
    probs <- ifelse(is.na(probs), 0, probs)
    total_probs <- sum(probs)
    if (total_probs > 0) {
      probs <- probs / total_probs
    } else {
      probs <- rep(1/length(probs), length(probs))  # Equal probability if total_probs is zero
    }
    
    # Assign '1' to one weighted candidate
    first_pick <- sample(ranked_candidates, 1, prob = probs)
    prefs[i, first_pick] <- 1
    
    # Assign remaining preferences 2 to 6 to other candidates
    remaining_candidates <- setdiff(ranked_candidates, first_pick)
    prefs[i, remaining_candidates] <- sample(2:6)
  }
  
  return(prefs)
}

# Candidates and target weight percentages for '1'
candidates <- c("Chinook", "DouglasFir", "ThunderEgg", "Filbert", "Chanterelle", "Dungeness", "Pear", "Potato") # Adjust here for more or less, make sure next line matches in number and count.
weights <- c(30, 10, 2, 20, 3, 15, 10, 10)  # Weights as integers for '1' preference

print(paste0("Sum of weights is: ", sum(weights), ". Must be 100 exactly." )) # Test your math!

# Number of rows (ballots)
num_rows <- 110000

# Generate data
data <- as.data.frame(generate_preferences(candidates, weights, num_rows))

# Adding formatted BallotID with 'BLT_' prefix
data$BallotID <- sprintf("BLT_%03d", 1:num_rows)

# Assign precincts; roughly a quarter each
data$Precinct <- sample(c(110, 111, 112, 113), size = num_rows, replace = TRUE)

# Write to CSV
write.csv(data, here("data", "preferences_simulation_with_ranking_bias.csv"), row.names = FALSE)

print("CSV file with ranking bias and customized BallotID has been created successfully.")


# Adding Weights to Later Rounds - and undervotes


