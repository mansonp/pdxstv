# Handling Replacements - BROKEN

# Sample data
data <- data.frame(
  Chinook = c(1,0,0,1,4,2,1),
  DouglasFir = c(0,0,2,3,0,6,0),
  ThunderEgg = c(0,3,4,0,0,0,0),
  Filbert = c(6,0,0,1,1,3,2),
  Chanterelle = c(0,5,0,0,0,0,0),
  Dungeness = c(5,0,1,5,3,0,4),
  Pear = c(2,2,6,0,0,4,0),
  Potato = c(3,4,5,4,1,1,0)
)

# Function to adjust values in the row to remove gaps in the sequence 1 to 5
adjust_row <- function(row) {
  # Values above 0 are sorted, zeros removed
  non_zeros <- sort(row[row > 0])    
  desired = 1:5  # Define desired values range
  valid_values = non_zeros[non_zeros <= 5]
  
  replacement_values = setdiff(desired, valid_values)
  extra_values = non_zeros[non_zeros > 5]
  
  # Additional check to match replacements and adjusts length
  num_replacements = min(length(replacement_values), length(extra_values))
  valid_values = c(valid_values, replacement_values[seq_len(num_replacements)])
  valid_values = sort(valid_values[valid_values <= 5])
  
  # Construct the final output
  output = rep(0, length(row))  # Start with all zeros
  output[1:length(valid_values)] = valid_values  # Place valid values in the beginning
  
  return(output)
}

# Apply the function to each row in the data frame
adjusted_data <- as.data.frame(t(apply(data, 1, adjust_row)))
colnames(adjusted_data) <- colnames(data)

# Print adjusted data
print(adjusted_data)

