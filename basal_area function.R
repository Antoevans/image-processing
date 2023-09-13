# Function to calculate basel area
calculate_basal_area <- function(df) {
  # Create an empty vector to store the calculated basel area
  basal_area <- numeric(length = nrow(df))
  
  # Iterate over each row in the data frame
  for (i in 1:nrow(df)) {
    # Calculate basel area based on the dbh using the equation dbh^2 * 3.14 / 4
    basal_area[i] <- (df$d[i]^2) * 3.142 / 4
  }
  
  # Return the calculated basel area vector
  return(basal_area)
}
