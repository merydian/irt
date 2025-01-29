# Rasch model ICC function 

rasch_icc <- function(ability_levels, item_difficulty) {
  # Description:
  # Computes the probability of a correct response using the Rasch model (1PL)
  # and plots the Item Characteristic Curve (ICC).
  #
  # Arguments:
  #   ability_levels: A vector of respondent ability levels.
  #   item_difficulty: Difficulty parameter of the item (scalar).
  #
  # Returns:
  #   A plot of the ICC and a data frame of ability levels and probabilities.
  
  # Compute probabilities using the Rasch model equation
  probabilities <- 1 / (1 + exp(-1.7 * (ability_levels - item_difficulty)))
  
  # Create a data frame for plotting and returning results
  results <- data.frame(
    Ability = ability_levels, 
    Probability = probabilities
  )
  
  # Plot the ICC
  plot(
    results$Ability, results$Probability, 
    type = "l", col = "blue", lwd = 2, 
    xlab = "Respondent Ability", ylab = "Probability of Correct Response",
    main = paste("ICC for Rasch Model (Item Difficulty =", item_difficulty, ")"),
    ylim = c(0, 1)
  )
  abline(h = 0.5, col = "red", lty = 2)  # Add a horizontal line at 50%
  abline(v = item_difficulty, col = "green", lty = 2)  # Add a vertical line at difficulty
  
  # Return the results
  return(results)
}

# Example Usage
ability_values <- seq(-3, 3, by = 0.1)  # Respondent ability levels from -3 to 3
ability_values
item_difficulty <- 2  # Difficulty of the item
rasch_icc(ability_levels = ability_values, item_difficulty = item_difficulty)

