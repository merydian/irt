# ------------------------------------------------------------------------------------
# Item Response Simulation & Model Fit Assessment
#
# This script simulates item responses based on the Rasch model and evaluates 
# the fit of an Item Characteristic Curve (ICC). It:
# - Defines ability levels and assigns examinees.
# - Computes theoretical response probabilities.
# - Simulates observed responses using a probabilistic model.
# - Quantifies model fit through a Chi-square index.
# - Visualizes theoretical vs. observed response patterns.
# ------------------------------------------------------------------------------------


# Set a seed for reproducibility
set.seed(123)

# Generate a sequence of ability levels
ability_levels <- seq(-3, 3, 0.1875)  # From -3 to 3 with increments of 0.1875
ability_levels

# Set the number of examinees per ability level
num_examinees_per_level <- rep(21, length(ability_levels))  # 21 examinees for each level
num_examinees_per_level

# Generate the item difficulty parameter for the Rasch model
item_difficulty <- 0  # Change this as you like

# Calculate probabilities of a correct response (the ICC curve)
# This calculates the probability of a correct response at each ability level using the Rasch model formula
probability_correct <- numeric(length(ability_levels))  # Initialize probability vector
for (i in 1:length(ability_levels)) {
  # Apply Rasch formula
  probability_correct[i] <- 1 / (1 + exp(-1.7 * (ability_levels[i] - item_difficulty)))
}

# Simulate observed probability of correct responses i.e. "proportions"
observed_proportions <- rbinom(length(ability_levels), num_examinees_per_level, probability_correct) / num_examinees_per_level

# Calculate the Chi-square goodness-of-fit
chi_square_index <- 0  # Initialize Chi-square index
for (i in 1:length(ability_levels)) {
  chi_square_term <- num_examinees_per_level[i] * (observed_proportions[i] - probability_correct[i])^2 / 
    (probability_correct[i] * (1 - probability_correct[i]))
  chi_square_index <- chi_square_index + chi_square_term
}
chi_square_index <- round(chi_square_index, 2)  # Round to 2 decimal places

# Plot the observed proportions and the ICC on the same canvas
plot(
  ability_levels, observed_proportions,
  xlim = c(-3, 3), ylim = c(0, 1),
  xlab = "Ability", ylab = "Probability of Correct Response",
  main = paste("Rasch Model: Chi-square =", chi_square_index, "\nItem Difficulty (b) =", item_difficulty),
  pch = 16, col = "blue"  # Blue points for observed proportions
)

# Add the ICC (probability_correct) as a red line
lines(ability_levels, probability_correct, col = "red", lwd = 2)  # Red line for the ICC

# Add a legend to differentiate between the observed proportions and the ICC
legend(
  "bottomright", legend = c("Observed Proportions", "Item Characteristic Curve (ICC)"),
  col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2)
)

