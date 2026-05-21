# Practical on MLE for Exponential Distribution

set.seed(123)

# True value of theta
theta <- 2

# Sample sizes
n_values <- c(20, 50, 80, 100)

# Store results
theta_hat <- numeric(length(n_values))
mse <- numeric(length(n_values))

# Number of repetitions for MSE calculation
R <- 1000

for(i in 1:length(n_values))
{
  n <- n_values[i]
  
  estimates <- numeric(R)
  
  for(j in 1:R)
  {
    # Generate sample from exponential distribution
    x <- rexp(n, rate = theta)
    
    # MLE of theta
    estimates[j] <- 1 / mean(x)
  }
  
  # Average estimate
  theta_hat[i] <- mean(estimates)
  
  # Mean Squared Error
  mse[i] <- mean((estimates - theta)^2)
}

# Display results
result <- data.frame(
  Sample_Size = n_values,
  MLE_Theta = theta_hat,
  MSE = mse
)

print(result)

# Plot MSE vs Sample Size
plot(n_values, mse,
     type = "b",
     pch = 19,
     col = "blue",
     xlab = "Sample Size (n)",
     ylab = "MSE",
     main = "MSE vs Sample Size")

