# Parameters
mu0 <- 500
sigma <- 5
n <- 16
N <- 1000
z_critical <- 1.645

# -------------------------------
# Part 1: Simulation under H0
# -------------------------------

# Store Z statistics
z_values_H0 <- numeric(N)

for(i in 1:N){
  
  # Generate sample from N(500,25)
  x <- rnorm(n, mean = 500, sd = sigma)
  
  # Sample mean
  xbar <- mean(x)
  
  # Z statistic
  z <- (xbar - mu0) / (sigma / sqrt(n))
  
  z_values_H0[i] <- z
}

# Reject H0 if Z > 1.645
reject_H0 <- z_values_H0 > z_critical

# Proportion of rejection
type1_error <- mean(reject_H0)

# -------------------------------
# Part 2: Simulation under H1
# -------------------------------

z_values_H1 <- numeric(N)

for(i in 1:N){
  
  # Generate sample from N(503,25)
  x <- rnorm(n, mean = 503, sd = sigma)
  
  # Sample mean
  xbar <- mean(x)
  
  # Z statistic
  z <- (xbar - mu0) / (sigma / sqrt(n))
  
  z_values_H1[i] <- z
}

# Reject H0
reject_H1 <- z_values_H1 > z_critical

# Estimate power
power_test <- mean(reject_H1)

# -------------------------------
# Results
# -------------------------------

cat("Estimated Type I Error =", type1_error, "\n")
cat("Estimated Power of Test =", power_test, "\n")

