# Observed frequencies
obs <- c(5,8,12,15,18,14,10,9,5,4)

# Class intervals midpoint
mid <- c(1,3,5,7,9,11,13,15,17,19)

# Total frequency
N <- sum(obs)

# Mean
mean1 <- sum(obs * mid) / N

# Standard deviation
sd1 <- sqrt(sum(obs * (mid - mean1)^2) / N)

# Class limits
lower <- c(0,2,4,6,8,10,12,14,16,18)
upper <- c(2,4,6,8,10,12,14,16,18,20)

# Expected probabilities using Normal distribution
p <- pnorm(upper, mean1, sd1) - pnorm(lower, mean1, sd1)

# Expected frequencies
exp <- N * p

# Chi-square statistic
chi <- sum((obs - exp)^2 / exp)

# Degrees of freedom
df <- length(obs) - 2 - 1

# Critical value at 5%
cv <- qchisq(0.95, df)

# Results
cat("Mean =", mean1,"\n")
cat("SD =", sd1,"\n")
cat("Chi-square =", chi,"\n")
cat("Critical value =", cv,"\n")

# Decision
if(chi < cv){
  cat("Accept H0 : Data follows Normal distribution")
} else {
  cat("Reject H0 : Data does not follow Normal distribution")
}

