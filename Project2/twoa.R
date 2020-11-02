library(matlib)
# Task 2a)
# Define a regular grid of parameter values from θ= 0.25 to θ= 0.50, 
# with spacing 0.005 (n= 51 grid points).
# Construct the mean vector and the covariance matrices required to 
# compute the conditional mean and covariance matrix of the process 
# at the 51 grid points conditional on the five evaluation points.  
# Display the prediction as a function of θ, along with 90% prediction intervals.

theta_0 <- 0.25
theta_1 <- 0.50
mean <- 0.5
sigma <- 0.5

# Make grid of parameters. 
theta <- seq(theta_0, theta_1, by = 0.005)

# Correlation function. Unsure if needed. 
corr <- function(t1, t2){
  return (1+15*abs(t1-t2)*exp(-15*abs(t1-t2)))
}

# Build distance matrices.
H_A <- as.matrix(dist(theta, diag = TRUE))
H_B <- as.matrix(dist(theta, diag = TRUE))

H_AB <- matrix(nrow = length(theta), ncol = length(theta))

print(H_A)

for (i in 1:length(theta)){
  for (j in 1:length(theta)){
    H_AB[i,j] = dist(rbind(as.matrix(theta[i]),as.matrix(theta[j])))
  }
}

# Covariance matrices.
Sigma_A <- sigma**2*(1+15*H_A)*exp(-15*H_A)
Sigma_B <- sigma**2*(1+15*H_B)*exp(-15*H_B)
Sigma_AB <- sigma**2*(1+15*H_AB)*exp(-15*H_AB)

# Mean of values values from Gaussian process on grid.
mu_A <- rep(mean, length(theta))
mu_B <- rep(mean, length(theta)) # Hva skal mu_B være?

x_B <- c(0.5, 0.32, 0.40, 0.35, 0.60)

# Find x_A from simulation of Gaussian process
L <- chol(Sigma_A)
z_vec <- pnorm(theta, mean = mu_A, sd = rep(sigma**2, length(theta)))
x_A <- z_vec + L %*% z_vec

E <- mu_A + Sigma_AB %*% inv(Sigma_B) %*% (x_B - mu_B)
Var <- Sigma_A - Sigma_AB %*% Sigma_B %*% t(Sigma_AB)


  