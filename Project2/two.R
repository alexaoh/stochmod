# Task 2a)
# Define a regular grid of parameter values from θ = 0.25 to θ = 0.50, 
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
theta <- seq(theta_0, theta_1, by = 0.00499) # Var is not positive definite with 0.005. 

# Observed points.
x_B <- c(0.5, 0.32, 0.40, 0.35, 0.60)
nodes <- c(0.30, 0.35, 0.39, 0.41, 0.45)

# Build distance matrices.
H_A <- as.matrix(dist(theta, diag = TRUE))
H_B <- as.matrix(dist(nodes, diag = TRUE))

H_AB <- matrix(nrow = length(theta), ncol = length(nodes))

for (i in 1:length(theta)){
  for (j in 1:length(x_B)){
    H_AB[i,j] = dist(rbind(as.matrix(theta[i]),as.matrix(nodes[j])))
  }
}

# Covariance matrices.
Sigma_A <- sigma**2*(1+15*H_A)*exp(-15*H_A)
Sigma_B <- sigma**2*(1+15*H_B)*exp(-15*H_B)
Sigma_AB <- sigma**2*(1+15*H_AB)*exp(-15*H_AB)

# Mean of values values from Gaussian process on grid.
mu_A <- rep(mean, length(theta))
mu_B <- rep(mean, length(x_B)) 

E <- mu_A + Sigma_AB %*% solve(Sigma_B) %*% (x_B - mu_B)
Var <- Sigma_A - Sigma_AB %*% solve(Sigma_B) %*% t(Sigma_AB)

# Draw values from the conditional multivariate Gaussian.
L <- chol(Var)
z_vec <- rep(0, length(theta))
for (i in 1:length(theta)){
  z_vec[i] <- rnorm(1)
}
x_A <- E + L %*% z_vec

# Plot the prediction and the 90% prediction interval.
# plot(theta, x_A, main = "Prediction as a Function of Theta", ylab = "Prediction", xlab = "Theta")
# lines(theta, x_A)
# lines(theta, E + 1.64*sqrt(diag(Var)), col = "red", lwd = 2, "l", lty = 2)
# lines(theta, E - 1.64*sqrt(diag(Var)), col = "red", lwd = 2, "l", lty= 2)
# legend(0.35,0.7, legend=c("Pred. Int."), col=c("red"), lty=2)#, cex=0.8)

#2 b): Probabilities.
library("expm")
y <- rep(0.30, length(theta))
stand <- (Var %^% -0.5)%*%(y-E)
prob <- pnorm(stand)
plot(theta, prob, main = "Conditional Probability as Function of Theta", ylab = "Conditional Prob.", xlab = "Theta")
lines(theta, prob)

# 2 c): Add new point and run again. 
# Observed points.
x_B <- c(0.5, 0.32, 0.40, 0.35, 0.60, 0.40)
nodes <- c(0.30, 0.35, 0.39, 0.41, 0.45, 0.33)

# Compute everything again.
# Build distance matrices.
H_A <- as.matrix(dist(theta, diag = TRUE))
H_B <- as.matrix(dist(nodes, diag = TRUE))

H_AB <- matrix(nrow = length(theta), ncol = length(nodes))

for (i in 1:length(theta)){
  for (j in 1:length(x_B)){
    H_AB[i,j] = dist(rbind(as.matrix(theta[i]),as.matrix(nodes[j])))
  }
}

# Covariance matrices.
Sigma_A <- sigma**2*(1+15*H_A)*exp(-15*H_A)
Sigma_B <- sigma**2*(1+15*H_B)*exp(-15*H_B)
Sigma_AB <- sigma**2*(1+15*H_AB)*exp(-15*H_AB)

# Mean of values values from Gaussian process on grid.
mu_A <- rep(mean, length(theta))
mu_B <- rep(mean, length(x_B)) 

E <- mu_A + Sigma_AB %*% solve(Sigma_B) %*% (x_B - mu_B)
Var <- Sigma_A - Sigma_AB %*% solve(Sigma_B) %*% t(Sigma_AB)

# Draw values from the conditional multivariate Gaussian.
L <- chol(Var)
z_vec <- rep(0, length(theta))
for (i in 1:length(theta)){
  z_vec[i] <- rnorm(1)
}
x_A <- E + L %*% z_vec

# Plot the prediction and the 90% prediction interval.
# plot(theta, x_A, main = "Prediction as a Function of Theta", ylab = "Prediction", xlab = "Theta")
# lines(theta, x_A)
# lines(theta, E + 1.64*sqrt(diag(Var)), col = "red", lwd = 2, "l", lty = 2)
# lines(theta, E - 1.64*sqrt(diag(Var)), col = "red", lwd = 2, "l", lty= 2)
# legend(0.35,0.7, legend=c("Pred. Int."), col=c("red"), lty=2)#, cex=0.8)

# Probabilities.
y <- rep(0.30, length(theta))
stand <- (Var %^% -0.5)%*%(y-E)
prob2 <- pnorm(stand)
plot(theta, prob2, main = "Conditional Probability as Function of Theta", ylab = "Conditional Prob.", xlab = "Theta")
lines(theta, prob2)
lines(theta, prob, col = "red", lty = 2)
legend(0.30, 0.38, c("Old Prob.", "New Prob."), col = c("red", "black"), lty=2:1)

