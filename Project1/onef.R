# Task 1f: Code that simulates Markov chain Yn for n = 300 steps.

N <- 1000 # Individuals.

Y0 <- c(950, 50, 0) # Starting state.

# Probabilities (to begin with).
gamma <- 0.10
alpha <- 0.01
beta <- function(Y){
  return (0.5*Y[2]/N)
}
n <- 300 # Time steps.

values <- matrix(data=NA,nrow=3,ncol=n) # Preallocate matrix for simulated values. 
values[, 1] <- Y0

# Run simulation.
for (t in 2:n){
  # Use a binomial to simulate. 
  old_susc <- values[1, t-1]
  old_inf <- values[2, t-1]
  old_rec <- values[3, t-1]
  
  new_inf <- rbinom(n = 1, size = old_susc, beta(values[, t-1]))
  new_rec <- rbinom(n = 1, size = old_inf, gamma) 
  new_susc <- rbinom(n = 1,size =  old_rec, alpha) 
  Y <- c(old_susc - new_inf + new_susc, old_inf - new_rec + new_inf, old_rec - new_susc + new_rec)
  values[, t] <- Y
}

par_lty <- c(3,2,1)
par_col <- c("blue", "red", "green")
plot(1:n, values[1, ], type = "l", lty = par_lty[1], col = par_col[1], xlab="Time [days]", ylab = "Individuals", main = "One Realization")
lines(1:n, values[2, ], type = "l", lty = par_lty[2], col = par_col[2])
lines(1:n, values[3, ], type = "l", lty = par_lty[3], col = par_col[3])
legend("topright", legend= c("Susceptible", "Infected", "Recovered"), lty = par_lty, col = par_col)
