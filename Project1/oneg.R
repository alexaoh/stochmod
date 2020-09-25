# Task 1g) Simulate Markov Chain Y for 100 years. 

N <- 1000 # Individuals.

Y0 <- c(950, 50, 0) # Starting state.

# Probabilities (to begin with).
gamma <- 0.10
alpha <- 0.01
beta <- function(Y){
  return (0.5*Y[2]/N)
}
n <- 36500 # Time steps.

values <- matrix(data=NA,nrow=3,ncol=n) # Preallocate matrix for simulated values. 
values[, 1] <- Y0

sim <- function(values){
  # Run simulation.
  for (t in 2:n){
    # Use a binomial to simulate. 
    old_susc <- values[1, t-1]
    old_inf <- values[2, t-1]
    old_rec <- values[3, t-1]
    
    new_susc <- rbinom(n = 1, size = old_susc, 1-beta(values[, t-1]))
    new_inf <- rbinom(n = 1, size = old_inf, 1-gamma) 
    new_rec <- rbinom(n = 1,size =  old_rec, 1-alpha) 
    Y <- c(new_susc - (new_rec - old_rec), new_inf - (new_susc - old_susc), new_rec - (new_inf - old_inf))
    values[, t] <- Y
  }
  return (values)
}

take.mean <- function(amount, fun = sim){
  # Take mean of 'amount' number of simulations. 
  average_susc <- c(length = amount)
  average_inf <- c(length = amount)
  average_rec <- c(length = amount)
  
  # Could take some averages if we want :)
  for (i in 1:amount){
    # Take an average. 
    val <- sim(values)
    susc <- val[1, ncol(val)]/N
    inf <- val[2, ncol(val)]/N
    rec <- val[3, ncol(val)]/N
    average_susc[i] <- susc
    average_inf[i] <- inf
    average_rec[i] <- rec
  }
  return (list("avg1" = mean(average_susc), "avg2" = mean(average_inf), "avg3" = mean(average_rec)))
}

amount <- 100 # Number of simulations to take mean over. 

par_lty <- c(3,2,1)
par_col <- c("blue", "red", "green")
plot(1:n, val[1, ], type = "l", lty = par_lty[1], col = par_col[1], xlab="Time Steps", 
     ylab = "Individuals", main = capture.output(cat("Mean over ", amount, " realizations")))
lines(1:n, val[2, ], type = "l", lty = par_lty[2], col = par_col[2])
lines(1:n, val[3, ], type = "l", lty = par_lty[3], col = par_col[3])
legend("topright", legend= c("Susceptible", "Infected", "Recovered"), lty = par_lty, col = par_col)

avg_prop <- take.mean(amount, sim)
print("Mean Proportions:")
cat("Susceptible ", avg_prop$avg1, "\n")
cat("Infected ", avg_prop$avg2, "\n")
cat("Recovered ", avg_prop$avg3)
