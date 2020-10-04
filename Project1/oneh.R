# Task 1h) 1000 realizations of simulation in task 1g) and find expected values. 

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
  m <- max(values[2, ]) # Find maximum value of the infected individuals at once. 
  t <- which.max(values[2, ]) # Find index of the maximum of the infected individuals at once.
  return (list("values" = values, "m" = m, "t" = t))
}


find.expected.values <- function(amount, fun = sim){
  maximums <- c()
  argmax <- c()
  for (i in 1:amount){
    vec <- sim(values)
    val <- vec$values
    m <- vec$m
    t <- vec$t
    maximums[i] <- m
    argmax[i] <- t
  }

  return (list("exp1" = mean(maximums), "exp2" = mean(t)))
}

means <- find.expected.values(1000, sim)
cat("Expected value of max of I's ", means$exp1, "\n")
cat("Expected time at which the number of infected individuals first takes its highest values ", means$exp2)
