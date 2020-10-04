# ==============================================
# PROBLEM 1d)
# ==============================================

beta <-  0.05
gamma <-  0.10
alpha  <-  0.01

P  <-  matrix(c(1-beta, beta, 0, 0, 1 - gamma, gamma, alpha, 0, 1-alpha), nrow <- 3, ncol <- 3, byrow <- TRUE)

# Return the mean over a specified number of simulations of the expected values.
simulate_expected_values <- function(P, num_sim = 1) {
  
  result <- matrix(nrow = num_sim, ncol = 3)
  
  for (j in 1:num_sim) {
    
    v1 <- 0 # Total number of "loops" in state 0.
    v2 <- 0 # Total number of "loops" in state 1.
    v3 <- 0 # Total number of transitions from 0 to 1 and 1 to 2.
    
    v1_counter <- 0 # Number of transitions from 0 to 1.
    v2_counter <- 0 # Number of transitions from 1 to 2.
    v3_counter <- 0 # Number of complete cycles.
    
    year <- 365
    state <- 0
    for (i in 1:(year*50)) {
      
      newstate <- sample.int(3, size <- 1, replace <- TRUE, prob <- P[state+1,])-1
      
      if (newstate != state) { # A new state has been transitioned to.
        if(state == 0) {
          v1_counter <- v1_counter + 1
          v1 <- v1 + 1
          v2 <- v2 + 1
          v3 <- v3 + 1
        }
        else if(state == 1) {
          v2_counter <- v2_counter + 1
          v2 <- v2 + 1
          v3 <- v3 + 1
        }
        else if(state == 2) {
          v3_counter <- v3_counter + 1
          v3 <-  v3 + 1
        }
        
      }
      else { # Transistion to the same state.
        if (state == 0) {
          v1 <- v1 + 1
          v2 <-  v2 + 1
          v3 <- v3 + 1}
        else if (state == 1) {
          v2 <- v2 + 1
          v3 <- v3 + 1}
        else if (state == 2){
          v3 <- v3 + 1
        }
      }
      
      state <- newstate
      
    }
    
    result[j,] <- c(v1/v1_counter,v2/v2_counter,v3/v3_counter)
  }
  return (c(mean(result[,1]),mean(result[,2]),mean(result[,3]))) # Return the means over the simulations.
}

# ++++++++++++++
# Run code below
# ++++++++++++++

# Find the mean of the expected values of 100 realizations of the Markov chain simulated for 50 years. 
result <- simulate_expected_values(P,100)
cat("Theoretical value 1: ", 1/beta, "\n")
cat("Simulated value 1: ", result[1], "\n")
cat("Theoretical value 2: ", 1/beta + 1/gamma, "\n")
cat("Simulated value 2: ", result[2], "\n")
cat("Theoretical value 3: ", 1/beta + 1/alpha + 1/gamma, "\n")
cat("Simulated value 3: ", result[3], "\n")


# ==============================================
# PROBLEM 1f)
# ==============================================

# Probabilities in the transition probability matrix. 
# alpha and gamma are already defined above. 
beta_n <- function(Y) {
  return (0.5 * Y[2]/N)
}

sim <- function(values, n){
  # Run simulation.
  for (t in 2:n){
    # Use a binomial to simulate. 
    old_susc <- values[1, t-1]
    old_inf <- values[2, t-1]
    old_rec <- values[3, t-1]
    
    new_inf <- rbinom(n = 1, size = old_susc, beta_n(values[, t-1]))
    new_rec <- rbinom(n = 1, size = old_inf, gamma) 
    new_susc <- rbinom(n = 1,size =  old_rec, alpha) 
    Y <- c(old_susc - new_inf + new_susc, old_inf - new_rec + new_inf, old_rec - new_susc + new_rec)
    values[, t] <- Y
  }

  # Foreshadow: For task h). 
  m <- max(values[2, ]) # Find maximum value of the infected individuals at once. 
  t <- which.max(values[2, ]) # Find index of the maximum of the infected individuals at once.
  return (list("values" = values, "m" = m, "t" = t))
}

plot_one_realization <- function(values, n, fun = sim){
  list_of_values <- fun(values, n)
  values <- list_of_values$values # Only need the values in this part of the task. 
  par_lty <- c(3,2,1)
  par_col <- c("blue", "red", "green")
  plot(1:n, values[1, ], type = "l", lty = par_lty[1], col = par_col[1], xlab="Time [days]", ylab = "Individuals", main = "One Realization")
  lines(1:n, values[2, ], type = "l", lty = par_lty[2], col = par_col[2])
  lines(1:n, values[3, ], type = "l", lty = par_lty[3], col = par_col[3])
  legend("topright", legend= c("Susceptible", "Infected", "Recovered"), lty = par_lty, col = par_col)
}

# ++++++++++++++
# Run code below
# ++++++++++++++

# Initial distribution over the categories.
Y0 <- c(950,50,0)
N <- sum(Y0) # Number of individuals. 

n <- 300 # Time steps.

# Preallocate matrix for simulated values.
values <- matrix(data=NA,nrow=3,ncol=n)  
values[, 1] <- Y0

# Plot figure showing one realization. 
plot_one_realization(values, n, sim)

# ==============================================
# PROBLEM 1g)
# ==============================================

take.mean <- function(values, amount, n, fun = sim){
  # Take mean of 'amount' number of simulations. 
  average_susc <- c(length = amount)
  average_inf <- c(length = amount)
  average_rec <- c(length = amount)
  
  for (i in 1:amount){
    # Take an average. 
    list_of_values <- sim(values, n)
    val <- list_of_values$values # Only need the values in this part of the task. 
    susc <- val[1, ncol(val)]/N
    inf <- val[2, ncol(val)]/N
    rec <- val[3, ncol(val)]/N
    average_susc[i] <- susc
    average_inf[i] <- inf
    average_rec[i] <- rec
  }
  return (list("val" = val, "avg1" = mean(average_susc), "avg2" = mean(average_inf), "avg3" = mean(average_rec)))
}

plot_one_hundred_years <- function(val){
  # Parameters for color and linetypes in plot. 
  par_lty <- c(3,2,1)
  par_col <- c("blue", "red", "green")
  # Plot the lines. 
  plot(1:n, val[1, ], type = "l", lty = par_lty[1], col = par_col[1], xlab="Time [days]", 
      ylab = "Individuals", main = capture.output(cat("Mean over ", amount, " realizations")))
  lines(1:n, val[2, ], type = "l", lty = par_lty[2], col = par_col[2])
  lines(1:n, val[3, ], type = "l", lty = par_lty[3], col = par_col[3])
  legend("topright", legend= c("Susceptible", "Infected", "Recovered"), lty = par_lty, col = par_col)
}

# ++++++++++++++
# Run code below
# ++++++++++++++

n <- 36500 # Time steps. 100 years. 

# Preallocate matrix for simulated values.
values <- matrix(data=NA,nrow=3,ncol=n)  
values[, 1] <- Y0

amount <- 100 # Number of simulations to take mean over. 
avg_prop <- take.mean(values, amount, n, sim)
val <- avg_prop$val # Values to plot. 

plot_one_hundred_years(val)

# Print simulated values. 
print("Mean Proportions:", quote=FALSE)
cat("Susceptible ", avg_prop$avg1, "\n")
cat("Infected ", avg_prop$avg2, "\n")
cat("Recovered ", avg_prop$avg3, "\n")


# ==============================================
# PROBLEM 1h)
# ==============================================

# Find the expected values in question.  
find.expected.values <- function(values, amount, n, fun = sim){
  maximums <- c()
  argmax <- c()
  for (i in 1:amount){
    vec <- sim(values, n)
    val <- vec$values
    
    # Using the maximum and argmax from sim() here. 
    m <- vec$m
    t <- vec$t
    maximums[i] <- m
    argmax[i] <- t
  }

  return (list("exp1" = mean(maximums), "exp2" = mean(t)))
}

# ++++++++++++++
# Run code below
# ++++++++++++++

n <- 300 # Time steps.

# Preallocate matrix for simulated values.
values <- matrix(data=NA,nrow=3,ncol=n)  
values[, 1] <- Y0

means <- find.expected.values(values, N, n, sim)
cat("Expected value of max of I's ", means$exp1, "\n")
cat("Expected time at which the number of infected individuals first takes its highest values ", means$exp2, "\n")

# ==============================================
# PROBLEM 2a)
# ==============================================

# Simulate the Poisson process for a given number of simulations.
simulate_pois <- function(lambda, t_start,t_end, plot, numSim = 1) {
  result <- rep(NA,numSim)
  for (i in 1:numSim) {
    n <-  rpois(1,lambda = lambda*(t_end - t_start))
    result[i] <-  n
    v_vec <-  runif(n,t_start,t_end)
    w_vec <-  sort(v_vec)
    x_vec <- seq(from = 0, to = n, by = 1)
    
    if(plot && i == 1) {
      
      plot(NULL, NULL, xlim = c(t_start, t_end), ylim = c(0, n), xlab = "Time [days]", lwd = 2, ylab = "Claims", main = "Insurance Claims: 10 Realizations", cex = 2)
      for(i in 1:n) {
        lines(w_vec[i:(i+1)], rep(x_vec[i],2), lwd = 1)
      }
    }
    else if(plot) {
      for(i in 1:n) {
        lines(w_vec[i:(i+1)], rep(x_vec[i],2), lwd = 1)
      }
    }
  }
  return (result)
}

# ++++++++++++++
# Run code below
# ++++++++++++++

lambda <- 1.5 # Poisson rate parameter. 

# Estimate the probability in question.
res <- simulate_pois(lambda,0,59,plot = FALSE, numSim = 1000)
num <- sum(res > 100)/length(res)
cat("Probability: ", num, "\n")

# Plot.
simulate_pois(lambda,0,59,plot = TRUE, numSim = 10)

# ==============================================
# PROBLEM 2B
# ==============================================

# Takes a list of Poisson distributed values (claims) and returns the size of the claims summed together
calculate.claims <- function(claims) {
  result = rep(0,length(claims))
  for (i in 1:length(claims)) {
    for (j in 1:claims[i]) {
      c <-  rexp(1,rate = 10)
      result[i] <- result[i] + c
    }
  }
  return(result)
}

# ++++++++++++++
# Run code below
# ++++++++++++++

claims <- simulate_pois(lambda,0,59,plot = FALSE, numSim = 1000*10)
res <- calculate.claims(claims)

cat("Expected total claim amount: ", mean(res), "\n")
cat("Variance of the total claim amount: ", var(res))
