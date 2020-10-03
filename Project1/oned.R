#Problem 1: Modelling the outbreak of measles.
# d) Simulate the Markov chain and the given expected values.
# Compare to the theoretical values. 

# State 0: Susceptible.
# State 1: Infected.
# State 2: Recovered and immune.

# Probabilities.
beta <- 0.05
gamma <- 0.10
alpha <- 0.01

# Transition probability matrix. 
P = matrix(c(1 - beta, beta, 0, 0, 1 - gamma, gamma, alpha, 0, 1 - alpha), nrow = 3, byrow=TRUE)
print("Transition probability matrix; ", quote=FALSE)
print(P)
year = 365 # Days in the year. 
B = 18250 # Number of time steps to simulate for (50 years).

simulate <- function(){
  # Variables used to simulate the expected values. 
  x0 <- 0 # Starting state of the Markov chain.
  v0 <- 0 # Number of transitions to state 0.
  v1 <- 0 # Number of transitions to state 1.
  v2 <- 0 # Number of transitions to state 2.
  v01.counter <- 0 # Count number of transitions from 0 to 1.
  v12.counter <- 0 # Count number of transitions from 1 to 2.
  v20.counter <- 0 # Count number of transitions from 2 to 0. 
  
  for (y in 1:B){
    sample <- sample.int(3, size = 1, replace = TRUE, prob = P[x0+1,])-1
    
    if (sample != x0){ # The state cannot have been transitioned to from itself. 
      if (sample == 0){
        v20.counter <- v20.counter + 1
        v0 <- v0 + 1
      } else if (sample == 1){
        v1 <- v1 + 1
        v01.counter <- v01.counter + 1
      } else {
        v2 <- v2 + 1
        v12.counter <- v12.counter + 1
      }
    } else {# The state has been transitioned to from itself. in the previous step. 
      if (sample == 0){
        v0 <- v0 + 1
      } else if (sample == 1){
        v1 <- v1 + 1
      } else {
        v2 <- v2 + 1
      }
    }
    
    x0 <- sample
  }
  sim1 <- v0/v01.counter
  sim2 <- v0/v01.counter + v1/v12.counter
  sim3 <- v0/v01.counter + v1/v12.counter + v2/v20.counter
  
  return (list("sim1" = sim1, "sim2" = sim2, "sim3" = sim3))
}

take.means <- function(amount, fun = simulate){
  # Store values from each simulation in vectors. 
  vec1 <- c(length = amount)
  vec2 <- c(length = amount)
  vec3 <- c(length = amount)
  
  # Run 'amount' number of simulations.
  for (i in 1:amount){
    values <- simulate()  
    vec1[i] <- values$sim1
    vec2[i] <- values$sim2
    vec3[i] <- values$sim3
  }
  
  # Now find means of the vectors which have values from each simulation and return. 
  return (list("sim1" = mean(vec1), "sim2" = mean(vec2), "sim3" = mean(vec3)))
}

# Take mean over 100 simulations
final_values <- take.means(100)
sim1 <- final_values$sim1
sim2 <- final_values$sim2
sim3 <- final_values$sim3

cat("Theoretical value 1: ", 1/beta, "\n")
cat("Simulated value 1: ", sim1, "\n")
cat("Theoretical value 2: ", 1/beta + 1/gamma, "\n")
cat("Simulated value 2: ", sim2, "\n")
cat("Theoretical value 3: ", 1/beta + 1/alpha + 1/gamma, "\n")
cat("Simulated value 3: ", sim3, "\n")

# It is apparent that the simulated values are good estimators 
# of the theoretical values. 
