# Exercise Week 36, task 2c). 
# Simulate the weather for one year based on the given Markov chain. 

# State 0 = It rained today.
# State 1 = It did not rain today. 

P <- matrix(c(0.70, 0.25, 0.30, 0.75), nrow = 2)

# Simulate for one year.
B <- 365

# Initialize storage
x <- vector('numeric', length = B+1)


# Initial state
x[1] <- 0

simulate <- function(B, x){
  # Simulate.
  for (y in 1:B){
    x[y+1] <- sample.int(2, size = 1, replace = TRUE, prob = P[x[y]+1,])-1
  }
  return (x)
}
 
x <- simulate(B, x)

plot(0:B, x, type = "o", xlab="Day in Year", ylab="State")


# What is the probability of rain for a random day during the year?

sims <- 3000
occurrences <- 0
for (s in 1:sims){
  randomDay = runif(1, min = 1, max = 365) 
  x <- simulate(B, x)
  
  if (x[randomDay] == 0){
    occurrences <- occurrences + 1
  }
}

print("Probability of rain for a random day during the year:")
cat("\n")
print(occurrences/sims)
