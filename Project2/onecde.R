# Problem 1c) + 1d) + 1e)
# Simulate a continuous-time MC over a time period of 1000 years. 

days <- 365
years <- 1000

# Transition rates. Indexing according to state transitioning to. 
# Could naturally also put this in a matrix and make the code simpler below also. 
q0out <- c(0, 0.0090, 0.0010)
q1out <- c(1/7, 0, 0)
q2out <- c(1/20, 0, 0)

# States.
x <- c(0)

# Sojourn times.
s <- c(0)
s_inf <- 0 # For task 1d)

# For task 1e)
E_heavy <- c() # Vector of times between heavy infections, for task 1e). 
heavy_end <- 0
heavy_start <- 0

i <- 1
# Simulate forward in time.
while (tail(s, n= 1) < years*days){
  curr <- x[i]
    if (curr == 0){
    rate <- sum(q0out)
    prob1 <- q0out[2]/rate
    prob2 <- q0out[3]/rate
    # Sojourn time.
    s <- c(s, tail(s,n=1) + rexp(n = 1, rate = rate))
    # Draw from uniform distribution.
    r <- runif(1)
    if (prob1 > r){
      x <- c(x, curr+1)
    } else{
      x <- c(x, curr+2)
      heavy_start <- tail(s, n=1) # For task 1e)
    }
  } else if (curr == 1){
    rate <- sum(q1out)
    prob1 <- q1out[1]/rate
    prob2 <- q1out[3]/rate
    # Sojourn time.
    new_s <- rexp(n = 1, rate = rate)
    s <- c(s, tail(s,n=1) + new_s)
    s_inf <- s_inf + new_s # Add to time infected, for task 1d)
    # Draw from uniform distribution.
    r <- runif(1)
    if (prob1 > r){
      x <- c(x, curr-1)
    } else{
      x <- c(x, curr+1)
      heavy_start <- tail(s, n=1) # For task 1e)
    }
  } else{
    rate <- sum(q2out)
    prob1 <- q2out[1]/rate
    prob2 <- q2out[2]/rate
    # Sojourn time.
    new_s <- rexp(n = 1, rate = rate)
    s <- c(s, tail(s,n=1) + new_s)
    s_inf <- s_inf + new_s # Add to time infected, for task 1d)
    # Draw from uniform distribution.
    r <- runif(1)
    if (prob1 > r){
      x <- c(x, curr-2)
    } else{
      x <- c(x, curr-1)
    }
    
    # For task 1e)
    E_heavy <- c(E_heavy, heavy_start - heavy_end)
    heavy_end <- 0
  }
  i <- i+1
  
  # Shitty code, very messy + not exactly 1000 years :)
}

# Plot one realization over 5 years. 
par_lty <- c(3,2,1)
par_col <- c("blue", "red", "green")

# Find index in s closest to 5 years. 
index <- which(abs(s-days*5)==min(abs(s-days*5)))
plot(s[1:index], x[1:index], xlim = c(0, days*5), ylim = c(0, 2), xlab = "Time (s)", ylab = "States", main = "One Realization, 5 years")

# Calculations in task d).
# Estimate the long-run mean fraction of time that an individual has an infection, 
# based on one realization of 1000 years. 

cat("Mean fraction of time infected ", s_inf/(years*days))
cat("\nExpected time between heavy infections", mean(E_heavy)/365, "years. ")
