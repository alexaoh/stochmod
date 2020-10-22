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
    }
  } else if (curr == 1){
    rate <- sum(q1out)
    prob1 <- q1out[1]/rate
    prob2 <- q1out[3]/rate
    # Sojourn time.
    s <- c(s, tail(s,n=1) + rexp(n = 1, rate = rate))
    # Draw from uniform distribution.
    r <- runif(1)
    if (prob1 > r){
      x <- c(x, curr-1)
    } else{
      x <- c(x, curr+1)
    }
  } else{
    rate <- sum(q2out)
    prob1 <- q2out[1]/rate
    prob2 <- q2out[2]/rate
    # Sojourn time.
    s <- c(s, tail(s,n=1) + rexp(n = 1, rate = rate))
    # Draw from uniform distribution.
    r <- runif(1)
    if (prob1 > r){
      x <- c(x, curr-2)
    } else{
      x <- c(x, curr-1)
    }
  }
  i <- i+1
  print(tail(s, n=1))
  
  # Shitty code, very messy + not exactly 1000 years :)
}

# Plot one realization over 5 years. 
par_lty <- c(3,2,1)
par_col <- c("blue", "red", "green")

# Find index in s closest to 5 years. 
index <- which(abs(s-days*5)==min(abs(s-days*5)))
plot(s[1:index], x[1:index], xlim = c(0, days*5), ylim = c(0, 2), xlab = "Time (s)", ylab = "States", main = "One Realization, 5 years")
#legend("center", legend= c("S", "I_L", "I_H"), lty = par_lty, col = par_col)
#for (i in 1:days*5){
#  lines(s[i:(i+1)], rep(x[i],2))
#}
