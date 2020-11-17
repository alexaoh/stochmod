#Project 2 Stochastic Modelling
#Problem 1: Modelling the common cold

#Constants
alpha = 0.1
lambda = 1/100
mu_L = 1/7
mu_H = 1/20

simulate_markov <- function(state0, tEnd, plot) {
  
  time_infected = 0
  time_between_Ih = 0
  time_between_Ih_list <- c()
  
  if (plot) {
    plot(NULL, NULL, 
         xlim = c(0, tEnd), 
         ylim = c(-0.2, 2.2), 
         xlab = "Time (days)", 
         lwd = 5,
         ylab = "State", 
         main = "One Realization, 5 years")
  }
  
  time = 0
  state = state0
  while (time < tEnd) {
    if (state == 0) {
      #Sojourn time
      S =  rexp(1,rate = lambda)
      if (plot) {
        lines(c(time,time + S), c(state,state), lwd = 1)
      }
      
      #In case the sojourn time exceeds the end time
      if ( time + S > tEnd) {
        S = tEnd - time
      }
      time = time + S
      sample = runif(1)
      if (sample <= alpha) {
        state = 2
        if (time_between_Ih > 0) {
          time_between_Ih_list <- c(time_between_Ih_list, time - time_between_Ih)
          time_between_Ih = 0
        }
      }
      else {
        state = 1
      }
    }
    
    else if (state == 1) {
      S = rexp(1,rate = mu_L)
      if (plot) {
        lines(c(time,time + S), c(state,state), lwd = 2)
      }
      
      if ( time + S > tEnd) {
        S = tEnd - time
      }
      time = time + S
      time_infected = time_infected + S
      state = 0
    }
    
    else if (state == 2) {
      S = rexp(1,rate = mu_H)
      if (plot) {
        lines(c(time,time + S), c(state,state), lwd = 3)
      }
      
      if ( time + S > tEnd) {
        S = tEnd - time
      }
      time = time + S
      time_infected = time_infected + S
      time_between_Ih = time
      state = 0
    }
  }  
  
  return (c(time_infected/tEnd,mean(time_between_Ih_list)))
}


get_mean <- function(state0, tEnd, numSim) {
  result_list <- c()
  for (i in 1:numSim) {
    result <- simulate_markov(0,tEnd,FALSE)
    result_list <- c(result_list,result[2])
  }
  return (mean(result_list))
}

#1.c
tEnd = 5*365
result <-  simulate_markov(0, tEnd, TRUE)

#1.d
tEnd = 1000*365
result <-  simulate_markov(0, tEnd, FALSE)
time_infected <- result[1]
cat("Proportion of time infected in one realization: ",time_infected)

#1.e
numSim = 100
result <- get_mean(0,tEnd,numSim)
cat("\nAverage time between heavy infections over ", numSim, " realizations: ", result)

####### Problem 2
#Calibrating Climate Models

#2.a
std = 0.5
mu = 0.5
cov_function <- function(theta1, theta2){
  val = (1+15*abs(theta1 - theta2))*exp(-15*abs(theta1 -theta2)) * std^2
  return (val)
}

theta_grid = seq(from = 0.25, to = 0.50, by = 0.0049999)

#Known values
sample_points =  c(0.30,0.35,0.39,0.41,0.45)
sample_values = c(0.5, 0.32, 0.40, 0.35, 0.60)

#Function for creating covariance matrix
get_cov <-  function(vecA,vecB) {
  nA <- length(vecA)
  nB <- length(vecB)
  result <- matrix(nrow = nA, ncol = nB)
  for (i in 1:nA) {
    for (j in 1:nB) {
      result[i,j] = cov_function(vecA[i],vecB[j])
    }
  }
  return (result)
}

Sigma_AB <-  get_cov(theta_grid, sample_points)
Sigma_AA <-  get_cov(theta_grid, theta_grid)
Sigma_BB <- get_cov(sample_points, sample_points)

muA <-  rep(mu,length(theta_grid))
muB <- rep(mu,length(sample_points))

muC <- muA + Sigma_AB %*% solve(Sigma_BB) %*% (sample_values - muB)
Sigma_C <- Sigma_AA - Sigma_AB %*% solve(Sigma_BB) %*% t(Sigma_AB)

L <-  chol(Sigma_C)
z <- rnorm(length(theta_grid))
predict <- muC + L %*% z

plot(theta_grid, predict, main = "Prediction as a Function of Theta", xlab = "Theta", ylab = "Prediction")
lines(theta_grid, predict)

#Prediction interval
z005 = qnorm(0.95)

upper = muC + z005*sqrt(diag(Sigma_C))
lower = muC - z005*sqrt(diag(Sigma_C))

lines(theta_grid, muC, col = "blue", lwd = 2, lty = 3)

lines(theta_grid,upper, col = "red", lwd = 2, "l", lty = 2)
lines(theta_grid,lower, col = "red", lwd = 2, "l", lty = 2)
legend(0.35,0.7, legend=c("Pred. Int.", "Cond. Mean"), col=c("red", "blue"), lty=c(2,3))

#2.b
library(expm)   
y <- rep(0.30,length(theta_grid))
std_matrix <- Sigma_C %^% -0.5
standardize <- std_matrix %*% (y - muC)
probs1 <- pnorm(standardize)

# Done differently, since the above was wrong.
chance_vec1 <- rep(0,51)
for (i in 1:51){
  chance_vec1[i] <- pnorm(0.3,mean=muC[i],sd=sqrt(abs(Sigma_C[i,i])))
}

plot(theta_grid, chance_vec1, main = "Conditional Probability as Function of Theta", ylab = "Conditional Prob.", xlab = "Theta")
lines(theta_grid, chance_vec1)

#2.c
#Same as in a), but with one more point.
sample_points =  c(0.30,0.35,0.39,0.41,0.45, 0.33)
sample_values = c(0.5, 0.32, 0.40, 0.35, 0.60, 0.40)

Sigma_AB <-  get_cov(theta_grid, sample_points)
Sigma_AA <-  get_cov(theta_grid, theta_grid)
Sigma_BB <- get_cov(sample_points, sample_points)


muA <-  rep(mu,length(theta_grid))
muB <- rep(mu,length(sample_points))

muC <- muA + Sigma_AB %*% solve(Sigma_BB) %*% (sample_values - muB)
Sigma_C <- Sigma_AA - Sigma_AB %*% solve(Sigma_BB) %*% t(Sigma_AB)
Sigma_C


L = chol(Sigma_C)
z <- rnorm(length(theta_grid))
predict <- muC + L %*% z


plot(theta_grid, predict, main = "Prediction as a Function of Theta", xlab = "Theta", ylab = "Prediction")
lines(theta_grid, predict)

#Prediction interval
z005 <-  qnorm(0.95)

upper <-  muC + z005*sqrt(diag(Sigma_C))
lower <-  muC - z005*sqrt(diag(Sigma_C))

lines(theta_grid,upper, col = "red", lwd = 2, "l", lty = 2)
lines(theta_grid,lower, col = "red", lwd = 2, "l", lty = 2)
legend(0.35,0.7, legend=c("Pred. Int."), col=c("red"), lty=2)

y <- rep(0.30,length(theta_grid))
std_matrix <- Sigma_C %^% -0.5
standardize <- std_matrix %*% (y - muC)
probs <- pnorm(standardize)

# Done differently, since the above was wrong.
chance_vec <- rep(0,51)
for (i in 1:51){
  chance_vec[i] <- pnorm(0.3,mean=muC[i],sd=sqrt(abs(Sigma_C[i,i])))
}

plot(theta_grid, chance_vec, ylim = c(0, 0.25), main = "Conditional Probability as Function of Theta", ylab = "Conditional Prob.", xlab = "Theta")
lines(theta_grid, chance_vec)
lines(theta_grid, chance_vec1, col = "red", lty = 2)
legend(0.40, 0.10, c("Old Prob.", "New Prob."), col = c("red", "black"), lty=2:1)
