# Verify calculations from this week's exercise with the following simulations.

lv <- 1.2/90 # Lambda for VIF. 
lr <- 2/90 # Lambda for RBK. 

N = 1000000 # Number of random values to return from the distributions. 

# b) Prob of no goals during the first half of the match?
goals <- rpois(N,(lr + lv)*45)
print(sum(goals==0)/N)
# The theoretical value was approx 0.2019.

# c) Prob that the final result was 2-2?
goals_vif <- rpois(N, lr*90)
goals_rbk <- rpois(N, lv*90)
print(sum(goals_vif == 2 & goals_rbk == 2)/N)
# The theoretical value was approx 0.0587.

# d) Expected time until the first goal is scored.
print(mean(rexp(N, lr + lv)))
# The theoretical value was approx 28.125.

# e) Assume no goals scored in the first 15 mins. 
# Prob that VIF score at least one goal before break?
goals_vif_before_break <- rpois(N, lv*30) # Goals in the last 30 minutes. 
print(sum(goals_vif_before_break > 0)/N) # More than 0 goals in the last 30 minutes are counted. 
# The theoretical value was approx 0.33. 
