# Exercise D in note on Gaussian Processes.
# Distances.

# Study ways to build the distance matrices specified between
# supplied points. Check out various methods
# - for-loops
# - Vectorization method
# - Other built-in approaches.

t_A <- c(3,5,9,10,13,20)
t_B <- c(3.5,5.2,7.8,12.1)

H_A <- dist(t_A, diag = TRUE)
H_B <- dist(t_B, diag = TRUE)

H_AB <- matrix(nrow = 6, ncol = 4)
for (i in 1:6){
  for (j in 1:4){
    H_AB[i,j] = dist(rbind(as.matrix(t_A[i]),as.matrix(t_B[j])))
  }
}

print(H_AB)
print(rbind(as.matrix(t_A[1]),as.matrix(t_B[1])))
