# Simulate the Markov chain given by the transitional probability matrix P.
P <- matrix(c(1, 0, 0, 0.05, 0.8, 0.15, 0, 0, 1), nrow = 3, byrow=TRUE)
B = 100
for (i in 1:B){
  P = P%*%P
}

plot(P[2, ], type = "h")
print(P[2, ])
