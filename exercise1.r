# Exercise 1: Coin Toss

simulate_coin_toss_a <- function(N){
    # Simulates a coin toss with one biased coin and one unbiased coin. 
    heads <- 0
    biased <- 0
    for (num in 1:N){
        r <- runif(1)
        if (r > 0.5){
            # Biased coin
            biased = biased + 1
            heads = heads + 1

        } else {
            # Unbiased coin
            r <- runif(1)
            if (r > 0.5){
                # Heads 
                heads = heads + 1
            } # else tails
        }
    }
    return (biased / heads)
}

sim <- 10000

print("Coin toss a)", quote=FALSE)
print(simulate_coin_toss_a(sim))

simulate_coin_toss_b <- function(N){
    # Simulate to coin tosses with the same coin (either biased or unbiased). 
    double_heads <- 0
    biased <- 0
    for (num in 1:N){
        r <- runif(1)
        if (r > 0.5){
            # Biased coin
            biased = biased + 1
            double_heads = double_heads + 1
        } else {
            # Unbiased coin
            r <- runif(1)
            if (r > 0.5){
                # Heads once
                r <- runif(1)
                if (r > 0.5){
                    # Heads twice
                    double_heads = double_heads + 1
                }
            }
        }
    }

    return (biased / double_heads)

}

print("Coin toss b)", quote=FALSE)
print(simulate_coin_toss_b(sim))


# Exercise 2: Insurance Claims


simulate_insurance_claims_a <- function(B){
    E <- 0
    V <- 0
    v <- vector()
    for (y in 1:B){
        amounts <- rlnorm(1, -2, 1)
        E = E + amounts
        v <- c(v, amounts)
    }

    E = E / B

    for (val in v){
        V = V + (E - val)**2
    }

    V = V / B

    values = list("E" = E, "V" = V)
    return (values)
}

simulate_insurance_claims_b <- function(B){
    E <- 0
    V <- 0
    v <- vector()
    for (y in 1:B){
        N <- rpois(1, 6) # Number of claims.
        amount_sum <- 0
        for (n in 1:N){
            C <- rlnorm(1, -2, 1) # Claim amount. 
            amount_sum = amount_sum + C
        }
        
        E = E + amount_sum
        v <- c(v, amount_sum)
    }

    E = E / B

    for (val in v){
        V = V + (E - val)**2
    }

    V = V / B

    values = list("E" = E, "V" = V)
    return (values)
}

B <- 20000 # Years of insurance claims to simulate.
print("Expected value and variance of claim amounts:", quote=FALSE)
b <- simulate_insurance_claims_a(B)
expected = b$E
variance = b$V
print("Expected value:", quote=FALSE)
print(expected)
print("Variance:", quote=FALSE)
print(variance)
cat("\n")
print("Expected value and variance of SUM of claim amounts:", quote=FALSE)
c <- simulate_insurance_claims_b(B)
expected = c$E
variance = c$V
print("Expected value:", quote=FALSE)
print(expected)
print("Variance:", quote=FALSE)
print(variance)
