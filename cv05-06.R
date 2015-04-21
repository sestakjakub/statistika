#	Vypocetni pomucka k Prikladu 6

lambdaX <- 1 
lambdaY <- 1/2 
w <- 2

# (b) 
1 - pexp (w, rate = lambdaX)

# (c) 
(1 - pexp (w, rate = lambdaX)) * (1 - pexp (w, rate = lambdaY))

# (d) 
pexp (w, rate = lambdaX) * (1 - pexp (w, rate = lambdaY)) + 
	(1 - pexp (w, rate = lambdaX)) * pexp (w, rate = lambdaY)

# (e) 
1 - pexp (w, rate = lambdaX) * pexp (w, rate = lambdaY)
