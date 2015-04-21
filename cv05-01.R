#	Vypocetni pomucka k Prikladu 1

X <- seq (0, 4, by = 1)
p <- c (1/2, 1/4, 1/8, 1/16, 1/16)
F <- cumsum (p)
data.frame (X, p, F)
