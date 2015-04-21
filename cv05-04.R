#	Vypocetni pomucka k Prikladu 4

v <- c(1, 6, 3, 0, 10, 15, 0, 0, 10) / 45
p <- matrix (v, 3, 3, dimnames = list (seq (0, 2), seq (0, 2)))
p

pX <- rowSums (p)
pY <- colSums (p)

FX <- cumsum (pX)
FY <- cumsum (pY)

data.frame (pX, FX)
data.frame (pY, FY)

# F(1,1)
sum (p[1:2, 1:2])

# F(2,1)
sum (p[, 1:2])

# P(x >= 1)
sum (pX[2:3])
