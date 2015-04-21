#	Vypocetni pomucka k Prikladu 3

v <- c(1/3, 0, 1/3, 0, 1/3, 0)
p <- matrix (v, 3, 2, dimnames = list (seq (-1, 1), seq (0, 1)))
p

pX <- rowSums (p)
pY <- colSums (p)

FX <- cumsum (pX)
FY <- cumsum (pY)

data.frame (pX, FX)
data.frame (pY, FY)
