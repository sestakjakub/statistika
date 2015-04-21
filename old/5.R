# Priklad 12

a <- 10
r <- 2
n <- 200

par (mfrow = c(1,1), mar = c (2,2,4,1))
plot (c(-a/2, 3/2*a), c(-a/2, 3/2*a), type = "n")
abline (h = 0, col = "sienna", lty = 2)
abline (h = a, col = "sienna", lty = 2)
abline (v = 0, col = "sienna", lty = 2)
abline (v = a, col = "sienna", lty = 2)

Sx <- runif (n, min = 0, max = a) 
Sy <- runif (n, min = 0, max = a) 

V <- data.frame (cbind (Sx, Sy))
V$ok <- (V$Sx > r) & (V$Sx < a - r) & (V$Sy > r) & (V$Sy < a - r)
V$color <- ifelse (V$ok, "#00cc00", "#ff0000")
V$symbol <- ifelse (V$ok, 3, 4)

points (V$Sx, V$Sy, col = V$color, pch = V$symbol, lwd = 2)
nA <- sum (V$ok)
fA <- nA / n
title (main = paste (nA, "/", n, "=", fA))

# zakomentujte nasl. radek, pokud nechcete kreslit obrys micku
symbols (V$Sx, V$Sy, circles = rep (r, nrow (V)), fg = V$color, lty = 2, inches = FALSE, add = TRUE)

