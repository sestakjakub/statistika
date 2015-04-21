#  Graficka pomucka k Prikladu 2

#	Definujeme vlastni hustotu pravdepodobnosti
f <- function (x) {
  f <- ifelse (x >= 0 & x <= 10, 1/5 - x/50, 0)
  return (f)
}

#	Definujeme vlastni distribucni funkci
F <- function (x) {
  F <- x/5 - x^2/100
  F[x < 0] <- 0
  F[x > 10] <- 1
  return (F)
}

x <- seq (-2, 12, by = 0.01)

plot (x, f(x), type = "l", col = "blue", lwd = 2)

plot (x, F(x), type = "l", col = "red", lwd = 2)
abline (v = 5, lty = 2)
abline (h = 0.75, lty = 2)

plot (x, f(x), type = "l", col = "blue", lwd = 2)
indexy <- which (x >= 0 & x <= 5)
polygon (x[c(min(indexy), indexy, max(indexy))], c(0, f(x[indexy]), 0), col = "#FFCC00")

