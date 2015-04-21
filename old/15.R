#  Graficka pomucka k Prikladu 5

# Grafy marginalnich hustot pravdepodobnosti

fX <- function (w) {
  f <- ifelse (w >= 0 & w <= 10, (10 - w) / 50, 0)
  return (f)
}

fY <- function (w) {
  f <- ifelse (w >= 0 & w <= 10, w / 50, 0)
  return (f)
}

w <- seq (-2, 12, by = 0.01)

plot (w, fX(w), type = "l", col = "blue", lwd = 2, ylab = "marg. hustoty pravdepodobnosti")
lines (w, fY(w), col = "blue", lwd = 2, lty = 2)

# Grafy marginalnich distribucnich funkci

FX <- function (w) {
  F <- (10*w - w^2/2) / 50
  F[w < 0] <- 0
  F[w > 10] <- 1
  return (F)
}

FY <- function (w) {
  F <- w^2/100
  F[w < 0] <- 0
  F[w > 10] <- 1
  return (F)
}

plot (w, FX(w), type = "l", col = "blue", lwd = 2, ylab = "marg. distribucni funkce")
lines (w, FY(w), col = "blue", lwd = 2, lty = 2)

# P (X >= 4) = 1 - FX (4)
1 - FX (4)

