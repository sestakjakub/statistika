# Priklad 6

mu <- 40
sigma <- 4

# konce intervalu hodnot x nutno vhodne upravovat
x <- seq (25, 55, by = 0.01)
titulek <- paste ("Normalni (Gaussovo) rozdeleni, ", "mu =", mu, ", sigma = ", sigma)

# hustota pravdepodobnosti 
# POZOR! Matematicky znacime N (mu, sigma^2) 
# ale v R se zadava mu, sigma
hustota <- dnorm (x, mean = mu, sd = sigma)

#	hustotu graficky zobrazujeme jako krivku
plot (x, hustota, type = "l", col = "blue", lwd = 3, ylab = "hustota pravdepodobnosti", main = titulek)

polygon (c(min (x), x, max(x)), c(0, hustota, 0), col = "#CCFFCC")

# distribucni funkce je spojita, 
#	graficky ji zobrazujeme jako krivku
distribucni <- pnorm (x, mean = mu, sd = sigma)
plot (x, distribucni, type = "l", col = "blue", lwd = 3, ylab = "distribucni funkce", main = titulek)

#	=======================================================================

# Porovnani grafu pro nekolik hodnot parametru mu a sigma

titulek <- "porovnani normalnich rozdeleni"
x <- seq (-5, 55, by = 0.01)

hustota1 <- dnorm (x, mean = 40, sd = 4)
hustota2 <- dnorm (x, mean = 40, sd = 5)
hustota3 <- dnorm (x, mean = 40, sd = 1)
hustota4 <- dnorm (x, mean = 0, sd = 1)
# hustota4 odpovida standardizovanemu normalnimu rozdeleni pravdepodobnosti

plot (x, hustota1, type = "l", col = "blue", lwd = 3, ylab = "hustoty pravdepodobnosti", main = titulek, xlim = c (-5, 55), ylim = c (0, 0.45))
lines (x, hustota2, col = "green", lwd = 3)
lines (x, hustota3, col = "red", lwd = 3)
lines (x, hustota4, col = "black", lwd = 3)

distribucni1 <- pnorm (x, mean = 40, sd = 4)
distribucni2 <- pnorm (x, mean = 40, sd = 5)
distribucni3 <- pnorm (x, mean = 40, sd = 1)
distribucni4 <- pnorm (x, mean = 0, sd = 1)

plot (x, distribucni1, type = "l", col = "blue", lwd = 3, ylab = "distribucni funkce", main = titulek, xlim = c (-5, 55))
lines (x, distribucni2, col = "green", lwd = 3)
lines (x, distribucni3, col = "red", lwd = 3)
lines (x, distribucni4, col = "black", lwd = 3)
