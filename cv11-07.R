# =============================================================================================================================
# Priklad 7

tabulka <- read.csv (file = "cv11-07.csv", header = TRUE, sep = ",", dec = ".")
tabulka$ropa <- log (tabulka$MBBl)
summary (tabulka)


# grafika

plot (c (1850, 2000), c (30, 22000), type = "n", xlab = "rok", ylab = "objem vytezene ropy")

points (tabulka$Year, tabulka$MBBl, col = "blue", pch = 24, lwd = 1.5, cex = 1.0)

# primka

model1 <- lm (ropa ~ Year, data = tabulka) 
model1
prehled1 <- summary (model1)
prehled1

# parabola

model2 <- lm (ropa ~ Year + I(Year^2), data = tabulka) 
model2
prehled2 <- summary (model2)
prehled2

# pruchystame si sit na x-ove ose
x <- seq (1850, 2000, by = 0.1)
# pomoci funkce "predict" nechame dopocitat odpovidajici Y-ove hodnoty na regresni funkci
Y1 <- predict (model1, data.frame (Year = x))
Y2 <- predict (model2, data.frame (Year = x))

lines (x, exp (Y1), col = "red", lwd = 2)
lines (x, exp (Y2), col = "#00cc00", lwd = 2)

# 95% intervaly spolehlivosti pro koeficienty modelu
confint (model1, level = 0.95)
# porovnejte je s testy vyznamnosti jednotlivych parametru
prehled1

confint (model2, level = 0.95)
prehled2

# muzeme prikreslit 95% pasy spolehlivosti pro stredni hodnotu, tj. kolem regresni funkce
CI1 <- predict (model1, data.frame (Year = x), interval = "confidence", level = 0.95)
CI2 <- predict (model2, data.frame (Year = x), interval = "confidence", level = 0.95)
lines (x, exp(CI1[,2]), col = "red", lty = 2)
lines (x, exp(CI1[,3]), col = "red", lty = 2)
lines (x, exp(CI2[,2]), col = "#00cc00", lty = 2)
lines (x, exp(CI2[,3]), col = "#00cc00", lty = 2)
dev.copy2pdf (file = "obrazek.pdf", width = 5, height = 4)

# boxploty rezidui

boxplot (model1$residuals, model2$residuals, ylab = "rezidua pro logaritmovany objem", names = c(1,2), border = c ("red", "#00cc00"))
dev.copy2pdf (file = "obrazekr.pdf", width = 3.5, height = 4)

# nelogaritmovana rezidua
r1 <- tabulka$MBBl - exp(model1$fitted.values)
r2 <- tabulka$MBBl - exp(model2$fitted.values)
boxplot (r1, r2, ylab = "rezidua pro logaritmovany objem", names = c(1,2), border = c ("red", "#00cc00"))
dev.copy2pdf (file = "obrazekr2.pdf", width = 3.5, height = 4)

# porovnani modelu

sum (model1$residuals^2) / model1$df.residual
prehled1$r.squared
prehled1$adj.r.squared

sum (model2$residuals^2) / model2$df.residual
prehled2$r.squared
prehled2$adj.r.squared

anova (model1, model2) 

# z pohledu logaritmovaneho objemu volime model s regresni parabolou
# ANOVA zde hypotezu o rovnosti strednich hodnot obou modelu zamita, nebot p-hodnota testu < 0.05



# jeste doplnime graf pro logaritmovane hodnoty objemu vytezene ropy

plot (c (1850, 2000), c (3, 10.5), type = "n", xlab = "rok", ylab = "logaritmovany objem vytezene ropy")
points (tabulka$Year, tabulka$ropa, col = "blue", pch = 24, lwd = 1.5, cex = 1.0)
lines (x, Y1, col = "red", lwd = 2)
lines (x, Y2, col = "#00cc00", lwd = 2)
lines (x, CI1[,2], col = "red", lty = 2)
lines (x, CI1[,3], col = "red", lty = 2)
lines (x, CI2[,2], col = "#00cc00", lty = 2)
lines (x, CI2[,3], col = "#00cc00", lty = 2)
dev.copy2pdf (file = "obrazek2.pdf", width = 5, height = 4)
