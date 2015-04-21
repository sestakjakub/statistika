#  Priklad 1.1 z prednasky

getwd ()
# Nastavte si pracovni adresar tam, kde mate ulozeny datove soubory, prikazem "setwd"
# anebo v zalozce Files se prepnete do adresare a zvolte More > Set As Working Directory
setwd (".")

data <- read.csv (file = "cv08-01.csv", header = TRUE, sep = ",", dec = ".")
str (data)

#	pocet skupin
r <- nrow (data)

#	absolutni cetnosti
n.j <- data$cetnosti

#	pocet pozorovani
n <- sum (n.j)

#	relativni cestnosti
p.j <- n.j / n
sum (p.j)

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n

#	tabulka 
tabulka <- data.frame (znamka = data$znamka, n.j, p.j, N.j, F.j)
tabulka

#	graf cetnostni funkce
plot (tabulka$znamka, tabulka$n.j, type = "p", pch = 20, cex = 2, col = "red", xlab = "znamka", ylab = "hodnoty cetnostni funkce", main = "Graf cetnostni funkce")

# graf empiricke distribucni funkce
plot (c(0,tabulka$znamka,5), c(0,tabulka$F.j,1), type="s", xlab = "znamka", ylab = "hodnoty empir. distr. fce", main="Graf empiricke distribucni funkce", col = "red", lwd = 2)

X <- rep (tabulka$znamka, tabulka$n.j)
plot (ecdf (X), col = "red", lwd = 2, xlab = "znamka", ylab = "hodnoty empir. distr. fce", main="Graf empiricke distribucni funkce")

# sloupkovy diagram
barplot (tabulka$n.j, names.arg = tabulka$znamka, xlab = "znamka", ylab = "pocet pozorovani", main = "Sloupkovy diagram")

# polygon cetnosti
plot (tabulka$znamka, tabulka$n.j, type="b", pch = 20, xlab = "znamka", ylab = "pocet pozorovani", main = "Polygon cetnosti", col = "red", cex = 2, lty = 2)

#	histogram 
hist (X, breaks = seq (0.5, 4.5, by = 1), freq = FALSE, col = "yellow", xlab = "znamka", ylab = "hodnoty cetnostni hustoty", main = "Histogram", xlim = c (0, 5))



#	=====================================================================================================================================
# Priklad 1.2 z prednasky 

data <- read.csv (file = "cv08-02.csv", header = TRUE, sep = ",", dec = ".")
str (data)

#	pocet skupin
r <- nrow (data)

# stredy intervalu
stredy <- (data$mez.d + data$mez.h) / 2

# delky intervalu 
d.j <- data$mez.h - data$mez.d

#	absolutni cetnosti
n.j <- data$pocet

#	pocet pozorovani
n <- sum (n.j)

#	relativni cestnosti
p.j <- n.j / n
sum (p.j)

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n

# cetnostni hustota
f.j <- p.j / d.j

#	tabulka 
tabulka <- data.frame (mez.d = data$mez.d, mez.h = data$mez.h, stredy, d.j, n.j, p.j, N.j, F.j, f.j)
tabulka

# sloupkovy diagram cetnosti
barplot (tabulka$n.j, names.arg = paste (tabulka$mez.d, "-", tabulka$mez.h, sep = ""), xlab = "mez plasticity", ylab = "pocet pozorovani")

# sloupkovy diagram relativnich cetnosti
barplot (tabulka$p.j, names.arg = paste (tabulka$mez.d, "-", tabulka$mez.h, sep = ""), xlab = "mez plasticity", ylab = "relativni cetnost")

# sloupkovy diagram cetnostni hustoty
barplot (tabulka$f.j, names.arg = paste (tabulka$mez.d, "-", tabulka$mez.h, sep = ""), space = 0, xlab = "mez plasticity", ylab = "cetnostni hustota", main = "Histogram")

#	histogram 
X <- rep (tabulka$stredy, tabulka$n.j)
hist (X, breaks = c (tabulka$mez.d[1], tabulka$mez.h), freq = FALSE, col = "yellow", xlab = "mez plasticity", ylab = "hodnoty cetnostni hustoty", main = "Histogram")

# graf empiricke distribucni funkce
plot (c(0,tabulka$mez.d[1],tabulka$mez.h,200), c(0,0,tabulka$F.j,1), type = "b", xlab = "mez plasticity", ylab = "hodnoty empir. distr. fce", main = "Graf empiricke distribucni funkce", col = "red")



#	=====================================================================================================================================
# Priklad 1.3 z prednasky 

#...
mean(X)

rozptyl <- mean (X^2) - mean(X)^2
rozptyl

smerodatnaodchylka = sqrt(rozptyl)
smerodatnaodchylka

var (X) *(n-1) / n

#	=====================================================================================================================================
# Priklad 1.4 z prednasky 

# opraveny prumer

 (110 * 20 +10 +30)/20

# opraveny rozptyl




#	=====================================================================================================================================
# Priklad 1.5 z prednasky 

data <- read.csv (file = "cv08-01.csv", header = TRUE, sep = ",", dec = ".")
str (data)

X <- rep (data$znamka, data$cetnosti)
n <- sum (data$cetnosti)

# vektor poradi a serazeny vzorek
R <- rank (X)
X.sorted <- sort (X)

# median, kvartily, kvartilova odchylka
c.25 <- 0.25 * n
c.50 <- #...
  c.75 <- #...
  c (c.25, c.50, c.75)


x.25 <- (X.sorted[c.25] + X.sorted[c.25 + 1]) / 2
x.50 <- #...
  x.75 <- #...
  
  c (x.25, x.50, x.75)
q <- x.75 - x.25
q

# hradby boxplotu
c (x.25, x.75) + c (-1, 1) * 1.5 * q
c (x.25, x.75) + c (-1, 1) * 3 * q

# porovnejte s nasledujicim
median (X)
quantile (X, c (0.25, 0.5, 0.75))
# 25% kvantil se lisi, R totiz pri vypoctu kvantilu vzdy pouziva prumerovani dvou okolnich hodnot

# boxplot pomoci stejnojmenne funkce
boxplot (X, ylim = range (X), main = "Krabicovy diagram (boxplot)")
# "fousy" oznacuji nejmensi a nejvetsi pozorovani, ktera jeste jsou uvnitr vnitrni hradby 
stripchart (X, vertical = TRUE, method = "jitter", pch = 21, col = "red", cex = 0.6, bg = "yellow", add = TRUE)

