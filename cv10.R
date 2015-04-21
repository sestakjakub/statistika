# Priklad 8.5 zadany na 9. prednasce

# ...



# Priklad 8.6 zadany na 9. prednasce

n1 <- 50
n2 <- 40
prumer1 <- 275
prumer2 <- 280
rozptyl1 <- 48
rozptyl2 <- 41
alpha <- 0.05

# i
(z <- abs (prumer1 - prumer2) / sqrt (rozptyl1 / n1 + rozptyl2 / n2))
(kvantil <- qnorm (1 - alpha))
z > kvantil

# ii
(p <- 1 - pnorm (z))
p < alpha

# iii
# Pocitame horni odhad v IS a divame se, jestli je cislo 0 (tj. neni rozdil v produkci za nulove hypotezy) mensi nez H
H <- (prumer1 - prumer2) + kvantil * sqrt (rozptyl1 / n1 + rozptyl2 / n2)
0 <= H


#	Priklad 2

data <- read.csv (file = "cv09-02.csv", header = TRUE, sep = ";", dec = ",")
str (data)
X <- data$spotreba 
n <- length (X)
prumer <- mean (X)
rozptyl <- var (X)
odchylka <- sd (X)
alpha <- 0.05

# stredni hodnota
# prvni zpusob 
(t <- abs ((prumer - 9) / odchylka * sqrt(n)))
(kvantil <- qt (1-alpha/2, n-1))
t > kvantil

# druhy zpusob
(p <- 2 * min (1 - pt (abs (prumer - 9) / odchylka * sqrt(n), n-1), pt (abs (prumer - 9) / odchylka * sqrt(n), n-1)))
p < alpha
t.test (X, mu = 9)

# treti zpusob
D <- prumer - kvantil * odchylka / sqrt(n)
H <- prumer + kvantil * odchylka / sqrt(n)
c (D, H)
(D <= 9) & (9 <= H)

# odchylka, resp. rozptyl
# prvni zpusob
(k <- rozptyl / 0.5^2 * (n-1))
(kvantil1 <- qchisq (alpha/2, n-1))
(kvantil2 <- qchisq (1-alpha/2, n-1))
(k < kvantil1) | (kvantil2 < k)

# druhy zpusob
(p <- 2 * min (1 - pchisq (rozptyl * (n-1) / 0.5^2, n-1), pchisq (rozptyl * (n-1) / 0.5^2, n-1)))
p < alpha

# treti zpusob
kvantil1 <- qchisq (1 - alpha/2, n - 1)
kvantil2 <- qchisq (alpha/2, n - 1)
D <- (n - 1) * rozptyl / kvantil1
H <- (n - 1) * rozptyl / kvantil2
c (D, H)
(D <= 0.5^2) & (0.5^2 <= H)



#	Priklad 3

# ...



#	Priklad 4 

data <- read.csv (file = "cv09-04.csv", header = TRUE, sep = ";", dec = ",")
str (data)
X <- data$pevnost
n <- length (X)
rozptyl <- var (X)
odchylka <- sd (X)
alpha <- 0.05

# i 
(k <- rozptyl / 0.36 * (n-1))
(kvantil2 <- qchisq (1-alpha, n-1))
k > kvantil2

# ii
(p <- pchisq (rozptyl * (n - 1)/0.36, n - 1, lower.tail = rozptyl < 0.36))
p < alpha

# iii
kvantil <- qchisq (1 - alpha, n - 1)
D <- (n - 1) * rozptyl / kvantil
D
D <= 0.36



#	Priklad 5 

# ...



# Priklad 6

data <- read.csv (file = "cv09-06.csv", header = TRUE, sep = ",", dec = ".")
str (data)
X <- data$zakaznici
n <- length (X)
prumer <- mean (X)
odchylka <- sqrt (prumer)
rozptyl <- odchylka^2
alpha <- 0.05
# Pozn.: 20 sekund cekani odpovida 3 zakaznikum za minutu, tj. H_O: lambda=3

# i
(z <- abs (prumer - 3) / odchylka * sqrt (n))
(kvantil <- qnorm (1 - alpha/2))
z > kvantil

# ii
(p <- 2 * (1 - pnorm (z)))
p < alpha

# iii
kvantil <- qnorm (1 - alpha / 2)
D <- prumer - kvantil * odchylka / sqrt (n) 
H <- prumer + kvantil * odchylka / sqrt (n) 
c (D, H) 
(D <= 3) & (3 <= H)



#	Priklad 7

# ...
