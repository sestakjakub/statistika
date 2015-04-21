# Priklad 10

n <- 10000

# nastaveni grafickych parametru, vice viz "?par"
par (mfrow = c(1,1), mar = c (4,4,4,1))
# prikazem "plot" prichystame prazdny souradnicovy system 
plot (c(0,24), c(0,24), type = "n", xlab = "jedna", ylab = "dva")

# funkce "runif" generuje vektor (delky N) nahodnych cisel nezavisle generovanych v zadanem intervalu 
A <- runif (n, 0, 24)
B <- runif (n, 0, 24)

# funkce "cbind" vytvari matici z vektoru (matic) tak, ze je umisti jako sloupce vedle sebe
# funkce "data.frame" vytvari datovou tabulku
V <- data.frame (cbind (A, B))
# logicka formule
V$ok <- (V$A - 1 <= V$B) & (V$B <= V$A + 2)
V$ok <- (V$A - 23 <= V$B) & (V$B <= V$A + 22)
V$color <- ifelse (V$ok, "#00cc00", "#ff0000")
V$symbol <- ifelse (V$ok, 1, 4)

# prikaz "points" vykresluje body, jejich soiradnice jsou zadany vektory jako prvni dva argumenty
points (V$A, V$B, col = V$color, pch = V$symbol, lwd = 2)
# spocitame a do obrazku vypiseme relativni cetnost
nA <- sum (V$ok)
fA <- nA / n
title (main = paste (nA, "/", n, "=", fA))

