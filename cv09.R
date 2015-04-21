# Pomucka k reseni Prikladu 7

data <- read.csv (file = "cv08-06.csv", header = TRUE, sep = ",", dec = ".")

# cyklus pomoci funkce "sapply", kde se promenna M (= cislo mesice) postupne meni od 1 do 12
# v kazdem cyklu vratime 4 cisla: cislo mesice, dolni odhad stredni hodnoty, prumer, a horni odhad stredni hodnoty 
matice <- sapply (seq (1, 12), function (M) {
	X <- subset (data, mnth == M)$cnt

	# doplnte si vzorce pro vypocet prumeru, a dolniho a horniho 95% odhadu stredni hodnoty z pozorvani ve vektoru X 

	prumer <- # ... prumer
	D <- # ... dolni odhad stredni hodnoty
	H <- # ... horni odhad stredni hodnoty

	# jako posledni prikaz tela funkce v cyklu se uvede vektor cisel, ktera se maji ulozit do matice vysledku
	return (c (mesic = M, D = D, prumer = prumer, H = H))
})

matice 
# prehodime jeste sloupce a radky funkci "t" (= transpozice matice) a vytvorime tabulku 

tabulka <- data.frame (t (matice))
tabulka

# vykreslete zavislosti ve sloupcich "D", "prumer" a "H" na promenne "mesic" pomoci tri lomenych car 
# u prvni zavislosti pouzijte "plot" s vhodnym nastavenim rozmezi obou os a s popisky os
# dalsi dve zavislosti pak prikreslujte pomoci "lines"
