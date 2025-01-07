attach(dane_do_zestawu_17)

dane=dane_do_zestawu_17

#zadanie 1 - decyle D3 i D7
quantile(dane$WielkośćSpółki, probs=c(.3,.7))

#   30%      70% 
#917.490 1567.289 

D3=quantile(dane$WielkośćSpółki, probs=.3)
D7=quantile(dane$WielkośćSpółki, probs=.7)

dane$Wielkość <- cut(dane$WielkośćSpółki,
                               breaks = c(-Inf, D3, D7, Inf),
                               labels = c("mała", "średnia", "duża"))
View(dane)

rozkład_liczebności <- table(dane$Wielkość)
rozkład_liczebności_df <- as.data.frame(rozkład_liczebności)
colnames(rozkład_liczebności_df) <- c("Kategoria wielkości", "Liczebność")
