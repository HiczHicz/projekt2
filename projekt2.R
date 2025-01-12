#FED789FF, #023743FF, #476F84, #72874EFF, #A4BED5FF, #453947FF


dane=dane_do_zestawu_17
library(moments)

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

barplot(rozkład_liczebności, ylim=c(0,100), main="Rozkład liczebności spółek", xlab="Kategoria wielkości", ylab="Liczebność", col=c("#FED789FF", "#476F84", "#72874EFF"))


#ZADANIE 2
#MAŁE
małe_spółki <- dane[dane$Wielkość == "mała", ]
#średnia
śr_małe=mean(małe_spółki$WskaźnikCZ)
#odchylenie
n1=length(małe_spółki$WskaźnikCZ)
odchyl_małe=sqrt(1/n1*sum((małe_spółki$WskaźnikCZ-śr_małe)^2))
#mediana
mediana_małe=median(małe_spółki$WskaźnikCZ)
#Q1
q1_małe=quantile(małe_spółki$WskaźnikCZ, probs=.25)
#Q2
q2_małe=quantile(małe_spółki$WskaźnikCZ, probs=.75)
#xmin 
xmin_małe=min(małe_spółki$WskaźnikCZ)
#xmax
xmax_małe=max(małe_spółki$WskaźnikCZ)
#współczynnik asymetrii
wspasym_małe=skewness(małe_spółki$WskaźnikCZ)
#kurtoza
kur_małe=kurtosis(małe_spółki$WskaźnikCZ)-3

r_małe=rbind(śr_małe, odchyl_małe, mediana_małe, q1_małe, q2_małe, xmin_małe, xmax_małe,wspasym_małe, kur_małe)

#ŚREDNIE 
średnie_spółki <- dane[dane$Wielkość == "średnia", ]
#średnia
śr_średnie=mean(średnie_spółki$WskaźnikCZ)
#odchylenie
n1=length(średnie_spółki$WskaźnikCZ)
odchyl_średnie=sqrt(1/n1*sum((średnie_spółki$WskaźnikCZ-śr_średnie)^2))
#mediana
mediana_średnie=median(średnie_spółki$WskaźnikCZ)
#Q1
q1_średnie=quantile(średnie_spółki$WskaźnikCZ, probs=.25)
#Q2
q2_średnie=quantile(średnie_spółki$WskaźnikCZ, probs=.75)
#xmin 
xmin_średnie=min(średnie_spółki$WskaźnikCZ)
#xmax
xmax_średnie=max(średnie_spółki$WskaźnikCZ)
#współczynnik asymetrii
wspasym_średnie=skewness(średnie_spółki$WskaźnikCZ)
#kurtoza
kur_średnie=kurtosis(średnie_spółki$WskaźnikCZ)-3

r_średnie=rbind(śr_średnie, odchyl_średnie, mediana_średnie, q1_średnie, q2_średnie, xmin_średnie, xmax_średnie,wspasym_średnie, kur_średnie)


#DUŻE 
duże_spółki <- dane[dane$Wielkość == "duża", ]
#średnia
śr_duże=mean(duże_spółki$WskaźnikCZ)
#odchylenie
n1=length(duże_spółki$WskaźnikCZ)
odchyl_duże=sqrt(1/n1*sum((duże_spółki$WskaźnikCZ-śr_duże)^2))
#mediana
mediana_duże=median(duże_spółki$WskaźnikCZ)
#Q1
q1_duże=quantile(duże_spółki$WskaźnikCZ, probs=.25)
#Q2
q2_duże=quantile(duże_spółki$WskaźnikCZ, probs=.75)
#xmin 
xmin_duże=min(duże_spółki$WskaźnikCZ)
#xmax
xmax_duże=max(duże_spółki$WskaźnikCZ)
#współczynnik asymetrii
wspasym_duże=skewness(duże_spółki$WskaźnikCZ)
#kurtoza
kur_duże=kurtosis(duże_spółki$WskaźnikCZ)-3

r_duże=rbind(śr_duże, odchyl_duże, mediana_duże, q1_duże, q2_duże, xmin_duże, xmax_duże,wspasym_duże, kur_duże)

#CAŁA TABELKA - zmienić nazwy, zweryfikować
cbind(r_małe, r_średnie, r_duże)

write.csv(cbind(r_małe, r_średnie, r_duże))


  