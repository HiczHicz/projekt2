
dane=dane_do_zestawu_17
library(moments)

#zadanie 1 - decyle D3 i D7
quantile(dane$WielkośćSpółki, probs=c(.3,.7))
D3=quantile(dane$WielkośćSpółki, probs=.3)
D7=quantile(dane$WielkośćSpółki, probs=.7)

dane$Wielkość <- cut(dane$WielkośćSpółki,breaks = c(-Inf, D3, D7, Inf),
                     labels = c("mała", "średnia", "duża"))

#MAŁE SPÓŁKI

małe_spółki <- dane[dane$Wielkość == "mała", ]
n=length(małe_spółki$WskaźnikCZ)
k=round(sqrt(n)) 
a=0.01 #to alfa
mi=min(małe_spółki$WskaźnikCZ)
ma=max(małe_spółki$WskaźnikCZ)
R=ma-mi
#dlugość przedziału
b=ceiling((R+a/2)/(k*a))*a
b
#wektor krańca przedziałów
kr=mi-a/2+b*c(0:k)
kr

#SZEREG ROZDZIELCZY
klasy=1:k
lewy_kr=kr[1:k]
prawy_kr=kr[2:(k+1)]
lewy_kr
prawy_kr

hist=hist(małe_spółki$WskaźnikCZ,breaks=kr)
liczebność=hist$counts

#SZEREG ROZDZIELCZY:
Szereg_rozdzielczy=as.data.frame(cbind(klasy,lewy_kr,prawy_kr,liczebność))
Szereg_rozdzielczy

#write.csv(Szereg_rozdzielczy, "szereg_rozdzielczy_male.csv", row.names = FALSE, fileEncoding = "UTF-8")

długości_klas=prawy_kr-lewy_kr
długości_klas


#Wieloboki
k_n=length(kr)

nowe_środki=(lewy_kr+prawy_kr)/2
length(nowe_środki)
ppx=kr[1]-długości_klas[1]/2  #wsp. x pierwszego punktu

#UWAGA! sprawdzić czy na pewno 
opx=nowe_środki[length(nowe_środki)]+długości_klas[1] #wsp. x ostatniego punktu - UWAGA! SPRAWDZIĆ
opx

wsp_x=c(ppx,nowe_środki,opx)

              #wielobok natężeń liczebności
              licz_y=c(0,liczebność,0)
              plot(wsp_x,licz_y, type="l",lwd=2) 
              #w funkcji plot - typ "l" aby była linia
              #lwd - grubość linii

hist(małe_spółki$WskaźnikCZ, breaks=kr,xlim=c(0,85), ylim=c(0,20),main="Histogram wartości wskaźnika C/Z w grupie małych firm", xlab="Klasy", ylab="Liczebności")
points(wsp_x, licz_y, type="l", lwd=2, col="blue")
#axis(side=1, at=seq(0, 85, by=10))



#ZADANIE 4

#wyznaczamy natężenia liczebności oraz częstości - przyjmiemy 
#h=1 (wówczas natężenia częstości to gęstości)

nat_licz=liczebność/długości_klas
nat_licz
gęst=nat_licz/n
gęst


#podajmy z wartościami nat. liecznoścoi zaokrąglonymi
#do jednego, oraz gęstością do trzech miejsc po przecinku
nowy_szereg_z=data.frame(
  klasy,lewy_kr,prawy_kr,liczebność,gęstość=round(gęst,3)
)
nowy_szereg_z

#write.csv(nowy_szereg_z, "szereg_rozdzielczy_male_gestosci.csv", row.names = FALSE, fileEncoding = "UTF-8")


#Generowanie odpowiedniego histogramu

#w poleceniu hist, parametr freq=F wygeneruje gęstości (podobnie density=T)

hist1=hist(małe_spółki$WskaźnikCZ,breaks=kr,freq=F,labels=T, ylim=c(0,0.04))
hist1$counts
hist1$density

#wielobok gęstości
gęst_y=c(0,gęst,0)
plot(wsp_x,gęst_y, type="l",lwd=2) 

#naniesiemy wielobok na histogram

hist(małe_spółki$WskaźnikCZ,breaks=kr,freq=F, ylim=c(0,0.03), xlim=c(0, 85),
     main="Histogram wartości wskaźnika C/Z w grupie małych firm", xlab="Klasy", ylab="Gęstości")
points(wsp_x,gęst_y,type="l",lwd=2,col="blue")







#ŚREDNIE SPÓŁKI

średnie_spółki <- dane[dane$Wielkość == "średnia", ]
n=length(średnie_spółki$WskaźnikCZ)
k=round(sqrt(n)) 
a=0.01 #to alfa
mi=min(średnie_spółki$WskaźnikCZ)
ma=max(średnie_spółki$WskaźnikCZ)
R=ma-mi
#dlugość przedziału
b=ceiling((R+a/2)/(k*a))*a
b
#wektor krańca przedziałów
kr=mi-a/2+b*c(0:k)
kr

#SZEREG ROZDZIELCZY
klasy=1:k
lewy_kr=kr[1:k]
prawy_kr=kr[2:(k+1)]
lewy_kr
prawy_kr

hist=hist(średnie_spółki$WskaźnikCZ,breaks=kr)
liczebność=hist$counts

#SZEREG ROZDZIELCZY:
Szereg_rozdzielczy=as.data.frame(cbind(klasy,lewy_kr,prawy_kr,liczebność))
Szereg_rozdzielczy

#write.csv(Szereg_rozdzielczy, "szereg_rozdzielczy_srednie.csv", row.names = FALSE, fileEncoding = "UTF-8")

długości_klas=prawy_kr-lewy_kr
długości_klas


#Wieloboki
k_n=length(kr)

nowe_środki=(lewy_kr+prawy_kr)/2
length(nowe_środki)
ppx=kr[1]-długości_klas[1]/2  #wsp. x pierwszego punktu

#UWAGA! sprawdzić czy na pewno 
opx=nowe_środki[length(nowe_środki)]+długości_klas[1] #wsp. x ostatniego punktu - UWAGA! SPRAWDZIĆ
opx

wsp_x=c(ppx,nowe_środki,opx)

#wielobok natężeń liczebności
licz_y=c(0,liczebność,0)
plot(wsp_x,licz_y, type="l",lwd=2) 
#w funkcji plot - typ "l" aby była linia
#lwd - grubość linii

hist(średnie_spółki$WskaźnikCZ, breaks=kr,xlim=c(0,90), ylim=c(0,30),main="Histogram wartości wskaźnika C/Z w grupie średnich firm", xlab="Klasy", ylab="Liczebności")
points(wsp_x, licz_y, type="l", lwd=2, col="green")
#axis(side=1, at=seq(0, 90, by=10))



#ZADANIE 4

#wyznaczamy natężenia liczebności oraz częstości - przyjmiemy 
#h=1 (wówczas natężenia częstości to gęstości)

nat_licz=liczebność/długości_klas
nat_licz
gęst=nat_licz/n
gęst


#podajmy z wartościami nat. liecznoścoi zaokrąglonymi
#do jednego, oraz gęstością do trzech miejsc po przecinku
nowy_szereg_z=data.frame(
  klasy,lewy_kr,prawy_kr,liczebność,gęstość=round(gęst,3)
)
nowy_szereg_z

#write.csv(nowy_szereg_z, "szereg_rozdzielczy_srednie_gestosci.csv", row.names = FALSE, fileEncoding = "UTF-8")


#Generowanie odpowiedniego histogramu

#w poleceniu hist, parametr freq=F wygeneruje gęstości (podobnie density=T)

hist1=hist(średnie_spółki$WskaźnikCZ,breaks=kr,freq=F,labels=T, ylim=c(0,0.04))
hist1$counts
hist1$density

#wielobok gęstości
gęst_y=c(0,gęst,0)
plot(wsp_x,gęst_y, type="l",lwd=2) 

#naniesiemy wielobok na histogram

hist(średnie_spółki$WskaźnikCZ,breaks=kr,freq=F, ylim=c(0,0.035), xlim=c(0, 90),
     main="Histogram wartości wskaźnika C/Z w grupie średnich firm", xlab="Klasy", ylab="Gęstości")
points(wsp_x,gęst_y,type="l",lwd=2,col="green")




#DUŻE SPÓŁKI

duże_spółki <- dane[dane$Wielkość == "duża", ]
n=length(duże_spółki$WskaźnikCZ)
k=round(sqrt(n)) 
a=0.01 #to alfa
mi=min(duże_spółki$WskaźnikCZ)
ma=max(duże_spółki$WskaźnikCZ)
R=ma-mi
#dlugość przedziału
b=ceiling((R+a/2)/(k*a))*a
b
#wektor krańca przedziałów
kr=mi-a/2+b*c(0:k)
kr

#SZEREG ROZDZIELCZY
klasy=1:k
lewy_kr=kr[1:k]
prawy_kr=kr[2:(k+1)]
lewy_kr
prawy_kr

hist=hist(duże_spółki$WskaźnikCZ,breaks=kr)
liczebność=hist$counts

#SZEREG ROZDZIELCZY:
Szereg_rozdzielczy=as.data.frame(cbind(klasy,lewy_kr,prawy_kr,liczebność))
Szereg_rozdzielczy

#write.csv(Szereg_rozdzielczy, "szereg_rozdzielczy_duze.csv", row.names = FALSE, fileEncoding = "UTF-8")

długości_klas=prawy_kr-lewy_kr
długości_klas


#Wieloboki
k_n=length(kr)

nowe_środki=(lewy_kr+prawy_kr)/2
length(nowe_środki)
ppx=kr[1]-długości_klas[1]/2  #wsp. x pierwszego punktu

#UWAGA! sprawdzić czy na pewno 
opx=nowe_środki[length(nowe_środki)]+długości_klas[1] #wsp. x ostatniego punktu - UWAGA! SPRAWDZIĆ
opx

wsp_x=c(ppx,nowe_środki,opx)

#wielobok natężeń liczebności
licz_y=c(0,liczebność,0)
plot(wsp_x,licz_y, type="l",lwd=2) 
#w funkcji plot - typ "l" aby była linia
#lwd - grubość linii

hist(duże_spółki$WskaźnikCZ, breaks=kr,xlim=c(0,110), ylim=c(0,15),main="Histogram wartości wskaźnika C/Z w grupie dużych firm", xlab="Klasy", ylab="Liczebności")
points(wsp_x, licz_y, type="l", lwd=2, col="red")
#axis(side=1, at=seq(0, 90, by=10))



#ZADANIE 4

#wyznaczamy natężenia liczebności oraz częstości - przyjmiemy 
#h=1 (wówczas natężenia częstości to gęstości)

nat_licz=liczebność/długości_klas
nat_licz
gęst=nat_licz/n
gęst


#podajmy z wartościami nat. liecznoścoi zaokrąglonymi
#do jednego, oraz gęstością do trzech miejsc po przecinku
nowy_szereg_z=data.frame(
  klasy,lewy_kr,prawy_kr,liczebność,gęstość=round(gęst,3)
)
nowy_szereg_z

#write.csv(nowy_szereg_z, "szereg_rozdzielczy_duze_gestosci.csv", row.names = FALSE, fileEncoding = "UTF-8")


#Generowanie odpowiedniego histogramu

#w poleceniu hist, parametr freq=F wygeneruje gęstości (podobnie density=T)

hist1=hist(duże_spółki$WskaźnikCZ,breaks=kr,freq=F,labels=T, ylim=c(0,0.04))
hist1$counts
hist1$density

#wielobok gęstości
gęst_y=c(0,gęst,0)
plot(wsp_x,gęst_y, type="l",lwd=2, col="red") 

#naniesiemy wielobok na histogram

hist(duże_spółki$WskaźnikCZ,breaks=kr,freq=F, ylim=c(0,0.02), xlim=c(0, 110),
     main="Histogram wartości wskaźnika C/Z w grupie dużych firm", xlab="Klasy", ylab="Gęstości")
points(wsp_x,gęst_y,type="l",lwd=2,col="red")







#PORÓWNYWANIE:
#MAŁE
małe_spółki <- dane[dane$Wielkość == "mała", ]
n_m=length(małe_spółki$WskaźnikCZ)
k_m=round(sqrt(n_m)) 
a=0.01 #to alfa
mi_m=min(małe_spółki$WskaźnikCZ)
ma_m=max(małe_spółki$WskaźnikCZ)
R_m=ma_m-mi_m
#dlugość przedziału
b_m=ceiling((R_m+a/2)/(k_m*a))*a
b_m
#wektor krańca przedziałów
kr_m=mi_m-a/2+b_m*c(0:k_m)
kr_m
#SZEREG ROZDZIELCZY
hist=hist(małe_spółki$WskaźnikCZ,breaks=kr_m)
liczebność_m=hist$counts
klasy_m=1:k_m
lewy_kr_m=kr_m[1:k_m]
prawy_kr_m=kr_m[2:(k_m+1)]
lewy_kr_m
prawy_kr_m
długości_klas_m=prawy_kr_m-lewy_kr_m
długości_klas_m
#Wieloboki
k_n_m=length(kr_m)
nowe_środki_m=(lewy_kr_m+prawy_kr_m)/2
length(nowe_środki_m)
ppx_m=kr_m[1]-długości_klas_m[1]/2  #wsp. x pierwszego punktu
#UWAGA! sprawdzić czy na pewno 
opx_m=nowe_środki_m[length(nowe_środki_m)]+długości_klas_m[1] #wsp. x ostatniego punktu - UWAGA! SPRAWDZIĆ
opx_m
wsp_x_m=c(ppx_m,nowe_środki_m,opx_m)
#ZADANIE 4
#wyznaczamy natężenia liczebności oraz częstości - przyjmiemy 
#h=1 (wówczas natężenia częstości to gęstości)

nat_licz_m=liczebność_m/długości_klas_m
nat_licz_m
gęst_m=nat_licz_m/n_m
gęst_m

#wielobok gęstości
gęst_y_m=c(0,gęst_m,0)
plot(wsp_x_m,gęst_y_m, type="l",lwd=2, col="blue") 


#ŚREDNIE
średnie_spółki <- dane[dane$Wielkość == "średnia", ]
n_s=length(średnie_spółki$WskaźnikCZ)
k_s=round(sqrt(n_s)) 
a=0.01 #to alfa
mi_s=min(średnie_spółki$WskaźnikCZ)
ma_s=max(średnie_spółki$WskaźnikCZ)
R_s=ma_s-mi_s
#dlugość przedziału
b_s=ceiling((R_s+a/2)/(k_s*a))*a
b_s
#wektor krańca przedziałów
kr_s=mi_s-a/2+b_s*c(0:k_s)
kr_s
#SZEREG ROZDZIELCZY
hist=hist(średnie_spółki$WskaźnikCZ,breaks=kr_s)
liczebność_s=hist$counts
klasy_s=1:k_s
lewy_kr_s=kr_s[1:k_s]
prawy_kr_s=kr_s[2:(k_s+1)]
lewy_kr_s
prawy_kr_s
długości_klas_s=prawy_kr_s-lewy_kr_s
długości_klas_s
#Wieloboki
k_n_s=length(kr_s)
nowe_środki_s=(lewy_kr_s+prawy_kr_s)/2
length(nowe_środki_s)
ppx_s=kr_s[1]-długości_klas_s[1]/2  #wsp. x pierwszego punktu
#UWAGA! sprawdzić czy na pewno 
opx_s=nowe_środki_s[length(nowe_środki_s)]+długości_klas_s[1] #wsp. x ostatniego punktu - UWAGA! SPRAWDZIĆ
opx_s
wsp_x_s=c(ppx_s,nowe_środki_s,opx_s)
#ZADANIE 4
#wyznaczamy natężenia liczebności oraz częstości - przyjmiemy 
#h=1 (wówczas natężenia częstości to gęstości)

nat_licz_s=liczebność_s/długości_klas_s
nat_licz_s
gęst_s=nat_licz_s/n_s
gęst_s

#wielobok gęstości
gęst_y_s=c(0,gęst_s,0)
plot(wsp_x_s,gęst_y_s, type="l",lwd=2, col="green") 



#DUŻE
duże_spółki <- dane[dane$Wielkość == "duża", ]
n_d=length(duże_spółki$WskaźnikCZ)
k_d=round(sqrt(n_d)) 
a=0.01 #to alfa
mi_d=min(duże_spółki$WskaźnikCZ)
ma_d=max(duże_spółki$WskaźnikCZ)
R_d=ma_d-mi_d
#dlugość przedziału
b_d=ceiling((R_d+a/2)/(k_d*a))*a
b_d
#wektor krańca przedziałów
kr_d=mi_d-a/2+b_d*c(0:k_d)
kr_d
#SZEREG ROZDZIELCZY
hist=hist(duże_spółki$WskaźnikCZ,breaks=kr_d)
liczebność_d=hist$counts
klasy_d=1:k_d
lewy_kr_d=kr_d[1:k_d]
prawy_kr_d=kr_d[2:(k_d+1)]
lewy_kr_d
prawy_kr_d
długości_klas_d=prawy_kr_d-lewy_kr_d
długości_klas_d
#Wieloboki
k_n_d=length(kr_d)
nowe_środki_d=(lewy_kr_d+prawy_kr_d)/2
length(nowe_środki_d)
ppx_d=kr_d[1]-długości_klas_d[1]/2  #wsp. x pierwszego punktu
#UWAGA! sprawdzić czy na pewno 
opx_d=nowe_środki_d[length(nowe_środki_d)]+długości_klas_d[1] #wsp. x ostatniego punktu - UWAGA! SPRAWDZIĆ
opx_d
wsp_x_d=c(ppx_d,nowe_środki_d,opx_d)
#ZADANIE 4
#wyznaczamy natężenia liczebności oraz częstości - przyjmiemy 
#h=1 (wówczas natężenia częstości to gęstości)

nat_licz_d=liczebność_d/długości_klas_d
nat_licz_d
gęst_d=nat_licz_d/n_d
gęst_d

#wielobok gęstości
gęst_y_d=c(0,gęst_d,0)
plot(wsp_x_d,gęst_y_d, type="l",lwd=2, col="red") 


plot(wsp_x_m,gęst_y_m, type="l",lwd=2, col="blue",ylim=c(0,0.04), xlim=c(0,110), 
     main="Wieloboki wskaźnika C/Z", xlab="Wartości wskaźnika", ylab="Wartości wieloboku gęstości") 
points(wsp_x_s,gęst_y_s, type="l",lwd=2, col="green") 
points(wsp_x_d,gęst_y_d, type="l",lwd=2, col="red")
legend("topright", legend=c("Grupa małych firm", "Grupa średnich firm", "Grupa dużych firm"),
       col=c("blue", "green", "red"), lwd=2, bty="n")



#ZADANIE 6
#ŚREDNIE

sum(nowe_środki_m*liczebność_m)/n_m
sum(nowe_środki_s*liczebność_s)/n_s
sum(nowe_środki_d*liczebność_d)/n_d

#ODCHYLENIE
średnia_zgr_m <- sum(nowe_środki_m*liczebność_m)/n_m
wariancja_m <- sum(liczebność_m * (nowe_środki_m - średnia_zgr_m)^2) / n_m
odchylenie_standardowe_m <- sqrt(wariancja_m)
odchylenie_standardowe_m

średnia_zgr_s <- sum(nowe_środki_s*liczebność_s)/n_s
wariancja_s <- sum(liczebność_s * (nowe_środki_s - średnia_zgr_s)^2) / n_s
odchylenie_standardowe_s <- sqrt(wariancja_s)
odchylenie_standardowe_s

średnia_zgr_d <- sum(nowe_środki_d*liczebność_d)/n_d
wariancja_d <- sum(liczebność_d * (nowe_środki_d - średnia_zgr_d)^2) / n_d
odchylenie_dtandardowe_d <- sqrt(wariancja_d)
odchylenie_dtandardowe_d

#MEDIANA

#MAŁE
#n - parzysta, czyli
mediana_pozycja_m=n_m/2+1
suma_liczebność_m=cumsum(liczebność_m)
klasa_mediana_m=which(suma_liczebność_m>=mediana_pozycja_m)[1]
mediana_m=lewy_kr_m[4]+(b_m/liczebność_m[4])*(n_m/2-sum(liczebność_m[1:(klasa_mediana_m-1)]))

#ŚREDNIE
mediana_pozycja_s=(n_s+1)/2
suma_liczebność_s=cumsum(liczebność_s)
klasa_mediana_s=which(suma_liczebność_s>=mediana_pozycja_s)[1]
mediana_s=lewy_kr_s[klasa_mediana_s]+(b_s/liczebność_s[klasa_mediana_s])*(n_s/2-sum(liczebność_s[1:(klasa_mediana_s-1)]))
mediana_s

# DUŻE
mediana_pozycja_d=(n_d+1)/2
suma_liczebność_d=cumsum(liczebność_d)
klasa_mediana_d=which(suma_liczebność_d>=mediana_pozycja_d)[1]
mediana_d=lewy_kr_d[klasa_mediana_d]+(b_d/liczebność_d[klasa_mediana_d])*(n_d/2-sum(liczebność_d[1:(klasa_mediana_d-1)]))
mediana_d

#WSPÓŁCZYNNIK ASYMETRII
e3_m=sum(liczebność_m * (nowe_środki_m - średnia_zgr_m)^3) / n_m
wsp_m=e3_m/odchylenie_standardowe_m^3
#KURTOZA
e4_m=sum(liczebność_m * (nowe_środki_m - średnia_zgr_m)^4) / n_m
kurtoza_m=e4_m/odchylenie_standardowe_m^4-3

#WSPÓŁCZYNNIK ASYMETRII ŚREDNIE
e3_s=sum(liczebność_s * (nowe_środki_s - średnia_zgr_s)^3) / n_s
wsp_s=e3_s/odchylenie_standardowe_s^3
#KURTOZA
e4_s=sum(liczebność_s * (nowe_środki_s - średnia_zgr_s)^4) / n_s
kurtoza_s=e4_s/odchylenie_standardowe_s^4-3

#WSPÓŁCZYNNIK ASYMETRII DUŻE
e3_d=sum(liczebność_d * (nowe_środki_d - średnia_zgr_d)^3) / n_d
wsp_d=e3_d/odchylenie_dtandardowe_d^3
#KURTOZA
e4_d=sum(liczebność_d * (nowe_środki_d - średnia_zgr_d)^4) / n_d
kurtoza_d=e4_d/odchylenie_dtandardowe_d^4-3


#KWARTYLE MAŁE
n_m  # całkowita liczebność
kwartyl1_pozycja_m <- n_m/4
kwartyl3_pozycja_m <- 3*n_m/4
# Znajdowanie indeksu klasy l dla Q1
suma_liczebność_m <- cumsum(liczebność_m)  # skumulowane liczebności
l_m <- which(suma_liczebność_m >= kwartyl1_pozycja_m)[1]
p_m <- which(suma_liczebność_m >= kwartyl3_pozycja_m)[1]
lewy_kr_m
# Wartości potrzebne do obliczeń
lewy_kr_m_l <- lewy_kr_m[l_m]  # dolna granica l-tej klasy
b_m    # szerokość l-tej klasy
n_l_m <- liczebność[l_m]   # liczebność l-tej klasy
lewy_kr_m_p <- lewy_kr_m[p_m]  # dolna granica p-tej klasy
b_m    # szerokość p-tej klasy
n_p_m <- liczebność[p_m]   # liczebność p-tej klasy
# Obliczenia dla Q1
Q1_m <- lewy_kr_m_l + (b_m / n_l_m) * (n_m/4 - sum(liczebność_m[1:(l_m-1)]))
# Obliczenia dla Q3
Q3_m <- lewy_kr_m_p + (b_m / n_p_m) * (3*n_m/4 - sum(liczebność_m[1:(p_m-1)]))
# Wyświetlenie wyników
Q1_m
Q3_m

#KWARTYLE ŚREDNIE
n_s  # całkowita liczebność
kwartyl1_pozycja_s <- n_s/4
kwartyl3_pozycja_s <- 3*n_s/4
# Znajdowanie indeksu klasy l dla Q1
suma_liczebność_s <- cumsum(liczebność_s)  # skumulowane liczebności
l_s <- which(suma_liczebność_s >= kwartyl1_pozycja_s)[1]
p_s <- which(suma_liczebność_s >= kwartyl3_pozycja_s)[1]
lewy_kr_s
# Wartości potrzebne do obliczeń
lewy_kr_s_l <- lewy_kr_s[l_s]  # dolna granica l-tej klasy
b_s    # szerokość l-tej klasy
n_l_s <- liczebność[l_s]   # liczebność l-tej klasy
lewy_kr_s_p <- lewy_kr_s[p_s]  # dolna granica p-tej klasy
b_s    # szerokość p-tej klasy
n_p_s <- liczebność[p_s]   # liczebność p-tej klasy
# Obliczenia dla Q1
Q1_s <- lewy_kr_s_l + (b_s / n_l_s) * (n_s/4 - sum(liczebność_s[1:(l_s-1)]))
# Obliczenia dla Q3
Q3_s <- lewy_kr_s_p + (b_s / n_p_s) * (3*n_s/4 - sum(liczebność_s[1:(p_s-1)]))
# Wyświetlenie wyników
Q1_s
Q3_s

#KWARTYLE DUŻE
n_d  # całkowita liczebność
kwartyl1_pozycja_d <- n_d/4
kwartyl3_pozycja_d <- 3*n_d/4
# Znajdowanie indeksu klasy l dla Q1
suma_liczebność_d <- cumsum(liczebność_d)  # skumulowane liczebności
l_d <- which(suma_liczebność_d >= kwartyl1_pozycja_d)[1]
p_d <- which(suma_liczebność_d >= kwartyl3_pozycja_d)[1]
lewy_kr_d
# Wartości potrzebne do obliczeń
lewy_kr_d_l <- lewy_kr_d[l_d]  # dolna granica l-tej klasy
b_d    # szerokość l-tej klasy
n_l_d <- liczebność[l_d]   # liczebność l-tej klasy
lewy_kr_d_p <- lewy_kr_d[p_d]  # dolna granica p-tej klasy
b_d    # szerokość p-tej klasy
n_p_d <- liczebność[p_d]   # liczebność p-tej klasy
# Obliczenia dla Q1
Q1_d <- lewy_kr_d_l + (b_d / n_l_d) * (n_d/4 - sum(liczebność_d[1:(l_d-1)]))
# Obliczenia dla Q3
Q3_d <- lewy_kr_d_p + (b_d / n_p_d) * (3*n_d/4 - sum(liczebność_d[1:(p_d-1)]))
# Wyświetlenie wyników
Q1_d
Q3_d



#KWARTYLE Q1 MAŁE
kwantyl_pozycja_m=(n_m+1)/4
suma_liczebność_m=cumsum(liczebność_m)
klasa_kwantyl_m=which(suma_liczebność_m>=kwantyl_pozycja_m)[1]
Q1_m=lewy_kr_m[klasa_kwantyl_m]+(b_m/liczebność_m[klasa_kwantyl_m])*(n_m/4-sum(liczebność_m[1:(klasa_kwantyl_m-1)]))
Q1_m

#ŚREDNIE
kwantyl_pozycja_s=(n_s+1)/4
suma_liczebność_s=cumsum(liczebność_s)
klasa_kwantyl_s=which(suma_liczebność_s>=kwantyl_pozycja_s)[1]
Q1_s=lewy_kr_s[klasa_kwantyl_s]+(b_s/liczebność_s[klasa_kwantyl_s])*(n_s/4-sum(liczebność_s[1:(klasa_kwantyl_s-1)]))
Q1_s

#DUŻE
kwantyl_pozycja_d=(n_d+1)/4
suma_liczebność_d=cumsum(liczebność_d)
klasa_kwantyl_d=which(suma_liczebność_d>=kwantyl_pozycja_d)[1]
Q1_d=lewy_kr_d[klasa_kwantyl_d]+(b_d/liczebność_d[klasa_kwantyl_d])*(n_d/4-sum(liczebność_d[1:(klasa_kwantyl_d-1)]))
Q1_d

#KWANTYLE Q3 MAŁE
kwantyl_pozycja_m=(3*(n_m+1))/4
suma_liczebność_m=cumsum(liczebność_m)
klasa_kwantyl_m=which(suma_liczebność_m>=kwantyl_pozycja_m)[1]
Q1_m=lewy_kr_m[klasa_kwantyl_m]+(b_m/liczebność_m[klasa_kwantyl_m])*((3*n_m)/4-sum(liczebność_m[1:(klasa_kwantyl_m-1)]))
Q1_m

#ŚREDNIE
kwantyl_pozycja_s=(3*(n_s+1))/4
suma_liczebność_s=cumsum(liczebność_s)
klasa_kwantyl_s=which(suma_liczebność_s>=kwantyl_pozycja_s)[1]
Q1_s=lewy_kr_s[klasa_kwantyl_s]+(b_s/liczebność_s[klasa_kwantyl_s])*((3*n_s)/4-sum(liczebność_s[1:(klasa_kwantyl_s-1)]))
Q1_s

#DUŻE
kwantyl_pozycja_d=(3*(n_d+1))/4
suma_liczebność_d=cumsum(liczebność_d)
klasa_kwantyl_d=which(suma_liczebność_d>=kwantyl_pozycja_d)[1]
Q1_d=lewy_kr_d[klasa_kwantyl_d]+(b_d/liczebność_d[klasa_kwantyl_d])*((3*n_d)/4-sum(liczebność_d[1:(klasa_kwantyl_d-1)]))
Q1_d
