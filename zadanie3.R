
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



#FED789FF



