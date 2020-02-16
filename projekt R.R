data_frame <- function(index){
  df <- data.frame()
  start=3+14*(index -1)
  end=16+14*(index -1)
  
  for(i in 1:12)
    df <- rbind(df,data.frame(matrix(unlist(Data[(start+14*10*(i-1)):(end+14*10*(i-1))]),nrow = 17)))
  
  return(df)
}

usrednianie_cen_produktow <- function(Frame){
  produkt <- data.frame()
  for(j in 1:17){
    zakres <- list(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    for(i in 1:12){
      zakres <- mapply("+", zakres, Frame[j+17*(i-1),] , SIMPLIFY = FALSE)
    }
    zmienna_pomocnicza <- data.frame(matrix(unlist(zakres),nrow=1)/12)
    produkt <- rbind(produkt,zmienna_pomocnicza)
  }
  
  return (produkt)
}



Data <-(read.csv(file = ".\\ceny.csv",header = TRUE,sep = ';', dec = ',', stringsAsFactors = FALSE, encoding = 'UTF-8'))
wojewodztwo <- c("Polska", "Dolnoslaskie","Kujawsko-Pomorskie","Lubelskie","Lubuskie","Lodzkie","Malopolskie",
                 "Mazowieckie","Opolskie","Podkarpackie","Podlaskie","Pomorskie","Slaskie","Swietokrzyskie",
                 "Warminsko-Mazurskie","Wielkopolskie","Zachodniopomorskie")
rocznik <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")



maka  <- data_frame(1)
maka_srednia <- usrednianie_cen_produktow(maka)
print("Srednia cena maki pszennej w latach 2006-2019 wynosi:")
print(maka_srednia)

wolowe_bez_kosci <- data_frame(2)
wolowe_bez_kosci_srednia <- usrednianie_cen_produktow(wolowe_bez_kosci)
print("Srednia cena miesa wolowego bez kosci w latach 2006-2019 wynosi: ")
print(wolowe_bez_kosci_srednia)

wieprzowe_bez_kosci <- data_frame(3)
wieprzowe_bez_kosci_srednia <- usrednianie_cen_produktow(wieprzowe_bez_kosci)
print("Srednia cena miesa wieprzowego bez kosci w latach 2006-2019 wynosi: ")
print(wieprzowe_bez_kosci_srednia)

karp <- data_frame(4)
karp_srednia <- usrednianie_cen_produktow(karp)
print("Srednia cena karpia w latach 2006-2019 wynosi: ")
print(karp_srednia)

mleko_3_35_procenta <- data_frame(5)
mleko_3_35_procenta_srednia <- usrednianie_cen_produktow(mleko_3_35_procenta)
print("Srednia cena mleka 3%-3,5% w latach 2006-2019 wynosi: ")
print(mleko_3_35_procenta_srednia)

maslo <- data_frame(6)
maslo_srednia <- usrednianie_cen_produktow(maslo)
print("Srednia cena masla w latach 2006-2019 wynosi: ")
print(maslo_srednia)

Pomarancze <- data_frame(7)
Pomarancze_srednia <- usrednianie_cen_produktow(Pomarancze)
print("Srednia cena pomaranczy w latach 2006-2019 wynosi: ")
print(Pomarancze_srednia)

herbata_czarna <- data_frame(8)
herbata_czarna_srednia <- usrednianie_cen_produktow(herbata_czarna)
print("Srednia cena herbaty czarnej w latach 2006-2019 wynosi: ")
print(herbata_czarna_srednia)

prosze_do_prania <- data_frame(9)
prosze_do_prania_srednia <- usrednianie_cen_produktow(prosze_do_prania)
print("Srednia cena proszki do prania w latach 2006-2019 wynosi: ")
print(prosze_do_prania_srednia)

bilet_kino <- data_frame(10)
bilet_kino_srednia <- usrednianie_cen_produktow(bilet_kino)
print("Srednia cena biletow do kina w latach 2006-2019 wynosi: ")
print(bilet_kino_srednia)