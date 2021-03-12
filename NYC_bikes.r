### Praca projektowa nr 3
### Rozwiązanie zadan
### Dawid Janus 305742 Maciek Chylak 305699 kierunek IAD

library(dplyr)
library(ggplot2)
library(stringi)
library(leaflet)
library(forcats)
library(ggpubr)

options(stringsAsFactors = FALSE)

bledne_dane_JC<-function(arg){
  df<- na.omit(arg) %>% group_by(birth.year) %>% count() %>% filter(n>20) %>% select(birth.year)
  bez_bledow <- filter(arg,arg$birth.year %in% df$birth.year)
  bez_bledow
}

bledne_dane_NYC<-function(arg){
  df<- na.omit(arg) %>% group_by(birth.year) %>% count() %>% filter(n>1100) %>% select(birth.year)
  bez_bledow <- filter(arg,arg$birth.year %in% df$birth.year)
  bez_bledow
}

NYC_2020_kwiecien <- read.csv("rowery/202004-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2020_marzec <- read.csv("rowery/202003-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2020_luty <- read.csv("rowery/202002-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2018_sierpien <- read.csv("rowery/201808-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2019_sierpien <- read.csv("rowery/201908-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2018_luty <- read.csv("rowery/201802-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2019_luty <- read.csv("rowery/201902-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2019_marzec <- read.csv("rowery/201903-citibike-tripdata.csv") %>% bledne_dane_NYC()
NYC_2019_kwiecien <- read.csv("rowery/201904-citibike-tripdata.csv") %>% bledne_dane_NYC()
JC_2020_kwiecien <- read.csv("rowery/JC-202004-citibike-tripdata.csv") %>% bledne_dane_JC()
JC_2020_marzec <- read.csv("rowery/JC-202003-citibike-tripdata.csv") %>% bledne_dane_JC()
JC_2020_luty <- read.csv("rowery/JC-202002-citibike-tripdata.csv")%>% bledne_dane_JC()
JC_2019_kwiecien <- read.csv("rowery/JC-201904-citibike-tripdata.csv") %>% bledne_dane_JC()
JC_2019_marzec <- read.csv("rowery/JC-201903-citibike-tripdata.csv") %>% bledne_dane_JC()
JC_2019_luty <- read.csv("rowery/JC-201902-citibike-tripdata.csv") %>% bledne_dane_JC()




pomocnicza_wiek<-function(arg,czas_czy_ilosc){
if(czas_czy_ilosc==0){
df<- group_by(arg,birth.year) %>% count() %>% rename(c(Ilosc_Wycieczek=n))%>% 
mutate(Wiek=abs(birth.year-2020)) %>% ungroup() %>% select(c(Ilosc_Wycieczek, Wiek))}
else{
  df<-group_by(arg,birth.year) %>% select(tripduration, birth.year)  %>% 
    summarise(tripduration=mean(tripduration)) %>% rename(c(Dlugosc_Wycieczek=tripduration)) %>%
    mutate(Wiek=abs(birth.year-2020))}
df
}




facet_wiek<-function(a,b,c,d,e,f,czas_czy_ilosc,tytul){
  df1<-pomocnicza_wiek(a,czas_czy_ilosc)
  df2<-pomocnicza_wiek(b,czas_czy_ilosc)
  df3<-pomocnicza_wiek(c,czas_czy_ilosc)
  df4<-pomocnicza_wiek(d,czas_czy_ilosc)
  df5<-pomocnicza_wiek(e,czas_czy_ilosc)
  df6<-pomocnicza_wiek(f,czas_czy_ilosc)
  
  df1<-bind_cols(df1,data.frame(Data=rep(("Luty 2019"),length(df1$Wiek))))
  df2<-bind_cols(df2,data.frame(Data=rep(("Marzec 2019"),length(df2$Wiek))))
  df3<-bind_cols(df3,data.frame(Data=rep(("Kwiecien 2019"),length(df3$Wiek))))
  df4<-bind_cols(df4,data.frame(Data=rep(("Luty 2020"),length(df4$Wiek))))
  df5<-bind_cols(df5,data.frame(Data=rep(("Marzec 2020"),length(df5$Wiek))))
  df6<-bind_cols(df6,data.frame(Data=rep(("Kwiecien 2020"),length(df6$Wiek))))
  
  df7<-bind_rows(df1,df2)%>%bind_rows(df3)%>%bind_rows(df4)%>%bind_rows(df5)%>%bind_rows(df6)%>%
    mutate(Data = fct_relevel(Data,"Luty 2019", "Marzec 2019", "Kwiecien 2019","Luty 2020", "Marzec 2020", "Kwiecien 2020"))
  if(czas_czy_ilosc==0){
  ggplot(data=df7, aes(x=Wiek, y=Ilosc_Wycieczek)) + geom_point(size=0.5, color="red4")+
    labs(title = tytul)+ theme(plot.title = element_text(hjust = 0.5))+ facet_wrap(vars(Data))}
  else{
    ggplot(data=df7, aes(x=Wiek, y=Dlugosc_Wycieczek)) + geom_point(size=0.5, color="red4")+
      labs(title = tytul)+ theme(plot.title = element_text(hjust = 0.5))+ facet_wrap(vars(Data))}
    
  }
  




pomocnicza_plec<-function(arg, czas_czy_ilosc){
  if(czas_czy_ilosc==0){
  df<- group_by(arg,gender) %>% count() %>% rename(c(Ilosc_Wycieczek=n))}
  else{
    df<- group_by(arg,gender) %>% select(tripduration, gender)  %>% 
      summarise(tripduration=mean(tripduration)) %>% rename(c(Dlugosc_Wycieczek=tripduration)) 
    
  }
  df$gender[which(df$gender==0)]<-"Nieokresleni"
  df$gender[which(df$gender==1)]<-"Mezczyzni"
  df$gender[which(df$gender==2)]<-"Kobiety"
  df
}





ilosc_czas_plec<- function(arg, arg2,arg3,czas_czy_ilosc,tytul){
  if(czas_czy_ilosc==0){
  df<-pomocnicza_plec(arg,czas_czy_ilosc)
  df1<-pomocnicza_plec(arg2,czas_czy_ilosc)
  df2<-pomocnicza_plec(arg3,czas_czy_ilosc)}
  else{
    df<-pomocnicza_plec(arg,czas_czy_ilosc)
    df1<-pomocnicza_plec(arg2,czas_czy_ilosc)
    df2<-pomocnicza_plec(arg3, czas_czy_ilosc)}
  
  
  df<-bind_cols(data.frame(Miesiac=rep(("II"),3)),df)
  df1<-bind_cols(data.frame(Miesiac=rep(("III"),3)),df1)
  df2<-bind_cols(data.frame(Miesiac=rep(("IV"),3)),df2)
  df3<-bind_rows(df,df1)%>% bind_rows(df2) %>% rename(plec=gender) %>% 
    mutate(Data = fct_relevel(Miesiac,"II", "III", "IV"))
  
  if(czas_czy_ilosc==0){
  ggplot(data=df3)+
  geom_bar(stat= "identity", mapping = aes(x=Miesiac, y=Ilosc_Wycieczek,fill=plec), position = "dodge")+
  labs(title = tytul)+ theme(plot.title = element_text(hjust = 0.5))}
  
  else{
  ggplot(data=df3)+
  geom_bar(stat= "identity", mapping = aes(x=Miesiac, y=Dlugosc_Wycieczek,fill=plec), position = "dodge") +
    labs(title = tytul)+ theme(plot.title = element_text(hjust = 0.5))} 
}

pomocnicza_hour<-function(x){
  x1 <- mutate(x, hour = stri_extract_first_regex(x$starttime,"(?<=^.{11}).{2}"))
  x1 <- count(x1, x1$hour)
  x1 <- x1 %>% rename(c("godzina" = "x1$hour", "liczba_przejazdow" = "n"))
  x1
  
}

starttime_hour <- function(a,b,c,d,e,f,tytul){
  df1<-pomocnicza_hour(a)
  df2<-pomocnicza_hour(b)
  df3<-pomocnicza_hour(c)
  df4<-pomocnicza_hour(d)
  df5<-pomocnicza_hour(e)
  df6<-pomocnicza_hour(f)
  
  df1<-bind_cols(df1,data.frame(Data=rep(("Luty 2019"),length(df1$godzina))))
  df2<-bind_cols(df2,data.frame(Data=rep(("Marzec 2019"),length(df2$godzina))))
  df3<-bind_cols(df3,data.frame(Data=rep(("Kwiecien 2019"),length(df3$godzina))))
  df4<-bind_cols(df4,data.frame(Data=rep(("Luty 2020"),length(df4$godzina))))
  df5<-bind_cols(df5,data.frame(Data=rep(("Marzec 2020"),length(df5$godzina))))
  df6<-bind_cols(df6,data.frame(Data=rep(("Kwiecien 2020"),length(df6$godzina))))
  df7<-bind_rows(df1,df2)%>%bind_rows(df3)%>%bind_rows(df4)%>%bind_rows(df5)%>%bind_rows(df6)%>%
    mutate(Data = fct_relevel(Data,"Luty 2019", "Luty 2020", "Marzec 2019","Marzec 2020", "Kwiecien 2019", "Kwiecien 2020"))
  
  ggplot(data = df7) + 
    geom_point(color="red4",mapping = aes(x = godzina, y = liczba_przejazdow), stat = "identity") +
    labs(title = tytul) + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(vars(Data),nrow=3)
}

#base
y <- mutate(NYC_2019_marzec, date = sprintf("%s", stri_extract_first_regex(NYC_2019_marzec$starttime,"^.{10}")))
y <- count(y, y$date)
y <- mutate(y, avg_temp = c(1, 2, 3, 1, 0, -3.5, -3, -2.5, 0 ,4, 4, 8, 4, 4, 10, 15, 9, 5, 5, 6, 9, 7, 6, 11, 10, 6, 6, 8, 12.5, 14, 14))
y <- mutate(y, rain= c("snieg","snieg","snieg","snieg","bez","bez","bez","bez","bez","lekki_deszcz","bez","bez","bez","bez","lekki_deszcz", "bez","bez","bez","bez","bez","mocny_deszcz","mocny_deszcz","bez", "bez","bez","bez", "bez","bez","bez", "bez", "lekki_deszcz" ))
y <- mutate(y, day_of_week = c( "fri", "sat", "sun", rep(c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 4)))
y <- y %>% rename(c("dzien" = "y$date"))

x <- mutate(NYC_2019_kwiecien, date = sprintf("%s", stri_extract_first_regex(NYC_2019_kwiecien$starttime,"^.{10}")))
x <- count(x, x$date)
x <- mutate(x, avg_temp = c(4.5, 8, 13, 12, 4.5, 13, 15.5, 20, 9, 13, 10.5, 15.5, 21, 19.5, 12.5, 13, 15, 15, 21, 15.5, 15.5, 15.5, 18, 18, 13, 13, 12, 12, 12.5, 12.5))
x <- mutate(x, rain= c("bez","bez","bez","bez","lekki_deszcz","bez","bez","bez","bez","bez","bez","lekki_deszcz","bez","bez","lekki_deszcz", "bez","lekki_deszcz","bez","bez","bez","bez","lekki_deszcz","bez","bez","bez","lekki_deszcz", "bez","bez","bez","lekki_deszcz" ))
x <- mutate(x, day_of_week = c(rep(c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 4), "mon", "tue"))
x <- x %>% rename(c("dzien" = "x$date"))

#base
#sorted
arrange(x, desc(n)) 
#sorted
#sortowanie po sredniej temp
subscriber_type1 <- function(x){
  count(x,  x$usertype) %>% arrange(desc(n))
}

subscriber_type <-function(x, y, z){
  
  kwiecien <- as.numeric(subscriber_type1(x)$n)
  marzec <- as.numeric(subscriber_type1(y)$n)
  luty <- as.numeric(subscriber_type1(z)$n)
  stats <- data.frame(rok = rep(c("Subscriber", "Customer"), 3), miesiac = c(rep("luty",2), rep("marzec",2), rep("kwiecien", 2)), liczba_przejazdow = c(luty, marzec, kwiecien))
  stats$miesiac <- factor(stats$miesiac, levels = c("luty", "marzec", "kwiecien"))
  ggplot(data = stats) + geom_bar( mapping = aes(x = miesiac, y = liczba_przejazdow, fill = rok), stat = "identity", position = "dodge") + labs(title = sprintf("%s", stri_extract_first_regex(deparse(substitute(x)), "^.{8}"))) + theme(plot.title = element_text(hjust = 0.5))
}


av_temp <- function(x, y){
  x1 <- arrange(x, desc(avg_temp))
  x1 <- mutate(x1, dzien = stri_extract_first_regex(x1$dzien, "(?<=-.{2}-).{2}"))
  x1$dzien <- factor(x1$dzien, levels = x1$dzien)
  x1 <- x1 %>% rename(c("liczba_przejazdow" = "n"))
  x1 <- mutate(x1, temp = ifelse(x1$avg_temp < -5, "<-5", ifelse(x1$avg_temp < 0, "-5-0", 
                                                                 ifelse(x1$avg_temp < 5, "0-5", ifelse(x1$avg_temp < 10, "5-10", ifelse(x1$avg_temp < 15, "10-15", 
                                                                                                                                        ifelse(x1$avg_temp < 20, "15-20", ifelse(x1$avg_temp < 25, "20-25", ">25"))))))))
  x1$temp  <- with(x1, reorder(temp, avg_temp))
  ggplot(data = x1) + geom_histogram(mapping = aes(x = dzien, y = liczba_przejazdow, fill = temp), stat = "identity") + labs(title = sprintf("%s", y)) + theme(plot.title = element_text(hjust = 0.5))
}



rain <- function(x, y){
  x2 <- mutate(x, rain2 = ifelse(x$rain == "bez", 1, ifelse(x$rain == "lekki_deszcz", 2, ifelse((x$rain == "mocny_deszcz"), 3, 4))))
  x2 <- mutate(x2, dzien = stri_extract_first_regex(x2$dzien, "(?<=-.{2}-).{2}"))
  x2 <- arrange(x2, rain)
  x2 <- x2 %>% rename(c("liczba_przejazdow" = "n", "opady" = "rain"))
  x2$dzien <- factor(x2$dzien, levels = x2$dzien)
  ggplot(data = x2) + geom_histogram(mapping = aes(x = dzien, y = liczba_przejazdow, fill = opady), stat = "identity") + labs(title = sprintf("%s", y)) + theme(plot.title = element_text(hjust = 0.5))
}
number_of_routes1 <- function(x){
  x2 <- count(x,  x$start.station.name, x$end.station.name,  x$start.station.latitude, x$start.station.longitude, 
              x$end.station.latitude, x$end.station.longitude) %>% arrange(desc(n))
  x2 <- filter(x2, x2$`x$start.station.name` != "NULL" & x2$`x$end.station.name` != "NULL")
  x2 <- mutate(x2, route = sprintf("%s-%s", x2$`x$start.station.name`, x2$`x$end.station.name`))
  x2 <- select(x2, c("route", "n", sprintf("%s$start.station.latitude", deparse(substitute(x))), 
                     sprintf("%s$start.station.longitude", deparse(substitute(x))), sprintf("%s$end.station.latitude", deparse(substitute(x))), 
                     sprintf("%s$end.station.longitude", deparse(substitute(x)))))
  x2 <-  x2%>% rename(c("liczba_przejazdow" ="n", "start.station.latitude"= sprintf("%s$start.station.latitude", deparse(substitute(x))),  "start.station.longitude"= sprintf("%s$start.station.longitude", deparse(substitute(x))), "end.station.latitude"= sprintf("%s$end.station.latitude", deparse(substitute(x))),  "end.station.longitude"= sprintf("%s$end.station.longitude", deparse(substitute(x))), "trasa" = "route"))
  comment(x2) <- deparse(substitute(x))
  head(x2, 5)
}
number_of_routes_to_plot <- function(x){
  pom <- paste(stri_extract_first_regex(x$trasa, "(^.{6})"), stri_extract_first_regex(x$trasa, "(-.{6})"), sep="")
  x2pom <- mutate(x, trasa = pom)
  ggplot(data = head(x2pom)) + geom_bar(mapping = aes(x = trasa, y = liczba_przejazdow), stat = "identity") + labs(title = comment(x)) + theme(plot.title = element_text(hjust = 0.5))
}

number_of_routes1_to_map <- function(x){
  icon.red1 <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = '#FFFFFF', spin = TRUE)
  icon.green1 <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion', iconColor = '#FFFFFF', spin = TRUE)
  icon.red2 <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = '#11BBAA')
  icon.green2 <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion', iconColor = '#11BBAA')
  icon.red3 <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = '#447744')
  icon.green3 <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion', iconColor = '#447744')
  icon.red4 <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = '#7744FF')
  icon.green4 <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion', iconColor = '#7744FF')
  icon.red5 <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = '#AA00AA')
  icon.green5 <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion', iconColor = '#AA00AA')
  x <- mutate(x, start.station.latitude =  x$start.station.latitude + rnorm(5, mean = 0, sd = 0.00009),
              start.station.longitude = x$start.station.longitude + rnorm(5, mean = 0, sd = 0.00009), end.station.latitude =  x$end.station.latitude + rnorm(5, mean = 0, sd = 0.00009), 
              end.station.longitude = x$end.station.longitude + rnorm(5, mean = 0, sd = 0.00009))
  leaflet() %>% 
    addTiles() %>%  # Add default OpenStreetMap map tiles 
    addAwesomeMarkers(lng=x$start.station.longitude, 
                      lat=x$start.station.latitude, 
                      popup=stri_extract_first_regex(x$route, "^.{8}"), icon = 
                        awesomeIconList(icon.green1, icon.green2, icon.green3, 
                                        icon.green4, icon.green5)) %>% addAwesomeMarkers(lng=x$end.station.longitude, 
                                                                                         lat=x$end.station.latitude, popup=stri_extract_first_regex(x$route, "(?<=-).{8}"), 
                                                                                         icon = awesomeIconList(icon.red1, icon.red2, icon.red3, icon.red4, icon.red5))
}
