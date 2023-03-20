# Load data
## Loading data for NNA
### Load libraries
library(sf)
library(tidyverse)
library(here)
library(spatstat)

# Този файл е оригинален с оригиналните местположения на обектите. 
# За CFMN моделите, тъй като Тунджа е 200 метров буфер, се наложи да 
# направя друг файл в който да преместя някои от обектите на около 50 
# м разстояние от оригиналното им местоположение, за да не влизат в NA 
# стойностите на Тунджа.

Sites <- read_csv(here("data/ChronCatalogue6Extract.csv")) 

# load and make the boxes as owin class

Box1 <-st_read("../data/Box1.shp")
Box1_w <- as.owin(Box1)

Box2 <-st_read("../data/Box2.shp")
Box2_w <- as.owin(Box2)

Municipal <-st_read("../data/Municipal.shp")
Municipal_w <- as.owin(Municipal)

Rectangular <-st_read("../data/Rectangular.shp")
Rectangular_w <- as.owin(Rectangular)




# Loading certain period sites

## Филтрирам само обектите, попадащи в петте общини, намиращи се в Котловината:

Settlements5muni <- Sites %>% filter(Община=="Павел баня" | Община=="Казанлък" | Община=="Мъглиж" | Община=="Гурково" | Община=="Николаево")%>%

## След това филтрирам само обектите, които по един или друг начин, могат да бъдат класифицирани като селища:
  
  filter(Вид == "Бани" | Вид == "Военен лагер/ град" | Вид == "Град" | Вид == "Единична постройка" | Вид == "Единична постройка?" | Вид == "Крепост" | Вид == "Крепост/ кула?" | Вид == "Крепост/ светилище?" | Вид == "Крепост/ селище?" | Вид == "Крепост?" | Вид == "Кула" | Вид == "Кула?" | Вид == "Манастир" | Вид == "Манастир/ черква?" | Вид == "Пътна станция" | Вид == "Пътна станция?" | Вид == "Светилище" | Вид == "Светилище/ селище?" | Вид == "Светилище?" | Вид == "Селище" | Вид == "Селище?" | Вид == "Селищна могила" | Вид == "Стопанство" | Вид == "Стопанство/ селище" | Вид == "Укрепен град" | Вид == "Укрепление?")

## След това правим селекция само на обектите от различните епохи със сигурни дати в съответните епохи:

## Праистория

Pre_Settl_1 <- Settlements5muni %>% 
  filter( Пра.==1 | Палео.==1 | Нео.==1 | РНео.==1 | СНео.==1 | КНео.==1 | Х==1 | РХ==1 | КХ==1) %>% 
  mutate(Праистория=Пра.+Палео.+Нео.+РНео.+СНео.+КНео.+Х+РХ+КХ)%>% # Бройката съвпада с тази в ГИС
  dplyr::select(Cat, E_coord:N_coord, Вид, Праистория) # Тук селектирам само колоните, които ще са ми нужни

Pre_Settl_1_sf <- st_as_sf(Pre_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Неолит:

NEO_Settl_1 <- Settlements5muni %>% 
  filter( Нео.==1 | РНео.==1 | СНео.==1 | КНео.==1) %>% 
  mutate(Неолит=Нео.+РНео.+СНео.+КНео.)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Неолит) # Тук селектирам само колоните, които ще са ми нужни

NEO_Settl_1_sf <- st_as_sf(NEO_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Халколит:

CHALCO_Settl_1 <- Settlements5muni %>% 
  filter( Х==1 | РХ==1 | КХ==1) %>% 
  mutate(Халколит=Х+РХ+КХ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Халколит) # Тук селектирам само колоните, които ще са ми нужни

CHALCO_Settl_1_sf <- st_as_sf(CHALCO_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Бронзова епоха

BA_Settl_1 <- Settlements5muni %>% 
  filter( БЕ==1|РБЕ==1|РБЕ1==1|РБЕ2==1|РБЕ3==1|СБЕ==1|КБЕ==1) %>% 
  mutate(Бронз=БЕ+РБЕ+РБЕ1+РБЕ2+РБЕ3+СБЕ+КБЕ)%>% # Бройката съвпада с тази в ГИС
  dplyr::select(Cat, E_coord:N_coord, Вид, Бронз) # Тук селектирам само колоните, които ще са ми нужни

BA_Settl_1_sf <- st_as_sf(BA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Раннобронзова

EBA_Settl_1 <- Settlements5muni %>% 
  filter( РБЕ==1|РБЕ1==1|РБЕ2==1|РБЕ3==1) %>% 
  mutate(Раннобронзова=РБЕ+РБЕ1+РБЕ2+РБЕ3)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Раннобронзова) # Тук селектирам само колоните, които ще са ми нужни

EBA_Settl_1_sf <- st_as_sf(EBA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## СБЕ няма регистрирани обекти

## КБЕ няма други подпериоди, така че за нея ще използвам колоната КБЕ. Когато селектираш обектите от този период, не забравяй, че тук не си използвал мутейт, и ако искаш да избереш само тези с точна дата, трябва да е == 1, а ако искаш да включиш и тези с несигурна дата, трябва да филтрираш с >= 1  

LBA_Settl_1 <- Settlements5muni %>% 
  filter( КБЕ==1) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, КБЕ) # Тук селектирам само колоните, които ще са ми нужни

LBA_Settl_1_sf <- st_as_sf(LBA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Желязна епоха

IA_Settl_1 <- Settlements5muni %>% 
  filter( ЖЕ==1 | РЖЕ==1 | РЖЕ1==1 | РЖЕ2==1 | КЖЕ==1 | КЖЕ1==1 | КЖЕ2==1 | К==1 | Ел.==1 | РЕл.==1 | КЕл.==1  ) %>% 
  mutate(Желязна=ЖЕ+РЖЕ+РЖЕ1+РЖЕ2+КЖЕ+КЖЕ1+КЖЕ2+К+Ел.+РЕл.+КЕл.)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Желязна) # Тук селектирам само колоните, които ще са ми нужни

IA_Settl_1_sf <- st_as_sf(IA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Ранножелязна:

EIA_Settl_1 <- Settlements5muni %>% 
  filter( РЖЕ==1 | РЖЕ1==1 | РЖЕ2==1) %>% 
  mutate(Ранножелязна=РЖЕ+РЖЕ1+РЖЕ2)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Ранножелязна) # Тук селектирам само колоните, които ще са ми нужни

EIA_Settl_1_sf <- st_as_sf(EIA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Късножелязна

LIA_Settl_1 <- Settlements5muni %>% 
  filter( КЖЕ==1 | КЖЕ1==1 | КЖЕ2==1 | К==1 | Ел.==1 | РЕл.==1 | КЕл.==1  ) %>% 
  mutate(Късножелязна=КЖЕ+КЖЕ1+КЖЕ2+К+Ел.+РЕл.+КЕл.)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Късножелязна) # Тук селектирам само колоните, които ще са ми нужни

LIA_Settl_1_sf <- st_as_sf(LIA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Класика

## Елинизъм

HELLEN_Settl_1 <- Settlements5muni %>% 
  filter( Ел.==1 | РЕл.==1 | КЕл.==1  ) %>% 
  mutate(Елинизъм=Ел.+РЕл.+КЕл.)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Елинизъм) # Тук селектирам само колоните, които ще са ми нужни

HELLEN_Settl_1_sf <- st_as_sf(HELLEN_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## РЕ. За римската епоха няма други подпериоди, така че за него ще използвам колоната РЕ. Когато селектираш обектите от този период, не забравяй, че тук не си използвал мутейт, и ако искаш да избереш само тези с точна дата, трябва да е == 1, а ако искаш да включиш и тези с несигурна дата, трябва да филтрираш с >= 1

RP_Settl_1 <- Settlements5muni %>% 
  filter(РЕ==1) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, РЕ) # Тук селектирам само колоните, които ще са ми нужни

RP_Settl_1_sf <- st_as_sf(RP_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635) # Бройката съвпада с тази в ГИС = 35

## КРЕ. За късноримската епоха няма други подпериоди, така че за него ще използвам колоната КРЕ. Когато селектираш обектите от този период, не забравяй, че тук не си използвал мутейт, и ако искаш да избереш само тези с точна дата, трябва да е == 1, а ако искаш да включиш и тези с несигурна дата, трябва да филтрираш с >= 1

LRP_Settl_1 <- Settlements5muni %>% 
  filter(КРЕ==1) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, КРЕ) # Тук селектирам само колоните, които ще са ми нужни

LRP_Settl_1_sf <- st_as_sf(LRP_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635) 

## Късноантична:

LA_Settl_1 <- Settlements5muni %>% 
  filter( КА==1 | КРЕ==1 |РВ ==1  ) %>% 
  mutate(Късноантична=КА+КРЕ+РВ)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Късноантична) # Тук селектирам само колоните, които ще са ми нужни

LA_Settl_1_sf <- st_as_sf(LA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Средновеквие (без Късното):

MA_XIV_Settl_1 <- Settlements5muni %>% 
  filter( Ср.==1 | РСр.==1 | ЗСр.==1 | ПБЦ==1 | ВБЦ==1  ) %>% 
  mutate(СредновековиеXIV=Ср.+РСр.+ЗСр.+ПБЦ+ВБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, СредновековиеXIV) # Тук селектирам само колоните, които ще са ми нужни

MA_XIV_Settl_1_sf <- st_as_sf(MA_XIV_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Средновеквие (с Късното)

MA_XVII_Settl_1 <- Settlements5muni %>% 
  filter( Ср.==1 | РСр.==1 | ЗСр.==1 | КСр.==1 | ПБЦ==1 | ВБЦ==1  ) %>% 
  mutate(СредновековиеXVII=Ср.+РСр.+ЗСр.+КСр.+ПБЦ+ВБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, СредновековиеXVII) # Тук селектирам само колоните, които ще са ми нужни

MA_XVII_Settl_1_sf <- st_as_sf(MA_XVII_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Ранно средновековие

EMA_Settl_1 <- Settlements5muni %>% 
  filter( РСр.==1 | ПБЦ==1  ) %>% 
  mutate(Ранноср.=РСр.+ПБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Ранноср.) # Тук селектирам само колоните, които ще са ми нужни

EMA_Settl_1_sf <- st_as_sf(EMA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Зряло средновековие

HMA_Settl_1 <- Settlements5muni %>% 
  filter( ЗСр.==1 | ВБЦ==1  ) %>% 
  mutate(Зрялоср.=ЗСр.+ВБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Зрялоср.) # Тук селектирам само колоните, които ще са ми нужни

HMA_Settl_1_sf <- st_as_sf(HMA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Късно средновековие

LMA_Settl_1 <- Settlements5muni %>% 
  filter( КСр.==1  ) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, КСр.) # Тук селектирам само колоните, които ще са ми нужни

LMA_Settl_1_sf <- st_as_sf(LMA_Settl_1, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)




# Loading sites including uncertain period

# Използвам файлът Settlements5muni, който съдържа в себе си всичките селищни обекти от петте общини

Settlements5muni

# След това правим селекция само на обектите от различните епохи включващи и несигурни дати в съответните епохи:

## Праистория

Pre_Settl_1_2 <- Settlements5muni %>% 
  filter( Пра.>0 | Палео.>0 | Нео.>0 | РНео.>0 | СНео.>0 | КНео.>0 | Х>0 | РХ>0 | КХ>0) %>% 
  mutate(Праистория=Пра.+Палео.+Нео.+РНео.+СНео.+КНео.+Х+РХ+КХ)%>% # Бройката съвпада с тази в ГИС
  dplyr::select(Cat, E_coord:N_coord, Вид, Праистория) # Тук селектирам само колоните, които ще са ми нужни

Pre_Settl_1_2_sf <- st_as_sf(Pre_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Неолит:

NEO_Settl_1_2 <- Settlements5muni %>% 
  filter( Нео.>0 | РНео.>0 | СНео.>0 | КНео.>0) %>% 
  mutate(Неолит=Нео.+РНео.+СНео.+КНео.)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Неолит) # Тук селектирам само колоните, които ще са ми нужни

NEO_Settl_1_2_sf <- st_as_sf(NEO_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Халколит:

CHALCO_Settl_1_2 <- Settlements5muni %>% 
  filter( Х>0 | РХ>0 | КХ>0) %>% 
  mutate(Халколит=Х+РХ+КХ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Халколит) # Тук селектирам само колоните, които ще са ми нужни

CHALCO_Settl_1_2_sf <- st_as_sf(CHALCO_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Бронзова епоха

BA_Settl_1_2 <- Settlements5muni %>% 
  filter( БЕ>0|РБЕ>0|РБЕ1>0|РБЕ2>0|РБЕ3>0|СБЕ>0|КБЕ>0) %>% 
  mutate(Бронз=БЕ+РБЕ+РБЕ1+РБЕ2+РБЕ3+СБЕ+КБЕ)%>% # Бройката съвпада с тази в ГИС
  dplyr::select(Cat, E_coord:N_coord, Вид, Бронз) # Тук селектирам само колоните, които ще са ми нужни

BA_Settl_1_2_sf <- st_as_sf(BA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Раннобронзова

EBA_Settl_1_2 <- Settlements5muni %>% 
  filter( РБЕ>0|РБЕ1>0|РБЕ2>0|РБЕ3>0) %>% 
  mutate(Раннобронзова=РБЕ+РБЕ1+РБЕ2+РБЕ3)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Раннобронзова) # Тук селектирам само колоните, които ще са ми нужни

EBA_Settl_1_2_sf <- st_as_sf(EBA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

# СБЕ няма регистрирани обекти

## КБЕ няма други подпериоди, така че за нея ще използвам колоната КБЕ. Когато селектираш обектите от този период, не забравяй, че тук не си използвал мутейт, и ако искаш да избереш само тези с точна дата, трябва да е == 1, а ако искаш да включиш и тези с несигурна дата, трябва да филтрираш с >= 1  

LBA_Settl_1_2 <- Settlements5muni %>% 
  filter( КБЕ>0) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, КБЕ) # Тук селектирам само колоните, които ще са ми нужни

LBA_Settl_1_2_sf <- st_as_sf(LBA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Желязна епоха

IA_Settl_1_2 <- Settlements5muni %>% 
  filter( ЖЕ>0 | РЖЕ>0 | РЖЕ1>0 | РЖЕ2>0 | КЖЕ>0 | КЖЕ1>0 | КЖЕ2>0 | К>0 | Ел.>0 | РЕл.>0 | КЕл.>0  ) %>% 
  mutate(Желязна=ЖЕ+РЖЕ+РЖЕ1+РЖЕ2+КЖЕ+КЖЕ1+КЖЕ2+К+Ел.+РЕл.+КЕл.)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Желязна) # Тук селектирам само колоните, които ще са ми нужни

IA_Settl_1_2_sf <- st_as_sf(IA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Ранножелязна:

EIA_Settl_1_2 <- Settlements5muni %>% 
  filter( РЖЕ>0 | РЖЕ1>0 | РЖЕ2>0) %>% 
  mutate(Ранножелязна=РЖЕ+РЖЕ1+РЖЕ2)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Ранножелязна) # Тук селектирам само колоните, които ще са ми нужни

EIA_Settl_1_2_sf <- st_as_sf(EIA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Късножелязна

LIA_Settl_1_2 <- Settlements5muni %>% 
  filter( КЖЕ>0 | КЖЕ1>0 | КЖЕ2>0 | К>0 | Ел.>0 | РЕл.>0 | КЕл.>0  ) %>% 
  mutate(Късножелязна=КЖЕ+КЖЕ1+КЖЕ2+К+Ел.+РЕл.+КЕл.)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Късножелязна) # Тук селектирам само колоните, които ще са ми нужни

LIA_Settl_1_2_sf <- st_as_sf(LIA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Класика

## Елинизъм

HELLEN_Settl_1_2 <- Settlements5muni %>% 
  filter( Ел.>0 | РЕл.>0 | КЕл.>0  ) %>% 
  mutate(Елинизъм=Ел.+РЕл.+КЕл.)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Елинизъм) # Тук селектирам само колоните, които ще са ми нужни

HELLEN_Settl_1_2_sf <- st_as_sf(HELLEN_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## РЕ. За римската епоха няма други подпериоди, така че за него ще използвам колоната РЕ. Когато селектираш обектите от този период, не забравяй, че тук не си използвал мутейт, и ако искаш да избереш само тези с точна дата, трябва да е == 1, а ако искаш да включиш и тези с несигурна дата, трябва да филтрираш с >= 1 или >0

RP_Settl_1_2 <- Settlements5muni %>% 
  filter(РЕ>0) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, РЕ) # Тук селектирам само колоните, които ще са ми нужни

RP_Settl_1_2_sf <- st_as_sf(RP_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635) # Бройката съвпада с тази в ГИС = 35

## КРЕ. За късноримската епоха няма други подпериоди, така че за него ще използвам колоната КРЕ. Когато селектираш обектите от този период, не забравяй, че тук не си използвал мутейт, и ако искаш да избереш само тези с точна дата, трябва да е == 1, а ако искаш да включиш и тези с несигурна дата, трябва да филтрираш с >= 1

LRP_Settl_1_2 <- Settlements5muni %>% 
  filter(КРЕ>0) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, КРЕ) # Тук селектирам само колоните, които ще са ми нужни

LRP_Settl_1_2_sf <- st_as_sf(LRP_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635) 

## Късноантична:

LA_Settl_1_2 <- Settlements5muni %>% 
  filter( КА>0 | КРЕ>0 |РВ>0  ) %>% 
  mutate(Късноантична=КА+КРЕ+РВ)%>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, Късноантична) # Тук селектирам само колоните, които ще са ми нужни

LA_Settl_1_2_sf <- st_as_sf(LA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Средновеквие (без Късното):

MA_XIV_Settl_1_2 <- Settlements5muni %>% 
  filter( Ср.>0 | РСр.>0 | ЗСр.>0 | ПБЦ>0 | ВБЦ>0  ) %>% 
  mutate(СредновековиеXIV=Ср.+РСр.+ЗСр.+ПБЦ+ВБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, СредновековиеXIV) # Тук селектирам само колоните, които ще са ми нужни

MA_XIV_Settl_1_2_sf <- st_as_sf(MA_XIV_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

# Средновеквие (с Късното)

MA_XVII_Settl_1_2 <- Settlements5muni %>% 
  filter( Ср.>0 | РСр.>0 | ЗСр.>0 | КСр.>0 | ПБЦ>0 | ВБЦ>0  ) %>% 
  mutate(СредновековиеXVII=Ср.+РСр.+ЗСр.+КСр.+ПБЦ+ВБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, СредновековиеXVII) # Тук селектирам само колоните, които ще са ми нужни

MA_XVII_Settl_1_2_sf <- st_as_sf(MA_XVII_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Ранно средновековие

EMA_Settl_1_2 <- Settlements5muni %>% 
  filter( РСр.>0 | ПБЦ>0  ) %>% 
  mutate(Ранноср.=РСр.+ПБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Ранноср.) # Тук селектирам само колоните, които ще са ми нужни

EMA_Settl_1_2_sf <- st_as_sf(EMA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Зряло средновековие

HMA_Settl_1_2 <- Settlements5muni %>% 
  filter( ЗСр.>0 | ВБЦ>0  ) %>% 
  mutate(Зрялоср.=ЗСр.+ВБЦ)%>%
  dplyr::select(Cat, E_coord:N_coord, Вид, Зрялоср.) # Тук селектирам само колоните, които ще са ми нужни

HMA_Settl_1_2_sf <- st_as_sf(HMA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)

## Късно средновековие

LMA_Settl_1_2 <- Settlements5muni %>% 
  filter( КСр.>0  ) %>% 
  dplyr::select(Cat, E_coord:N_coord, Вид, КСр.) # Тук селектирам само колоните, които ще са ми нужни

LMA_Settl_1_2_sf <- st_as_sf(LMA_Settl_1_2, coords = c("E_coord", "N_coord"), crs=4326)%>% # Convert the data frame to an sf object (for working with vector data)
  st_transform(crs = 32635)




#grep("*_1$|*_2$", ls())  # use as rm(list = ls()[grep("A", ls())])
rm(list = ls(pattern = "*_1$|*_2$")) # This pull out all the tibles, which I don't need for the NNA 
ls()
