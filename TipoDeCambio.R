library(quantmod)

getSymbols(c('EURUSD=X','EURGBP=X','EURJPY=X'))
euro_dolar <- `EURUSD=X`
euro_libra <- `EURGBP=X`
euro_yen <- `EURJPY=X`

plot(index(euro_dolar),as.numeric(euro_dolar[,6]),typ='l',xlab='',ylab='EUR/USD', main = "Tipo de Cambio Euro/Dólar")
plot(index(euro_libra),as.numeric(euro_libra[,6]),typ='l',xlab='',ylab='EUR/GBP', main = "Tipo de Cambio Euro/Libra")
plot(index(euro_yen),as.numeric(euro_yen[,6]),typ='l',xlab='',ylab='EUR/JPY', main = "Tipo de Cambio Euro/Yen")

library(BIS)
datasets <- get_datasets()
head(datasets, 20)
rer <- get_bis(datasets$url[datasets$name == "Effective exchange rate indices (monthly)"], quiet = TRUE)

library(tidyverse)
rer_spain <- rer %>%
  dplyr::filter(type=="Real" & reference_area=="Spain" & eer_basket=="B" & date > "1999-12") %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  select(date, obs_value)

ggplot(data = rer_spain, aes(x=date, y=obs_value))+
  geom_line(size=1.6)+
  labs(title = "Tipo de cambio real España", x="", y="", caption = 'Fuente = BIS')

rer %>% 
  dplyr::filter(reference_area %in% c("United States","Euro area" )) %>%
  dplyr::filter(type=="Real" & eer_basket=="B" & date > "1999-12") %>%
  mutate(date = as.Date(as.yearmon(date))) %>% 
  ggplot(aes(x=date, y=obs_value, group=reference_area, colour=reference_area))+
  geom_line(size=1.0)+
  labs(title = "Tipo de cambio real: Eurozona vs USA", x="", y="", caption = 'Fuente = BIS')+
  theme(legend.title=element_blank())+
  theme(legend.position = "bottom")+
  theme(plot.title=element_text(face="bold",hjust=0.5,vjust=2,colour="#3C3C3C",size=18))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(legend.text=element_text(size=13))

getSymbols('CPIAUCSL',src='FRED')


CPI <- CPIAUCSL["2001-01-01/2020-01-01"]

plot(index(CPI),CPI$CPIAUCSL,typ='l',xlab='',ylab='', main = "IPC USA")

getSymbols('CP0000EZ19M086NEST',src='FRED')

CPI_EU <- CP0000EZ19M086NEST

CPIEU <- CPI_EU["2001-01-01/2020-01-01"]

plot(index(CPIEU),CPIEU$CP0000EZ19M086NEST,typ='l',xlab='',ylab='', main = "IPC USA vs EU", col="red")
lines(index(CPI),CPI$CPIAUCS, col="blue")
