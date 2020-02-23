library(quantmod)
library(tidyverse)

#Obtenemos los tipos de cambio
getSymbols(c('EURUSD=X','EURGBP=X','EURJPY=X'))
euro_dolar <- `EURUSD=X`
euro_libra <- `EURGBP=X`
euro_yen <- `EURJPY=X`

#Graficamos los tipos de cambio
plot(index(euro_dolar),as.numeric(euro_dolar[,6]),typ='l',xlab='',ylab='EUR/USD', main = "Tipo de Cambio Euro/D?lar")
plot(index(euro_libra),as.numeric(euro_libra[,6]),typ='l',xlab='',ylab='EUR/GBP', main = "Tipo de Cambio Euro/Libra")
plot(index(euro_yen),as.numeric(euro_yen[,6]),typ='l',xlab='',ylab='EUR/JPY', main = "Tipo de Cambio Euro/Yen")

#Calculamos las variaciones diarias
euro_dolar$adjusteddiff <- diff(euro_dolar$`EURUSD=X.Adjusted` )
euro_libra$adjusteddiff <- diff(euro_libra$`EURGBP=X.Adjusted` )
euro_yen$adjusteddiff <- diff(euro_yen$`EURJPY=X.Adjusted`)

#Graficamos las variaciones
plot(index(euro_dolar),as.numeric(euro_dolar[,7]),typ='l',xlab='',ylab='EUR/USD', main = "Variaci?n del Tipo de Cambio Euro/D?lar")
plot(index(euro_libra),as.numeric(euro_libra[,7]),typ='l',xlab='',ylab='EUR/GBP', main = "Variaci?n del Tipo de Cambio Euro/Libra")
plot(index(euro_yen),as.numeric(euro_yen[,7]),typ='l',xlab='',ylab='EUR/USD', main = "Variaci?n del Tipo de Cambio Euro/Yen")


#Grafico variaciones y nivel
par(mfrow=c(2,1))
plot(index(euro_dolar),as.numeric(euro_dolar[,6]),typ='l',xlab='',ylab='EUR/USD', main = "Tipo de Cambio Euro/D?lar")
plot(index(euro_dolar),as.numeric(euro_dolar[,7]),typ='l',xlab='',ylab='EUR/USD', main = "Variaci?n del Tipo de Cambio Euro/D?lar")
dev.off()

#Calculamos y graficamos  las variaciones anuales
diff.xts(euro_yen$EURJPY.X.Adjusted, lag = 250) %>%
  na.omit() %>%
  plot(type="l", main = "Variación anual Tipo de cambio Euro/Yen")
diff.xts(euro_dolar$EURUSD.X.Adjusted, lag = 250) %>% 
  na.omit() %>%
  plot(type="l", main = "Variación anual Tipo de cambio Euro/Dólar")
diff.xts(euro_libra$EURGBP.X.Adjusted, lag = 250) %>%
  na.omit() %>%
  plot(type="l", main = "Variación anual Tipo de cambio Euro/Libra")

#Guardamos
save(euro_dolar, file = "data/euro_dolar.RData")
save(euro_libra, file = "data/euro_libra.RData")
save(euro_yen, file = "data/euro_yen.RData")


#Empleamos la librer?a BIS para descargar los datos de tipo de cambio real
library(BIS)
datasets <- get_datasets()
head(datasets, 20)
rer <- get_bis(datasets$url[datasets$name == "Effective exchange rate indices (monthly)"], quiet = TRUE)

#Analizamos el caso espa?ol
rer_spain <- rer %>%
  dplyr::filter(type=="Real" & reference_area=="Spain" & eer_basket=="B" & date > "1999-12") %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  select(date, obs_value)

p1 <- ggplot(data = rer_spain, aes(x=date, y=obs_value))+
  geom_line(size=1.6)+
  labs(title = "Tipo de cambio real Espa?a", x="", y="", caption = 'Fuente = BIS')+
  theme(plot.title=element_text(face="bold",hjust=0.5,vjust=2,colour="#3C3C3C",size=12))

#Graficamos la cuenta corriente y comparamos con el TCR

#Usamos la libreria FRED
library(fredr)

fredr_set_key("be9498fedfcbead42dae240c1a633924")

CA_spain <- fredr(
  series_id = "ESPB6BLTT02STSAQ",
  observation_start = as.Date("2000-01-01")
)

#Para alinear los graficos
library(patchwork)

p2 <- ggplot(data = CA_spain, aes(x=date, y=value))+
  geom_line(size=1.6, colour="blue")+
  labs(title = "Cuenta corriente España (% PIB)", x="", y="", caption = 'Fuente = FRED')+
  geom_hline(yintercept=0)

p1 / p2


#Graficamos el TCR de la zona euro con el de USA
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

#Guardamos los datos
save(rer, file = "data/rer.RData")
save(rer_spain, file = "data/rer_spain.RData")
save(CA_spain, file = "data/CA_spain.RData")

#Indices de precios
#USA
getSymbols('CPIAUCSL',src='FRED')
CPI <- CPIAUCSL["2001-01-01/2019-12-01"]

plot(index(CPI),CPI$CPIAUCSL,typ='l',xlab='',ylab='', main = "IPC USA")

#EU
getSymbols('CP0000EZ19M086NEST',src='FRED')
CPI_EU <- CP0000EZ19M086NEST
CPIEU <- CPI_EU["2001-01-01/2019-12-01"]

#Creamos un indice base 100 en el 1? a?o con los dops ipc's
CPIEU$INDEX <- (CPIEU$CP0000EZ19M086NEST/as.numeric(CPIEU$CP0000EZ19M086NEST[1,1]))*100
CPI$INDEX <- (CPI$CPIAUCSL/as.numeric(CPI$CPIAUCSL[1,1]))*100

#Graficamos ambos indices
plot(index(CPIEU),CPIEU$INDEX,typ='l',xlab='',ylab='', main = "IPC USA vs EU", col="red", ylim = c(95,150))
lines(index(CPI),CPI$INDEX, col="blue")
legend("topleft", legend=c("EU", "USA"),col=c("red", "blue"), lty=1:2, cex=0.8)

CPI$INFLATION <- quantmod::Delt(CPI$CPIAUCSL, k = 12)*100
CPIEU$INFLATION <- quantmod::Delt(CPIEU$CP0000EZ19M086NEST, k = 12)*100

#Graficamos las inflaciones
plot(index(CPIEU),CPIEU$INFLATION,typ='l',xlab='',ylab='', main = "Inflación anual USA vs EU", col="red", ylim = c(-2,5.5))
lines(index(CPI),CPI$INFLATION, col="blue")
legend("topleft", legend=c("EU", "USA"),col=c("red", "blue"), lty=1:2, cex=0.8)

#Guardamos los datos
save(CPI, file = "data/CPI.RData")
save(CPIEU, file = "data/CPIEU.RData")

#Grafico diferencial de inflacion y variaciones del tipo de cambio

infdiff_USEU <- CPI$INFLATION - CPIEU$INFLATION
infdiff_EUUS <- CPIEU$INFLATION - CPI$INFLATION

diff.xts(euro_libra$EURGBP.X.Adjusted, lag = 250) %>% na.omit() %>% plot(type="l", main = "Variación anual Tipo de cambio Euro/Libra")

plot(infdiff_EUUS["2008-01-01/2019-12-31"], type = "l")
diff.xts(euro_dolar$EURUSD.X.Adjusted, lag = 250) %>%
na.omit() %>% lines(col = "blue",type = "l")

plot(infdiff_EUUS["2008-01-01/2019-12-31"], type = "l")
lines(diff_anual[endpoints(diff_anual, on = "months")], col = "blue")

diff.xts(euro_dolar$EURUSD.X.Adjusted, lag = 250) %>%
to.period(period = "months")




