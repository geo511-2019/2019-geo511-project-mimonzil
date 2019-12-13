library(fredr)
library(wbstats)
library(dplyr)
library(ggplot2)
library(foreach)
library(spData)
library(sf)
library(plm)
library(forecast)
library(sp)
library(rworldmap)
library(ggmap)



fredr_set_key("6b0cdd4cca87cf2616946fd1b9c3e865")

# Political Stability - Indicator = Public Debt



Arg = fredr_series_search_text("Argentina debt GDP")
Arg$title
Arg$id[5]
Bol = fredr_series_search_text("Bolivia public debt")
Bol$title
Bol$id[3]
Bra = fredr_series_search_text("Brazil public debt")
Bra$title
Bra$id[5]
Chi = fredr_series_search_text("Chile public debt")
Chi$title
Chi$id[2]
Col = fredr_series_search_text("Colombia public debt")
Col$title
Col$id[7]
Cos = fredr_series_search_text("Costa Rica public debt")
Cos$title
Cos$id[3]
Dom = fredr_series_search_text("Dominican Republic public debt")
Dom$title
Dom$id[6]
Ecu = fredr_series_search_text("Ecuador public debt")
Ecu$title
Ecu$id[4]
Els = fredr_series_search_text("El Salvador public debt")
Els$title
Els$id[3]
Mex = fredr_series_search_text("Mexico public debt")
Mex$title
Mex$id[5]
Pan = fredr_series_search_text("Panama public debt")
Pan$title
Pan$id[4]
Par = fredr_series_search_text("Paraguay public debt")
Par$title
Par$id[3]
Per = fredr_series_search_text("Peru public debt")
Per$title
Per$id[7]
Uru = fredr_series_search_text("Uruguay public debt")
Uru$title
Uru$id[5]


dates = c(1999:2014)
dates.1 = c(1980:2017)
debtnames = c("date", "debt")


Arg.debt = fredr("DDDM07ARA156NWDB")
Arg.debt
Arg.debt.1 = cbind(dates.1, Arg.debt) %>%
  filter(dates.1 >= 2000) %>%
  filter(dates.1 <= 2014) %>%
  dplyr::select(dates.1, value) %>%
  arrange((desc(dates.1)))
Arg.debt.1
colnames(Arg.debt.1) <- c("date", "Arg.debt")
Arg.debt.1

Bol.debt = fredr("DDDM07BOA156NWDB")
head(Bol.debt)
tail(Bol.debt)
Bol.debt.1 = cbind(dates.1, Bol.debt) %>%
  filter(dates.1 >= 2000) %>%
  filter(dates.1 <= 2014) %>%
  dplyr::select(dates.1, value) %>%
  arrange((desc(dates.1))) 
Bol.debt.1
colnames(Bol.debt.1) <- c("date", "Bol.debt")
Bol.debt.1

Bra.debt = fredr("DDDM07BRA156NWDB")
head(Bra.debt)
tail(Bra.debt)
dates.2 = c(1982:2017)
Bra.debt.1 = cbind(dates.2, Bra.debt) %>%
  filter(dates.2 >= 2000) %>%
  filter(dates.2 <= 2014) %>%
  dplyr::select(dates.2, value)  %>%
  arrange((desc(dates.2)))
Bra.debt.1
colnames(Bra.debt.1) <- c("date", "Bra.debt")
Bra.debt.1

Chi.debt = fredr("DDDM07CLA156NWDB")
head(Chi.debt)
tail(Chi.debt)
Chi.debt.1 = cbind(dates.1, Chi.debt) %>%
  filter(dates.1 >= 2000) %>%
  filter(dates.1 <= 2014) %>%
  dplyr::select(dates.1, value)  %>%
  arrange((desc(dates.1)))
Chi.debt.1
colnames(Chi.debt.1) <- c("date", "Chi.debt")
Chi.debt.1

Col.debt = fredr("DDDM07COA156NWDB")
head(Col.debt)
tail(Col.debt)
Col.debt.1 = cbind(dates.1, Col.debt) %>%
  filter(dates.1 >= 2000) %>%
  filter(dates.1 <= 2014) %>%
  dplyr::select(dates.1, value)  %>%
  arrange((desc(dates.1)))
Col.debt.1
colnames(Col.debt.1) <- c("date", "Col.debt")
Col.debt.1

Cos.debt = fredr("DDDM04COA156NWDB")
head(Cos.debt)
tail(Cos.debt)
dates.3=c(1989:2017)
Cos.debt.1 = cbind(dates.3, Cos.debt) %>%
  filter(dates.3 >= 2000) %>%
  filter(dates.3 <= 2014) %>%
  dplyr::select(dates.3, value)  %>%
  arrange((desc(dates.3)))
Cos.debt.1
colnames(Cos.debt.1) <- c("date", "Cos.debt")
Cos.debt.1

Dom.debt = fredr("DDDM07DOA156NWDB")
head(Dom.debt)
tail(Dom.debt)
dates.4 = c(1994:2017)
Dom.debt.1 = cbind(dates.4, Dom.debt) %>%
  filter(dates.4 >= 2000) %>%
  filter(dates.4 <= 2014) %>%
  dplyr::select(dates.4, value)  %>%
  arrange((desc(dates.4)))
Dom.debt.1
colnames(Dom.debt.1) <- c("date", "Dom.debt")
Dom.debt.1

Ecu.debt = fredr("DDDM07ECA156NWDB")
head(Ecu.debt)
tail(Ecu.debt)
Ecu.debt.1 = cbind(dates.1, Ecu.debt) %>%
  filter(dates.1 >= 2000) %>%
  filter(dates.1 <= 2014) %>%
  dplyr::select(dates.1, value)  %>%
  arrange((desc(dates.1)))
Ecu.debt.1
colnames(Ecu.debt.1) <- c("date", "Ecu.debt")
Ecu.debt.1

Els.debt = fredr("DDDM07SVA156NWDB")
head(Els.debt)  
tail(Els.debt)
dates.9 = c(1997:2017)
Els.debt.1 = cbind(dates.9, Els.debt) %>%
  filter(dates.9 >= 2000) %>%
  filter(dates.9 <= 2014) %>%
  dplyr::select(dates.9, value)  %>%
  arrange((desc(dates.9)))
Els.debt.1
colnames(Els.debt.1) <- c("date", "Els.debt")
Els.debt.1


Mex.debt = fredr("DDDM07MXA156NWDB")
head(Mex.debt)
tail(Mex.debt)
Mex.debt.1 = cbind(dates.1, Mex.debt) %>%
  filter(dates.1 >= 2000) %>%
  filter(dates.1 <= 2014) %>%
  dplyr::select(dates.1, value)  %>%
  arrange((desc(dates.1)))
Mex.debt.1
colnames(Mex.debt.1) <- c("date", "Mex.debt")
Mex.debt.1

Pan.debt = fredr("DDDM07PAA156NWDB")
head(Pan.debt)
tail(Pan.debt)
Pan.debt.1 = cbind(dates.1, Pan.debt) %>%
  filter(dates.1 >= 2000) %>%
  filter(dates.1 <= 2014) %>%
  dplyr::select(dates.1, value)  %>%
  arrange((desc(dates.1)))
Pan.debt.1
colnames(Pan.debt.1) <- c("date", "Pan.debt")
Pan.debt.1

Par.debt = fredr("DDDM10PYA156NWDB")
head(Par.debt)
tail(Par.debt)
dates.6 = c(2000:2017)
Par.debt.1 = cbind(dates.6, Par.debt) %>%
  filter(dates.6 >= 2000) %>%
  filter(dates.6 <= 2014) %>%
  dplyr::select(dates.6, value)  %>%
  arrange((desc(dates.6)))
Par.debt.1
colnames(Par.debt.1) <- c("date", "Par.debt")
Par.debt.1

Per.debt = fredr("DDDM07PEA156NWDB")
head(Per.debt)
tail(Per.debt)
Per.debt.1 = cbind(dates.4, Per.debt) %>%
  filter(dates.4 >= 2000) %>%
  filter(dates.4 <= 2014) %>%
  dplyr::select(dates.4, value)  %>%
  arrange((desc(dates.4)))
Per.debt.1
colnames(Per.debt.1) <- c("date", "Per.debt")
Per.debt.1

Uru.debt = fredr("DDDM07UYA156NWDB")
head(Uru.debt)
tail(Uru.debt)
dates.7 = c(1991:2017)
Uru.debt.1 = cbind(dates.7, Uru.debt) %>%
  filter(dates.7 >= 2000) %>%
  filter(dates.7 <= 2014) %>%
  dplyr::select(dates.7, value)  %>%
  arrange((desc(dates.7)))
Uru.debt.1
colnames(Uru.debt.1) <- c("date", "Uru.debt")
Uru.debt.1


# Environmental Sustainability - Indicator = CO2 Emissions

wbsearch(pattern = " CO2 emission")

CO2 = wb(indicator = "EN.ATM.CO2E.KT")

Arg.CO2 = CO2 %>%
  filter(country == "Argentina") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value) %>%
  arrange((desc(date)))
colnames(Arg.CO2) <- c("date1", "Arg.CO2")
Arg.CO2


Bol.CO2 = CO2 %>%
  filter(country == "Bolivia") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Bol.CO2) <- c("date1", "Bol.CO2")
Bol.CO2

Bra.CO2 = CO2 %>%
  filter(country == "Brazil") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Bra.CO2) <- c("date1", "Bra.CO2")
Bra.CO2

Chi.CO2 = CO2 %>%
  filter(country == "Chile") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Chi.CO2) <- c("date1", "Chi.CO2")
Chi.CO2

Col.CO2 = CO2 %>%
  filter(country == "Colombia") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Col.CO2) <- c("date1", "Col.CO2")
Col.CO2

Cos.CO2 = CO2 %>%
  filter(country == "Costa Rica") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Cos.CO2) <- c("date1", "Cos.CO2")
Cos.CO2

Dom.CO2 = CO2 %>%
  filter(country == "Dominican Republic") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Dom.CO2) <- c("date1", "Dom.CO2")
Dom.CO2

Ecu.CO2 = CO2 %>%
  filter(country == "Ecuador") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Ecu.CO2) <- c("date1", "Ecu.CO2")
Ecu.CO2

Els.CO2 = CO2 %>%
  filter(country == "El Salvador") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Els.CO2) <- c("date1", "Els.CO2")
Els.CO2

Mex.CO2 = CO2 %>%
  filter(country == "Mexico") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Mex.CO2) <- c("date1", "Mex.CO2")
Mex.CO2

Pan.CO2 = CO2 %>%
  filter(country == "Panama") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Pan.CO2) <- c("date1", "Pan.CO2")
Pan.CO2

Par.CO2 = CO2 %>%
  filter(country == "Paraguay") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Par.CO2) <- c("date1", "Par.CO2")
Par.CO2

Per.CO2 = CO2 %>%
  filter(country == "Peru") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Per.CO2) <- c("date1", "Per.CO2")
Per.CO2

Uru.CO2 = CO2 %>%
  filter(country == "Uruguay") %>%
  filter(date >= 2000) %>%
  dplyr::select(date, value)
colnames(Uru.CO2) <- c("date1", "Uru.CO2")
Uru.CO2

# Economic Stability = Inflation Rate

Arg.CPI = fredr_series_search_text("Argentina CPI")
Arg.CPI$title
Arg.CPI$id[2]
Arg.CPI = fredr("DDOE01ARA086NWDB")
head(Arg.CPI)
tail(Arg.CPI)
CPIdates.1 = c(1960:2014)
Arg.CPI.1 = cbind(CPIdates.1, Arg.CPI) %>%
  filter(CPIdates.1 >= 2000) %>%
  filter(CPIdates.1 <= 2014) %>%
  dplyr::select(CPIdates.1, value) %>%
  arrange((desc(CPIdates.1)))
Arg.CPI.1
colnames(Arg.CPI.1) <- c("date2", "Arg.CPI")
Arg.CPI.1

Bol = fredr_series_search_text("Bolivia CPI")
Bol$title
Bol$id[2]
Bol.CPI = fredr("DDOE01BOA086NWDB")
head(Bol.CPI)
tail(Bol.CPI)
CPIdates.2 = c(1960:2017)
Bol.CPI.1 = cbind(CPIdates.2, Bol.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Bol.CPI.1
colnames(Bol.CPI.1) <- c("date2", "Bol.CPI")
Bol.CPI.1

Bra = fredr_series_search_text("Brazil CPI")
Bra$title
Bra$id[16]
Bra.CPI = fredr("DDOE01BRA086NWDB")
head(Bra.CPI)
tail(Bra.CPI)
CPIdates.3 = c(1979:2017)
Bra.CPI.1 = cbind(CPIdates.3, Bra.CPI) %>%
  filter(CPIdates.3 >= 2000) %>%
  filter(CPIdates.3 <= 2014) %>%
  dplyr::select(CPIdates.3, value) %>%
  arrange((desc(CPIdates.3)))
Bra.CPI.1
colnames(Bra.CPI.1) <- c("date2", "Bra.CPI")
Bra.CPI.1

Chi = fredr_series_search_text("Chile CPI")
Chi$title
Chi$id[5]
Chi.CPI = fredr("DDOE01CLA086NWDB")
head(Chi.CPI)
tail(Chi.CPI)
Chi.CPI.1 = cbind(CPIdates.2, Chi.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Chi.CPI.1
colnames(Chi.CPI.1) <- c("date2", "Chi.CPI")
Chi.CPI.1

Col = fredr_series_search_text("Colombia CPI")
Col$title
Col$id[1]
Col.CPI = fredr("DDOE01COA086NWDB")
head(Col.CPI)
tail(Col.CPI)
Col.CPI.1 = cbind(CPIdates.2, Col.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Col.CPI.1
colnames(Col.CPI.1) <- c("date2", "Col.CPI")
Col.CPI.1

Cos = fredr_series_search_text("Costa Rica CPI")
Cos$title
Cos$id[2]
Cos.CPI = fredr("DDOE02CRA086NWDB")
head(Cos.CPI)
tail(Cos.CPI)
Cos.CPI.1 = cbind(CPIdates.2, Cos.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Cos.CPI.1
colnames(Cos.CPI.1) <- c("date2", "Cos.CPI")
Cos.CPI.1

Dom = fredr_series_search_text("Dominican Republic CPI")
Dom$title
Dom$id[2]
Dom.CPI = fredr("DDOE01DOA086NWDB")
head(Dom.CPI)
tail(Dom.CPI)
Dom.CPI.1 = cbind(CPIdates.2, Dom.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Dom.CPI.1
colnames(Dom.CPI.1) <- c("date2", "Dom.CPI")
Dom.CPI.1

Ecu = fredr_series_search_text("Ecuador CPI")
Ecu$title
Ecu$id[2]
Ecu.CPI = fredr("DDOE01ECA086NWDB")
head(Ecu.CPI)
tail(Ecu.CPI)
Ecu.CPI.1 = cbind(CPIdates.2, Ecu.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Ecu.CPI.1
colnames(Ecu.CPI.1) <- c("date2", "Ecu.CPI")
Ecu.CPI.1

Els = fredr_series_search_text("El Salvador CPI")
Els$title
Els$id[2]
Els.CPI = fredr("DDOE02SVA086NWDB")
head(Els.CPI)
tail(Els.CPI)
Els.CPI.1 = cbind(CPIdates.2, Els.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Els.CPI.1
colnames(Els.CPI.1) <- c("date2", "Els.CPI")
Els.CPI.1


Mex = fredr_series_search_text("Mexico CPI")
Mex$title
Mex$id[29]
Mex.CPI = fredr("DDOE02MXA086NWDB")
head(Mex.CPI)
tail(Mex.CPI)
Mex.CPI.1 = cbind(CPIdates.2, Mex.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Mex.CPI.1
colnames(Mex.CPI.1) <- c("date2", "Mex.CPI")
Mex.CPI.1

Pan = fredr_series_search_text("Panama CPI")
Pan$title
Pan$id[1]
Pan.CPI = fredr("DDOE02PAA086NWDB")
head(Pan.CPI)
tail(Pan.CPI)
Pan.CPI.1 = cbind(CPIdates.2, Pan.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Pan.CPI.1
colnames(Pan.CPI.1) <- c("date2", "Pan.CPI")
Pan.CPI.1

Par = fredr_series_search_text("Paraguay CPI")
Par$title
Par$id[2]
Par.CPI = fredr("DDOE01PYA086NWDB")
head(Par.CPI)
tail(Par.CPI)
Par.CPI.1 = cbind(CPIdates.2, Par.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Par.CPI.1
colnames(Par.CPI.1) <- c("date2", "Par.CPI")
Par.CPI.1

Per = fredr_series_search_text("Peru CPI")
Per$title
Per$id[2]
Per.CPI = fredr("DDOE01PEA086NWDB")
head(Per.CPI)
tail(Per.CPI)
Per.CPI.1 = cbind(CPIdates.2, Per.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Per.CPI.1
colnames(Per.CPI.1) <- c("date2", "Per.CPI")
Per.CPI.1

Uru = fredr_series_search_text("Uruguay CPI")
Uru$title
Uru$id[1]
Uru.CPI = fredr("DDOE01UYA086NWDB")
head(Uru.CPI)
tail(Uru.CPI)
Uru.CPI.1 = cbind(CPIdates.2, Uru.CPI) %>%
  filter(CPIdates.2 >= 2000) %>%
  filter(CPIdates.2 <= 2014) %>%
  dplyr::select(CPIdates.2, value) %>%
  arrange((desc(CPIdates.2)))
Uru.CPI.1
colnames(Uru.CPI.1) <- c("date2", "Uru.CPI")
Uru.CPI.1

# Create Country Dataframes, Gather Geometric Means

Arg = cbind(Arg.debt.1, Arg.CO2) 
Arg = cbind(Arg, Arg.CPI.1) %>%
  dplyr::select(date, Arg.debt, Arg.CO2, Arg.CPI)
Arg.mean= ((Arg$Arg.debt*Arg$Arg.CO2*Arg$Arg.CPI)^(1/3))
Arg = cbind(Arg, Arg.mean)
Arg

Bol = cbind(Bol.debt.1, Bol.CO2) 
Bol = cbind(Bol, Bol.CPI.1) %>%
  dplyr::select(date, Bol.debt, Bol.CO2, Bol.CPI)
Bol.mean= ((Bol$Bol.debt*Bol$Bol.CO2*Bol$Bol.CPI)^(1/3))
Bol = cbind(Bol, Bol.mean)
Bol

Bra = cbind(Bra.debt.1, Bra.CO2) 
Bra = cbind(Bra, Bra.CPI.1) %>%
  dplyr::select(date, Bra.debt, Bra.CO2, Bra.CPI)
Bra.mean= ((Bra$Bra.debt*Bra$Bra.CO2*Bra$Bra.CPI)^(1/3))
Bra = cbind(Bra, Bra.mean)
Bra

Chi = cbind(Chi.debt.1, Chi.CO2) 
Chi = cbind(Chi, Chi.CPI.1) %>%
  dplyr::select(date, Chi.debt, Chi.CO2, Chi.CPI)
Chi.mean= ((Chi$Chi.debt*Chi$Chi.CO2*Chi$Chi.CPI)^(1/3))
Chi = cbind(Chi, Chi.mean)
Chi

Col = cbind(Col.debt.1, Col.CO2) 
Col = cbind(Col, Col.CPI.1) %>%
  dplyr::select(date, Col.debt, Col.CO2, Col.CPI)
Col.mean= ((Col$Col.debt*Col$Col.CO2*Col$Col.CPI)^(1/3))
Col = cbind(Col, Col.mean)
Col

Cos = cbind(Cos.debt.1, Cos.CO2) 
Cos = cbind(Cos, Cos.CPI.1) %>%
  dplyr::select(date, Cos.debt, Cos.CO2, Cos.CPI)
Cos.mean= ((Cos$Cos.debt*Cos$Cos.CO2*Cos$Cos.CPI)^(1/3))
Cos = cbind(Cos, Cos.mean)
Cos

Dom = cbind(Dom.debt.1, Dom.CO2) 
Dom = cbind(Dom, Dom.CPI.1) %>%
  dplyr::select(date, Dom.debt, Dom.CO2, Dom.CPI)
Dom.mean= ((Dom$Dom.debt*Dom$Dom.CO2*Dom$Dom.CPI)^(1/3))
Dom = cbind(Dom, Dom.mean)
Dom

Ecu = cbind(Ecu.debt.1, Ecu.CO2) 
Ecu = cbind(Ecu, Ecu.CPI.1) %>%
  dplyr::select(date, Ecu.debt, Ecu.CO2, Ecu.CPI)
Ecu.mean= ((Ecu$Ecu.debt*Ecu$Ecu.CO2*Ecu$Ecu.CPI)^(1/3))
Ecu = cbind(Ecu, Ecu.mean)
Ecu

Els = cbind(Els.debt.1, Els.CO2) 
Els = cbind(Els, Els.CPI.1) %>%
  dplyr::select(date, Els.debt, Els.CO2, Els.CPI)
Els.mean= ((Els$Els.debt*Els$Els.CO2*Els$Els.CPI)^(1/3))
Els = cbind(Els, Els.mean)
Els

Mex = cbind(Mex.debt.1, Mex.CO2) 
Mex = cbind(Mex, Mex.CPI.1) %>%
  dplyr::select(date, Mex.debt, Mex.CO2, Mex.CPI)
Mex.mean= ((Mex$Mex.debt*Mex$Mex.CO2*Mex$Mex.CPI)^(1/3))
Mex = cbind(Mex, Mex.mean)
Mex

Pan = cbind(Pan.debt.1, Pan.CO2) 
Pan = cbind(Pan, Pan.CPI.1) %>%
  dplyr::select(date, Pan.debt, Pan.CO2, Pan.CPI)
Pan.mean= ((Pan$Pan.debt*Pan$Pan.CO2*Pan$Pan.CPI)^(1/3))
Pan = cbind(Pan, Pan.mean)
Pan

Par = cbind(Par.debt.1, Par.CO2) 
Par = cbind(Par, Par.CPI.1) %>%
  dplyr::select(date, Par.debt, Par.CO2, Par.CPI)
Par.mean= ((Par$Par.debt*Par$Par.CO2*Par$Par.CPI)^(1/3))
Par = cbind(Par, Par.mean)
Par

Per = cbind(Per.debt.1, Per.CO2) 
Per = cbind(Per, Per.CPI.1) %>%
  dplyr::select(date, Per.debt, Per.CO2, Per.CPI)
Per.mean= ((Per$Per.debt*Per$Per.CO2*Per$Per.CPI)^(1/3))
Per = cbind(Per, Per.mean)
Per

Uru = cbind(Uru.debt.1, Uru.CO2) 
Uru = cbind(Uru, Uru.CPI.1) %>%
  dplyr::select(date, Uru.debt, Uru.CO2, Uru.CPI)
Uru.mean= ((Uru$Uru.debt*Uru$Uru.CO2*Uru$Uru.CPI)^(1/3))
Uru = cbind(Uru, Uru.mean)
Uru

Arg



countrynames = c("country", "date", "debt", "CO2", "CPI", "mean")

Argentina = "Argentina"
Argentina = rep(Argentina, length.out = 15)
Arg = cbind(Argentina, Arg)
colnames(Arg) = countrynames
Arg

Bolivia = "Bolivia"
Bolivia = rep(Bolivia, length.out = 15)
Bol = cbind(Bolivia, Bol)
Bol
colnames(Bol) = countrynames
Bol

Brazil = "Brazil"
Brazil = rep(Brazil, length.out = 15)
Bra = cbind(Brazil, Bra)
Bra
colnames(Bra) = countrynames
Bra

Chile = "Chile"
Chile = rep(Chile, length.out = 15)
Chi = cbind(Chile, Chi)
Chi
colnames(Chi) = countrynames
Chi

a = "Colombia"
b = rep(a, length.out = 15)
Col = cbind(b, Col)
colnames(Col) = countrynames

a = "Costa Rica"
b = rep(a, length.out = 15)
Cos = cbind(b, Cos)
colnames(Cos) = countrynames
Cos

a = "Dominican Republic"
b = rep(a, length.out = 15)
Dom = cbind(b, Dom)
colnames(Dom) = countrynames
Dom

a = "Ecuador"
b = rep(a, length.out = 15)
Ecu = cbind(b, Ecu)
colnames(Ecu) = countrynames
Ecu

a = "El Salvador"
b = rep(a, length.out = 15)
Els = cbind(b, Els)
colnames(Els) = countrynames
Els

a = "Mexico"
b = rep(a, length.out = 15)
Mex = cbind(b, Mex)
colnames(Mex) = countrynames
Mex

a = "Panama"
b = rep(a, length.out = 15)
Pan = cbind(b, Pan)
colnames(Pan) = countrynames
Pan

a = "Paraguay"
b = rep(a, length.out = 15)
Par = cbind(b, Par)
colnames(Par) = countrynames
Par

a = "Peru"
b = rep(a, length.out = 15)
Per = cbind(b, Per)
colnames(Per) = countrynames
Per

a = "Uruguay"
b = rep(a, length.out = 15)
Uru = cbind(b, Uru)
colnames(Uru) = countrynames
Uru

AB = rbind(Arg, Bol)  
ABr = rbind(AB, Bra)
ACh = rbind(ABr, Chi)
ACo = rbind(ACh, Col)
ACR = rbind(ACo, Cos)
AD = rbind(ACR, Dom)
AEc = rbind(AD, Ecu)
AEl = rbind(AEc, Els)
AM = rbind(AEl, Mex)
APn = rbind(AM, Pan)
APg = rbind(APn, Par)
APe = rbind(APg, Per)
AU = rbind(APe, Uru)
AU
class(AU)

summary(AU)

countrynames

hist(AU$mean)

ggplot(AU, aes(date, mean, col = country))+
  geom_point()+
  geom_line()+
  facet_wrap(~country)

# Build a Panel Data Model

AU.fe <- plm(mean~date, data = AU, model = "within")
summary(AU.fe)
AU.po <- plm(mean~date, data = AU, model = "pooling")
summary(AU.po)
AU.fd <- plm(mean~date, data = AU, model = "fd")
summary(AU.fd)
AU.bt <- plm(mean~date, data = AU, model = "between")
summary(AU.bt)
AU.ra <- plm(mean~date, data = AU, model = "random")
summary(AU.ra)

fitted.AU.fe = function(AU.fe)
  AU.fe$model[[1]] - AU.fe$residuals

# Mapping

AU.2014 = AU %>%
  dplyr::filter(date == 2014)
AU.2014

library(rworldmap)

library(rworldmap)  
df <- NULL  
df$country <- c("Brazil","Mexico","Argentina")  
df$code<-c("BRA", "MEX", "ARG")  
df$popsize<-c(1000, 5000, 200)  
df<-as.data.frame(df)  
sPDF <- joinCountryData2Map( df, joinCode = "ISO3", nameJoinColumn = "code")  
mapCountryData(sPDF, nameColumnToPlot="popsize", mapRegion='latin america')

df <- NULL  
df$country <- c("Argentina", "Bolivia", "Brazil", "Chile", "Costa Rica", "Ecuador", "Dominican Republic", "Ecuador", "El Salvador", "Mexico", "Panama", "Paraguay", "Peru", "Uruguay")
df$code<- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "SLV", "MEX", "PAN", "PRY", "PER", "URY")  
df$New.Development.Indicator = AU$mean
df<-as.data.frame(df)  
sPDF <- joinCountryData2Map( df, joinCode = "NAME", nameJoinColumn = "code") 
mapCountryData(sPDF, nameColumnToPlot="New.Development.Indicator", mapRegion='latin america')


lat.am = mapCountryData(sPDF, nameColumnToPlot="mean", mapRegion='latin america',xlim=bbox(sPDFmyCountries)[1,], ylim=bbox(sPDFmyCountries)[2,])

sPDFmyCountries <- sPDF[sPDF$NAME %in% df$country,]
#use the bbox to define xlim & ylim
k = bbox(sPDFmyCountries)

x = (-34.72999+117.12776)/2
x
y = (32.72083+55.61183)/2
y

map = get_map(location = k, maptype = "toner")
ggmap(map)
class(map)

# I need to fortify the data AND keep trace of the commune code! (Takes ~2 minutes)
library(broom)
spdf_fortified <- tidy(lat.am, region = "code")

# Now I can plot this shape easily as described before:
library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

?get_map


library(ggmap)
library(leaflet)

?joinCountryData2Map


# Forecasting 
Arg.ar = auto.arima(Arg.mean)
summary(Arg.ar)
Arg.for = forecast(Arg$Arg.mean, level = 95, model = Arg.ar)
