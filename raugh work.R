state="Maharashtra"
d=subset(covid_states,covid_states$States==state)
d["Active"]=d$Confirmed-d$Deaths-d$Cured
d=d[,-2]
library(ggplot2)
library(plotly)
library(reshape)
t=melt(d)

p=ggplot(t, aes(fill=variable, y=value, x=States)) + 
  geom_bar(position="stack", stat="identity")

ggplotly(p)


library(leaflet)
map=leaflet()
map=addTiles(map) %>%
map
d=read.csv("https://raw.githubusercontent.com/sahasuman343/COVID-19-usefull-data/master/countrywise%20data.csv")
d["Active"]=d$Confirmed-d$Deaths-d$Recovered
d=d[,-2]
d1=subset(d,d$Country=="China")
library(reshape)
t=melt(d1)
library(dplyr)
library(tidyverse)
data=read.csv("time_series_covid19_cumudf_global.csv")
d=read.csv("time_series_covid19_cumudf_global.csv")

data$Confirmed=log10(data$Confirmed)
data$Deaths=log10(data$Deaths)
data$Recoveries=log10(data$Recoveries)
data$Active=log10(data$Active)
View(data)
p=ggplot(data)+
  geom_line(aes(x=Days,y=Confirmed))

conf=read.csv("time_series_covid19_confirmed_global.csv")
country="China"
data=subset(conf,conf$Country.Region==country)
d1=colSums(data[,5:ncol(data)])
cumu_conf=d1
daily_conf=d1
for (i in 2:(length(d1)-1))
{
  daily_conf[i]=daily_conf[i+1]-daily_conf[i]
  
}
n=nrow
daily_conf=daily_conf[-length(d1)]

#
library(dplyr)
library(stringr)
library(stringi)
update_jhu = function(input_df) {
  names(input_df)[1:2] = c("Province", "Country")
  input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
  input_df$Country[input_df$Province=="Macau"] = "Macao"
  input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
  input_df$Country[input_df$Country=="Korea, South"] = "RepublicofKorea"
  input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
  input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
  input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
  input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
  input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
  input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
  input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
  input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
  input_df$Country = input_df$Country %>% str_replace_all(., " ", "") 
  dates = names(input_df)[4:ncol(input_df)]
  input_df = input_df %>% 
    select(-c(Province, Lat, Long)) %>% 
    group_by(Country) %>% 
    summarise_each(funs(sum)) %>%
    data.frame()
  rownames(input_df) = input_df$Country
  #rownames(input_df) = paste0(input_df$Country,"_",tag)
  input_df = input_df %>% select(-c(Country)) %>% t()
  input_df = data.frame(input_df)
  input_df$Date = dates
  rownames(input_df) = 1:nrow(input_df)
  input_df$Date = format(as.Date(input_df$Date,"%m/%d/%y"))
  input_df
}
input_df=read.csv("time_series_covid19_confirmed_global.csv")
#dates = names(input_df)[5:ncol(input_df)]
#date=seq(1,length.out = ncol(input_df)-4,by=1)
names(input_df)[1:2] = c("Province", "Country")
#names(input_df)[5:ncol(input_df)]=date
input_df = input_df %>% 
  select(-c(Province, Lat, Long)) %>% 
  group_by(Country) %>% 
  summarise_each(funs(sum)) %>%
  data.frame()

rownames(input_df) = input_df$Country
input_df = input_df %>% select(-c(Country)) %>% t()
input_df = data.frame(input_df)
#input_df$Date = dates
rownames(input_df) = c()
Date = seq(as.Date("2020-01-22"),length.out = nrow(input_df),by="day")
input_df=cbind.data.frame(Date,input_df)

c="US"

d=input_df[names(input_df)==c]

library(RColorBrewer)
str(display.brewer.pal(4,"BrBG"))
library(reshape)
library(ggplot2)
source("main.R")
data=read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv")

data$Date=as.Date(data$Date)
country=c("US","Italy","India")
subset=data[data$Country.Region==country,]
g=ggplot(subset,aes(x=Date,y=Confirmed,group=subset$Country.Region))+
  geom_line(aes(color=subset$Country.Region))+theme(axis.text.x = element_text(angle =90))

