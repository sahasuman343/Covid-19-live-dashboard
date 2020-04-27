case_cumu=function(a,b,c,country){
  country=as.character(country)
  conf=a
  deaths=b
  recov=c
  date=seq(from = as.Date("2020-01-22"), length.out = ncol(conf)-4, by = 'day')
  
  data=subset(conf,conf$Country.Region==country)
  d1=colSums(data[,5:ncol(data)])
  data=subset(deaths,deaths$Country.Region==country)
  d2=colSums(data[,5:ncol(data)])
  data=subset(recov,recov$Country.Region==country)
  d3=colSums(data[,5:ncol(data)])
  df=cbind.data.frame(date,d1,d2,d3)
  colnames(df)=c("Date","Confirmed","Deaths","Recovered")
  rownames(df)=NULL
  return(df)
}

casestudy_data=function(input_df){
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
  colnames(input_df[,-1])=unique(input_df$Country.Region)
  return(input_df)
}

misc_data=function(dat,country){
  pop=read.csv("new_horizon_dataframe.csv")
  s_data=dat[,c(7,8,12)]
  s_data[is.na(s_data)]=0
  popln=subset(s_data,Country_Region==country)$Population%>%
    sum()
  s2_data=pop[,c(1:3,13)]
  colnames(s2_data)[3:4]=c("area","temp")
  #s_data[is.na(s2_data)]=0
  #library(dplyr)
  area=subset(s2_data,s2_data$Country.Region==country)$area%>%
    sum()
  temp=subset(s2_data,s2_data$Country.Region==country)$temp%>%
    sum()
  
  df=cbind.data.frame(popln,area,temp)
  colnames(df)=c("Population","Area","Temp")
  return(df)
  
}








cumu_data=function(a,b,c){
  data1=a#read.csv("time_series_covid19_confirmed_global.csv")
  data2=b#read.csv("time_series_covid19_deaths_global.csv")
  data3=c#read.csv("time_series_covid19_recovered_global.csv")
  conf=c()
  death=c()
  recov=c()
  for (i in 5:ncol(data1))
  {
    conf[i]=sum(data1[,i])
  }
  
  conf=conf[-c(1:4)]
  
  for (i in 5:ncol(data2))
  {
    death[i]=sum(data2[,i])
  }
  
  death=death[-c(1:4)]
  
  for (i in 5:ncol(data3))
  {
    recov[i]=sum(data3[,i])
  }
  
  recov=recov[-c(1:4)]
  
  date=seq(from = as.Date("2020-01-22"), length.out = length(conf), by = 'day')
  
  cum_data=data.frame(cbind(conf,death,recov,conf-death-recov))
  cum_data=cbind(date,cum_data)
  colnames(cum_data)=c("Date","Confirmed","Deaths","Recoveries","Active")
  return(cum_data)
}


daily_data=function(cumu_data){
  conf=cumu_data$Confirmed
  death=cumu_data$Deaths
  recov=cumu_data$Recoveries
  
  
  for (i in (2:length(conf)-1))
  {
    conf[i]=conf[i+1]-conf[i]
  }
  
  
  for (i in (2:length(death)-1))
  {
    death[i]=death[i+1]-death[i]
  }
  
  
  
  for (i in (2:length(recov)-1))
  {
    recov[i]=recov[i+1]-recov[i]
  }
  date=seq(from = as.Date("2020-01-22"), length.out = length(conf)-1, by = 'day')
  
  n=length(recov)
  daily_data=data.frame(cbind(conf[-n],death[-n],recov[-n]))
  daily_data=cbind(date,daily_data)
  colnames(daily_data)=c("Date","Confirmed","Deaths","Recoveries")
  return(daily_data)
}

country_data=function(x,y,z){
  conf=x   #read.csv("https://raw.githubusercontent.com/sahasuman343/COVID-19-usefull-data/master/time_series_covid19_confirmed_global.csv")
  conf["new_cases"]=conf[,ncol(conf)]-conf[,ncol(conf)-1]
  conf=conf[,-c(3:(ncol(conf)-2))]
  
  death=y    #read.csv("https://raw.githubusercontent.com/sahasuman343/COVID-19-usefull-data/master/time_series_covid19_deaths_global.csv")
  death["new_cases"]=death[,ncol(death)]-death[,ncol(death)-1]
  death=death[,-c(3:(ncol(death)-2))]
  
  recov=z       #read.csv("https://raw.githubusercontent.com/sahasuman343/COVID-19-usefull-data/master/time_series_covid19_recovered_global.csv")
  recov["new_cases"]=recov[,ncol(recov)]-recov[,ncol(recov)-1]
  recov=recov[,-c(3:(ncol(recov)-2))]
  
  #population_data=read.csv("data/population_by_country_2020.csv")
  country=unique(conf$Country.Region)
  a=b=c=d=e=f=mort=recovery=c()
  Population=c()
  population_Density=c()
  fertility_rate=c()
  for (i in country){
    temp1=conf[conf$Country.Region==i,]
    a[i]=sum(temp1[,3])
    b[i]=sum(temp1[,4])
    
    temp2=death[death$Country.Region==i,]
    c[i]=sum(temp2[,3])
    d[i]=sum(temp2[,4])
    
    temp3=recov[recov$Country.Region==i,]
    e[i]=sum(temp3[,3])
    f[i]=sum(temp3[,4])
    
    #Population[i]=subset(population_data,population_data$Country..or.dependency.==i)$Population..2020.
    # population_Density[i]=subset(population_data,population_data$Country..or.dependency.==i)$Density..P.KmÂ²
    #fertility_rate[i]=subset(population_data,population_data$Country..or.dependency.==i)$Fert..Rate
    
    mort[i]=round(c[i]/a[i],4)
    recovery[i]=round(e[i]/a[i],4)
  }
  
  df=cbind.data.frame(country,a,b,c,d,e,f,a-c-e,mort,recovery)
  colnames(df)=c("Country","Confirmed","New Cases","Deaths","New Deaths","Recovered","New Recovered","Active","Mortality Rate","Recovery Rate")
  rownames(df)=c()
  
  return(df)
}

