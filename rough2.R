library(dplyr)


data=read.csv("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/coronavirus.csv")
country_data=read.csv("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/countries_codes_and_coordinates.csv")
if(all(unique(data$country) %in% country_data$country==FALSE))
  print("Error!!")

df=merge(data,country_data,by="country")
df$date=as.Date(df$date)
df=df[,-c(4,12:16,19,21)]
colnames(df)[1:13]=c("Country","jhu_id","Date","Confirmed","New_cases","Deaths","New_deaths","Recoveries","New_recoveries","Active","Lat","Long","Population")


#cumulative cases
cumu_data=df%>%
  select("Confirmed","Deaths","Recoveries","Active")%>%
  aggregate(by=list(df$Date),FUN = sum)
colnames(cumu_data)[1]="Date"
#daily_cases
daily_data=df%>%
  select("New_cases","New_deaths","New_recoveries")%>%
  aggregate(by=list(df$Date),FUN= sum)
colnames(daily_data)=c("Date","Confirmed","Deaths","Recovered")
#countrywise cases
countrywise=aggregate(df[,c(5:11,20)],by=list(df$country),FUN = sum)
 

  
country="USA"
t=df%>%
  filter(Country==country)
  
  max(t$Poulation)

  pop=read.csv("new_horizon_dataframe.csv")
  colnames(pop)[]
  pop1=aggregate(pop[,c(5,13)],by=list(pop$Country.Region ),FUN = mean)
  colnames(pop1)[1]="jhu_id"
  df2=merge(df,pop1,by="jhu_id")
  df2[is.na(df2)]=0
  
colnames(df2)[16:17]=c("Pop_density","Avg_temp")  

df2%>%filter(Country==country)%>%select("Pop_density")%>%max()

df2%>%filter(Country==country)%>%filter(Date<=date)
