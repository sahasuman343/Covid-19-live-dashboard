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

