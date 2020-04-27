#get url
library(rvest)
library(dplyr, help, pos = 2, lib.loc = NULL)
library(purrr, help, pos = 2, lib.loc = NULL)
live_data=function() {
url="https://www.worldometers.info/coronavirus/"
#read HTML
data=read_html(url)
d=data%>%
html_nodes("span")
cases=d[5]%>%
html_text()
deaths=d[6]%>%
html_text()
recovery=d[7]%>%
html_text()
active=data%>%html_nodes(".number-table-main")
active=active[1]%>%
html_text()
result=c(cases,deaths,recovery,active)
result=gsub(",","",result)
result=as.numeric(result)
return(result)

}
