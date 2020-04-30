#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(wordcloud)
library(gganimate)
library(gridExtra)

source("main.R")
source("live data.R")
# Define server 
shinyServer(function(input, output,session) {
   output$currenttime= renderText({
      
      invalidateLater(1, session)
      
      format(Sys.time())
   })
    
   livedata=reactive({
      live_data()
   })
  
    conf=reactive({
       data=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
       data
       })
    death=reactive({
       data=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
       data
    })
    recov=reactive({
       data=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
       data
    })
    agg_data=reactive({
      data=read.csv("data/coronavirus.csv")
      country_data=read.csv("data/countries_codes_and_coordinates.csv")
      if(all(unique(data$country) %in% country_data$country==FALSE))
        print("Error!!")
      
      df=merge(data,country_data,by="country")
      df$date=as.Date(df$date)
      df=df[,-c(4,12:16,19,21)]
      colnames(df)[1:13]=c("Country","jhu_id","Date","Confirmed","New_cases","Deaths","New_deaths","Recoveries","New_recoveries","Active","Lat","Long","Population")
      
      return(df)
    })
    df=reactive({
      df=agg_data()
       d= df%>%
        select("Confirmed","Deaths","Recoveries","Active")%>%
        aggregate(by=list(df$Date),FUN = sum)
       colnames(d)[1]="Date"
       return(d)
    })
    daily=reactive({
      df=agg_data()
      daily_data=df%>%
        select("New_cases","New_deaths","New_recoveries")%>%
        aggregate(by=list(df$Date),FUN= sum)
      colnames(daily_data)=c("Date","Confirmed","Deaths","Recoveries")
      return(daily_data)
      
    })
    current_date=reactive({
      max(df()$Date)
    })
    countrywise=reactive({
      country_data(conf(),death(),recov())
    })
    
    
   output$conf=renderText({
       cases=livedata()[1]
       paste0(cases, " Cases")
   })
   output$deaths=renderText({
       death_cases=livedata()[2] 
       paste0(prettyNum(death_cases), "  Deaths")
   })
   output$recov=renderText({
       recov_cases=livedata()[3]  
       paste0(prettyNum(recov_cases), "  Recovered")
   })
   output$active=renderText({
       active_cases=livedata()[4]  
       paste0(prettyNum(active_cases), "  Active Cases")
       
   })
   output$country=renderText({
       c=length(unique(agg_data()$Country))
       paste0(prettyNum(c), big.mark=" ", "Countries/Regions Affected")
       
   })
   output$date_input=renderUI({
     max_date=current_date()
     start=as.Date("2020-01-22")
     dateInput("date_upto","Select Date",start = max_date,min = start
               )
   })
   output$global_mort=renderText({
      mort=livedata()[2]/livedata()[1]
      mort=round(mort*100,2)
      paste0("Current Mortality Rate: ",prettyNum(mort)," %")
   })
   output$global_recov=renderText({
      rec=livedata()[3]/livedata()[1]
      rec=round(rec*100,2)
      paste0("Current Recovery Rate: ",prettyNum(rec)," %")
   })
   
  output$date_upto=renderUI({
    min_date=min(df()$Date)
    max_date=max(df()$Date)
    sliderInput("date",
                label = h6("select date"),
                min=as.Date(min_date,"%Y-%m-%d"),
                max = as.Date(max_date,"%Y-%m-%d"),
                value = as.Date(max_date,"%Y-%m-%d"),
                timeFormat = "%d %b") 
               
  })
  output$daily_conf=renderPlot({
    data=daily()   
    p = ggplot(data,aes(x=Date,y=Confirmed))+
      geom_bar(position="stack", stat="identity",fill="green")+
      xlab("Date")+
      ylab("Confirmed Cases")+
      ggtitle("Daily New Cases ")+theme(legend.position = "none")
    p
  })



   output$stacked=renderPlot({
      d=countrywise()%>%
        select(Country,Confirmed,Deaths,Recovered)
      d=d[order(d$Confirmed,decreasing = TRUE),]#ordering data by Confirmed cases
      d["Active"]=d$Confirmed-d$Deaths-d$Recovered
      d=d[,-2]
      d1=d[1:5,]#taking top 5 countries
      t=melt(d1)#making dataframe suitable for stagged bar plot
      #View(t)
      
      p=ggplot(t, aes(fill=variable, y=value, x=Country)) + 
         geom_bar(position="stack", stat="identity")+
         ggtitle("Top 5 Countries")
      
      p
   })
   
   output$mymap <- renderLeaflet({ 
     
      mymap=leaflet(conf())%>%
         addTiles()%>%
         addProviderTiles("CartoDB.DarkMatter")%>%
         addLayersControl(
           baseGroups = c("Dark Theme","Light Theme"),
          position = "bottomright",
          overlayGroups = c("Confirmed Cases","Death Cases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup(c("Light Theme","Death Cases"))  %>%
        #addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(~-100,-50,~80,80) 
      
   
      mymap
   })
   
   map_data=reactive({
     date=as.Date(input$date)
     df=agg_data()%>%
       filter(Date==date)
     return(df)
   })
   observeEvent(input$date,{
     leafletProxy("mymap") %>% 
       clearMarkers() %>%
       addProviderTiles("CartoDB.DarkMatter",group="Dark")%>%
       addProviderTiles("CartoDB.Positron",group = "Light Theme")%>%
       addCircleMarkers(data=map_data(),lng = ~Long ,lat = ~Lat,
                        weight = 1,
                        radius = ~sqrt(Confirmed)/30,
                        fillOpacity = 0.1,
                        color = "red",
                        label = sprintf("<strong>%s</strong><br/>Confirmed cases: %g<br/>Deaths: %d", map_data()$Country, map_data()$Confirmed, map_data()$Deaths) %>% lapply(htmltools::HTML),
                        group = "Confirmed Cases"
                        )%>%
       addCircleMarkers(data=map_data(),lng = ~Long ,lat = ~Lat,
                        weight = 1,
                        radius = ~sqrt(Deaths)/15,
                        fillOpacity = 0.1,
                        color = "blue",
                        label = sprintf("<strong>%s</strong><br/>Confirmed cases: %g<br/>Deaths: %d", map_data()$Country, map_data()$Confirmed, map_data()$Deaths) %>% lapply(htmltools::HTML),
                        group = "Death Cases"
       )
     
   })
   
   
   

   #Vizualization tab
 df1=reactive({
   df1=df()
   df1$Confirmed=log10(df()$Confirmed)
   df1$Deaths=log10(df()$Deaths)
   df1$Recoveries=log10(df()$Recoveries)
   return(df1)
 })
 viz_data=reactive({
   
   if( input$log==TRUE)
     return(df1())
   else
     return(df())
   
 })
   
  output$cumu=renderPlotly({
    #Sys.sleep(5)
    data=subset(viz_data(),Date<=input$date_upto)
     if(input$case_select=="Confirmed")
     {
        p = ggplot(data,aes(x=Date))+
           geom_line(aes(y=Confirmed),color ="#ff0000",alpha=0.9,linetype="twodash")+
           xlab("Date")+
           ylab("Confirmed Cases")+
           ggtitle("Confirmed Cases over the World")+theme(
              legend.position = "none",
              plot.title = element_text(hjust="centre"),
              title = element_text("bold",color='red'),
              axis.line.x  = element_line('yellow'),
              axis.line.y = element_line("yellow"),
              axis.text.x = element_text("red"),
              axis.text.y = element_text("red"),
              panel.grid.major = element_line("pink"),
              panel.grid.minor = element_line("pink"),
              plot.background = element_rect("black"),
              panel.background = element_rect("black"),
              axis.title.x = element_text(color="orange"),
              axis.title.y = element_text(color = "orange")
           )+scale_color_gradient(low="blue",high="red")
        
     }
     else if(input$case_select=="Deaths")
     {
        p = ggplot(data,aes(x=Date))+
           geom_line(aes(y=Deaths),color ="#66ff33",alpha=0.9,linetype="twodash")+
           xlab("Date")+
           ylab("Death Cases")+
           ggtitle("Death Cases over the World")+theme(
              legend.position = "none",
              plot.title = element_text(hjust="centre"),
              title = element_text("bold",color='red'),
              axis.line.x  = element_line('yellow'),
              axis.line.y = element_line("yellow"),
              axis.text.x = element_text("red"),
              axis.text.y = element_text("red"),
              panel.grid.major = element_line("pink"),
              panel.grid.minor = element_line("pink"),
              plot.background = element_rect("black"),
              panel.background = element_rect("black"),
              axis.title.x = element_text(color="orange"),
              axis.title.y = element_text(color = "orange")
           )+scale_color_gradient(low="blue",high="red")
        
     }
     else
     {
        p = ggplot(data,aes(x=Date))+
           geom_line(aes(y=Recoveries),color ="blue",alpha=0.9,linetype="twodash")+
           xlab("Date")+
           ylab("Recovered Cases")+
           ggtitle("Recovered Cases over the World")+theme(
              legend.position = "none",
              plot.title = element_text(hjust="centre"),
              title = element_text("bold",color='red'),
              axis.line.x  = element_line('yellow'),
              axis.line.y = element_line("yellow"),
              axis.text.x = element_text("red"),
              axis.text.y = element_text("red"),
              panel.grid.major = element_line("pink"),
              panel.grid.minor = element_line("pink"),
              plot.background = element_rect("black"),
              panel.background = element_rect("black"),
              axis.title.x = element_text(color="orange"),
              axis.title.y = element_text(color = "orange")
           )+scale_color_gradient(low="blue",high="red")
        
     }
     
     ggplotly(p)
  })
  
  output$daily=renderPlotly({
    data=subset(daily(),Date<=input$date_upto)
     if(input$case_select=="Confirmed")
     {
        p = ggplot(data,aes(x=Date,y=Confirmed,fill='#fc0b03'))+
           geom_bar(position="stack", stat="identity")+
           xlab("Date")+
           ylab("Confirmed")+
           ggtitle("Daily Cases ")+theme(
              legend.position = "none",
              plot.title = element_text(hjust="centre"),
              title = element_text("bold",color='red'),
              axis.line.x  = element_line('yellow'),
              axis.line.y = element_line("yellow"),
              axis.text.x = element_text("red"),
              axis.text.y = element_text("red"),
              panel.grid.major = element_line("pink"),
              panel.grid.minor = element_line("pink"),
              plot.background = element_rect("black"),
              panel.background = element_rect("black"),
              axis.title.x = element_text(color="orange"),
              axis.title.y = element_text(color = "orange")
           )+scale_color_gradient(low="blue",high="red")
        
     }
     else if(input$case_select=="Deaths")
     {
        p = ggplot(data,aes(x=Date,y=Deaths,fill='#059b00'))+
           geom_bar(position="stack", stat="identity")+
           xlab("Date")+
           ylab("Deaths")+
           ggtitle("Daily Death Cases ")+theme(
              legend.position = "none",
              plot.title = element_text(hjust="centre"),
              title = element_text("bold",color='red'),
              axis.line.x  = element_line('yellow'),
              axis.line.y = element_line("yellow"),
              axis.text.x = element_text("red"),
              axis.text.y = element_text("red"),
              panel.grid.major = element_line("pink"),
              panel.grid.minor = element_line("pink"),
              plot.background = element_rect("black"),
              panel.background = element_rect("black"),
              axis.title.x = element_text(color="orange"),
              axis.title.y = element_text(color = "orange")
           )+scale_color_gradient(low="blue",high="red")
        
     }
     else
     {
        p = ggplot(data,aes(x=Date,y=Recoveries,fill='#0246fd'))+
           geom_bar(position="stack", stat="identity")+
           xlab("Date")+
           ylab("Recoveries ")+
           ggtitle("Daily Recovery Cases ")+theme(
              legend.position = "none",
              plot.title = element_text(hjust="centre"),
              title = element_text("bold",color='red'),
              axis.line.x  = element_line('yellow'),
              axis.line.y = element_line("yellow"),
              axis.text.x = element_text("red"),
              axis.text.y = element_text("red"),
              panel.grid.major = element_line("pink"),
              panel.grid.minor = element_line("pink"),
              plot.background = element_rect("black"),
              panel.background = element_rect("black"),
              axis.title.x = element_text(color="orange"),
              axis.title.y = element_text(color = "orange")
           )+scale_color_gradient(low="blue",high="red")
        
     }
     
     ggplotly(p)
     
  })
  df2=reactive({
     temp1=df()$Deaths/df()$Confirmed
     temp2=df()$Recoveries/df()$Confirmed
     df2=cbind.data.frame(df()$Date,temp1,temp2)
     colnames(df2)=c("Date","Mortality.Rate","Recovery.Rate")
     return(df2)
  })
  output$mort=renderPlotly({
     d=subset(df2(),Date<=input$date_upto)
     m1 = ggplot(d,aes(x=Date))+
        geom_line(aes(y=Mortality.Rate),color ="red2",alpha=0.9,linetype="twodash")+
        geom_abline(intercept=mean(d$Mortality.Rate),slope=0,color='navy',linetype=2)+
        xlab("Date")+
        ylab("Mortality Rate")+
        ggtitle("Mortality rate over time")+theme(
           legend.position = "none",
           plot.title = element_text(hjust="centre"),
           title = element_text("bold",color='red'),
           axis.line.x  = element_line('yellow'),
           axis.line.y = element_line("yellow"),
           axis.text.x = element_text("red"),
           axis.text.y = element_text("red"),
           panel.grid.major = element_line("pink"),
           panel.grid.minor = element_line("pink"),
           plot.background = element_rect("black"),
           panel.background = element_rect("black"),
           axis.title.x = element_text(color="orange"),
           axis.title.y = element_text(color = "orange")
        )+scale_color_gradient(low="blue",high="red")
     ggplotly(m1)
  })
  output$recov_rate=renderPlotly({
     d=subset(df2(),Date<=input$date_upto)
     m2 = ggplot(d,aes(x=Date))+
        geom_line(aes(y=Recovery.Rate),color ="red2",alpha=0.9,linetype="twodash")+
        geom_abline(intercept=mean(d$Recovery.Rate),slope=0,color='navy',linetype=2)+
        xlab("Date")+
        ylab("Recovery Rate")+
        ggtitle("Recovery rate over time")+theme(
           legend.position = "none",
           plot.title = element_text(hjust="centre"),
           title = element_text("bold",color='red'),
           axis.line.x  = element_line('yellow'),
           axis.line.y = element_line("yellow"),
           axis.text.x = element_text("red"),
           axis.text.y = element_text("red"),
           panel.grid.major = element_line("pink"),
           panel.grid.minor = element_line("pink"),
           plot.background = element_rect("black"),
           panel.background = element_rect("black"),
           axis.title.x = element_text(color="orange"),
           axis.title.y = element_text(color = "orange")
        )+scale_color_gradient(low="blue",high="red")
     ggplotly(m2)
     
  })
  
  #casestudies tab
  output$Countries=renderUI({
     count=agg_data()$Country%>%unique()
     selectInput("c","Select Country",choices=count)
  })
  output$plotting_date=renderUI({
    max_date=max(agg_data()$Date)
    start=min(agg_data()$Date)
    dateInput("plot_date","Select Date",value = as.Date(max_date,"%Y-%m-%d")  ,max = as.Date(max_date,"%Y-%m-%d"),min = as.Date(start,"%Y-%m-%d")
    )
    
  })
  
  casestudy_df=reactive({
    df=agg_data()
    pop=read.csv("data/new_horizon_dataframe.csv")
    colnames(pop)[]
    pop1=aggregate(pop[,c(5,13)],by=list(pop$Country.Region ),FUN = mean)
    colnames(pop1)[1]="jhu_id"
    df2=merge(df,pop1,by="jhu_id")
    df2$Country=as.character(df2$Country)
    df2[is.na(df2)]=0
    colnames(df2)[16:17]=c("Pop_density","Avg_temp")
    
    df2%>%filter(Country==input$c)%>%filter(Date<=input$plot_date)
  })
  d=reactive({

    casestudy_df()%>%filter(Date==input$plot_date)
})
 output$map=renderLeaflet({
     d=casestudy_df()
     map=leaflet(d)%>%
       addTiles()%>%
       addMarkers(lng = d$Long,lat = d$Lat)%>%
       fitBounds(~-100,-50,~80,80) 
   
     map
   })
  
   output$c_conf=renderText({
     
     c=casestudy_df()%>%select("Confirmed")%>%max()
     paste0("Confirmed: ",prettyNum(c))
    
   })
   output$c_conf_new=renderText({
     
     new=d()$New_cases
     paste0(" +",prettyNum(new))
     
   })
   
   output$c_death=renderText({
     d=casestudy_df()%>%select("Deaths")%>%max()
     paste0("Deaths: ",prettyNum(d))
   })
   output$c_death_new=renderText({
     paste0(" +",prettyNum(d()$New_deaths))
     
   })
   output$c_recov=renderText({
     r=casestudy_df()%>%select("Recoveries")%>%max()
    paste0("Recovered: ",prettyNum(r))
   })
   output$c_recov_new=renderText({
     paste0(" +",prettyNum(d()$New_recoveries))
     
   })
   output$c_mort=renderText({
     d=casestudy_df()%>%select("Deaths")%>%max()
     c=casestudy_df()%>%select("Confirmed")%>%max()
     paste0("Mortality Rate: ",prettyNum(100*d/c),"%")
   })
   output$c_recovr=renderText({
     r=casestudy_df()%>%select("Recoveries")%>%max()
     
     c=casestudy_df()%>%select("Confirmed")%>%max()
     
     paste0("Recovery Rate: ",prettyNum(100*r/c),"%")
   })
   output$population=renderText({
      paste0("Population: ",prettyNum(d()$Population))
   })
   output$density=renderText({
      paste0("Population Density: ",prettyNum(d()$Pop_density),"/sq kms")
   })
   output$temp=renderText({
      paste0("Avg. Temp.: ",prettyNum(d()$Avg_temp),"°C")
   })   


#case study plots

   output$case_cumu_plot=renderPlotly({
    
     
     data=casestudy_df()
     p1 = ggplot(data,aes(x=Date))+
       geom_line(aes(y=Confirmed),color ="#ff0000")+
       geom_line(aes(y=Deaths),color ="#00cc00")+
       geom_line(aes(y=Recoveries),color ="#0000ff")+
       xlab("Date")+
       ylab("Confirmed Cases")+theme(
          legend.position = "none",
          plot.title = element_text(hjust="centre"),
          axis.text.x.bottom = element_text("red"),
          axis.text.y.left = element_text("red"),
          title = element_text("bold",color='red'),
          axis.line.x  = element_line('yellow'),
          axis.line.y = element_line("yellow"),
          axis.text.x = element_text("red"),
          axis.text.y = element_text("red"),
          panel.grid.major = element_line("pink"),
          panel.grid.minor = element_line("pink"),
          plot.background = element_rect("#1f1e1e"),
          panel.background = element_rect("#1f1e1e"),
          axis.title.x = element_text(color="orange"),
          axis.title.y = element_text(color = "orange")
          
       )

     
     ggplotly(p1)
     
   })

   output$case_daily_plot_conf=renderPlotly({
     data=casestudy_df()
     p=ggplot(data,aes(x=Date,y=New_cases,fill="#fe0202"))+
       geom_bar(position="stack", stat="identity")+
       xlab("Date")+
       ylab("Confirmed Cases")+
       ggtitle("New Cases")+theme(
         legend.position = "none",
         plot.title = element_text(hjust="centre"),
         axis.text.x.bottom = element_text("red"),
         axis.text.y.left = element_text("red"),
         title = element_text("bold",color='red'),
         axis.line.x  = element_line('yellow'),
         axis.line.y = element_line("yellow"),
         axis.text.x = element_text("red"),
         axis.text.y = element_text("red"),
         panel.grid.major = element_line("pink"),
         panel.grid.minor = element_line("pink"),
         plot.background = element_rect("#1f1e1e"),
         panel.background = element_rect("#1f1e1e"),
         axis.title.x = element_text(color="orange"),
         axis.title.y = element_text(color = "orange")
         
       )
     ggplotly(p)
   })
   output$case_daily_plot_death=renderPlotly({
     data=casestudy_df()
     p=ggplot(data,aes(x=Date,y=New_deaths))+
       geom_bar(aes(fill="blue"),position="stack", stat="identity")+
       xlab("Date")+
       ylab("Death Cases")+
       ggtitle("New Death Cases")+theme(
         legend.position = "none",
         plot.title = element_text(hjust="centre"),
         axis.text.x.bottom = element_text("red"),
         axis.text.y.left = element_text("red"),
         title = element_text("bold",color='red'),
         axis.line.x  = element_line('yellow'),
         axis.line.y = element_line("yellow"),
         axis.text.x = element_text("red"),
         axis.text.y = element_text("red"),
         panel.grid.major = element_line("pink"),
         panel.grid.minor = element_line("pink"),
         plot.background = element_rect("#1f1e1e"),
         panel.background = element_rect("#1f1e1e"),
         axis.title.x = element_text(color="orange"),
         axis.title.y = element_text(color = "orange")
       )
     ggplotly(p)
   })
   output$case_daily_plot_recov=renderPlotly({
     data=casestudy_df()
     p=ggplot(data,aes(x=Date,y=New_recoveries))+
       geom_bar(aes(fill="blue"),position="stack", stat="identity")+
       xlab("Date")+
       ylab("Recovered Cases")+
       ggtitle("New Recovery Cases")+theme(
         legend.position = "none",
         plot.title = element_text(hjust="centre"),
         axis.text.x.bottom = element_text("red"),
         axis.text.y.left = element_text("red"),
         title = element_text("bold",color='red'),
         axis.line.x  = element_line('yellow'),
         axis.line.y = element_line("yellow"),
         axis.text.x = element_text("red"),
         axis.text.y = element_text("red"),
         panel.grid.major = element_line("pink"),
         panel.grid.minor = element_line("pink"),
         plot.background = element_rect("#1f1e1e"),
         panel.background = element_rect("#1f1e1e"),
         axis.title.x = element_text(color="orange"),
         axis.title.y = element_text(color = "orange")
       )
     ggplotly(p)
   })
   #Comparison Tab
   #Comparison Inputs Rendering
   output$country_comp_input=renderUI({
     country=unique(agg_data()$Country)
     pickerInput("comp_country", "Select Country:",   
                 choices = as.character(country),
                 options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                 selected = c("USA","Italy","Spain","India"),
                 multiple = TRUE)
   })

  
   output$comp_plot=renderPlotly({
     data=agg_data()%>%
       select("Date","Country","Confirmed","Deaths","Recoveries")
     d=data%>%
       filter(Country %in% input$comp_country)

    if(input$case=="Confirmed")
    {
      g=ggplot(d,aes(x=Date,y=Confirmed,group=Country))+
        geom_line(aes(color=Country))+theme(
          legend.position = "right",
          plot.title = element_text(hjust="centre"),
          axis.text.x.bottom = element_text("red"),
          axis.text.y.left = element_text("red"),
          title = element_text("bold",color='red'),
          axis.line.x  = element_line('yellow'),
          axis.line.y = element_line("yellow"),
          axis.text.x = element_text("red"),
          axis.text.y = element_text("red"),
          panel.grid.major = element_line("pink"),
          panel.grid.minor = element_line("pink"),
          plot.background = element_rect("#1f1e1e"),
          panel.background = element_rect("#1f1e1e"),
          axis.title.x = element_text(color="orange"),
          axis.title.y = element_text(color = "orange"))
    }
     else if(input$case=="Death")
     {
       g=ggplot(d,aes(x=Date,y=Deaths,group=Country))+
         geom_line(aes(color=Country))+theme(
           legend.position = "right",
           plot.title = element_text(hjust="centre"),
           axis.text.x.bottom = element_text("red"),
           axis.text.y.left = element_text("red"),
           title = element_text("bold",color='red'),
           axis.line.x  = element_line('yellow'),
           axis.line.y = element_line("yellow"),
           axis.text.x = element_text("red"),
           axis.text.y = element_text("red"),
           panel.grid.major = element_line("pink"),
           panel.grid.minor = element_line("pink"),
           plot.background = element_rect("#1f1e1e"),
           panel.background = element_rect("#1f1e1e"),
           axis.title.x = element_text(color="orange"),
           axis.title.y = element_text(color = "orange"))
       
     }
     else
     {
     g=ggplot(d,aes(x=Date,y=Recoveries,group=Country))+
       geom_line(aes(color=Country))+theme(
         legend.position = "right",
         plot.title = element_text(hjust="centre"),
         axis.text.x.bottom = element_text("red"),
         axis.text.y.left = element_text("red"),
         title = element_text("bold",color='red'),
         axis.line.x  = element_line('yellow'),
         axis.line.y = element_line("yellow"),
         axis.text.x = element_text("red"),
         axis.text.y = element_text("red"),
         panel.grid.major = element_line("pink"),
         panel.grid.minor = element_line("pink"),
         plot.background = element_rect("#1f1e1e"),
         panel.background = element_rect("#1f1e1e"),
         axis.title.x = element_text(color="orange"),
         axis.title.y = element_text(color = "orange"))}
     ggplotly(g)
   })
   
   
   #Data Tab
   
   output$datatable=renderDataTable({
     data=countrywise()
     datatable(data,
               rownames = FALSE,
               escape = FALSE,
               selection = "none",
               style = "bootstrap"
               
               
               )%>%
       formatStyle(
         columns = "Country",
         fontWeight = "bold",
         color = "red",
        # backgroundColor = "skyblue"
       )
     #return(data)
   })
   
   output$downloadcsv=  downloadHandler(
     filename = function() {
       paste("data-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(countrywise(), file)
     }
   )
   
})
