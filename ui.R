#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
#if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
#if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
#if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library(wordcloud)
library(gganimate)
library(shinycssloaders)
library(DT)
library(htmltools)

#options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


#ui
shinyUI(bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  
  navbarPage(theme = shinytheme("darkly"),
             collapsible = TRUE,
             "COVID-19 Global", id="nav",
             
            
             tabPanel("Overview",icon = icon("globe"),
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          tags$link(rel="shortcut icon",type="image/png",href="corona.png"),
                          
                          #tags$head(includeCSS("style2.css")),
                          withSpinner(leafletOutput("mymap",width = "100%",height="650px")),
                           
                          absolutePanel(id="controls",class = "panel panel-default",
                                    top = 60, left = 20, width = 350, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                                    span( h2(textOutput("conf"), align = "right"),style="color:#ff0000"),
                                    span(h4(textOutput("deaths"),align = "right"),style="color:#e56d02"),
                                    span(h4(textOutput("recov"), align = "right"), style="color:#006d2c"),
                                    span(h4(textOutput("active"), align = "right"), style="color:#cc4c02"),
                                    span(h6(textOutput("country"),align="right"),style="color:#006d3c"),
                                    span(h6("Live"),textOutput("currenttime"),class="blinking"),
                                    withSpinner(plotOutput("daily_conf", height="130px", width="100%"),type = 7),
                                    plotOutput("stacked", height="130px", width="100%"),
                                    uiOutput("date_upto")
                                    
                                    
                                    
                                    
                                    ),
                      
                     
                        
                    
                                   
                      
                      )
                      ),
             tabPanel("visualizations",icon = icon("bar-chart-o"),
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("case_select", "cases:",   
                                      choices = c("Confirmed", "Deaths", "Recovery"), 
                                      selected = c("Confirmed"),
                                      multiple = FALSE),
                          uiOutput("date_input"),
                          conditionalPanel(condition="input.tabvals==3",
                                           h4(textOutput("global_mort")),br(),h5("Initial estimate was 2%")
                                           ,p("Initially, the World Health Organization (WHO) had mentioned 2% as a mortality rate estimate in a press conference on Wednesday, January 29 and again on February 10."),br(),
                                           h5("3.4% Mortality Rate estimate by the World Health Organization (WHO) as of March 3.")
                                           ),
                          conditionalPanel(condition="input.tabvals==4",
                                           h4(textOutput("global_recov") ),
                                           
                                             ),
                          span(h2(tags$img(src="masked2.jpg",width="375px",height="250px")),align="centre")
                     
                          
                          
                          
                        ),
                        mainPanel(
                          column(12,tabBox(id="tabvals",width = NULL,
                            tabPanel("Cumulative Plot",
                                    withSpinner( plotlyOutput("cumu",height = "400px",width = "100%")),
                                     span(h4(checkboxInput("log","logarithmic Scale"
                                     )),align="right"),
                                     value = 1
                                     ),
                            tabPanel("Daily Cases",
                                    withSpinner( plotlyOutput("daily",height = "400px",width = "100%")),
                                    value = 2
                                     ),
                            tabPanel("Mortality Rate",
                                    withSpinner( plotlyOutput("mort",height = "400px",width = "100%")),
                                    value = 3
                                     ),
                            tabPanel("Recovery Rate",
                                    withSpinner( plotlyOutput("recov_rate",height= "400px",width="100%")),value = "4"
                                     )
                          ))
                        )
                      )
                  ),
             tabPanel("case studies",icon = icon("flag"),
                      #includeCSS("style2.css"),
                     #setBackgroundColor("black",gradient="linear",direction = "top"),
                     sidebarLayout(
                       sidebarPanel(
                         uiOutput("Countries"),
                         uiOutput("plotting_date"),
                        # actionButton("go","Show Plots"),
                         #h4("Confirmed: "),
                         #tags$span(textOutput("c_conf"),style="color:#f91907"),
                         span(h4(textOutput("c_conf")),style="color:#e21200"),
                         #span(h3(textOutput("c_conf_new")),style="color:#f91907"),
                         span(h4(textOutput("c_death")),style="color:#e56d02"),
                         span(h4(textOutput("c_recov")),style="color:#2ce313"),
                         span(h4(textOutput("c_mort")),style="color:#07e2b4"),
                         span(h4(textOutput("c_recovr")),style="color:#e5f305"),
                         span(h4(textOutput("population")),style="color:#6600ff"),
                         span(h5(textOutput("density")),style="color:#ff6600"),
                         span(h4(textOutput("temp")),style="color:#0066ff"),
                         
                         
                         leafletOutput("map",width = "100%",height = "200px")
                       ),
                       mainPanel(fluidRow(
                         column(4,h3("New Cases: "),
                               span( h4(textOutput("c_conf_new")),class="case")
                           
                         ),
                         column(4,h3("New Deaths: "),
                                span( h4(textOutput("c_death_new")),class="death")
                                
                         ),
                         column(4,h3("New Recoveries: "),
                                span( h4(textOutput("c_recov_new")),class="recov")
                                
                         )
                       ),tags$br(),
                        
                        withSpinner(plotlyOutput("case_cumu_plot",height = "400px",width = "100%")),
                       tags$br(),
                        fluidRow(column(4,
                        plotlyOutput("case_daily_plot_conf",height = "300px",width = "100%")),
                        column(4,
                        plotlyOutput("case_daily_plot_death",height = "300px",width = "100%")),
                        column(4,
                        plotlyOutput("case_daily_plot_recov",height = "300px",width = "100%")))
                        
                        
                        
                         
                       )
                     )
                      
                      ),
             tabPanel("Comparison",icon=icon("not-equal"),
                      fluidRow(
                        column(4,
                               
                               selectInput("case","Select Case",
                                           choices = c("Confirmed","Death","Recovery"),
                                           selected = "Confirmed"),
                               uiOutput("country_comp_input"),
                               ),
                        column(8,tags$br(),
                               withSpinner(plotlyOutput("comp_plot"),type = 5)
                               )
                      )
               
             ),
           
             tabPanel("Data",icon = icon("database"),

                      fluidPage(#theme=shinytheme("flatly"),
                                dataTableOutput("datatable"),
                                downloadButton("downloadcsv","Download Data")
                                
                        
                      )
                      ),
             tabPanel("About",icon = icon("user"),
                      fluidRow(column(width=8,
                        tags$div(
                        span(h4("Last Update"),style="color:#3366ff"),
                        paste(Sys.time()),tags$br(),
                        span(tags$h4("Background: "),style="color:#3366ff"), 
                        "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders.Till mid-February it was mainly been concentrated on china,but after that it became a worldwide threat. 
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                        tags$br(),tags$br(),
                        "In isolation, these headlines can be hard to interpret.How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics? This site is updated daily based on data published by Johns Hopkins University. By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                        
                        span(tags$h4("Data Sources:"),style="color:#3366ff"),
                        "Live data is collected from ",tags$a(href="https://www.worldometers.info/coronavirus/","Worldmeter.info"),tags$br(),
                        "Data for further visualizations has been collected from Johns Hopkins University's official ",tags$a(href="https://github.com/CSSEGISandData/COVID-19"," Github"),"page.",tags$br(),
                        "Johns Hopkins University's live dashboad is available ",tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6","here(Desktop)."),tags$br(),
                        "Source code of this site is available ",tags$a(href="https://github.com/sahasuman343/Covid-19-live-dashboard","here"),tags$br(),
                        "Who live dashboard link : ",tags$a(href="https://covid19.who.int/","who.int"),tags$br(),
                        "Follow ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public","WHO"),"guidlines.",tags$br(),
                               span("STAY SAFE ,STAY HEALTHY!!",class="last")
                        
                        
                      )
                               
                                      
                                      
                                      ),
                               column(width=4,tags$b("Contacts:"),tags$br(),tags$br(),
                                      tags$img(src="profile.ico",class="center"),tags$br(),
                                      span(h4("Suman Saha"),style="color:#3366ff"),
                                      span(h4("University of Hyderabad,India"),style="color:#3366ff"),
                                      tags$a(class="fa fa-google","    Email: sahasuman343@gmail.com"),tags$br(),
                                      tags$a(class="fa fa-linkedin-square",href="https://www.linkedin.com/in/suman-saha-309799107/","    LinkedIn"),tags$br(),
                                      tags$a(class="fa fa-github",href="https://github.com/sahasuman343","    Github"),tags$br(),
                                      tags$a(class="fa fa-whatsapp","    Whatsapp no. 9647265601")
                                      
                                      )
                        
                      )
                      )

)
  
))