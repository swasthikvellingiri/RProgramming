################
##   SHINY    ##
################

## Group Assignment - Open Source Programming 2021

## By - Medina Martinez Juan Jose, KELLY Elina, VELLINGIRI KOWSALYA Swasthik


# load package
#library(OpenStreetMap)
library(DT)
library(RColorBrewer)
library(mapproj)
library(sf)
library(RgoogleMaps)
library(scales)
library(rworldmap)
library(maps)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)
library(maptools)
library(leaflet)
library(sf)
library(tmap)
library(here)
library(rgdal)
library(scales)
library(flextable)
library(plyr)
library(shiny)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)


#load database

load ("Base_Table_R.RData")


# Define UI for application that draws a histogram

ui <- navbarPage("Marketing Data Mart",
                 
                #Table Overview
                 tabPanel("Overview",
                          sidebarLayout(
                            sidebarPanel(
                              navlistPanel(
                                conditionalPanel(condition ="input.tabsover==3",
                                                 radioButtons("OVER_TOTAL_AVG", "Select Type:",
                                                              choices = c("Total" = "Total",
                                                                          "Average" = "Avg")),
                                                 radioButtons("OVER_STAKES_WIN_BET", "Select Variable:",
                                                              choices = c("Stakes" = "Stakes",
                                                                          "Winnings" = "Winnings",
                                                                          "Bets" = "Bets")),
                                                 checkboxGroupInput("DAYZ", "Select the days", choices = factor(unique(weekdays1_$day),level = c('Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday'))
                                                                    )
                                                 ),
                                conditionalPanel(condition ="input.tabsover==4",
                                                 checkboxGroupInput("PROD", "Select Products", choices = unique(Pivot_product$rowname))),
                                conditionalPanel(condition ="input.tabsover==1",
                                                 checkboxGroupInput("LANG", "Select Languages", choices = unique(df$LanguageName)))
                                          )
                                         ),
                            mainPanel(
                              tabsetPanel(type= "tabs", id="tabsover",
                                          tabPanel("Languages", value =1,
                                                   plotOutput("languages")
                                          ),
                                          tabPanel("Type Distribution", value= 2,
                                                   plotOutput("Pie") 
                                          ),
                                          tabPanel("Daily Statistics", value=3,
                                                   plotOutput("typeday") 
                                          ),
                                          tabPanel("Product Popularity",value=4,
                                                   plotOutput("Product")
                                          )
                              )
                            )
                          )
                        ),
                 tabPanel("Types",
                          sidebarLayout(
                            sidebarPanel(
                              navlistPanel(
                                selectInput("GAMES_TYPES","Select type of games",
                                            c("All" = "AllPlayer",
                                              "Poker" = "PokerPlayer",
                                              "Games" = "GamesPlayer",
                                              "Casino"= "CasinoPlayer")
                                )),
                              conditionalPanel(condition ="input.tabselected==1|input.tabselected==2",
                                               radioButtons("TOTAL_AVG", "Select type:",
                                                            choices = c("Total" = "Total",
                                                                        "Average" = "Avg"))),
                              conditionalPanel(condition ="input.tabselected==1",
                                               radioButtons("STAKES_WIN", "Select Variable:",
                                                            choices = c("Stakes" = "Stakes",
                                                                        "Winnings" = "winnings"))),
                              conditionalPanel(condition ="input.tabselected==2",
                                               radioButtons("STAKES_WIN_BET", "Select Variable:",
                                                            choices = c("Stakes" = "Stakes",
                                                                        "Winnings" = "winnings",
                                                                        "Bets" = "Bets")
                                               ),
                                               sliderInput("N_BINS","Number of Bins",min= 1, max= 30, value = 20)),
                              conditionalPanel(condition ="input.tabselected==4",
                                               checkboxGroupInput("APPS", "Applications", choices = unique(df$App_name)))
                            ),
                            mainPanel(
                              tabsetPanel(type= "tabs", id= "tabselected",
                                          tabPanel("Scatter View", value= 1,
                                                   plotOutput("total_vs_B")
                                          ),
                                          tabPanel("Histogram View",value=2,
                                                   plotOutput("total_vs_B_Hist")
                                          ),
                                          tabPanel("Map", value=3,
                                                   plotOutput("Map")     
                                          ),
                                          tabPanel("Application Popularity", value=4,
                                                   plotOutput("Weekday")
                                          )
                                       )
                                    )
                                )
                            ),
                 tabPanel("Poker",
                          sidebarLayout(
                            sidebarPanel(
                              conditionalPanel(condition ="input.pokerselected==1",
                                               sliderInput("N_BINS2","Number of Bins",min= 1, max= 30, value = 20),
                                               selectInput("BUYSELL" ,"Buy or Sell",
                                                           c("Buy" = "AVG_Total_N_Buy",
                                                             "Sell" = "AVG_Total_N_Sell" ))
                              ),
                              conditionalPanel(condition ="input.pokerselected==2",
                                               checkboxGroupInput("COUNTRY", "Select Countries", choices = unique(Best_Country_Revenue1$CountryName)))
                              
                              ),
                                 mainPanel(
                                    tabsetPanel(type= "tabs", id="pokerselected",
                                          tabPanel("Buy-Sell Tokens",value=1,
                                                   plotOutput("Buy_Sell")),
                                          tabPanel("Country Profitability",value=2,
                                                   plotOutput("Revenue_Poker")
                                          )
                                       )
                                    )
                                )
                            )
                         )




# Define server logic required to draw a histogram
server <- function(input, output) {
  

  output$languages <- renderPlot({
    
    CountLanguages <- count(df, "LanguageName")
    CountLanguages <- arrange(CountLanguages, freq)
    CountLanguages$LanguageName <- factor(CountLanguages$LanguageName, levels = CountLanguages$LanguageName)
    CountLanguages%>%
      filter(LanguageName %in% input$LANG)%>%
      ggplot(aes(LanguageName, freq, fill = LanguageName)) + geom_col() + coord_flip() +
      xlab("Number of Users")
  })
  
## Players Based on Game type

    output$Pie <- renderPlot({
    AllCasinoPlayers <- sum(df$CasinoPlayer == 1)
    AllGamesPlayers <- sum(df$GamesPlayer == 1)
    AllPokerPlayers <- sum(df$PokerPlayer == 1)
    
    # Create data for the graph.
    x <- c(AllCasinoPlayers, AllGamesPlayers, AllPokerPlayers)
    labels <- c("Casino Players", "Games Players", "Poker Players")
    
    # Plot the chart.
    piepercent<- round(100*x/sum(x), 1)
    pie(x, labels = piepercent, explode = 0.1, main = "Types of players in %",col = rainbow(length(x)))
    legend("right", c("Casino Players","Games Players","Poker Players"), cex = 1,
           fill = rainbow(length(x)))
    
  })

## Total Stakes and Total winnings distrubution based on the days of the week
    
  output$typeday <- renderPlot({
    merge_variable2 <- paste(input$OVER_TOTAL_AVG,input$OVER_STAKES_WIN_BET,sep="_")  #merge variable used to concat the 2 input selections
    weekdays1_%>%
      filter(day %in% input$DAYZ)%>%
      ggplot(aes(x=factor(day, level = c('Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday')), y=.data[[merge_variable2]],fill=day))+
      geom_bar(stat="identity")+
      scale_fill_hue(c = 40) +
      theme(legend.position="none") +
      scale_y_continuous(labels = scales::comma)+
      theme(axis.text.x = element_text(angle = 90))+
      ylab( merge_variable2)+   #see if this works (It did!!)
      xlab("Day of the week")
   
  })
  
  
  output$Product <- renderPlot({
    Pivot_product%>%
      filter(rowname %in% input$PROD)%>%
      ggplot(aes(x=rowname, y=V1))+
      geom_bar(stat="identity")+
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position="none")+
      scale_y_continuous(labels = scales::comma)+
      theme(axis.text.x = element_text(angle = 90))+
      ylab("Number of Bets")+
      xlab("Products")
    
  })

  ## Total and Average Stakes and winnings distribution based on the products
  
  data <- reactive({ df%>%
      filter(.data[[input$GAMES_TYPES]]==1)  #Use .datap[[var]] to use it as a column input
  })
 
  output$total_vs_B <- renderPlot({
    merge_variable <- paste(input$TOTAL_AVG,input$STAKES_WIN,sep="_")
    ggplot(data(),aes(x=Total_Bets,y=.data[[merge_variable]],color=Gender))+   #use data() to call a reactive datase inside outputs
      geom_point()
    
  })
  
  output$total_vs_B_Hist <- renderPlot({
    merge_variable <- paste(input$TOTAL_AVG,input$STAKES_WIN_BET,sep="_")
    ggplot(data(),aes(x=.data[[merge_variable]])) +
      geom_histogram( bins=input$N_BINS, fill="#69b3a2", color="#e9ecef", alpha=0.9) 
    
    
  })
  
#Map for the players
  
  output$Map <- renderPlot({
    options(stringsAsFactors = F)         # no automatic data transformation
    options("scipen" = 100, "digits" = 4) # suppress math annotation
    op <- options(gvis.plot.tag='chart')  # set gViz options
    CountCountries <- count(data(), "CountryName")
    CountCountries <- CountCountries[order(-CountCountries$freq),]
    
    # create data frame with country names and frequencies
    countriesvisited <- data.frame(CountCountries$CountryName,CountCountries$freq)
    
    # combine data frame with map
    visitedMap <- joinCountryData2Map(countriesvisited, 
                                      joinCode = "NAME",
                                      nameJoinColumn = "CountCountries.CountryName")
    
    # def. map parameters, e.g. def. colors
    # https://stackoverflow.com/questions/56101927/flip-color-range-of-heatmap-in-base-r
    mapParams <- mapCountryData(visitedMap, 
                                nameColumnToPlot="CountCountries.freq",
                                oceanCol = "azure2",
                                catMethod = "categorical",
                                missingCountryCol = gray(.8),
                                colourPalette = rev(heat.colors(41)),
                                addLegend = F,
                                mapTitle = "Where the customers come from",
                                border = NA)
    # add legend and display map
    
    # Plot a corresponding legend
    # https://stackoverflow.com/questions/57395558/how-to-show-legend-in-heatmap
    legend(title="nbr of customers", x="left", legend=c("low", "med", "high"),horiz = FALSE,fill=rev(heat.colors(3)),bg = "transparent")
    
    
    
  })

#Application based on the number of users
  
  output$Weekday <- renderPlot({
    Pivot_app_names <- data() %>%
      group_by(App_name) %>%
      dplyr::summarise(number = n())   
    
    Pivot_app_names%>%
      filter(App_name %in% input$APPS)%>%
      ggplot(aes(x=App_name,y=number))+
      geom_bar(stat="identity")+
      scale_fill_hue(c = 40) +
      theme(legend.position="none") +
      scale_y_continuous(labels = scales::comma)+
      theme(axis.text.x = element_text(angle = 90))+
      ylab("Number of Users")+
      xlab("Application")
    
    
    
  })
  
# BUy and Sell token Information for Poker
  
  output$Buy_Sell <- renderPlot({
    Poker <- df%>%
      filter(PokerPlayer == 1)
    Poker$Avg_Lag_Buy2 <- as.POSIXct(strptime(Poker$Avg_Lag_Buy, "%H:%M:%S"))
    ggplot(Poker,aes(x=.data[[input$BUYSELL]]))+
      geom_histogram(bins = input$N_BINS2 ,alpha= 0.5)
    
    
    
  })

#Poker revenue per country
  
  output$Revenue_Poker <- renderPlot({
    Best_Country_Revenue1%>%
      dplyr::filter(CountryName %in% input$COUNTRY)%>%
      ggplot(aes(x=CountryName,y=Country_Profit_Comp))+
      geom_bar(stat="identity")+
      scale_fill_hue(c = 40) +
      theme(legend.position="none") +
      scale_y_continuous(labels = scales::comma)+
      theme(axis.text.x = element_text(angle = 90))+
      ylab("Companies Profit")+
      xlab("Top Countries")
  })

}

# Run the application 

shinyApp(ui = ui, server = server)
