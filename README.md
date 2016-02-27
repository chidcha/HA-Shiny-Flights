# HA-Shiny-Flights
# https://xcabbage.shinyapps.io/Fligths/

############################   ui.R   ##############################

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(nycflights13)
library(data.table)


todat <- function(a,b,c) {paste(a,sprintf('%02d',b),sprintf('%02d',c),sep='/')}


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput('dist','max distance',
                    min = min(flights$distance),
                    max = max(flights$distance),
                    value = c(min(flights$distance),min(flights$distance)+300)
                  ),
      selectInput('grp', 'group by', 
                  choices = c("origin","dest","carrier")
                  ),
      
      selectInput('ddate1', 'dep date from', 
                   choices = sort(unique(todat(flights$year,flights$month,flights$day)))
                 ),
      selectInput('ddate2', 'dep date to', 
                   choices = sort(unique(todat(flights$year,flights$month,flights$day)))
                 )
      
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Some statistics', dataTableOutput('tab1')),
        tabPanel('Distance stat', plotOutput("dist")),
        tabPanel('Air time stat', plotOutput("airt")),
        tabPanel('Dealy stat', plotOutput("dela")),
        tabPanel('Selected rows', dataTableOutput('tab2'))
      )
    )
  )
))


############################   server.R   ##############################


# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)

todat <- function(a,b,c) {paste(a,sprintf('%02d',b),sprintf('%02d',c),sep='/')}
f <- flights
f$dep_date <- todat(flights$year,flights$month,flights$day)
setDT(f)
f[, arr_time := paste(substr(sprintf('%04d',f$arr_time), 1, 2),substr(sprintf('%04d',f$arr_time), 3, 4),sep = ':')]
f[, dep_time := paste(sprintf('%02d',f$hour),sprintf('%02d',f$minute),sep = ':')]


shinyServer(function(input, output) {
  
  fl <- reactive({
    f[dep_date >= input$ddate1 
      & dep_date <= input$ddate2 
      & distance >= input$dist[1]
      & distance <= input$dist[2],
      .(dep_date,
        dep_time,
        dep_delay,
        arr_time,
        arr_delay,
        carrier,
        tailnum,
        flight,
        origin,
        dest,
        air_time,
        distance
        )]
  }) 
  
  output$dist <- renderPlot({
    #ggplot(fl(), aes(fl()[,input$grp], distance)) + geom_boxplot()
    ggplot(fl(), aes(origin, distance)) + geom_boxplot()
  })
  output$airt <- renderPlot({
    ggplot(fl(), aes(origin, air_time)) + geom_boxplot()
  })
  output$dela <- renderPlot({
    ggplot(fl(), aes(origin, arr_delay)) + geom_boxplot()
  })
  
  output$tab1 <- renderDataTable({
    fl()[, .("flights no" = .N,
             "avg distance" = mean(distance, na.rm = TRUE),
             "avg air time" = mean(air_time, na.rm = TRUE),
             "avg delay" = mean(arr_delay, na.rm = TRUE),
             "max delay" = max(arr_delay, na.rm = TRUE)
             ), by = eval(input$grp)]
  })
  
  
  output$tab2 <- renderDataTable({
    fl()[,]
  })  

    
})

