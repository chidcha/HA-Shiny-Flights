
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)

todat <- function(a,b,c) {as.Date(paste(a, b, c, sep = "-"))}
f <- flights
f$dep_date <- todat(flights$year,flights$month,flights$day)
setDT(f)
f[, arr_time := paste(substr(sprintf('%04d',f$arr_time), 1, 2),substr(sprintf('%04d',f$arr_time), 3, 4),sep = ':')]
f[, dep_time := paste(sprintf('%02d',f$hour),sprintf('%02d',f$minute),sep = ':')]


shinyServer(function(input, output) {
  
  fl <- reactive({
    f[dep_date >= input$date[1] 
      & dep_date <= input$date[2] 
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
    #ggplot(fl(), aes_string(input$grp, fl()$distance)) + geom_boxplot()
    ggplot(fl(), aes_string(input$xval, input$metric)) + geom_boxplot()
    #ggplot(fl(), aes(origin, distance)) + geom_boxplot()
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

