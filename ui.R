
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(nycflights13)
library(data.table)


#todat <- function(a,b,c) {paste(a,sprintf('%02d',b),sprintf('%02d',c),sep='-')}
todat <- function(a,b,c) {as.Date(paste(a, b, c, sep = "-"))}

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
      
      dateRangeInput("date",
                     "Date",
                     min = min(flights$date),
                     max = max(flights$date),
                     start = min(unique(todat(flights$year,flights$month,flights$day))),
                     end = min(unique(todat(flights$year,flights$month,flights$day))) + 1
                     )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Some statistics', dataTableOutput('tab1')),
        tabPanel('Distance stat', plotOutput("dist"),
                 selectInput('metric', 'Metric', 
                             choices = c("air_time","distance","arr_delay")
                 ),
                 selectInput('xval', 'Dimension', 
                             choices = c("origin","dest","carrier")
                 )
                 ),
        tabPanel('Selected rows', dataTableOutput('tab2'))
        )
    )
  )
))

