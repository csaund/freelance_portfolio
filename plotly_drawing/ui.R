#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("US10y Open"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("inDateRange", "Date range input:", 
                     start="2018-05-19",
                     end="2019-05-19"),
      textOutput("x"),
      textOutput("y"),
      textOutput("hovered")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       #plotOutput("distPlot"),
       plotlyOutput("fig"),
       plotlyOutput("figs"),
       plotlyOutput("figp"),
       plotlyOutput("figs10"),
       plotlyOutput("figp10")
    )
  )
))
