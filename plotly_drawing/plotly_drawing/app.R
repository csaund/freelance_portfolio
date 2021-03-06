library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(birk)

ui <- fluidPage(
  fluidRow(
  ),
  sidebarPanel(
    h3('Draw lines:'),
    selectInput("plot_line", "on plot:",
                c("Default" = 0,
                  "S" = 1,
                  "P" = 2,
                  "S10Y" = 3,
                  "P10Y" = 4),
                selected="Default"),
    selectInput("line_color", "Line Color:",
                c("Green" = "green",
                  "Red" = "red",
                  "Blue" = "blue",
                  "Black" = "black"),
                selected="Green"),
    h3('from these dates:'),
    h4('selected point:'),
    verbatimTextOutput("clicks"),
    actionButton("setP1", "Set P1"),
    verbatimTextOutput("click1"),
    actionButton("setP2", "Set P2"),
    verbatimTextOutput("click2"),
    actionButton("addLineClick", "Click to create a line with P1 and P2"),
    h4("Or create a line from these dates and values:"),
    numericInput("y0", label="y0", value=3, step=0.1),
    numericInput("y1", label="y1", value=3, step=0.1),
    dateRangeInput("lineDrawDateRange", "Date range input:", 
                   start="2018-05-19",
                   end="2019-05-17"),
    actionButton("addLine", "Add Line From Dates")
  ),
  mainPanel(
    fluidRow(
      column(12,
             #h3("Plot size settings"),
             #column(3, 
             #        numericInput("plot_height", label="plot height", value=200, step=10)
             #        ),
             #column(3, 
             #       numericInput("plot_width", label="plot width", value=600, step=10)
             #),
             column(12, 
                    dateRangeInput("inDateRange", "Plot Range:", 
                                   start="2018-05-16",
                                   end="2019-05-19")
             )
             #actionButton("set_plot_dimensions", "Set plot dimensions")
      )
    ),
    fluidRow(
      column(12,
             h3('Load lines into plot'),
             column(6, 
                    fileInput("inputLineFile", "Choose CSV File",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    )
             ),
             column(2, 
                    actionButton("up","Upload")
             ),
             column(4,
                    downloadButton("downloadData", "Download Line Data")
             )
      )
    ),
    plotlyOutput("combinedPlots", height="800px")
  )
)

server <- function(input, output, session) {
  # start by loading 
  load('w.ws.RData')
  
  # Input the data
  inputLines <- observeEvent(input$up, {
    values$shouldUpload <- TRUE
    inFile <- input$inputLineFile
    if (is.null(inFile))
      return(NULL)
    inputLineData <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    loadLines(inputLineData)
    return(inputLineData)
  })

  # Load lines from data input
  loadLines <- function(lineData){
    if(values$shouldUpload)
    for (row in 1:nrow(lineData)) {
      x0 <- lineData[row, "start"]
      x1  <- lineData[row, "end"]
      y0 <- lineData[row, "y0"]
      y1 <- lineData[row, "y1"]
      line_color <- lineData[row, "color"]
      p <- lineData[row, "plot"]
      add_lines(x0,x1,y0,y1,line_color,p)
    }
    values$shouldUpload=NULL
  }

  # Set all reactive values
  values <- reactiveValues(shouldUpload=NULL,   # So it only uploads once
                           j=0,              # value dervied from date input
                           k=365,              # value dervied from date input
                           clicks=NULL,         # to display the last click
                           click1=NULL,         # to display P1
                           y0=NULL,    # keep track to automatically set y value
                           y1=NULL,    # on point click
                           curve=NULL,
                           click2=NULL,         # to display P2
                           lastClick=NULL,      # to keep track of the last click
                           lines=list(),        # lines for default plot
                           slines=list(),       # lines for s
                           plines=list(),       # lines for p
                           s10lines=list(),     # lines for s10y
                           p10lines=list(),     # lines for p10y
                           plot_height=200,
                           plot_width=600,
                           line_data_table=data.frame(   # default line df
                             start=c(),
                             end=c(),
                             percent_diff=c(),
                             y0=c(),
                             y1=c(),
                             color=c(),
                             plot=c(),
                             stringsAsFactors = FALSE))
  
  # Printable output
  output$click1 <- renderPrint({
    values$click1
  })
  output$click2 <- renderPrint({
    values$click2
  })
  output$lastClick <- renderPrint({
    values$lastClick
  })
  
  values$clicks <- reactive({
    event_data('plotly_click')
  })

  p <- reactive({
    start <- which.closest(as.Date(w.ws$Date), as.Date(input$inDateRange[1]))
    finish <- which.closest(as.Date(w.ws$Date),  as.Date(input$inDateRange[2]))
    values$j <- start
    values$k <- finish
    
    dat <- w.ws %>% 
      filter(Date >= input$inDateRange[1] & Date < input$inDateRange[2])
    if(nrow(dat)==0) {
      return(w.ws)
    }
    else {
      return(dat)
    }
  })
  
  main_plot <- reactive({
    b <- plot_ly(p(), 
              x=~Date,
              type='candlestick',
              open=~Open,
              high=~High,
              low=~Low,
              close=~Settle) %>%
        layout(
          shapes = values$lines)
    return(b)
  })
  
  s <- reactive({
    p <- us10y.p$cwc$f.d5.w[values$j:values$k] %>%
      plot_ly(x=w.ws[values$j:values$k,Date],
              y=~us10y.f.d5.suw,
              type='scatter',
              mode='lines+markers') %>%
      layout(
        shapes=values$slines) 
    return(p)
  })
  fp <- reactive({
    p <- us10y.p$cwc$f.d5.w[values$j:values$k]  %>%
      plot_ly(x=w.ws[values$j:values$k,Date],
              y=~us10y.f.d5.prw,
              type='scatter',
              mode='lines+markers') %>%
      layout(
        shapes=values$plines)
    return(p)
  })
  s10 <- reactive({
    p <- us10y.p$cwc$f.d10.w[values$j:values$k]  %>%
      plot_ly(x=w.ws[values$j:values$k,Date],
              y=~us10y.f.d10.suw,
              type='scatter',
              mode='lines+markers') %>%
      layout(
        shapes=values$s10lines)
    return(p)
  })
  p10 <- reactive({
    p <- us10y.p$cwc$f.d10.w[values$j:values$k]  %>%
      plot_ly(x=w.ws[values$j:values$k,Date],
              y=~us10y.f.d10.prw,type='scatter',
              mode='lines+markers') %>%
      layout(
        shapes=values$p10lines)
    return(p)
  })
  
  
  output$combinedPlots <- renderPlotly({
    subplot(main_plot(), s(), fp(), s10(), p10(),
            nrows = 5, 
            margin =0.01, 
            heights = c(0.2, 0.2, 0.2, 0.2, 0.2),
            shareX = TRUE,
            shareY=FALSE,
            titleY=TRUE,
            which_layout = "merge")
  })
  
  # Observe events from input buttons
  output$clicks <- renderPrint({
    values$lastClick <- event_data("plotly_click")
    event_data("plotly_click")
  })
  
  observeEvent(input$set_plot_dimensions, {
    values$plot_height <- input$plot_height
    values$plot_width <- input$plot_width
  })
  
  observeEvent(input$addLine, {
    x0 = toString(input$lineDrawDateRange[1])
    x1 = toString(input$lineDrawDateRange[2])
    add_lines(x0, 
              x1,
              input$y0,
              input$y1,
              input$line_color,
              input$plot_line) 
  })
  
  observeEvent(input$setP1, {
    values$click1 <- values$lastClick
    if(values$click1[['curveNumber']] == 0) {
      res <- filter(w.ws, as.Date(Date) == as.Date(values$lastClick$x))
      values$y0 <- res$Settle
    }
    else {
      values$y0 = values$click1[['y']]
    }
    values$curve = values$click1[['curveNumber']]
  })
  
  observeEvent(input$setP2, {
    values$click2 <- values$lastClick
    if(values$click2[['curveNumber']] == 0) {
      res <- filter(w.ws, as.Date(Date) == as.Date(values$lastClick$x))
      values$y1 <- res$Settle
    }
    else {
      values$y1 = values$click2[['y']]
    }
    # TODO make sure this is the same.
    # validate(values$click1[['curveNumber']] == values$curve)
  })
  
  observeEvent(input$addLineClick, {
    if (is.null(values$click1[['x']])) {
      return(NULL)
    }
    add_lines(
      values$click1[['x']], 
      values$click2[['x']],
      values$y0,
      values$y1,
      input$line_color,
      values$curve)
    values$click1 <- NULL
    values$click2 <- NULL
    values$y0 <- NULL
    values$y1 <- NULL
    values$curve <- NULL
  })
  
  # Add line to reactive values
  add_lines <- function(xs, xe, ys, ye, col, plot_to_add) {
    li <- list(
      type='line',
      x0 = xs, 
      x1 = xe,
      y0 = ys,
      y1 = ye,
      xref='x',yref='y',
      line=list(color=col,width=0.5))
    if(plot_to_add==0) {
      i <- length(values$lines)+1
      values$lines[[i]] <- li      
    }
    else if(plot_to_add==1) {
      i <- length(values$slines)+1
      values$slines[[i]] <- li      
    }
    else if(plot_to_add==2) {
      i <- length(values$plines)+1
      values$plines[[i]] <- li      
    }
    else if(plot_to_add==3) {
      i <- length(values$s10lines)+1
      values$s10lines[[i]] <- li      
    }
    else if(plot_to_add==4) {
      i <- length(values$p10lines)+1
      values$p10lines[[i]] <- li      
    }
    add_line_to_dt(xs, xe, ys, ye, col, plot_to_add)
  }

  # Add lines to actual df
  add_line_to_dt <- function(xs, xe, ys, ye, line_color, plot_to_add) {
    new <- data.frame(xs, 
                      xe,
                      ye - ys,
                      ys,
                      ye,
                      line_color,
                      plot_to_add,
                      stringsAsFactors = FALSE)
    names(new) <- c('start', 'end', 'percent_diff', 'y0', 'y1', 'color', 'plot')
    values$line_data_table <- rbind(values$line_data_table, new)
  }
  
  # Download the line list. 
  output$downloadData <- downloadHandler(
    filename = "line_data.csv",
    content = function(file) {
      write.csv(values$line_data_table, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)