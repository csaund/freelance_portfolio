library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(
    h3('Load lines into plot'),
    fileInput("inputLineFile", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    actionButton("up","Upload"),
    br(),
    h3('Draw lines:'),
    selectInput("plot_line", "on plot:",
                c("Default" = "default",
                  "S" = "s",
                  "P" = "p",
                  "S10Y" = "s10y",
                  "P10Y" = "p10y"),
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
                   end="2019-05-19"),
    actionButton("addLine", "Add Line From Dates"),
    h3("Plot size settings"),
    numericInput("plot_height", label="plot height", value=200, step=10),
    numericInput("plot_width", label="plot width", value=400, step=10),
    numericInput("j", label="j", value=2600, step=10),
    numericInput("k", label="k", value=2700, step=10),
    actionButton("set_plot_dimensions", "Set plot dimensions"),
    dateRangeInput("inDateRange", "Plot Range:", 
                   start="2018-05-19",
                   end="2019-05-19")
  ),
  mainPanel(
    downloadButton("downloadData", "Download Line Data"),
    plotlyOutput("p", height="auto"),
    # DT::dataTableOutput("p_line_table"),
    plotlyOutput("figs", height="auto"),
    plotlyOutput("figp", height="auto"),
    plotlyOutput("figs10", height="auto"),
    plotlyOutput("figp10", height="auto"),
    plotlyOutput("testPlot", height="auto")
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
                           j=NULL,              # value dervied from date input
                           k=NULL,              # value dervied from date input
                           clicks=NULL,         # to display the last click
                           click1=NULL,         # to display P1
                           y0=NULL,    # keep track to automatically set y value
                           y1=NULL,    # on point click
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
  
  # Table and plot outputs
  output$p_line_table <- DT::renderDataTable({
    if(nrow(values$line_data_table)==0) {
      return(NULL)
    }
    values$line_data_table %>%
      select('start', 'end', 'percent_diff')
  })
  
  j=2600
  k=2700
  
  p <- reactive({
    print(input$inDateRange[1])
    print(input$inDateRange[2])
    values$j=input$j
    values$k=input$k
    t <- tail(w.ws, length(us10y.p$cwc$f.d5.w$us10y.f.d5.suw))   # I guess assume 
                                                                 # that this is the latest 
                                                                 # data from w.ws?
    start <- which(as.Date(t$Date) == as.Date(input$inDateRange[1]))
    finish <- which(as.Date(t$Date) == as.Date(input$inDateRange[2]))
    print(start)
    print(finish)
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
  
  output$p <- renderPlotly({
    plot_ly(p(), 
            x=~Date,
            type='candlestick',
            open=~Open,
            high=~High,
            low=~Low,
            close=~Settle,
            width = values$plot_width, height = values$plot_height) %>%
      layout(
        shapes = values$lines) %>%
      config(editable = TRUE)
  })

  output$figs <- renderPlotly({
    us10y.p$cwc$f.d5.w[values$j:values$k] %>%
      plot_ly(x=w.ws[values$j:values$k,Date],
            y=~us10y.f.d5.suw,
            type='scatter',
            mode='lines+markers',
            width = values$plot_width, height = values$plot_height) %>%
      layout(
        shapes=values$slines) %>%
      config(editable=TRUE)
  })
  output$figp <- renderPlotly({
    us10y.p$cwc$f.d5.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d5.prw,
            type='scatter',
            mode='lines+markers',
            width = values$plot_width, height = values$plot_height) %>%
      layout(
        shapes=values$plines) %>%
      config(editable=TRUE)
  })
  output$figs10 <- renderPlotly({
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.suw,
            type='scatter',
            mode='lines+markers',
            width = values$plot_width, height = values$plot_height) %>%
      layout(
        shapes=values$s10lines) %>%
      config(editable=TRUE)
  })
  output$figp10 <- renderPlotly({
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.prw,type='scatter',
            mode='lines+markers',
            width = values$plot_width, height = values$plot_height) %>%
      layout(
        shapes=values$p10lines) %>%
      config(editable=TRUE)
  })
  
  t <- reactive({
    p <- us10y.p$cwc$f.d5.w[values$j:values$k] %>%
      plot_ly(x=w.ws[values$j:values$k,Date],
              y=~us10y.f.d5.suw,
              type='scatter',
              mode='lines+markers') %>%
      layout(
        shapes=values$slines) 
    return(p)
  })
  
  output$testPlot <- renderPlotly({
    subplot(t(), t(), t(), t(), plot_ly(), 
            nrows = 4, 
            margin = 0.0, 
            heights = c(0.25, 0.25, 0.25, 0.25),
            shareX = TRUE,
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
    res <- filter(w.ws, as.Date(Date) == as.Date(values$lastClick$x))
    values$y0 <- res$Settle
    values$click1 <- values$lastClick
  })
  
  observeEvent(input$setP2, {
    res <- filter(w.ws, as.Date(Date) == as.Date(values$lastClick$x))
    values$y1 <- res$Settle
    values$click2 <- values$lastClick
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
      input$plot_line)
    values$click1 <- NULL
    values$click2 <- NULL
    values$y0 <- NULL
    values$y1 <- NULL
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
    if(plot_to_add=='default') {
      i <- length(values$lines)+1
      values$lines[[i]] <- li      
    }
    else if(plot_to_add=='s') {
      i <- length(values$slines)+1
      values$slines[[i]] <- li      
    }
    else if(plot_to_add=='p') {
      i <- length(values$plines)+1
      values$plines[[i]] <- li      
    }
    else if(plot_to_add=='s10y') {
      i <- length(values$s10lines)+1
      values$s10lines[[i]] <- li      
    }
    else if(plot_to_add=='p10y') {
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