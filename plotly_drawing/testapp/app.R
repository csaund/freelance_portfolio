library(shiny)
library(DT)
library(dplyr)

ui <- fluidPage(
  sidebarPanel(
    dateRangeInput("inDateRange", "Plot Range:", 
                   start="2018-05-19",
                   end="2019-05-19"),
    h3('Load lines into plot'),
    fileInput("inputLineFile", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    actionButton("up","Upload"),
    actionButton("setP1", "Set P1"),
    verbatimTextOutput("click1"),
    actionButton("setP2", "Set P2"),
    verbatimTextOutput("click2"),
    verbatimTextOutput("clicks"),
    numericInput("y0_click", label="y0", value=3),
    numericInput("y1_click", label="y1", value=3),
    actionButton("addLineClick", "Click to create a line with P1 and P2"),
    # verbatimTextOutput("info"),
    h3("manual line entry:"),
    selectInput("line_color", "Line Color:",
                c("Green" = "green",
                  "Red" = "red",
                  "Blue" = "blue",
                  "Black" = "black"),
                selected="Green"),
    dateRangeInput("lineDrawDateRange", "Date range input:", 
                   start="2018-05-19",
                   end="2019-05-19"),
    numericInput("y0", label="y0", value=3),
    numericInput("y1", label="y1", value=3),
    actionButton("addLine", "Add Line Manually"),
    selectInput("plot_line", "Plot to draw on",
                c("Default" = "default",
                  "S" = "s",
                  "P" = "p",
                  "S10Y" = "s10y",
                  "P10Y" = "p10y"),
                selected="Default")
    # actionButton("addLineDrag", "Add line by clicking")
  ),
  mainPanel(
    tableOutput("lines"),
    plotlyOutput("p"),
    DT::dataTableOutput("p_line_table"),
    downloadButton("downloadData", "Download Line Data"),
    plotlyOutput("figs"),
    plotlyOutput("figp"),
    plotlyOutput("figs10"),
    plotlyOutput("figp10")
  )
)

server <- function(input, output, session) {
  load('w.ws.RData')
  
  inputLines <- observeEvent(input$up, {
    print('input lines')
    values$shouldUpload <- TRUE
    print(input$inputLineFile)
    inFile <- input$inputLineFile
    print(class(inFile))
    if (is.null(inFile))
      return(NULL)
    inputLineData <- read.csv(inFile$datapath)
    loadLines(inputLineData)
    return(inputLineData)
  })
  # Load Lines
  
  #output$lines <- renderTable({
  #  print(input$inputLineFile)
  #  inFile <- input$inputLineFile
  #  print(class(inFile))
  #  if (is.null(inFile))
  #    return(NULL)
  #  inputLineData <- read.csv(inFile$datapath)
  #  loadLines(inputLineData)
  #  return(inputLineData)
  #})
  
  loadLines <- function(lineData){
    if(values$shouldUpload)
    print('did stuff about lines here?')
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

  values <- reactiveValues(shouldUpload=NULL, 
                           clicks=NULL,
                           click1=NULL,
                           click2=NULL,
                           lastClick=NULL,
                           listenForClick=FALSE,
                           lines=list(), 
                           slines=list(),
                           plines=list(),
                           s10lines=list(),
                           p10lines=list(),
                           line_data_table=data.frame(
                             start=c(),
                             end=c(),
                             percent_diff=c(),
                             y0=c(),
                             y1=c(),
                             color=c(),
                             plot=c()))
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
  
  output$p_line_table <- DT::renderDataTable({
    line_data_table() %>%
      select('start', 'end', 'percent_diff')
  })
  
  line_data_table <- reactive({
    return(values$line_data_table)
  })
  
  lines <- reactive({
    return(values$lines)
  })
  slines <- reactive({
    return(values$slines)
  })

  reactiveMaster <- reactive({
    dat <- w.ws %>% 
      filter(Date >= input$inDateRange[1] & Date < input$inDateRange[2])
    return(dat)
  })
  
  p <- reactive({
    reactiveMaster()
  })
  
  output$p <- renderPlotly({
    plot_ly(p(), 
            x=~Date,
            type='candlestick',
            open=~Open,
            high=~High,
            low=~Low,
            close=~Settle) %>%
      layout(
        shapes = lines()) %>%
      config(editable = TRUE)
  })
  # TODO make this inputable
  j=2600
  k=2700
  output$figs <- renderPlotly({
    us10y.p$cwc$f.d5.w[j:k] %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d5.suw,
            type='scatter',
            mode='lines+markers') %>%
      layout(
        shapes=slines()) %>%
      config(editable=TRUE)
  })
  output$figp <- renderPlotly({
    us10y.p$cwc$f.d5.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d5.prw,
            type='scatter',
            mode='lines+markers')
  })
  output$figs10 <- renderPlotly({
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.suw,
            type='scatter',
            mode='lines+markers')
  })
  output$figp10 <- renderPlotly({
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.prw,type='scatter',
            mode='lines+markers')
  })
  
  output$info <- renderPrint({
    event_data("plotly_relayout")
  })
  output$clicks <- renderPrint({
    values$lastClick <- event_data("plotly_click")
    event_data("plotly_click")
  })
  
  observeEvent(input$addLine, {
    add_lines(input$lineDrawDateRange[1], 
              input$lineDrawDateRange[2],
              input$y0,
              input$y1,
              input$line_color,
              input$plot_line) 
  })
  
  observeEvent(input$setP1, {
    values$click1 <- values$lastClick
  })
  
  observeEvent(input$setP2, {
    values$click2 <- values$lastClick
  })
  
  observeEvent(input$addLineClick, {
    i <- length(values$lines)+1
    add_lines(
      values$click1[['x']], 
      values$click2[['x']],
      input$y0_click,
      input$y1_click,
      input$line_color,
      input$plot_line)
    values$click1 <- NULL
    values$click2 <- NULL
  })
  
  add_lines <- function(xs, xe, ys, ye, col, plot_to_add) {
    i <- length(values$lines)+1
    values$lines[[i]] <- list(
      type='line',
      x0 = xs, 
      x1 = xe,
      y0 = ys,
      y1 = ye,
      xref='x',yref='y',
      line=list(color=col,width=0.5))
    add_line_to_dt(xs, xe, ys, ye, col, plot_to_add)
  }

  add_line_to_dt <- function(xs, xe, ys, ye, line_color, plot_to_add) {
    print(xs)
    print(xe)
    print(ys)
    print(ye)
    print(line_color)
    print(plot_to_add)
    new <- data.frame(xs, 
                      xe,
                      ye - ys,
                      ys,
                      ye,
                      line_color,
                      plot_to_add)
    names(new) <- c('start', 'end', 'percent_diff', 'y0', 'y1', 'color', 'plot')
    values$line_data_table <- rbind(values$line_data_table, new)
    print(values$line_data_table)    
  }
  
  output$downloadData <- downloadHandler(
    filename = "line_data.csv",
    content = function(file) {
      write.csv(line_data_table(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)