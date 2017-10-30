library(shiny)
shinyUI(pageWithSidebar (
  headerPanel( "Forecast"),
  sidebarPanel(width=3,
    fileInput('file1', 'Choose a file to upload',
              accept = c(
                '.txt',
                '.csv',
                '.tsv',
                ".xlsx"
              )),
    helpText("(Only .txt, .csv and .xlsx files can be uploaded)"),
    numericInput("col", label = "My Data is at Column:", value=1,min=1,max=100),
    helpText("(i.e if your data is at column A of the excel file, put 1)"),
    numericInput("year", label = "Enter the starting year", value=2000),
    selectInput("time", label = "Frequecy of Data", choices=c("Daily","Monthly","Quarterly","Yearly"),selected="Monthly"),
    selectInput("month", label = "Enter the starting day/month/quarter", choices=as.numeric(c(1:366)), selected=as.numeric(1)),
    selectInput("Model","Select the Model:", choices=c("ARIMA","Holt-Winters Additive","Holt-Winters Multiplicative","HOLT's Exponential Smoothing"),selected="ARIMA"),
    numericInput("length",label="Enter the length of forecast",value=24, min=1, max=200),
    conditionalPanel("output.fileUploaded",
                     downloadButton('downloadData', 'Forecast'),
                     downloadButton('downloadPlot', 'Plot'))
  ),
  mainPanel (
    tabsetPanel(
      tabPanel("Forecasted Plot", plotOutput("M")),
      tabPanel("Model", verbatimTextOutput("P"), class="span7"),
      tabPanel("Forecasted Values", tableOutput("F"))
    ))
)
)
