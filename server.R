shinyServer(function(input,output,session) {
  library(forecast)
  library(openxlsx)
  library(tools)
  if(as.vector(Sys.info()['sysname'])=="Windows") {
    Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
  }
  observe({
    val <- input$time
    val1<-input$year
    v1<-c("Daily","Monthly","Quarterly","Yearly")
    v2<-list(c(1:366),c(1:12),c(1:4),c(1))
    v3<-c("Day","Month","Quarter","Year")
    w<-which(v1==val)
    if(val!="Yearly") {
      updateSelectInput(session, "month", paste("Enter the starting", v3[w],sep=" "), selected = as.numeric(1),
                        choices= as.numeric(v2[[w]][1]:v2[[w]][length(v2[[w]])]))}
    else {updateSelectInput(session, "month", paste("Enter the starting", v3[w],sep=" "), selected = val1,
                            choices= val1)}
  })
  observe ({
    vall<-input$time
    if (vall=="Yearly") {
      updateSelectInput(session,"Model", "Select the Model:", selected="ARIMA", choices=c("ARIMA","HOLT's Exponential Smoothing"))
    }
    else {updateSelectInput(session,"Model", "Select the Model:", selected="ARIMA", choices=c("ARIMA","HOLT's Exponential Smoothing","Holt-Winters Additive","Holt-Winters Multiplicative"))}
  })
  mm<-function(Model,col,time,year, month,length) { 
    inFile <- reactive({input$file1})
    d <- reactive({
      validate(
        need(input$file1 != "", "Please select a data set, right now only .txt, .csv and .xlsx data files can be processed, make sure the 1st row of your data contains the variable name.")
      )
      if (is.null(inFile))
        return(NULL)
      if (file_ext(inFile()$name)=="xlsx") {
        read.xlsx(inFile()$datapath)
      }
      else if(file_ext(inFile()$name)=="csv")  {
        read.csv(inFile()$datapath, header=T
        )}
      else {
        read.table(inFile()$datapath, header=T
        )}
    }) 
    output$fileUploaded <- reactive({
      return(!is.null(inFile()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    xx<-c("Daily","Monthly","Quarterly","Yearly")
    yy<-c(365,12,4,1)
    if (time!="Yearly") {
      b <- ts(d()[,col], frequency=yy[which(xx==time)], start=c(year,month))}
    else {b <- ts(d()[,col], frequency=yy[which(xx==time)], start=c(year))
    }
    bb<-holt(b,h=length)
    if (Model=="ARIMA") {
      a<-auto.arima(b)
    }
    else if (Model=="Holt-Winters Additive") {
      a<-hw(b, "additive", h=length)$model
    }
    else if (Model=="Holt-Winters Multiplicative") {
      a<-hw(b, "multiplicative", h=length)$model
    }
    else {a<-holt(b,h=length)$model}
    f<-forecast(a,level=c(80,95),h=length)
    pp<-plot(f,col="red", shadecols="oldstyle")
    ff<-as.data.frame(f)
    fff<-data.frame(date=row.names(ff),ff)
    if(input$time=="Daily") {row.names(ff)<-((nrow(d())+1):(nrow(d())+length))}
    list(model=a,plot=pp,fore=f,foreT=ff,tab=fff)
  }
  output$M<-renderPlot({mm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)[[2]]})
  output$P<- renderPrint({mm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$model})
  output$F<-renderTable({mm(input$Model, input$col,input$time,input$year,as.numeric(input$month),input$length)[[4]]})
  output$downloadData <- downloadHandler(
    filename = paste("forecast_", Sys.Date(),".xlsx"),
    content = function(file) {
      write.xlsx(mm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)[[5]], file, asTable=T, row.names=F)
    }
  )
  output$downloadPlot <- downloadHandler(
    filename = paste("forecast",Sys.Date(),".png",sep=""),
    content = function(file) {
      png(file,width=800,height=500, units="px")
      plot(mm(input$Model, input$col,input$time,input$year,as.numeric(input$month),input$length)[[3]],col="red")
      dev.off()
    })    
})
