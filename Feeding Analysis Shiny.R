list.of.packages <- c("shiny","ggplot2","data.table","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)


ui <- pageWithSidebar(
  headerPanel("Feeding Analysis"),
  sidebarPanel(fileInput("file1", "File",multiple = T, buttonLabel = "Browse...", placeholder= "No File Selected",accept = c(".clfdr")),
               uiOutput("slider"),
               sliderInput("hoffset", "Hour Offset (Wavelet)", min = 0, max = 24, step = 1, value = 0),
               checkboxInput("median", "Show Median (Wavelet)", value = F),
               checkboxInput("acrophase", "Show 'Acrophase' (Wavelet)", value = F),
               br(),
               submitButton("Apply Changes", icon("refresh")),
               br(), br(),
               downloadButton('downloadTable', 'Download Raw Data'),
               br(),br(),
               downloadButton('downloadPlot', 'Download Plot'),
               br(),br(),
               downloadButton('downloadWavelet', 'Download Wavelet')),
  mainPanel(textOutput("filen"),
            tabsetPanel(tabPanel("Plot", icon = icon("chart-line", lib = "font-awesome"), plotOutput("plot"),tableOutput("table2")),
                tabPanel("Table", icon = icon("table"),tableOutput("table1")),
                tabPanel("Wavelet", icon = icon("brialle", lib = "font-awesome"), plotOutput("wavelet"))
    )
  )
)



server <- function(input, output){
  
  #Get counts of every animal and put into readable file
  getMaster <- reactive({
    if (is.null(input$file1)){
      return(NULL)
    } else {
      numfiles <- nrow(input$file1)
      dfs <- list()
      master <- read.delim(input$file1[[1,'datapath']], header=FALSE, col.names=c("SoftName", "TakenTimeStamp", "TakenDate", "TakenTime", "Col5", "Col6", "Error", "DrpDate", "DrpTime", "col10", "col11"))
      master <- data.table(master$TakenDate)
      colnames(master) <- c("TakenDate")
      dates <- unique(master$TakenDate)
      master <- data.frame("TakenDate" = dates, "Day" = c(1:length(dates)))
      dfs[[1]] <- master
      
      for (i in 1:numfiles){
        df <- read.delim(input$file1[[i,'datapath']], header=FALSE, col.names=c("SoftName", "TakenTimeStamp", "TakenDate", "TakenTime", "Col5", "Col6", "Error", "DrpDate", "DrpTime", "col10", "col11"))
        feeder <- df$SoftName[1]+1
        df <- data.table("TakenDate" = df$TakenDate, "Time" = df$TakenTime)
        tkn_time <- as.character(df$Time) 
        df$Time <- sapply(strsplit(tkn_time,":"),
                          function(x) {
                            x <- as.numeric(x)
                            x[1]
                          })
        df <- count(df$TakenDate)
        colnames(df) <- c("TakenDate", paste("Count ",feeder))
        dfs[[i+1]] <- df
      }
      mymerge <- function(x,y){
        join(x,y,by="TakenDate", type="left", match = "all")
      }
      master <- Reduce(mymerge,dfs)
      average <- sapply(c("Count"),function(x){ rowMeans(master[grep(x,names(master))],na.rm=T)})
      errorav <- sapply(c("Count"),function(x){ rowMeans(master[grep(x,names(master))],na.rm=T)/ncol(master[grep(x,names(master))])})
      setNames(cbind(average,errorav, master),c("Average", "Error", names(master)))
    }
  })
  
  
  #Generate slider based on feeder file input
  output$slider <- renderUI({
    Master <- getMaster()
    if (is.null(Master)){
      sliderInput("range", "Range", 1, 10, value = c(1,10))
    } else {
      sliderInput("range", "Range", 1,max(nrow(Master)),value = c(1,max(nrow(Master))))
    }
  })
  
  #Generate plot of all
  getPlot <- reactive({
      Master <- getMaster()
      if (is.null(Master)){return(NULL)}else{
        Master <- Master[1:4]
        if (nrow(input$file1) == 1){Master$Error <- 0}
        ggplot(Master, aes(x=Day, y=Average)) +
          geom_errorbar(color = "grey", data = Master, aes(x = Day, ymin= Average-Error, ymax = Average+Error)) +
          geom_point(color = "steelblue", data = Master, aes(x = Day, y = Average), size = 2) +
          scale_x_continuous(expand = c(0,1), limits = c(input$range[1],input$range[2]), breaks = seq(from = 0, to = max(Master$Day)+1, by = 6)) +
          scale_y_continuous(expand = c(0,1), limits = c(0,max(Master$Average)+max(Master$Error)),breaks = seq(from = 0, to = 1000, by = 4)) +
          #geom_line(colour = "black", data = Master, aes(x = Day, y = Average), size = 0.4)+
          xlab("Day")+
          ylab("Count") +
          theme_bw() +
          theme(axis.title.y = element_text(face = "plain", color = "black", size = 15), 
                axis.title.x = element_text(face = "plain", color = "black", size = 15), 
                axis.text.y = element_text(face = "plain", color = "black", size = 10),
                axis.text.x = element_text(face = "plain", color = "black", size = 10),
                title = element_text(face = "bold", color = "black", size = 15))
      }
  })
  
  #Get raw data of all 
  getData <- reactive({
    Master <- getMaster()
    if (is.null(Master)){return(NULL)}else{
      Master
    }
  })
  
  #Get average sum of all 
  getSum <- reactive({
    Master <- getMaster()
    if (is.null(Master)){return(NULL)}else{
      days <- Master$Day
      if (is.null(input$range)){range<-days}else{range <- c(input$range[1]:input$range[2])}
      df <- Master[which(Master$Day %in% range),]
      Master <- cbind(sum(df$Average, na.rm = T), sum(df$Error,na.rm=T),mean(df$Average),mean(df$Error), nrow(df))
      colnames(Master) <- c("Sum", "SumError", "Average", "AverageError", "Days")
      Master
    }
  })
  
  #Get individual sum values of each animal based on slider
  getIndSum <- reactive({
    Master <- getMaster()
    if (is.null(Master)){return(NULL)}else{
      days <- Master$Day
      if (is.null(input$range)){range<-days}else{range <- c(input$range[1]:input$range[2])}
      df <- Master[which(Master$Day %in% range),]
      dfg <- df[-c(1:4)]
      dfs <- data.table("Animal" = colnames(dfg), "Sum" = colSums(dfg, na.rm = T))
      dfs
    }
  })
  
  getWavelet <- reactive({
    #start master generation
    if (is.null(input$file1)){
      return(NULL)
    } else {
      numfiles <- nrow(input$file1)
      dfs <- list()
      master <- read.delim(input$file1[[1,'datapath']], header=FALSE, col.names=c("SoftName", "TakenTimeStamp", "TakenDate", "TakenTime", "Col5", "Col6", "Error", "DrpDate", "DrpTime", "col10", "col11"))
      master <- data.table(master$TakenDate)
      colnames(master) <- c("TakenDate")
      dates <- unique(master$TakenDate)
      master <- data.frame("TakenDate" = dates, "Day" = c(1:length(dates)))
      days <- master$Day
      if (is.null(input$range)){range<-days}else{range <- c(input$range[1]:input$range[2])}
      df <- master[which(master$Day %in% range),]
      #browser()
      for (d in df$Day){
        t <- data.frame("TakenDate" = rep(df$TakenDate[which(df$Day == d)]), "Day" = rep(d), "Hour" = c(0:23))
        if (exists("times")){times <- rbind(times, t)}else{times<-t}
      }
      master <- times
      dfs[[1]] <- master
      for (i in 1:numfiles){
        df <- read.delim(input$file1[[i,'datapath']], header=FALSE, col.names=c("SoftName", "TakenTimeStamp", "TakenDate", "TakenTime", "Col5", "Col6", "Error", "DrpDate", "DrpTime", "col10", "col11"))
        feeder <- df$SoftName[1]+1
        df <- data.table("TakenDate" = df$TakenDate, "Time" = df$TakenTime)
        tkn_time <- as.character(df$Time) 
        df$Time <- sapply(strsplit(tkn_time,":"),
                          function(x) {
                            x <- as.numeric(x)
                            x <- x[1] + input$hoffset[1]
                            if (x > 23){x <- x-24}
                            x
                          })
        if (exists("dfm")){rm(dfm)}
        #browser()
        for (d in unique(df$TakenDate)){
          dfd <- df[which(df$TakenDate == d),]
          dfd <- data.frame(rep(d),count(dfd, vars = "Time"))
          if (length(dfd) < 3){next}
          if (exists("dfm")){dfm<-rbind(dfm,dfd)}else{dfm<-dfd}
        }
        #browser()
        colnames(dfm) <- c("TakenDate", "Hour", paste("Count ",feeder))
        dfs[[i+1]] <- dfm
      }
      mymerge <- function(x,y){
        join(x,y,by=c("TakenDate", "Hour"), type="left", match = "all")
      }
      master <- Reduce(mymerge,dfs)
      sum <- sapply(c("Count"),function(x){ rowSums(master[grep(x,names(master))],na.rm=T)})
      master <- cbind("Sum" = sum, master)
    }
    #end master generation
    if (is.null(master))(return(NULL))
    #plot data
    plot <- ggplot(master,aes(x=Day, y=Hour)) +
      geom_raster(aes(fill = master$Count)) +
      scale_fill_gradient(name = "Total Counts", low = "white", high = "steelblue") +
      scale_y_continuous(breaks = seq(from = 0, to =24, by = 3), expand = c(0,0)) +
      scale_x_continuous(breaks = seq(from = 1, by = 5, to = max(master$Day)), expand = c(0,0))+
      theme_bw(base_size = 18)+
      theme(axis.ticks = element_blank(),
            plot.background = element_blank(),
            panel.border = element_rect(size = 1))
    
    #Calculate median per day
    if (input$median == T){
      for (d in unique(master$Day)){
        df <- master[which(master$Day == d),]
        med <- df$Hour[which(df$Count == max(df$Count))]
        med <- mean(med)
        med <- data.frame("Day" = d, "Median" = med)
        if (exists("Median")){Median <- rbind(Median,med)}else{Median <- med}
      }
      plot <- plot + geom_point(data = Median, aes(x = Day, y = Median))
    }
    #Calculate 'Acrophase' of feeding
    if (input$acrophase == T){
      withProgress(message = "Calculating Acrophase", value = 0, {
        n <- max(unique(master$Day))
        for (d in unique(master$Day)){
          df <- master[which(master$Day == d),]
          fit <- lm(Count ~ sin(2*pi*Hour/24)+cos(2*pi*Hour/24), data = df)
          
          AlphaC<-fit$coefficients[1];
          BetaC<-fit$coefficient[2];
          GammaC<-fit$coefficient[3];
          
          AllT <- seq(from=0,to=24, by=0.00001)
          AllFit <- (GammaC*cos(2*pi*AllT/24)+BetaC*sin(2*pi*AllT/24)+AlphaC);
          acro <- AllT[which(AllFit==max(AllFit))]; ##Acrophase##
          acro <- data.frame("Day" = d, "Acrophase" = acro)
          if (exists("Acrophase")){Acrophase <- rbind(Acrophase,acro)}else{Acrophase <- acro}
          incProgress(1/n, detail = paste("Day: ", d))
        }
        plot <- plot + geom_point(data = Acrophase, aes(x = Day, y = Acrophase), colour = "red")
        })
  }
    
    plot

  })
  
  output$table1 <- renderTable(getData())
  output$plot <- renderPlot(getPlot(),height = 400, width = 600)
  output$table2 <- renderTable(getSum())
  output$table3 <- renderTable(getIndSum())
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$plot, '.png', sep='') },
    content = function(file) {
      ggsave(file,getPlot())
    }
  )
  output$downloadTable <- downloadHandler(
    filename = function() {paste(input$table1, '.csv', sep='')},
    content = function(file) {write.csv(getData(), file, row.names = F)}
  )
  output$downloadWavelet <- downloadHandler(
    filename = function() { paste(input$wavelet, '.png', sep='') },
    content = function(file) {
      ggsave(file,getWavelet())
    }
  )
  output$wavelet <- renderPlot(getWavelet(), height = 800, width = 1200)
  output$filen <- renderText({if (is.null(input$file1)){return(NULL)}else{if (nrow(input$file1) < 2){paste("There is ",nrow(input$file1)," file")
    }else{paste("There are ",nrow(input$file1), " files")}}})
}

shinyApp(ui, server)
