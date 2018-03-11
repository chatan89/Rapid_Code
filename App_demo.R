rm(list = ls())
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_162')
.libPaths( c( .libPaths(), "C:/Users/Anand Suresh/Documents/R/win-library/3.4") )

library(shiny)
library(shinydashboard)
library(tm)
library(RWeka)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(stringr)
library(Matrix)
library(lubridate)
library(data.table)
library(tidyr)
library(readr)
library(DT)
library(RColorBrewer) 
library(gridExtra)
library(grid)
library(sqldf)

ui <- navbarPage(title=img(src='logo.png', align = "right",height="50px",width="200px"),
                 tabPanel("DEFECT-ANALYTICS",
                          dashboardPage(
                            dashboardHeader(title = "DEFECT-ANALYTICS",titleWidth = 250),
                            dashboardSidebar(
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              sidebarMenu(
                                menuItem("WordCloud", tabName = "WordCloud", icon = icon("dashboard")),
                                menuItem("BarChart", tabName = "BarChart",icon = icon("bar-chart-o")),
                                menuItem("DefectTab", tabName = "DefectTab",icon = icon("table"))
                              ),   
                              width = 350,
                              br(),
                              fileInput("InputTextVariable1", "Enter the defect folder path", accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv",
                                "readLines",
                                "scan")),
                              radioButtons("gram", "Select Word Gram:",
                                           c("1 Word" = "1",
                                             "2 Words" = "2",
                                             "3 Words" = "3",
                                             "4 Words" = "4")),
                              column(1, align="center", offset = 3,actionButton("submit", "Submit")),
                              br(),br(),
                              textInput("InputText", "Enter words to be filtered separated by comma", value = "Enter text..."),
                              column(1, align="center", offset = 0,actionButton("submit1", "Filter")),
                              br(),br(),
                            sliderInput("Inputfrequency", "Filter Frequency:", 1, 50, 2),
                              h4(" Search Word",align = "center",font="bold"),
                              sidebarSearchForm(textId = "InputTextVariable2", buttonId = "searchButton",label = "Search...")
                              
                            ),
                            dashboardBody(
                              # Boxes need to be put in a row (or column)
                              tags$head(
                                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                              ),
                              tabItems(
                                tabItem(tabName = "WordCloud",
                                        fluidRow(
                                          box(
                                            downloadButton("Word_Cloud", "Download"),
                                            title = "WordCloud", width = 12, status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                            plotOutput("WordCloudPlot", height = 600)
                                            
                                          )
                                        )     
                                ),
                                tabItem(tabName = "BarChart",
                                        fluidRow(box(
                                          downloadButton('Bar_Chart', 'Download'),
                                          title = "BarChart",  width = 12,status = "info", solidHeader = TRUE,collapsible = TRUE,
                                          plotOutput("BarChartPlot", height = 600)
                                        )
                                        )
                                ),
                                tabItem(tabName = "DefectTab",
                                        tabPanel('DefectTab',dataTableOutput("mytable"))
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("DQ-OUTLIER", 
                          dashboardPage(skin = "black",
                                        dashboardHeader(title = "DQ-OUTLIER",titleWidth = 250),
                                        dashboardSidebar(
                                          tags$style(type="text/css",
                                                     ".shiny-output-error { visibility: hidden; }",
                                                     ".shiny-output-error:before { visibility: hidden; }"
                                          ),
                                          sidebarMenu(
                                            menuItem("DQ-Tab", tabName = "DQ-Tab",icon = icon("table"))
                                          ),   
                                          width = 350,
                                          br(),
                                          fileInput("DQTextVariable1", "Enter training set:", accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv",
                                            "readLines",
                                            "scan")),
                                          fileInput("DQTextVariable2", "Enter test set:", accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv",
                                            "readLines",
                                            "scan")),
                                          column(1, align="center", offset = 3,actionButton("submit2", "Submit")),
                                          br()
                                        ),
                                        dashboardBody(
                                          tags$head(
                                            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                                          ),
                                          tabItems(
                                            tabItem(tabName = "DQ-Tab",tabPanel('DQ-Tab',DT::dataTableOutput("DQmytable"))
                                            )
                                          )
                                        ) 
                          )
                 ),
                 tabPanel("RTS", 
                          dashboardPage(skin = "black",
                                        dashboardHeader(title = "RTS",titleWidth = 250),
                                        dashboardSidebar(
                                          tags$style(type="text/css",
                                                     ".shiny-output-error { visibility: hidden; }",
                                                     ".shiny-output-error:before { visibility: hidden; }"
                                          ),
                                          sidebarMenu(
                                            menuItem("Naive-Output", tabName = "Navie-Output",icon = icon("table"))
                                          ),   
                                          width = 350,
                                          br(),
                                          fileInput("RbtTextVariable1", "Upload Test Case:", accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv",
                                            "readLines",
                                            "scan")),
                                          fileInput("RbtTextVariable2", "Upload Defect dump:", accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv",
                                            "readLines",
                                            "scan")),
                                          column(1, align="center", offset = 3,actionButton("submit4", "Submit")),
                                          br()
                                        ),
                                        dashboardBody(
                                          tags$head(
                                            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                                          ),
                                          tabItems(
                                            tabItem(tabName = "Navie-Output",tabPanel('Navie-Output',DT::dataTableOutput("Navietable")))
                                            
                                          )
                                        ) 
                          )
                 ),
                 inverse = TRUE
)

server <- function(input, output,session) {
  
  ############################Defect Cluster Code#########################          
  fileload <- reactive({
    fileload <- input$InputTextVariable1
    if (is.null(fileload)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    data.frame(iconv(readLines(fileload$datapath),"UTF-8", "UTF-8",sub=''))
  }) 
  
  Data <- reactive({
    getWordCloud <- function(file1) {
      Data <- VCorpus(VectorSource(file1))
      #Removing numbers
      Data <- tm_map(Data,removeNumbers)
      #Removing stopwords (common words) that usually have no analytic value like words (a, and, also, the,)
      Data <- tm_map(Data, removeWords, stopwords("english"))
      #Replace punctuation:
      rmsep <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      Data <- tm_map(Data, rmsep, "[[:punct:]]")
      #Converting all letters to lowercase for make
      Data <- tm_map(Data, content_transformer(tolower))  
      #Converting Processed Data into text documents
      Data <- tm_map(Data, PlainTextDocument) 
      rem <- content_transformer(function(x, pattern) gsub(pattern, "", x))
      strng <- gsub(",","|",input$InputText)
      Data <- tm_map(Data, rem, strng)
      #Removing whitespace
      Data <- tm_map(Data, stripWhitespace) 
      gramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = input$gram, max = input$gram))
      TermDataMatrix <- TermDocumentMatrix(Data, control = list(tokenize = gramTokenizer))
      TermDataMatrix <- removeSparseTerms(TermDataMatrix, 0.1)
      Datafrequency <- sort(rowSums(as.matrix(TermDataMatrix)), decreasing=TRUE)
      data.frame(word=names(Datafrequency), freq=as.integer(Datafrequency),stringsAsFactors = FALSE)
      
    }
    if(input$submit1 > 0 && !is.null(fileload())) {
      isolate({
        withProgress({
          setProgress(message = "Processing VCorpus for word cloud to get Workflow...") 
          getWordCloud(fileload())
        })  
      })
    }
    if(input$submit > 0) {
      isolate({
        withProgress({
          setProgress(message = "Processing VCorpus for word cloud to get Workflow...") 
          getWordCloud(fileload())
        })  
      })
    }
    
  }) 
  
  
  
  wordcloud_rep <- repeatable(wordcloud)
  search <- eventReactive(input$searchButton, {
    input$InputTextVariable2 
  } , ignoreNULL = FALSE
  )
  
  output$WordCloudPlot <- renderPlot({
    WordCloudData <- Data()
    WordCloudData <- subset(WordCloudData, grepl(search(), WordCloudData$word))
    if(!is.null(WordCloudData$freq)){
      freqValue <- input$Inputfrequency
        if( input$Inputfrequency >= max(WordCloudData$freq)){
        freqValue <- coalesce(max(WordCloudData$freq),1)  
      } else {
        freqValue <- input$Inputfrequency
      }
    }
    suppressMessages(wordcloud_rep(WordCloudData$word,WordCloudData$freq,min.freq = freqValue,colors=brewer.pal(8,"Dark2"),scale=c(2.5,0.65),random.color=FALSE,random.order=FALSE))
  })
  
  output$BarChartPlot <- renderPlot({
    BarChartData <- Data()
    BarChartData <- subset(BarChartData, grepl(search(), BarChartData$word))
    subset(BarChartData, freq >= input$Inputfrequency) %>%
      ggplot(aes(word, freq,fill=freq)) + geom_bar(stat="identity")+ theme(text = element_text(size=20),axis.text.x=element_text(angle=45,hjust=1))
    
  })
  
  getDefectTable <- reactive({
    getData <- function(file1) {
      Data <- VCorpus(VectorSource(file1))
      #Removing numbers
      Data <- tm_map(Data,removeNumbers)
      #Removing stopwords (common words) that usually have no analytic value like words (a, and, also, the,)
      Data <- tm_map(Data, removeWords, stopwords("english"))
      #Replace punctuation:
      rmsep <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      Data <- tm_map(Data, rmsep, "[[:punct:]]")
      #Converting all letters to lowercase for make
      Data <- tm_map(Data, content_transformer(tolower))  
      #Converting Processed Data into text documents
      Data <- tm_map(Data, PlainTextDocument) 
      rem <- content_transformer(function(x, pattern) gsub(pattern, "", x))
      strng <- gsub(",","|",input$InputText)
      Data <- tm_map(Data, rem, strng)
      #Removing whitespace
      Data <- tm_map(Data, stripWhitespace) 
      dataframe <- data.frame(text=unlist(sapply(Data, `[`, "content")), stringsAsFactors=F,row.names = NULL)
      gramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = input$gram, max = input$gram))
      TermDataMatrix <- TermDocumentMatrix(Data, control = list(tokenize = gramTokenizer))
      TermDataMatrix <- removeSparseTerms(TermDataMatrix, 0.1)
      Datafrequency <- sort(rowSums(as.matrix(TermDataMatrix)), decreasing=TRUE)
      wordList <- as.matrix(names(Datafrequency))
      dataDefectTab <- NULL
      for ( i in 1:nrow(wordList)){
        
        Word <- wordList[i]
        value <- as.data.frame(t(gsub("content","",rownames(subset(dataframe,grepl(Word, dataframe$text))))))
        DefectNo <-  apply(value,1,paste,collapse="|")
        mergeCol <- cbind(Word,DefectNo)
        dataDefectTab <- rbind(dataDefectTab,mergeCol)
      }
      dataDefectTab
    }
    if(input$submit1 > 0 && !is.null(fileload())) {
      isolate({
        withProgress({
          setProgress(message = "Processing Defect Report...") 
          getData(fileload())
        })  
      })
    }
    if(input$submit > 0) {
      isolate({
        withProgress({
          setProgress(message = "Processing Defect Report...") 
          getData(fileload())
        })  
      })
    }
    
  }) 
  output$mytable <- DT::renderDataTable({
    getDefectTable()
    
  })  
  ########################Generate PDF Report########################### 
  output$Word_Cloud <- downloadHandler(
    # For PDF output, change this to "report.html"
    filename = "Rapid_word_cloud.pdf",
    content = function(file) {
      WordCloudData <- Data()
      WordCloudData <- subset(WordCloudData, grepl(search(), WordCloudData$word))
      if(!is.null(WordCloudData$freq)){
        if( input$Inputfrequency >= max(WordCloudData$freq)){
          freqValue <- coalesce(max(WordCloudData$freq),1)  
        } else {
          freqValue <- input$Inputfrequency
        }
      }
      pdf(file)
      plot <- wordcloud(WordCloudData$word,WordCloudData$freq,min.freq = freqValue,colors=brewer.pal(8,"Dark2"),scale=c(2.5,0.65),random.color=FALSE,random.order=FALSE)
      print(plot)
      dev.off()     
      
    }
  )
  
  output$Bar_Chart <- downloadHandler(
    # For PDF output, change this to "report.html"
    filename = "Rapid_bar_chart.pdf",
    content = function(file) {
      BarChartData <- Data()
      BarChartData <- subset(BarChartData, grepl(search(), BarChartData$word))
      pdf(file)
      plot <- subset(BarChartData, freq >= input$Inputfrequency) %>%
        ggplot(aes(word, freq,fill=freq)) + geom_bar(stat="identity")+ theme(text = element_text(size=20),axis.text.x=element_text(angle=45,hjust=1))
      print(plot)
      dev.off() 
      
    }
  )
  
  
  ############################Regression Reactive code#########################        
  fileloadTrain <- reactive({
    fileload <- input$DQTextVariable1
    if (is.null(fileload)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(file = fileload$datapath, header = TRUE, 
             stringsAsFactors = FALSE, strip.white = TRUE)
  }) 
  
  fileloadTest <- reactive({
    fileload <- input$DQTextVariable2
    if (is.null(fileload)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(file = fileload$datapath, header = TRUE, 
             stringsAsFactors = FALSE, strip.white = TRUE)
  })        
  
  trainedstat <- reactive({
    #function for handling missing data value 
    handle_missing_rows<-function(data)
    {
      uniquevalue1<- unique(select(data,trx_date))
      uniquevalue1<-as.character(uniquevalue1[[1]])
      uniquevalue2<- unique(select(data,datavalue))
      Refset<-data.table()
      for(i in 1:length(uniquevalue1)){
        Refset<-rbind(Refset,
                      cbind(data.table(trx_date=rep((uniquevalue1[i]), times = nrow(uniquevalue2))),
                            uniquevalue2))
      }
      Refset$trx_date<-ymd(Refset$trx_date)
      cleansed_data<-full_join(x=Refset,y=data, by = c("trx_date", "datavalue"))
      n_coll<-ncol(cleansed_data)
      cleansed_data[,n_coll][is.na(cleansed_data[,n_coll])]<-0
      return(cleansed_data)
    }
    
    cleanData <- function(file1) {
      #reading data
      sampledata <- file1
      #Convert date format
      sampledata[,1]<-mdy(sampledata[,1])
      #column information in sample data
      Ctotal<-ncol(sampledata)
      Cnames<-colnames(sampledata)
      #filling blank values
      sampledata[,2:(Ctotal-1)][is.na(sampledata[,2:(Ctotal-1)])]<-"UNKNOWN"
      sampledata[,2:(Ctotal-1)][sampledata[,2:(Ctotal-1)]==""]<-"UNKNOWN" 
      #concatenate all data attribuites into single column
      sampledata$attribute<-do.call(paste, c(sampledata[,-c(1,Ctotal)],sep = "|"))
      #select only concatenated columnn, date, count
      formated_sampledata<-sampledata[,c(1, ncol(sampledata),ncol(sampledata)-1 )] %>% data.table()
      #adding column names
      setnames(formated_sampledata, names(formated_sampledata),c("trx_date", "datavalue","datacount"))
      #cal function to missing data vallue
      formated_sampledata<-handle_missing_rows(data = formated_sampledata)%>%data.table()
      #count number of transactions per day
      formated_sampledata<-formated_sampledata[,Total_trxperday := sum(datacount), by = trx_date]
      #create name value for statistics
      Dstat0<-formated_sampledata
      #calculate data distribution per day
      Dstat0<-Dstat0[, percent_distribution := round(datacount/Total_trxperday, 8)]
      #Dstat0
    }        
    getTrainData <- function(x) {
      Dstat0 <- cleanData(fileloadTrain())
      Cnames <- colnames(fileloadTrain())
      Ctotal<-ncol(fileloadTrain())
      #calcualte sample mean and population mean
      Dstat0<-Dstat0[,":="(average_distribution = round(mean(percent_distribution),8),
                           standard_deviation =round(sd(percent_distribution),8),
                           sample_count = .N),
                     by = datavalue]%>%
        mutate(standard_error = round(standard_deviation/sqrt(sample_count),8)) %>% # std_err
        mutate(training_distribution_lwr = round(qnorm(0.0251,average_distribution-standard_error,standard_deviation ),8), #lowerrange taile
               training_distribution_upr = round(qnorm(0.975,average_distribution-standard_error,standard_deviation ),8) #upper range tail
        )
      #select sample stat columns
      samplestat<-unique(select(Dstat0, datavalue,training_distribution_lwr,training_distribution_upr )) %>%
        separate(col=datavalue,into=Cnames[2:(Ctotal-1)],sep ="\\|")
      write.csv(samplestat, file = "./Results/trainmodel_summary.csv", row.names = FALSE)
      write.csv(Dstat0, file = "./Results/trainmodel_detail.csv", row.names = FALSE)
      Dstat0
    }
    getTestData <- function(x) {
      Dstat0 <- cleanData(fileloadTest())
      Cnames <- colnames(fileloadTest())
      Ctotal<-ncol(fileloadTest())
      #calcualte sample mean and population mean
      Dstat0<-Dstat0[,":="(test_distribution = round(mean(percent_distribution),8)),
                     by = datavalue]
      #select sample stat columns
      teststat<-unique(select(Dstat0, datavalue,test_distribution )) %>%
        separate(col=datavalue,into=Cnames[2:(Ctotal-1)],sep ="\\|")
      ##comparision process
      #read training data. check needs to be added if no trained value found
      trainedstat<- read.csv("./Results/trainmodel_summary.csv", header = TRUE, strip.white = TRUE, 
                             stringsAsFactors = FALSE)
      #test process
      Result<-full_join(teststat,trainedstat)%>% 
        mutate (test_distribution=ifelse(is.na(test_distribution),0,test_distribution),
                training_distribution_lwr=ifelse(training_distribution_lwr<0,0,training_distribution_lwr))%>%
        mutate(action = ifelse(test_distribution>=training_distribution_lwr & test_distribution<=training_distribution_upr,"passed","failed"))
      
      
      #writing data into csv
      write.csv(Result, file="./Results/Analysis_0412.csv",row.names = FALSE )
      Result
      
    }
    
    if(input$submit2 > 0 && !is.null(fileloadTrain()) && !is.null(fileloadTest())) {
      isolate({
        withProgress({
          setProgress(message = "Processing Training and Test Data...") 
          getTrainData(x)
          getTestData(x)
        })  
      })
    }
    
  }) 
  
  
  output$DQmytable <- DT::renderDataTable({
    trainedstat()
    
  }) 
  
  ############################RTS code#########################        
  TestCaseload <- reactive({
    fileload <- input$RbtTextVariable1
    if (is.null(fileload)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(file = fileload$datapath, header = TRUE, 
             stringsAsFactors = FALSE, strip.white = TRUE)
  }) 
  
  Defectdumpload <- reactive({
    fileload <- input$RbtTextVariable2
    if (is.null(fileload)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    data <- data.frame(read.csv(file = fileload$datapath,header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE))
    sample <- head(data,1)
    col <- grep("^Desc",colnames(sample),value=TRUE,ignore.case = TRUE)
    DataDesc <- data[,col]
    Encoding(DataDesc) <- "Latin1"
    data.frame(iconv(DataDesc,"Latin1", "UTF-8",sub=''))
    
  })        
  
  RbtData <- reactive({
    
    RTS <- function(x) {
      BRD_keywords <- readLines("./RTS/Data_Dictionary_BRD_Keywords.txt")
      BRD_keywords <- tolower(BRD_keywords) 
      
      test_data <- read.csv("D:/Project/RAPID/Data/Data/Claims_DS_Rls.csv",header = TRUE)
      # LIBRARY DEPLOYMENT
      test_data <- test_data[!(is.na(test_data$TS_TEST_ID) | test_data$TS_TEST_ID==""),] 
      test_data <- test_data[test_data$TC_STATUS != "Blocked" ,]
      test_data <- test_data[test_data$TC_STATUS != "No Run" ,] 
      test_data <- test_data[test_data$TC_STATUS != "N/A" ,]
      test_data <- test_data[test_data$TC_STATUS != "Not Completed" ,] 
      
      # FIND TEST CASE EXECUTION COUNT USING TEST CASE ID Vs TEST CASE STATUS 
      # ORDER THEM BASED ON TEST CASE ID 
      
      DT <- data.table(test_data) 
      DT[ , TC_COUNT := .N, by = list(TS_TEST_ID, TC_STATUS)] 
      DT <- DT[order(DT$TS_TEST_ID),]
      WG <- subset(DT,DT$TC_STATUS=="Failed",c("DS_DESCRIPTION"))
      
      
      #EXTRACTING QUERY AND STATUS 
      query <- tolower(DT$Query)
      TC_STATUS <- tolower(DT$TC_STATUS) 
      
      
      # DETERMING QUERY COMPLEXITY 
      # USING SQL / ORACLE FUNCTIONS FREQUENCY FINDER
      ## IDENTIFYING ALL ORACLE FUNCTIONS 
      select_Cnt<-str_count(query,"select")
      Join_Cnt<-str_count(query,"join")
      Predicate_Cnt<-str_count(query,"where")
      Union_Intrsct_Cnt<-str_count(query,'union')+str_count(query,'intersect')
      Base_Fun_Cnt<-str_count(query,'count')+str_count(query,'sum')+str_count(query,'sp_help')+str_count(query,'distinct')+str_count(query,'date_add')+str_count(query,'is')+str_count(query,'length')+str_count(query,'to_char')+str_count(query,'except')
      Base_Fun_Cnt<- Base_Fun_Cnt+str_count(query,'avg')+str_count(query,'substring')+str_count(query,'max')+str_count(query,'substr')+str_count(query,'date_diff')+str_count(query,'get_date')+str_count(query,'any')+str_count(query,'minus')
      Base_Fun_Cnt<-Base_Fun_Cnt+str_count(query,'charindex')+str_count(query,'abs')+str_count(query,'round')+str_count(query,'to_date')+str_count(query,' in ')+str_count(query,'like')+str_count(query,'all')+str_count(query,'and')
      Base_Fun_Cnt<-Base_Fun_Cnt+str_count(query,'cast')+str_count(query,'trim')+str_count(query,'group')+str_count(query,'having')+str_count(query,'format')+str_count(query,'order')
      Complx_Fun_cnt<-str_count(query,"pivot")+str_count(query ,"unpivot")+str_count(query,"partition")+str_count(query,"rank")+str_count(query,"regexp")+str_count(query,"with")+str_count(query,"dense_rank")
      Case_Cnt<-str_count(query,"case")+str_count(query,"when")
      
      ## ASSIGNING WEIGHTAGE TO FIND QUERY COMPLEXITY AND PARTITIONING THEM INTO A RANGE FACTOR 
      Weightage<- (select_Cnt*0.05)+(Join_Cnt*0.20)+(Predicate_Cnt*0.10)+(Case_Cnt*.15)+(Complx_Fun_cnt*.30)+((Base_Fun_Cnt+Union_Intrsct_Cnt)*.20)
      
      Query_Calc_1a <- aggregate(RN_RUN_ID ~ TS_TEST_ID, DT, function(RN_RUN_ID)  max((RN_RUN_ID))) 
      DT=merge(x=DT,y=Query_Calc_1a,by="TS_TEST_ID")
      Query_Calc1 <- ifelse((DT$RN_RUN_ID.x == DT$RN_RUN_ID.y), Weightage, 0) 
      DT$Weightage <- Query_Calc1 
      colnames(DT)[which(names(DT)=="RN_RUN_ID.x")]="RN_RUN_ID"
      
      #This step will get the final weightage from all steps
      Query_Calc2 <- aggregate(Weightage ~ TS_TEST_ID, DT, function(Weightage) sum((unique((Weightage))))) 
      Query_Calc3 <- sqldf("SELECT L.TS_TEST_ID, R.Weightage FROM DT as L LEFT JOIN Query_Calc2 as r ON L.TS_TEST_ID=r.TS_TEST_ID ORDER by L.TS_TEST_ID") 
      DT$Query_weight <- Query_Calc3$Weightage 
      
      ## SLICING FREQUENCY 
      lbl=c("1","2","3")
      wt_label<-cut(DT$Query_weight,3,labels=lbl)
      DT[,"Query_Complexity"]<-wt_label
      
      # IDENTIFY BUSINESS USE CASE 
      # USING KEYWORD DRIVEN APPROACH 
      # USING KEYWORD LOOKUP FUNCTION BETWEEN TEST SCENARIO DESCRIPTION VS BRD KEYWORDS 
      # If match found, then categorize "Business Use case" 
      # If No match found, then categorize "NOT a Business Use case" 
      
      ## READING TEST SCENARIO DESCRIPTION 
      Hash_Business_Case <- DT$TS_SCENARIO_NAME
      Hash_Business_Case <- tolower(Hash_Business_Case)
      
      ## INPUT BRD KEYWORDS 
      ## TABLE FORMAT TO DISPLAY BRD KEYWORDS
      ## DERIVE BUSINESS_USE CASE TEST CASES 
      ## IDENTIFY TEST SCENARIO DESCRIPTION STARTS WITH Hash_Business_Case(#) AND CONTAINS BRD KEYWORDS 
      ## THEN LOOKUP BRD KEYWORDS PASSED BY USER VS TEST SCENARIO DESCRIPTION 
      
      Hash_Business_Case1 <- ifelse(grepl("#",Hash_Business_Case),ifelse(grepl(paste(BRD_keywords,collapse="|"),
                                                                               Hash_Business_Case), "1", "0"),"0") 
      Hash_Business_Case1 <- ifelse(grepl(paste("#",BRD_keywords, sep="", collapse = "|"), Hash_Business_Case) , 
                                    "1", "0") 
      #Hash_Business_Case1 
      DT$Hash_Business_Case<-Hash_Business_Case1 
      
      
      # IDENTIFY REQUIREMENT COMPLEXITY
      ## FINDING TEST STEPS FOR A TEST INSTANCE
      ## SLICING FREQUENCY 
      lbl=c("1","2","3")
      wt_label<-cut(DT$TS_STEPS,3,labels=lbl)
      DT[,"Step_complexity"]<-wt_label
      
      
      ################################################################################################## 
      # IDENTIFY DEFECT PRONE NATURE OF TEST CASE 
      # DERIVED BASED ON NUMBER OF TIMES A TEST CASE HAS FAILED 
      ################################################################################################## 
      
      ## BUG SEVERITY
      DT <- sqldf("select t.*,case when (t.DEFECT_SEVERITY like '%Critical%') then 4 when ( t.DEFECT_SEVERITY like '%High%') then 3 when ( t.DEFECT_SEVERITY like '%Medium%') then 2 when (t.DEFECT_SEVERITY like '%Low%') then 1 else 0 end as Severity_Bug from DT t",drv="SQLite")
      
      ## TC_Failure_calculation
      DT1<- sqldf("select TS_TEST_ID,count(distinct RN_RUN_ID) as TC_Fail_recurrence from DT where lower(TC_STATUS)='failed' group by TS_TEST_ID,DEFECT_ID",drv="SQLite")
      
      DT=merge(x=DT,y=DT1,by="TS_TEST_ID",all.x = TRUE)
      
      DT$TC_Fail_recurrence[is.na(DT$TC_Fail_recurrence)]=0
      DT$Severity_TC= DT$TC_Fail_recurrence
      DT=within(DT,Severity_TC[TC_Fail_recurrence>=4],4)
      
      DT=filter(DT,RN_RUN_ID==RN_RUN_ID.y)
      DT_TC= DT[!duplicated(DT[c("TS_TEST_ID","Query_weight","Query_Complexity","Hash_Business_Case","Step_complexity","Severity_Bug","TC_Fail_recurrence","Severity_TC","TS_STEPS")]),]
      
      DT_TC$Weight1= with(DT_TC,as.numeric(Severity_Bug)+as.numeric(TC_Fail_recurrence)+(as.numeric(TS_STEPS)*0.1)+as.numeric(Hash_Business_Case)+as.numeric(Query_weight))
      
      # when using Caret NB method, we get errors due to sparse data set
      #model = train(Res_tab[,-2],Res_tab[,2],'nb')
      
      ######################################################################################
      #Read Defect Dump to get N-Grams
      ######################################################################################
      
      file1 <- Defectdumpload()
      WG <- VCorpus(VectorSource(file1))
      #Removing stopwords Numbers/Digits
      WG <- tm_map(WG,removeNumbers)
      #Replace punctuation:
      rmsep <- content_transformer(function(x, pattern) gsub(pattern, " ", x,perl=T))
      WG <- tm_map(WG, rmsep, "[[:punct:]]")
      #Removing stopwords (common words) that usually have no analytic value like words (a, and, also, the,)
      WG <- tm_map(WG, removeWords, stopwords("english"))
      #Converting all letters to lowercase for make
      WG <- tm_map(WG, content_transformer(tolower))  
      #Converting Processed WG into text documents
      WG <- tm_map(WG, PlainTextDocument) 
      #Removing whitespace
      WG <- tm_map(WG, stripWhitespace) 
      gramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
      TermWGMatrix <- TermDocumentMatrix(WG, control = list(tokenize = gramTokenizer))
      TermWGMatrix <- removeSparseTerms(TermWGMatrix, 0.1)
      WGfrequency <- sort(rowSums(as.matrix(TermWGMatrix)), decreasing=TRUE)
      Ngram <- data.frame(word=names(WGfrequency), freq=WGfrequency)
      wordlist <- as.matrix(subset(Ngram,freq > 1))
      Res_tab_ngram <- NULL
      NGRAM_Value <-0
      Word_Match <- NULL
      for ( i in 1:nrow(DT_TC)){
        for ( j in 1:100){
          Word <- paste0("\\<",wordlist[j],"\\>")
          
          if ( str_count(Word, "\\S+") > 1 && grepl(Word, gsub("[[:punct:]]", " ", tolower(DT_TC[i,"DS_DESCRIPTION"]))) ){
            value <- as.numeric(wordlist[j,2])
            Word_Match <- paste(Word_Match,"|",wordlist[j,1],":",wordlist[j,2])
          }
          else { value <- 0 
          }
          NGRAM_Value <- sum(NGRAM_Value,value)
          if( NGRAM_Value > 0 ){
            NGRAM_word <-  Word_Match
          }else { NGRAM_word <- "NO MATCH"}
        }
        mergeCol <- cbind(NGRAM_Value,NGRAM_word)
        Res_tab_ngram <- rbind(Res_tab_ngram,mergeCol)
        NGRAM_Value <- 0
        Word_Match <- NULL
      }
      
      DT_TC=cbind(DT_TC,Res_tab_ngram)
      DT_TC=arrange(DT_TC,desc(Weight1),desc(NGRAM_Value))
      
      #write.csv(DT_TC, file ="Res_tab1.csv") 
      
      ################################################################################################## 
      #Naive Bayes Algorithm - The Beginning 
      ##################################################################################################  
      
      Input2 <- sqldf("select GROUP_CONCAT(TC_STATUS) TC_STATUS1, test_data.* from test_data group by TS_TEST_ID || RN_RUN_ID ") 
      Input1 <- sqldf("select * from DT_TC")
      sts=DT_TC$TC_STATUS
      DT_TC[,"FailedTC"]=ifelse(sts=='Failed',1,0)
      
      # The below code uses defect severity and recurrence as a factor and weights mostly on the failed TC
      # ss=aggregate(Res_tab_ngram$FailedTC,by=list(Query_complexity=Res_tab_ngram$Query_Complexity,
      #                                         Business_use_case=Res_tab_ngram$Hash_Business_Case,
      #                                         defect_severity=Res_tab_ngram$Severity_Bug,
      #                                         TC_failure_rate=Res_tab_ngram$Severity_TC,
      #                                          step_complexity=Res_tab_ngram$Step_complexity),FUN =mean)
      
      ss=aggregate(DT_TC$FailedTC,by=list(Query_complexity=DT_TC$Query_Complexity,
                                          Business_use_case=DT_TC$Hash_Business_Case,
                                          Step_complexity=DT_TC$Step_complexity),FUN =mean)
      
      ss1=aggregate(DT_TC$FailedTC,by=list(Query_complexity=DT_TC$Query_Complexity),FUN =mean)
      ss2=aggregate(DT_TC$FailedTC,by=list(Business_use_case=DT_TC$Hash_Business_Case),FUN =mean)             
      ss3=aggregate(DT_TC$FailedTC,by=list(Step_complexity=DT_TC$Step_complexity),FUN =mean)             
      
      DT_TC=merge(x=DT_TC,y=ss, by.x = c("Query_Complexity","Hash_Business_Case","Step_complexity"),by.y = c("Query_complexity","Business_use_case","Step_complexity"))
      colnames(DT_TC)[which(names(DT_TC)=="x")]="bayes_weight"
      
      DT_TC=merge(x=DT_TC,y=ss1, by.x = c("Query_Complexity"),by.y = c("Query_complexity"))
      colnames(DT_TC)[which(names(DT_TC)=="x")]="Qry_nb"
      DT_TC$Qry_nb=DT_TC$Qry_nb+(1/(length(DT_TC)+1))
      DT_TC=merge(x=DT_TC,y=ss2, by.x = c("Hash_Business_Case"),by.y = c("Business_use_case"))
      colnames(DT_TC)[which(names(DT_TC)=="x")]="Bus_nb"
      DT_TC$Bus_nb=DT_TC$Bus_nb+(1/(length(DT_TC)+1))
      DT_TC=merge(x=DT_TC,y=ss3, by.x = c("Step_complexity"),by.y = c("Step_complexity"))
      colnames(DT_TC)[which(names(DT_TC)=="x")]="Step_nb"
      DT_TC$Step_nb=DT_TC$Step_nb+(1/(length(DT_TC)+1))
      DT_TC$nb = (DT_TC$Qry_nb*DT_TC$Bus_nb*DT_TC$Step_nb)
      
      Norm1 = (DT_TC$FailedTC)/sum(DT_TC$FailedTC)
      Norm2 = (DT_TC$TC_Fail_recurrence)/sum(DT_TC$TC_Fail_recurrence)
      Norm3 = (DT_TC$Qry_nb*DT_TC$Bus_nb*DT_TC$Step_nb) / sum(DT_TC$Qry_nb*DT_TC$Bus_nb*DT_TC$Step_nb)
      Norm4 = as.integer(DT_TC$NGRAM_Value)/sum(as.integer(DT_TC$NGRAM_Value))
      Norm5 = (DT_TC$Weight1)/sum(DT_TC$Weight1)
      
      DT_TC$Normalization <- (400*Norm1) + (300*Norm2) + (200*Norm3) + (100*Norm4)+(Norm5)
      
      DT_TC$Norm_Percentage <- paste(round(100*(DT_TC$Normalization/sum(DT_TC$Normalization)),2))
      DT_TC=arrange(DT_TC,desc(FailedTC),desc(TC_Fail_recurrence),desc(Normalization),desc(NGRAM_Value),desc(Weight1))
      write.csv(DT_TC, file="./RTS/RBT_Output.csv")
      DT_TC[,c(1,4,5,15,16,34)]
      
    }
    if(input$submit4 > 0 && !is.null(TestCaseload()) && !is.null(Defectdumpload())) {
      isolate({
        withProgress({
          setProgress(message = "Processing data please wait...") 
          RTS(x)
        })  
      })
    }
    
  }) 
  
  
  output$Navietable <- DT::renderDataTable({
    RbtData()
    
  }) 
  
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
