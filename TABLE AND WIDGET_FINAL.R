library(shiny)
library(tidyverse)
library(ggvis)
library(memoise)
library(tm)
library(wordcloud)
library(ggplot2)
library(tidytext)
library(shinythemes)
library(tibble)
library(plyr)
library(rsconnect)

rsconnect::setAccountInfo(name='visualanalyticsikea', token='661D4F7E18356180294E60814634F549', secret='eAw/U6z3a1KJ86VZO9hJZlv2xraC/FSO4GuPSRH0')

ikea <- read_csv('/cloud/project/IKEA_SA_Furniture_Web_Scrapings_sss.csv')

#Variables that can be used on the X axis for the 'Data Exploration'
axis_vars <- c(
  "Designer Name" = "designer",
  "Furniture Category" = "category"
)
getTermMatrix <- memoise(function(word) {
  
  wc_text <- readLines(file("/cloud/project/IKEA_SA_Furniture_Web_Scrapings_sss.csv"))
  Encoding(wc_text) <- "UTF-8"
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

ui <- fluidPage(
  titlePanel("IKEA Furniture Breakdown"),
  theme = shinytheme("readable"),
  setBackgroundColor("LemonChiffon"),
  
    tabsetPanel(
      #Code for the tab of the data table
      tabPanel("IKEA Table", 
               #      titlePanel("Ikea Furniture Breakdown"),
               fluidRow(
                 column(2,selectInput
                        ("cat", "Category:",
                          c("All",sort(unique(as.character(ikea$category)))))),
                 column(2,selectInput
                        ("pr", "Price:",
                          c("All",sort(unique(as.numeric(ikea$price)))))),
                 column(2,selectInput
                        ("hgt", "Height (cm):",
                          c("All",sort(unique(as.numeric(ikea$height)))))),
                 column(2,selectInput
                        ("wdt", "Width (cm):",
                          c("All",sort(unique(as.numeric(ikea$width)))))),
                 column(2,selectInput
                        ("dpt", "Depth (cm):",
                          c("All",sort(unique(as.numeric(ikea$depth)))))),
                 img(src="https://www.wearecatalyst.co.uk/hs-fs/hubfs/ikea-flag.gif?width=400&name=ikea-flag.gif", 
                     align = "center",height='150px',width='300px'),hr()),DT::dataTableOutput("table")
               
      ),
      #Code for the tab of the data exploration
      tabPanel("IKEA Data Explorer",titlePanel("Exploring the IKEA Catalog by Size"), 
               sidebarLayout( 
                 sidebarPanel(selectInput("xvar", "X-axis variable", axis_vars, selected = "Category")),
                 plotOutput("plot1"))),
    #Code for the Word Cloud
    tabPanel("IKEA Word Cloud for Designers",titlePanel("A Word Cloud of IKEA's Designers"), 
             sidebarLayout( 
               sidebarPanel(),
               mainPanel(plotOutput(
                 "cloud", width = "600px", height = "600px"))
               )
             ),
    tabPanel("IKEA Word Cloud for Furniture",titlePanel("A Word Cloud of IKEA's Furniture Sold"), 
             sidebarLayout( 
               sidebarPanel(),
               mainPanel(plotOutput(
                 "cloud2", width = "600px", height = "600px"))
             )
    )
    )
)

                
 
 
server <- function(input, output,session) {
  output$table <- DT::renderDataTable(DT::datatable({
    data <- ikea
    if (input$cat !="All") {
      data <- data[data$category == input$cat,]
    }
    if (input$pr !="All") {
      data <- data[data$price == input$pr,]
    }
    if (input$hgt !="All") {
      data <- data[data$height == input$hgt,]
    }
    if (input$wdt !="All") {
      data <- data[data$width == input$wdt,]
    }
    data}))
  
  # A reactive expression with the ggvis plot
  
  test_data <- reactive({

    if("Designer Name" %in% input$xvar) return(ikea$designer)
    if("Furniture Category" %in% input$xvar) return(ikea$category)

    
    
  })
  output$plot1 <- renderPlot({ 
    data <- test_data()
    
    ggplot(ikea, aes_string(x=input$xvar, y = ikea$price,color=input$xvar,xlab=input$xvar),show.legend = FALSE)+geom_bar(position="stack",stat='identity')+theme(legend.position = "none")
  })

  
  # Define a reactive expression for the document term matrix
  wc_data <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        wc_text <- readLines(file("/cloud/project/Designer.txt"))
        Encoding(wc_text)  <- "UTF-8"
        wc_corpus <- Corpus(VectorSource(wc_text))
        wc_corpus <- tm_map(wc_corpus, tolower)
        wc_corpus <- tm_map(wc_corpus, removeNumbers)
        wc_corpus <- tm_map(wc_corpus, stripWhitespace)
        dtm <- TermDocumentMatrix(wc_corpus)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
      })
    })
  })
  
  wc_data2 <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        wc_text <- readLines(file("/cloud/project/Category.txt"))
        Encoding(wc_text)  <- "UTF-8"
        wc_corpus <- Corpus(VectorSource(wc_text))
        wc_corpus <- tm_map(wc_corpus, tolower)
        wc_corpus <- tm_map(wc_corpus, removeNumbers)
        wc_corpus <- tm_map(wc_corpus, stripWhitespace)
        wc_corpus <- tm_map(wc_corpus, removeWords, c("furniture")) 
        dtm <- TermDocumentMatrix(wc_corpus)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloud <- renderPlot({
    withProgress({
      setProgress(message = "Creating wordcloud...")
      wc_corpus <- wc_data()
      wordcloud(words = wc_corpus$word, freq =wc_corpus$freq, min.freq = 1,
                max.words=1000, random.order=FALSE, rot.per=0.4, 
                colors=brewer.pal(8, "Set2"))
    })
  })
  
  output$cloud2 <- renderPlot({
    withProgress({
      setProgress(message = "Creating wordcloud...")
      wc_corpus <- wc_data2()
      wordcloud(words = wc_corpus$word, freq =wc_corpus$freq, min.freq = 1,
                max.words=50, random.order=FALSE, rot.per=0.4, 
                colors=brewer.pal(8, "Set2"))
    })
  })
  }
  

  shinyApp(ui = ui, server = server)