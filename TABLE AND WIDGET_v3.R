library(shiny)
library(tidyverse)
library(ggvis)
library(memoise)

ikea <- read_csv('/cloud/project/data/IKEA_SA_Furniture_Web_Scrapings_sss.csv')

# Variables that can be put on the x and y axes
axis_vars <- c(
  "Designer Name" = "designer",
  "Furniture Category" = "category",
  "Price" = "price",
  "Height (cm)" = "height",
  "Width (cm)" = "width",
  "Depth (cm)" = "depth"
)

getTermMatrix <- memoise(function(words) {
  
  text <- readLines(sprintf(
    "/cloud/project/data/IKEA_SA_Furniture_Web_Scrapings_sss.csv", words), 
    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

ui <- fluidPage(
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      h3("IKEA Explorer"),
      selectInput("xvar", "X-axis variable", axis_vars, selected = "Category"),
      selectInput("yvar", "Y-axis variable", axis_vars, selected = "Price"),
      h3("IKEA Word Cloud"), 
      radioButtons("words", "Input type:",
                 c("Category" = "cat",
                   "Designer" = "design"))),
  mainPanel(
    # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    tabsetPanel(
      id = "switcher",
      tabPanel("IKEA Table", 
#      titlePanel("Ikea Furniture Breakdown"),
      fluidRow(
         column(4,selectInput
           ("cat", "Category:",
             c("All",sort(unique(as.character(ikea$category)))))),
        column(4,selectInput
           ("pr", "Price:",
             c("All",sort(unique(as.numeric(ikea$price)))))),
        column(4,selectInput
           ("hgt", "Height (cm):",
             c("All",sort(unique(as.numeric(ikea$height)))))),
        column(4,selectInput
           ("wdt", "Width (cm):",
             c("All",sort(unique(as.numeric(ikea$width)))))),
    ),DT::dataTableOutput("table")), 
      tabPanel("Explorer", ggvisOutput("plot1")),
      tabPanel("Word Cloud", plotOutput(
        "cloud", width = "100%", height = "600px"))
  ))))

  
server <- function(input, output) {
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
      output$plot1 <- renderPlot({ 
      vis <- reactive({
        # Lables for axes
        xvar_name <- names(axis_vars)[axis_vars == input$xvar]
        yvar_name <- names(axis_vars)[axis_vars == input$yvar]
        
        # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
        # but since the inputs are strings, we need to do a little more work.
        xvar <- prop("x", as.symbol(input$xvar))
        yvar <- prop("y", as.symbol(input$yvar))
        
        ikea %>%
          ggvis(x = xvar, y = yvar) %>%
          layer_points(size := 50, size.hover := 200,
                       fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
     #     add_tooltip(movie_tooltip, "hover") %>%
          add_axis("x", title = xvar_name) %>%
          add_axis("y", title = yvar_name) %>%
          set_options(width = 500, height = 500)
      })
      
      vis %>% bind_shiny("plot1")
      })
  }
  

  shinyApp(ui = ui, server = server)