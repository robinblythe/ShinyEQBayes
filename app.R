library(shiny)
library(shinydashboard)

# Define UI for application that samples from joint posterior of EQ5D and EQVAS
ui <- dashboardPage(
  
  dashboardHeader(title = "EQBayes"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("home"), selected = TRUE),
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Explore", tabName = "explore", icon = icon("globe")),
      menuItem("Analyse", tabName = "analyse", icon = icon("magnifying-glass-chart"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #About
      tabItem(tabName = "about",
      box(p("Welcome to EQBayes, an RShiny app for sampling from the joint posterior of the EQ5D utility score and the EQVAS.",
            "This app is based on the paper by ",
            a("Blythe et al (2020).", href = "https://doi.org/10.1016/j.jval.2022.01.017")))),
      
      #Upload
      tabItem(tabName = "upload",
      fluidRow(
        box(fileInput("file1", "Upload a CSV file of EQ-5D and EQVAS scores",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            uiOutput("var_ui1"),
            uiOutput("var_ui2"),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t")),
            radioButtons("disp", "Display",
                         choices = c(Head = "Head",
                                     All = "All"),
                         selected = "Head"))),
      
      fluidRow(box(tableOutput("contents")))
      ),
      
      #Explore
      tabItem(tabName = "explore",
              fluidRow(box(
                plotOutput("eq5dhist"),
                sliderInput(inputId = "eq5dbins",
                            label = "Number of bins",
                            min = 1,
                            max = 50,
                            value = 25))),
              
              fluidRow(box(
                plotOutput("eqvashist"),
                sliderInput(inputId = "eqvasbins",
                            label = "Number of bins",
                            min = 1,
                            max = 50,
                            value = 25)))
              )
      )
    )
)




#Define server logic
server <- function(input, output) {
  
  #Upload
  df <- reactive({
    file1 <- input$file1
    if(is.null(file1)){
      return()
    }
    
  df = read.csv(file=file1$datapath,
                header = input$header,
                sep = input$sep)
  df
  })
  
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep)
    
    if(input$disp == "Head") {
      return(head(df))
    }
    else{
      return(df)
    }
  })
  
  #Select columns to analyse
  output$var_ui1 <- renderUI({
    selectInput("vareq5d", "Select EQ5D utility column:", choices = names(df()))
  })
  output$var_ui2 <- renderUI({
    selectInput("vareqvas", "Select EQVAS column:", choices = names(df()))
  })
  
  #Plot/explore
  output$eq5dhist <- renderPlot({
    x <- na.omit(as.numeric(df()[,input$vareq5d]))
    bins <- seq(min(x), max(x), length.out = input$eq5dbins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "EQ5D utility values",
         xlim = c(-1,1),
         main = "Histogram of EQ5D utility scores")
  })
  
  output$eqvashist <- renderPlot({
    x <- na.omit(as.numeric(df()[,input$vareqvas]))
    bins <- seq(min(x), max(x), length.out = input$eqvasbins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "EQVAS utility values",
         xlim = c(0,100),
         main = "Histogram of EQVAS scores")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
