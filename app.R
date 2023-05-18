library(shiny)
library(shinydashboard)
library(nimble)
options(shiny.error = browser)

# Define UI for application that samples from joint posterior of EQ5D and EQVAS with or without sex
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
      box(title = "EQBayes",
          p("Welcome to EQBayes, an RShiny app for sampling from the joint posterior of the EQ5D utility score and the EQVAS.", br(),
            "This app is based on the paper by ",
            a("Blythe et al (2022).", href = "https://doi.org/10.1016/j.jval.2022.01.017"), br(),
            "This app requires you to upload a CSV file containing at least two columns:", br(),
            "EQ5D utility scores, either from the 3L or 5L versions, and EQVAS scores.",
            "EQ5D utilities should be numeric values no greater than 1, and EQVAS scores should range from 0 to 100.",
            "Sex can also be added as a binary variable (0/1).", br(),
            "The app will automatically:", br(),
            "- Drop missing values for analysis", br(),
            "- Divide the VAS by 100 to scale it to utility", br(),
            "To get started, click the drop down menu at the top left to open the sidebar, and load in some data!",
            hr(),
            "The underlying software uses the nimble package, a fast, intuitive C++ compiling program for Markov Chain Monte Carlo (MCMC).",
            "The model assumes flat priors of Beta(1,1) and Normal(0,10000) for utility and sex variables, respectively.",
            "If you want to specify your own priors, this app may not be useful for you.",
            "Check out the r-nimble.org examples to get coding on your own models.",
            "For any questions, comments, or bug reports, please email: robin.blythe@qut.edu.au"))),
      
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
            uiOutput("var_ui3"),
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
              ),
      
      #Analyse
      tabItem(tabName = "analyse",
              fluidRow(box(
                numericInput(inputId = "MCMC",
                             label = "Number of posterior draws per chain",
                             value = 5000,
                             min = 0,
                             max = 10000),
                numericInput(inputId = "n.chains",
                             label = "Number of chains",
                             value = 2,
                             min = 1,
                             max = 6),
                numericInput(inputId = "thin",
                             label = "How much to thin out each chain",
                             value = 0,
                             min = 0,
                             max = 6),
                numericInput(inputId = "burnin",
                             label = "Number of iterations to burn for warmup",
                             value = 500,
                             min = 0,
                             max = 5000),
                numericInput(inputId = "seed",
                             label = "Set random seed",
                             value = as.integer(runif(1,1,.Machine$integer.max))),
                actionButton("runwithoutx",
                             label = "Run model (intercept only)"),
                actionButton("runwithx",
                             label = "Run model (by sex)"))),
              
              fluidRow(box(
                plotOutput("traceplot")
              ))
              
              
              )
      )
    )
)




#Define server logic
server <- function(input, output, session) {
  
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
  output$var_ui3 <- renderUI({
    selectInput("sex", "Select binary (0/1) sex variable:", choices = c("None", names(df())))
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
         xlab = "EQVAS values",
         xlim = c(0,100),
         main = "Histogram of EQVAS scores")
  })

  
  #Analysis: run nimble models
  
  source("./run_nimble_intercept_only.R")
  source("./run_nimble_with_sex.R")
  
  observeEvent(input$runwithoutx,{
    run_nimble_intercept_only(vareq5d = input$vareq5d,
                              vareqvas = input$vareqvas,
                              MCMC = input$MCMC,
                              n.chains = input$n.chains,
                              thin = input$thin,
                              burnin = input$burnin,
                              seed = input$seed)
  })
  
  observeEvent(input$runwitx,{
    run_nimble_intercept_only(vareq5d = input$vareq5d,
                              vareqvas = input$vareqvas,
                              sex = input$sex,
                              MCMC = input$MCMC,
                              n.chains = input$n.chains,
                              thin = input$thin,
                              burnin = input$burnin,
                              seed = input$seed)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
