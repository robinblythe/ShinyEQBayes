library(shiny)
library(shinydashboard)
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
          p("Welcome to EQBayes, an RShiny app for sampling from the joint posterior of the EQ5D utility score and the EQVAS.",
            "This app is based on the paper by ",
            a("Blythe et al (2022).", href = "https://doi.org/10.1016/j.jval.2022.01.017"),
            "To get started, upload a CSV file containing at least two columns:",
            "EQ5D utility scores, either from the 3L or 5L versions, and EQVAS scores.",
            "EQ5D utilities should be numeric values no greater than 1, and EQVAS scores should range from 0 to 100.",
            "Sex can also be added as a binary variable (0/1).",
            "The app will automatically drop missing values for analysis."))),
      
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
                             label = "Set random seed?",
                             value = 88888),
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

  #Decide which regression to run based on action button input
  nimble0 <- reactiveValues()

  #Regression - no independent variables
  eventReactive(input$runwithoutx, {
    reactive({
      req(input$vareq5d)
      req(input$vareqvas)
    
      eq5dutility <- as.numeric(df()[,input$vareqvas])
      eq5dvas <- as.numeric(df()[,input$vareqvas/100])
      Y = na.omit(as.matrix(eq5dutility, eq5dvas))
      R = matrix(data = c(1,0,0,1), nrow = 2)
      N = nrow(Y)
      bvnorm1 <- list(N = N, Y = Y, R = R)
    
      eq5dnox <- nimbleCode({
        #Model 
        for (i in 1:N) {
          Y[i,1:2] ~ dmnorm(mean = mu[i,1:2], prec = Omega[1:2,1:2])
          mu[i,1] <- beta[1]
          mu[i,2] <- beta[1] + beta[2]
        }
        beta[1] ~ dbeta(1,1)
        beta[2] ~ dbeta(1,1)
        Omega[1:2,1:2] ~ dwish(R[1:2,1:2], 2)
        sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
      })
    
      inits1 <- list(beta = c(mean(df()[,input$vareq5d]), mean(df()[,input$vareqvas])), Omega = R)
      inits1 = rep(list(inits), input$n.chains) # repeat initial values per chain
    
      nimblereg1 <- nimbleMCMC(code = eq5dnox, 
                              data = bvnorm1,
                              inits = inits1, 
                              nchains = input$n.chains,
                              nburnin = input$burnin,
                              niter = ifelse(input$MCMC*input$n.chains*input$thin == 0,
                                             input$MCMC*input$n.chains + input$burnin,
                                             input$MCMC*input$n.chains*input$thin + input$burnin),
                              setSeed = input$seed)
    })
  })
  
  #Regression - sex variable included
  eventReactive(input$runwithx, {
    reactive({
    req(input$vareq5d)
    req(input$vareqvas)
    req(input$sex)
    
    eq5dutility <- as.numeric(df()[,input$vareqvas])
    eq5dvas <- as.numeric(df()[,input$vareqvas/100])
    sex <- as.numeric(df()[,input$sex])
    Y = na.omit(as.matrix(eq5dutility, eq5dvas))
    R = matrix(data = c(1,0,0,1), nrow = 2)
    N = nrow(Y)
    bvnorm2 <- list(N = N, Y = Y, R = R, sex = sex)
    
    eq5dyesx <- nimbleCode({
      #Model 
      for (i in 1:N) {
        Y[i,1:2] ~ dmnorm(mean = mu[i,1:2], prec = Omega[1:2,1:2])
        mu[i,1] <- beta[1] + beta[3]*sex[i]
        mu[i,2] <- beta[1] + beta[2] + beta[3]*sex[i]
      }
      beta[1] ~ dbeta(1,1)
      beta[2] ~ dbeta(1,1)
      beta[3] ~ dnorm(0,10^5)
      Omega[1:2,1:2] ~ dwish(R[1:2,1:2], 2)
      sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
    })
    
    inits2 <- list(beta = c(mean(df()[,input$vareq5d]), mean(df()[,input$vareqvas]), 0), Omega = R)
    inits2 = rep(list(inits), input$n.chains) # repeat initial values per chain
    
    nimblereg2 <- nimbleMCMC(code = eq5dyesx, 
                             data = bvnorm2,
                             inits = inits2, 
                             nchains = input$n.chains,
                             nburnin = input$burnin,
                             niter = ifelse(input$MCMC*input$n.chains*input$thin == 0,
                                            input$MCMC*input$n.chains + input$burnin,
                                            input$MCMC*input$n.chains*input$thin + input$burnin),
                             setSeed = input$seed)
    })
  })
  
  output$traceplot <- renderPlot({
    plot(nimblereg1[,'beta[1]'], type = "l")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
