library(shiny)
library(shinydashboard)
library(nimble)
library(tidybayes)
library(data.table)
library(dplyr)
library(ggplot2)

source("./run_nimble_intercept_only.R")
source("./run_nimble_with_x.R")
source("./constants.R")

# Define UI for application that samples from joint posterior of EQ5D and EQVAS with or without X var
ui <- dashboardPage(
  dashboardHeader(title = "EQBayes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("home"), selected = TRUE),
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Explore", tabName = "explore", icon = icon("globe")),
      menuItem("Analyse", tabName = "analyse", icon = icon("magnifying-glass-chart")))
  ),
  
  dashboardBody(
    tabItems(
      
      # About
      tab_about,

      # Upload
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            fileInput("file1", "Upload a CSV file of EQ-5D and EQVAS scores",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
            ),
            
            uiOutput("var_ui1"),
            uiOutput("var_ui2"),
            uiOutput("var_ui3"),
            width = 6,
            height = 350),
            
            box(
              title = "File upload details",
              
              checkboxInput("header", "Header", TRUE),
              
              radioButtons("sep", "Separator",
                choices = c(
                  Comma = ",",
                  Semicolon = ";",
                  Tab = "\t"
                  )),
              
              radioButtons("disp", "Display",
                choices = c(
                  Head = "Head",
                  All = "All"
                  ),
                selected = "Head"
            ),
            width = 6,
            height = 350
          )
        ),
        fluidRow(box(tableOutput("contents"), width = 12))
      ),

      # Explore
      #Consider scatterplot using x = EQ5D, y = VAS, density plot in the margins (ggside)
      tabItem(
        tabName = "explore",
        fluidRow(
          box(
            plotOutput("eq5dhist"),
            sliderInput(
              inputId = "eq5dbins",
              label = "Number of bins",
              min = 1,
              max = 50,
              value = 25
              ),
            width = 6
            ),
          box(
            plotOutput("eqvashist"),
            sliderInput(
              inputId = "eqvasbins",
              label = "Number of bins",
              min = 1,
              max = 50,
              value = 25
              ),
            width = 6
            )
          )
      ),
      

      # Analyse
      tabItem(
        tabName = "analyse",
        fluidRow(box(
          title = "Bayesian model inputs",
          numericInput(
            inputId = "MCMC",
            label = "Number of posterior draws per chain",
            value = 5000,
            min = 1000,
            max = 10000
          ),
          numericInput(
            inputId = "n.chains",
            label = "Number of chains",
            value = 2,
            min = 1,
            max = 3
          ),
          numericInput(
            inputId = "thin",
            label = "How much to thin out each chain",
            value = 3,
            min = 0,
            max = 6
          ),
          numericInput(
            inputId = "burnin",
            label = "Number of iterations to burn for warmup",
            value = 1000,
            min = 0,
            max = 5000
          ),
          numericInput(
            inputId = "seed",
            label = "Set random seed",
            value = as.integer(runif(1, 1, .Machine$integer.max))
          ),
          
          hr(),
          actionButton("runwithoutx",
            label = "Run model (intercept only)"
          ),
          hr(),
          actionButton("runwithx",
            label = "Run model (with X variable)"
          ),
          width = 3),
          
          #Put boxes of results/halfeye plots here, in two tabs
          #Put console underneath
          
          #Instead of boxes with the trace plots, just put a two column panel. Put this below
          #Tab 1 - intercept only; Tab 2 - with X Var
          #Column 1 - all trace plots
          #Column 2 - all density plots
          
          box(
            title = "Trace plots, intercept only",
            uiOutput("chainselect1"),
            uiOutput("parameterselect1"),
            width = 9),
          
          box(
            title = "Trace plots, with X variable",
            uiOutput("chainselect2"),
            uiOutput("parameterselect2"),
            width = 9)
          ),
        
        
          
        fluidRow(
          box(
            title = "Results, intercept only models",
            plotOutput("posteriors1"),
            hr(),
            tableOutput("posteriors1table"),
            width = 6),
          
          box(
            title = "Results, with X variable",
            plotOutput("posteriors2"),
            hr(),
            tableOutput("posteriors2table"),
            width = 6)
        )
      )
    )
  )
)




# Define server logic
server <- function(input, output, session) {
  # Upload
  df <- reactive({
    file1 <- input$file1
    if (is.null(file1)) {
      return()
    }

    df <- read.csv(
      file = file1$datapath,
      header = input$header,
      sep = input$sep
    )
    df
  })

  output$contents <- renderTable({
    df <- filedata()

    if (input$disp == "Head") {
      return(head(df))
    } else {
      return(df)
    }
  })

  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep
    )
  })

  # Select columns to analyse
  output$var_ui1 <- renderUI({
    selectInput("vareq5d", "Select EQ5D utility column:", choices = names(df()))
  })
  output$var_ui2 <- renderUI({
    selectInput("vareqvas", "Select EQVAS column:", choices = names(df()))
  })
  output$var_ui3 <- renderUI({
    selectInput("xvar", "Select binary (0/1) variable:", choices = c("None", names(df())))
  })

  # Plot/explore
  output$eq5dhist <- renderPlot({
    x <- na.omit(as.numeric(df()[, input$vareq5d]))
    bins <- seq(min(x), max(x), length.out = input$eq5dbins + 1)
    hist(x,
      breaks = bins, col = "#75AADB", border = "white",
      xlab = "EQ5D utility values",
      xlim = c(-1, 1),
      main = "Histogram of EQ5D utility scores"
    )
  })

  output$eqvashist <- renderPlot({
    x <- na.omit(as.numeric(df()[, input$vareqvas]))
    bins <- seq(min(x), max(x), length.out = input$eqvasbins + 1)
    hist(x,
      breaks = bins, col = "#75AADB", border = "white",
      xlab = "EQVAS values",
      xlim = c(0, 100),
      main = "Histogram of EQVAS scores"
    )
  })


  # Analysis: run nimble models
  
  output$posteriors1 <- renderPlot(NULL)
  output$posteriors2 <- renderPlot(NULL)
  output$chainselect1 <- renderUI({
    selectInput("select_chain_1",
                "Run intercept only model to proceed",
                choices = NULL,
    )
  })
  output$parameterselect1 <- renderUI({
    selectInput("select_parameter_1",
                "Run intercept only model to proceed",
                choices = NULL,
    )
  })
  output$chainselect2 <- renderUI({
    selectInput("select_chain_2",
                "Run model with x variable to proceed",
                choices = NULL,
    )
  })
  output$parameterselect2 <- renderUI({
    selectInput("select_parameter_2",
                "Run model with x variable to proceed",
                choices = NULL,
    )
  })

  observeEvent(input$runwithoutx, {
    isolate({
      df <- filedata()
      if (is.null(df)) {
        return(NULL)
      }
    })
    
    nimble1 <- run_nimble_intercept_only(
      data = df(),
      vareq5d = input$vareq5d,
      vareqvas = input$vareqvas,
      MCMC = input$MCMC,
      n.chains = input$n.chains,
      thin = input$thin,
      burnin = input$burnin,
      seed = input$seed
    )
    
    output$posteriors1 <- renderPlot({
      nimble1[[1]] %>%
        ggplot(aes(x = Estimate, y = Method)) +
        stat_halfeye(.width = c(0.95, 0.5)) #+
      #add a qnorm plot with these params - mean(posterior draws) and sd (posterior draws)
      })
    
    #Add SD to the table
    output$posteriors1table <- renderTable({
      nimble1[[1]] %>%
        group_by(Method) %>%
        summarise(Mean = mean(Estimate),
                  Lower = quantile(Estimate, 0.025),
                  Upper = quantile(Estimate, 0.975))
    })
    
    updateSelectInput(inputId = "select_chain_1",
                      label = "Select chain to inspect",
                      choices = names(nimble1[[2]])
                      )
    
  })

  observeEvent(input$runwithx, {
    isolate({
      df <- filedata()
      if (is.null(df)) {
        return(NULL)
      }
    })
    
    nimble2 <- run_nimble_with_x(
      data = df(),
      vareq5d = input$vareq5d,
      vareqvas = input$vareqvas,
      xvar = input$xvar,
      MCMC = input$MCMC,
      n.chains = input$n.chains,
      thin = input$thin,
      burnin = input$burnin,
      seed = input$seed
    )

    output$posteriors2 <- renderPlot({
      nimble2[[1]] %>%
        ggplot(aes(x = Estimate, y = Method)) +
        facet_wrap(~xvar) +
        stat_halfeye(.width = c(0.95, 0.5))
    })
    
    #Add SD to the table
    output$posteriors2table <- renderTable({
      nimble2[[1]] %>%
        group_by(Method, xvar) %>%
        summarise(Mean = mean(Estimate),
                  Lower = quantile(Estimate, 0.025),
                  Upper = quantile(Estimate, 0.975))
    })
    
    output$chainselect2 <- renderUI({
      selectInput("select_chain_2",
                  "Select chain to inspect",
                  choices = names(nimble2[[2]])
                  )
    })
    })
  
  eventReactive(input$select_chain_1,{
    updateSelectInput(inputId = "select_parameter_1",
                      label = "Select parameter to inspect",
                      choices = colnames(nimble1[[2]][[input$select_chain_1]]))
  })
  
  eventReactive(input$select_chain_2,{
    updateSelectInput(inputId = "select_parameter_2",
                      label = "Select parameter to inspect",
                      choices = colnames(nimble2[[2]][[input$select_chain_2]]))
  })
  

  
  
  
    
}
     

# Run the application
shinyApp(ui = ui, server = server)
