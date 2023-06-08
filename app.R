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
source("./utils.R")

# Define UI for application that samples from joint posterior of EQ5D and EQVAS with or without X var
ui <- dashboardPage(
  dashboardHeader(title = "EQBayes"),
  
  sidebar,
  
  dashboardBody(
    tabItems(
      
      # About
      tab_about,

      # Upload
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            filein,
            uiOutput("var_ui1"),
            uiOutput("var_ui2"),
            uiOutput("var_ui3"),
            width = 6,
            height = 350),
            
            filein_details
        ),
        fluidRow(box(tableOutput("contents"), width = 12))
      ),

      # Explore
      tabItem(
        tabName = "explore",
        fluidRow(
          box(
            plotOutput("eq5dhist"),
            width = 6
            ),
          box(
            plotOutput("eqvashist"),
            width = 6
            ),
          box(
            plotOutput("eqscatter"),
            width = 12
          )
          )
      ),
      

      # Analyse
      tabItem(
        tabName = "analyse",
        fluidRow(
          analyseinputs, #inputs for MCMC
          
          #Put boxes of results/halfeye plots here, in two tabs
          #Put console underneath
          
          box(
            tabsetPanel(type = "tabs",
                      tabPanel("Intercept only model", box(
                        title = "Results, intercept only models",
                        plotOutput("posteriors1"),
                        hr(),
                        tableOutput("posteriors1table"),
                        width = 9)),
                      tabPanel("Model with X variable", box(
                        title = "Results, with X variable",
                        plotOutput("posteriors2"),
                        hr(),
                        tableOutput("posteriors2table"),
                        width = 9)
                      )
            ),
            width = 9
          )
        )
      ),
      
      tabItem(
        tabName = "diagnose",
        tabsetPanel(type = "tabs",
                    tabPanel("Intercept only model"),
                    tabPanel("Model with X variable"))
      )
    )
  )
)





# Define server logic
server <- function(input, output, session) {
  # File Upload
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
    hist(x,
         breaks = "FD", 
         col = "#75AADB", 
         border = "white",
         xlab = "EQ5D utility values",
         xlim = c(-1, 1),
         main = "Histogram of EQ5D utility scores"
    )
  })

  output$eqvashist <- renderPlot({
    x <- na.omit(as.numeric(df()[, input$vareqvas]))
    hist(x,
         breaks = "FD", 
         col = "#75AADB", 
         border = "white",
         xlab = "EQVAS values",
         xlim = c(0, 100),
         main = "Histogram of EQVAS scores"
    )
  })
  
  output$eqscatter <- renderPlot({
    ggplot(data = df(),
           aes(x = df()[,input$vareq5d], 
               y = as.numeric(df()[,input$vareqvas]))) +
      geom_point() + 
      geom_rug(alpha = 0.5, 
               position = "jitter") +
      labs(x = "EQ5D scores", 
           y = "EQVAS scores (unscaled)",
           title = "EQ5D questions vs EQVAS scores (missing rows omitted)")
  })


  # Analysis: run nimble models
  
  output$posteriors1 <- renderPlot(NULL)
  output$posteriors2 <- renderPlot(NULL)
  
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
          nimble1 %>%
            ggplot(aes(x = Estimate, y = Method)) +
            stat_halfeye(.width = c(0.95, 0.5))
          })
        
        
        output$posteriors1table <- renderTable({
          nimble1 %>%
            group_by(Method) %>%
            summarise(Mean = mean(Estimate),
                      SD = sd(Estimate),
                      Lower = quantile(Estimate, 0.025),
                      Upper = quantile(Estimate, 0.975))
          })
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
      nimble2 %>%
        ggplot(aes(x = Estimate, y = Method)) +
        facet_wrap(~xvar) +
        stat_halfeye(.width = c(0.95, 0.5))
    })
    
    output$posteriors2table <- renderTable({
      nimble2 %>%
        group_by(Method, xvar) %>%
        summarise(Mean = mean(Estimate),
                  SD = sd(Estimate),
                  Lower = quantile(Estimate, 0.025),
                  Upper = quantile(Estimate, 0.975))
    })
    
    
  })
    
}
     

# Run the application
shinyApp(ui = ui, server = server)
