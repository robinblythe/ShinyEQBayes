library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(nimble)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(here)

source(file.path(here(), "R", "run_nimble_intercept_only.R"))
source(file.path(here(), "R", "run_nimble_with_x.R"))
source(file.path(here(), "R", "ui-content.R"))

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
            height = 350
          ),
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
          analyseinputs, # inputs for MCMC

          box(
            tabsetPanel(
              type = "tabs",
              tabPanel("Intercept only model", box(
                title = "Results, intercept only models",
                plotOutput("posteriors1") %>% withSpinner(color = "#75AADB"),
                hr(),
                tableOutput("posteriors1table"),
                width = 9
              )),
              tabPanel("Model with X variable", box(
                title = "Results, with X variable",
                plotOutput("posteriors2") %>% withSpinner(color = "#75AADB"),
                hr(),
                tableOutput("posteriors2table"),
                width = 9
              ))
            ),
            width = 9
          )
        )
      ),
      
      
      #Diagnose - trace plots, densities
      tabItem(
        tabName = "diagnose",
        tabsetPanel(
          type = "tabs",
          tabPanel("Intercept only model",
                   box(
                     title = "Trace plots, beta[1] parameter",
                     plotOutput("traceplot1_beta1") %>% withSpinner(color = "#75AADB"),
                     width = 12
                   ),
                   box(
                     title = "Trace plots, beta[2] parameter (joint model only)",
                     plotOutput("traceplot1_beta2") %>% withSpinner(color = "#75AADB"),
                     width = 12
                   )),
          tabPanel("Model with X variable",
                   box(
                     title = "Trace plots, beta[1] parameter",
                     plotOutput("traceplot2_beta1") %>% withSpinner(color = "#75AADB"),
                     width = 12
                   ),
                   box(
                     title = "Trace plots, beta[2] parameter (xvar)",
                     plotOutput("traceplot2_beta2") %>% withSpinner(color = "#75AADB"),
                     width = 12
                   ),
                   box(
                     title = "Trace plots, beta[3] parameter (joint model only)",
                     plotOutput("traceplot2_beta3") %>% withSpinner(color = "#75AADB"),
                     width = 12
                   ))
        )
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
           aes(x = df()[, input$vareq5d], y = as.numeric(df()[, input$vareqvas]))) +
      geom_point() +
      geom_rug(alpha = 0.5, position = "jitter") +
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
        summarise(
          Mean = mean(Estimate),
          SD = sd(Estimate),
          Lower = quantile(Estimate, 0.025),
          Upper = quantile(Estimate, 0.975)
        )
    })
    
    output$traceplot1_beta1 <- renderPlot({
      nimble1 %>%
        group_by(Chain, Method) %>%
        mutate(Iteration = row_number()) %>%
        ungroup() %>%
        ggplot(aes(x = Iteration, y = `beta[1]`)) +
        geom_line() +
        facet_wrap(~Chain+Method, labeller = label_wrap_gen(multi_line=FALSE))
      })
    
    output$traceplot1_beta2 <- renderPlot({
      na.omit(nimble1) %>%
        group_by(Chain, Method) %>%
        mutate(Iteration = row_number()) %>%
        ungroup() %>%
        ggplot(aes(x = Iteration, y = `beta[2]`)) +
        geom_line() +
        facet_wrap(~Chain+Method, labeller = label_wrap_gen(multi_line=FALSE))
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
        summarise(
          Mean = mean(Estimate),
          SD = sd(Estimate),
          Lower = quantile(Estimate, 0.025),
          Upper = quantile(Estimate, 0.975)
        )
    })
    
    output$traceplot2_beta1 <- renderPlot({
      nimble2 %>%
        group_by(Chain, Method) %>%
        mutate(Iteration = row_number()) %>%
        ungroup() %>%
        ggplot(aes(x = Iteration, y = `beta[1]`)) +
        geom_line() +
        facet_wrap(~Chain+Method, labeller = label_wrap_gen(multi_line=FALSE))
    })
    
    output$traceplot2_beta2 <- renderPlot({
      na.omit(nimble2[,c(2, 3, 6)]) %>%
        group_by(Chain, Method) %>%
        mutate(Iteration = row_number()) %>%
        ungroup() %>%
        ggplot(aes(x = Iteration, y = `beta[2]`)) +
        geom_line() +
        facet_wrap(~Chain+Method, labeller = label_wrap_gen(multi_line=FALSE))
    })
    
    output$traceplot2_beta3 <- renderPlot({
      na.omit(nimble2[,c(2, 3, 7)]) %>%
        group_by(Chain, Method) %>%
        mutate(Iteration = row_number()) %>%
        ungroup() %>%
        ggplot(aes(x = Iteration, y = `beta[3]`)) +
        geom_line() +
        facet_wrap(~Chain+Method, labeller = label_wrap_gen(multi_line=FALSE))
    })
  })
}


# Run the application
shinyApp(ui = ui, server = server)
