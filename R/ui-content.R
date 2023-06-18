# ui-content.R contains UI elements to avoid clutter in app.R
library(shiny)
library(shinydashboard)

tab_about <- tabItem(
  tabName = "about",
  title = "EQBayes",
  p(
    "Welcome to EQBayes, an RShiny app for sampling from the joint posterior",
    "of the EQ5D utility score and the EQVAS.", br(),
    "This app is based on the paper by ",
    a("Blythe et al (2022).", href = "https://doi.org/10.1016/j.jval.2022.01.017"),
    hr(),
    "This app requires you to upload a CSV file containing at least two columns:", br(),
    "EQ5D utility scores, either from the 3L or 5L versions, and EQVAS scores.",
    "EQ5D utilities should be numeric values no greater than 1, and EQVAS scores should range from 0 to 100.",
    "A binary variable (0/1) such as sex or treatment effect can also be used to analyse the data.", br(),
    "The app will automatically:", br(),
    "- Drop missing values for analysis", br(),
    "- Divide the VAS by 100 to scale it to utility", br(),
    "To get started, click the drop down menu at the top left to open the sidebar, and load in some data!", br(),
    "NOTE: Data is not retained by the app. However, if you are concerned about confidentiality,",
    " you can download the app from",
    a("the github page", href = "https://github.com/robinblythe/ShinyEQBayes"),
    "and run it in your local environment.",
    hr(),
    "The underlying software uses the nimble package, a fast, intuitive C++ compiling program",
    "for Markov Chain Monte Carlo (MCMC).",
    "The model assumes flat priors of Beta(1,1) and Normal(0,10000)",
    "for utility and binary variables, respectively.",
    "If you know that you want to specify your own priors,",
    "you may be better off specifying your own model as per the paper linked above.",
    "Check out the r-nimble.org examples to get started on your own models.",
    "If you require an introduction to Bayesian methods, Andrew Gelman and colleagues have a",
    a("free online textbook.", href = "http://www.stat.columbia.edu/~gelman/book/"),
    hr(),
    "For any questions, comments, or bug reports, please email:",
    a("robin.blythe@qut.edu.au", href = "mailto:robin.blythe@qut.edu.au")
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("home"), selected = TRUE),
    menuItem("Upload", tabName = "upload", icon = icon("upload")),
    menuItem("Explore", tabName = "explore", icon = icon("globe")),
    menuItem("Analyse", tabName = "analyse", icon = icon("magnifying-glass-chart")),
    menuItem("Diagnostics", tabName = "diagnose", icon = icon("flask"))
  )
)

filein <- fileInput("file1", "Upload a CSV file of EQ-5D and EQVAS scores",
  multiple = FALSE,
  accept = c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv"
  )
)

filein_details <- box(
  title = "File upload details",
  checkboxInput("header", "Header", TRUE),
  radioButtons("sep", "Separator",
    choices = c(
      Comma = ",",
      Semicolon = ";",
      Tab = "\t"
    )
  ),
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

analyseinputs <- box(
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
    min = 2,
    max = 4
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
  width = 3
)
