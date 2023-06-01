#Constants for use in global environment

tab_about <-  tabItem(
  tabName = "about",
  title = "EQBayes",
  p("Welcome to EQBayes, an RShiny app for sampling from the joint posterior of the EQ5D utility score and the EQVAS.", br(),
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
    "To get started, click the drop down menu at the top left to open the sidebar, and load in some data!",br(),
    "NOTE: Data is not retained by the app. However, if you are concerned about confidentiality,",
    " you can download the app from Github and run it in your local environment.",
    hr(),
    "The underlying software uses the nimble package, a fast, intuitive C++ compiling program for Markov Chain Monte Carlo (MCMC).",
    "The model assumes flat priors of Beta(1,1) and Normal(0,10000) for utility and binary variables, respectively.",
    "If you know that you want to specify your own priors, you may be better off specifying your own model as per the paper linked above.",
    "Check out the r-nimble.org examples to get started on your own models.",
    "For any questions, comments, or bug reports, please email: robin.blythe@qut.edu.au"
  )
)