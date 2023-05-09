library(shiny)

# Define UI for application that samples from joint posterior of EQ5D and EQVAS
ui <- fluidPage(

    # Application title
    titlePanel("ShinyEQBayes: A Bayesian method for joint sampling from EQ5D and EQVAS"),
    h4("This RShiny app samples from the joint posterior distribution of the EQ5D and EQVAS scores."),
    h4("For an in-depth explanation of the methods, please see Blythe et al (2022): https://doi.org/10.1016/j.jval.2022.01.017"),

    # Upload button 
    sidebarLayout(
      
      #Panel for inputs
        sidebarPanel(
            fileInput("file1", "Upload a CSV file of EQ-5D and EQVAS scores",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            #Input: Select separator
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            #Input: Select number of rows to display
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            #Try 1
            uiOutput("columns"),
            
            #Input: Select EQ5D column
            selectInput("eq5d", "EQ5D utility:",
                        choices = c("Col1",
                                    "Col2")),
            
            #Input: Select EQVAS column
            selectInput("eqvas", "EQVAS score (0-100)",
                        choices = c("Col1", 
                                    "Col2")),
            
            #Input: Select method
            selectInput("method", "Posterior sampling method:",
                        choices = c("Gibbs sampler",
                                    "INLA",
                                    "TMB"))
        ),
        
        #Main panel to display outputs
        mainPanel(
          
          tableOutput("contents")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$columns = renderUI({
    inputdata = get(input$file1)
    selectInput('columns2', 'Columns', names(inputdata))
  })
  
  output$contents <- renderTable({
    
    #Load in CSV
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep=input$sep)
    
    if(input$disp == "head"){
      return(head(df))
    }
    else{
      return(df)
    }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
