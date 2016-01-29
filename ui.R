library(shiny)

shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Exponential Data and the Central Limit Theorem"),
    
    sidebarPanel(
      numericInput('lambda', 'Lambda', 0.2, min = 0.1, max = 10, step = 0.1),
      numericInput('population', 'Population Count', 1000, min = 100, max = 10000, step = 100),
      numericInput('subsample.size', 'Sub Sample Size', 40, min = 10, max = 100, step = 10),
      numericInput('subsample.count', 'Sub Samples', 1000, min = 100, max = 10000, step = 100),
      submitButton('Submit')
    ),
    mainPanel(
      h2('Parameters'),
      h3('You entered'),
      tableOutput("inputTable"),
      h3('Results'),
      tableOutput("resultsTable"),
      plotOutput('newDist')
    )
  )
)