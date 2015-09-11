library(shiny)

# Define UI for application that draws joint probability surface
shinyUI(
fluidPage(
  fluidRow(
    column(width = 2,
        sliderInput("N",
                  "Number of data points:",
                  min = 2,
                  max = 41,
                  value = 2,
                  step = 5),          
        sliderInput("Gri",
                  "Resolution of likelihood:",
                  min = 11,
                  max = 35,
                  value = 11,
                  step = 3),
        sliderInput("seed",
                  "Random seed:",
                  min = 1,
                  max = 100,
                  value = 1,
                  step = 1),
        checkboxInput("log", "Log-likelihood?", FALSE)
    ),
    column(width = 4,
      plotOutput("plot2", height = 350
      ),
      plotOutput("plot1", height = 350,
        click = "plot1_click"
      )
    ),    
    column(width = 4,
        dataTableOutput("mytable")
        )
  )
)

)
