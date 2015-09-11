library(shiny)

# Define UI for application that draws joint probability surface
shinyUI(fluidPage(

  # Application title
  titlePanel("Joint, Conditional, and Marginal Probability"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Parameters of the Banana distribution"),
      sliderInput("A",
                  "Banana-ness:",
                  min = 0,
                  max = 3,
                  value = 0,
                  step = 0.2),
#     sliderInput("B",
#                 "B:",
#                 min = 0,
#                 max = 1,
#                 value = 0,
#                  step = 0.1),
      sliderInput("C",
                  "Direction:",
                  min = -1.5,
                  max = 1.5,
                  value = 0,
                  step = 0.1),

      h3(""),
      h3("Side plots"),

      selectInput("plotTypeX", "On X-axis:",
        c(None="none", Conditional="conditional", Marginal="marginal")),
        
      conditionalPanel(
          condition = "input.plotTypeX == 'conditional'",
          sliderInput("FixY",
                  "Condition on value of Y:",
                  min = -4,
                  max = 4,
                  value = 0,
                  step = 0.1)
                  ),
      
      selectInput("plotTypeY", "On Y-axis:",
        c(None="none", Conditional="conditional", Marginal="marginal")),
        
      conditionalPanel(
          condition = "input.plotTypeY == 'conditional'",
          sliderInput("FixX",
                  "Condition on value of X:",
                  min = -4,
                  max = 4,
                  value = 0,
                  step = 0.1)
                  )

         ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("jointPlot", height="600px", width="700px")
    )
  )
))
