library(gridExtra)
library(ggplot2)
library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Probability distribution functions"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("distr", "Probability Distribution:",
        c(Poisson="pois", Binomial="binom", Normal="normal", Uniform="uniform")),
        
      conditionalPanel(
          condition = "input.distr == 'pois'",
          sliderInput("lam",
                  "Rate parameter:",
                  min = 0.1,
                  max = 10,
                  value = 3,
                  step = 0.5)
                  ),
      conditionalPanel(
          condition = "input.distr == 'binom'",
          sliderInput("phi",
                  "Probability of success:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.1),
          sliderInput("N",
                  "Number of trials:",
                  min = 2,
                  max = 30,
                  value = 10,
                  step = 2)
                  ),
      conditionalPanel(
          condition = "input.distr == 'normal'",
          sliderInput("mu",
                  "Mean:",
                  min = -10,
                  max = 10,
                  value = 0,
                  step = 0.5),
          sliderInput("va",
                  "Standard deviation:",
                  min = 1,
                  max = 8,
                  value = 1,
                  step = 1)
                  ),
      conditionalPanel(
          condition = "input.distr == 'uniform'",
          sliderInput("upper",
                  "Upper bound:",
                  min = 0.5,
                  max = 5,
                  value = 1,
                  step = 0.1)
                  )
         ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height="600px", width="700px")
    )
  )
))

