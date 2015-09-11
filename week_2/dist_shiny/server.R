library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  output$distPlot <- renderPlot({
    if(input$distr == "pois"){
        x <- 0:20
        pD <- data.frame(x=x, P=dpois(x, input$lam))
        p1 <- ggplot(pD, aes(x=x, y=P)) + geom_bar(stat="identity", fill = "darkblue") + theme_minimal() + theme(panel.border=element_rect(fill=NA)) + xlim(-0.5,20.5) + ylim(0,1) +
            theme(axis.title = element_text(size=16), axis.text = element_text(size=12)) + ylab("Probability")
    } else if(input$distr == "binom"){
        x <- 0:input$N
        pD <- data.frame(x=x, P=dbinom(x, prob=input$phi, size=input$N))    
        p1 <- ggplot(pD, aes(x=x, y=P)) + geom_bar(stat="identity", fill = "darkblue") + theme_minimal() + theme(panel.border=element_rect(fill=NA)) + geom_vline(xintercept=input$N+0.5, color="red") + xlim(-0.5,30.5) + ylim(0,1) +
            theme(axis.title = element_text(size=16), axis.text = element_text(size=12)) + ylab("Probability")
    } else if(input$distr == "normal"){
        x <- seq(-10, 10, 0.01)
        pD <- data.frame(x=x, P=dnorm(x, input$mu, input$va))
        pD <- rbind(c(-10, 0), pD, c(10, 0))
        p1 <- ggplot(pD, aes(x=x, y=P)) + geom_polygon(fill = "darkblue", alpha=0.5, color = "darkblue") + theme_minimal() + theme(panel.border=element_rect(fill=NA)) + xlim(-10,10)+ ylim(0,0.45) +
            theme(axis.title = element_text(size=16), axis.text = element_text(size=12)) + ylab("Probability density")
    } else if(input$distr == "uniform"){
        x <- seq(0,5, 0.01)
        pD <- data.frame(x=x, P=1/input$upper)
        pD <- rbind(c(0,0), pD, c(5,0))
        p1 <- ggplot(pD, aes(x=x, y=P)) + geom_polygon(fill = "darkblue", alpha=0.5, color = "darkblue") + theme_minimal() + theme(panel.border=element_rect(fill=NA)) + xlim(0,8) + geom_vline(xintercept=input$upper, color="red") + xlim(0,5) + ylim(0,2.5) +
            theme(axis.title = element_text(size=16), axis.text = element_text(size=12)) + ylab("Probability density")
    }
  p1 + xlab("Value of random variable")
  })
  
#  runApp("~/Dropbox/School/biostatistics2015/dist_shiny")

})
