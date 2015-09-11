library(shiny)
library(mvtnorm)
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

  output$jointPlot <- renderPlot({
    gri <- expand.grid(X = round(seq(-4,4,0.05),2), Y = round(seq(-4,4,0.05),2))
    C1 <- input$C
    C2 <- input$C
    B <- input$A#input$B
    A <- input$A
    FixY <- input$FixY
    FixX <- input$FixX
    gri$P1 <- dnorm(gri$X,mean=(B*gri$Y+C1)/(A*(gri$Y^2)+1),sd=sqrt(1/(A*(gri$Y^2)+1))) 
    gri$P2 <- dnorm(gri$Y,mean=(B*gri$X+C2)/(A*(gri$X^2)+1),sd=sqrt(1/(A*(gri$X^2)+1)) )
    gri$P <- gri$P1*gri$P2
    conditionalX = input$plotTypeX
    conditionalY = input$plotTypeY
    condLimitX <- condLimitY <- range(gri$P)
    # draw the histogram with the specified number of bins
    g1 <- ggplot(gri, aes(x=X, y=Y)) + geom_tile(aes(color=P, fill=P)) + geom_contour(aes(z=P, alpha=P), color="black") + 
        scale_fill_gradient2(low="black", mid="goldenrod", high="white", midpoint = mean(range(gri$P)), limits=range(gri$P)) +
        scale_color_gradient2(low="black", mid="goldenrod", high="white", midpoint = mean(range(gri$P)), limits=range(gri$P)) +
        theme_minimal() + theme(legend.position="none", axis.text=element_blank(), axis.ticks=element_blank(), panel.border=element_rect(fill=NA)) +
        scale_x_continuous(expand=c(0,0), limits=c(-4,4)) + scale_y_continuous(expand=c(0,0), limits=c(-4,4)) +
        theme(plot.margin=unit(c(-0.45,0.1,0.1,0.1), "cm")) + xlab("Value of X") + ylab("Value of Y") + 
        theme(axis.title = element_text(size=16))
   
    g2 <- ggplot(rbind(c(-4,0,0,0,0), gri[gri$Y==FixY,], c(4,0,0,0,0)), aes(x=X, y=P)) + geom_blank() +
        theme_minimal() + theme(legend.position="none", axis.text=element_blank(), axis.ticks=element_blank(), axis.title.x = element_blank(), panel.grid=element_blank()) +
        theme(plot.margin=unit(c(0.3,0.1,0,0.1), "cm"))
    if(conditionalX == "conditional"){
        g2 <- g2 + geom_polygon(alpha=0.75, color="firebrick", fill="firebrick") +
            scale_x_continuous(expand=c(0,0), limits=c(-4,4)) + scale_y_continuous(expand=c(0,0), limits=c(0, condLimitX[2]))
        g1 <- g1 + geom_hline(yintercept=FixY, color="red", size=0.8, lty = 3)
        } else if(conditionalX=="marginal") {
        
        margX <- Vectorize(function(X){
            integrate(function(y){dnorm(X,mean=(B*y+C1)/(A*(y^2)+1),sd=sqrt(1/(A*(y^2)+1)))*dnorm(y,mean=(B*X+C1)/(A*(X^2)+1),sd=sqrt(1/(A*(X^2)+1)))}, lower=-Inf, upper=Inf)$val
            })
        mX <- data.frame(X = round(seq(-4,4,0.1),2))
        mX$P <- margX(mX$X)
        
        g2 <- ggplot(rbind(c(-4,0,0,0,0), mX, c(4,0,0,0,0)), aes(x=X, y=P)) + geom_polygon(alpha=0.75, color="darkblue", fill="darkblue") +
            theme_minimal() + theme(legend.position="none", axis.text=element_blank(), axis.ticks=element_blank(), axis.title.x = element_blank()) +
            scale_x_continuous(expand=c(0,0), limits=c(-4,4)) + scale_y_continuous(expand=c(0,0), limits=c(0, max(mX$P))) +
            theme(plot.margin=unit(c(0.3,0.1,0,0.1), "cm"))
        }

    g3 <- ggplot(rbind(c(0,-4,0,0,0), gri[gri$X==FixX,], c(0,4,0,0,0)), aes(x=Y, y=P)) + geom_blank() +
                    theme_minimal() + theme(legend.position="none", axis.text=element_blank(), axis.ticks=element_blank(), axis.title = element_blank(), panel.grid=element_blank()) + 
                    theme(plot.margin=unit(c(-0.45,0.4,0.425,-0.53), "cm")) + coord_flip()

    if(conditionalY == "conditional"){
        g3 <- g3 + geom_polygon(alpha=0.75, color="firebrick", fill="firebrick") +
            scale_x_continuous(expand=c(0,0), limits=c(-4,4)) + scale_y_continuous(expand=c(0,0), limits=c(0, condLimitY[2]))
        g1 <- g1 + geom_vline(xintercept=FixX, color="red", size=0.8, lty = 3)
        } else if(conditionalY == "marginal"){
        
        margY <- Vectorize(function(Y){
            integrate(function(x){dnorm(Y,mean=(B*x+C1)/(A*(x^2)+1),sd=sqrt(1/(A*(x^2)+1)))*dnorm(x,mean=(B*Y+C1)/(A*(Y^2)+1),sd=sqrt(1/(A*(Y^2)+1)))}, lower=-Inf, upper=Inf)$val
            })
        mY <- data.frame(Y = round(seq(-4,4,0.1),2))
        mY$P <- margY(mY$Y)

        g3 <- ggplot(rbind(c(0,-4,0,0,0), mY, c(0,4,0,0,0)), aes(x=Y, y=P)) + geom_polygon(alpha=0.75, color="darkblue", fill="darkblue") +
            theme_minimal() + theme(legend.position="none", axis.text=element_blank(), axis.ticks=element_blank(), axis.title = element_blank()) +
            scale_x_continuous(expand=c(0,0), limits=c(-4,4)) + scale_y_continuous(expand=c(0,0), limits=c(0, max(mY$P))) +
            theme(plot.margin=unit(c(-0.45,0.4,0.425,-0.53), "cm")) + coord_flip()
        
        }

    g4 <- ggplot(data.frame(x=1,y=1), aes(x=x, y=y)) + geom_blank() +
        theme_minimal() + theme(legend.position="none", axis.text=element_blank(), panel.grid=element_blank(), axis.ticks=element_blank(), axis.title = element_blank())
        
    
    g2 = g2 + ylab(" ") + 
        theme(axis.title = element_text(size=16))
    grid.arrange(g2, g4, g1, g3, nrow=2, heights=c(0.3,0.7), widths=c(0.7,0.3))
    
  })
  
#  runApp("~/Dropbox/School/biostatistics2015/probability_shiny")

})
