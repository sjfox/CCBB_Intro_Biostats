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


## some random formatting stuff
fancy_scientific <- function(l) {
     # turn in to character string in scientific notation
     l <- format(l, digits=2, scientific = TRUE)
     # quote the part before the exponent to keep all the digits
     l <- gsub("^(.*)e", "'\\1'e", l)
     # turn the 'e+' into plotmath format
     l <- gsub("e", "%*%10^", l)
     # return this as an expression
     parse(text=l)
}


  v <- reactiveValues(
    mu = NULL,
    col = NA
  )
  output$plot1 <- renderPlot({
      log=input$log
      set.seed(input$seed)
      N <- input$N
      Y <- rnorm(N, 0, 1)
      Y <- Y[order(Y)]
      Gri <- input$Gri
          gV <- seq(0, 3, length.out=(Gri-1)/2+1)[-c(1)]
          gV <- c(-gV[((Gri-1)/2):1], 0, gV[1:((Gri-1)/2)])
      if(log){ 
          Py <- sapply(gV, function(m) dnorm(Y, m, 1, log=T))
          Lik <- sapply(gV, function(m) sum(dnorm(Y, m, 1, log=T)))
      } else {
          Py <- sapply(gV, function(m) dnorm(Y, m, 1))
          Lik <- sapply(gV, function(m) prod(dnorm(Y, m, 1)))      
      }
      likPl <- data.frame(mu=gV, P=Lik)
      likPl$col <- 1

    if(!is.na(v$col)) likPl$col[v$col] <- 2
    p1 <- ggplot(likPl, aes(x=mu, y=P)) + geom_line() + geom_point(aes(color=factor(col)), size=7) + 
        theme_minimal() + theme(legend.position="none", panel.border=element_rect(fill=NA)) + ylab("Likelihood") + xlab("Value of mean parameter") +
        scale_color_manual(values=c("1"="blue", "2"="red")) + 
        scale_x_continuous(breaks=seq(-3,3,0.5), limits=c(-3,3)) + 
        theme(axis.text = element_text(size=12), axis.title = element_text(size=16))
    if(!log) p1 = p1 + scale_y_continuous(labels=fancy_scientific)
    if(log) p1 = p1 + ylab("Log likelihood") + ylim(-260, 0)
    p1
  })
  
  output$plot2 <- renderPlot({  
      log=input$log
      set.seed(input$seed)
      N <- input$N
      Y <- rnorm(N, 0, 1)
      Y <- Y[order(Y)]
      Gri <- input$Gri
          gV <- seq(0, 3, length.out=(Gri-1)/2+1)[-c(1)]
          gV <- c(-gV[((Gri-1)/2):1], 0, gV[1:((Gri-1)/2)])
      if(log){ 
          Py <- sapply(gV, function(m) dnorm(Y, m, 1, log=T))
          Lik <- sapply(gV, function(m) sum(dnorm(Y, m, 1, log=T)))
      } else {
          Py <- sapply(gV, function(m) dnorm(Y, m, 1))
          Lik <- sapply(gV, function(m) prod(dnorm(Y, m, 1)))      
      }
      likPl <- data.frame(mu=gV, P=Lik)
      likPl$col <- 1

      if(!is.null(v$mu)){
            pD <- data.frame(P = dnorm(Y,v$mu,1,log=log), Y=Y)
            g2<-ggplot(pD,aes(x=Y,y=P)) + stat_function(fun=function(x) dnorm(x,v$mu,1,log=log)) + geom_point(color="red", size=5) + geom_rug(sides="t", color="red", size=1.5)
    } else {
            pD <- data.frame(P = dnorm(Y,0,1,log=log), Y=Y)
            g2<-ggplot(pD,aes(x=Y,y=P)) + geom_blank() + geom_rug(sides="t", color="red", size=1.5)
    }
    g2 = g2 + theme_minimal() +  theme(panel.border=element_rect(fill=NA)) + xlab("Value of data") + ylab("Probability density") + 
            scale_x_continuous(breaks=seq(-3,3,0.5), limits=c(-3,3)) + 
        theme(axis.text = element_text(size=12), axis.title = element_text(size=16)) 
    if(log){ g2 = g2 + ylab("Log probability density") + ylim(min(Py), max(Py)+diff(range(Py))*0.05)}
    else { g2 = g2 + scale_y_continuous(labels=fancy_scientific, limits=c(min(Py), max(Py)+diff(range(Py))*0.05))}
    g2
  })

output$mytable <- renderDataTable({
      log=input$log
      set.seed(input$seed)
      N <- input$N
      Y <- rnorm(N, 0, 1)
      Y <- Y[order(Y)]
      Gri <- input$Gri
          gV <- seq(0, 3, length.out=(Gri-1)/2+1)[-c(1)]
          gV <- c(-gV[((Gri-1)/2):1], 0, gV[1:((Gri-1)/2)])
      if(log){ 
          Py <- sapply(gV, function(m) dnorm(Y, m, 1, log=T))
          Lik <- sapply(gV, function(m) sum(dnorm(Y, m, 1, log=T)))
      } else {
          Py <- sapply(gV, function(m) dnorm(Y, m, 1))
          Lik <- sapply(gV, function(m) prod(dnorm(Y, m, 1)))      
      }
      likPl <- data.frame(mu=gV, P=Lik)
      likPl$col <- 1
    if(log) {
       po <-round(t(Py),2)
    }
    else { po <- round(t(Py),3) }
    colnames(po) <- paste0("Y", 1:N)
    if(log){
        gg <- data.frame(point = 1:nrow(po), mu = round(gV,2), log_likelihood = round(Lik,2), po)
    }
    else{
        gg <- data.frame(point = 1:nrow(po), mu = round(gV,2), likelihood = format(Lik,digits=3), po)
        }
    return(gg)
    })


  # Toggle points that are clicked
    observeEvent(input$plot1_click, {
      log=input$log
      set.seed(input$seed)
      N <- input$N
      Y <- rnorm(N, 0, 1)
      Y <- Y[order(Y)]
      Gri <- input$Gri
          gV <- seq(0, 3, length.out=(Gri-1)/2+1)[-c(1)]
          gV <- c(-gV[((Gri-1)/2):1], 0, gV[1:((Gri-1)/2)])
      if(log){ 
          Py <- sapply(gV, function(m) dnorm(Y, m, 1, log=T))
          Lik <- sapply(gV, function(m) sum(dnorm(Y, m, 1, log=T)))
      } else {
          Py <- sapply(gV, function(m) dnorm(Y, m, 1))
          Lik <- sapply(gV, function(m) prod(dnorm(Y, m, 1)))      
      }
      likPl <- data.frame(mu=gV, P=Lik)
      likPl$col <- 1
      res <- nearPoints(likPl, input$plot1_click, threshold=7, allRows = TRUE)
            print(res$selected_)
            print(all(!res$selected))
        if(all(!res$selected_)){
            v$mu <- NULL
            v$col <- NA
        } else {
            v$mu <- likPl$mu[res$selected_][1]
            v$col <- which(likPl$mu==v$mu)
       }
       print(v$mu)
    })
}
#  runApp("~/Dropbox/School/biostatistics2015/likelihood_shiny")

)
