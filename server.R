library(shiny)
library(ggplot2)

shinyServer(
  function(input, output) {
    output$inputTable <- renderTable(
      {
        data.frame(
          Lambda=input$lambda,
          Population.Size=input$population,
          Subsample.Size=input$subsample.size,
          Subsample.Count=input$subsample.count
        )
      },
      include.rownames=FALSE
    )
    output$newDist <- renderPlot({
      set.seed(123123)
      lambda <- input$lambda
      population <- input$population
      subsample.size <- input$subsample.size
      subsample.count <- input$subsample.count
      vals <- data.frame(X = rexp(population,lambda))
      mns <- NULL
      sds <- NULL
      variances <- NULL
      for (i in 1 : subsample.count) {
        x <- rexp(subsample.size,lambda)
        mns <- c(mns, mean(x))
        sds <- c(sds,sd(x))
        variances <- c(variances,var(x))
      }
      dat <- data.frame(mns=mns,sds=sds)

      mean.of.means <- mean(mns)
      sd.of.means <- sd(mns)
      variance.of.means <- var(mns)
      sdsmn <- mean(sds)
      variancesmn <- mean(variances)
      theory.sd.of.means <- 1/(lambda*sqrt(subsample.size))
      output$resultsTable <- renderTable({
        data.frame(
          #Variance.of.Means = c( theory.sd.of.means^2, variance.of.means),
          Mean.of.Subsamples = c( 1/lambda, mean.of.means ),
          SD.of.Subsample.Means = c( theory.sd.of.means, sd.of.means),
          #Population.Variance = c((1/lambda)^2, var(vals$X)),
          row.names = c("Theoretical", "Simulated")
        )
      })
      
      
      g <- ggplot(data=dat, aes(x=mns) ) +
        geom_histogram(color="black", fill="blue",binwidth=.3,aes( y = ..density..), alpha = .2) +
        xlab("X") +
        ylab("Density") +
        ggtitle("Distribution of means from the exponential distribution") +
        stat_function(fun = dnorm, args = list(mean = 1/lambda, sd = (1/lambda)/sqrt(subsample.size)),aes(color="red")) +
        stat_function(fun = dnorm, args = list(mean = mean.of.means, sd = sd.of.means), aes(color="blue")) +
        geom_vline(xintercept=c(1/lambda + ((1/lambda)/sqrt(subsample.size)) * c(-3,-2,-1,0,1,2,3)), linetype="dashed", color="red") +
        geom_vline(xintercept=mean.of.means + sd.of.means * c(-3,-2,-1,0,1,2,3), linetype="dashed", color="blue") +
        scale_color_manual(
          name="Legend", 
          values = c("blue", "red", "blue","red"), 
          labels = c("Sample Normal Distribution", "CLT Normal Distribution" )
        ) + 
        theme(legend.position="bottom",legend.title=element_blank())
      g
    })
  }
)