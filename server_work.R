library(ggplot2)
library(knitr)
set.seed(123123)
lambda <- 0.2
# Simulate and plot an exponential distribution of numbers
vals <- data.frame(X = rexp(1000,lambda));
# Simulate sampling 40 values from the distribution and collect statistics about the samples
mns <- NULL
sds <- NULL
variances <- NULL
for (i in 1 : 1000) {
  x <- rexp(40,lambda)
  mns <- c(mns, mean(x))
  sds <- c(sds,sd(x))
  variances <- c(variances,var(x))
}
dat <- data.frame(mns=mns,sds=sds)
mnsmn <- mean(mns)
mnssd <- sd(mns)
mnsvar <- var(mns)
sdsmn <- mean(sds)
variancesmn <- mean(variances)
mnssd_theory <- 1/(lambda*sqrt(40))
mnsvar_theory <- mnssd_theory^2
# Display a table of means for the two simulations
means_table <- data.frame(
  "sample.mean"=mean(vals$X),
  "means.mean"=mnsmn,
  "theory.mean"=1/lambda
)
# Plot the variances for the large sample distribution
variance_table <- data.frame(
  "sample.variance"=var(vals$X),
  "samples.variance.mean"=variancesmn,
  "theory.population.var"=(1/lambda)^2
)
variance_table <- data.frame(
  "sample.means.var"= mnsvar,
  "theory.sample.means.var"=(1/(lambda*sqrt(40)))^2
)
# Distribution Plot
g <- ggplot(data=dat, aes(x=mns) ) +
  geom_histogram(color="black", fill="blue",binwidth=.3,aes( y = ..density..), alpha = .2) +
  xlab("X") +
  ylab("Density") +
  ggtitle("Distribution of means from the exponential distribution") +
  stat_function(fun = dnorm, args = list(mean = 1/lambda, sd = (1/lambda)/sqrt(40)),aes(color="red")) +
  stat_function(fun = dnorm, args = list(mean = mnsmn, sd = mnssd), aes(color="blue")) +
  geom_vline(xintercept=c(1/lambda + ((1/lambda)/sqrt(40)) * c(-3,-2,-1,0,1,2,3)), linetype="dashed", color="red") +
  geom_vline(xintercept=mnsmn + mnssd * c(-3,-2,-1,0,1,2,3), linetype="dashed", color="blue") +
  scale_color_manual(
    name="Legend", 
    values = c("blue", "red", "blue","red"), 
    labels = c("Sample Normal Distribution", "CLT Normal Distribution" )
  ) + 
  theme(legend.position="bottom",legend.title=element_blank())
g