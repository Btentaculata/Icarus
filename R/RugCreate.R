#' Creates "RUG" plots for binomial GLMs
#'
#' This function creates "RUG" plots that can be used to test for overfitting of binomial GLMs
#' @param x: Your independent variable.  Only GLMs with 1 independent variable are supported
#' @param y: Your dependent variable.
#' @param num.bins: The number of bins to break your data into
#' @param xlab: Label for X axis
#' @param ylab: Label for Y axis
#' @param xlim: Two value vector of x axis start and stop
#' @keywords GLM Binomial Icarus
#' @export
#' @examples
#' RugCreate()

RugCreate <- function(x, 
                      y, 
                      num.bins, 
                      xlab = "x", 
                      ylab = "y", 
                      xlim = c(min(x), max(x)), 
                      pch = 1,
                      plot.model = TRUE){

  model <- glm(y ~ x, family = binomial(link = "logit")) #This should be the same as your model
  plot(x, y, xlab = xlab, ylab = ylab, xlim = xlim, pch = pch) #Plots points on a graph

  #rug() creates a series of tick marks at each point. Successes connect to top.  Failures to bottom.
  for(i in 1:length(x)){
    if(y[i] == 1){
      rug(x[i], side = 3)
    } else {rug(x[i], side = 1)
    	}
    }

  #Plots your model's predicted probabilities as a line
  xv <- seq(from = min(x), to = max(x), length.out = length(y))
  yv <- predict(model, list(length = xv), type = "response")
  if(plot.model == TRUE){lines(xv, yv)}

  #Breaks your data into bins and finds the mean probabilities of each bin
  cutl <- cut(x, num.bins)
  tapply(y, cutl, sum)
  table(cutl)
  probs <- tapply(y, cutl, sum) / table(cutl)
  probs
  probs <- as.vector(probs)
  resmeans <- tapply(x, cutl, mean)
  lenmeans <- tapply(x, cutl, mean)
  resmeans <- as.vector(resmeans)
  lenmeans <- as.vector(lenmeans)

  #Defines and plots the mean probabilities of the bins (these are the balls)
  points(lenmeans, probs, pch = 16, cex=2)

  #Defines the standard error bars of the bins
  se <- sqrt(probs * (1 - probs) / table(cutl))
  up <- probs + as.vector(se)
  down <- probs - as.vector(se)

  #Plots the standard error bars on the appropriate bins
  for(i in 1:num.bins){
    lines(c(resmeans[i], resmeans[i]), c(up[i], down[i]))
  	}
}
