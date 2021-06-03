# Returns the likelihood function for a Rayleigh distribution with the given data (formatted as a vector).
likeFunc <- function(data) {
  n = length(data)
  sumLnRi <- sum(log(data))
  sumRiSquared <- sum(data^2)
  output <- function(theta) {
    #Do the exp-log of the likelihood to avoid overflow errors
    exp(
      -2*n*log(theta) + sumLnRi - (sumRiSquared / (2 * theta**2))
    )
  }
  return(output)
}

#Returns the maximum likelihood estimate and estimated variance.
#output$est is the estimate, output$var is the estimated variance of the MLE estimator
mle <- function(data) {
  n <- length(data)
  output <- list()
  output$est <- sqrt(mean(data**2) / 2)
  output$var <- output$est / (4*n)
  return(output)
}

# Takes data from a Rayleigh distribution and estimates the parameter theta via
# maximum likelihood estimation
# Output:
# $est - the MLE estimate
# $var - the estimated variance of the MLE estimator
# $likeFunc - the likelihood function
mle_analysis <- function(filepath) {
  data <- read.table(filepath, quote="\"", comment.char="")
  data <- data[[1]]
  output <- mle(data)
  output$likeFunc <- likeFunc(data)
  return(output)
}

# Takes data from a Rayleigh distribution and estimates the parameter theta via
# method of moments
# Output:
# $ est - the method of moments estimate
# $ var - the estimated variance of the MoM estimator
mom_analysis <- function(filepath) {
  data <- read.table(filepath, quote="\"", comment.char="")
  data <- data[[1]]
  output <- list()
  n <- length(data)
  output$est <- mean(data) * sqrt(2 / pi)
  output$var <- ((4 / pi) - 1) * (output$est **2 / n)
  return(output)
}