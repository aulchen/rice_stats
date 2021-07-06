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
#by using the asymptotic variance.
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
mle_analysis <- function(data) {
  output <- mle(data)
  output$likeFunc <- likeFunc(data)
  return(output)
}

# Takes data from a Rayleigh distribution and estimates the parameter theta via
# method of moments
# Output:
# $ est - the method of moments estimate
# $ var - the estimated variance of the MoM estimator
mom_analysis <- function(data) {
  output <- list()
  n <- length(data)
  output$est <- mean(data) * sqrt(2 / pi)
  output$var <- ((4 / pi) - 1) * (output$est **2 / n)
  return(output)
}

# Returns the density of a Rayleigh distribution with parameter theta at point x
drayleigh <- function(x, theta = 1) {
  return((x / (theta**2)) * exp((-x**2) / (2 * theta**2)))
}

#Generates Rayleigh random variables with parameter theta
rrayleigh <- function(n, theta = 1) {
  #Uniform(0, 1)
  output <- runif(n, min = 0, max = 1)
  #Rayleigh 1
  output <- sqrt(-2 * log(1-output))
  #Rayleigh theta
  output <- theta * output
  return(output)
}