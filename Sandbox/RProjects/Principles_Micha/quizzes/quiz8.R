#install.packages("docstring")
library(docstring)

VarianceToSd <- function(x, is_sd=T){
  #' Helper function
  #' 
  #' @param x given variance

  if (is_sd == F){x = sqrt(x)}
  return(x)
}

#+++++++ What is Confidence Level? +++++++#
#denoted 1-α, CL is the probability that the real value of the
#expectancy is within the Confidence Interval range 

#+++++++ Zehuyot for Confidence Level +++++++#
# 1-α = p 
# α = 1-p
# α/2 = (1-p)/2
# 1-α/2 = 1-(1-p)/2
# 1-α/2 = (2-(1-p))/2
# 1-α/2 = (1+p)/2

rangeConfIntervalZ<-function(n, p, avg, sd, is_sd=T){
  #' CI for Normal distribution with given variance
  #'
  #' The Confidence Interval is calculated using the Z table, i.e.
  #' the cumulative probability function of the standardized norm distr. 
  #'
  #' @param n number of samples
  #' @param p required Confidence Level, 1-α
  #' @param avg mean of all samples, as the expectancy's estimator 
  #' @param sd given standard deviation
  #' @param is_sd F when given variance, otherwise will be treated as sd 

  VarianceToSd(sd,is_sd)
  
  z=qnorm((1+p)/2)
  cat("Confidence interval for a confidence level of:",paste0(p*100,"%"))
  cat("\n lower bound (left border): ", avg-z*(sd/sqrt(n)))
  cat("\n upper bound (right border): ", avg+z*(sd/sqrt(n)))
  cat("\n length is :" , 2*z*sd/sqrt(n))
}

#run example:
rangeConfIntervalZ(n=10 ,sd=12.9 ,p=0.92,avg=75 ,is_sd=F)

#++++++++++++++++++++++++++++++++++++++++

rangeConfIntervalT<-function(n, sd, p, avg, is_sd=T){
  #when given the variance enter F, otherwise will be treated as sd
  #' CI for Normal distribution with unknown variance
  #'
  #' The Confidence Interval is calculated using the T distribution
  #'
  #' @param n number of samples
  #' @param p required Confidence Level, 1-α
  #' @param avg mean of all samples, as the expectancy's estimator 
  #' @param sd estimate of standard deviation
  #' @param is_sd F when given variance, otherwise will be treated as sd 
  
  VarianceToSd(sd,is_sd)
  
  t=qt(((1+p)/2),(n-1)) #quantile of variance estimate
  cat("Confidence interval for a confidence level of:",paste0(p*100,"%"))
  cat("\n lower bound (left border): ", avg-t*(sd/sqrt(n)))
  cat("\n upper bound (right border): ", avg+t*sd/sqrt(n))
  cat("\n length is: " , 2*t*sd/sqrt(n))
  
}


#run example:
rangeConfIntervalT(n= 10,sd= 12.9, p= 0.92, avg=75 ,is_sd=T)

ConfLevel<-function(n,upperb, lowerb=0, sd, is_sd=T){
  #' CL for Normal distribution with given variance
  #'
  #' The Confidence Level is reverse-engineered from the
  #' given bounds of the Confidence Interval
  #'
  #' @param n number of samples
  #' @param upperb upper bound (right), set to length if given
  #' @param lowerb lower bound (left), leave if given length
  #' @param sd given standard deviation
  #' @param is_sd F when given variance, otherwise will be treated as sd 
  
  VarianceToSd(sd, is_sd)
  
  L=upperb-lowerb
  p= 2*pnorm( L*sqrt(n) / (2*sd) ) -1
  
  
  cat(" Quantile of Confidence Level: 1-α=", round(p,3))
  cat("\n Confidence Level that was used: ", paste0(round(p,3)*100,"%"))
  cat("\n Confidence interval length is : ", L)
}

#run example:
ConfLevel(n= 20, sd= 5, upperb=4, lowerb=0, is_sd=T)
