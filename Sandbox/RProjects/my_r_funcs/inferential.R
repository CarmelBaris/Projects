sampleSize <- function(conf.prcnt, sd, L0){
  #' Calculates the sample size required to meet required confidence level percentage.
  #' It is assumed that given Bernouli random variable can be approximated to a Normal distribution.
  #' To achieve widest CI (length), we'll assume maximum possible value of var(p)=p(1-p) i.e. 0.25.
  #'
  #' @param conf.prcnt given required confidence level (in decimal fractions, i.e. 0.95 for 95%)
  #' @param sd given standard deviation. if given variance, use <sqrt(varience)>
  #' @param L0 point estimator

  alpha <- 1-conf.prcnt
  z <- 1-0.5*alpha # required quantile
  min.size <- (2*qnorm(z)*sd/L0)^2 # `n` is the unknown sample size
  return(cat("A sample size of",
             ceiling(min.size), # assuming partial samples are unacceptable (e.g. there is no half of a person)
             "is required for a Confidence Level of",
             paste0(round(z,3)*100,"%", ".\n")))
}

ciExpectancy <- function(conf.prcnt, mode="UP", is.var.est="KN",
                         avg.x=0, sd.x, n.x,
                         avg.y=0, sd.y=0, n.y=9999){
  #' Calculates confidence interval bounds & length for expectancy of either one or two given samples.
  #'
  #' @param conf.prcnt given required confidence level (in decimal fractions, i.e. 0.95 for 95%)
  #' @param sd given standard deviation. if given variance, use <sqrt(varience)>
  #' @param n given sample size. when given a vector of observations, use <length(vector)>.
  #' @param is.var.est whether variance was given or estimated
  diff <- avg.x-avg.y #used to calculate bounds
  q <- 1-0.5*(1-conf.prcnt) # required quantile
  df <- if (avg.y != 0){n.x+n.y-2} else {n.x-1} # degrees of freedom, depends on whether variable is singular
  weight <- if (avg.y != 0){n.x-1} else {1} # weight of first estimator, depends on singularity of variable
  sd.combo <- switch(is.var.est,
                     "KN" = sqrt( (sd.x^2)/n.x + (sd.y^2)/n.y ), # both known
                     "EQ" = sqrt( (sd.x^2)/n.x + (sd.y^2)/n.y ), # both estimated yet equal
                     "UN" = sqrt( ( weight*(sd.x^2) + (n.y-1)*(sd.y^2) )/ df)) # both estimated and unequal
  error <- if (is.var.est=="KN") {qnorm(q)*sd.combo} else {qt(q, df)*sd.combo}
  
  cat(switch(mode,
             "UP" = cat("Confidence Interval upper bound is", round(diff+error, 3)),
             "LO" = cat("Confidence Interval lower bound is", round(diff-error, 3)),
             "LEN"= cat("Confidence Interval length is", round(2*error, 3))
  ), "for a confidence level of", paste0(round(conf.prcnt,3)*100,"%", ".\n")
  )
}



# ANSWER 5 = 163.3754
Question5 <- function(){
  avg.x <- 155
  n.x <- 12
  var.x <- 261
  
  alpha <- 1-0.9
  t <- 1-0.5*alpha
  
  #given that variance is unknown
  mu_x <- avg.x+qt(t, n.x-1)*sqrt(var.x/n.x)
}

Question6 <- function(){
  # #ANSWER 6 = -311.039
  #given that variance is unknown yet equal in both stores
  avg.x <- 155
  n.x <- 12
  var.x <- 261
  
  avg.y <- 455
  n.y <- 12
  var.y <- 79
  
  alpha <- 1-0.95
  t <- 1-0.5*alpha
  df.combo <- n.x+n.y-2
  t.prob <- qt(t, df.combo)
  
  #given that variance is unknown
  var.pooled <- ( (n.x-1)*var.x + (n.y-1)*var.y )/ df.combo
  diff <- avg.x-avg.y
  theta.diff <- diff-t.prob*sqrt((var.pooled/n.x) + (var.pooled/n.y))
  theta.diff
}

# #ANSWER 7 = 0.8641118
Question7 <- function(){
  return(pchisq(7,4)) #returns cumulative probability
}

Question8 <- function(){
  # #ANSWER 8 = 15.03321
  return(qchisq(0.98,6)) #returns quantile
}

Question9 <- function(){
  # #ANSWER 9 = 3.197917
  x <- c(2.04, 3.84, 2.45, 2.28, 3.34, 2.97, 3.24, -0.15, 1.53, 2.16, 3.63, 3.92)
  x.mean <- mean(x)
  x.var <- var(x)
  theta_l <- (length(x)-1) * x.var/qchisq(1-0.05/2, length(x)-1)
  theta_u <- (length(x)-1) * x.var/qchisq(0.05/2, length(x)-1)
  L <- theta_u - theta_l
  return(L)
}

Question10 <- function(){
  # #ANSWER 10 = 298168.3
  x_mean <- 24
  n <- 79
  alpha <- 1-0.84
  z <- 1-alpha/2
  z_prob <- qnorm(z)
  sd.x <- 4
  power <- 4
  theta_l_4 <- (x_mean - z_prob * sd.x/sqrt(n))^(power)
  return(theta_l_4)
}

