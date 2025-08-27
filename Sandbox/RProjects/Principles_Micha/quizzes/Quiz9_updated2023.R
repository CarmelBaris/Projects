

VarEstimator <- function(samples, cntr=NULL, is_biased){
  #' Calculates estimator for unknown variance of 1 given population sample
  #' If unbiased, use built-in `var(x)` func: s^2 = [σ (xi – x.bar)^2] / (n-1)
  #' If biased, calculates explicitly: σ^2 = [σ (xi – x.bar)^2] / n
  #' 
  #' @param samples a vector of observed values
  #' @param cntr expectancy; if `NULL`, defaults to norm. distr. formula
  #' @param is_biased whether the given expectancy is estimated or concrete
  
  #expectancy estimator in *normally* distributed sample:
  if(cntr.est==NULL){cntr.est=mean(samples)}
  
  n=length(samples)
  
  var <- ifelse(is_biased,
                for(xi in samples){sum(xi-cntr.est)^2}/n,
                var(samples))
  
  return(var)
}

ConfInterval <- function(cntr.est, var, c){
  #' Returns Confidence Interval (CI) bounds for requested point estimator.
  #' C*SD is the required distance (in SD units) from the point estimator.
  #' 
  #' @param cntr.est point estimator of central measurement (e.g. sample mean)
  #' @param c probability of 'success'
  #' @param var either simple (1 population sample) or complex (weighted sum)
  
  margin.error = c*sqrt(var) #weighted standard distance from the central measurement
  ci <- c(cntr.est-margin.error, cntr.est+margin.error) #upper & lower bounds of CI
  return(ci)
}

ConfLvl <- function(conf.lvl){
  #' Converts Confidence Level from percentages to quantile
  #' 
  #' @param conf.lvl Confidence Level (1-α), in decimal points (90% >> 0.9)
  α = 1-conf.lvl
  quantile = 1-0.5*α
  return(quantile)
}

CI.Exp <- function(samples, is.known, conf.lvl){
  #' Calculates Confidence Interval (CI) bounds for expectancy
  #' 
  #' @param samples a vector of observed values
  #' @param is.known whether sample's variance is given ('T') or estimated ('F')
  #' @param conf.lvl required Confidence Level (1-α), in decimal points

  cntr.est <- mean(samples)
  var <- VarEstimator(samples, cntr.est, ifelse(is.known, F, T))
  quantile <- ConfLvl(conf.lvl)
  ci <- ConfInterval(cntr.est, var, ifelse(is.known,
                                         pnorm(quantile),
                                         pt(quantile)))
  return(ci)
}  

CI.DifExps <-function(mu.x, mu.y, sigma.x=NULL, sigmay=NULL, is.known){
  #' Returns Confidence Interval bounds for {difference btw expectancies}.
  #' 
  #' @param mu.x expectancy estimator of 1st random variable μ('X')
  #' @param mu.y expectancy estimator of 2nd random variable μ('Y')
  #' @param σx variance estimator of 1st random variable ('X')
  #' @param σy variance estimator of 2nd random variable ('Y')

  
  diff <- mu.x-mu.y #Difference of given expectancy values, the central measurement
  
  # ifelse(is.known & NOT is.empty(sigma.x) & NOT is.empty(sigma.y))

  
  # c <- function(x, type) {
  #   switch(type,
  #          mean = mean(x),
  #          median = median(x))
  # }
  
  
  ci <- ConfInterval(center=diff, c=c, var=sd^2)
  return(ci)
}

CI.Var <- function(samples, is.known, conf.lvl){
  #' Calculates Confidence Interval (CI) bounds for expectancy
  #' 
  #' @param samples a vector of observed values
  #' @param is.known whether sample's variance is given ('T') or estimated ('F')
  #' @param conf.lvl required Confidence Level (1-α), in decimal points

  cntr.est <- mean(samples)
  var <- VarEstimator(samples, cntr, ifelse(is.known, F, T))
  quantile <- ConfLvl(conf.lvl)
  ci<- ConfInterval(cntr.est, var, ifelse(is.known,
                                         pnorm(quantile),
                                         pt(quantile)))
  return(ci)
}  

VarEqKn <- function(dif){
  #' Returns Confidence Interval bounds for difference btw expectancies.
  #' 
  #' @param dif Difference of given expectancy values, the central measurement.
  return(dif)
}

PropVarianceEst <- function(samples, p.max.bound=0.5, sigma, prop=F){
  #' Required
  #' 
  #' @param CL required quantile of confidence level (e.g. if 90%, insert 0.9)
  #' @param CI.len.max.bound given maximal length of confidence interval
  #' @param p.max.bound given maximal proportion of "successful" samples (default in Bernoulli is 0.5, given cannot exceed this default)
  
  if(estimator=="proportion"){
    est.var.population <- p.max.bound*(1-p.max.bound)}
  
  else if(estimator=="expectancy" & is.empty(sigma))
    {est.var.population <- sigma^2} #known standard deviance
  
  else est.var.population <- Var(samples, method = c("unbiased"))
  
  return(est.var.population)
}


#++++++++++++++++++++++++++++++++
#+++ ESTIMATING SAMPLE SIZES ++++

ConfToSampleSize <- function(samples){
  #' Calculates sample size s.t. required CL is ensured
  #' Formula: s^2 = sigma (xi – xbar)^2 / (n-1)
  #' 
  #' @param samples a vector of samples
  
  xbar=mean(samples)
  n=length(samples)
  varianceN1 <- vector(length=n)
  
  for(xi in samples){
    varianceN1[i] <- sum((xi-xbar)^2) / (n-1)
  }
  
  return((sqrt.n)^2)
}


PropConfToSampleSize <- function(CL, CI.len.max.bound, p.max.bound=0.5, prop=F){
  #' Calculates 
  #' 
  #' @param CL required quantile of confidence level (e.g. if 90%, insert 0.9)
  #' @param CI.len.max.bound given max length of confidence interval
  #' @param p.max.bound given max proportion of "successful" samples
  #'        (default in Bernoulli is 0.5, given cannot exceed this default)
  #' @param sigma
  
  α=1-CL
  conf.lvl=1-0.5*α
  Z=pnorm(conf.lvl)
  
  est.var.population <- sigma^2
  
  sqrt.n <- 2*Z*est.var.population / CI.len.max.bound
  
  return((sqrt.n)^2)
}

#++++++++++++++++++++++++++++++++
