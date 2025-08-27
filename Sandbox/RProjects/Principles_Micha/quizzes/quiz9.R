sampleSize <- function(conf.prcnt, sd, L0){
  # assuming given Bernouli random variable can be approximated to a Normal distribution
  # to achieve widest CI (length), we'll assume maximum possible value of var(p)=p(1-p) i.e. 0.25
  alpha <- 1-conf.prcnt
  z <- 1-0.5*alpha # required quantile
  min.size <- (2*qnorm(z)*sd/L0)^2 # `n` is the unknown sample size
  return(cat(ceiling(min.size), # assuming partial samples are unacceptable (e.g. there is no half of a person)
             "for a Confidence Level of",
             paste0(round(z,3)*100,"%", ".\n")))
}

ciExpLen <- function(conf.prcnt, sd, n, is.var.est=0){
  alpha <- 1-conf.prcnt
  q <- 1-0.5*alpha # required quantile
  ans <- switch(is.var.est, 2*qnorm(q)*sd/sqrt(n), 2*qt(q)*sd/sqrt(n))

  return(cat("Length of CI is",
             round(ans, 3), # assuming partial samples are unacceptable (e.g. there is no half of a person)
             "for a Confidence Level of",
             paste0(round(z,3)*100,"%", ".\n"))
  )}

ciExpWeighted <- function(conf.prcnt, mode="Up", is.var.est="KN",
                         avg.x, sd.x, n.x,
                         avg.y=0, sd.y=0, n.y=9999){
  diff <- avg.x-avg.y
  error <- switch(is.var.est, qnorm(q), qt(q, df))*sd.combo
  q <- 1-0.5*(1-conf.prcnt) # required quantile
  df <- switch(avg.y, n.x+n.y-2, n.x-1) # degrees of freedom, depends on whether variable is singular
  sd.combo <- switch(is.var.est,
                     "KN" = sqrt( (sd.x^2)/n.x + (sd.y^2)/n.y ), # both known
                     "EQ" = sqrt( (sd.x^2)/n.x + (sd.y^2)/n.y ), # both estimated yet equal
                     "UN" = sqrt( ( (n.x-1)*(sd.x^2) + (n.y-1)(sd.y^2) )/ df)) # both estimated and unequal

  cat(switch(mode,
             "Up" = cat("Confidence Interval upper bound is", round(diff+error, 3)),
             "Lo" = cat("Confidence Interval lower bound is", round(diff-error, 3)),
             "Len"= cat("Confidence Interval length is", round(2*error, 3))
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
  x=c(2.04,3.84,2.45,2.28,3.34,2.97,3.24,-0.15,1.53,2.16,3.63,3.92)
  x.mean = mean(x)
  x.var = var(x)
  theta_l <- (length(x)-1) * x.var/qchisq(1-0.05/2, length(x)-1)
  theta_u = (length(x)-1) * x.var/qchisq(0.05/2,length(x)-1)
  L = theta_u - theta_l
  return(L)
}

Question10 <- function(){
  # #ANSWER 10 = 298168.3
  x_mean = 24
  n = 79
  alpha = 1-0.84
  z = 1-alpha/2
  z_prob = qnorm(z)
  sd.x <- 4
  power <- 4
  theta_l_4 = (x_mean - z_prob * sd.x/sqrt(n))**power
  return(theta_l_4)
}

# +++++++++++++ EXAMPLE RUNS +++++++++++++
# Answer to question 1:
sampleSize(conf.prcnt=0.9, sd=0.5, L0=0.19)

# Answer to question 2:
sampleSize(conf.prcnt=0.95, sd=3.06, L0=0.5)

# Answer to question 3:
ciExpLen(conf.prcnt = 0.8, sd=16, n=58)

# Answer to question 4:
ciExpWeighted(conf.prcnt = 0.94, mode="Up",
             avg.x = 58.6, sd.x = 7, n.x = 60,
             avg.y = 54.6, sd.y = 16, n.y = 58)

# Answer to question 5:
ciExpWeighted(conf.prcnt = .9, is.var.est = 1,
             avg.x = 155, sd.x = sqrt(261), n.x = 12)

ciExpWeighted(conf.prcnt = .9, is.var.est = 1,
             avg.x = 640, sd.x = sqrt(163), n.x = 12)

# Answer to question 6:
ciExpWeighted(conf.prcnt = .90, mode = "Up", is.var.est = 1,
             avg.x = 155, sd.x = sqrt(261), n.x = 12,
             avg.y = 455, sd.y = sqrt(79), n.y = 12)

ciExpWeighted(conf.prcnt = .95, mode = "Lo", is.var.est = 1,
             avg.x = 11100, sd.x = sqrt(1500), n.x = 100,
             avg.y = 7500, sd.y = sqrt(800), n.y = 150)
# Answer to question 6:
